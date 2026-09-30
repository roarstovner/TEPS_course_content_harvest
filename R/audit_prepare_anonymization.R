# R/audit_prepare_anonymization.R
# Build per-institution review packets for the `anonymization` audit
# (.claude/skills/audit-institutions/checks/anonymization.md).
#
# Audits anonymize_fulltext() (R/anonymize.R): extracted_text -> course_plan.
# The course_plan is recomputed here with the current code, so a fix to
# anonymize.R can be re-audited without re-running the dedup pipeline.
# For each sampled course the packet shows
#   (a) the anonymized course_plan — audit for personal data left in, and
#   (b) every span anonymization removed, with context — audit for content
#       removed that should have been kept.
#
# Deterministic pre-pass flags (course-level, on course_plan):
#   email       e-mail address left
#   phone       8-digit phone-number-like sequence left
#   name_label  staff label (emneansvarlig, faglærer, godkjent av, ...) followed
#               by a capitalised two-word name
#   name_like   2+ capitalised word pairs outside literature references
#   admin_date  dates / semester-year labels / admin year stamps left
#   removed     unusually large share of the text removed (over-removal)
#   artifact    empty brackets left behind by removals
#
# Inputs:  data/html_{inst}.RDS (harvest output: extracted_text)
# Outputs: data/audit/anonymization/packets/{inst}.md, manifest.csv, sample.csv
#
# Run:  Rscript R/audit_prepare_anonymization.R [inst ...]

source("R/audit_utils.R")
source("R/anonymize.R")

# ── Tunables ─────────────────────────────────────────────────────────────────
SUSPECT_N     <- 20
RANDOM_N      <- 8
PLAN_TRUNC    <- 7000   # max chars of course_plan shown
REMOVED_TRUNC <- 3000   # max chars of the removed-spans list shown
CONTEXT_TOK   <- 12     # tokens of context around each removed span
OUTLIER_Z     <- 3
SEED          <- 42

CHECK   <- "anonymization"
out_dir <- file.path(audit_dir(CHECK), "packets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

EMAIL_RX <- "\\b[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\b"
PHONE_RX <- "(?<![\\d-])(?:\\+47\\s*)?\\d{2}\\s?\\d{2}\\s?\\d{2}\\s?\\d{2}(?![\\d-])"
NAME_LABEL_RX <- paste0(
  "(?i:emneansvarle?i?g|faglærer|faglærar|foreleser|forelesar|kontaktperson|",
  "koordinator|studieveileder|godkjent av|course coordinator|lecturer|",
  "contact person|teacher)",
  "\\s*[:\\-–]?\\s*\\p{Lu}\\p{Ll}+(?:\\s+\\p{Lu}\\.?)?\\s+\\p{Lu}\\p{Ll}+")
# Mid-sentence capitalised word pair (after a lowercase word or comma), e.g.
# "ved Kari Nordmann". Line-initial pairs are mostly headings.
NAME_PAIR_RX <- "(?<=[\\p{Ll},;]\\s)\\p{Lu}\\p{Ll}{2,}\\s+\\p{Lu}\\p{Ll}{2,}\\b"
REFERENCE_LINE_RX <- regex(paste(
  "\\(\\d{4}[a-z]?\\)", "\\b\\d{4}[a-z]?\\)", "\\bforlag", "\\bpress\\b",
  "\\bred\\.", "\\beds?\\.", "isbn", "universitetsforlaget", "cappelen",
  "gyldendal", "fagbokforlaget", "\\bdoi\\b", "https?://", sep = "|"),
  ignore_case = TRUE)
ADMIN_DATE_RX <- regex(paste(
  "\\b\\d{1,2}\\.\\s?(?:jan|feb|mar|apr|mai|jun|jul|aug|sep|okt|nov|des)[a-z]*\\.?\\s+\\d{4}",
  "\\b\\d{1,2}\\.\\d{1,2}\\.\\d{2}\\b",
  "(?:høst|vår|haust|autumn|spring)\\s*\\d{2,4}\\b",
  "(?:opprettet|oppdatert|revidert|vedtatt|godkjent|gjeldende fra|gyldig fra|gjelder fra)\\s*:\\s*\\d{4}",
  sep = "|"), ignore_case = TRUE)

name_like_count <- function(txt) {
  if (is.na(txt)) return(0L)
  l <- str_split_1(txt, "\n")
  l <- l[!str_detect(l, REFERENCE_LINE_RX)]
  sum(str_count(l, NAME_PAIR_RX))
}

tok <- function(x) str_extract_all(coalesce(x, ""), "\\S+|\\s+")[[1]]

# Spans removed (or rewritten) between extracted_text and course_plan, each
# with a little context: "… before [−removed−] after …".
removed_spans <- function(before, after) {
  a <- tok(before)
  b <- tok(after)
  if (length(a) == 0) return(character())
  d <- diffobj::ses_dat(a, b, warn = FALSE)
  op <- as.character(d$op)
  del <- op == "Delete"
  if (!any(del)) return(character())
  runs <- rle(del)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  out <- character()
  for (k in which(runs$values)) {
    idx <- starts[k]:ends[k]
    removed <- str_squish(paste(d$val[idx], collapse = ""))
    if (!nzchar(removed)) next
    keep_ctx <- which(op != "Delete")
    pre  <- tail(keep_ctx[keep_ctx < starts[k]], CONTEXT_TOK)
    post <- head(keep_ctx[keep_ctx > ends[k]], CONTEXT_TOK)
    out <- c(out, paste0("… ", str_squish(paste(d$val[pre], collapse = "")),
                         " [−", removed, "−] ",
                         str_squish(paste(d$val[post], collapse = "")), " …"))
  }
  out
}

# ── Per institution ──────────────────────────────────────────────────────────
files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
institutions <- sort(str_match(basename(files), "^html_(.*)\\.RDS$")[, 2])
institutions <- setdiff(institutions, "samas")   # no extracted text by design
requested <- audit_args_institutions()
if (length(requested) > 0) institutions <- intersect(institutions, requested)
if (length(institutions) == 0) stop("No data/html_{inst}.RDS for: ",
                                    paste(requested, collapse = ", "), call. = FALSE)
manifest <- list()
sample   <- list()

for (inst in institutions) {
  df <- readRDS(file.path("data", paste0("html_", inst, ".RDS")))
  if (!"extracted_text" %in% names(df) && "fulltext" %in% names(df)) {
    df$extracted_text <- df$fulltext
  }
  df <- df |>
    select(-any_of(c("html", "html_error"))) |>
    filter(!is.na(extracted_text), nzchar(extracted_text))
  if (nrow(df) == 0) next
  df$course_plan <- anonymize_fulltext(df$institution, df$extracted_text,
                                       .progress = FALSE)
  df <- df |>
    mutate(
      plan      = coalesce(course_plan, ""),
      dedup_key = vapply(plan, digest::digest, character(1), algo = "xxhash64",
                         USE.NAMES = FALSE),
      removed_share = 1 - nchar(plan) / nchar(extracted_text)
    )
  rs  <- df$removed_share
  med <- median(rs, na.rm = TRUE)
  md  <- mad(rs, na.rm = TRUE)
  # When most plans lose nothing, MAD is 0; fall back to a fixed margin.
  z   <- if (is.na(md) || md == 0) ifelse(rs - med > 0.15, Inf, 0) else (rs - med) / (1.4826 * md)

  df <- df |>
    mutate(
      n_name_like     = vapply(course_plan, name_like_count, integer(1)),
      flag_email      = str_detect(plan, EMAIL_RX),
      flag_phone      = str_detect(plan, PHONE_RX),
      flag_name_label = str_detect(plan, NAME_LABEL_RX),
      flag_name_like  = n_name_like >= 2,
      flag_admin_date = str_detect(plan, ADMIN_DATE_RX),
      flag_removed    = (z > OUTLIER_Z & removed_share > 0.05) | removed_share > 0.5,
      flag_artifact   = str_detect(plan, "\\(\\s*\\)|\\[\\s*\\]")
    )
  flag_cols <- c("flag_email", "flag_phone", "flag_name_label", "flag_name_like",
                 "flag_admin_date", "flag_removed", "flag_artifact")
  weights <- c(3, 2, 3, 1, 1, 2, 1)
  df$score <- as.vector(as.matrix(df[flag_cols]) %*% weights)

  selected <- audit_sample(select(df, course_id, dedup_key, score),
                           SUSPECT_N, RANDOM_N, SEED)

  flag_counts <- colSums(df[flag_cols])
  lines <- c(
    audit_packet_header(CHECK, inst, selected, paste(
      "For each course, audit **Anonymized course plan** for personal data or admin",
      "dates left in, and **Removed by anonymization** for course content that was",
      "removed but should have been kept. `[−…−]` marks removed text; the words",
      "around it are unchanged context.")),
    "## Institution context",
    "",
    sprintf("- %d course offerings with extracted text; median share of text removed %.1f%%",
            nrow(df), 100 * median(df$removed_share, na.rm = TRUE)),
    sprintf("- Institution-specific handler in R/anonymize.R: %s",
            if (grepl(paste0("\\.anon_", inst, "\\b"),
                      paste(deparse(.anon_institution), collapse = "\n"))) {
              paste0("`.anon_", inst, "()`")
            } else "none (generic rules only)"),
    sprintf("- Pre-pass flag counts (all offerings): %s",
            paste(sprintf("%s %d", str_remove(flag_cols, "^flag_"), flag_counts), collapse = ", ")),
    ""
  )

  for (i in seq_len(nrow(selected))) {
    cid  <- selected$course_id[i]
    kind <- selected$kind[i]
    r <- df[match(cid, df$course_id), ]
    on <- str_remove(flag_cols[unlist(r[flag_cols]) %in% TRUE], "^flag_")
    spans <- removed_spans(r$extracted_text, r$course_plan)
    lines <- c(lines,
      sprintf("---\n\n## COURSE %d — `%s`  [%s]", i, cid, kind),
      "",
      sprintf("- %s (%s) · %s %s · %.1f%% of text removed",
              r$Emnekode_raw, r$Emnenavn, r$Semesternavn, r$Årstall,
              100 * r$removed_share),
      if (length(on) > 0) sprintf("- ⚑ flags: %s", paste(on, collapse = ", ")) else character(),
      "",
      "### Anonymized course plan (audit: personal data or admin dates left?)",
      "",
      audit_fence(audit_trunc(coalesce(r$course_plan, "(empty after anonymization)"), PLAN_TRUNC)),
      "",
      sprintf("### Removed by anonymization (%d spans — audit: content lost?)", length(spans)),
      "",
      audit_fence(audit_trunc(if (length(spans) > 0) paste(spans, collapse = "\n") else "(nothing removed)",
                              REMOVED_TRUNC)),
      "")
  }

  path <- file.path(out_dir, paste0(inst, ".md"))
  writeLines(lines, path)
  manifest[[inst]] <- tibble(
    institution = inst,
    n_suspect = sum(selected$kind == "SUSPECT"),
    n_random  = sum(selected$kind == "RANDOM"),
    packet    = path
  )
  sample[[inst]] <- mutate(selected, institution = inst, .before = 1)
  cat(sprintf("  %-8s %2d suspects + %2d random -> %s (%.0f KB)\n", inst,
              manifest[[inst]]$n_suspect, manifest[[inst]]$n_random, path,
              file.size(path) / 1024))
}

audit_write_index(CHECK, bind_rows(manifest), bind_rows(sample))
