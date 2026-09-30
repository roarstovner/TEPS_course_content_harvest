# R/audit/prepare_fulltext.R
# Build per-institution review packets for the `fulltext` audit
# (.claude/skills/audit-institutions/checks/fulltext.md).
#
# Audits the harvest's `extracted_text` (the CSS/JSON/PDF extraction of each
# course page) against the fetched page itself. For each sampled course the
# packet shows
#   (a) metadata (code, year, semester, status, language, URL),
#   (b) extracted_text — what to audit, and
#   (c) page text that is NOT in extracted_text, with site chrome removed —
#       the evidence for missing content.
# "Site chrome" = lines occurring on >= CHROME_FRAC of the institution's pages
# (navigation, footers, and recurring headings, which are captured anyway).
#
# Deterministic pre-pass flags (course-level):
#   empty      page fetched but extracted_text empty
#   short/long robust length outlier within institution (|z| > OUTLIER_Z)
#   wall       long text with almost no line breaks
#   junk       known junk phrases (cookie banners, page numbers, JS, ...)
#   dup_code   identical extracted_text for >= DUP_CODES different course codes
#   year       semester/academic-year labels in the text all far from Årstall
#   uncaptured much non-chrome page text missing from extracted_text
#
# Inputs:  data/html_{inst}.RDS (harvest output)
# Outputs: data/audit/fulltext/packets/{inst}.md, manifest.csv, sample.csv
#
# Run:  Rscript R/audit/prepare_fulltext.R [inst ...]

source("R/audit/utils.R")

# ── Tunables ─────────────────────────────────────────────────────────────────
SUSPECT_N     <- 20
RANDOM_N      <- 8
FAILED_N      <- 3      # failed fetches shown as extra examples (URL + error)
TEXT_TRUNC    <- 7000   # max chars of extracted_text shown
UNCAP_TRUNC   <- 2500   # max chars of uncaptured page text shown
PAGE_SAMPLE   <- 250    # pages parsed per institution for chrome + coverage
CHROME_FRAC   <- 0.4
OUTLIER_Z     <- 3
DUP_CODES     <- 3
UNCAP_MIN     <- 1500   # uncaptured non-chrome chars that raise a flag
SEED          <- 42

CHECK   <- "fulltext"
out_dir <- file.path(audit_dir(CHECK), "packets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source("R/fetch_html_cols.R")       # institution_config depends on fetch_fn refs
source("R/extract_fulltext.R")      # pre/post fns used by institution_config
source("R/institution_config.R")

JUNK_RX <- regex(paste(
  "informasjonskapsler", "\\bcookies?\\b", "hopp til (hoved)?innhold",
  "skip to (main )?content", "\\blogg inn\\b", "\\bsid(e|en) \\d+ av \\d+",
  "powered by", "tcpdf", "function\\s*\\(", "moment\\.locale", "\\bvar \\w+ =",
  "velg studieår", "document\\.", "window\\.", "undefined",
  sep = "|"), ignore_case = TRUE)

YEAR_RX <- regex(paste0(
  "(?:høst|vår|haust|autumn|spring)\\s+(\\d{4})",
  "|studieår\\s+(\\d{4})",
  "|\\b(20\\d{2})\\s*/\\s*(?:20)?\\d{2}\\b"), ignore_case = TRUE)

error_msg <- function(e) {
  if (is.null(e)) return(NA_character_)
  if (inherits(e, "condition")) return(conditionMessage(e))
  as.character(e)[1]
}

# Visible text of a fetched page: HTML is parsed (scripts/styles dropped);
# anything else (shadow-DOM text, PDF text, JSON) is used as is.
page_text <- function(h) {
  if (is.na(h) || !nzchar(h)) return(NA_character_)
  if (!grepl("^\\s*<", h)) return(h)
  doc <- tryCatch(xml2::read_html(h), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  xml2::xml_remove(rvest::html_elements(doc, "script, style, noscript, svg"))
  body <- rvest::html_element(doc, "body")
  rvest::html_text2(if (inherits(body, "xml_missing")) doc else body)
}

page_lines <- function(txt) {
  if (is.na(txt)) return(character())
  l <- unique(str_squish(str_split_1(txt, "\n")))
  l[nchar(l) >= 3]
}

# Lines of the page that are neither chrome nor present in extracted_text.
# A line counts as captured when its first 60 chars occur in the text, which
# tolerates different line wrapping of the same content.
uncaptured <- function(lines, chrome, ext) {
  lines <- setdiff(lines, chrome)
  if (length(lines) == 0) return(list(text = "", n = 0L, share = NA_real_))
  ext_n <- str_to_lower(str_squish(ext %|% ""))
  hit <- vapply(str_to_lower(str_sub(lines, 1, 60)),
                \(l) grepl(l, ext_n, fixed = TRUE), logical(1))
  miss <- lines[!hit]
  list(text = paste(miss, collapse = "\n"),
       n = sum(nchar(miss)),
       share = sum(nchar(miss)) / sum(nchar(lines)))
}
`%|%` <- function(a, b) if (is.na(a)) b else a

year_far <- function(txt, year) {
  if (is.na(txt)) return(FALSE)
  m <- str_match_all(txt, YEAR_RX)[[1]]
  if (nrow(m) == 0) return(FALSE)
  yrs <- as.integer(na.omit(c(m[, 2], m[, 3], m[, 4])))
  length(yrs) > 0 && min(abs(yrs - year)) > 1
}

# ── Per institution ──────────────────────────────────────────────────────────
files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
institutions <- sort(str_match(basename(files), "^html_(.*)\\.RDS$")[, 2])
requested <- audit_args_institutions()
if (length(requested) > 0) institutions <- intersect(institutions, requested)
if (length(institutions) == 0) stop("No data/html_{inst}.RDS for: ",
                                    paste(requested, collapse = ", "), call. = FALSE)
manifest <- list()
sample   <- list()

for (inst in institutions) {
  cfg <- get_institution_config(inst)
  if (identical(cfg$strategy, "noop")) {
    cat(sprintf("  %-8s skipped (noop strategy, no extracted text by design)\n", inst))
    next
  }
  df <- readRDS(file.path("data", paste0("html_", inst, ".RDS")))
  if (!"extracted_text" %in% names(df) && "fulltext" %in% names(df)) {
    df$extracted_text <- df$fulltext
  }
  df <- df |>
    mutate(
      html_error_msg = vapply(html_error, error_msg, character(1)),
      has_html  = !is.na(html) & nzchar(html),
      txt_nchar = nchar(coalesce(extracted_text, "")),
      dedup_key = if_else(txt_nchar > 0,
                          vapply(coalesce(extracted_text, ""), digest::digest, character(1),
                                 algo = "xxhash64", USE.NAMES = FALSE),
                          course_id)
    )

  # Text-level flags on every course with a fetched page
  lognc <- ifelse(df$txt_nchar > 0, log(df$txt_nchar), NA_real_)
  med <- median(lognc, na.rm = TRUE)
  md  <- mad(lognc, na.rm = TRUE)
  z   <- if (is.na(md) || md == 0) rep(0, nrow(df)) else (lognc - med) / (1.4826 * md)
  n_lines <- str_count(coalesce(df$extracted_text, ""), "\n") + 1
  codes_per_text <- df |>
    filter(txt_nchar > 0) |>
    group_by(dedup_key) |>
    summarise(n_codes = n_distinct(Emnekode), .groups = "drop")

  df <- df |>
    left_join(codes_per_text, by = "dedup_key") |>
    mutate(
      flag_empty    = has_html & txt_nchar == 0,
      flag_short    = !is.na(z) & z < -OUTLIER_Z,
      flag_long     = !is.na(z) & z >  OUTLIER_Z,
      flag_wall     = txt_nchar > 2000 & n_lines < txt_nchar / 1000,
      flag_junk     = str_detect(coalesce(extracted_text, ""), JUNK_RX),
      flag_dup_code = coalesce(n_codes, 1L) >= DUP_CODES,
      flag_year     = purrr::map2_lgl(extracted_text, Årstall, year_far)
    )

  # Page-level: chrome lines and uncaptured text, on a sample of pages plus
  # every text-flagged course (bounded by PAGE_SAMPLE + flagged).
  set.seed(SEED)
  text_flagged <- df$course_id[df$has_html & (df$flag_empty | df$flag_short |
                                                df$flag_long | df$flag_wall |
                                                df$flag_junk | df$flag_dup_code |
                                                df$flag_year)]
  pool_ids <- df |> filter(has_html) |> distinct(dedup_key, .keep_all = TRUE) |>
    slice_sample(n = min(PAGE_SAMPLE, sum(df$has_html))) |> pull(course_id)
  parse_ids <- unique(c(pool_ids, head(text_flagged, 150)))
  parsed <- df |> filter(course_id %in% parse_ids) |>
    select(course_id, html, extracted_text) |>
    mutate(lines = purrr::map(html, \(h) page_lines(page_text(h))))
  line_freq <- table(unlist(parsed$lines)) / nrow(parsed)
  chrome <- names(line_freq)[line_freq >= CHROME_FRAC]
  unc <- purrr::map2(parsed$lines, parsed$extracted_text, \(l, e) uncaptured(l, chrome, e))
  parsed <- parsed |>
    mutate(uncap_text  = purrr::map_chr(unc, "text"),
           uncap_n     = purrr::map_int(unc, "n"),
           uncap_share = purrr::map_dbl(unc, "share")) |>
    select(course_id, uncap_text, uncap_n, uncap_share)

  flag_cols <- c("flag_empty", "flag_short", "flag_long", "flag_wall",
                 "flag_junk", "flag_dup_code", "flag_year", "flag_uncaptured")
  weights <- c(3, 1, 1, 1, 1, 3, 2, 2)
  df <- df |>
    left_join(parsed, by = "course_id") |>
    mutate(flag_uncaptured = coalesce(uncap_n >= UNCAP_MIN, FALSE))
  df$score <- as.vector(as.matrix(df[flag_cols]) %*% weights)
  df$score[!df$has_html & df$txt_nchar == 0] <- 0

  # Eligible: parsed pages (so the uncaptured-text evidence exists) and text
  # without a stored page (PDF sources: steiner, part of uis).
  pool <- df |>
    filter(course_id %in% parse_ids | (!has_html & txt_nchar > 0)) |>
    select(course_id, dedup_key, score)
  selected <- audit_sample(pool, SUSPECT_N, RANDOM_N, SEED)
  failed <- df |>
    filter(!has_html, txt_nchar == 0, !is.na(url)) |>
    distinct(html_error_msg, .keep_all = TRUE) |>
    head(FAILED_N)
  selected <- bind_rows(selected,
                        tibble(course_id = failed$course_id,
                               kind = rep("FAILED", nrow(failed))))
  if (nrow(selected) == 0) next

  # ── Packet ──
  fetch_summary <- df |>
    mutate(outcome = case_when(has_html ~ "page fetched",
                               txt_nchar > 0 ~ "text without stored page (PDF source)",
                               is.na(url) ~ "no URL",
                               .default = coalesce(str_trunc(html_error_msg, 90), "not fetched"))) |>
    count(outcome, sort = TRUE) |> head(6)
  flag_counts <- colSums(df[flag_cols], na.rm = TRUE)
  lines <- c(
    audit_packet_header(CHECK, inst, filter(selected, kind != "FAILED"), paste(
      "For each course, audit **Extracted text** as a faithful, clean copy of the",
      "course plan on the page. **Page text not in extracted text** lists the page's",
      "lines that the extraction did not capture, with site-wide navigation/footer",
      "lines removed — use it to judge missing content. `[FAILED]` courses were not",
      "fetched; judge only whether the URL looks wrong.")),
    "## Institution context",
    "",
    sprintf("- Harvest config: strategy `%s`, selector `%s` (%s), year_in_url %s",
            cfg$strategy, cfg$selector %||% "-", cfg$selector_mode %||% "-",
            cfg$year_in_url %||% NA),
    sprintf("- %d course offerings; %d with a fetched page; %d with extracted text; median length %d chars",
            nrow(df), sum(df$has_html), sum(df$txt_nchar > 0),
            as.integer(median(df$txt_nchar[df$txt_nchar > 0]))),
    sprintf("- Pre-pass flag counts (all offerings): %s",
            paste(sprintf("%s %d", str_remove(flag_cols, "^flag_"), flag_counts), collapse = ", ")),
    sprintf("- %d recurring lines (site chrome and shared headings, on >= %.0f%% of %d parsed pages) left out of the page text below, e.g.: %s",
            length(chrome), 100 * CHROME_FRAC, length(parse_ids),
            paste0("\"", str_trunc(head(chrome, 8), 40), "\"", collapse = ", ")),
    "- Fetch outcomes:",
    paste0("  - ", fetch_summary$outcome, ": ", fetch_summary$n),
    ""
  )

  for (i in seq_len(nrow(selected))) {
    cid  <- selected$course_id[i]
    kind <- selected$kind[i]
    r <- df[match(cid, df$course_id), ]
    on <- str_remove(flag_cols[unlist(r[flag_cols]) %in% TRUE], "^flag_")
    lines <- c(lines,
      sprintf("---\n\n## COURSE %d — `%s`  [%s]", i, cid, kind),
      "",
      sprintf("- %s (%s) · %s %s · status %s · language %s",
              r$Emnekode_raw, r$Emnenavn, r$Semesternavn, r$Årstall,
              r$Statusnavn %||% r$Status, r$`Underv.språk` %||% "?"),
      sprintf("- URL: %s", r$url %||% "NA"),
      if (length(on) > 0) sprintf("- ⚑ flags: %s", paste(on, collapse = ", ")) else character(),
      ""
    )
    if (kind == "FAILED") {
      lines <- c(lines, sprintf("Fetch error: %s", r$html_error_msg %||% "(none recorded)"), "")
      next
    }
    lines <- c(lines,
      sprintf("### Extracted text (%d chars — audit this)", r$txt_nchar),
      "",
      audit_fence(audit_trunc(coalesce(r$extracted_text, "(empty)"), TEXT_TRUNC)),
      "")
    if (!r$has_html) {
      lines <- c(lines, "### Page text not in extracted text",
                 "", "_(no stored page: the text comes from a PDF — judge the text on its own)_", "")
      next
    }
    lines <- c(lines,
      sprintf("### Page text not in extracted text (%d chars, %s of non-chrome page text)",
              r$uncap_n %|% 0L,
              if (is.na(r$uncap_share)) "n/a" else sprintf("%.0f%%", 100 * r$uncap_share)),
      "",
      audit_fence(audit_trunc(if (nzchar(r$uncap_text %|% "")) r$uncap_text else "(none)", UNCAP_TRUNC)),
      "")
  }

  path <- file.path(out_dir, paste0(inst, ".md"))
  writeLines(lines, path)
  manifest[[inst]] <- tibble(
    institution = inst,
    n_suspect = sum(selected$kind == "SUSPECT"),
    n_random  = sum(selected$kind == "RANDOM"),
    n_failed  = sum(selected$kind == "FAILED"),
    packet    = path
  )
  sample[[inst]] <- mutate(selected, institution = inst, .before = 1)
  cat(sprintf("  %-8s %2d suspects + %2d random + %d failed -> %s (%.0f KB)\n", inst,
              manifest[[inst]]$n_suspect, manifest[[inst]]$n_random,
              manifest[[inst]]$n_failed, path, file.size(path) / 1024))
}

audit_write_index(CHECK, bind_rows(manifest), bind_rows(sample))
