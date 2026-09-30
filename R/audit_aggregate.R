# R/audit_aggregate.R
# Verify and merge per-institution audit findings
# (data/audit/{check}/findings/*.json) into one ranked table + markdown report.
# Part of the audit harness in .claude/skills/audit-institutions.
#
# Each input JSON follows .claude/skills/audit-institutions/findings_schema.md.
# Missing/extra fields are tolerated so one malformed agent output does not
# sink the aggregation; problems are reported instead.
#
# Verification (mechanical — catches invented findings, not wrong judgement):
#   - schema:   enum values are allowed for the check (AUDIT_CHECKS)
#   - ids:      every example_course_id was in that institution's packet
#               (data/audit/{check}/sample.csv)
#   - evidence: the quoted evidence occurs verbatim in the packet
#               (whitespace/case-insensitive; "…" separates fragments)
#
# Comparison: findings for each institution are compared with the version of
# the same JSON file at a git ref (default HEAD), keyed by target + error_type,
# so a re-run after a fix shows what persisted, what is new and what is gone.
#
# Outputs (for --dir data/audit/{check}/findings, the default):
#   data/audit/{check}/findings_all.RDS    one row per finding
#   data/audit/{check}/findings_report.md  ranked report
# A non-default --dir <path> writes <basename>_all.RDS / <basename>_report.md.
#
# Run:  Rscript R/audit_aggregate.R <check> [--dir <findings dir>] [--compare <git ref>|none]

source("R/audit_utils.R")

# ── Arguments ────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
check <- args[1]
if (is.na(check) || !check %in% names(AUDIT_CHECKS)) {
  stop("Usage: Rscript R/audit_aggregate.R <", paste(names(AUDIT_CHECKS), collapse = "|"),
       "> [--dir <findings dir>] [--compare <git ref>|none]", call. = FALSE)
}
base_dir    <- audit_dir(check)
find_dir    <- opt("dir", file.path(base_dir, "findings"))
compare_ref <- opt("compare", "HEAD")
out_stem    <- file.path(base_dir, basename(find_dir))

files <- list.files(find_dir, pattern = "\\.json$", full.names = TRUE)
if (length(files) == 0) stop("No findings JSON in ", find_dir,
                             " — run the review agents first.", call. = FALSE)

`%|%` <- function(a, b) ifelse(is.na(a), b, a)   # NA-coalesce
SEV_ORDER  <- c(high = 1, medium = 2, low = 3)
PREV_ORDER <- c(widespread = 1, common = 2, occasional = 3, rare = 4)
chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else
  paste(unlist(x), collapse = "; ")

# ── Read ─────────────────────────────────────────────────────────────────────
# Older reports used `institution_short` and `section`; accept both.
parse_findings <- function(obj, inst_fallback) {
  inst <- obj$institution %||% obj$institution_short %||% inst_fallback
  purrr::map(obj$findings %||% list(), \(x) tibble(
    institution           = inst,
    target                = chr(x$target %||% x$section),
    error_type            = chr(x$error_type),
    severity              = chr(x$severity),
    prevalence            = chr(x$prevalence),
    example_course_ids    = chr(x$example_course_ids),
    evidence              = chr(x$evidence),
    description           = chr(x$description),
    root_cause_hypothesis = chr(x$root_cause_hypothesis),
    suggested_fix         = chr(x$suggested_fix),
    confidence            = chr(x$confidence)
  )) |> bind_rows()
}

read_json_safely <- function(f) {
  tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
           error = function(e) { warning("Bad JSON: ", f, " (", e$message, ")"); NULL })
}

reports <- list()
rows <- list()
for (f in files) {
  obj <- read_json_safely(f)
  if (is.null(obj)) next
  inst <- str_remove(basename(f), "\\.json$")
  reports[[inst]] <- tibble(institution = inst,
                            model = chr(obj$model),
                            n_courses_reviewed = obj$n_courses_reviewed %||% NA_integer_,
                            overall_assessment = chr(obj$overall_assessment))
  rows[[inst]] <- parse_findings(obj, inst)
}
reports  <- bind_rows(reports)
findings <- bind_rows(rows)
if (nrow(findings) == 0) {
  findings <- tibble(institution = character(), target = character(),
                     error_type = character(), severity = character(),
                     prevalence = character(), example_course_ids = character(),
                     evidence = character(), description = character(),
                     root_cause_hypothesis = character(),
                     suggested_fix = character(), confidence = character())
}

# ── Verify ───────────────────────────────────────────────────────────────────
cfg <- AUDIT_CHECKS[[check]]
bad_enum <- function(x, allowed, field) {
  ifelse(is.na(x) | x %in% allowed, NA_character_, paste0(field, "='", x, "'"))
}

sample_path <- file.path(base_dir, "sample.csv")
sample_ids <- if (file.exists(sample_path)) read.csv(sample_path, stringsAsFactors = FALSE) else NULL

norm_txt <- function(x) {
  x |>
    str_replace_all("[“”«»]", "\"") |>
    str_replace_all("[‘’]", "'") |>
    str_to_lower() |>
    str_squish()
}

packet_text <- list()
get_packet <- function(inst) {
  if (is.null(packet_text[[inst]])) {
    p <- file.path(base_dir, "packets", paste0(inst, ".md"))
    packet_text[[inst]] <<- if (file.exists(p)) norm_txt(paste(readLines(p, warn = FALSE), collapse = "\n")) else NA_character_
  }
  packet_text[[inst]]
}

# Evidence is often a quote, several quotes, or a quote with "…" elisions.
# Every fragment of >= 12 chars must occur in the packet.
evidence_status <- function(evidence, inst) {
  pk <- get_packet(inst)
  if (is.na(pk)) return("no_packet")
  if (is.na(evidence) || !nzchar(evidence)) return("missing")
  ev <- norm_txt(evidence)
  quoted <- str_match_all(ev, "\"([^\"]{12,})\"|'([^']{12,})'")[[1]]
  parts <- if (nrow(quoted) > 0) coalesce(quoted[, 2], quoted[, 3]) else ev
  frags <- unlist(str_split(parts, "…|\\.\\.\\.|\\[\\.\\.\\.\\]|\\[…\\]"))
  frags <- str_trim(str_remove_all(frags, "^[\\s\"',.:;]+|[\\s\"',.:;]+$"))
  frags <- frags[nchar(frags) >= 12]
  if (length(frags) == 0) return("too_short")
  if (all(vapply(frags, \(f) grepl(f, pk, fixed = TRUE), logical(1)))) "verified" else "not_found"
}

ids_status <- function(ids, inst) {
  if (is.null(sample_ids)) return(NA_character_)
  ids <- str_split_1(ids %|% "", ";\\s*")
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return("no ids")
  unknown <- setdiff(ids, sample_ids$course_id[sample_ids$institution == inst])
  if (length(unknown) == 0) NA_character_ else paste("not in packet:", paste(unknown, collapse = ", "))
}

findings <- findings |>
  rowwise() |>
  mutate(
    schema_problem = paste(na.omit(c(
      bad_enum(target, cfg$targets, "target"),
      bad_enum(error_type, cfg$error_types, "error_type"),
      bad_enum(severity, names(SEV_ORDER), "severity"),
      bad_enum(prevalence, names(PREV_ORDER), "prevalence"),
      bad_enum(confidence, c("high", "medium", "low"), "confidence"))), collapse = "; "),
    ids_problem = ids_status(example_course_ids, institution),
    evidence_check = evidence_status(evidence, institution)
  ) |>
  ungroup() |>
  mutate(schema_problem = na_if(schema_problem, ""),
         # NA = could not be checked (packet or sample.csv missing)
         verified = case_when(
           !is.na(schema_problem) | !is.na(ids_problem) |
             evidence_check %in% c("not_found", "missing", "too_short") ~ FALSE,
           evidence_check == "verified" & !is.null(sample_ids) ~ TRUE,
           .default = NA))

# ── Compare with previous run ────────────────────────────────────────────────
git_json <- function(ref, path) {
  out <- suppressWarnings(system2("git", c("show", paste0(ref, ":", path)),
                                  stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

changes <- NULL
if (!identical(compare_ref, "none")) {
  changes <- purrr::map(files, \(f) {
    inst <- str_remove(basename(f), "\\.json$")
    old <- git_json(compare_ref, f)
    if (is.null(old)) return(NULL)
    key <- \(d) unique(paste(d$target, d$error_type, sep = " / "))
    old_keys <- key(parse_findings(old, inst))
    new_keys <- key(filter(findings, institution == inst))
    tibble(institution = inst,
           n_before = length(old_keys), n_now = length(new_keys),
           persisting = paste(intersect(new_keys, old_keys), collapse = "; "),
           new = paste(setdiff(new_keys, old_keys), collapse = "; "),
           gone = paste(setdiff(old_keys, new_keys), collapse = "; "))
  }) |> bind_rows()
  if (nrow(changes) == 0) changes <- NULL
}

# ── Rank + save ──────────────────────────────────────────────────────────────
findings <- findings |>
  mutate(.sev = SEV_ORDER[severity] %|% 9L,
         .prev = PREV_ORDER[prevalence] %|% 9L) |>
  arrange(.sev, .prev, institution) |>
  select(-.sev, -.prev)

saveRDS(findings, paste0(out_stem, "_all.RDS"))

# ── Report ───────────────────────────────────────────────────────────────────
esc <- function(x) str_replace_all(x %|% "", "\\|", "\\\\|") |> str_squish()
md_tbl <- function(df) {
  if (nrow(df) == 0) return("_(none)_")
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  body <- apply(df, 1, \(r) paste(esc(as.character(r)), collapse = " | "))
  paste(c(paste0("| ", hdr, " |"), paste0("| ", sep, " |"),
          paste0("| ", body, " |")), collapse = "\n")
}

unverified <- findings |>
  filter(verified %in% FALSE) |>
  rowwise() |>
  transmute(institution, target, error_type, severity,
            problem = paste(na.omit(c(schema_problem, ids_problem,
                                      if_else(evidence_check == "verified",
                                              NA_character_,
                                              paste0("evidence ", evidence_check)))),
                            collapse = "; "),
            evidence = str_trunc(evidence, 100)) |>
  ungroup()

cross <- findings |>
  group_by(target, error_type) |>
  summarise(n_inst = n_distinct(institution),
            institutions = paste(sort(unique(institution)), collapse = ", "),
            worst = names(SEV_ORDER)[min(SEV_ORDER[severity] %|% 3L)],
            .groups = "drop") |>
  filter(n_inst >= 3) |>
  arrange(desc(n_inst), worst)

top <- findings |>
  transmute(ok = case_when(verified %in% TRUE ~ "✓", verified %in% FALSE ~ "✗", .default = "?"),
            institution, target, error_type,
            severity, prevalence,
            example = str_trunc(example_course_ids, 60),
            suggested_fix = str_trunc(suggested_fix, 140))

models <- paste(sort(unique(na.omit(reports$model))), collapse = ", ")
report <- c(
  sprintf("# %s audit — aggregated findings", check),
  "",
  sprintf("%d findings across %d institutions (from %d agent reports in `%s`%s).",
          nrow(findings), n_distinct(findings$institution), nrow(reports), find_dir,
          if (nzchar(models)) paste0("; model: ", models) else ""),
  sprintf("Mechanical verification: %d passed (✓), %d failed (✗), %d not checkable (?, packet or sample.csv missing).",
          sum(findings$verified %in% TRUE), sum(findings$verified %in% FALSE),
          sum(is.na(findings$verified))),
  "Sorted by severity then prevalence. Source: `R/audit_aggregate.R`.",
  "",
  "## Failed verification — check by hand before acting",
  "",
  "Unknown course ids or evidence that does not occur verbatim in the packet.",
  "Usually a paraphrased quote; occasionally an invented finding.",
  "", md_tbl(as.data.frame(unverified)), "",
  "## Patterns in 3+ institutions",
  "", md_tbl(as.data.frame(cross)), "",
  if (!is.null(changes)) c(
    sprintf("## Change since `%s` (keyed by target / error_type)", compare_ref),
    "", md_tbl(as.data.frame(changes)), "") else character(),
  "## By error type",
  "", md_tbl(as.data.frame(count(findings, error_type, severity, sort = TRUE))), "",
  "## By target",
  "", md_tbl(as.data.frame(count(findings, target, sort = TRUE))), "",
  "## Per institution",
  "", md_tbl(as.data.frame(transmute(reports, institution, model, n_courses_reviewed,
                                     overall_assessment = str_trunc(overall_assessment, 300)))), "",
  "## All findings (ranked)",
  "", md_tbl(as.data.frame(top))
)
writeLines(report, paste0(out_stem, "_report.md"))

cat(sprintf("Aggregated %d findings from %d reports (%d verified, %d failed, %d not checkable) -> %s_report.md\n",
            nrow(findings), nrow(reports), sum(findings$verified %in% TRUE),
            sum(findings$verified %in% FALSE), sum(is.na(findings$verified)), out_stem))
if (nrow(unverified) > 0) {
  cat("\nFailed verification:\n")
  print(as.data.frame(unverified), row.names = FALSE)
}
