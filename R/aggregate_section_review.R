# R/aggregate_section_review.R
# Merge per-institution review-agent findings (data/section_review/findings/*.json)
# into one ranked table + markdown report (issue #200, stage A).
#
# Each input JSON conforms to section_review_findings_schema.md. This script is
# tolerant of missing/extra fields so a single malformed agent output does not
# sink the aggregation.
#
# Outputs:
#   data/section_review/findings_all.RDS    one row per finding
#   data/section_review/findings_report.md  ranked human report (-> chainlink)
#
# Run:  Rscript R/aggregate_section_review.R

suppressMessages({
  library(dplyr)
  library(stringr)
})

find_dir <- "data/section_review/findings"
files <- list.files(find_dir, pattern = "\\.json$", full.names = TRUE)
if (length(files) == 0) stop("No findings JSON in ", find_dir,
                             " — run the review agents first.")

`%|%` <- function(a, b) ifelse(is.na(a), b, a)   # NA-coalesce
SEV_ORDER  <- c(high = 1, medium = 2, low = 3)
PREV_ORDER <- c(widespread = 1, common = 2, occasional = 3, rare = 4)
chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else
  paste(unlist(x), collapse = "; ")

rows <- list()
for (f in files) {
  obj <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                  error = function(e) { warning("Bad JSON: ", f, " (", e$message, ")");
                                        NULL })
  if (is.null(obj)) next
  inst <- obj$institution_short %||% str_remove(basename(f), "\\.json$")
  fnd <- obj$findings %||% list()
  for (x in fnd) {
    rows[[length(rows) + 1]] <- tibble(
      institution_short     = inst,
      section               = chr(x$section),
      error_type            = chr(x$error_type),
      severity              = chr(x$severity),
      prevalence            = chr(x$prevalence),
      example_course_ids    = chr(x$example_course_ids),
      evidence              = chr(x$evidence),
      description           = chr(x$description),
      root_cause_hypothesis = chr(x$root_cause_hypothesis),
      suggested_fix         = chr(x$suggested_fix),
      confidence            = chr(x$confidence)
    )
  }
}

findings <- bind_rows(rows) |>
  mutate(.sev = SEV_ORDER[severity] %|% 9L,
         .prev = PREV_ORDER[prevalence] %|% 9L) |>
  arrange(.sev, .prev, institution_short) |>
  select(-.sev, -.prev)

saveRDS(findings, "data/section_review/findings_all.RDS")

# ── Report ───────────────────────────────────────────────────────────────────
by_type <- findings |> count(error_type, severity, sort = TRUE)
by_sec  <- findings |> count(section, sort = TRUE)
n_inst  <- n_distinct(findings$institution_short)

esc <- function(x) str_replace_all(x %|% "", "\\|", "\\\\|") |> str_squish()
md_tbl <- function(df) {
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  body <- apply(df, 1, \(r) paste(esc(as.character(r)), collapse = " | "))
  paste(c(paste0("| ", hdr, " |"), paste0("| ", sep, " |"),
          paste0("| ", body, " |")), collapse = "\n")
}

top <- findings |>
  transmute(institution_short, section, error_type, severity, prevalence,
            example = str_trunc(example_course_ids, 60),
            suggested_fix = str_trunc(suggested_fix, 140))

report <- c(
  "# Section-extraction review — aggregated agent findings",
  "",
  sprintf("%d findings across %d institutions (from %d agent reports).",
          nrow(findings), n_inst, length(files)),
  "Sorted by severity then prevalence. Source: `R/aggregate_section_review.R`.",
  "",
  "## By error type",
  "", md_tbl(as.data.frame(by_type)), "",
  "## By section",
  "", md_tbl(as.data.frame(by_sec)), "",
  "## All findings (ranked)",
  "", md_tbl(as.data.frame(top)), "",
  "## Next step",
  "",
  "Cross-check against `data/sections_qa_report.md` (deterministic flags):",
  "agreement raises confidence; agent-only findings are the long tail the",
  "heuristics missed. Feed high/medium findings into the extractor-fix issue (#198)."
)
writeLines(report, "data/section_review/findings_report.md")

cat(sprintf("Aggregated %d findings from %d reports -> data/section_review/findings_report.md\n",
            nrow(findings), length(files)))
print(as.data.frame(by_type), row.names = FALSE)
