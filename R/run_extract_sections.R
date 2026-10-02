# run_extract_sections.R
# Section-extraction pipeline: reads html_*.RDS files, runs per-institution
# section extraction, anonymizes section text, writes data/sections_raw.RDS,
# prints diagnostics (coverage per canonical section + list of unmapped
# headings for #192).
#
#   Rscript R/run_extract_sections.R              # all institutions
#   Rscript R/run_extract_sections.R uia oslomet  # only these; their rows are
#                                                 # replaced in sections_raw.RDS

library(dplyr)

source("R/fetch_html_cols.R")       # institution_config depends on fetch_fn refs
source("R/extract_fulltext.R")      # pre/post fns used by institution_config
source("R/institution_config.R")
source("R/section_heading_map.R")
source("R/extract_sections.R")
source("R/anonymize.R")

only <- commandArgs(trailingOnly = TRUE)

cat("Loading harvested data...\n")
html_files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
if (length(only)) {
  html_files <- html_files[sub("^html_(.*)\\.RDS$", "\\1", basename(html_files)) %in% only]
}
courses_raw <- html_files |> lapply(readRDS) |> bind_rows()

# Saved RDS files still use the legacy `fulltext` column name; the pipeline's
# current name is `extracted_text`. Alias on read.
if (!"extracted_text" %in% colnames(courses_raw) &&
    "fulltext" %in% colnames(courses_raw)) {
  courses_raw$extracted_text <- courses_raw$fulltext
}

cat("Loaded", nrow(courses_raw), "course rows from",
    length(html_files), "files\n\n")

# ── Extraction ──────────────────────────────────────────────────────────────

institutions <- sort(unique(courses_raw$institution))

cat("Extracting sections per institution...\n")
sections_list <- purrr::map(institutions, function(inst) {
  df <- courses_raw |> filter(institution == inst)
  cat(sprintf("  %-8s %d courses\n", inst, nrow(df)))
  extract_sections(
    institution = inst,
    html              = df$html %||% rep(NA_character_, nrow(df)),
    extracted_text    = df$extracted_text %||% rep(NA_character_, nrow(df)),
    course_id         = df$course_id
  )
})
sections_raw <- bind_rows(sections_list)

# Sections are cut from un-anonymized html/extracted_text, so the personal data
# anonymize_text() strips from course_plan (e-mails, contact and approval
# lines) has to be stripped here too before saving (#208).
sections_raw <- sections_raw |>
  mutate(raw_text = anonymize_text(institution, raw_text,
                                   .progress = "Anonymizing sections")) |>
  filter(!is.na(raw_text))
if (length(only) && file.exists("data/sections_raw.RDS")) {
  sections_raw <- readRDS("data/sections_raw.RDS") |>
    filter(!institution %in% only) |>
    bind_rows(sections_raw)
}

saveRDS(sections_raw, "data/sections_raw.RDS")
cat(sprintf("\nSaved %d section rows to data/sections_raw.RDS\n\n",
            nrow(sections_raw)))

# ── Coverage diagnostics ────────────────────────────────────────────────────

cat("=== COVERAGE PER INSTITUTION ===\n")
cat("(% of courses with extracted_text that have each canonical section)\n\n")

canonical_sections <- setdiff(sort(unique(section_heading_patterns$section)), ".drop")

denom <- courses_raw |>
  filter(!is.na(extracted_text), nzchar(extracted_text)) |>
  count(institution, name = "n_courses")

coverage <- sections_raw |>
  distinct(institution, course_id, section) |>
  count(institution, section, name = "n_with") |>
  left_join(denom, by = "institution") |>
  mutate(pct = n_with / n_courses * 100)

for (inst in sort(unique(denom$institution))) {
  total <- denom$n_courses[denom$institution == inst]
  cat(sprintf("%-8s (%d courses with text)\n", inst, total))
  rows <- coverage |> filter(institution == inst) |> arrange(section)
  if (nrow(rows) == 0) {
    cat("  (no sections extracted)\n")
    next
  }
  for (sec in canonical_sections) {
    r <- rows |> filter(section == sec)
    pct <- if (nrow(r) == 1) sprintf("%5.1f%%", r$pct) else "    -"
    cat(sprintf("  %-28s %s\n", sec, pct))
  }
}

# ── Unmapped heading diagnostics ────────────────────────────────────────────

cat("\n=== UNMAPPED HEADING CANDIDATES ===\n")
cat("(headings the extractor encountered but couldn't map — feeds into #192)\n\n")

# Parsing every course's HTML is expensive; sample per institution instead.
# Headings repeat across courses, so a sample surfaces the same unmapped set.
SAMPLE_N <- 50

unmapped_for_institution <- function(inst, rows) {
  ic <- get_institution_config(inst)
  strategy <- ic$section_strategy
  if (is.null(strategy) || strategy %in% c("noop", "text_split", "json_nla")) {
    return(character())
  }
  htmls <- rows$html
  htmls <- htmls[!is.na(htmls) & nzchar(htmls)]
  if (length(htmls) > SAMPLE_N) {
    set.seed(42)
    htmls <- sample(htmls, SAMPLE_N)
  }
  candidates <- character()
  for (h in htmls) {
    cands <- tryCatch(
      .collect_heading_candidates(h, strategy, ic),
      error = function(e) character()
    )
    candidates <- c(candidates, cands)
  }
  unmapped <- candidates[is.na(vapply(candidates, match_heading_to_section,
                                      character(1)))]
  unmapped <- unmapped[nzchar(trimws(unmapped))]
  sort(unique(trimws(unmapped)))
}

for (inst in institutions) {
  rows <- courses_raw |> filter(institution == inst)
  if (!"html" %in% colnames(rows)) next
  cat(sprintf("  scanning %s...\n", inst))
  unmapped <- unmapped_for_institution(inst, rows)
  if (length(unmapped) == 0) next
  cat(sprintf("%-8s (%d unmapped)\n", inst, length(unmapped)))
  for (u in head(unmapped, 20)) cat("  -", u, "\n")
  if (length(unmapped) > 20) cat(sprintf("  ... and %d more\n",
                                         length(unmapped) - 20))
}

if (file.exists("data/course_offerings_full.RDS")) {
  cat("\n=== METRICS VS SNAPSHOT ===\n\n")
  source("R/pipeline_metrics.R")
  check_pipeline_metrics()
}

cat("\nDone.\n")
