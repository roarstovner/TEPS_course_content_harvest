# run_extract_sections.R
# Section-extraction pipeline: reads html_*.RDS files, runs per-institution
# section extraction, writes data/sections_raw.RDS, prints diagnostics
# (coverage per canonical section + list of unmapped headings for #192).

library(dplyr)

source("R/fetch_html_cols.R")       # institution_config depends on fetch_fn refs
source("R/extract_fulltext.R")      # pre/post fns used by institution_config
source("R/institution_config.R")
source("R/section_heading_map.R")
source("R/extract_sections.R")

cat("Loading harvested data...\n")
html_files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
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

institutions <- sort(unique(courses_raw$institution_short))

cat("Extracting sections per institution...\n")
sections_list <- purrr::map(institutions, function(inst) {
  df <- courses_raw |> filter(institution_short == inst)
  cat(sprintf("  %-8s %d courses\n", inst, nrow(df)))
  extract_sections(
    institution_short = inst,
    html              = df$html %||% rep(NA_character_, nrow(df)),
    extracted_text    = df$extracted_text %||% rep(NA_character_, nrow(df)),
    course_id         = df$course_id
  )
})
sections_raw <- bind_rows(sections_list)

saveRDS(sections_raw, "data/sections_raw.RDS")
cat(sprintf("\nSaved %d section rows to data/sections_raw.RDS\n\n",
            nrow(sections_raw)))

# ── Coverage diagnostics ────────────────────────────────────────────────────

cat("=== COVERAGE PER INSTITUTION ===\n")
cat("(% of courses with extracted_text that have each canonical section)\n\n")

canonical_sections <- sort(unique(section_heading_patterns$section))

denom <- courses_raw |>
  filter(!is.na(extracted_text), nzchar(extracted_text)) |>
  count(institution_short, name = "n_courses")

coverage <- sections_raw |>
  distinct(institution_short, course_id, section) |>
  count(institution_short, section, name = "n_with") |>
  left_join(denom, by = "institution_short") |>
  mutate(pct = n_with / n_courses * 100)

for (inst in sort(unique(denom$institution_short))) {
  total <- denom$n_courses[denom$institution_short == inst]
  cat(sprintf("%-8s (%d courses with text)\n", inst, total))
  rows <- coverage |> filter(institution_short == inst) |> arrange(section)
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
  rows <- courses_raw |> filter(institution_short == inst)
  if (!"html" %in% colnames(rows)) next
  cat(sprintf("  scanning %s...\n", inst))
  unmapped <- unmapped_for_institution(inst, rows)
  if (length(unmapped) == 0) next
  cat(sprintf("%-8s (%d unmapped)\n", inst, length(unmapped)))
  for (u in head(unmapped, 20)) cat("  -", u, "\n")
  if (length(unmapped) > 20) cat(sprintf("  ... and %d more\n",
                                         length(unmapped) - 20))
}

cat("\nDone.\n")
