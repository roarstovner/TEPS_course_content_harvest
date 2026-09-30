# R/scan_candidate_sections.R
# Reconnaissance (#201): how prevalent are course-plan section types that are
# NOT among the 7 canonical sections? Feeds a human decision on whether any
# (e.g. praksis) should become its own canonical section.
#
# Approach: reuse the html_headings heading-candidate collector across a sample
# of courses per institution, normalise the heading text, and count — per
# institution — how many sampled courses carry each candidate label. Reports
# coverage (% of sampled courses) so rare-but-real sections are visible.
#
# Output: data/section_review/candidate_sections.md  (+ stdout)
# Run:    Rscript R/scan_candidate_sections.R

suppressMessages({
  library(dplyr)
  library(stringr)
})

source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/institution_config.R")
source("R/section_heading_map.R")
source("R/extract_sections.R")   # .collect_heading_candidates

SAMPLE_N <- 120   # courses sampled per institution (headings repeat across courses)

# Candidate section types worth considering beyond the canonical 7. Each is a
# case-insensitive regex over the *normalised* (lower-cased) heading text.
candidates <- tibble::tribble(
  ~candidate,                ~rx,
  "praksis",                 "praksis|utplasser|placement",
  "studiepoeng_overlap",     "studiepoeng(s)?reduksjon|reduksjon i studiepoeng|overlappende emner|emneoverlapp|emnet overlapper",
  "kostnader",               "kostnad|utgifter|studieavgift",
  "arbeidsomfang",           "arbeidsomfang|arbeidsmengde|forventet arbeidsinnsats",
  "undervisningssprak",      "undervisningsspråk|undervisnings- og eksamensspråk"
)

html_files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
courses_raw <- html_files |> lapply(readRDS) |> bind_rows()
institutions <- sort(unique(courses_raw$institution))

# Per institution, collect the set of heading texts each sampled course carries,
# then test each candidate regex against that course's headings.
rows <- list()
praksis_examples <- character()

for (inst in institutions) {
  ic <- get_institution_config(inst)
  strat <- ic$section_strategy
  if (is.null(strat) || strat %in% c("noop", "text_split", "json_nla")) next
  df <- courses_raw |> filter(institution == inst, !is.na(html), nzchar(html))
  if (nrow(df) == 0) next
  if (nrow(df) > SAMPLE_N) { set.seed(42); df <- df[sample(nrow(df), SAMPLE_N), ] }
  n <- nrow(df)

  hit_counts <- setNames(integer(nrow(candidates)), candidates$candidate)
  for (h in df$html) {
    cands <- tryCatch(.collect_heading_candidates(h, strat, ic),
                      error = function(e) character())
    if (length(cands) == 0) next
    low <- str_to_lower(trimws(cands))
    for (j in seq_len(nrow(candidates))) {
      if (any(str_detect(low, candidates$rx[j]))) {
        hit_counts[j] <- hit_counts[j] + 1L
      }
    }
    # grab a couple of praksis heading examples for the report
    pk <- cands[str_detect(low, "praksis")]
    if (length(pk) && length(praksis_examples) < 12) {
      praksis_examples <- c(praksis_examples, paste0(inst, ": ", pk[1]))
    }
  }
  rows[[inst]] <- tibble(institution = inst, n_sampled = n,
                         candidate = names(hit_counts), n_hit = as.integer(hit_counts))
}

res <- bind_rows(rows) |>
  mutate(pct = round(100 * n_hit / n_sampled, 1))

# Wide table: candidate × institution coverage %.
wide <- res |>
  select(candidate, institution, pct) |>
  tidyr::pivot_wider(names_from = institution, values_from = pct)

summary_tbl <- res |>
  group_by(candidate) |>
  summarise(
    n_inst_present = sum(n_hit > 0),
    median_pct_where_present = round(median(pct[n_hit > 0]), 1),
    max_pct = max(pct),
    .groups = "drop"
  ) |>
  arrange(desc(n_inst_present))

cat("=== Candidate section prevalence (html-strategy institutions) ===\n")
print(as.data.frame(summary_tbl))
cat("\n=== Per-institution coverage %% ===\n")
print(as.data.frame(wide))
cat("\n=== Example praksis headings ===\n")
cat(paste(praksis_examples, collapse = "\n"), "\n")

saveRDS(list(res = res, wide = wide, summary = summary_tbl,
             praksis_examples = praksis_examples),
        "data/section_review/candidate_sections_scan.RDS")
cat("\nSaved scan to data/section_review/candidate_sections_scan.RDS\n")
