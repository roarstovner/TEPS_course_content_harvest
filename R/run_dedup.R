# run_dedup.R
# Deduplication pipeline: combines harvested data and identifies duplicate course plans

library(dplyr)

# Source required functions
source("R/anonymize.R")
source("R/normalize_plan_text.R")
source("R/deduplicate_plans.R")

# Load all harvested HTML files
cat("Loading harvested data...\n")
html_files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)

courses_raw <- html_files |>
  lapply(readRDS) |>
  bind_rows()

cat("Loaded", nrow(courses_raw), "course rows from", length(html_files), "files\n\n")

# Anonymize extracted_text -> course_plan
cat("Anonymizing extracted text...\n")
courses_raw$course_plan <- anonymize_fulltext(
  courses_raw$institution_short,
  courses_raw$extracted_text
)

# Run deduplication (normalizes course_plan and builds plan IDs)
cat("Running deduplication...\n")
result <- deduplicate_plans(courses_raw)

# Save outputs
saveRDS(result$plans, "data/plan_lookup.RDS")
saveRDS(result$courses, "data/courses_with_plan_id.RDS")

cat("Saved outputs:\n")
cat("  - data/plan_lookup.RDS\n")
cat("  - data/courses_with_plan_id.RDS\n\n")

# Print summary statistics
cat("=== DEDUPLICATION SUMMARY ===\n\n")

total_courses <- nrow(result$courses)
total_plans <- nrow(result$plans)
overall_dedup_ratio <- 1 - (total_plans / total_courses)

cat("Total course rows:", total_courses, "\n")
cat("Total unique plans:", total_plans, "\n")
cat("Overall dedup ratio:", sprintf("%.2f%%", overall_dedup_ratio * 100), "\n\n")

# Per-institution summary
cat("=== PER-INSTITUTION BREAKDOWN ===\n\n")

inst_summary <- result$courses |>
  group_by(institution_short) |>
  summarise(
    rows = n(),
    unique_plans = n_distinct(plan_content_id),
    .groups = "drop"
  ) |>
  mutate(
    dedup_pct = (1 - unique_plans / rows) * 100
  ) |>
  arrange(institution_short)

for (i in seq_len(nrow(inst_summary))) {
  cat(sprintf(
    "%s: %d rows, %d unique plans, %.1f%% dedup\n",
    inst_summary$institution_short[i],
    inst_summary$rows[i],
    inst_summary$unique_plans[i],
    inst_summary$dedup_pct[i]
  ))
}

cat("\nDeduplication complete.\n")
