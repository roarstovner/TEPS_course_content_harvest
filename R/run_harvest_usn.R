library(dplyr)

source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "usn") |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>  # Returns NA for USN
  validate_courses("with_url") |>  # Will have NA urls - that's expected
  # New step: Discover URLs through version trial-and-error
  resolve_course_urls(
    checkpoint_path = "data/checkpoint/usn_urls.RDS"
  )

# Fetch HTML for discovered URLs
df <- fetch_html_with_checkpoint(
  df,
  checkpoint_path = "data/checkpoint/html_usn.RDS"
)

# Extract fulltext
df$fulltext <- extract_fulltext(df$institution_short, df$html)

# Save results
saveRDS(df, "data/html_usn.RDS")

# Print summary
cat("\nUSN Harvest Summary:\n")
cat("Total courses:", nrow(df), "\n")
cat("URLs discovered:", sum(!is.na(df$url)), "\n")
cat("URLs not found:", sum(is.na(df$url)), "\n")
cat("HTML fetched successfully:", sum(df$html_success, na.rm = TRUE), "\n")
cat("Fulltext extracted:", sum(!is.na(df$fulltext)), "\n")
