library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "uib") |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

df <- fetch_html_with_checkpoint(
  df,
  checkpoint_path = "data/checkpoint/html_uib.RDS"
)

df$fulltext <- extract_fulltext(df$institution_short, df$html)

saveRDS(df, "data/html_uib.RDS")

# Print summary
cat("\nUiB Harvest Summary:\n")
cat("Total courses:", nrow(df), "\n")
cat("URLs generated:", sum(!is.na(df$url)), "\n")
cat("HTML fetched successfully:", sum(df$html_success, na.rm = TRUE), "\n")
cat("Fulltext extracted:", sum(!is.na(df$fulltext)), "\n")
