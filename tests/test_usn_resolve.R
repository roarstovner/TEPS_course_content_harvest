# Test script for USN URL resolution
# This tests the new resolve_course_urls() functionality

library(dplyr)

source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")

# Load test data or create a small sample
if (file.exists("data/test_courses.RDS")) {
  courses <- readRDS("data/test_courses.RDS")
} else if (file.exists("data/courses.RDS")) {
  courses <- readRDS("data/courses.RDS") |>
    filter(institution_short == "usn") |>
    slice(1:3)  # Just test with 3 courses
} else {
  stop("No test data found. Please create data/test_courses.RDS or data/courses.RDS")
}

# Test the workflow
df <- courses |>
  filter(institution_short == "usn") |>
  add_course_id() |>
  add_course_url()

cat("After add_course_url():\n")
cat("URLs are NA:", all(is.na(df$url)), "\n\n")

# Try resolving URLs
df <- df |>
  resolve_course_urls(
    checkpoint_path = "data/checkpoint/test_usn_urls.RDS"
  )

cat("After resolve_course_urls():\n")
cat("Sample results:\n")
print(df |> select(Emnekode, Årstall, Semesternavn, url, url_version))

cat("\nSummary:\n")
cat("Total courses:", nrow(df), "\n")
cat("URLs found:", sum(!is.na(df$url)), "\n")
cat("URLs not found:", sum(is.na(df$url)), "\n")

if (sum(!is.na(df$url)) > 0) {
  cat("\nExample URLs:\n")
  print(df |> filter(!is.na(url)) |> select(Emnekode, url) |> head(3))
}
