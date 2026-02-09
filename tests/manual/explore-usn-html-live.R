# Manual exploration script for USN HTML live session handling
#
# Use this script interactively to explore and debug USN URL resolution behavior.
# For automated tests, see tests/testthat/test-usn-html-live.R

library(dplyr)

source(here::here("R/resolve_course_urls.R"))
source(here::here("R/add_course_url.R"))
source(here::here("R/utils.R"))

# Create a small test dataframe with teacher education courses
test_df <- tibble::tibble(
  Emnekode = c("LH-NOD1000", "LRFY240", "FAKE999"),
  Årstall = c(2024L, 2024L, 2024L),
  Semesternavn = c("Høst", "Høst", "Høst")
)

cat("Testing batch URL resolution with session reuse\n")
cat("================================================\n")
cat("Courses to resolve:\n")
print(test_df)
cat("\n")

start_time <- Sys.time()
result <- resolve_urls_usn_batch(test_df, .progress = TRUE)
end_time <- Sys.time()

cat("\nResults:\n")
print(result |> dplyr::select(Emnekode, Årstall, Semesternavn, url, url_version))

cat("\nTime elapsed:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")
cat("Expected: LH-NOD1000 and LRFY240 should have URLs, FAKE999 should be NA\n")

# Additional exploration with real course data
courses <- readRDS(here::here("data/courses.RDS"))

hits <- courses |>
  filter(institution_short == "usn") |>
  add_course_id() |>
  filter(
    course_id %in% c("usn_MG1NO1-2_2025_autumn_1")
  ) |>
  add_course_url() |>
  resolve_course_urls(checkpoint_path = NULL) |>
  select(course_id, institution_short, Årstall, Semesternavn, contains("url"))

cat("\nResults for MG1NO1-2:\n")
print(hits)
