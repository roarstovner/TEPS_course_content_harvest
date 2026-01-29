# Test USN batch URL resolution with session reuse
#
# Tests that resolve_urls_usn_batch() correctly:
# - Finds URLs for existing courses
# - Returns NA for non-existent courses
# - Reuses browser session efficiently

test_that("USN batch resolution finds valid courses and rejects fake ones", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  # Create a small test dataframe with teacher education courses
  test_df <- tibble::tibble(
    Emnekode = c("LH-NOD1000", "MG1FOU", "FAKE999"),
    Årstall = c(2024L, 2024L, 2024L),
    Semesternavn = c("Høst", "Høst", "Høst")
  )

  result <- resolve_urls_usn_batch(test_df, .progress = TRUE)

  # LH-NOD1000 should have a URL
  expect_false(is.na(result$url[result$Emnekode == "LH-NOD1000"]),
               info = "LH-NOD1000 should have a valid URL")

  # LRFY240 (physics for teacher education) should have a URL
  expect_false(is.na(result$url[result$Emnekode == "MG1FOU"]),
               info = "MG1FOU should have a valid URL")

  # FAKE999 should be NA (doesn't exist)
  expect_true(is.na(result$url[result$Emnekode == "FAKE999"]),
              info = "FAKE999 should return NA (non-existent course)")
})

test_that("USN resolution works for specific course from courses.RDS", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  courses <- readRDS(here::here("data/courses.RDS"))

  hits <- courses |>
    filter(institution_short == "usn") |>
    add_course_id() |>
    filter(course_id %in% c("usn_MG1NO1-2_2025_autumn_1")) |>
    add_course_url() |>
    resolve_course_urls(checkpoint_path = NULL)

  # Should have resolved the URL
  expect_equal(nrow(hits), 1, info = "Should have exactly one course")
  expect_false(is.na(hits$url[1]), info = "URL should be resolved")
})
