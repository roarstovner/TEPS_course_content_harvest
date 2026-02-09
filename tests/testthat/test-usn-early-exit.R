# Test early exit criteria for USN URL resolution
#
# This test verifies two optimization scenarios:
# 1. Course code not in content -> version doesn't exist, stop checking
# 2. Version 1's teaching start > requested year -> course didn't exist yet

# Helper to check a single version and return diagnostic info
check_version <- function(session, course_code, version, year, semester) {
  sem <- toupper(semester)
  if (sem == "VAR") sem <- "VÅR"
  if (sem == "HOST") sem <- "HØST"

  hash <- glue("#/emne/{course_code}_{version}_{year}_{sem}")
  full_url <- paste0("https://www.usn.no/studier/studie-og-emneplaner/", hash)

  session$session$Runtime$evaluate(
    sprintf("window.location.hash = '%s';", hash)
  )
  Sys.sleep(3)

  content <- read_usn_live_html(session)

  result <- list(
    version = version,
    url = full_url,
    content_length = nchar(content %||% ""),
    course_code_found = FALSE,
    teaching_start_year = NA_integer_,
    teaching_start_sem = NA_character_
  )

  if (!is.null(content)) {
    result$course_code_found <- str_detect(content, fixed(course_code))

    pattern <- "Undervisningsstart\\s+(høst|vår)\\s+(\\d{4})"
    match <- str_match(content, regex(pattern, ignore_case = TRUE))
    if (!is.na(match[1])) {
      result$teaching_start_year <- as.integer(match[3])
      result$teaching_start_sem <- tolower(match[2])
    }
  }

  result
}

test_that("Early exit: course code not found means version doesn't exist", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  session <- read_html_live(base_url)
  Sys.sleep(5)

  # LH-NOD1000 2022 Høst - we know version 1 exists but versions 2+ don't
  v1 <- check_version(session, "LH-NOD1000", 1, 2022, "HØST")
  v2 <- check_version(session, "LH-NOD1000", 2, 2022, "HØST")

  session$session$close()

  # Version 1 should have the course code
  expect_true(v1$course_code_found, info = "Version 1 should contain course code")

  # Version 2 should NOT have the course code (it doesn't exist)
  expect_false(v2$course_code_found, info = "Version 2 should NOT contain course code")
})

test_that("Early exit: teaching start year > requested year means course didn't exist", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  session <- read_html_live(base_url)
  Sys.sleep(5)

  # LH-NOD1000 2022 Høst - version 1 shows 2025, not 2022
  v1 <- check_version(session, "LH-NOD1000", 1, 2022, "HØST")

  session$session$close()

  # Version 1 exists and has course code
  expect_true(v1$course_code_found, info = "Version 1 should contain course code")

  # But teaching start is 2025, which is > 2022
  expect_true(
    !is.na(v1$teaching_start_year) && v1$teaching_start_year > 2022,
    info = sprintf("Teaching start (%s) should be > requested year (2022)",
                   v1$teaching_start_year)
  )
})

test_that("Verify: valid course returns correct version", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  session <- read_html_live(base_url)
  Sys.sleep(5)

  # LRFY240 2019 Høst - known to have valid URL at version 1
  v1 <- check_version(session, "LRFY240", 1, 2019, "HØST")

  session$session$close()

  expect_true(v1$course_code_found, info = "Version 1 should contain course code")
  expect_equal(v1$teaching_start_year, 2019, info = "Teaching start should be 2019")
  expect_equal(v1$teaching_start_sem, "høst", info = "Teaching start should be høst")
})
