# Test USN URL resolution
#
# Tests resolve_course_urls() with year verification
# Key test: Ensure that when a URL redirects to a different year, it's detected
# and the URL is rejected (returns NA instead of wrong year's content)

test_that("usn_page_term reads the semester of a Norwegian or English page", {
  expect_equal(usn_page_term("Planen gjelder for:\n\n     Undervisningsstart høst 2025\n"),
               list(year = 2025L, sem = "høst"))
  # a course taught in English (MAR4020 autumn 2022, MET9000 spring 2025)
  expect_equal(usn_page_term("Applies to:\n\n     Course start autumn 2022\n"),
               list(year = 2022L, sem = "høst"))
  expect_equal(usn_page_term("Applies to:\n     Course start spring 2025"),
               list(year = 2025L, sem = "vår"))
  # an old cohort plan ("Kull 2017 HØST") or an empty page: no semester
  expect_equal(usn_page_term("Planen gjelder for:\n     Kull 2017 HØST"),
               list(year = NA_integer_, sem = NA_character_))
  expect_true(grepl(USN_NOT_READY, "Note: The course plan for this semester is not available yet."))
  expect_true(grepl(USN_NOT_READY, "MERK: Emneplanen for dette semesteret er ikke klar ennå."))
})

test_that("USN URL resolution handles LH-NOD1000 across multiple years", {
  skip_on_ci()
  skip_if_not(interactive(), "Manual test - requires browser")

  # Load actual course data and filter for LH-NOD1000 test cases
  courses <- dbh_courses("teacher_ed", input_dir = here::here(INPUT_DIR))

  test_data <- courses |>
    filter(
      institution == "usn",
      course_code == "LH-NOD1000",
      year %in% c(2024, 2022, 2018),
      semester_name == "Høst"
    )

  expect_gt(nrow(test_data), 0)

  df <- test_data |>
    add_course_id() |>
    add_course_url()

  # After add_course_url(), USN URLs should be NA (requires discovery)
  expect_true(all(is.na(df$url)), info = "USN URLs should be NA before resolution")

  # Resolve URLs
  df <- df |>
    resolve_course_urls(checkpoint_path = NULL, .progress = TRUE)

  # Expected results:
  # - 2024 Høst: Should find valid URL (version 1)
  # - 2022 Høst: Should return NA (redirects to 2025)
  # - 2018 Høst: Should find valid URL (version 1)
  df_2024 <- df |> filter(year == 2024)
  df_2022 <- df |> filter(year == 2022)
  df_2018 <- df |> filter(year == 2018)

  if (nrow(df_2024) > 0) {
    expect_false(is.na(df_2024$url[1]),
                 info = "2024 Høst should have valid URL")
  }

  if (nrow(df_2022) > 0) {
    expect_true(is.na(df_2022$url[1]),
                info = "2022 Høst should return NA (redirects to wrong year)")
  }

  if (nrow(df_2018) > 0) {
    expect_false(is.na(df_2018$url[1]),
                 info = "2018 Høst should have valid URL")
  }
})
