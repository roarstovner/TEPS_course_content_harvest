# Tests for the data files of the course plan browser (R/browser_data.R,
# app/browser/README.md "Data Contract").

browser_fixture <- function() {
  off <- function(code, year, semester, programme, plan, students = TRUE, inst = "x") {
    tibble::tibble(
      course_id = paste(inst, code, year, semester, programme, sep = "_"), institution = inst,
      institution_code = "0001", institution_name = paste("Inst", year), department_name = "Dept",
      year = year, semester = semester, programme_code = programme, programme_name = paste("Prog", programme),
      course_code = code, course_name = paste(code, year), level_code = "LN", credits = 15,
      nus_code = "1", language_code = "NOR", subject_code = "S", subject_name = "Subject",
      plan_year_basis = "url", plan_content_id = plan, had_students = students)
  }
  offerings <- dplyr::bind_rows(
    # B 2024: two semesters x two programmes, one plan: one course-year
    off("B", 2024L, 1L, "P1", "pb1", FALSE), off("B", 2024L, 3L, "P1", "pb1", FALSE),
    off("B", 2024L, 1L, "P2", "pb1", TRUE), off("B", 2024L, 3L, "P2", "pb1", FALSE),
    # B 2025: a new plan in the autumn
    off("B", 2025L, 1L, "P1", "pb1"), off("B", 2025L, 3L, "P1", "pb2"),
    # A 2025: no plan
    off("A", 2025L, 3L, "P1", NA, FALSE))
  plans <- tibble::tibble(
    plan_content_id = c("pb2", "pb1"), institution = "x", course_code = "B",
    year_from = c(2025L, 2024L), year_to = 2025L,
    source_course_id = c("x_B_2025_3_P1", "x_B_2024_1_P1"), course_plan = c("Plan two", "Plan one"))
  sections <- tibble::tibble(
    plan_content_id = c("pb2", "pb2", "pb1", "pb1"), institution = "x", course_code = "B",
    source_course_id = NA_character_, section = c("learning_outcomes", "course_content", "course_content", "assessment"),
    text = c("out 2", "content 2", "content 1", "exam 1"))
  sources <- offerings |>
    dplyr::transmute(course_id, url = paste0("https://x.no/", course_id), harvested_at = as.Date("2026-01-01"),
                     extracted_text = "Kontakt: ola@x.no", html = "<p>raw</p>")
  programmes <- tibble::tibble(
    Institusjonskode = "0001", Studieprogramkode = c("P1", "P1", "P2", "P2", "P1"),
    Årstall = c(2024L, 2024L, 2024L, 2025L, 2025L), Studiumkode = c("GLU", "GLU", "LEKTOR", "LEKTOR", "GLU"),
    Studnavn = c("Grunnskole", "Grunnskole", "Lektor", "Lektor", "Grunnskole"))
  gaps <- tibble::tibble(institution = "x", course_code = c("A", "B", "B"), year = c(2025L, 2024L, 2025L),
                         has_plan = c(FALSE, TRUE, TRUE), reason = c("404", "plan", "plan"))
  list(plans = plans, sections = sections, offerings = offerings, sources = sources,
       programmes = programmes, gaps = gaps, version = "test 1", frame = "teacher_ed")
}

test_that("browser_tables() follows the data contract", {
  tb <- do.call(browser_tables, browser_fixture())
  expect_named(tb$courses, c("institution", "course_code", "course_name", "level_code", "credits",
                             "language_code", "subject_code", "subject_name", "nus_code",
                             "department_name", "year_first", "year_last"))
  expect_named(tb$course_years, c("institution", "course_code", "year", "programme_code", "study_type",
                                  "plan_content_id", "had_students", "no_plan_reason"))
  expect_named(tb$programmes, c("institution", "programme_code", "programme_name", "study_type"))
  expect_named(tb$plans, c("plan_content_id", "institution", "course_code", "year_from", "year_to",
                           "source_url", "harvested_at", "course_plan"))
  expect_named(tb$sections, c("plan_content_id", "institution", "course_code", "section", "text"))

  # courses: attributes of the latest offering
  expect_equal(tb$courses$course_code, c("A", "B"))
  expect_equal(tb$courses$course_name, c("A 2025", "B 2025"))
  expect_equal(tb$courses$year_first, c(2025L, 2024L))

  # course-years: once per programme and plan, not per DBH row
  cy <- tb$course_years
  expect_equal(nrow(cy), 1 + 2 + 2)
  expect_equal(nrow(dplyr::distinct(cy, institution, course_code, year, programme_code, plan_content_id)), nrow(cy))
  b24 <- dplyr::filter(cy, course_code == "B", year == 2024L)
  expect_equal(b24$programme_code, c("P1", "P2"))
  expect_equal(b24$study_type, c("GLU", "LEKTOR"))
  expect_true(all(b24$had_students))            # any of the course-year's rows
  expect_equal(dplyr::filter(cy, course_code == "B", year == 2025L)$plan_content_id, c("pb1", "pb2"))
  expect_equal(cy$no_plan_reason, c("404", NA, NA, NA, NA))
  expect_false(cy$had_students[cy$course_code == "A"])

  # plans sorted by course and first year, sections in that order
  expect_equal(tb$plans$plan_content_id, c("pb1", "pb2"))
  expect_equal(tb$plans$source_url, c("https://x.no/x_B_2024_1_P1", "https://x.no/x_B_2025_3_P1"))
  expect_equal(tb$sections$text, c("content 1", "exam 1", "out 2", "content 2"))

  # meta
  expect_equal(tb$meta$counts$course_years, 3)
  expect_equal(tb$meta$counts$course_years_with_plan, 2)
  expect_equal(tb$meta$institutions$institution_name, "Inst 2025")
  expect_equal(tb$meta$study_types$name, c("Grunnskole", "Lektor"))
  expect_true(all(tb$sections$section %in% tb$meta$sections$code))
})

test_that("write_browser_data() writes no raw text and stops on personal data", {
  out <- withr::local_tempdir()
  tb <- do.call(browser_tables, browser_fixture())
  paths <- write_browser_data(tb, out)
  expect_setequal(basename(paths), c("meta.json", paste0(c("courses", "course_years", "programmes",
                                                           "plans", "sections"), ".parquet")))
  cols <- unlist(lapply(grep("parquet$", paths, value = TRUE), function(p) names(arrow::read_parquet(p))))
  expect_false(any(c("extracted_text", "html", "course_plan_normalized") %in% cols))
  expect_equal(jsonlite::read_json(file.path(out, "meta.json"))$version, "test 1")

  tb$plans$course_plan[1] <- "Kontakt: ola.nordmann@uit.no"
  out2 <- file.path(withr::local_tempdir(), "data")
  expect_error(write_browser_data(tb, out2), "personal data.*plans\\$course_plan \\(email")
  expect_false(dir.exists(out2))
})
