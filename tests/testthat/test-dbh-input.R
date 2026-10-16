# Tests for the DBH input: dated snapshots (data-raw/courses.R), the frames
# of dbh_courses(), and course ids across snapshots

write_snapshot <- function(dir, date, courses, programmes) {
  arrow::write_parquet(courses, file.path(dir, paste0("dbh_courses_", date, ".parquet")))
  arrow::write_parquet(programmes, file.path(dir, paste0("dbh_programmes_", date, ".parquet")))
}

dbh_rows <- function(inst, prog, code, year = 2025L) {
  tibble::tibble(Institusjonskode = inst, Årstall = year, Semester = 3L, Semesternavn = "Høst",
                 Studieprogramkode = prog, Emnekode = code, Status = 1L)
}

test_that("the teacher-ed frame is the courses of teacher-ed programmes, by institution and code", {
  dir <- withr::local_tempdir()
  # "P1" is a teacher-ed programme at UiT (1130) only; MGL1 is in DBH twice
  courses <- dplyr::bind_rows(dbh_rows("1130", "P1", "MGL1-1"), dbh_rows("1130", "P1", "MGL1-1"),
                              dbh_rows("1120", "P1", "BIO1"), dbh_rows("1130", "P2", "ECON1"))
  programmes <- tibble::tibble(Institusjonskode = c("1130", "1130", "1120", "1130"),
                               Studieprogramkode = c("P1", "P1", "P1", "P2"),
                               Studiumkode = c("INTMASTER", "MASTER", "MASTER", "BACHELOR"))
  write_snapshot(dir, "2026-10-16", courses, programmes)

  te <- dbh_courses("teacher_ed", input_dir = dir)
  expect_equal(te$Emnekode_raw, "MGL1-1")
  expect_equal(te$Emnekode, "MGL1")
  expect_equal(te$institution, "uit")
  expect_equal(names(te)[1], "institution")
  all <- dbh_courses(input_dir = dir)
  expect_equal(sort(all$Emnekode_raw), c("BIO1", "ECON1", "MGL1-1"))
  expect_equal(all$institution[all$Emnekode == "BIO1"], "uib")
})

test_that("dbh_courses() reads the latest snapshot unless given a date", {
  dir <- withr::local_tempdir()
  programmes <- tibble::tibble(Institusjonskode = "1130", Studieprogramkode = "P1", Studiumkode = "LUPE")
  write_snapshot(dir, "2026-10-16", dbh_rows("1130", "P1", "OLD1"), programmes)
  write_snapshot(dir, "2027-02-01", dbh_rows("1130", "P1", "NEW1"), programmes)
  expect_equal(dbh_courses(input_dir = dir)$Emnekode, "NEW1")
  expect_equal(dbh_courses("teacher_ed", date = "2026-10-16", input_dir = dir)$Emnekode, "OLD1")
  expect_error(dbh_courses(date = "2025-01-01", input_dir = dir), "No DBH courses snapshot")
})

test_that("an offering keeps the course id it was first harvested under", {
  # a later snapshot gives MGL1 2025 autumn Status 3; codes can hold "_"
  known <- c("uit_MGL1-1_2025_autumn_1", "uit_A_B_2024_spring_2")
  ids <- c("uit_MGL1-1_2025_autumn_3", "uit_A_B_2024_spring_2", "uit_MGL1-1_2026_spring_1")
  expect_equal(first_course_id(ids, known), c(known, "uit_MGL1-1_2026_spring_1"))
  expect_equal(first_course_id(ids, character()), ids)
  expect_identical(first_course_id(character(), known), character())
  expect_equal(offering_key("uit_A_B_2024_spring_2"), "uit_A_B_2024_spring")
})
