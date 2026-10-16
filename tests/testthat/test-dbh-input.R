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
  expect_equal(te$course_code_raw, "MGL1-1")
  expect_equal(te$course_code, "MGL1")
  expect_equal(te$institution, "uit")
  # DBH's columns under the pipeline's names, in DBH's order
  expect_named(te, c("institution", "institution_code", "year", "semester", "semester_name",
                     "programme_code", "course_code_raw", "course_code", "status"))
  all <- dbh_courses(input_dir = dir)
  expect_equal(sort(all$course_code_raw), c("BIO1", "ECON1", "MGL1-1"))
  expect_equal(all$institution[all$course_code == "BIO1"], "uib")
})

test_that("dbh_courses() reads the latest snapshot unless given a date", {
  dir <- withr::local_tempdir()
  programmes <- tibble::tibble(Institusjonskode = "1130", Studieprogramkode = "P1", Studiumkode = "LUPE")
  write_snapshot(dir, "2026-10-16", dbh_rows("1130", "P1", "OLD1"), programmes)
  write_snapshot(dir, "2027-02-01", dbh_rows("1130", "P1", "NEW1"), programmes)
  expect_equal(dbh_courses(input_dir = dir)$course_code, "NEW1")
  expect_equal(dbh_courses("teacher_ed", date = "2026-10-16", input_dir = dir)$course_code, "OLD1")
  expect_error(dbh_courses(date = "2025-01-01", input_dir = dir), "No DBH courses snapshot")
})

test_that("an offering keeps the course id it was first harvested under", {
  # a later snapshot gives MGL1 2025 autumn status 3; codes can hold "_"
  known <- c("uit_MGL1-1_2025_autumn_1", "uit_A_B_2024_spring_2")
  ids <- c("uit_MGL1-1_2025_autumn_3", "uit_A_B_2024_spring_2", "uit_MGL1-1_2026_spring_1")
  expect_equal(first_course_id(ids, known), c(known, "uit_MGL1-1_2026_spring_1"))
  expect_equal(first_course_id(ids, character()), ids)
  expect_identical(first_course_id(character(), known), character())
  expect_equal(offering_key("uit_A_B_2024_spring_2"), "uit_A_B_2024_spring")
})

test_that("data/variables.csv documents every variable of the data files, with its old name", {
  v <- utils::read.csv(here::here(VARIABLES_FILE))
  expect_named(v, c("file", "variable", "old_name", "type", "source", "description"))
  expect_false(any(duplicated(v[c("file", "variable")])))
  # every DBH column, with its name in the releases up to data-2026-10-16 (DBH_COLUMNS)
  dbh <- v[v$file == "course_offerings" & v$variable %in% names(DBH_COLUMNS), ]
  expect_setequal(dbh$variable, names(DBH_COLUMNS))
  expect_equal(dbh$old_name, unname(DBH_COLUMNS[dbh$variable]))
  # the built files: these variables, in this order, of these types
  for (f in unique(v$file)) {
    path <- here::here("data/processed", paste0(f, ".RDS"))
    if (!file.exists(path)) next
    x <- readRDS(path)
    expect_equal(names(x), v$variable[v$file == f], label = f)
    expect_equal(unname(vapply(x, function(col) if (inherits(col, "Date")) "Date" else typeof(col), "")),
                 v$type[v$file == f], label = f)
  }
})
