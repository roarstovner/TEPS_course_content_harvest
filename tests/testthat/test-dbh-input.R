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

# DBH's student counts (tables 905 and 900) of a snapshot: one row per course
# and semester
write_counts <- function(dir, date, exams, credits) {
  arrow::write_parquet(exams, file.path(dir, paste0("dbh_exams_", date, ".parquet")))
  arrow::write_parquet(credits, file.path(dir, paste0("dbh_credits_", date, ".parquet")))
}
counts <- function(code, year, semester, n, name) {
  df <- tibble::tibble(Institusjonskode = "1130", Emnekode = code, Årstall = as.integer(year),
                       Semester = as.integer(semester))
  df[[name]] <- n
  df
}

test_that("offered course-years: had_students is exams or credits, summed over the year", {
  dir <- withr::local_tempdir()
  exams <- counts(c("BOTH-1", "BOTH-1", "EXAM-1", "SPLIT-1", "ZERO-1", "NEW-1"),
                  c(2024, 2024, 2024, 2024, 2024, 2025), c(1, 3, 3, 1, 3, 1), c(5L, 0L, 3L, 4L, 0L, 9L),
                  "Oppmeldt totalt")
  # SPLIT: exams in spring, credits in autumn (a year-long course); a
  # course-year with no row in 900 has no credits
  credits <- counts(c("BOTH-1", "CRED-1", "SPLIT-1", "ZERO-1", "NEW-1", "LATE-1"),
                    c(2024, 2024, 2024, 2024, 2025, 2024), c(3, 1, 3, 3, 1, 3), c(1.5, 0.25, 2, 0, 1, 0.5),
                    "Ny produksjon totalt")
  write_counts(dir, "2026-11-04", exams, credits)
  o <- dbh_offered("2026-11-04", input_dir = dir)
  get <- function(code, col) o[[col]][o$course_code_raw == code]
  expect_equal(get("BOTH-1", "exam_registrations"), 5L)
  expect_equal(get("BOTH-1", "credits_earned"), 1.5)
  expect_true(get("BOTH-1", "had_students"))
  expect_equal(c(get("SPLIT-1", "exam_registrations"), get("SPLIT-1", "credits_earned")), c(4L, 2))
  expect_equal(c(get("EXAM-1", "had_students"), get("EXAM-1", "credits_earned")), c(TRUE, 0))
  expect_equal(c(get("CRED-1", "had_students"), get("CRED-1", "exam_registrations")), c(TRUE, 0L))
  expect_false(get("ZERO-1", "had_students"))
  # 2024's autumn is in both tables, 2025's is not: 2025 is provisional
  expect_equal(attr(o, "complete_through"), 2024)
  expect_equal(get("NEW-1", "counts_provisional"), TRUE)
  expect_false(any(o$counts_provisional[o$year == 2024]))
  expect_equal(attr(o, "dbh_snapshot"), "2026-11-04")
})

test_that("the student counts are a snapshot of their own, dated apart from the course list", {
  dir <- withr::local_tempdir()
  write_snapshot(dir, "2026-10-16", dbh_rows("1130", "P1", "A-1"),
                 tibble::tibble(Institusjonskode = "1130", Studieprogramkode = "P1", Studiumkode = "MASTER"))
  # two counts snapshots: the later has A-1's 2025 autumn
  write_counts(dir, "2026-11-04", counts("A-1", 2025, 1, 3L, "Oppmeldt totalt"),
               counts("A-1", 2025, 1, 1, "Ny produksjon totalt"))
  write_counts(dir, "2027-03-01", counts("A-1", 2025, c(1, 3), c(3L, 4L), "Oppmeldt totalt"),
               counts("A-1", 2025, c(1, 3), c(1, 2), "Ny produksjon totalt"))
  # the latest counts, whatever the course list's date; their own date
  # decides which years are provisional
  expect_equal(attr(dbh_courses(input_dir = dir), "dbh_snapshot"), "2026-10-16")
  o <- dbh_offered(input_dir = dir)
  expect_equal(attr(o, "dbh_snapshot"), "2027-03-01")
  expect_equal(c(o$exam_registrations, o$counts_provisional), c(7, 0))
  expect_true(dbh_offered("2026-11-04", input_dir = dir)$counts_provisional)
  # a counts snapshot needs both tables
  file.remove(file.path(dir, "dbh_credits_2027-03-01.parquet"))
  expect_error(dbh_offered(input_dir = dir), "No DBH credits snapshot")
})

test_that("dbh_fetch() asks DBH once per institution code, per course and semester, and trims", {
  calls <- list()
  local_mocked_bindings(dbh_data = function(table, filters = NULL, group_by = NULL, ...) {
    calls[[length(calls) + 1]] <<- list(table = table, filters = filters, group_by = group_by)
    tibble::tibble(Institusjonskode = filters$Institusjonskode, Emnekode = "ABC-1   ",
                   Årstall = 2024L, Semester = 1L, `Oppmeldt totalt` = 5L)
  }, .package = "rdbhapi")
  x <- dbh_fetch(905, c("1130", "1120"), wait = 0)
  expect_s3_class(x, "tbl_df")
  expect_equal(x$Institusjonskode, c("1130", "1120"))
  expect_equal(unique(x$Emnekode), "ABC-1")
  expect_equal(vapply(calls, function(x) x$filters$Institusjonskode, ""), c("1130", "1120"))
  expect_equal(calls[[1]]$group_by, c("Institusjonskode", "Emnekode", "Årstall", "Semester"))
  # no codes: the whole table, one request without filters
  calls <- list()
  dbh_fetch(208)
  expect_equal(length(calls), 1)
  expect_null(calls[[1]]$filters)
})

test_that("offerings get the flags of their course-year, by code with its version", {
  offered <- tibble::tibble(institution_code = "1130", course_code_raw = c("ABC-1", "ABC-2"), year = 2024L,
                            exam_registrations = c(5L, 0L), credits_earned = c(1.5, 0), had_students = c(TRUE, FALSE))
  attr(offered, "complete_through") <- 2024
  off <- tibble::tibble(institution_code = "1130", course_code_raw = c("ABC-1", "ABC-1", "ABC-2", "ABC-3", "ABC-1"),
                        course_code = "ABC", year = c(2024L, 2024L, 2024L, 2024L, 2025L),
                        semester_name = c("Vår", "Høst", "Vår", "Vår", "Vår"))
  x <- add_offered(off, offered)
  expect_equal(nrow(x), nrow(off))
  expect_equal(x$had_students, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(x$exam_registrations, c(5L, 5L, 0L, 0L, 0L))
  expect_equal(x$credits_earned, c(1.5, 1.5, 0, 0, 0))
  expect_equal(x$counts_provisional, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # flags already there are replaced
  expect_equal(add_offered(x, offered), x)
})

test_that("coverage over each set counts only the set's plans (methods.qmd, worked example)", {
  # ten course-years: A-F exams and credits, plans A-E; G credits only; H
  # exams only; I and J no students, a plan for I
  code <- LETTERS[1:10]
  off <- tibble::tibble(institution = "x", course_code_raw = rep(code, each = 2), year = 2024L,
                        plan_content_id = NA_character_,
                        exam_registrations = rep(ifelse(code %in% c(LETTERS[1:6], "H"), 5L, 0L), each = 2),
                        credits_earned = rep(ifelse(code %in% c(LETTERS[1:6], "G"), 1, 0), each = 2),
                        had_students = rep(code %in% LETTERS[1:8], each = 2),
                        counts_provisional = FALSE)
  # a plan in one semester of each of A-E and I
  off$plan_content_id[match(c(LETTERS[1:5], "I"), off$course_code_raw)] <- "p"
  cov <- offered_coverage(off)
  expect_equal(unlist(cov[c("n_all", "plans_all", "n_students", "plans_students", "n_both", "plans_both")]),
               c(n_all = 10, plans_all = 6, n_students = 8, plans_students = 5, n_both = 6, plans_both = 5))
  expect_equal(c(cov$cov_all, cov$cov_students, cov$cov_both), c(6 / 10, 5 / 8, 5 / 6))
  expect_false(cov$provisional)
  # a provisional year: with students as counted so far, with both empty
  off$counts_provisional <- TRUE
  cov <- offered_coverage(off)
  expect_equal(c(cov$n_students, cov$n_both), c(8, 0))
  expect_true(cov$provisional && is.na(cov$cov_both))
})
