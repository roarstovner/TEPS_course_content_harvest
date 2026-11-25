# Tests for the data dictionary of a release: data/codes.csv, the Data
# Package descriptor (datapackage.json) and the Parquet copies

codes_here <- function() release_codes(here::here(CODES_FILE), here::here(STUDY_TYPES_FILE))

# Three small tables with their variables and codes
fixture <- function() {
  variables <- data.frame(
    file = c("course_offerings", "course_offerings", "course_offerings", "course_offerings",
             "course_plans", "course_plans", "plan_sections", "plan_sections", "plan_sections"),
    variable = c("course_id", "semester", "plan_id", "harvested_at", "plan_id", "credits",
                 "plan_id", "section", "text"),
    type = c("character", "integer", "character", "Date", "character", "double",
             "character", "character", "character"),
    source = "derived", description = "d")
  codes <- data.frame(variable = c("semester", "semester", "section", "section"),
                      code = c("1", "3", "assessment", "reading_list"), label = c("Spring", "Autumn", "A", "R"),
                      source = c("DBH", "DBH", "project", "project"), description = "")
  tables <- list(
    course_offerings = tibble::tibble(course_id = c("a", "b", "c"), semester = c(1L, 3L, 1L),
                                      plan_id = c("p1", NA, "p2"), harvested_at = as.Date("2026-11-01") + 0:2),
    course_plans = tibble::tibble(plan_id = c("p1", "p2"), credits = c(10, 7.5)),
    plan_sections = tibble::tibble(plan_id = c("p1", "p1", "p2"),
                                   section = c("assessment", "reading_list", "assessment"), text = "t"))
  list(variables = variables, codes = codes, tables = tables)
}

test_that("write_datapackage() writes Parquet files and a v2 descriptor that reads back", {
  x <- fixture()
  dest <- withr::local_tempdir()
  paths <- write_datapackage(x$tables, dest, "data-2026-11-25", "teacher_ed", x$variables, x$codes)
  expect_true(all(file.exists(paths)))
  desc <- jsonlite::read_json(file.path(dest, "datapackage.json"))
  expect_equal(desc$`$schema`, "https://datapackage.org/profiles/2.0/datapackage.json")
  expect_equal(desc$name, "teps-course-plans-teacher-ed")
  expect_equal(desc$version, "data-2026-11-25")
  r <- desc$resources[[1]]
  expect_equal(r$path, "course_offerings.parquet")
  expect_equal(r$hash, unname(tools::md5sum(file.path(dest, "course_offerings.parquet"))))
  # a key of one field is still an array; integer codes stay integers
  expect_equal(r$schema$primaryKey, list("course_id"))
  expect_equal(r$schema$foreignKeys[[1]]$reference, list(resource = "course_plans", fields = list("plan_id")))
  sem <- r$schema$fields[[2]]
  expect_equal(sem$type, "integer")
  expect_equal(sem$constraints$enum, list(1L, 3L))
  expect_equal(sem$categories[[2]], list(value = 3L, label = "Autumn"))
  expect_equal(r$schema$fields[[4]]$type, "date")
  expect_equal(desc$resources[[3]]$schema$primaryKey, list("plan_id", "section"))
  # the Parquet files hold the tables, and match the descriptor read back
  back <- lapply(stats::setNames(nm = names(x$tables)), function(n)
    tibble::as_tibble(arrow::read_parquet(file.path(dest, paste0(n, ".parquet")))))
  expect_equal(back, x$tables)
  expect_identical(check_datapackage(desc, back), character())
})

test_that("check_datapackage() finds wrong types, unknown codes and broken keys", {
  x <- fixture()
  desc <- datapackage_descriptor("t", "all", x$variables, x$codes)
  bad <- x$tables
  bad$course_offerings$semester <- c(1L, 2L, 1L)
  bad$course_offerings$course_id[2] <- "a"
  bad$course_plans$credits <- as.character(bad$course_plans$credits)
  bad$plan_sections$plan_id[3] <- "p9"
  p <- check_datapackage(desc, bad)
  expect_length(p, 4)
  expect_match(p, "semester: values not in the codes: 2", all = FALSE, fixed = TRUE)
  expect_match(p, "course_offerings: primary key", all = FALSE, fixed = TRUE)
  expect_match(p, "credits: character, not number", all = FALSE, fixed = TRUE)
  expect_match(p, "plan_sections: 1 row(s) with plan_id not in course_plans", all = FALSE, fixed = TRUE)
  bad$course_plans <- bad$course_plans[2:1]
  expect_match(check_datapackage(desc, bad), "course_plans: columns", all = FALSE)
  expect_error(write_datapackage(bad, withr::local_tempdir(), "t", "all", x$variables, x$codes),
               "do not match the descriptor")
})

test_that("data/codes.csv labels coded variables of the published files, once each", {
  codes <- codes_here()
  v <- utils::read.csv(here::here(VARIABLES_FILE))
  expect_false(any(duplicated(codes[c("variable", "code")])))
  expect_true(all(codes$variable %in% v$variable))
  expect_true(all(codes$source %in% c("DBH", "project")))
  expect_false(any(codes$label == "" | is.na(codes$label)))
  # the study types come from study_types.csv, not codes.csv
  expect_false(any(c("study_type_code", "study_type_group") %in%
                     utils::read.csv(here::here(CODES_FILE))$variable))
  # integer variables have integer codes
  int <- codes$variable %in% v$variable[v$type == "integer"]
  expect_false(anyNA(suppressWarnings(as.integer(codes$code[int]))))
})

test_that("the built teacher-ed files match the descriptor: types, codes and keys", {
  files <- here::here(frame_dir("teacher_ed"), paste0(names(RELEASE_KEYS), ".RDS"))
  skip_if_not(all(file.exists(files)), "built data not available")
  desc <- datapackage_descriptor("test", "teacher_ed", utils::read.csv(here::here(VARIABLES_FILE)), codes_here())
  tables <- stats::setNames(lapply(files, readRDS), names(RELEASE_KEYS))
  expect_identical(check_datapackage(desc, tables), character())
})
