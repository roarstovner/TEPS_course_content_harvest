# Tests for the pipeline's raw partitions and frames (R/pipeline.R):
# read_raw(content = FALSE), read_harvest() without the pages, the plans,
# sections and offerings of each frame.

rows <- function(code, year, html, semester = "Høst", inst = "x") {
  tibble::tibble(
    course_id = paste(inst, code, year, ifelse(semester == "Høst", "autumn", "spring"), 1, sep = "_"),
    institution = inst, course_code_raw = code, course_code = code, year = year,
    semester_name = semester, url = paste0("u/", code, "/", year), html = html,
    html_error = list(NULL), html_success = !is.na(html), extracted_text = NA_character_)
}

test_that("the raw store is read by partition, and without the pages by page id", {
  raw <- withr::local_tempdir()
  run <- dplyr::bind_rows(rows("A", 2024L, "<p>a24</p>"), rows("A", 2025L, NA), rows("B", 2025L, NA))
  run$extracted_text[3] <- "PDF text"   # a PDF: its text is the raw data
  run$pdf <- list(NULL, NULL, charToRaw("%PDF-1.4"))
  write_raw_run(run, "x", "2026-10-16", raw)
  write_raw_run(rows("C", 2025L, "<p>c25</p>"), "x", "2026-10-17", raw)
  expect_equal(raw_partitions(raw), tibble::tibble(institution = "x", year = c("2024", "2025")))
  expect_equal(basename(harvest_files("x", raw, year = "2025")), c("2026-10-16.parquet", "2026-10-17.parquet"))

  files <- harvest_files("x", raw)
  full <- read_raw(files, page_id = TRUE)
  slim <- read_raw(files, content = FALSE)
  expect_equal(full$page_id, c("x/2024/2026-10-16/1", "x/2025/2026-10-16/2", "x/2025/2026-10-16/3",
                               "x/2025/2026-10-17/1"))
  expect_equal(slim$html, c(full$page_id[1], NA, NA, full$page_id[4]))
  expect_equal(slim$extracted_text, c(NA, NA, full$page_id[3], NA))
  expect_named(slim, setdiff(names(full), c("pdf", "page_id")))
  expect_equal(slim[setdiff(names(slim), c("html", "extracted_text"))],
               full[setdiff(names(slim), c("html", "extracted_text"))])
})

test_that("read_harvest() without the pages gives each offering the id of the page it takes", {
  raw <- withr::local_tempdir()
  # a current site: offerings take their course's page of the academic year
  april <- rows("A", 2025L, "2025/26 plan", c("Vår", "Høst"))
  write_raw_run(april, "x", "2026-04-04", raw)
  sept <- rows("A", 2025L, "2026/27 plan", c("Vår", "Høst")); sept$harvested_at <- as.Date("2026-09-30")
  write_raw_run(sept, "x", "2026-09-30", raw)
  later <- rows("A", 2026L, "2026/27 plan, again", c("Vår", "Høst")); later$harvested_at <- as.Date("2026-10-01")
  write_raw_run(later, "x", "2026-10-01", raw)
  cfg <- list(name = "x", plan_years = "current")
  files <- harvest_files("x", raw)
  full <- read_harvest(files, cfg)
  slim <- read_harvest(files, cfg, content = FALSE)
  page <- read_raw(files, page_id = TRUE)
  expect_equal(page$html[match(slim$html, page$page_id)], full$html)
  expect_equal(slim[setdiff(names(slim), c("html", "extracted_text"))],
               full[setdiff(names(full), c("html", "extracted_text"))])
  expect_equal(offering_pages(list(slim))$page_id, slim$html)
})

test_that("each frame has the plans of its own offerings, and their sections", {
  # course A has the same plan 2023-2025; only 2024 and 2025 are teacher-ed
  pages <- list(tibble::tibble(
    course_id = c("x_A_2023_autumn_1", "x_A_2024_autumn_1", "x_A_2025_autumn_1", "x_B_2025_autumn_1"),
    institution = "x", course_code = c("A", "A", "A", "B"), year = c(2023L, 2024L, 2025L, 2025L),
    semester_name = "Høst", url = "u", html = c("x/2023/r/1", "x/2024/r/1", "x/2025/r/1", NA),
    html_error = NA_character_, html_success = TRUE))
  text <- c("Innhold\nOm emnet 2023.", "Innhold\nOm emnet 2024.", "Innhold\nOm emnet 2025.")
  page_text <- tibble::tibble(page_id = c("x/2023/r/1", "x/2024/r/1", "x/2025/r/1"),
                              extracted_text = text, plan_text = NA_character_,
                              page_plans("x", text, rep(NA_character_, 3)), page_term = NA_integer_,
                              english_pointer = FALSE)
  config <- list(list(name = "x", plan_years = "url"))
  courses <- plan_offerings(pages, page_text, config)
  expect_equal(courses$course_plan[1:3], page_text$course_plan)
  expect_equal(length(unique(courses$plan_content_id[1:3])), 1)   # years are not in the id
  expect_true(is.na(courses$plan_content_id[4]))                  # no page
  expect_equal(courses$plan_year_basis, rep("url", 4))

  frames <- list(all = rep(TRUE, 4), teacher_ed = c(FALSE, TRUE, TRUE, FALSE))
  plans <- frame_plans(courses, frames)
  expect_equal(plans$all$source_course_id, "x_A_2023_autumn_1")
  expect_equal(plans$teacher_ed$source_course_id, "x_A_2024_autumn_1")
  expect_equal(c(plans$teacher_ed$year_from, plans$teacher_ed$year_to), c(2024L, 2025L))
  expect_equal(plans$teacher_ed$course_plan, courses$course_plan[2])

  page_map <- offering_pages(pages)
  expect_equal(plan_source_pages(plans, page_map), c("x/2023/r/1", "x/2024/r/1"))
  expect_equal(partition_page_ids(c("x/2023/r/1", "x/2024/r/1", "y/2024/r/1"),
                                  tibble::tibble(institution = "x", year = "2024")), "x/2024/r/1")
  source_sections <- tibble::tibble(page_id = c("x/2023/r/1", "x/2024/r/1", "x/2024/r/1"),
                                    section = c("course_content", "course_content", "assessment"),
                                    text = c("Om emnet.", "Om emnet!", "Eksamen."))
  sections <- lapply(plans, frame_sections, source_sections, page_map, "x")
  expect_equal(sections$all$text, "Om emnet.")
  expect_equal(sections$teacher_ed$text, c("Om emnet!", "Eksamen."))
  expect_equal(sections$teacher_ed$source_course_id, rep("x_A_2024_autumn_1", 2))

  offerings <- frame_offerings(courses, frames, plans, sections)
  expect_equal(offerings$teacher_ed$course_id, courses$course_id[2:3])
  expect_identical(offerings$all, courses)

  # a page that shows a version starting after the offering's semester
  # (`page_term`): no plan; autumn 2023 keeps the version from autumn 2023,
  # autumn 2024 loses the one from spring 2025, autumn 2025 keeps spring 2025's
  page_text$page_term <- semester_term(c(2023L, 2025L, 2025L), c("Høst", "Vår", "Vår"))
  later <- plan_offerings(pages, page_text, config)
  expect_true(is.na(later$plan_content_id[2]) && is.na(later$course_plan[2]))
  expect_equal(later$extracted_text[2], text[2])
  expect_equal(later$plan_content_id[c(1, 3)], courses$plan_content_id[c(1, 3)])
  # a page that only says that the plan is on the English page (`english_page`): no plan
  page_text$page_term <- NA_integer_
  page_text$english_pointer <- c(FALSE, FALSE, TRUE)
  pointer <- plan_offerings(pages, page_text, config)
  expect_true(is.na(pointer$plan_content_id[3]) && is.na(pointer$course_plan[3]))
  expect_equal(pointer$plan_content_id[1:2], courses$plan_content_id[1:2])
})

test_that("the teacher-ed frame holds the offerings of the DBH snapshot's teacher-ed frame", {
  dir <- withr::local_tempdir()
  courses <- tibble::tibble(Institusjonskode = "1130", Årstall = 2025L, Semester = 3L,
                            Semesternavn = "Høst", Studieprogramkode = c("P1", "P2"),
                            Emnekode = c("MGL1-1", "ECON1"), Status = 2L)
  arrow::write_parquet(courses, file.path(dir, "dbh_courses_2026-10-16.parquet"))
  arrow::write_parquet(tibble::tibble(Institusjonskode = "1130", Studieprogramkode = c("P1", "P2"),
                                      Studiumkode = c("LUPE", "BACHELOR")),
                       file.path(dir, "dbh_programmes_2026-10-16.parquet"))
  # harvested with status 1: the offering is matched without it
  f <- frame_members(c("uit_MGL1-1_2025_autumn_1", "uit_ECON1_2025_autumn_2"), input_dir = dir)
  expect_equal(f, list(all = c(TRUE, TRUE), teacher_ed = c(TRUE, FALSE)))
})
