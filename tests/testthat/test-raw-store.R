# Tests for the append-only raw store and current sites:
# write_raw_run(), read_raw(), harvest_all(), read_harvest(), finalize_release(),
# frozen_changes()

raw_rows <- function(code = "A", year = 2025L, semester = c("Vår", "Høst"), html = "plan",
                     inst = "x") {
  tibble::tibble(
    course_id = paste(inst, code, year, ifelse(semester == "Høst", "autumn", "spring"), 1, sep = "_"),
    institution = inst, course_code_raw = code, year = year, semester_name = semester,
    url = paste0("u/", code), html = html, html_error = list(NULL), html_success = TRUE,
    extracted_text = html)
}

test_that("read_raw() gives the raw files of the harvests until 2026-10-16 the pipeline's names", {
  raw <- withr::local_tempdir()
  old <- dplyr::rename(raw_rows(), institution_short = institution, Emnekode_raw = course_code_raw,
                       "Årstall" = year, Semesternavn = semester_name)
  old$html_error <- NA_character_
  old$harvest_row <- seq_len(nrow(old))
  .write_plain_parquet(old, raw_file("x", 2025L, "2026-04-04", raw))
  write_raw_run(raw_rows(year = 2026L), "x", "2026-10-17", raw)
  for (content in c(TRUE, FALSE)) {
    rows <- read_raw(harvest_files("x", raw), content = content)
    expect_named(rows, names(read_raw(harvest_files("x", raw)[2], content = content)))
    expect_equal(rows$year, c(2025L, 2025L, 2026L, 2026L))
    expect_equal(rows$institution, rep("x", 4))
  }
})

test_that("a current site's offering gets its course's page from a harvest in its academic year", {
  raw <- withr::local_tempdir()
  # the April 2026 snapshot was fetched for DBH 2025 rows, the old way
  # (institution_short, no harvested_at: the run's name gives the date)
  write_raw_run(dplyr::rename(raw_rows(html = "2025/26 plan"), institution_short = institution),
                "x", "2026-04-04", raw)
  # the harvest of 2026-09-30 fetched DBH 2025 rows and saw the 2026/27 plan
  base <- raw_rows(html = "2026/27 plan"); base$harvested_at <- as.Date("2026-09-30")
  write_raw_run(base, "x", "2026-09-30", raw)
  # a later harvest added DBH 2026 rows (fetched in autumn 2027: the 2027/28 plan)
  later <- raw_rows(year = 2026L, html = "2027/28 plan"); later$harvested_at <- as.Date("2027-10-01")
  write_raw_run(later, "x", "2027-10-01", raw)

  # a second harvest on 2026-09-30, fixing a page: the later run counts
  again <- raw_rows(html = "2026/27 plan, fixed"); again$harvested_at <- as.Date("2026-09-30")
  write_raw_run(again, "x", "2026-09-30_120000", raw)

  out <- read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "current"))
  plan <- function(y, s) out$html[out$year == y & out$semester_name == s]
  expect_equal(plan(2025, "Høst"), "2025/26 plan")   # 2025/26: April 2026
  expect_true(is.na(plan(2025, "Vår")))             # 2024/25: no harvest
  expect_equal(plan(2026, "Vår"), "2025/26 plan")   # 2025/26, a row April never saw
  expect_equal(plan(2026, "Høst"), "2026/27 plan, fixed")  # 2026/27: the September 2026 harvests
  expect_equal(nrow(out), 4)
  expect_equal(out$harvested_at[out$year == 2026 & out$semester_name == "Høst"], as.Date("2026-09-30"))
  expect_equal(academic_year_of_date(as.Date(c("2026-07-31", "2026-08-01", NA))),
               c("2025-2026", "2026-2027", NA))
  expect_identical(academic_year_of_date(as.Date(character())), character())
})

test_that("other sites combine their raw files, and an offering in two files is an error", {
  raw <- withr::local_tempdir()
  write_raw_run(raw_rows(), "x", "2026-09-30", raw)
  write_raw_run(raw_rows(year = 2026L), "x", "2027-10-01", raw)
  out <- read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "url"))
  expect_equal(sort(out$year), c(2025, 2025, 2026, 2026))
  write_raw_run(raw_rows(), "x", "2027-10-02", raw)   # the 2025 rows again
  expect_error(read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "url")),
               "more than one raw file")
})

test_that("a retry of offerings without a plan takes their new page", {
  raw <- withr::local_tempdir()
  write_raw_run(raw_rows(html = c(NA, "shell")), "x", "2026-09-30", raw)   # a 404, a page shell
  again <- raw_rows(html = c(NA, "plan")); again$retry <- TRUE
  write_raw_run(dplyr::bind_rows(again, raw_rows(code = "B")), "x", "2026-10-07", raw)  # B is new
  cfg <- list(name = "x", plan_years = "url")
  html_a <- function(out) setNames(out$html, out$semester_name)[out$course_code_raw == "A"][c("Vår", "Høst")]
  out <- read_harvest(harvest_files("x", raw), cfg)
  expect_equal(nrow(out), 4)
  expect_equal(unname(html_a(out)), c(NA, "plan"))
  expect_false("retry" %in% names(out))
  # a retry that fails keeps the earlier page
  raw2 <- withr::local_tempdir()
  write_raw_run(raw_rows(html = c(NA, "shell")), "x", "2026-09-30", raw2)
  again$html <- again$extracted_text <- NA_character_
  write_raw_run(again, "x", "2026-10-07", raw2)
  expect_equal(unname(html_a(read_harvest(harvest_files("x", raw2), cfg))), c(NA, "shell"))
})

test_that("a run is stored by year and read back in the order it was harvested", {
  raw <- withr::local_tempdir()
  rows <- dplyr::bind_rows(raw_rows(code = "B", year = 2024L), raw_rows(code = "A"),
                           raw_rows(code = "C", year = 2024L))
  rows$url <- glue::as_glue(rows$url)
  rows$html[2] <- NA                          # a PDF: its text is the raw data
  rows$extracted_text[2] <- "PDF text"
  rows$pdf <- list(NULL, charToRaw("%PDF-1.4"), NULL, NULL, NULL, NULL)
  rows$html_error[[3]] <- simpleError("HTTP 404 Not Found.")
  rows$html_error[4] <- list("Timeout")
  files <- write_raw_run(rows, "x", "2026-10-16", raw)
  expect_setequal(files, file.path(raw, "x", c("2024", "2025"), "2026-10-16.parquet"))
  out <- read_raw(harvest_files("x", raw))
  expect_equal(out$course_id, rows$course_id)
  expect_named(out, names(rows))
  expect_type(out$url, "character")
  expect_equal(out$html_error, c(NA, NA, "HTTP 404 Not Found.", "Timeout", NA, NA))
  # text from html is rebuilt from it; text without html is kept
  expect_equal(out$extracted_text, c(NA, "PDF text", NA, NA, NA, NA))
  expect_equal(out$pdf[[2]], charToRaw("%PDF-1.4"))
  expect_null(out$pdf[[1]])
  expect_false("pdf" %in% names(read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "url"))))
  expect_error(write_raw_run(rows[1, ], "x", "2026-10-16", raw), "not written twice")
  expect_equal(new_harvest_run("x", raw, as.POSIXct("2026-10-16 09:10:24")), "2026-10-16_091024")
  expect_equal(new_harvest_run("x", raw, as.POSIXct("2026-10-17 09:10:24")), "2026-10-17")
  # a run in the same second already: a second later
  write_raw_run(rows[1, ], "x", "2026-10-16_091024", raw)
  expect_equal(new_harvest_run("x", raw, as.POSIXct("2026-10-16 09:10:24")), "2026-10-16_091025")
  expect_setequal(raw_course_ids("x", raw), rows$course_id)
})

test_that("academic-year URLs: an offering takes the page of its academic year", {
  u <- function(inst, code, year, sem) add_course_url(tibble::tibble(
    institution = inst, course_code = code, course_code_raw = code, year = year, semester_name = sem,
    department_name = NA_character_))$url
  expect_equal(u("ntnu", "MGLU1101-A", 2022L, "Vår"), "https://www.ntnu.no/studier/emner/MGLU1101/2021")
  expect_equal(u("ntnu", "MGLU1101", 2022L, "Høst"), "https://www.ntnu.no/studier/emner/MGLU1101/2022")
  expect_equal(u("oslomet", "M1GSF2110", 2022L, "Vår"),
               "https://student.oslomet.no/studier/-/studieinfo/emne/M1GSF2110/2021/HØST")
  expect_equal(u("hvl", "MGX1", 2025L, "Sommer"), "https://www.hvl.no/studier/studieprogram/emne/2024/MGX1")
  # pages fetched before 2026-10-14: the calendar year for every semester
  hvl <- function(y) paste0("https://www.hvl.no/studier/studieprogram/emne/", y, "/MGX1")
  rows <- tibble::tibble(
    course_id = c("a23", "a24", "s24", "s25", "a25"), institution = "hvl", course_code = "MGX1",
    course_code_raw = "MGX1-1", department_name = NA_character_, year = c(2023L, 2024L, 2024L, 2025L, 2025L),
    semester_name = c("Høst", "Høst", "Vår", "Vår", "Høst"),
    url = hvl(c(2023, 2024, 2024, 2025, 2025)),
    html = c(NA, "2024/25", "2024/25 again", "2025/26", "2025/26 own"),
    html_error = list("HTTP 404", NULL, NULL, NULL, NULL), html_success = c(FALSE, TRUE, TRUE, TRUE, TRUE))
  out <- .academic_year_pages(rows)
  expect_equal(out$html, c(NA, "2024/25", NA, "2024/25", "2025/26 own"))
  expect_equal(out$url, hvl(c(2023, 2024, 2023, 2024, 2025)))
  expect_equal(out$html_error[[1]], "HTTP 404")   # its own URL: the 404 stays
  expect_null(out$html_error[[3]])                # /2023 for spring 2024: not fetched
})

test_that("menu URLs (uis): an offering takes the page of its academic year", {
  pdf <- function(y) paste0("https://www6.uis.no/Stine/EMNE/", y, "/VÅR/Bokmål/MGL1_1.pdf")
  page <- "https://www.uis.no/nb/student/course/MGL1_1"
  # pages fetched before 2026-10-14: the menu entry of the calendar year
  rows <- tibble::tibble(
    course_id = c("a23", "s24", "s25", "a25", "s26", "s16"), institution = "uis", course_code = "MGL1",
    course_code_raw = "MGL1-1", department_name = NA_character_,
    year = c(2023L, 2024L, 2025L, 2025L, 2026L, 2016L),
    semester_name = c("Høst", "Vår", "Vår", "Høst", "Vår", "Vår"),
    url = c(pdf(c(2023, 2024, 2025, 2025)), page, page),
    html = NA_character_, html_error = list(NULL),
    extracted_text = c("2023-2024", "2024-2025", "2025-2026", NA, "current", "current"))
  out <- .menu_year_pages(rows)
  # spring 2024 reads 2023/24 (fetched for autumn 2023), spring 2025 2024/25
  # (fetched for spring 2024); autumn 2025's own fetch gave nothing, so it
  # takes the same PDF fetched for spring 2025; spring 2026 takes 2025/26
  # over the course page; spring 2016's year was never fetched: course page
  expect_equal(out$extracted_text,
               c("2023-2024", "2023-2024", "2024-2025", "2025-2026", "2025-2026", "current"))
  expect_equal(out$url, c(pdf(c(2023, 2023, 2024, 2025, 2025)), page))
  # spring 2023's 2022/23 PDF was never fetched: no page, at the course page
  s23 <- .menu_year_pages(dplyr::filter(rows, course_id == "s24") |> dplyr::mutate(year = 2023L))
  expect_equal(c(s23$url, s23$extracted_text), c(page, NA))
})

test_that("nord: the academic year of the plan a page shows, as its autumn", {
  py <- get_institution_config("nord")$page_term
  expect_equal(py(c('<div class="h4 pre-title">Gjeldende emnebeskrivelse (sist oppdatert 2026/27)</div>',
                    "<div>Emnebeskrivelse for 2023/24</div>", "<p>Ingen</p>")),
               semester_term(c(2026L, 2023L, NA), "Høst"))
  # the English page (a course taught in English, `english_page`)
  expect_equal(py(c("<div>Current active course description (last updated 2026/27)</div>",
                    "<div>Course description for 2025/26</div>")), semester_term(c(2026L, 2025L), "Høst"))
})

test_that("inn: the start semester the page shows", {
  py <- get_institution_config("inn")$page_term
  rl <- function(s) sprintf('<span class="readinglist-start-semester" data-readinglist-start-semester="%s"></span>', s)
  # Norwegian also on the English page
  expect_equal(py(c(rl("2025 Haust"), rl("2027 Vår"), rl("2024 Høst"), "<p>Emnesøket</p>", NA)),
               c(4051L, 4054L, 4049L, NA, NA))
  # spring after autumn, summer with spring
  expect_equal(semester_term(c(2024L, 2025L, 2025L, 2025L), c("Høst", "Vår", "Sommer", "Høst")),
               c(4049L, 4050L, 4050L, 4051L))
})

test_that("uis: the academic year selected in the course page's menu, as its autumn", {
  py <- get_institution_config("uis")$page_term
  menu <- paste0('<option value="https://www6.uis.no/Stine/EMNE/2017/H&Oslash;ST/B/X_1.pdf" selected>\n',
                 '          2017 - 2018\n        </option>\n',
                 '<option value="https://www6.uis.no/Stine/EMNE/2016/H&Oslash;ST/B/X_1.pdf">\n',
                 '          2016 - 2017\n        </option>')
  expect_equal(py(c(menu, "<p>Fakta</p>", NA)), c(4035L, NA, NA))
})

test_that("harvest_all(retry = TRUE) fetches offerings without a plan again", {
  withr::local_dir(withr::local_tempdir())
  old <- institution_configs$samas$plan_years
  institution_configs$samas$plan_years <<- "url"
  withr::defer(institution_configs$samas$plan_years <<- old)
  dbh <- tibble::tibble(institution = "samas", course_code_raw = c("S-1", "T-1"),
                        course_code = c("S", "T"), year = 2025L, semester_name = "Høst",
                        status = 1L, department_name = "-")
  suppressMessages(harvest_all(dbh, institutions = "samas"))
  ids <- read_raw(harvest_files("samas"))$course_id
  dir.create("data/interim")
  saveRDS(tibble::tibble(course_id = ids, institution = "samas", url = NA_character_,
                         plan_content_id = c(NA, "p1")),
          "data/interim/course_offerings_full.RDS")
  expect_message(harvest_all(dbh, institutions = "samas"), "nothing new")
  suppressMessages(harvest_all(dbh, institutions = "samas", retry = TRUE))
  again <- read_raw(harvest_files("samas")[2])   # a second run the same day
  expect_match(harvest_files("samas")[2], "/samas/2025/\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.parquet$")
  expect_equal(again$course_id, ids[1])
  expect_true(again$retry)
})

test_that("harvest_all never writes a raw file twice", {
  withr::local_dir(withr::local_tempdir())
  dbh <- function(year) tibble::tibble(
    institution = "samas", course_code_raw = "S-1", course_code = "S", year = year,
    semester_name = c("Vår", "Høst"), status = 1L, department_name = "-")
  suppressMessages(harvest_all(dbh(2025L), institutions = "samas"))
  base <- harvest_files("samas")
  expect_equal(nrow(read_raw(base)), 2)
  md5 <- tools::md5sum(base)

  # a site with the year in the URL: a later harvest adds only new offerings
  old <- institution_configs$samas$plan_years
  institution_configs$samas$plan_years <<- "url"
  withr::defer(institution_configs$samas$plan_years <<- old)
  suppressMessages(harvest_all(rbind(dbh(2025L), dbh(2026L)), institutions = "samas"))
  added <- read_raw(setdiff(harvest_files("samas"), base))
  expect_equal(added$year, c(2026, 2026))
  expect_equal(tools::md5sum(base), md5)
  expect_message(harvest_all(rbind(dbh(2025L), dbh(2026L)), institutions = "samas"),
                 "nothing new")

  # a current site takes a new snapshot of fresh pages; harvested today
  # already, so into a new file named by the time too
  institution_configs$samas$plan_years <<- "current"
  checkpoint_append(tibble::tibble(url = "u", html = "old page"), checkpoint_dir("samas", "html", 2026))
  saveRDS(tibble::tibble(), "data/raw/checkpoint/html_samas.RDS")   # until 2026-10-28: not read, kept
  before <- tools::md5sum(harvest_files("samas"))
  expect_message(harvest_institution("samas", dbh(2026L)), "Checkpoints removed: data/raw/checkpoint/samas")
  expect_false(dir.exists("data/raw/checkpoint/samas"))
  expect_true(file.exists("data/raw/checkpoint/html_samas.RDS"))
  suppressMessages(harvest_all(dbh(2026L), institutions = "samas"))
  files <- harvest_files("samas")
  expect_length(files, 3)
  expect_match(files[3], "/samas/2026/\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.parquet$")
  expect_equal(nrow(read_raw(files[3])), 2)
  expect_equal(tools::md5sum(files[1:2]), before)
})

test_that("a finalized release is locked and checked", {
  withr::local_dir(withr::local_tempdir())
  processed <- frame_dir("teacher_ed")   # a release is of the teacher-ed frame
  for (d in c("data/raw", processed, "tests/snapshots")) dir.create(d, recursive = TRUE)
  f <- write_raw_run(raw_rows(inst = "samas"), "samas", "2026-10-01")
  lf <- write_fetch_ledger(fetch_ledger(raw_rows(inst = "samas"), "samas", "2026-10-01"),
                           "samas", "2026-10-01")
  plans <- tibble::tibble(plan_content_id = "p1", institution = "samas", course_code = "S",
                          course_plan = "Tekst.")
  secs <- tibble::tibble(plan_content_id = "p1", institution = "samas", course_code = "S",
                         section = "course_content", text = "Tekst.")
  offs <- tibble::tibble(course_id = c("o1", "o2"), institution = "samas",
                         plan_content_id = c("p1", NA))
  saveRDS(offs, file.path(processed, "course_offerings.RDS"))
  saveRDS(plans, file.path(processed, "course_plans.RDS"))
  saveRDS(secs, file.path(processed, "plan_sections.RDS"))
  check <- function() suppressWarnings(frozen_changes(
    RAW_MANIFEST, file.path(processed, "course_offerings.RDS"), file.path(processed, "course_plans.RDS"),
    file.path(processed, "plan_sections.RDS")))
  expect_equal(nrow(check()), 0)   # nothing finalized yet

  suppressMessages(finalize_release("data-test"))
  expect_equal(file.access(f, 2), c("data/raw/samas/2025/2026-10-01.parquet" = -1L))
  expect_equal(unname(file.access(lf, 2)), -1L)   # the fetch ledger too
  expect_equal(utils::read.csv(RAW_MANIFEST)$file, c(f, lf))
  expect_equal(nrow(check()), 0)
  # a release with DBH's column names (up to data-2026-10-16) is compared by value
  kept <- file.path("data/releases/data-test", c("course_plans.RDS", "plan_sections.RDS"))
  Sys.chmod(kept, "0644")
  for (k in kept) saveRDS(dplyr::rename(readRDS(k), Emnekode = course_code), k)
  expect_equal(nrow(check()), 0)

  # a new offering and plan (a later year) are fine; any change to the release is not
  saveRDS(dplyr::bind_rows(plans, dplyr::mutate(plans, plan_content_id = "p2")),
          file.path(processed, "course_plans.RDS"))
  saveRDS(dplyr::bind_rows(offs, tibble::tibble(course_id = "o3", institution = "samas",
                                                plan_content_id = "p2")),
          file.path(processed, "course_offerings.RDS"))
  expect_equal(nrow(check()), 0)
  saveRDS(offs[1, ], file.path(processed, "course_offerings.RDS"))   # o2 (no plan) gone
  saveRDS(dplyr::mutate(plans, course_plan = "Annen tekst."), file.path(processed, "course_plans.RDS"))
  saveRDS(dplyr::mutate(secs, text = "Annen tekst."), file.path(processed, "plan_sections.RDS"))
  Sys.chmod(f, "0644")
  arrow::write_parquet(raw_rows(inst = "samas", html = "ny")[, c("course_id", "html")], f)
  expect_setequal(check()$check, c("raw file changed or missing", "plan gone or text changed",
                                   "section changed", "offering gone or with another plan"))
})

test_that("section_changes() lists section fixes under ids both releases hold", {
  plan <- function(id) tibble::tibble(plan_content_id = id, institution = "samas", course_code = "S")
  sec <- function(id, section, text) dplyr::mutate(plan(id), section = section, text = text)
  was <- dplyr::bind_rows(sec("p1", "prerequisites", "Opptak til lektorutdanning."),
                          sec("p1", "assessment", "Mappe."), sec("p2", "assessment", "Eksamen."),
                          sec("p3", "assessment", "Gammel."))
  now <- dplyr::bind_rows(sec("p1", "assessment", "Mappe."), sec("p2", "assessment", "Hjemmeeksamen."),
                          sec("p2", "coursework_requirements", "Arbeidskrav."),
                          sec("p4", "assessment", "Ny."))   # p3 renamed p4: in the crosswalk
  res <- section_changes(was, now, plan(c("p1", "p2", "p3")), plan(c("p1", "p2", "p4")))
  expect_equal(paste(res$plan_content_id, res$section, res$change),
               c("p1 prerequisites removed", "p2 assessment changed", "p2 coursework_requirements added"))
})

test_that("a harvest run has a date only from its rows or its name", {
  raw <- withr::local_tempdir()
  expect_error(harvest_date("first"), "No harvest date")
  run <- c("2026-09-30", "2026-10-15_091024", "2026-10-15_091024")
  expect_equal(harvest_date(run), as.Date(c("2026-09-30", "2026-10-15", "2026-10-15")))
  expect_equal(harvest_date(run, as.Date(c(NA, "2026-10-16", NA))),
               as.Date(c("2026-09-30", "2026-10-16", "2026-10-16")))
  write_raw_run(raw_rows(), "x", "2026-09-30", raw)
  f <- write_raw_run(raw_rows(inst = "y"), "y", "2026-09-30", raw)
  dir.create(file.path(raw, "checkpoint"))   # not an institution
  saveRDS(tibble::tibble(), file.path(raw, "checkpoint", "html_x.RDS"))
  expect_equal(harvested_institutions(raw), c("x", "y"))
  expect_equal(harvest_files("y", raw), f)
})

test_that("hiof: the semester in the page title, so a later plan is dropped", {
  py <- get_institution_config("hiof")$page_term
  h1 <- function(s) sprintf("<h1>LSKNO22215 Litteraturhistorie (5.-10.) (%s)</h1>", s)
  expect_equal(py(c(h1("Vår 2020"), h1("Høst 2023"), "<h1>LSKNO22215 Uten semester</h1>", NA)),
               c(4040L, 4047L, NA, NA))
})
