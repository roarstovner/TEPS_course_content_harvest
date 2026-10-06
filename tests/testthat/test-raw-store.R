# Tests for the append-only raw store (#296) and current sites (#293):
# harvest_all(), read_harvest(), finalize_release(), frozen_changes()

raw_rows <- function(code = "A", year = 2025L, semester = c("Vår", "Høst"), html = "plan",
                     inst = "x") {
  tibble::tibble(
    course_id = paste(inst, code, year, ifelse(semester == "Høst", "autumn", "spring"), 1, sep = "_"),
    institution = inst, Emnekode_raw = code, Årstall = year, Semesternavn = semester,
    url = paste0("u/", code), html = html, html_error = list(NULL), html_success = TRUE,
    extracted_text = html)
}

test_that("a current site's offering gets its course's page from a harvest in its academic year (#293)", {
  raw <- withr::local_tempdir()
  apr <- dated_harvest_file("x", as.Date("2026-04-04"), raw)
  dir.create(dirname(apr), recursive = TRUE)
  # the April 2026 snapshot was fetched for DBH 2025 rows, the old way (institution_short)
  saveRDS(dplyr::rename(raw_rows(html = "2025/26 plan"), institution_short = institution), apr)
  # the first harvest, 2026-09-30, fetched DBH 2025 rows and saw the 2026/27 plan
  base <- raw_rows(html = "2026/27 plan"); base$harvested_at <- as.Date("2026-09-30")
  saveRDS(base, harvest_file("x", raw))
  # a later harvest added DBH 2026 rows (fetched in autumn 2027: the 2027/28 plan)
  later <- raw_rows(year = 2026L, html = "2027/28 plan"); later$harvested_at <- as.Date("2027-10-01")
  f27 <- dated_harvest_file("x", as.Date("2027-10-01"), raw)
  dir.create(dirname(f27), recursive = TRUE); saveRDS(later, f27)

  out <- read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "current"))
  plan <- function(y, s) out$html[out$Årstall == y & out$Semesternavn == s]
  expect_equal(plan(2025, "Høst"), "2025/26 plan")   # 2025/26: April 2026
  expect_true(is.na(plan(2025, "Vår")))             # 2024/25: no harvest
  expect_equal(plan(2026, "Vår"), "2025/26 plan")   # 2025/26, a row April never saw
  expect_equal(plan(2026, "Høst"), "2026/27 plan")  # 2026/27: the September 2026 harvest
  expect_equal(nrow(out), 4)
  expect_equal(out$harvested_at[out$Årstall == 2026 & out$Semesternavn == "Høst"], as.Date("2026-09-30"))
  expect_equal(academic_year_of_date(as.Date(c("2026-07-31", "2026-08-01", NA))),
               c("2025-2026", "2026-2027", NA))
  expect_identical(academic_year_of_date(as.Date(character())), character())
})

test_that("other sites combine their raw files, and an offering in two files is an error (#296)", {
  raw <- withr::local_tempdir()
  saveRDS(raw_rows(), harvest_file("x", raw))
  f <- dated_harvest_file("x", as.Date("2027-10-01"), raw)
  dir.create(dirname(f), recursive = TRUE)
  saveRDS(raw_rows(year = 2026L), f)
  out <- read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "url"))
  expect_equal(sort(out$Årstall), c(2025, 2025, 2026, 2026))
  saveRDS(raw_rows(), f)   # the 2025 rows again
  expect_error(read_harvest(harvest_files("x", raw), list(name = "x", plan_years = "url")),
               "more than one raw file")
})

test_that("harvest_all never writes a raw file twice (#296)", {
  withr::local_dir(withr::local_tempdir())
  dbh <- function(year) tibble::tibble(
    institution = "samas", Emnekode_raw = "S-1", Emnekode = "S", Årstall = year,
    Semesternavn = c("Vår", "Høst"), Status = 1L, Avdelingsnavn = "-")
  base <- harvest_file("samas")
  suppressMessages(harvest_all(dbh(2025L), institutions = "samas"))
  expect_equal(nrow(readRDS(base)), 2)
  md5 <- tools::md5sum(base)

  # a site with the year in the URL: a later harvest adds only new offerings
  old <- institution_configs$samas$plan_years
  institution_configs$samas$plan_years <<- "url"
  withr::defer(institution_configs$samas$plan_years <<- old)
  suppressMessages(harvest_all(rbind(dbh(2025L), dbh(2026L)), institutions = "samas"))
  added <- readRDS(dated_harvest_file("samas"))
  expect_equal(added$Årstall, c(2026, 2026))
  expect_equal(tools::md5sum(base), md5)
  expect_message(harvest_all(rbind(dbh(2025L), dbh(2026L)), institutions = "samas"),
                 "nothing new")

  # a current site takes a new snapshot, but not into a file that exists
  institution_configs$samas$plan_years <<- "current"
  expect_message(harvest_all(dbh(2026L), institutions = "samas"), "not written twice")
  expect_equal(nrow(readRDS(dated_harvest_file("samas"))), 2)
  expect_equal(tools::md5sum(base), md5)
})

test_that("a finalized release is locked and checked (#296)", {
  withr::local_dir(withr::local_tempdir())
  for (d in c("data/raw", "data/processed", "tests/snapshots")) dir.create(d, recursive = TRUE)
  saveRDS(raw_rows(inst = "samas"), harvest_file("samas"))
  plans <- tibble::tibble(plan_content_id = "p1", institution = "samas", Emnekode = "S",
                          course_plan = "Tekst.")
  secs <- tibble::tibble(plan_content_id = "p1", institution = "samas", Emnekode = "S",
                         section = "course_content", text = "Tekst.")
  offs <- tibble::tibble(course_id = c("o1", "o2"), institution = "samas",
                         plan_content_id = c("p1", NA))
  saveRDS(offs, "data/processed/course_offerings.RDS")
  saveRDS(plans, "data/processed/course_plans.RDS")
  saveRDS(secs, "data/processed/plan_sections.RDS")
  check <- function() suppressWarnings(frozen_changes(
    RAW_MANIFEST, "data/processed/course_offerings.RDS", "data/processed/course_plans.RDS",
    "data/processed/plan_sections.RDS"))
  expect_equal(nrow(check()), 0)   # nothing finalized yet

  suppressMessages(finalize_release("data-test"))
  expect_equal(file.access(harvest_file("samas"), 2), c("data/raw/html_samas.RDS" = -1L))
  expect_equal(nrow(check()), 0)

  # a new offering and plan (a later year) are fine; any change to the release is not
  saveRDS(dplyr::bind_rows(plans, dplyr::mutate(plans, plan_content_id = "p2")),
          "data/processed/course_plans.RDS")
  saveRDS(dplyr::bind_rows(offs, tibble::tibble(course_id = "o3", institution = "samas",
                                                plan_content_id = "p2")),
          "data/processed/course_offerings.RDS")
  expect_equal(nrow(check()), 0)
  saveRDS(dplyr::mutate(offs, plan_content_id = c("p1", "p2")), "data/processed/course_offerings.RDS")
  saveRDS(dplyr::mutate(plans, course_plan = "Annen tekst."), "data/processed/course_plans.RDS")
  saveRDS(dplyr::mutate(secs, text = "Annen tekst."), "data/processed/plan_sections.RDS")
  Sys.chmod(harvest_file("samas"), "0644")
  saveRDS(raw_rows(inst = "samas", html = "ny"), harvest_file("samas"))
  expect_setequal(check()$check, c("raw file changed or missing", "plan gone or text changed",
                                   "section changed", "offering gone or with another plan"))
})
