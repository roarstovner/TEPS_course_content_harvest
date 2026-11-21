# Tests for DMMH: a page per course and semester at /nb/emne/{CODE}/{year}-{host|var},
# the course search for a code without any plan (mocked), which counts as
# not found, and the version a page shows (`page_term`).

dmmh_page <- function(facts) {
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"), body = charToRaw(paste0(
    "<html><body><section id='main-content'><header><h1 id='page-title'>BLSRL1010 Samfunn 1</h1></header>",
    "<div id='content'>", facts, "<h2 class='label'>Emneinnhold</h2><div class='item'><p>Emnet omfatter ...</p></div>",
    "</div></section></body></html>")))
}

test_that("DMMH's URL is DBH's code with year and semester, from autumn 2014", {
  df <- tibble::tibble(institution = "dmmh", course_code = c("BLSRL1010", "BUPRA300", "BPBUL1000", "BPBUL1000"),
                       course_code_raw = paste0(c("BLSRL1010", "BUPRA300", "BPBUL1000", "BPBUL1000"), "-1"),
                       year = c(2025L, 2025L, 2014L, 2014L), semester_name = c("Høst", "Vår", "Høst", "Vår"))
  expect_equal(as.character(add_course_url(df)$url),
               c("https://studier.dmmh.no/nb/emne/BLSRL1010/2025-host", "https://studier.dmmh.no/nb/emne/BUPRA300/2025-var",
                 "https://studier.dmmh.no/nb/emne/BPBUL1000/2014-host", NA))
  cfg <- get_institution_config("dmmh")
  expect_equal(cfg$strategy, "standard")
  expect_equal(cfg$plan_years, "url")
  expect_equal(institution_from_code("8224"), "dmmh")
})

test_that("a DMMH page passes; the course search it redirects to is not found", {
  local_robots("studier.dmmh.no")
  facts <- paste0("<ul id='facts'><li><div class='facts-label'>Undervisningssemester</div>",
                  "<div class=\"facts-item\">2025 høst, 2026 vår</div></li></ul>")
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://studier.dmmh.no/nb/emne/BLSRL1010/2025-host" = dmmh_page(facts),
    "https://studier.dmmh.no/nb/emne/MATE2010/2017-host" = dmmh_page("<div class='views-row'>MATE2010</div>"),
    httr2::response(404)))
  fetch <- get_institution_config("dmmh")$fetch_fn
  resp <- fetch("https://studier.dmmh.no/nb/emne/BLSRL1010/2025-host")
  expect_equal(httr2::resp_status(resp), 200L)
  err <- tryCatch(fetch("https://studier.dmmh.no/nb/emne/MATE2010/2017-host"), error = identity)
  expect_s3_class(err, "dmmh_not_found_error")
  expect_equal(fetch_outcome("https://studier.dmmh.no/nb/emne/MATE2010/2017-host", FALSE, 200L,
                             fetch_error_text(list(err))), "not_found")
})

test_that("a DMMH page's version starts in its first teaching semester", {
  term <- get_institution_config("dmmh")$page_term
  item <- function(label, value) paste0("<div class=\"facts-label\">", label, "</div><div class=\"facts-item\">",
                                        value, "</div>")
  expect_equal(term(item("Undervisningssemester", "2025 høst, 2026 vår")), semester_term(2025L, "Høst"))
  expect_equal(term(item("Undervisningssemester", "2026 vår")), semester_term(2026L, "Vår"))
  expect_equal(term(item("Teaching semester", "2026 autumn")), semester_term(2026L, "Høst"))
  expect_true(is.na(term("<p>no facts</p>")))
})
