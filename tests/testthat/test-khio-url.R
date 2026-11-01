# Tests for KHiO: a page per course version at /emner/{code}-{version}, and
# the empty page of a version no longer taught (mocked), which counts as not
# found.

khio_page <- function(plan) {
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"), body = charToRaw(paste0(
    "<html><body><main><section class='content'><div class='cf'><div class='col six'>",
    "<h1>Kunstfag 2</h1><div class='programme-meta'><dl><dt>Studiepoeng</dt><dd>15</dd>",
    "<dt>Emnekode</dt><dd>KK202</dd></dl></div>",
    "<h2>Kort beskrivelse av emnet</h2>\n  ", plan,
    "\n<h2>Vurdering</h2>\n  </div></div></section></main></body></html>")))
}

test_that("KHiO's URL is DBH's code with its version, in lower case", {
  df <- tibble::tibble(institution = "khio", course_code = c("KK202", "CDPR528"),
                       course_code_raw = c("KK202-4", "CDPR528-1"), year = 2025L, semester_name = "Høst")
  expect_equal(as.character(add_course_url(df)$url),
               c("https://khio.no/emner/kk202-4", "https://khio.no/emner/cdpr528-1"))
  cfg <- get_institution_config("khio")
  expect_equal(cfg$strategy, "standard")
  expect_equal(cfg$plan_years, "current")
  expect_equal(institution_from_code("6220"), "khio")
})

test_that("a KHiO page with a plan passes; the empty page of an old version is not found", {
  local_robots("khio.no")
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://khio.no/emner/kk202-4" = khio_page("<p>Emnet gir <strong>en innføring</strong> i kunstfaglig arbeid.</p>"),
    "https://khio.no/emner/kk202-3" = khio_page(""),
    httr2::response(404)))
  resp <- fetch_html_cols_single_khio("https://khio.no/emner/kk202-4")
  expect_equal(httr2::resp_status(resp), 200L)
  err <- tryCatch(fetch_html_cols_single_khio("https://khio.no/emner/kk202-3"), error = identity)
  expect_s3_class(err, "khio_not_found_error")
  expect_equal(fetch_outcome("https://khio.no/emner/kk202-3", FALSE, 200L, fetch_error_text(list(err))),
               "not_found")
})
