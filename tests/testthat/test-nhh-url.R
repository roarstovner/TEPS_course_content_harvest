# Tests for NHH's URL discovery: the site search (mocked), one search per
# course code, the page whose course code is the offering's.

nhh_search <- function(...) {
  hits <- vapply(list(...), function(h) paste0(
    "<li><a href='", h[1], "'><article class='articleItem coursePageTeaser'>",
    "<h1 class='listHeading'>", h[2], "</h1><div class='courseCode'>\n  ", h[3], " (7,5 Stp)\n</div>",
    "</article></a></li>"), character(1))
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body><a href='/emner2/'>Emner</a><ul>",
                                          paste(hits, collapse = ""), "</ul></body></html>")))
}

test_that("NHH's pages come from its site search, by exact course code", {
  local_robots("www.nhh.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(sub("https://www.nhh.no/sok/?q=", "", req$url, fixed = TRUE),
      # another course whose code starts with the offering's comes first
      "MRR451" = nhh_search(c("/emner/revisjon-ii2/", "Revisjon II", "MRR451-OSL"),
                            c("/emner/revisjon-ii/", "Revisjon II (N)", "MRR451")),
      # an expired page of the code after the current one
      "BAN430" = nhh_search(c("/emner/forecasting-expired/", "Forecasting (expired)", "BAN430"),
                            c("/emner/forecasting/", "Forecasting (E)", "BAN430")),
      "NBD%20MAJOR" = nhh_search(),
      "AFA2" = nhh_search(c("/emner/finansregnskap/", "Finansregnskap", "BED2")),
      httr2::response(404))
  })
  df <- tibble::tibble(course_code = c("MRR451", "BAN430", "BAN430", "NBD MAJOR", "AFA2"), year = 2025L,
                       semester_name = c("Vår", "Vår", "Høst", "Høst", "Høst"), url = NA_character_)
  out <- resolve_urls_nhh_batch(df, .progress = FALSE)
  expect_equal(out$url, c("https://www.nhh.no/emner/revisjon-ii/", "https://www.nhh.no/emner/forecasting/",
                          "https://www.nhh.no/emner/forecasting/", NA, NA))
  # each code is searched once
  expect_equal(length(sent), 4)
})

test_that("add_course_url() leaves NHH's URL to the resolver", {
  df <- tibble::tibble(institution = "nhh", course_code = "BAN430", course_code_raw = "BAN430-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("nhh")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "current")
  expect_equal(institution_from_code("1240"), "nhh")
})
