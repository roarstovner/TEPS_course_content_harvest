# Tests for Forsvarets høgskole's URL discovery: each code's page (mocked)
# lists its plan versions, and an offering takes the one in force.

fhs_versions <- function(...) {
  a <- paste0("<li><a href='", c(...), "'><span>x</span></a></li>", collapse = "")
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body><a href='/utdanning'>x</a>",
                                          "<ul class='dropdown-menu' aria-labelledby='toggleSemesters'>",
                                          a, "</ul></body></html>")))
}

test_that("FHS offerings take the version of the plan in force", {
  local_robots("www.forsvaret.no")
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(sub("https://www.forsvaret.no/utdanning/emner/", "", req$url, fixed = TRUE),
      "MILM1701" = fhs_versions("2025-HØST", "2023-HØST", "2021-HØST"),
      "MILM1211" = fhs_versions("2025-VÅR", "2024-VÅR"),
      httr2::response(404))
  })
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  df <- tibble::tibble(course_code = c("MILM1701", "MILM1701", "MILM1701", "MILM1701", "MILM1211", "TM3041"),
                       year = c(2025L, 2025L, 2024L, 2021L, 2024L, 2019L),
                       semester_name = c("Høst", "Vår", "Høst", "Vår", "Høst", "Høst"))
  out <- resolve_urls_fhs_batch(df, .progress = FALSE)
  e <- "https://www.forsvaret.no/utdanning/emner/"
  expect_equal(out$url, c(
    paste0(e, "MILM1701/2025-H%C3%98ST"),
    paste0(e, "MILM1701/2023-H%C3%98ST"),   # the version from the autumn before
    paste0(e, "MILM1701/2023-H%C3%98ST"),   # no version for 2024: the earlier one in force
    NA,                                      # before the first version
    paste0(e, "MILM1211/2024-V%C3%85R"),
    NA))                                     # a code without pages: 404
  expect_equal(sum(endsWith(sent, "MILM1701")), 1)   # each code's page read once
})

test_that("add_course_url() leaves FHS's URL to the resolver", {
  df <- tibble::tibble(institution = "fhs", course_code = "MILM1701", course_code_raw = "MILM1701-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("fhs")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "url")
  expect_equal(institution_from_code("1303"), "fhs")
})
