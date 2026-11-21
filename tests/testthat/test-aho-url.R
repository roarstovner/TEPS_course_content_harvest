# Tests for AHO's URL discovery: the course pages of both trees from the
# sitemap (mocked), matched as at PHS (vortex_match_urls()), a DBH code with
# a space at the page with a dash.

test_that("AHO's pages come from its sitemap", {
  local_robots("www.aho.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  e <- "https://www.aho.no/"
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://www.aho.no/sitemap.xml" = httr2::response(200, headers = list(`Content-Type` = "application/xml"),
      body = charToRaw(paste0("<urlset>", paste0("<url><loc>", e, c(
        "studier/emner/2025/", "studier/emner/2025/host/40-333.html", "studier/emner/2025/var/70-135.html",
        "studier/emner/2025/host/murb9.html", "english/studies/courses/2025/autumn/40-571.html",
        "english/for-students/manage-studies/course-registration/spring-2025", "om/kontakt/"),
        "</loc></url>", collapse = ""), "</urlset>"))),
    httr2::response(404)))
  index <- aho_course_index()
  expect_equal(nrow(index$pages), 4)
  df <- tibble::tibble(course_code = c("40 333", "70 135", "MURB9", "40 571", "40 333", "70 135", "PHD-SPESIAL"),
                       year = 2025L, semester_name = c("Høst", "Vår", "Vår", "Høst", "Vår", "Høst", "Høst"))
  out <- resolve_urls_aho_batch(df, index)
  expect_equal(out$course_code, df$course_code)
  expect_equal(out$url, c(
    paste0(e, "studier/emner/2025/host/40-333.html"),            # the space as a dash
    paste0(e, "studier/emner/2025/var/70-135.html"),
    NA,                                                         # spring: only an autumn page after it
    paste0(e, "english/studies/courses/2025/autumn/40-571.html"),  # English only
    NA,                                                         # likewise
    NA,                                                         # autumn: no page
    NA))                                                        # not in the sitemap
})

test_that("add_course_url() leaves AHO's URL to the resolver", {
  df <- tibble::tibble(institution = "aho", course_code = "40 333", course_code_raw = "40 333-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("aho")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "url")
  expect_equal(institution_from_code("1220"), "aho")
})
