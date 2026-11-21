# Tests for Oslo Nye Høyskole's URL discovery: the course pages from the
# sitemap (mocked), matched by the code as a part of the page's path in its
# several spellings.

test_that("ONH's pages come from its sitemap", {
  local_robots("oslonyehoyskole.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  e <- "https://oslonyehoyskole.no/"
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://oslonyehoyskole.no/sitemap.xml" = httr2::response(200, headers = list(`Content-Type` = "application/xml"),
      body = charToRaw(paste0("<urlset>", paste0("<url><loc>", e, c(
        "Emner", "Aktuelt/PSY1120-nyhet", "digital-markedsforing/emner/okad1110-bedriftsokonomi",
        "Emner/Psykologi-som-fag-og-vitenskap-PSY1120-15-sp", "emner/Industriell-organisering-OKAD1250-75sp",
        "emner/okad3110-Etikk-baerekraft-og-samfunnsansvar-15-sp", "emner/Digital-technologies-%C3%98KAD1150-75sp",
        "Emner/Bedriftsokonomi-OKAD1110-75-sp"),
        "</loc></url>", collapse = ""), "</urlset>"))),
    httr2::response(404)))
  index <- onh_course_index()
  expect_length(index, 6)   # not the list, nor a news page
  df <- tibble::tibble(course_code = c("PSY1120", "ØKAD1250", "ØKAD3110", "ØKAD1150", "ØKAD1110", "PSY112", "MB1110"))
  expect_equal(resolve_urls_onh_batch(df, index)$url, c(
    paste0(e, "Emner/Psykologi-som-fag-og-vitenskap-PSY1120-15-sp"),
    paste0(e, "emner/Industriell-organisering-OKAD1250-75sp"),       # Ø as O, credits without a dash
    paste0(e, "emner/okad3110-Etikk-baerekraft-og-samfunnsansvar-15-sp"),  # the code first, lower case
    paste0(e, "emner/Digital-technologies-%C3%98KAD1150-75sp"),      # the code with Ø
    paste0(e, "Emner/Bedriftsokonomi-OKAD1110-75-sp"),               # /Emner/ before a programme's page
    NA,                                                             # a part of a code is no match
    NA))                                                            # no page
})

test_that("add_course_url() leaves ONH's URL to the resolver", {
  df <- tibble::tibble(institution = "onh", course_code = "PSY1120", course_code_raw = "PSY1120",
                       year = 2026L, semester_name = "Vår")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("onh")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "current")
  expect_equal(institution_from_code("1407"), "onh")
})
