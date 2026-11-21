# Tests for Barratt Due's URL discovery: the course pages from the studies
# sitemap (mocked), matched by the code as a part of the path, Ø as "oe",
# the Norwegian page and the page named by the code alone first.

test_that("Barratt Due's pages come from its studies sitemap", {
  local_robots("barrattdue.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  b <- "https://barrattdue.no/studier/hoyere-utdanning/bachelor-i-utovende-musikk/"
  en <- "https://barrattdue.no/en/studier/bachelor-in-music-performance/"
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://barrattdue.no/studier-sitemap.xml" = httr2::response(200, headers = list(`Content-Type` = "application/xml"),
      body = charToRaw(paste0("<urlset>", paste0("<url><loc>", c(
        "https://barrattdue.no/studier/musikklek/", paste0(en, "hins1200/"), paste0(b, "verk2100-2025/"),
        paste0(b, "hins1200/"), paste0(b, "hoer1100/"), paste0(b, "verk2100/"), paste0(en, "arr2100/"),
        paste0(b, "valgemner/nokv5100-norske-kvinnelige-utovere-og-komponister/")),
        "</loc></url>", collapse = ""), "</urlset>"))),
    httr2::response(404)))
  index <- barrattdue_course_index()
  expect_length(index, 7)   # not the music school's pages
  df <- tibble::tibble(course_code = c("HINS1200", "HØR1100", "VERK2100", "ARR2100", "NOKV5100", "UTPR5110"))
  expect_equal(resolve_urls_barrattdue_batch(df, index)$url, c(
    paste0(b, "hins1200/"),                 # Norwegian before English
    paste0(b, "hoer1100/"),                 # Ø as oe
    paste0(b, "verk2100/"),                 # the code alone before verk2100-2025/
    paste0(en, "arr2100/"),                 # English only
    paste0(b, "valgemner/nokv5100-norske-kvinnelige-utovere-og-komponister/"),
    NA))
})

test_that("add_course_url() leaves Barratt Due's URL to the resolver", {
  df <- tibble::tibble(institution = "barrattdue", course_code = "HINS1200", course_code_raw = "HINS1200",
                       year = 2026L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("barrattdue")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "current")
  expect_equal(institution_from_code("8227"), "barrattdue")
})
