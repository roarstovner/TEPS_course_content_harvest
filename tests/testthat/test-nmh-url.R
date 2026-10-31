# Tests for NMH's URL discovery: the study handbooks' course lists (mocked),
# read from the study-plan sitemaps, and the handbook of the offering's
# academic year, else the latest earlier one.

nmh_html <- function(body) {
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body><nav><a href='https://nmh.no/studier'>Studier</a></nav>",
                                          "<main id='main'>", body, "</main></body></html>")))
}
nmh_list <- function(list_url, ...) {
  items <- vapply(list(...), function(h) paste0("<li><a href='", list_url, "/", h[1], "'>", h[2], "</a></li>"),
                  character(1))
  nmh_html(paste0("<a href='", sub("/[^/]+$", "", list_url), "'>Startkull</a><ul>",
                  paste(items, collapse = ""), "</ul>"))
}
nmh_xml <- function(...) {
  httr2::response(200, headers = list(`Content-Type` = "application/xml"),
                  body = charToRaw(paste0("<urlset>", paste0("<url><loc>", c(...), "</loc></url>", collapse = ""),
                                          "</urlset>")))
}

test_that("NMH's pages come from its handbooks' course lists, by cohort", {
  local_robots("nmh.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  hb <- "https://nmh.no/studiehandboker/startkull-"
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(req$url,
      "https://nmh.no/sitemaps-1-sitemap.xml" = nmh_xml("https://nmh.no/sitemaps-1-section-news-1-sitemap.xml",
                                                        "https://nmh.no/sitemaps-1-section-studyplans_content-1-sitemap-p1.xml"),
      "https://nmh.no/sitemaps-1-section-studyplans_content-1-sitemap-p1.xml" = nmh_xml(
        paste0(hb, "2024/emner"), paste0(hb, "2024/emner/akkompagnement"), paste0(hb, "2024/studieprogram"),
        paste0(hb, "2025/emner"), paste0(hb, "2025/emner/akkompagnement"),
        paste0(hb, "2025/valgemner-2024-25"), paste0(hb, "2025/valgemner-2024-25/alexanderteknikk")),
      "https://nmh.no/studiehandboker/startkull-2024/emner" = nmh_list(
        paste0(hb, "2024/emner"), c("akkompagnement", "AKKOMP10 - Akkompagnement"),
        c("studioproduksjon", "MUSTEK22 - Studioproduksjon"),
        # a page for two codes, and a code with a lower-case letter
        c("barokkensemble", "BARENS20/21 - Barokkensemble I og II"), c("instrument", "FRINST10a/B - Instrument")),
      "https://nmh.no/studiehandboker/startkull-2025/emner" = nmh_list(
        paste0(hb, "2025/emner"), c("akkompagnement", "AKKOMP10 - Akkompagnement"),
        # two pages of a code: the first listed
        c("analyse", "ANPRAK20 - Analyse i praksis"), c("analyse-2", "ANPRAK20 - Analyse i praksis"),
        c("emne-1", "Emne 1 - Fellesfaglig kunstnerisk forankring")),
      "https://nmh.no/studiehandboker/startkull-2025/valgemner-2024-25" = nmh_list(
        paste0(hb, "2025/valgemner-2024-25"), c("valgemnekatalogen", "Valgemne-katalogen - Valgemnekatalogen"),
        c("alexanderteknikk", "ALEX20 - Alexanderteknikk")),
      httr2::response(404))
  })
  df <- tibble::tibble(
    course_code = c("AKKOMP10", "AKKOMP10", "AKKOMP10", "MUSTEK22", "BARENS21", "FRINST10B", "ANPRAK20",
                    "ALEX20", "ALEX20", "BLENS23"),
    year = c(2025L, 2026L, 2025L, 2026L, 2024L, 2024L, 2025L, 2025L, 2024L, 2025L),
    semester_name = c("Vår", "Vår", "Høst", "Høst", "Høst", "Høst", "Høst", "Høst", "Høst", "Høst"),
    url = NA_character_)
  out <- resolve_urls_nmh_batch(df)
  expect_equal(out$url, c(
    paste0(hb, "2024/emner/akkompagnement"),           # spring 2025: the 2024 cohort's handbook
    paste0(hb, "2025/emner/akkompagnement"),
    paste0(hb, "2025/emner/akkompagnement"),
    paste0(hb, "2024/emner/studioproduksjon"),         # in no later handbook: the latest earlier one
    paste0(hb, "2024/emner/barokkensemble"),
    paste0(hb, "2024/emner/instrument"),
    paste0(hb, "2025/emner/analyse"),
    paste0(hb, "2025/valgemner-2024-25/alexanderteknikk"),
    NA,                                                # only in a later handbook
    NA))                                               # in no handbook
  # the sitemaps and the three lists, each once; no course page
  expect_equal(length(sent), 5)
  expect_named(out, names(df))
})

test_that("NMH offerings before the first handbook ask nothing", {
  httr2::local_mocked_responses(function(req) stop("no request expected: ", req$url))
  df <- tibble::tibble(course_code = "AKKOMP10", year = c(2020L, 2021L), semester_name = c("Høst", "Vår"),
                       url = NA_character_)
  expect_equal(resolve_urls_nmh_batch(df)$url, c(NA_character_, NA_character_))
})

test_that("add_course_url() leaves NMH's URL to the resolver", {
  df <- tibble::tibble(institution = "nmh", course_code = "AKKOMP10", course_code_raw = "AKKOMP10-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("nmh")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "url")
  expect_equal(institution_from_code("1210"), "nmh")
})
