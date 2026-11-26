# Tests for Fjellhaug's URL discovery: the course pages from the subject
# sitemaps (mocked), matched by code without DBH's campus and variant
# suffixes, the Norwegian page before the English.

fjellhaug_xml <- function(...) {
  httr2::response(200, headers = list(`Content-Type` = "application/xml"),
                  body = charToRaw(paste0("<urlset>", paste0("<url><loc>", c(...), "</loc></url>", collapse = ""),
                                          "</urlset>")))
}

test_that("Fjellhaug's pages come from its subject sitemaps", {
  local_robots("fih.fjellhaug.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  r <- "https://fih.fjellhaug.no"
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(req$url,
      "https://fih.fjellhaug.no/sitemaps-1-sitemap.xml" = fjellhaug_xml(
        paste0(r, "/sitemaps-1-section-news-1-sitemap.xml"), paste0(r, "/sitemaps-1-section-subjects-1-sitemap.xml"),
        paste0(r, "/sitemaps-1-section-studies-1-sitemap.xml")),
      "https://fih.fjellhaug.no/en/sitemaps-1-sitemap.xml" = fjellhaug_xml(
        paste0(r, "/en/sitemaps-1-section-subjects-2-sitemap.xml")),
      "https://fih.fjellhaug.no/sitemaps-1-section-subjects-1-sitemap.xml" = fjellhaug_xml(
        paste0(r, "/emne/krle1001n"), paste0(r, "/emne/btm2008"),
        paste0(r, "/emne/btm2003n-markusevangeliet-10-ects-stp")),
      "https://fih.fjellhaug.no/en/sitemaps-1-section-subjects-2-sitemap.xml" = fjellhaug_xml(
        paste0(r, "/en/subjects/btm2003n-markusevangeliet-10-ects-stp"), paste0(r, "/en/subjects/bmm1001")),
      httr2::response(404))
  })
  index <- fjellhaug_course_index()
  expect_setequal(index, c(paste0(r, "/emne/", c("krle1001n", "btm2008", "btm2003n-markusevangeliet-10-ects-stp")),
                           paste0(r, "/en/subjects/", c("btm2003n-markusevangeliet-10-ects-stp", "bmm1001"))))
  expect_false(any(grepl("news|studies", sent)))

  df <- tibble::tibble(course_code = c("KRLE1001N", "BTM2008-DK1", "BTM2008-DK2", "BTM2003N", "BMM1001-IS",
                                       "MET501", "BTM2008N"),
                       url = NA_character_)
  out <- resolve_urls_fjellhaug_batch(df, index)
  expect_equal(out$url, c(
    paste0(r, "/emne/krle1001n"),
    paste0(r, "/emne/btm2008"),                               # the Danish campuses: the code without suffix
    paste0(r, "/emne/btm2008"),
    paste0(r, "/emne/btm2003n-markusevangeliet-10-ects-stp"), # the slug with the title; Norwegian before English
    paste0(r, "/en/subjects/bmm1001"),                        # English only
    NA,                                                       # no page
    NA))                                                      # another course (N: online), not BTM2008
})

test_that("a Fjellhaug slug of a code and a number serves the code in its page's title", {
  # The sitemap lists /emne/btm1003-2 (RLE1003N's page) before /emne/btm1003
  # (checked 2026-11-25)
  local_robots("fih.fjellhaug.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  r <- "https://fih.fjellhaug.no/emne/"
  page <- function(title) httr2::response(200, headers = list(`Content-Type` = "text/html"),
                                          body = charToRaw(paste0("<html><head><title>", title, "</title></head></html>")))
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(req$url,
      "https://fih.fjellhaug.no/emne/btm1003-2" = page("Fjellhaug - RLE1003N Kristen tro, etikk og livstolkning"),
      "https://fih.fjellhaug.no/emne/rle1001-2" = page("Fjellhaug - RLE1001N Innføring i Bibelen"),
      httr2::response(404))
  })
  index <- paste0(r, c("btm1003-2", "btm1003", "rle1001-2", "rle1001", "btm2003n-markusevangeliet-10-ects-stp",
                       "sje1011-sjelesorg-livsfaser-og-livshendelser-15-stp-ects", "gre1002-3"))
  df <- tibble::tibble(course_code = c("BTM1003", "BTM1003-DK1", "RLE1001", "RLE1003N", "RLE1003N-DK1", "RLE1001N",
                                       "BTM2003N", "SJE1011", "GRE1002"),
                       url = NA_character_)
  out <- resolve_urls_fjellhaug_batch(df, index)
  expect_equal(out$url, c(paste0(r, c("btm1003", "btm1003", "rle1001", "btm1003-2", "btm1003-2", "rle1001-2",
                                      "btm2003n-markusevangeliet-10-ects-stp",
                                      "sje1011-sjelesorg-livsfaser-og-livshendelser-15-stp-ects")),
                          NA))                              # a copy whose page does not answer serves no code
  expect_setequal(sent, paste0(r, c("btm1003-2", "rle1001-2", "gre1002-3")))  # only the copies are requested
})

test_that("add_course_url() leaves Fjellhaug's URL to the resolver", {
  df <- tibble::tibble(institution = "fjellhaug", course_code = "KRLE1001N", course_code_raw = "KRLE1001N-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("fjellhaug")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "current")
  expect_equal(institution_from_code("8234"), "fjellhaug")
})
