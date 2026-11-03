# Tests for Politihøgskolen's URL discovery: its year folders' listings
# (mocked) through vortex_course_index(), as HiMolde's (test-himolde-url.R),
# without department folders.

phs_links <- function(...) {
  a <- paste0("<a href='", c(...), "'>x</a>", collapse = "")
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body>", a, "</body></html>")))
}

test_that("PHS's pages come from its year folders in both trees", {
  local_robots("www.politihogskolen.no")
  institution_configs$phs$request_delay <<- 0
  withr::defer(institution_configs$phs$request_delay <<- NULL)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(paste0("/", sub("https://www.politihogskolen.no/", "", req$url, fixed = TRUE)),
      "/studier/emner/" = phs_links("2023/", "2023/host/", "2024/", "2025/"),
      "/studier/emner/2024/" = phs_links("host/", "var/"),
      "/studier/emner/2024/host/" = phs_links("patrulje01.html", "avhorutd.html", "elkrise.html", "ledlerutv.html", "?vrtx=feed"),
      "/studier/emner/2024/host/?page=2" = phs_links("patrulje01.html"),
      "/studier/emner/2024/var/" = phs_links(),
      "/studier/emner/2025/" = phs_links("host/", "var/"),
      "/studier/emner/2025/host/" = phs_links("forebygg01-2.html"),
      "/studier/emner/2025/host/?page=2" = phs_links(),
      "/studier/emner/2025/var/" = phs_links("etterforsk01.html"),
      "/studier/emner/2025/var/?page=2" = phs_links(),
      "/english/studies/courses/" = phs_links("2025/"),
      "/english/studies/courses/2025/" = phs_links("spring/ncfi861-1.html"),
      "/english/studies/courses/2025/?page=2" = phs_links(),
      httr2::response(404))
  })
  df <- tibble::tibble(course_code = c("PATRULJE01", "ETTERFORSK01", "FOREBYGG01-2", "AVHØRUTD", "NCFI861-1", "MA-910",
                                       "EL-KRISE", "LEDLÆRUTV"),
                       year = c(2025L, 2025L, 2025L, 2024L, 2025L, 2025L, 2024L, 2024L),
                       semester_name = c("Vår", "Vår", "Høst", "Høst", "Vår", "Høst", "Høst", "Høst"))
  out <- resolve_urls_phs_batch(df)
  e <- "https://www.politihogskolen.no/"
  expect_equal(out$url, c(
    paste0(e, "studier/emner/2024/host/patrulje01.html"),    # over two semesters: the autumn before
    paste0(e, "studier/emner/2025/var/etterforsk01.html"),
    paste0(e, "studier/emner/2025/host/forebygg01-2.html"),  # a code with a dash
    paste0(e, "studier/emner/2024/host/avhorutd.html"),      # without the Norwegian letter
    paste0(e, "english/studies/courses/2025/spring/ncfi861-1.html"),   # English only
    NA,                                                     # not listed
    paste0(e, "studier/emner/2024/host/elkrise.html"),       # the page's name without the dash
    paste0(e, "studier/emner/2024/host/ledlerutv.html")))    # and with æ as e
  expect_false(any(grepl("/2023/", sent)))   # only the offerings' years and the year before a spring one
})

test_that("add_course_url() leaves PHS's URL to the resolver", {
  df <- tibble::tibble(institution = "phs", course_code = "PATRULJE01", course_code_raw = "PATRULJE01-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  cfg <- get_institution_config("phs")
  expect_equal(cfg$strategy, "url_discovery")
  expect_equal(cfg$plan_years, "url")
  expect_equal(institution_from_code("1301"), "phs")
})

test_that("a PHS page without a plan (the English pages) is not found", {
  local_robots("www.politihogskolen.no")
  page <- function(plan) httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
    body = charToRaw(paste0("<html><body><div id='vrtx-content'><h1>NCFI861-1 null (Spring 2025)</h1>",
                            "<div id='vrtx-fs-emne-facts'><dl><dt>ECTS Credits:</dt><dd>15</dd></dl></div>",
                            plan, "</div></body></html>")))
  httr2::local_mocked_responses(function(req) switch(basename(req$url),
    "a.html" = page("<h2 class='vrtx-fs-infotype-title'>Vurdering</h2><div class='vrtx-fs-infotype-content'>Skoleeksamen.</div>"),
    "b.html" = page(""),
    httr2::response(404)))
  e <- "https://www.politihogskolen.no/studier/emner/2025/host/"
  expect_equal(httr2::resp_status(fetch_html_cols_single_phs(paste0(e, "a.html"))), 200L)
  err <- tryCatch(fetch_html_cols_single_phs(paste0(e, "b.html")), error = identity)
  expect_s3_class(err, "phs_not_found_error")
  expect_equal(fetch_outcome(paste0(e, "b.html"), FALSE, 200L, fetch_error_text(list(err))), "not_found")
})

test_that("a loose code match is not used where two codes share its loose form", {
  index <- list(pages = tibble::tibble(code = "lfptpdx", year = 2024L, semester = "host",
                                       url = "https://x/emner/2024/host/lfptpdx.html"))
  df <- tibble::tibble(course_code = c("LFPTPDX", "LFPTP-DX", "EL-KRISE"), year = 2024L,
                       semester_name = "Høst")
  expect_equal(vortex_match_urls(df, index)$url, c("https://x/emner/2024/host/lfptpdx.html", NA, NA))
})
