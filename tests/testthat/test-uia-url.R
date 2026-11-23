test_that("UiA: DBH's -G, -G2 and -D suffixes are not in the site's code", {
  u <- function(code, sem = "Høst") as.character(add_course_url_uia(code, 2022L, sem))
  expect_equal(u("MA-015-G"), "https://www.uia.no/studier/emner/2022/host/ma-015.html")
  expect_equal(u("IKT623-G", "Vår"), "https://www.uia.no/studier/emner/2022/var/ikt623.html")
  expect_equal(u("TFL011-G2"), "https://www.uia.no/studier/emner/2022/host/tfl011.html")
  expect_equal(u("PRA090-D"), "https://www.uia.no/studier/emner/2022/host/pra090.html")
})

test_that("UiA: other codes keep their form", {
  u <- function(code) as.character(add_course_url_uia(code, 2025L, "Vår"))
  expect_equal(u("BIO115N"), "https://www.uia.no/studier/emner/2025/var/bio115n.html")
  expect_equal(u("PRA1-302"), "https://www.uia.no/studier/emner/2025/var/pra1-302.html")
  expect_equal(u("GEO-DATA"), "https://www.uia.no/studier/emner/2025/var/geo-data.html")
})

# A course taught in English may have its page only in the English tree:
# resolve_urls_english_tree_batch() gives an offering whose Norwegian URL
# answered 404 the English tree's page, from that tree's year listing
# (mocked), and every other offering the Norwegian URL.
english_tree_links <- function(...) {
  a <- paste0("<a href='", c(...), "'>x</a>", collapse = "")
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body>", a, "</body></html>")))
}

test_that("UiA: the English tree's page where the Norwegian URL answered 404, else the Norwegian URL", {
  local_robots("www.uia.no")
  institution_configs$uia$request_delay <<- 0
  withr::defer(institution_configs$uia$request_delay <<- NULL)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(sub("https://www.uia.no", "", req$url, fixed = TRUE),
      "/english/studies/courses/" = english_tree_links("2024/", "2025/", "2025/spring/"),
      "/english/studies/courses/2025/" = english_tree_links("spring/mf-102.html", "autumn/me-112.html",
                                                            "spring/ikt623.html", "autumn/ped100.html",
                                                            "/studier/emner/"),
      "/english/studies/courses/2025/?page=2" = english_tree_links("autumn/org208.html"),
      "/english/studies/courses/2025/?page=3" = english_tree_links("autumn/org208.html"),
      httr2::response(404))
  })
  df <- tibble::tibble(course_code = c("MF-102", "MF-102", "ME-112", "ORG208", "IKT623-G", "PED100"),
                       year = 2025L, semester_name = c("Vår", "Høst", "Høst", "Høst", "Vår", "Høst"))
  n <- "https://www.uia.no/studier/emner/2025/"
  gone <- paste0(n, c("var/mf-102.html", "host/mf-102.html", "host/me-112.html", "host/org208.html",
                      "var/ikt623.html"))
  out <- resolve_urls_english_tree_batch(df, "uia", gone = gone)
  e <- "https://www.uia.no/english/studies/courses/2025/"
  expect_equal(out$url, c(
    paste0(e, "spring/mf-102.html"),
    paste0(n, "host/mf-102.html"),       # listed in spring only
    paste0(e, "autumn/me-112.html"),
    paste0(e, "autumn/org208.html"),     # on the listing's second page
    paste0(e, "spring/ikt623.html"),     # without DBH's -G
    paste0(n, "host/ped100.html")))      # a translation: the Norwegian page did not answer 404
  expect_false(any(grepl("/2024/|/studier/", sent)))   # only the year of the 404s, only the English tree
  # nothing answered 404: no listing is read
  sent <- character()
  expect_equal(resolve_urls_english_tree_batch(df[1, ], "uia", gone = character())$url, paste0(n, "var/mf-102.html"))
  expect_length(sent, 0)
})

test_that("raw_not_found_urls() gives the URLs whose latest request answered 404", {
  raw <- withr::local_tempdir()
  rows <- tibble::tibble(
    course_id = c("uia_A_2025_spring_1", "uia_B_2025_spring_1", "uia_C_2025_spring_1"),
    institution = "uia", year = 2025L, url = c("u/A", "u/B", "u/C"), html = c(NA, NA, "plan"),
    html_error = list("HTTP 404 Not Found.", "HTTP 404 Not Found.", NULL), html_success = c(FALSE, FALSE, TRUE))
  write_raw_run(rows, "uia", "2026-10-01", raw)
  again <- rows[2, ]
  again$html <- "plan B"; again$html_error <- list(NULL); again$html_success <- TRUE
  write_raw_run(again, "uia", "2026-10-02", raw)
  expect_equal(raw_not_found_urls("uia", raw), "u/A")   # B was found on the retry
  expect_equal(raw_not_found_urls("nih", raw), character())
})

test_that("add_course_url() leaves UiA's URL to the resolver", {
  df <- tibble::tibble(institution = "uia", course_code = "MF-102", course_code_raw = "MF-102-1",
                       year = 2025L, semester_name = "Vår")
  expect_true(is.na(add_course_url(df)$url))
  expect_equal(get_institution_config("uia")$strategy, "url_discovery")
})

test_that("UiA's English pages read as its Norwegian ones: evaluation and admission flags are facts, the exam gate is coursework", {
  cfg <- get_institution_config("uia")
  part <- function(h, x) paste0("<h2 class='vrtx-fs-infotype-title'>", h, "</h2><div>", x, "</div>")
  html <- paste0("<div id='right-main'><div id='vrtx-fs-emne-content'><h1>MA-100 Calculus</h1>",
                 part("Contents", "Limits and derivatives."),
                 part("Examination requirements", "Approved mandatory assignments."),
                 part("Examinations", "Written exam, 4 hours."),
                 part("Admission Requirement if given as a free-standing course", "General admission."),
                 part("Student evaluation", "The course is evaluated by the quality system."),
                 part("Offered as a free-standing course", "Yes"),
                 part("Admission for external candidates", "No"), "</div></div>")
  blocks <- page_blocks(html, NA, .block_cfg(cfg))
  plan <- page_fulltext(blocks, cfg, facts = FALSE)
  expect_false(grepl("Student evaluation|quality system|free-standing course\nYes|external candidates", plan))
  expect_match(plan, "Examination requirements\nApproved mandatory assignments.", fixed = TRUE)
  sec <- extract_sections(cfg, html, NA_character_, "x")
  sec <- setNames(sec$raw_text, sec$section)
  expect_equal(unname(sec["coursework_requirements"]), "Approved mandatory assignments.")
  expect_equal(unname(sec["assessment"]), "Written exam, 4 hours.")
  expect_false(any(grepl("General admission", sec)))
})
