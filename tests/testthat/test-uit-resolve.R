test_that("UiT: a semester listed twice keeps the first Norwegian document", {
  lookup <- tibble::tibble(
    course_code = "ENG-2980",
    year = c(2018L, 2017L, 2017L, 2016L, 2016L, 2015L),
    semester_name = "Vår",
    url = c("nb-531619", "en-509770", "nb-455351", "nb-413600", "nb-413564", "en-400000")
  )
  fetched <- character()
  is_english <- function(url) {
    fetched <<- c(fetched, url)
    startsWith(url, "en")
  }
  out <- uit_one_per_semester(lookup, is_english)

  expect_equal(out$url[order(-out$year)],
               c("nb-531619", "nb-455351", "nb-413600", "en-400000"))
  # only a semester listed twice is fetched; an English-only one stays
  expect_setequal(fetched, c("en-509770", "nb-455351", "nb-413600", "nb-413564"))
})

test_that("UiT: the document a course page shows is added when its picker lacks the semester", {
  page <- function(semester, options) xml2::read_html(paste0(
    '<div id="semester"><span class="sub_hero_text d-block">Semester / Year</span>',
    '<span class="sub_hero_text_innhold">', semester, '</span></div>',
    '<select id="aar"><option selected>Year / Semester</option>', options, '</select>',
    '<a href="https://uit.no/securerefresh?p_url=https://uit.no:443/utdanning/aktivt/emne/BIO-3529',
    '?p_document_id=873863"><span>Logg inn</span></a>'))
  opts <- paste0('<option value="/utdanning/emner/emne?p_document_id=842108">BIO-3529: H 2024</option>',
                 '<option value="/utdanning/emner/emne?p_document_id=822624">BIO-3529: V 2024</option>')

  out <- uit_semester_documents(page("\n  Autumn 2025\n", opts), "BIO-3529")
  expect_equal(out$year, c(2024L, 2024L, 2025L))
  expect_equal(out$semester_name, c("Høst", "Vår", "Høst"))
  expect_equal(out$url, paste0("https://uit.no/utdanning/emner/emne?p_document_id=",
                               c("842108", "822624", "873863")))
  # Norwegian page: "Vår\n 2012"; no picker
  out <- uit_semester_documents(page("Vår\n 2012", ""), "KJE-8810")
  expect_equal(out[c("year", "semester_name")], tibble::tibble(year = 2012L, semester_name = "Vår"))
  # a semester the picker lists keeps the picker's document
  out <- uit_semester_documents(page("Høst 2024", opts), "BIO-3529")
  expect_equal(nrow(out), 2)
  expect_equal(out$url[1], "https://uit.no/utdanning/emner/emne?p_document_id=842108")
  # no course with the code ("Error rendering component", p_document_id=0)
  none <- xml2::read_html(paste0('<p>Error rendering component</p><a href="https://uit.no/securerefresh?',
                                 'p_url=https://uit.no:443/utdanning/aktivt/emne/REL-1012?p_document_id=0">x</a>'))
  expect_equal(nrow(uit_semester_documents(none, "REL-1012")), 0)
})

# Replace global functions for one test (the project's functions are sourced
# into the global environment, not a package)
local_globals <- function(..., env = parent.frame()) {
  new <- list(...)
  old <- mget(names(new), envir = globalenv())
  list2env(new, globalenv())
  withr::defer(list2env(old, globalenv()), envir = env)
}

# A page of UiT's course list, /utdanning/emner?ar=...&semester=...
uit_list_page <- function(...) xml2::read_html(paste0(
  '<a href="/utdanning/emner?ar=2025&amp;semester=H&amp;ikbAction=ikb6&amp;ikb6.startrow=51">2</a>',
  paste0('<span class="studieProgramTittel"><a href="', c(...), '">', names(c(...)), ': Tittel</a></span>',
         collapse = "")))

test_that("UiT: a page of the course list gives each course's document", {
  page <- uit_list_page("FIL-0700" = "https://uit.no/utdanning/emner/emne?p_document_id=874490",
                        "MUS-2218" = "/utdanning/emner/emne?p_document_id=874889")
  out <- uit_list_documents(page, 2025, "Høst")
  expect_equal(out$course_code, c("FIL-0700", "MUS-2218"))
  expect_equal(out$url, paste0("https://uit.no/utdanning/emner/emne?p_document_id=", c("874490", "874889")))
  expect_equal(unique(out[c("year", "semester_name")]), tibble::tibble(year = 2025L, semester_name = "Høst"))
})

test_that("UiT: a semester the course page does not list takes the course list's document, read once", {
  requested <- character()
  active <- xml2::read_html(paste0(
    '<div id="semester"><span class="sub_hero_text d-block">Semester / år</span>',
    '<span class="sub_hero_text_innhold">Vår 2027</span></div>',
    '<select id="aar"><option value="/utdanning/emner/emne?p_document_id=786477">FIL-0700: V 2023</option></select>',
    '<a href="https://uit.no/securerefresh?p_url=https://uit.no:443/utdanning/aktivt/emne/FIL-0700',
    '?p_document_id=938847">Logg inn</a>'))
  lists <- list(
    "1" = uit_list_page("AKV-1400" = "/utdanning/emner/emne?p_document_id=874272",
                        "FIL-0700" = "/utdanning/emner/emne?p_document_id=874490"),
    "3" = uit_list_page("JUR-1003" = "/utdanning/emner/emne?p_document_id=874637"))
  local_globals(polite_pause = function(...) NULL, site_html = function(url) {
    requested <<- c(requested, url)
    if (grepl("aktivt/emne", url)) return(active)
    if (!grepl("ar=2025&semester=H", url, fixed = TRUE)) return(uit_list_page())   # no course
    start <- stringr::str_match(url, "startrow=(\\d+)")[, 2]
    lists[[dplyr::coalesce(start, "1")]] %||% lists[["3"]]   # past the end: the last page again
  })
  df <- tibble::tibble(course_code = "FIL-0700", year = c(2023L, 2025L, 2024L),
                       semester_name = c("Vår", "Høst", "Vår"), url = NA_character_)
  dir <- withr::local_tempdir()

  out <- resolve_urls_uit_batch(df, .progress = FALSE, checkpoint = dir)
  expect_equal(out$url, c("https://uit.no/utdanning/emner/emne?p_document_id=786477",
                          "https://uit.no/utdanning/emner/emne?p_document_id=874490", NA))
  # the lists of the two semesters the page lacks, until a page adds nothing
  expect_equal(sum(grepl("ar=2025&semester=H", requested)), 3)
  expect_equal(sum(grepl("ar=2024&semester=V", requested)), 1)

  # a resumed harvest reads the codes and the lists from the checkpoint
  requested <- character()
  expect_equal(resolve_urls_uit_batch(df, .progress = FALSE, checkpoint = dir)$url, out$url)
  expect_length(requested, 0)
})

test_that("UiT: a document serves only the semester it names", {
  html <- function(s) paste0('<div id="semester" class="py-1 col-6 col-md-auto">\n  <span class="sub_hero_text d-block">',
                             'Semester / Year</span>\n  <span class="sub_hero_text_innhold">\n\n   ', s, '\n  </span></div>')
  term <- institution_configs$uit$page_term
  expect_equal(term(html(c("Høst 2025", "Spring 2025", "Autumn 2024"))),
               semester_term(c(2025, 2025, 2024), c("Høst", "Vår", "Høst")))
  expect_true(is.na(term("<p>no label</p>")))
  expect_identical(institution_configs$uit$page_end_term(html("Vår 2026")), term(html("Vår 2026")))
})
