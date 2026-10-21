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
