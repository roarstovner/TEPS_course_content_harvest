test_that("UiT: a semester listed twice keeps the first Norwegian document (#313)", {
  lookup <- tibble::tibble(
    Emnekode = "ENG-2980",
    Årstall = c(2018L, 2017L, 2017L, 2016L, 2016L, 2015L),
    Semesternavn = "Vår",
    url = c("nb-531619", "en-509770", "nb-455351", "nb-413600", "nb-413564", "en-400000")
  )
  fetched <- character()
  is_english <- function(url) {
    fetched <<- c(fetched, url)
    startsWith(url, "en")
  }
  out <- uit_one_per_semester(lookup, is_english)

  expect_equal(out$url[order(-out$Årstall)],
               c("nb-531619", "nb-455351", "nb-413600", "en-400000"))
  # only a semester listed twice is fetched; an English-only one stays
  expect_setequal(fetched, c("en-509770", "nb-455351", "nb-413600", "nb-413564"))
})
