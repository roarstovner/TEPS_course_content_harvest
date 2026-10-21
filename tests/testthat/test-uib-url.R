test_that("UiB page URLs: lower case, semester code, space percent-encoded", {
  expect_equal(as.character(add_course_url_uib("DIDASAK1", 2010L, "Høst")),
               "https://www4.uib.no/studier/emner/didasak1?start_semester=2010H")
  expect_equal(as.character(add_course_url_uib("EXPHIL-HFSEM", 2019L, "Vår")),
               "https://www4.uib.no/studier/emner/exphil-hfsem?start_semester=2019V")
  expect_equal(as.character(add_course_url_uib("GEOV106", 2010L, "Sommer")),
               "https://www4.uib.no/studier/emner/geov106?start_semester=2010H")
  expect_equal(as.character(add_course_url_uib("UTV VISK 5H", 2022L, "Høst")),
               "https://www4.uib.no/studier/emner/utv%20visk%205h?start_semester=2022H")
})
