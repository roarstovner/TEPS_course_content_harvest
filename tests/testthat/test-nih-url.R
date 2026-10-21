test_that("NIH page URLs: lower case, Norwegian letters transliterated", {
  expect_equal(as.character(add_course_url_nih("LKI230", 2022L, "Høst")),
               "https://www.nih.no/studier/emner/2022/host/lki230.html")
  expect_equal(as.character(add_course_url_nih("KRØ100", 2025L, "Vår")),
               "https://www.nih.no/studier/emner/2025/var/kro100.html")
})
