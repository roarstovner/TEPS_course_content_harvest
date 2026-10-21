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
