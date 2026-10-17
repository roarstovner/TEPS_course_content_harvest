# The Wayback Machine helpers (R/wayback.R), without the network.

test_that("wayback_parse_cdx() reads the CDX API's JSON into a tibble", {
  json <- paste0('[["timestamp","original","statuscode"],',
                 '["20211025204403","https://www.uio.no/studier/emner/uv/ils/EDID4009/","200"],',
                 '["20220310120000","https://www.uio.no/studier/emner/uv/ils/EDID4009/index.html","200"]]')
  x <- wayback_parse_cdx(json)
  expect_equal(names(x), c("timestamp", "original", "statuscode"))
  expect_equal(nrow(x), 2)
  expect_equal(x$timestamp, c("20211025204403", "20220310120000"))
  expect_type(x$statuscode, "character")
  # no captures: an empty array, or a header without rows
  expect_equal(nrow(wayback_parse_cdx("[]")), 0)
  expect_equal(nrow(wayback_parse_cdx('[["timestamp","original"]]')), 0)
})

test_that("wayback_cdx_request() sends every filter and leaves out unset parameters", {
  req <- wayback_cdx_request("www.nmbu.no/emne/", "prefix", from = "202108", to = "202207",
                             filter = c("statuscode:200", "original:.*studieaar=.*"), collapse = "urlkey")
  q <- httr2::url_parse(req$url)$query
  expect_equal(q$url, "www.nmbu.no/emne/")
  expect_equal(q$matchType, "prefix")
  expect_equal(q$from, "202108")
  expect_equal(q$collapse, "urlkey")
  expect_null(q$limit)
  expect_equal(sum(names(q) == "filter"), 2)
  expect_equal(unname(unlist(q[names(q) == "filter"])), c("statuscode:200", "original:.*studieaar=.*"))
  expect_equal(req$options$useragent, TEPS_USER_AGENT)
})

test_that("wayback_capture_url() asks for the page as archived", {
  expect_equal(wayback_capture_url("20191215122653", "https://www.nmbu.no/emne/BIO100?studieaar=2019"),
               "https://web.archive.org/web/20191215122653id_/https://www.nmbu.no/emne/BIO100?studieaar=2019")
})
