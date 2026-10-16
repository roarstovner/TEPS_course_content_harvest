# The robots.txt of each site (data/robots_txt.csv) against the config: every
# configured institution's hosts have been read, and its request_delay is at
# least the Crawl-delay robots.txt asks of all user agents.

robots <- read.csv(here::here("data/robots_txt.csv"))

test_that("the robots.txt table has one row per host, each read on a date", {
  expect_false(anyDuplicated(robots$host) > 0)
  expect_false(anyNA(as.Date(robots$read_on, optional = TRUE)))
  # a host whose robots.txt answers 5xx may not be crawled at all (RFC 9309)
  expect_true(all(robots$status < 500))
})

test_that("every configured institution's robots.txt has been read", {
  expect_equal(setdiff(names(institution_configs), robots$institution), character())
})

test_that("request_delay is at least the robots.txt Crawl-delay", {
  r <- robots[robots$institution %in% names(institution_configs) & !is.na(robots$crawl_delay), ]
  delay <- vapply(r$institution, function(inst) {
    min(institution_configs[[inst]]$request_delay %||% DEFAULT_REQUEST_DELAY)
  }, numeric(1))
  expect_equal(r$host[delay < r$crawl_delay], character())
})
