# The robots.txt of each site (data/robots_txt.csv) against the config: every
# configured institution's hosts have been read, and its request_delay is at
# least the Crawl-delay robots.txt asks of all user agents. The harvest's
# guard: robots_allows() and site_request().

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

test_that("the rules are robots.txt path patterns, separated by spaces", {
  p <- unlist(strsplit(stats::na.omit(c(robots$disallow, robots$allow)), " +"))
  expect_true(length(p) > 0)
  expect_equal(p[!grepl("^[/*]", p)], character())
})

test_that("robots_allows() applies the rules for all user agents, the longest match winning", {
  local_robots(c("a.no", "b.no"), disallow = c("/dok/ /*.pdf$", NA), allow = c("/dok/open/", NA))
  u <- c("https://a.no/dok/x", "https://a.no/dok/open/x", "https://a.no/x/plan.pdf",
         "https://a.no/x/plan.pdf?v=2", "https://a.no/dokx", "https://a.no", "https://A.NO/dok/#x",
         "https://b.no/dok/x", NA)
  expect_equal(robots_allows(u), c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, NA))
  # a host not read: its robots.txt may be read, nothing else
  expect_true(robots_allows("https://c.no/robots.txt"))
  expect_error(robots_allows(c("https://a.no/x", "https://c.no/emne/X")),
               "robots.txt of c.no has not been read.*Step 0", class = "robots_unread")
})

test_that("the paths robots.txt disallows are the UiS PDF archive and Steiner's subject PDFs", {
  u <- c("https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf", "https://www6.uis.no/ansatt/x",
         "https://www.uis.no/nb/student/course/MGL101",
         "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-norsk-1",
         "https://www.ntnu.no/studier/emner/PED3001/2025", "https://www.bi.no/emne/plan.pdf")
  expect_equal(robots_allows(u), c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
})

test_that("a permission in the institution's config lets the harvest request a disallowed path", {
  local_robots("www.hiof.no", disallow = "/studier/", institution = "hiof")
  u <- "https://www.hiof.no/studier/emner/X"
  expect_false(robots_allows(u))
  old <- institution_configs$hiof$robots_permission
  institution_configs$hiof$robots_permission <<- "granted 2026-10-17 by a test"
  withr::defer(institution_configs$hiof$robots_permission <<- old)
  expect_true(robots_allows(u))
})

test_that("site_request() sends no request robots.txt disallows, and none to a host not read", {
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    httr2::response(200)
  })
  expect_error(site_request("https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf"),
               "robots.txt disallows .*: not requested", class = "robots_disallowed")
  expect_error(site_request("https://www.newsite.no/emne/X"), class = "robots_unread")
  expect_length(sent, 0)
  out <- fetch_html_cols("https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf", "uis", .progress = FALSE)
  expect_false(out$html_success)
  expect_true(is.na(out$http_status))
  expect_equal(fetch_outcome("u", FALSE, out$http_status, fetch_error_text(out$html_error)), "robots")
  expect_length(sent, 0)
  httr2::req_perform(site_request("https://www.uis.no/nb/student/course/MGL101"))
  expect_equal(sent, "https://www.uis.no/nb/student/course/MGL101")
})
