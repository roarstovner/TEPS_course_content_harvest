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
               "robots.txt of c.no has not been read.*Read robots.txt First", class = "robots_unread")
})

test_that("robots.txt disallows the UiS PDF archive; Steiner's subject PDFs are fetched by decision", {
  u <- c("https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf", "https://www6.uis.no/ansatt/x",
         "https://www.uis.no/nb/student/course/MGL101",
         "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-norsk-1",
         "https://www.ntnu.no/studier/emner/PED3001/2025", "https://www.bi.no/emne/plan.pdf")
  expect_equal(robots_allows(u), c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_match(institution_configs$steiner$robots_permission, "^decided .*not granted by Steiner")
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
  # DBH's old codes with a space: the space is percent-encoded, so the
  # request is sent (curl refuses a URL with a space)
  httr2::req_perform(site_request("https://www.uis.no/nb/student/course/MGL101 000"))
  expect_equal(sent[2], "https://www.uis.no/nb/student/course/MGL101%20000")
})

test_that("site_perform() follows redirects by hand, robots.txt checked at each hop", {
  local_robots(c("a.no", "www.a.no"), disallow = c(NA, "/dok/"))
  expect_false(site_request("https://a.no/x")$options$followlocation)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    to <- switch(req$url,
                 "https://a.no/x" = "https://www.a.no/emne/X",   # another host, allowed
                 "https://a.no/rel" = "/y?z=1",                   # relative, same host
                 "https://a.no/dok" = "https://www.a.no/dok/X",   # disallowed
                 "https://a.no/new" = "https://new.a.no/emne/X",  # a host not read
                 "https://a.no/loop" = "https://a.no/loop",
                 NULL)
    if (is.null(to)) httr2::response(200, body = charToRaw(req$url))
    else httr2::response(301, headers = list(Location = to))
  })
  resp <- site_perform(site_request("https://a.no/x"))
  expect_equal(httr2::resp_status(resp), 200L)
  expect_equal(sent, c("https://a.no/x", "https://www.a.no/emne/X"))
  site_perform(site_request("https://a.no/rel"))
  expect_equal(sent[4], "https://a.no/y?z=1")

  sent <- character()
  expect_error(site_perform(site_request("https://a.no/dok")),
               "robots.txt disallows https://www.a.no/dok/X, redirected from https://a.no/dok .*not requested",
               class = "robots_disallowed")
  expect_error(site_perform(site_request("https://a.no/new")), "robots.txt of new.a.no has not been read",
               class = "robots_unread")
  expect_equal(sent, c("https://a.no/dok", "https://a.no/new"))   # the hops were not requested
  expect_error(site_perform(site_request("https://a.no/loop")), "more than 5 redirects",
               class = "too_many_redirects")

  # in a harvest: a disallowed hop is outcome "robots"; a hop to a host not
  # read stops the fetch
  out <- fetch_html_cols("https://a.no/dok", "x", .progress = FALSE)
  expect_equal(fetch_outcome("u", FALSE, out$http_status, fetch_error_text(out$html_error)), "robots")
  expect_error(fetch_html_cols("https://a.no/new", "x", .progress = FALSE), class = "robots_unread")
})

test_that("every request of a site is performed by site_perform()", {
  code <- unlist(lapply(list.files(here::here("R"), "\\.R$", full.names = TRUE), readLines))
  expect_equal(grep("req_perform(", code, fixed = TRUE, value = TRUE),
               "    resp <- httr2::req_perform(req)")
})
