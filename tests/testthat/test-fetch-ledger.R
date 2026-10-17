# Tests for the fetch ledger and the retry policy by outcome class:
# fetch_outcome(), site_request(), fetch_html_cols(), fetch_ledger(),
# raw_ledger(), harvest_all(retry = ), offering_outcome(), fetch_coverage(),
# page_changes(); the URLs robots.txt disallows in a harvest. Sites are
# mocked (httr2::local_mocked_responses()), robots.txt by local_robots().

html_page <- function(text, head = "") {
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><head>", head, "</head><body>",
                                          "<div id='vrtx-fs-emne-content'><h1>Emne</h1><p>",
                                          text, "</p></div></body></html>")))
}

# hiof without its pause between requests, in a temporary working directory
local_hiof <- function(env = parent.frame()) {
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  old <- institution_configs$hiof$request_delay
  institution_configs$hiof$request_delay <<- 0
  withr::defer(institution_configs$hiof$request_delay <<- old, envir = env)
}

hiof_dbh <- function(codes) {
  dbh <- tibble::tibble(institution = "hiof", course_code_raw = codes, course_code = codes,
                        year = 2023L, semester_name = "Høst", status = 1L)
  attr(dbh, "dbh_snapshot") <- "2026-10-16"
  dbh
}

test_that("a fetch's outcome class comes from its page, status and error", {
  x <- tibble::tribble(
    ~url, ~page, ~status, ~error, ~outcome,
    "u", TRUE, 200L, NA, "ok",
    NA, TRUE, NA, NA, "ok",                       # a PDF's text (steiner)
    NA, FALSE, NA, NA, "no_url",
    "u", FALSE, 404L, "HTTP 404 Not Found.", "not_found",
    "u", FALSE, 400L, "HTTP 400 Bad Request.", "not_found",
    "u", FALSE, 200L, "Course not found (soft 404)", "not_found",
    "u", FALSE, 200L, "No course information available for this academic year", "shell",
    "u", FALSE, 429L, "HTTP 429 Too Many Requests.", "blocked",
    "u", FALSE, 403L, "HTTP 403 Forbidden.", "forbidden",
    "u", FALSE, NA, "robots.txt disallows u (data/robots_txt.csv): not requested", "robots",
    NA, FALSE, NA, "robots.txt disallows d/ (data/robots_txt.csv): not requested", "robots",
    "u", FALSE, 502L, "HTTP 502 Bad Gateway.", "server_error",
    "u", FALSE, NA, "Failed to perform HTTP request. Timeout was reached", "timeout",
    "u", FALSE, NA, "subscript out of bounds", "error")
  expect_equal(fetch_outcome(x$url, x$page, x$status, x$error), x$outcome)
  expect_true(all(x$outcome %in% FETCH_OUTCOMES))
})

test_that("a timeout or server error is tried again after a pause, a 404, 403 or 429 is not", {
  local_robots(c("example.org", "127.0.0.1"))
  req <- site_request("https://example.org/x")
  transient <- req$policies$retry_is_transient
  expect_true(transient(httr2::response(502)))
  expect_true(transient(httr2::response(503)))
  expect_false(transient(httr2::response(429)))
  expect_false(transient(httr2::response(403)))
  expect_false(transient(httr2::response(404)))
  expect_false(transient(httr2::response(200)))
  expect_true(req$policies$retry_on_failure)
  expect_equal(req$policies$retry_max_tries, FETCH_TRIES)
  expect_equal(req$policies$retry_backoff(1:2), FETCH_BACKOFF * c(1, 2))
  expect_equal(req$options$timeout_ms, FETCH_TIMEOUT * 1000)

  # no answer (nothing listens on port 1): tried FETCH_TRIES times, "timeout"
  old <- FETCH_BACKOFF
  FETCH_BACKOFF <<- 0
  withr::defer(FETCH_BACKOFF <<- old)
  tries <- 0
  out <- withCallingHandlers(fetch_html_cols("http://127.0.0.1:1/", "x", .progress = FALSE),
                             httr2_retry = function(cnd) tries <<- tries + 1)
  expect_equal(tries, FETCH_TRIES)   # httr2 signals each failed try
  expect_equal(fetch_outcome("u", FALSE, out$http_status, fetch_error_text(out$html_error)), "timeout")
})

test_that("fetch_html_cols() records each request's status and time", {
  local_robots(c("x.no", "www.ntnu.no"))
  httr2::local_mocked_responses(function(req) {
    if (grepl("gone", req$url)) httr2::response(404) else html_page("Plan")
  })
  before <- Sys.time()
  out <- fetch_html_cols(c("https://x.no/a", "https://x.no/gone", NA), rep("x", 3), .progress = FALSE)
  expect_equal(out$http_status, c(200L, 404L, NA))
  expect_equal(out$html_success, c(TRUE, FALSE, NA))
  expect_true(all(out$requested_at[1:2] >= before))
  expect_true(is.na(out$requested_at[3]))
  expect_equal(fetch_error_text(out$html_error), c(NA, "HTTP 404 Not Found.", NA))

  # NTNU's page without the year's plan: a shell, with the page's status
  httr2::local_mocked_responses(function(req) {
    html_page("There is no information available for the given academic year")
  })
  out <- fetch_html_cols("https://www.ntnu.no/studier/emner/X/2003", "ntnu",
                         get_institution_config("ntnu"), .progress = FALSE)
  expect_equal(out$http_status, 200L)
  rows <- dplyr::bind_cols(tibble::tibble(course_id = "c", year = 2003L, url = "u"), out)
  expect_equal(fetch_ledger(rows, "ntnu", "r")$outcome, "shell")
})

test_that("harvest_all() writes the fetch ledger with the run, and a retry skips pages not found", {
  local_hiof()
  httr2::local_mocked_responses(function(req) {
    if (grepl("gone1", req$url)) httr2::response(404) else html_page("Plan")
  })
  dbh <- hiof_dbh(c("OK1", "GONE1", "SHELL1"))
  suppressMessages(harvest_all(dbh, institutions = "hiof"))
  run <- raw_run(harvest_files("hiof"))
  led <- arrow::read_parquet(ledger_file("hiof", run))
  expect_equal(led$outcome, c("ok", "not_found", "ok"))
  expect_equal(led$http_status, c(200L, 404L, 200L))
  expect_false(anyNA(led$requested_at))
  expect_equal(unique(led$dbh_snapshot), "2026-10-16")
  expect_equal(unique(led$raw_file), raw_file("hiof", 2023, run))
  raw <- read_raw(harvest_files("hiof"))
  expect_false(any(REQUEST_COLUMNS %in% names(raw)))
  expect_true(all(raw_ledger(harvest_files("hiof"))$recorded))
  # reconstructed from the raw run: the same, but for what only the fetch knows
  rec <- fetch_ledger(raw, "hiof", run)
  cols <- c("run", "course_id", "url", "outcome", "bytes", "sha256", "raw_file")
  expect_equal(rec[cols], led[cols])
  expect_equal(rec$http_status, c(NA, 404L, NA))
  expect_true(all(is.na(rec$requested_at)))

  # no plan for any of them: a retry fetches the pages again, not the 404
  dir.create("data/interim")
  saveRDS(dplyr::transmute(led, course_id, institution, url, plan_content_id = NA_character_),
          "data/interim/course_offerings_full.RDS")
  suppressMessages(harvest_all(dbh, institutions = "hiof", retry = TRUE))
  rows <- read_raw(harvest_files("hiof"), run = TRUE)
  expect_equal(rows$course_id[rows$retry %in% TRUE], led$course_id[c(1, 3)])
  expect_message(harvest_all(dbh, institutions = "hiof", retry = "server_error"), "nothing new")
  Sys.sleep(1)   # runs named by the second
  # unless asked for: a page published late
  suppressMessages(harvest_all(dbh, institutions = "hiof", retry = "not_found"))
  rows <- read_raw(harvest_files("hiof"), run = TRUE)
  expect_equal(rows$course_id[rows$.run == max(rows$.run)], led$course_id[2])
  expect_equal(nrow(raw_ledger(harvest_files("hiof"))), 6)
  expect_error(harvest_all(dbh, institutions = "hiof", retry = "gone"), "outcome classes")
})

test_that("a site that refuses the harvest stops it, and the pages fetched stay in the checkpoint", {
  local_hiof()
  httr2::local_mocked_responses(function(req) {
    if (grepl("b1", req$url)) httr2::response(429) else html_page("Plan")
  })
  msg <- capture_messages(harvest_all(hiof_dbh(c("A1", "B1", "C1")), institutions = "hiof"))
  expect_match(paste(msg, collapse = ""), "refuses the harvest \\(HTTP 429")
  expect_length(harvest_files("hiof"), 0)
  expect_equal(readRDS("data/raw/checkpoint/html_hiof.RDS")$course_id, "hiof_A1_2023_autumn_1")
})

test_that("a site refuses the harvest with a 429, or with 403 to FORBIDDEN_RUN requests in a row and to a public page", {
  refused <- refusal_watch()
  expect_equal(refused(403L, "u"), 1L)
  expect_equal(refused(403L, "u"), 2L)
  expect_equal(refused(NA, "u"), 0L)   # a 403 among other answers is the page's own
  for (k in seq_len(FORBIDDEN_RUN - 1)) refused(403L, "u")
  saved <- NULL
  expect_error(refused(403L, "u/x", before_stop = function(run) saved <<- run),
               paste0("refuses the harvest \\(HTTP 403 at u/x, ", FORBIDDEN_RUN,
                      " in a row; no page known to be public to check\\)"))
  expect_equal(saved, FORBIDDEN_RUN)
  expect_error(refusal_watch()(429L, "u"), "refuses the harvest \\(HTTP 429 at u\\)")

  # a harvest: the site answers A1, then refuses every request, also A1 again
  # (the probe); the refusal's 403s stay out of the checkpoint, to be asked for again
  local_hiof()
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, basename(req$url))
    if (length(sent) > 1) httr2::response(403) else html_page("Plan")
  })
  codes <- c("A1", paste0("FORB", seq_len(FORBIDDEN_RUN)), "Z1")
  msg <- capture_messages(harvest_all(hiof_dbh(codes), institutions = "hiof"))
  expect_match(paste(msg, collapse = ""),
               paste0("refuses the harvest \\(HTTP 403 .*forb", FORBIDDEN_RUN, ".html, ", FORBIDDEN_RUN,
                      " in a row; https://www.hiof.no/.*/a1.html, known to be public, answered HTTP 403\\)"))
  expect_equal(sent, paste0(c(tolower(codes[-length(codes)]), "a1"), ".html"))
  expect_length(harvest_files("hiof"), 0)
  expect_equal(readRDS("data/raw/checkpoint/html_hiof.RDS")$course_id, "hiof_A1_2023_autumn_1")
})

test_that("403s in a row while a public page answers 200 are the pages' own; the probe is in the ledger", {
  local_hiof()
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, basename(req$url))
    if (grepl("/forb", req$url)) httr2::response(403) else html_page("Plan")
  })
  forb <- paste0("FORB", seq_len(FORBIDDEN_RUN))
  # no page known to be public: stop
  msg <- capture_messages(harvest_all(hiof_dbh(forb), institutions = "hiof"))
  expect_match(paste(msg, collapse = ""), "in a row; no page known to be public to check")
  expect_equal(sent, paste0(tolower(forb), ".html"))

  # a page in the raw store is known to be public: it is asked for, and the harvest goes on
  suppressMessages(harvest_all(hiof_dbh("OLD1"), institutions = "hiof"))
  Sys.sleep(1)   # runs named by the second
  sent <- character()
  suppressMessages(harvest_all(hiof_dbh(c(forb, "FORB9", "Z1")), institutions = "hiof"))
  expect_equal(sent, paste0(c(tolower(forb), "old1", "forb9", "z1"), ".html"))
  led <- raw_ledger(harvest_files("hiof"))
  expect_equal(led$outcome, c("ok", rep("forbidden", FORBIDDEN_RUN + 1), "ok", "probe"))
  probe <- led[led$outcome == "probe", ]
  expect_equal(basename(probe$url), "old1.html")
  expect_equal(probe$http_status, 200L)
  expect_true(is.na(probe$course_id) && !is.na(probe$requested_at))
  # the probe is not an offering's fetch
  expect_equal(offering_outcome("hiof_OLD1_2023_autumn_1", probe$url, led), "ok")
  expect_null(take_probes("hiof"))
})

test_that("a page that answers 403 among other answers is forbidden: the harvest goes on, no retry", {
  local_hiof()
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, basename(req$url))
    if (grepl("/forb", req$url)) httr2::response(403) else html_page("Plan")
  })
  dbh <- hiof_dbh(c("OK1", "FORB1", "FORB2", "OK2"))
  suppressMessages(harvest_all(dbh, institutions = "hiof"))
  led <- raw_ledger(harvest_files("hiof"))
  expect_equal(led$outcome, c("ok", "forbidden", "forbidden", "ok"))
  expect_equal(led$http_status, c(200L, 403L, 403L, 200L))

  dir.create("data/interim")
  saveRDS(dplyr::transmute(led, course_id, institution, url, plan_content_id = c("p1", NA, NA, "p2")),
          "data/interim/course_offerings_full.RDS")
  expect_message(harvest_all(dbh, institutions = "hiof", retry = TRUE), "nothing new")
  suppressMessages(harvest_all(dbh, institutions = "hiof", retry = "forbidden"))
  expect_equal(utils::tail(sent, 2), c("forb1.html", "forb2.html"))
})

test_that("a URL robots.txt disallows is not requested, and is not retried without a permission", {
  local_hiof()
  local_robots("www.hiof.no", disallow = "/*/no1.html", institution = "hiof")
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    html_page("Plan")
  })
  dbh <- hiof_dbh(c("OK1", "NO1"))
  msg <- capture_messages(harvest_all(dbh, institutions = "hiof"))
  expect_match(paste(msg, collapse = ""), "robots.txt disallows 1 of them: not requested")
  expect_equal(basename(sent), "ok1.html")
  led <- raw_ledger(harvest_files("hiof"))
  expect_equal(led$outcome, c("ok", "robots"))
  expect_equal(is.na(led$requested_at), c(FALSE, TRUE))
  expect_true(is.na(led$http_status[2]))
  expect_match(read_raw(harvest_files("hiof"))$html_error[2], "^robots.txt disallows https://www.hiof.no/")

  dir.create("data/interim")
  saveRDS(dplyr::transmute(led, course_id, institution, url, plan_content_id = c("p1", NA)),
          "data/interim/course_offerings_full.RDS")
  expect_message(harvest_all(dbh, institutions = "hiof", retry = TRUE), "nothing new")
  # with a permission, a retry by name fetches it
  old <- institution_configs$hiof$robots_permission
  institution_configs$hiof$robots_permission <<- "granted 2026-10-17 by a test"
  withr::defer(institution_configs$hiof$robots_permission <<- old)
  suppressMessages(harvest_all(dbh, institutions = "hiof", retry = "robots"))
  expect_equal(basename(sent), c("ok1.html", "no1.html"))
  expect_equal(utils::tail(raw_ledger(harvest_files("hiof"))$outcome, 1), "ok")
})

test_that("a host whose robots.txt has not been read stops its harvest before any request", {
  local_hiof()
  local_robots("www.other.no")
  sent <- 0
  httr2::local_mocked_responses(function(req) {
    sent <<- sent + 1
    html_page("Plan")
  })
  msg <- capture_messages(harvest_all(hiof_dbh("A1"), institutions = "hiof"))
  expect_match(paste(msg, collapse = ""),
               "ERROR harvesting hiof: the robots.txt of www.hiof.no has not been read.*Step 0")
  expect_equal(sent, 0)
  expect_length(harvest_files("hiof"), 0)
})

test_that("UiS's PDF archive and Steiner's subject PDFs are not requested", {
  withr::local_dir(withr::local_tempdir())
  old <- institution_configs$uis$request_delay
  institution_configs$uis$request_delay <<- 0
  withr::defer(institution_configs$uis$request_delay <<- old)
  sent <- character()
  menu <- paste0("<select id='fs-semester-select'><option ",
                 "value='https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf'>2019 - 2020</option></select>")
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    html_page(menu)
  })
  dbh <- function(inst, code, year, semester) {
    tibble::tibble(institution = inst, course_code_raw = code, course_code = code, year = year,
                   semester_name = semester, status = 1L)
  }
  uis <- suppressMessages(harvest_institution("uis", dbh("uis", "MGL101", 2019L, "Høst")))
  expect_equal(uis$url, "https://www6.uis.no/Stine/EMNE/2019/H/MGL101.pdf")
  expect_equal(fetch_ledger(uis, "uis", "r")$outcome, "robots")
  expect_equal(sent, "https://www.uis.no/nb/student/course/MGL101")   # the course page's menu only

  steiner <- suppressWarnings(suppressMessages(
    harvest_institution("steiner", dbh("steiner", c("M-LP1", "M-LP2"), 2025L, "Vår"))))
  expect_equal(fetch_ledger(steiner, "steiner", "r")$outcome, c("robots", "robots"))
  expect_match(fetch_error_text(steiner$html_error),
               "robots.txt disallows https://www.steinerhoyskolen.no/dokumenter/ ")
  expect_length(sent, 1)
})

test_that("a run without a ledger has one reconstructed; outcomes are by URL", {
  raw <- withr::local_tempdir()
  rows <- tibble::tibble(
    course_id = c("x_A_2024_autumn_1", "x_A_2025_spring_1", "x_B_2024_autumn_1", "x_C_2024_autumn_1"),
    institution = "x", year = c(2024L, 2025L, 2024L, 2024L),
    url = c("u/A24", "u/A25", "u/B24", NA), html = c("plan", NA, NA, NA),
    html_error = list(NULL, "HTTP 404 Not Found.", "HTTP 502 Bad Gateway.", NULL),
    html_success = c(TRUE, FALSE, FALSE, NA))
  write_raw_run(rows, "x", "2026-10-01", raw)
  led <- raw_ledger(harvest_files("x", raw))
  expect_false(any(led$recorded))
  expect_equal(led$outcome, c("ok", "not_found", "server_error", "no_url"))
  expect_equal(led$http_status, c(NA, 404L, 502L, NA))
  expect_equal(led$bytes, c(4L, NA, NA, NA))
  expect_equal(led$sha256[1], cli::hash_sha256("plan"))

  # a later run, as harvest_all() writes it: B's page is fetched again
  again <- rows[3, ]
  again$html <- "plan B"; again$html_error <- list(NULL); again$http_status <- 200L
  again$requested_at <- Sys.time()
  write_raw_run(dplyr::select(again, -dplyr::all_of(REQUEST_COLUMNS)), "x", "2026-10-02", raw)
  write_fetch_ledger(fetch_ledger(again, "x", "2026-10-02", raw_dir = raw), "x", "2026-10-02", raw)
  expect_error(write_fetch_ledger(fetch_ledger(again, "x", "2026-10-02"), "x", "2026-10-02", raw),
               "not written twice")
  led <- raw_ledger(harvest_files("x", raw))
  expect_equal(led$recorded, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(led$raw_file[5], file.path(raw, "x", "2024", "2026-10-02.parquet"))

  # A spring 2025 reads /A24, which gave a page; /A23 nobody requested
  ids <- c("x_A_2025_spring_1", "x_B_2024_autumn_1", "x_C_2024_autumn_1", "x_D_2024_autumn_1",
           "x_A_2024_autumn_1", "x_A_2025_spring_1")
  out <- offering_outcome(ids, c("u/A24", "u/B24", NA, "u/D24", "u/A23", "u/A25"), led)
  expect_equal(out, c("ok", "ok", "no_url", "not harvested", "not requested", "not_found"))
  expect_equal(retry_wanted(out, TRUE), c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(retry_wanted(out, "no_url"), c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(retry_wanted(c("robots", "not_found"), TRUE), c(FALSE, FALSE))
  expect_equal(retry_wanted("robots", "robots"), TRUE)
  expect_equal(nrow(raw_ledger(character())), 0)
})

test_that("the coverage of a frame: outcome, plan and whether a retry would fetch it", {
  frame <- tibble::tibble(institution = c("hiof", "hiof", "hiof", "hiof", "uio", NA),
                          course_code_raw = c("A", "B", "C", "D", "E", "F"),
                          course_code = c("A", "B", "C", "D", "E", "F"), year = 2023L,
                          semester_name = "Høst", status = c(1L, 1L, 2L, 1L, 1L, 1L))
  ledger <- tibble::tibble(course_id = c("hiof_A_2023_autumn_1", "hiof_B_2023_autumn_1",
                                         "hiof_C_2023_autumn_1", "uio_E_2023_autumn_1"),
                           url = c("a", "b", "c", "e"), outcome = c("ok", "not_found", "timeout", "ok"))
  offerings <- tibble::tibble(course_id = ledger$course_id, url = ledger$url,
                              plan_content_id = c("p1", NA, NA, NA))
  cov <- fetch_coverage(frame, ledger, offerings)
  expect_equal(cov$course_id, c(ledger$course_id[1:3], "hiof_D_2023_autumn_1", ledger$course_id[4]))
  expect_equal(cov$outcome, c("ok", "not_found", "timeout", "not harvested", "ok"))
  expect_equal(cov$has_plan, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  # C (DBH's status 2 now) keeps the id it was harvested under; uio takes a new snapshot instead
  expect_equal(cov$retry, c(FALSE, FALSE, TRUE, FALSE, FALSE))
})

test_that("page_changes() lists pages whose text changed between runs", {
  raw <- withr::local_tempdir()
  page <- function(text, token) rawToChar(html_page(text, paste0("<script>t='", token, "'</script>"))$body)
  rows <- function(html, ids = c("hiof_A_2023_autumn_1", "hiof_B_2023_autumn_1")) tibble::tibble(
    course_id = ids, institution = "hiof", year = 2023L, url = c("u/A", "u/B"),
    html = html, html_error = list(NULL), html_success = TRUE)
  for (r in list(list("2026-10-01", c(page("Plan A", "t1"), page("Plan B", "t1"))),
                 list("2026-10-14", c(page("Plan A", "t2"), page("Plan B reworded", "t2"))),
                 list("2026-10-15", c(page("Plan A", "t2"), page("Plan B reworded", "t2"))))) {
    write_raw_run(rows(r[[2]]), "hiof", r[[1]], raw)
  }
  files <- harvest_files("hiof", raw)
  led <- raw_ledger(files)
  # every body differs from the run before (the token), A's text does not
  out <- page_changes(led, files, get_institution_config("hiof"))
  expect_equal(out$url, "u/B")
  expect_equal(c(out$previous_run, out$run), c("2026-10-01", "2026-10-14"))
  expect_equal(nrow(page_changes(led[0, ], files, get_institution_config("hiof"))), 0)
})

test_that("nla: each academic year's page is fetched once and read by its own offerings", {
  withr::local_dir(withr::local_tempdir())
  local_robots("www.nla.no")
  old <- institution_configs$nla$request_delay
  institution_configs$nla$request_delay <<- 0
  withr::defer(institution_configs$nla$request_delay <<- old)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, basename(req$url))
    if (!grepl("2023-2024$", req$url)) return(httr2::response(404))
    json <- paste0('{"page": "EmneplanPage", "props": {"items": {"2023-2024": {"title": "Norsk 1",',
                   ' "accordions": [{"title": "Innhold", "content": "<p>Tekst.</p>"}]}}}}')
    httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                    body = charToRaw(paste0("<html><body><script>", json, "</script></body></html>")))
  })
  # autumn 2023 and spring 2024 share /NO1/2023-2024; autumn 2024 reads /NO1/2024-2025
  dbh <- tibble::tibble(institution = "nla", course_code_raw = "NO1", course_code = "NO1",
                        year = c(2023L, 2024L, 2024L), semester_name = c("Høst", "Vår", "Høst"),
                        status = 1L)
  suppressMessages(harvest_all(dbh, institutions = "nla"))
  expect_equal(sent, c("2023-2024", "2024-2025"))
  x <- read_raw(harvest_files("nla"))
  expect_equal(basename(x$url), c("2023-2024", "2023-2024", "2024-2025"))
  expect_equal(!is.na(x$html), c(TRUE, TRUE, FALSE))
  # the spring offering reads the 2023-2024 entry of the JSON too
  expect_match(extract_fulltext_from_raw(x, get_institution_config("nla"))[1:2], "Tekst.", fixed = TRUE)
  expect_equal(raw_ledger(harvest_files("nla"))$outcome, c("ok", "ok", "not_found"))
})
