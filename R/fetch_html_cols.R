#' Fetch HTML content from URLs
#'
#' Downloads HTML content from a vector of URLs and returns a data frame
#' with the results, including any errors encountered.
#'
#' @param urls A character vector containing URLs to fetch
#'
#' @return A data frame with columns:
#'   \item{html}{Character vector with HTML content, or NA if failed}
#'   \item{html_success}{Logical indicating if the fetch was successful}
#'   \item{html_error}{Error object if fetch failed (or NULL if successful)}
#'   \item{http_status}{The response's HTTP status, also of an error page;
#'     NA without a response}
#'   \item{requested_at}{When the request was sent}
#'
#' @examples
#' \dontrun{
#' urls <- c("https://example.com", "https://posit.co")
#' results <- fetch_html_cols(urls)
#' }
#'
#' @export
#' @noRd
fetch_html_cols <- function(urls, institution = NULL, config = NULL,
                            .progress = "Fetching HTML") {
  is_valid <- !is.na(urls) & nzchar(urls)

  if (is.null(institution)) {
    institution <- rep(NA_character_, length(urls))
  }

  http_resps <- purrr::map2(
    urls[is_valid],
    institution[is_valid],
    function(url, inst) {
      requested_at <- Sys.time()
      c(purrr::safely(fetch_html_cols_single)(url, inst, config), list(requested_at = requested_at))
    },
    .progress = .progress
  )
  
  # html (character)
  html <- character(length(urls))
  html[!is_valid] <- NA_character_
  html[is_valid] <- purrr::map_chr(http_resps, function(x) {
    if (is.null(x$result)) return(NA_character_)
    httr2::resp_body_string(x$result)
  })
  
  # errors (list)
  html_error <- vector("list", length(urls))
  html_error[!is_valid] <- replicate(sum(!is_valid), NULL, simplify = FALSE)
  html_error[is_valid] <- purrr::map(http_resps, "error")
  
  # success (logical)
  html_success <- logical(length(urls))
  html_success[!is_valid] <- NA
  html_success[is_valid] <- vapply(html_error[is_valid], is.null, logical(1))

  # the request, for the fetch ledger (fetch_ledger())
  http_status <- rep(NA_integer_, length(urls))
  http_status[is_valid] <- vapply(http_resps, function(x) http_status_of(x$result %||% x$error), integer(1))
  requested_at <- .POSIXct(rep(NA_real_, length(urls)))
  requested_at[is_valid] <- .POSIXct(vapply(http_resps, function(x) as.numeric(x$requested_at), numeric(1)))

  tibble::tibble(html = html, html_error = html_error, html_success = html_success,
                 http_status = http_status, requested_at = requested_at)
}

# The HTTP status of a response, or of the response a fetch error holds
# (httr2's HTTP errors; `http_status` of the fetchers' own not-found errors);
# NA when there was no answer.
http_status_of <- function(x) {
  if (inherits(x, "httr2_response")) return(httr2::resp_status(x))
  if (is.list(x) && !is.null(x$http_status)) return(as.integer(x$http_status))
  if (is.list(x) && inherits(x$resp, "httr2_response")) return(httr2::resp_status(x$resp))
  NA_integer_
}

# A site refuses the harvest when it answers 429 (too many requests), or 403
# (forbidden) to FORBIDDEN_RUN requests in a row and then also to a page
# known to be public: its harvest stops, so it is not asked again and again
# (harvest_all() goes on with the next site; the pages fetched before stay in
# the checkpoint). A 403 among other answers is the page's own (a course page
# closed to the public, as at MF on 2026-10-17): outcome "forbidden"
# (fetch_outcome()), and the harvest goes on; so are 403s in a row while the
# public page still answers 200 (MF's PHI courses, next to each other in a
# sample ordered by course code).
FORBIDDEN_RUN <- 5L

# The check for one loop of requests to a site of the institution `config`
# names: called with each answer's status (NA: no answer) and URL, it stops
# the loop when the site refuses the harvest, after `before_stop(run)`; else
# it returns `run`, the number of 403s in a row so far (0 after any other
# answer). After FORBIDDEN_RUN 403s in a row it requests a page of the same
# host known to be public (probe_request()): the last one that answered 200
# in this loop, else the host's latest page in the institution's raw store
# (known_public_url()). A 200 resets the run; any other answer, or no such
# page, stops the loop.
refusal_watch <- function(config = NULL) {
  run <- 0L
  public <- character()   # by host: the last URL that answered 200
  function(status, url, before_stop = NULL) {
    host <- url_host(url)
    if (isTRUE(status == 200L)) public[host] <<- url
    run <<- if (isTRUE(status == 403L)) run + 1L else 0L
    refused <- isTRUE(status == 429L)
    probe <- ""
    if (run >= FORBIDDEN_RUN) {
      known <- if (is.na(public[host])) known_public_url(config$name, host) else unname(public[host])
      answer <- if (is.na(known)) NA_integer_ else probe_request(known, config)
      refused <- !isTRUE(answer == 200L)
      if (!refused) run <<- 0L   # the site answers: the 403s are the pages' own
      probe <- if (is.na(known)) "; no page known to be public to check" else
        paste0("; ", known, ", known to be public, answered ", if (is.na(answer)) "nothing" else paste("HTTP", answer))
    }
    if (refused) {
      if (!is.null(before_stop)) before_stop(run)
      stop("the site refuses the harvest (HTTP ", status, " at ", url,
           if (run > 0L) paste0(", ", run, " in a row"), probe, "): stopped, harvest it again later",
           call. = FALSE)
    }
    invisible(run)
  }
}

# The host of URLs, in lower case.
url_host <- function(url) tolower(sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]*).*$", "\\1", url))

# The latest URL of `host` that gave a page in the institution's raw store
# (read_raw(content = FALSE)); NA if none.
known_public_url <- function(institution, host) {
  files <- if (length(institution)) harvest_files(institution) else character()
  if (!length(files)) return(NA_character_)
  rows <- read_raw(files, content = FALSE)
  url <- rows$url[!is.na(rows$html) & !is.na(rows$url) & url_host(rows$url) %in% host]
  if (length(url)) url[length(url)] else NA_character_
}

# The request of a page known to be public (refusal_watch()), after the
# institution's pause and with its user agent; its HTTP status (NA: no
# answer). harvest_all() adds it to the run's fetch ledger (take_probes()).
probe_request <- function(url, config = NULL) {
  polite_pause(config$request_delay)
  requested_at <- Sys.time()
  agent <- if (identical(config$user_agent, "browser")) BROWSER_USER_AGENT else TEPS_USER_AGENT
  status <- http_status_of(tryCatch(httr2::req_perform(site_request(url, agent)), error = identity))
  if (length(config$name)) {
    assign(config$name, rbind(take_probes(config$name),
                              data.frame(url = url, requested_at = requested_at, http_status = status)),
           envir = .probes)
  }
  status
}

# The probe requests not yet in a fetch ledger, by institution.
.probes <- new.env()

# An institution's probe requests (url, requested_at, http_status) since the
# last call, or NULL.
take_probes <- function(institution) {
  out <- get0(institution, envir = .probes, inherits = FALSE)
  assign(institution, NULL, envir = .probes)
  out
}

# Pause between requests to the same site. An institution can override the
# default with `request_delay` in institution_config.R (seconds, or a
# c(min, max) range), e.g. to honour a robots.txt Crawl-delay.
DEFAULT_REQUEST_DELAY <- c(3, 5)

polite_pause <- function(delay = NULL) {
  delay <- delay %||% DEFAULT_REQUEST_DELAY
  Sys.sleep(runif(1, min(delay), max(delay)))
}

TEPS_USER_AGENT <- "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
BROWSER_USER_AGENT <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                            "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

# Retries by outcome (fetch_outcome(); README "Fetch Ledger and Retries"): a
# request without an answer in FETCH_TIMEOUT seconds or a failed connection
# ("timeout") and HTTP 500, 502, 503 or 504 ("server_error") are tried again,
# FETCH_TRIES times in all, after FETCH_BACKOFF seconds and then twice as
# long. Any other answer is final: a 404 or a 403 is not tried again, and a
# site that refuses the harvest stops it (refusal_watch()).
FETCH_TIMEOUT <- 60
FETCH_TRIES <- 3
FETCH_BACKOFF <- 30

# A request to a course site, with the user agent and the retry policy. A URL
# robots.txt disallows is refused before any request (robots_allows()), with
# an error of class "robots_disallowed" (outcome "robots" in the ledger).
site_request <- function(url, user_agent = TEPS_USER_AGENT) {
  if (isFALSE(robots_allows(url))) rlang::abort(robots_refusal(url), class = "robots_disallowed")
  httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(FETCH_TIMEOUT) |>
    httr2::req_retry(max_tries = FETCH_TRIES, retry_on_failure = TRUE,
                     is_transient = function(resp) httr2::resp_status(resp) %in% c(500L, 502L, 503L, 504L),
                     backoff = function(i) FETCH_BACKOFF * 2^(i - 1))
}

# A course site's page as an HTML document (site_request()).
site_html <- function(url) {
  rvest::read_html(httr2::resp_body_string(httr2::req_perform(site_request(url))))
}

# --- robots.txt ---------------------------------------------------------------
# The harvest requests no URL whose path a host's robots.txt disallows for all
# user agents (README "robots.txt"). ROBOTS_FILE holds the robots.txt of every
# host the harvest requests, read before the first request to it: `disallow`
# and `allow` are its rules for `User-agent: *` that touch course plans, as
# robots.txt path patterns separated by spaces.
ROBOTS_FILE <- "data/robots_txt.csv"

#' Whether robots.txt lets the harvest request URLs
#'
#' By the rules of each URL's host in `file` (RFC 9309): a URL is disallowed
#' when a `disallow` pattern matches its path and query from the start (`*`
#' any characters, a final `$` the end) and no `allow` pattern as long or
#' longer does. A disallowed URL is allowed after all when the host's
#' institution has recorded a permission (`robots_permission` in
#' R/institution_config.R). A host's /robots.txt is always allowed.
#'
#' @param url URLs.
#' @param file The robots.txt table (`ROBOTS_FILE`).
#' @return TRUE (allowed), FALSE (disallowed: not to be requested) or NA (no
#'   URL). A host with no row in `file`, whose robots.txt has not been read,
#'   is an error of class "robots_unread", so the harvest stops before its
#'   first request to it.
robots_allows <- function(url, file = ROBOTS_FILE) {
  url <- as.character(url)
  host <- url_host(url)
  path <- sub("#.*", "", sub("^[A-Za-z][A-Za-z0-9+.-]*://[^/?#]*", "", url))
  path <- ifelse(startsWith(path, "/"), path, paste0("/", path))
  ask <- !is.na(url) & path != "/robots.txt"
  out <- ifelse(is.na(url), NA, TRUE)
  if (!any(ask)) return(out)
  rules <- utils::read.csv(file, colClasses = "character", na.strings = "", encoding = "UTF-8")
  row <- match(host, tolower(rules$host))
  unread <- unique(host[ask & is.na(row)])
  if (length(unread)) {
    rlang::abort(paste0("the robots.txt of ", paste(unread, collapse = ", "), " has not been read: ",
                        "read it and add its row to ", file, " before the first request to the host ",
                        "(add-institution skill, Step 0)"), class = "robots_unread")
  }
  # the length of the longest pattern that matches the path, 0 for none
  longest <- function(patterns, path) {
    p <- strsplit(patterns, " +")[[1]]
    p <- p[!is.na(p) & nzchar(p)]
    max(0, nchar(p)[vapply(robots_rx(p), grepl, logical(1), x = path, perl = TRUE)])
  }
  for (k in which(ask & !is.na(rules$disallow[row]))) {
    out[k] <- longest(rules$disallow[row[k]], path[k]) <= longest(rules$allow[row[k]], path[k])
  }
  configs <- get0("institution_configs", ifnotfound = list())
  permitted <- vapply(rules$institution, function(i) !is.na(i) && nzchar(configs[[i]]$robots_permission %||% ""),
                      logical(1), USE.NAMES = FALSE)
  out | (ask & permitted[row])
}

# robots.txt path patterns as regular expressions: from the start of the
# path, `*` any characters, a final `$` the end of the URL.
robots_rx <- function(pattern) {
  end <- endsWith(pattern, "$")
  rx <- gsub("([.\\\\+?^${}()|\\[\\]])", "\\\\\\1", sub("\\$$", "", pattern), perl = TRUE)
  paste0("^", gsub("*", ".*", rx, fixed = TRUE), ifelse(end, "$", ""), recycle0 = TRUE)
}

# Why a URL was not requested, as the fetch error that fetch_outcome() reads
# as "robots".
robots_refusal <- function(url) {
  paste0("robots.txt disallows ", url, " (", ROBOTS_FILE, "): not requested")
}

# A course page: the response to its URL, or, for a course whose Norwegian
# page only says that the plan is on the English page (`english_page` in
# R/institution_config.R), the English page's, requested after the
# institution's pause (robots.txt applies as to any request; an error there
# is the row's error).
fetch_html_cols_single <- function(url, institution = NULL, config = NULL) {
  resp <- fetch_page(url, institution, config)
  ep <- config$english_page
  english <- if (is.null(ep)) url else sub(ep$from, ep$to, url, fixed = TRUE)
  if (english != url && grepl(ep$notice, httr2::resp_body_string(resp), perl = TRUE)) {
    polite_pause(config$request_delay)
    resp <- fetch_page(english, institution, config)
  }
  resp
}

# The response to one request of a course page.
fetch_page <- function(url, institution = NULL, config = NULL) {
  # Config-driven fetch: use fetch_fn if provided
  if (!is.null(config) && !is.null(config$fetch_fn)) {
    return(config$fetch_fn(url))
  }

  # Legacy fallback: institution-specific overrides (for callers without config)
  if (!is.null(institution) && is.null(config)) {
    resp <- switch(
      institution,
      "ntnu" = fetch_html_cols_single_ntnu(url),
      "hvl"  = fetch_html_cols_single_hvl(url),
      NULL
    )
    if (!is.null(resp)) return(resp)
  }

  # User agent: config-driven or legacy fallback
  browser <- if (!is.null(config)) identical(config$user_agent, "browser") else identical(institution, "hiof")
  site_request(url, if (browser) BROWSER_USER_AGENT else TEPS_USER_AGENT) |>
    httr2::req_perform()
}

#' Fetch HTML for a single NTNU course URL
#'
#' Fetches the HTML and checks for the "no information available" message
#' that indicates the course plan doesn't exist for that year.
#'
#' @param url Character string containing the NTNU course URL
#'
#' @return Character string with HTML content, or throws an error if the page
#'   shows 'There is no information avaliable for the given academic year'
#'
#' @noRd
fetch_html_cols_single_ntnu <- function(url) {
  resp <- site_request(url) |> httr2::req_perform()
  
  html_content <- httr2::resp_body_string(resp)
  
  no_info_pattern <- "Det finnes ingen informasjon for dette studieåret|There is no information available for the given academic year"
  if (stringr::str_detect(html_content, no_info_pattern)) {
    rlang::abort("No course information available for this academic year", class = "ntnu_no_info_error",
                 http_status = httr2::resp_status(resp))
  }
  
  resp
}

#' Fetch HTML for a single HVL course URL
#'
#' Fetches the HTML and checks for the "course not found" error page
#' that indicates the course doesn't exist, also on the English site. (A
#' page that says the plan is only in English: `english_page` in the
#' config.)
#'
#' @param url Character string containing the HVL course URL
#'
#' @return httr2 response object, or throws an error if the page
#'   shows 'Vi kunne ikkje finne emnet du ser etter'
#'
#' @noRd
fetch_html_cols_single_hvl <- function(url) {
  resp <- site_request(url) |> httr2::req_perform()

  html_content <- httr2::resp_body_string(resp)

  if (stringr::str_detect(html_content, paste0("Vi kunne ikkje finne emnet du ser etter|",
                                               "Please try again at the search for study plans"))) {
    rlang::abort("Course not found (soft 404)", class = "hvl_not_found_error",
                 http_status = httr2::resp_status(resp))
  }

  resp
}