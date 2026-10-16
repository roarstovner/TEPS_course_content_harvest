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

# A site that answers 403 or 429 refuses the harvest: its harvest stops, so
# it is not asked again and again (harvest_all() goes on with the next site;
# the pages fetched stay in the checkpoint).
stop_if_blocked <- function(status, url) {
  if (isTRUE(status %in% BLOCKED_STATUS)) {
    stop("the site refuses the harvest (HTTP ", status, " at ", url, "): stopped, ",
         "harvest it again later", call. = FALSE)
  }
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
# long. Any other answer is final: a 404 is not tried again, and a block
# (HTTP 403, 429) stops the site's harvest (fetch_html_with_checkpoint()).
FETCH_TIMEOUT <- 60
FETCH_TRIES <- 3
FETCH_BACKOFF <- 30

# A request to a course site, with the user agent and the retry policy.
site_request <- function(url, user_agent = TEPS_USER_AGENT) {
  httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(FETCH_TIMEOUT) |>
    httr2::req_retry(max_tries = FETCH_TRIES, retry_on_failure = TRUE,
                     is_transient = function(resp) httr2::resp_status(resp) %in% c(500L, 502L, 503L, 504L),
                     backoff = function(i) FETCH_BACKOFF * 2^(i - 1))
}

fetch_html_cols_single <- function(url, institution = NULL, config = NULL) {
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
#' that indicates the course doesn't exist. A page that says the plan is
#' only in English is replaced by the English page.
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
  # "Emnebeskrivelsen finnes kun på engelsk": the plan is on the English page
  # (same layout); the row keeps the Norwegian URL
  if (stringr::str_detect(html_content, "Emnebeskrivelsen finnes kun på engelsk")) {
    return(fetch_html_cols_single_hvl(sub("/studier/studieprogram/emne/",
                                          "/en/studies-at-hvl/study-programmes/courses/", url, fixed = TRUE)))
  }

  resp
}