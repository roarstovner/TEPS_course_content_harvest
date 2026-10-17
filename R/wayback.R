# R/wayback.R
# The Internet Archive's Wayback Machine (web.archive.org): which captures it
# holds of a course page (its CDX API), and the address of a capture as it
# was archived. Not used by the harvest or the pipeline: tested on
# 2026-10-16 as a source of the plans a site no longer shows (README "Sites
# that show only the current plan").
#
# web.archive.org has no robots.txt (404, 2026-10-16). Its terms of use give
# access "for scholarship and research purposes only" and ask not to
# "interfere with the work of other users or Archive personnel, servers, or
# resources", so requests go one at a time, WAYBACK_DELAY seconds apart, with
# the project's user agent, and a 429 (too many requests) stops.

WAYBACK_DELAY <- c(6, 8)

#' The captures of a URL, or of every URL under a prefix, in the Wayback Machine
#'
#' One request to the CDX API, after a pause of `WAYBACK_DELAY` seconds.
#'
#' @param url The URL, without the scheme ("www.uio.no/studier/emner/uv/").
#' @param match_type "exact", "prefix" (every URL under `url`), "host" or
#'   "domain". The match ignores the scheme and "www.".
#' @param from,to The first and last capture time, as 1 to 14 digits of
#'   yyyyMMddhhmmss ("202108" is August 2021; `to` includes the whole month).
#' @param filter "[!]field:regex", one or several (all must hold). The regex
#'   matches anywhere in the field: anchor it with `$` to match the end.
#' @param collapse A field, "urlkey" for the first capture of each URL,
#'   "digest" for one capture per change of a page's content.
#' @param fl The fields: urlkey, timestamp, original, mimetype, statuscode,
#'   digest, length.
#' @param limit The most rows to return.
#' @return A tibble, one row per capture, the fields as character columns.
wayback_cdx <- function(url, match_type = "exact", from = NULL, to = NULL, filter = NULL,
                        collapse = NULL, fl = c("timestamp", "original", "statuscode", "digest"),
                        limit = NULL) {
  polite_pause(WAYBACK_DELAY)
  resp <- wayback_cdx_request(url, match_type, from, to, filter, collapse, fl, limit) |>
    httr2::req_perform()
  wayback_parse_cdx(httr2::resp_body_string(resp))
}

# The CDX request of wayback_cdx(): the project's user agent and retries
# (site_request()), a longer timeout, since a prefix query scans many captures.
wayback_cdx_request <- function(url, match_type = "exact", from = NULL, to = NULL, filter = NULL,
                                collapse = NULL, fl = c("timestamp", "original", "statuscode", "digest"),
                                limit = NULL) {
  site_request("https://web.archive.org/cdx/search/cdx") |>
    httr2::req_timeout(180) |>
    httr2::req_url_query(url = url, matchType = match_type, output = "json",
                         fl = paste(fl, collapse = ","), from = from, to = to,
                         collapse = collapse, limit = limit) |>
    httr2::req_url_query(filter = filter, .multi = "explode")
}

# The CDX API's JSON (a header row, then one row per capture) as a tibble.
wayback_parse_cdx <- function(json) {
  rows <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if (!length(rows)) return(tibble::tibble())
  fields <- unlist(rows[[1]])
  cols <- lapply(seq_along(fields), function(j) {
    vapply(rows[-1], function(r) as.character(r[[j]] %||% NA_character_), character(1))
  })
  tibble::as_tibble(stats::setNames(cols, fields))
}

#' The address of a capture as archived
#'
#' The `id_` flag gives the page as the site served it, without the Wayback
#' Machine's banner and rewritten links.
#'
#' @param timestamp The capture's timestamp (14 digits, from wayback_cdx()).
#' @param original The captured URL (`original` from wayback_cdx()).
#' @return Character, the capture's URL at web.archive.org.
wayback_capture_url <- function(timestamp, original) {
  paste0("https://web.archive.org/web/", timestamp, "id_/", original)
}
