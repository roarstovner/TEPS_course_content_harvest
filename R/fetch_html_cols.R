#' Fetch HTML content from URLs
#'
#' Downloads HTML content from a vector of URLs and returns a data frame
#' with the results, including any errors encountered.
#'
#' @param urls A character vector containing URLs to fetch
#'
#' @return A data frame with columns:
#'   \item{html}{The HTML content (or NULL if failed)}
#'   \item{html_success}{Logical indicating if the fetch was successful}
#'   \item{html_error}{Error object if fetch failed (or NULL if successful)}
#'
#' @examples
#' \dontrun{
#' urls <- c("https://example.com", "https://posit.co")
#' results <- fetch_html_cols(urls)
#' }
#'
#' @export
#' @noRd
fetch_html_cols <- function(urls, .progress = TRUE) {
  is_valid <- !is.na(urls)

  http_resps <- purrr::map(urls[is_valid], purrr::safely(function(url) fetch_html_cols_single(url)), .progress = .progress)
  
  html <- character(length(urls))
  html[!is_valid] <- NA_character_
  html[is_valid] <- purrr::map_chr(http_resps, function(x) if (is.null(x$result)) NA_character_ else x$result)
  
  html_error <- vector("list", length(urls))
  html_error[is_valid] <- purrr::map(http_resps, "error")
  
  html_success <- logical(length(urls))
  html_success[!is_valid] <- NA
  html_success[is_valid] = vapply(html_error[is_valid], is.null, logical(1))

  tibble::tibble(html, html_error, html_success)
}

fetch_html_cols_single <- function(url) {
  url |>
    httr2::request() |>
    httr2::req_user_agent(
      "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
    ) |> 
    httr2::req_perform() |>
    httr2::resp_body_string()
}
