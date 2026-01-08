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
#'
#' @examples
#' \dontrun{
#' urls <- c("https://example.com", "https://posit.co")
#' results <- fetch_html_cols(urls)
#' }
#'
#' @export
#' @noRd
fetch_html_cols <- function(urls, institution = NULL, .progress = TRUE) {
  is_valid <- !is.na(urls) & nzchar(urls)
  
  if (is.null(institution)) {
    institution <- rep(NA_character_, length(urls))
  }
  
  http_resps <- purrr::map2(
    urls[is_valid],
    institution[is_valid],
    purrr::safely(function(url, inst) fetch_html_cols_single(url, inst)),
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
  
  tibble::tibble(html = html, html_error = html_error, html_success = html_success)
}

fetch_html_cols_single <- function(url, institution = NULL) {
  # Institution-specific overrides (optional)
  if (!is.null(institution)) {
    resp <- switch(
      institution,
      "ntnu" = fetch_html_cols_single_ntnu(url),
      # "uio"  = fetch_html_cols_single_uio(url),  # behold hvis du har den
      NULL
    )
    if (!is.null(resp)) return(resp)
  }
  
  # Default fetch
  url |>
    httr2::request() |>
    httr2::req_user_agent(
      "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
    ) |>
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
  resp <- url |>
    httr2::request() |>
    httr2::req_user_agent(
      "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
    ) |>
    httr2::req_perform()
  
  html_content <- httr2::resp_body_string(resp)
  
  no_info_pattern <- "Det finnes ingen informasjon for dette studieåret|There is no information available for the given academic year"
  if (stringr::str_detect(html_content, no_info_pattern)) {
    rlang::abort("No course information available for this academic year", class = "ntnu_no_info_error")
  }
  
  resp
}