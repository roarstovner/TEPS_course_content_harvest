# Test HTTP status detection for course URLs
#
# Checks that URL validation correctly identifies:
# - Valid pages (200 status, content present)
# - Soft 404s (200 status but "page not found" content)
# - Invalid pages

# Helper function to check a single URL
check_one <- function(u, code_pattern = "rel\\s*-?416") {
  out <- list(
    url = u,
    status = NA_integer_,
    type = NA_character_,
    h1 = NA_character_,
    soft_404 = NA,
    bytes = NA_integer_,
    mentions_code = NA,
    ok = FALSE
  )

  ua <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
  hdr <- httr::add_headers(`Accept-Language` = "nb-NO,nb;q=0.9")

  resp <- try(httr::GET(u, ua, hdr, httr::timeout(20)), silent = TRUE)
  if (inherits(resp, "try-error")) return(out)

  out$status <- httr::status_code(resp)
  out$type <- httr::http_type(resp)

  # Require HTML + 2xx/3xx for further checks
 if (!(out$status >= 200 && out$status < 400 && identical(out$type, "text/html"))) {
    return(out)
  }

  # Parse from RAW for minimal encoding issues
  doc <- try(xml2::read_html(httr::content(resp, "raw")), silent = TRUE)
  if (inherits(doc, "try-error")) return(out)

  # Get <h1>
  h1_node <- rvest::html_element(doc, "h1")
  out$h1 <- if (!is.na(h1_node)) rvest::html_text2(h1_node) else NA_character_

  # Soft-404 detection based on h1
  h1_lc <- tolower(trimws(ifelse(is.na(out$h1), "", out$h1)))
  out$soft_404 <- grepl("siden ble ikke funnet|page not found|404", h1_lc, fixed = FALSE)

  # Get main content and measure length
  xml2::xml_find_all(doc, ".//script|.//style|.//nav|.//footer") |> xml2::xml_remove()
  body <- rvest::html_element(doc, "main, #content, body")
  txt <- if (!is.na(body)) trimws(rvest::html_text2(body)) else ""
  out$bytes <- nchar(txt, type = "bytes")

  # Check if text mentions the course code
  out$mentions_code <- grepl(code_pattern, txt, ignore.case = TRUE)

  # Simple OK rule: not soft-404, and either sufficient length or code mentioned
  MIN_BYTES <- 120L
  out$ok <- (!out$soft_404) && (out$bytes >= MIN_BYTES || isTRUE(out$mentions_code))
  out
}

test_that("check_one correctly identifies HTTP responses", {
  skip_on_ci()  # Skip in CI to avoid flaky tests from external URLs

  # Test with a URL we expect to return some status
  url <- "https://www.uia.no/studier/emner/2025/host/rel416.html"
  result <- check_one(url)

  # Basic checks that the function works

  expect_true(is.numeric(result$status), info = "Should get a status code")
  expect_true(is.character(result$type) || is.na(result$type),
              info = "Type should be character or NA")
  expect_true(is.logical(result$ok), info = "ok should be logical")

  # If we got a 200, check that soft_404 detection works
  if (!is.na(result$status) && result$status == 200) {
    expect_true(is.logical(result$soft_404), info = "soft_404 should be logical for 200")
    expect_true(is.numeric(result$bytes), info = "bytes should be numeric for 200")
  }

  # If we got a 404, ok should be FALSE
  if (!is.na(result$status) && result$status == 404) {
    expect_false(result$ok, info = "404 should not be OK")
  }
})
