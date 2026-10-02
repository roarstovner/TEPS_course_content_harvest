# Tests for extract_fulltext_from_raw() (#256)

test_that("html rows use the config selector and post_fn; PDF rows keep their text", {
  config <- list(strategy = "html_pdf_discovery", selector = "main",
                 selector_mode = "single", post_fn = toupper)
  df <- tibble::tibble(
    html = c("<html><body><nav>x</nav><main><p>plan</p></main></body></html>", NA),
    extracted_text = c("stale text from harvest time", "text from a PDF")
  )
  expect_equal(extract_fulltext_from_raw(df, config), c("PLAN", "text from a PDF"))
})

test_that("noop gives NA, pdf_split keeps the harvested text, usn is cleaned", {
  df <- tibble::tibble(html = "  line keyboard_backspace\n\n\n\nnext  ",
                       extracted_text = "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "noop")), NA_character_)
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "pdf_split")), "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "shadow_dom")), "line\n\nnext")
})
