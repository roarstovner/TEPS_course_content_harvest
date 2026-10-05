# Tests for extract_fulltext_from_raw() (#256)

test_that("html rows are their blocks as text, with post_fn; PDF rows keep their text", {
  config <- list(strategy = "html_pdf_discovery", section_strategy = "html",
                 selector = "main", exclude = ".contact", post_fn = toupper)
  df <- tibble::tibble(
    course_id = c("a", "b"),
    html = c("<html><body><nav>x</nav><main><h2>Innhold</h2><p>plan</p><p>mer</p>
              <div class='contact'>Ola</div></main></body></html>", NA),
    extracted_text = c("stale text from harvest time", "text from a PDF")
  )
  expect_equal(extract_fulltext_from_raw(df, config),
               c("INNHOLD\n\nPLAN\n\nMER", "text from a PDF"))
})

test_that("the blocks as text keep lines, paragraphs and split paragraphs", {
  cfg <- list(reader = "html", container = "main", heading = "h2", sub = "p")
  b <- page_blocks("<main><h2>Innhold</h2><div>Linje</div><ul><li>A</li><li>B</li></ul>
    <p><em>Faget i praksis</em>Studentene</p><p>Slutt</p></main>", NA, cfg)
  expect_equal(.blocks_text(b), "Innhold\nLinje\nA\nB\n\nFaget i praksis\nStudentene\n\nSlutt")
})

test_that("noop gives NA, pdf_split keeps the harvested text, usn is cleaned", {
  df <- tibble::tibble(html = "  line keyboard_backspace\n\n\n\nnext  ",
                       extracted_text = "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "noop")), NA_character_)
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "pdf_split")), "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "shadow_dom")), "line\n\nnext")
})
