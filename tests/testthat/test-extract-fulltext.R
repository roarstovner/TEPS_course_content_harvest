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

test_that("a table is one line per row, cells separated by tabs (#312)", {
  # hivolda's and inn's exam table: rows in thead/tbody, values in div.item;
  # uit's cells break with <br>, which html_text2() drops inside a <span>
  cfg <- list(reader = "html", container = "main", heading = "h2")
  b <- page_blocks("<main><h2>Vurdering</h2><table>
    <thead><tr><th>Vurderingsform</th><th>Andel</th><th>Kommentar</th></tr></thead>
    <tbody><tr><td><div class='form'><div class='item'>Hjemmeeksamen</div></div></td>
      <td><div class='item'>40</div></td><td></td></tr>
    <tr><td>Oppgave</td><td>60</td><td><span>Karakterregel:<br>A–E, stryk F</span></td></tr></tbody>
    </table></main>", NA, cfg)
  expect_equal(.blocks_text(b), paste0("Vurdering\nVurderingsform\tAndel\tKommentar\n",
                                       "Hjemmeeksamen\t40\t\nOppgave\t60\tKarakterregel: A–E, stryk F"))
})

test_that("a <br> inside an inline element is a space, not glue (#315)", {
  # a <br> straight under a block element stays a line break
  cfg <- list(reader = "html", container = "main", heading = "h2")
  b <- page_blocks("<main><h2>Vurdering</h2><p>Varighet: 45 minutter.<br>Neste</p>
    <div><span>45 minutter.<br>Det tas</span></div>
    <p><span>Kunnskapar<br></span>Studenten skal</p>
    <div>løp <a href='x'>Læringssenteret<br>Birkeland</a> og</div></main>", NA, cfg)
  expect_equal(.blocks_text(b), paste0("Vurdering\n\nVarighet: 45 minutter.\nNeste\n\n45 minutter. Det tas",
                                       "\n\nKunnskapar Studenten skal\n\nløp Læringssenteret Birkeland og"))
})

test_that("noop gives NA, pdf_split keeps the harvested text, usn is cleaned", {
  df <- tibble::tibble(html = "  line keyboard_backspace\n\n\n\nnext  ",
                       extracted_text = "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "noop")), NA_character_)
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "pdf_split")), "harvested")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "shadow_dom")), "line\n\nnext")
})
