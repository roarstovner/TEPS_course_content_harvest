# Tests for extract_fulltext_from_raw()

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

test_that("a table is one line per row, cells separated by tabs", {
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

test_that("a <br> inside an inline element is a space, not glue", {
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
  # usn: a banner left from the page shown before
  df$html <- paste0("Emneplan for Norsk 1\n\n  MERK:\n  Emneplanen for dette semesteret er ikke klar ennå.\n\n",
                    "For at du skal få et inntrykk av hva emnet inneholder, viser vi derfor ",
                    "emneplanen fra et tidligere semester.\n\nEmnekode:")
  expect_equal(extract_fulltext_from_raw(df, list(strategy = "shadow_dom")),
               "Emneplan for Norsk 1\n\nEmnekode:")
})

test_that("uit: the year picker and the contacts go, the responsible unit stays", {
  txt <- paste("Om emnet", "Tekst om emnet.", "Error rendering component", "Andre år og semester",
               "Kontakt oss",
               "Ansvarlig enhet:Institutt for lærerutdanning og pedagogikkE-post: studie@hjelp.uit.no",
               "Kari Nordmann", "Professor", sep = "\n")
  expect_equal(.pre_uit(txt),
               "Om emnet\nTekst om emnet.\n\nAnsvarlig enhet\nInstitutt for lærerutdanning og pedagogikk")
  # in no section, also when the text is read for the sections
  out <- page_sections(.empty_blocks(), .pre_uit(txt), list(reader = "text"))
  expect_equal(out$raw_text, "Tekst om emnet.")
  # the banner goes also after the title and facts
  expect_equal(.pre_uit("Emnekode: LER-1\nTittel\nOBS! Dette emnet tilhører et tidligere semester / år\nOm emnet\nTekst."),
               "Emnekode: LER-1\nTittel\nOm emnet\nTekst.")
  # a page whose plan did not render stays empty
  expect_equal(.pre_uit("Om emnet\nError rendering component\nKontakt oss\nAnsvarlig enhet: Institutt"), "")
})

test_that("uit: link labels to the exam page go, plan text with such a link stays", {
  html <- "<div id='maincontents'><h2>Eksamen</h2><p>Skoleeksamen, 4 timer.</p>
    <p>Alt du trenger å vite om før, under og etter eksamen, oppmelding, fravær klage og vitnemål:
      <a href='https://uit.no/eksamen'>UiTs samleside om eksamen </a></p>
    <p><a href='https://uit.no/eksamen#modal_717927'>Her finner du mer informasjon om eksamen i nedlagte emner</a></p>
    <p>Ved sykdom, se <a href='https://uit.no/eksamen'>regler for fravær</a>.</p></div>"
  b <- page_blocks(html, NA, .block_cfg(get_institution_config("uit")))
  expect_equal(.blocks_text(b), "Eksamen\n\nSkoleeksamen, 4 timer.\n\nVed sykdom, se regler for fravær.")
})

test_that("steiner: a PDF emne joins the bachelor's DBH name, PEL1.4/1.5 also the 1-7 programme's M-PEL2", {
  key <- function(x) lapply(.steiner_normalize_emne_name(x), .steiner_remap_pel_emne)
  expect_equal(key("Pedagogikk og elevkunnskap1, emne 5. FoU-oppgave, kunnskapsteori og forskningsmetode."),
               list(c("pedagogikk og elevkunnskap1, emne 5", "pedagogikk og elevkunnskap2, emne 2")))
  expect_equal(key(c("Pedagogikk og elevkunnskap 2, emne 1", "Norsk 1, emne 3")),
               list("pedagogikk og elevkunnskap2, emne 1", "norsk1, emne 3"))
  expect_equal(key("Pedagogikk og elevkunnskap 1, emne 4")[[1]][2], "pedagogikk og elevkunnskap2, emne 1")
})
