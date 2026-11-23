# Tests for the block readers — R/blocks.R

html_reader <- function(heading = "h2", sub = NULL, ...) {
  list(reader = "html", container = "main", heading = heading, sub = sub, ...)
}
roles <- function(b) paste(b$role, b$section, sep = ":")

test_that("the html reader gives headings, sub-headings and text in order", {
  b <- page_blocks("<main><h2>Læringsutbytte</h2><p>Kunnskap</p><ul><li>Studenten kan</li></ul>
    <h2>Nyttige lenker</h2><p>Ola</p></main>", NA, html_reader(sub = "p"))
  expect_equal(b$text, c("Læringsutbytte", "Kunnskap", "Studenten kan", "Nyttige lenker", "Ola"))
  expect_equal(roles(b), c("heading:learning_outcomes", "sub:learning_outcomes", "text:NA",
                           "heading:NA", "text:NA"))
})

test_that("text next to headings is kept, and inline runs stay one block", {
  b <- page_blocks("<main><div><h2>Innhold</h2>Emnet tar for seg <b>norsk</b> grammatikk.
    <p>Mer.</p></div></main>", NA, html_reader())
  expect_equal(b$text, c("Innhold", "Emnet tar for seg norsk grammatikk.", "Mer."))
})

test_that("blocks inside an inline element keep their breaks (nmbu 2026-04)", {
  html <- "<main><h2>Om emnet</h2><p><strong>Frekvens:</strong><span class='block'><p>Årlig</p>
    <p>Tilbys første gang 2025-2026.</p></span></p><p><strong>Språk:</strong><span>Norsk</span></p></main>"
  b <- page_blocks(html, NA, html_reader(sub = "p"))
  expect_equal(b$text[-1], c("Frekvens:\n\nÅrlig\n\nTilbys første gang 2025-2026.", "Språk:Norsk"))
})

test_that("excluded nodes are not read and the container limits the page", {
  b <- page_blocks("<body><nav>Meny</nav><main><h2>Innhold</h2><p>Tekst</p>
    <div class='contact'>Ola Nordmann</div></main></body>", NA,
    html_reader(exclude = ".contact"))
  expect_equal(b$text, c("Innhold", "Tekst"))
})

test_that("a scope element is marked with start and end blocks", {
  b <- page_blocks("<main><h2>Innhold</h2><p>A</p><details><summary>Pensum</summary>
    <p>Bok</p></details><p>B</p></main>", NA,
    html_reader(heading = "h2, summary", scope = "details"))
  expect_equal(b$role, c("heading", "text", "start", "heading", "text", "end", "text"))
  expect_equal(b$section[4], "reading_list")
})

test_that("an untitled element is a section of its own without a heading", {
  b <- page_blocks("<main><h1>Tittel</h1><div class='lead'>Kort om emnet.</div>
    <p>Mer</p><h2>Pensum</h2><p>Bok</p></main>", NA,
    html_reader(untitled = c("div.lead" = "course_content")))
  expect_equal(roles(b), c("text:NA", "start:NA", "heading:course_content", "text:NA",
                           "end:NA", "text:NA", "heading:reading_list", "text:NA"))
  expect_equal(sectionize(b)$raw_text, c("Kort om emnet.", "Bok"))
  # the fulltext is the same as without the option
  expect_equal(.blocks_text(b), .blocks_text(page_blocks(
    "<main><h1>Tittel</h1><div class='lead'>Kort om emnet.</div><p>Mer</p><h2>Pensum</h2><p>Bok</p></main>",
    NA, html_reader())))
})

test_that("a sub-heading paragraph splits into the sub-heading and its text", {
  sub <- function(html) page_blocks(paste0("<main><h2>Innhold</h2>", html, "</main>"),
                                    NA, html_reader(sub = "p"))[-1, ]
  lead <- sub("<p><em>Faget i praksis</em>I løpet av emnet ...</p>")
  expect_equal(roles(lead), c("sub:teaching_methods", "text:NA"))
  expect_equal(lead$text, c("Faget i praksis", "I løpet av emnet ..."))
  tail <- sub("<p>Motorisk utvikling.<br>Faget i praksis</p>")
  expect_equal(roles(tail), c("text:NA", "sub:teaching_methods"))
  label <- sub("<p><strong>Vurdering for studentar som tar faget 3. studieår</strong></p>")
  expect_equal(roles(label), "sub:NA")
  leadin <- sub("<p>Emnet har følgende obligatoriske aktiviteter:</p>")
  expect_equal(roles(leadin), "sub:coursework_requirements")
  expect_true(leadin$keep)
  expect_equal(roles(sub("<p>Studentene leser om arbeidskrav i skolen.</p>")), "text:NA")
})

test_that("a numbered coursework requirement is a kept coursework sub-heading", {
  sub <- function(html) page_blocks(paste0("<main><h2>Undervisning</h2>", html, "</main>"),
                                    NA, html_reader(sub = "p"))[-1, ]
  for (h in c("<p><strong>Arbeidskrav II: Helseundervisning</strong></p>",
              "<p><strong>Arbeidskrav III folkehelsearbeid i praksis</strong></p>",
              "<p>Obligatorisk arbeidskrav 1</p>", "<p>Arbeidskrav 2 - Refleksjonsnotat</p>")) {
    b <- sub(h)
    expect_equal(roles(b), "sub:coursework_requirements", info = h)
    expect_true(b$keep, info = h)
  }
  # a bold lead: the paragraph stays whole, so the fulltext does not change
  lead <- sub("<p><strong>Arbeidskrav 1:</strong> Et refleksjonsnotat.</p>")
  expect_equal(roles(lead), "sub:coursework_requirements")
  expect_equal(lead$text, "Arbeidskrav 1: Et refleksjonsnotat.")
  expect_equal(roles(sub("<p><strong>Arbeidskrav 1</strong> skal leveres i Canvas.</p>")), "text:NA")
  # sentences, a title after a space in plain text, and other numbered headings are not
  for (h in c("<p>Arbeidskrav 4 skal være godkjent før yrkespraksis kan påbegynnes</p>",
              "<p>Arbeidskrav 1-3 må være gjennomført innen 20.09</p>",
              "<p>Arbeidskrav 1 leveres i Canvas.</p>", "<p>Arbeidskrav 2 refleksjonsnotat</p>",
              "<p><strong>Arbeidskrav i emnet</strong></p>", "<p><strong>Eksamen 1</strong></p>")) {
    expect_false(identical(sub(h)$section[1], "coursework_requirements"), info = h)
  }
})

test_that("a paragraph in a list item is text unless the item holds a heading", {
  b <- page_blocks("<main><h2>Innhold</h2><ul><li><p>Pensum</p></li></ul>
    <ul><li><h2>Vurdering</h2><p>Pensum</p></li></ul></main>", NA, html_reader(sub = "p"))
  expect_equal(roles(b), c("heading:course_content", "text:NA", "heading:assessment",
                           "sub:reading_list"))
})

test_that("the text reader marks heading-shaped lines that switch section", {
  b <- page_blocks(NA, "Emnekode: X\n\nIntro.\n\nLæringsutbytte\nKunnskap\n\nKunnskap\nKan ting.",
                   list(reader = "text", text_header = "^Emnekode"))
  expect_equal(roles(b), c("heading:course_content", "text:NA", "heading:learning_outcomes",
                           "text:NA", "text:NA", "text:NA"))
  expect_equal(b$text[-1], c("Intro.", "Læringsutbytte", "Kunnskap", "Kunnskap", "Kan ting."))
})

test_that("text pages: the title and facts box before the plan are in no section or plan text", {
  usn <- "Emneplan for Norsk 1\n\nEmnekode:\n\nNO1\n\nInnholdsfortegnelse\n\nLæringsutbytte\n\nLæringsutbytte\n\nKan ting."
  cfg <- list(reader = "text")
  expect_equal(.text_header(stringr::str_split_1(usn, "\n"), cfg), list(n = 6L, intro = FALSE))
  expect_equal(text_plan_text(c(usn, "Læringsutbytte\n\nKan ting.", NA), cfg),
               c("Innholdsfortegnelse\n\nLæringsutbytte\n\nLæringsutbytte\n\nKan ting.", NA, NA))
  expect_equal(sectionize(page_blocks(NA, usn, cfg))$raw_text, "Kan ting.")
  # English pages: "Table of contents"
  en <- "Course plan for Law 1\n\nCourse code\n\nFE1\n\nTable of contents\n\nLearning outcome\n\nLearning outcome\n\nKnows things."
  expect_equal(.text_header(stringr::str_split_1(en, "\n"), cfg), list(n = 6L, intro = FALSE))
  expect_equal(text_plan_text(en, cfg),
               "Table of contents\n\nLearning outcome\n\nLearning outcome\n\nKnows things.")
  expect_equal(sectionize(page_blocks(NA, en, cfg))$raw_text, "Knows things.")
  # a metadata block: through its paragraph; the untitled text after it is the introduction
  pdf <- "Norsk 1\nStudiepoeng   10\nSemester   Vår\n\nIntro.\n\nLæringsutbytte\nKan ting."
  cfg <- list(reader = "text", text_header = "^(?:Studiepoeng|Semester)\\s{2,}")
  expect_equal(text_plan_text(pdf, cfg), "Intro.\n\nLæringsutbytte\nKan ting.")
  expect_equal(sectionize(page_blocks(NA, pdf, cfg))$section, c("course_content", "learning_outcomes"))
})

test_that("the json reader marks the parts named in `facts`", {
  json <- paste0('{"page": "EmneplanPage", "props": {"items": {"2024-2025": {"title": "Norsk 1",',
                 ' "table": [{"title": "Emnekode:", "content": "NO1"}],',
                 ' "accordions": [{"title": "Innhold", "content": "<p>Tekst.</p>"}]}}}}')
  html <- paste0("<html><body><script>", json, "</script></body></html>")
  b <- page_blocks(html, NA, list(reader = "json", facts = c("title", "table")), "nla_NO1_2024_autumn_1")
  expect_equal(b$facts, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(sectionize(b)$raw_text, "Tekst.")
  expect_false(any(page_blocks(html, NA, list(reader = "json"), "nla_NO1_2024_autumn_1")$facts))
  # its editor's <list><listItem> are list items, a line each
  html <- sub("<p>Tekst.</p>", '<list listType=\\\\"bulleted\\\\"><listItem>Delta på seminar</listItem><listItem>Lever utkast</listItem></list>', html)
  b <- page_blocks(html, NA, list(reader = "json"), "nla_NO1_2024_autumn_1")
  expect_equal(tail(b$text, 1), "Delta på seminar\nLever utkast")
})

test_that("fields are headings in scopes of their own; learning-outcome group labels are kept", {
  html <- "<article><h1>X1 Tittel</h1><div class='field-course-content'><div class='label'>Innhald</div>Tekst</div>
    <div class='field-learning-outcome-knowledge'><div class='label'>Kunnskapar</div>Kan</div>
    <div class='field-learning-outcome-skills'><div class='label'>Ferdigheiter</div>Gjer</div>
    <div class='programs'>Inngår i MGLU</div></article>"
  cfg <- list(reader = "html", container = "article", fields = c(
    "field-course-content" = "course_content",
    "field-learning-outcome-knowledge" = "learning_outcomes",
    "field-learning-outcome-skills" = "learning_outcomes"))
  b <- page_blocks(html, NA, cfg)
  h <- b[b$role == "heading", ]
  expect_equal(h$section, c("course_content", "learning_outcomes", "learning_outcomes"))
  expect_equal(b$text[b$role == "text"],
               c("X1 Tittel", "Tekst", "Kan", "Ferdigheiter\nGjer", "Inngår i MGLU"))
  s <- sectionize(b)
  expect_equal(s$raw_text, c("Tekst", "Kunnskapar\nKan\n\nFerdigheiter\nGjer"))
  expect_match(.blocks_text(b), "^X1 Tittel\nInnhald\nTekst")
})

test_that("the json reader reads nla's titles and contents", {
  f <- harvest_files("nla", test_path("../../data/raw"))
  skip_if_not(length(f) > 0, "harvested nla data not available")
  x <- read_raw(f)
  # the latest autumn: in harvests until 2026-10-17 most earlier offerings have
  # another year's page, whose JSON holds no plan for them
  i <- which(!is.na(x$html) & x$year == max(x$year) & x$semester_name == "Høst")[1]
  b <- page_blocks(x$html[i], NA, list(reader = "json"), x$course_id[i])
  expect_gt(sum(b$role == "heading" & !is.na(b$section)), 3)
})

test_that("'Litteratur:' inside learning outcomes stays there; the heading after it is the reading list", {
  b <- page_blocks("<main><h2>Studentens læringsutbytte ved bestått emne</h2><p>Kunnskap</p>
    <p>Litteratur:</p><p>Kan om forfattere.</p><h2>Litteratur</h2><p>Boka</p></main>", NA,
    html_reader(sub = "p"))
  s <- sectionize(b)
  expect_equal(s$section, c("learning_outcomes", "reading_list"))
  expect_equal(s$raw_text, c("Kunnskap\nLitteratur:\nKan om forfattere.", "Boka"))
  # outside learning outcomes it still switches to the reading list
  b <- page_blocks("<main><h2>Innhold</h2><p>Tekst.</p><p>Litteratur:</p><p>Boka</p></main>", NA,
                   html_reader(sub = "p"))
  expect_equal(sectionize(b)$section, c("course_content", "reading_list"))
})

test_that("on a page with a table of contents only whole headings switch outside it (usn)", {
  lines <- c("Innholdsfortegnelse", "Faglig innhold i emnet", "Vurderingsformer", "Annet",
             "Faglig innhold i emnet", "Hovedtema 1", "Kartlegging av smerte",
             "Vurdering av pasientens kartlagte smerte", "Etikk og smerte",
             "Vurderingsformer", "Hjemmeeksamen.", "Ny og utsatt eksamen", "Neste semester.",
             "Annet", "Evalueres jevnlig.", "Detaljert emnebeskrivelse", "Algebra.")
  s <- sectionize(.text_blocks(paste(lines, collapse = "\n\n"), list(reader = "text")))
  # a bullet that only contains "vurdering" stays in the course content; a
  # whole heading still switches: the resit heading drops its text, and the
  # detailed description under the unmapped "Annet" is course content
  expect_equal(s$section, c("course_content", "assessment", ".drop"))
  expect_equal(s$raw_text, c(
    "Hovedtema 1\nKartlegging av smerte\nVurdering av pasientens kartlagte smerte\nEtikk og smerte\n\nAlgebra.",
    "Hjemmeeksamen.", "Neste semester."))
})

test_that("without a table of contents only whole headings switch (uis PDFs)", {
  lines <- c("Emnekode: X", "Vurdering for læring", "Elevens egen vurdering.",
             "Litteratur og det monstrøse", "Myter.", "Læringsutbytte",
             "Vekt på forberedelse knyttet til kunnskapstilegnelse", "Kan ting.",
             "Ingen forkunnskaper kreves", "Arbeidsformer", "Forelesninger.",
             "Vilkår for å gå opp til eksamen/vurdering", "Arbeidskrav", "To oppgaver.",
             "Eksamen/vurdering", "Hjemmeeksamen.", "Litteratur:", "Litteratur (Litteratur og myte)",
             "Boka.")
  s <- sectionize(.text_blocks(paste(lines, collapse = "\n\n"), list(reader = "text", text_header = "^Emnekode")))
  expect_equal(s$section, c("course_content", "learning_outcomes", "teaching_methods",
                            "coursework_requirements", "assessment", "reading_list"))
  expect_equal(s$raw_text[1:2], c(
    "Vurdering for læring\nElevens egen vurdering.\nLitteratur og det monstrøse\nMyter.",
    "Vekt på forberedelse knyttet til kunnskapstilegnelse\nKan ting.\nIngen forkunnskaper kreves"))
  expect_match(s$raw_text[6], "^Litteratur \\(Litteratur og myte\\)")
})

test_that("an exact .drop heading needs no blank line before it (uis PDFs)", {
  text <- paste("Emnekode: X", "", "Tekster.", "Arbeidsmengd",
                "Et studieår er 40 veker.", "", "Litteratur", "", "Boka.", sep = "\n")
  s <- sectionize(.text_blocks(text, list(reader = "text", text_header = "^Emnekode")))
  expect_equal(s$section, c("course_content", ".drop", "reading_list"))
  expect_equal(s$raw_text[1:2], c("Tekster.", "Et studieår er 40 veker."))
})

test_that("a numbered requirement keeps its labels until the next heading (usn)", {
  text <- paste("Innholdsfortegnelse", "", "Faglig innhold", "", "Obligatorisk aktivitet", "",
                "Vurderingsformer", "", "Faglig innhold", "", "Pasient.", "",
                "Obligatorisk aktivitet", "", "Arbeidskrav 1: Samtale", "", "INNHOLD:",
                "Plan for perioden.", "", "VURDERING:", "Godkjent.", "", "Vurderingsformer", "",
                "Bestått/ikke bestått.", sep = "\n")
  s <- sectionize(.text_blocks(text, list(reader = "text")))
  expect_equal(s$section, c("course_content", "coursework_requirements", "assessment"))
  expect_match(s$raw_text[2], "INNHOLD:\nPlan for perioden.\nVURDERING:\nGodkjent.$")
  expect_equal(s$raw_text[3], "Bestått/ikke bestått.")
})

test_that("a page is read in NFC, without soft hyphens or zero-width characters", {
  # "a" + combining ring (U+030A) is one letter and the heading maps; a soft hyphen
  # (literal or &shy;), a zero-width space and a byte order mark go
  b <- page_blocks(paste0("<main><h2>Vilka\u030ar for a\u030a avlegge eksamen</h2>",
                          "<p>Studenten kan le\u0301se obli\u00adga&shy;torisk opp\u200bgave ",
                          "pa\u030a \ufeffnorsk</p></main>"), NA, html_reader())
  expect_equal(b$text, c("Vilk\u00e5r for \u00e5 avlegge eksamen",
                         "Studenten kan l\u00e9se obligatorisk oppgave p\u00e5 norsk"))
  expect_equal(b$section[1], "coursework_requirements")
  t <- page_blocks(NA, "Pensum\nfra\u030a b\u00f8ker \u00adog\u200b artikler", list(reader = "text"))
  expect_true(any(t$text == "fr\u00e5 b\u00f8ker og artikler"))
})

test_that("the text reader looks up the institution's own headings first", {
  text <- paste("Hensikten med emnet", "Emnet gir en innføring.", "", "Emnets læringsutbytte", "Studenten kan forklare.",
                "", "Vurderings-/eksamensform og vurderingsuttrykk:", "Skoleeksamen, A-F.", "", "Vurdering",
                "Muntlig.", sep = "\n")
  own <- c("emnets læringsutbytte" = "learning_outcomes",
           "vurderings-/eksamensform og vurderingsuttrykk" = "assessment", "vurdering" = ".drop")
  b <- .text_blocks(text, list(reader = "text", sub_map = own))
  expect_equal(b$section[b$role == "heading"], c("learning_outcomes", "assessment", ".drop"))
  # without them, only the heading map's: "Vurdering" is assessment
  b <- .text_blocks(text, list(reader = "text"))
  expect_equal(b$text[b$role == "heading"], "Vurdering")
  expect_equal(b$section[b$role == "heading"], "assessment")
})

test_that("a labelled field that maps is plan text inside a facts box; the fulltext stays", {
  # barrattdue's facts column (a list), ldh's facts card (paragraphs)
  li <- paste0("<main><div class='col'><h4>Kort om emnet</h4><ul><li>Studiepoeng: 5</li>",
               "<li>Krav til forkunnskaper: Bestått HIST11</li><li>Undervisningssted: Oslo</li></ul></div>",
               "<h2>Læringsutbytte</h2><p>Kan ting.</p></main>")
  p <- paste0("<main><div class='card'><h2>Om emnet</h2><div class='content'><p>Studiepoeng: 5</p>",
              "<p>Krav til forkunnskaper: BSY-100, BSY-101</p></div></div><h2>Læringsutbytte</h2><p>Kan ting.</p></main>")
  for (x in list(list(html = li, facts = "div.col", labels = "div.col > ul", pre = "Krav til forkunnskaper: Bestått HIST11"),
                 list(html = p, facts = "div.card", labels = "div.content", pre = "Krav til forkunnskaper: BSY-100, BSY-101"))) {
    cfg <- html_reader(c("h2, h4"), facts = x$facts, labels = x$labels,
                       sub_map = c("krav til forkunnskaper" = ".admission"))
    b <- page_blocks(x$html, NA, cfg)
    expect_equal(trimws(b$text[!b$facts & b$role == "text"]), c(x$pre, "Kan ting."))
    expect_equal(stats::setNames(sectionize(b)$raw_text, sectionize(b)$section),
                 c(.admission = x$pre, learning_outcomes = "Kan ting."))
    expect_equal(.blocks_text(b), .blocks_text(page_blocks(x$html, NA, utils::modifyList(cfg, list(labels = NULL)))))
  }
})

test_that("an unlabelled field that names a passed course maps only where 'bestått' is mapped", {
  html <- paste0("<main><div class='col'><h4>Kort om emnet</h4><ul><li>Studiepoeng: 5</li>",
                 "<li>Bestått KAM3300 Ensemblesang 2</li><li>Bestått opptaksprøve</li></ul></div>",
                 "<h2>Læringsutbytte</h2><p>Kan ting.</p></main>")
  cfg <- html_reader(c("h2, h4"), facts = "div.col", labels = "div.col > ul", sub_map = c("bestått" = ".admission"))
  b <- page_blocks(html, NA, cfg)
  expect_equal(stats::setNames(sectionize(b)$raw_text, sectionize(b)$section),
               c(.admission = "Bestått KAM3300 Ensemblesang 2", learning_outcomes = "Kan ting."))
  b <- page_blocks(html, NA, utils::modifyList(cfg, list(sub_map = c("krav til forkunnskaper" = ".admission"))))
  expect_equal(sectionize(b)$section, "learning_outcomes")
})

test_that("the text reader makes a mapped field of the metadata block a section of its own (ldh)", {
  own <- c("krav til forkunnskaper" = ".admission")
  hdr <- "^\\s*(?:Studiepoeng|Krav til forkunnskaper)\\b"
  cfg <- list(reader = "text", text_header = hdr, sub_map = own)
  pdf <- "Sykepleie\nStudiepoeng: 15\nKrav til forkunnskaper: BSY-100, BSY-101\n\nHensikt\nEmnet gir."
  b <- .text_blocks(pdf, cfg)
  expect_equal(b$section[b$role == "heading"][1], ".admission")
  expect_equal(b$text[b$role == "text"][1], "BSY-100, BSY-101")
  # the table layout, with a wrapped value; an empty field gives none
  tab <- paste0(" Studiepoeng                        15\n Krav til forkunnskaper             BSY-100, BSY-201\n",
                "                                    BSYP-200\n\nHensikt\nEmnet gir.")
  expect_equal(.header_fields(stringr::str_split_1(tab, "\n")[1:3], own),
               list(c(section = ".admission", text = "BSY-100, BSY-201 BSYP-200")))
  expect_length(.header_fields(c(" Studiepoeng   15", " Krav til forkunnskaper"), own), 0)
  # without the institution's own map, no field opens a section
  expect_length(.header_fields(stringr::str_split_1(pdf, "\n"), NULL), 0)
})
