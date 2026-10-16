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
})

test_that("fields are headings in scopes of their own; later labels are kept", {
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
  expect_equal(s$raw_text, c("Tekst", "Kan\n\nFerdigheiter\nGjer"))
  expect_match(.blocks_text(b), "^X1 Tittel\nInnhald\nTekst")
})

test_that("the json reader reads nla's titles and contents", {
  f <- harvest_files("nla", test_path("../../data/raw"))
  skip_if_not(length(f) > 0, "harvested nla data not available")
  x <- read_raw(f)
  # the latest autumn: earlier academic years are in the JSON for few courses
  i <- which(!is.na(x$html) & x$Årstall == max(x$Årstall) & x$Semesternavn == "Høst")[1]
  b <- page_blocks(x$html[i], NA, list(reader = "json"), x$course_id[i])
  expect_gt(sum(b$role == "heading" & !is.na(b$section)), 3)
})
