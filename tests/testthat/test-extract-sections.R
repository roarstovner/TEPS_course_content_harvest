# Tests for section extraction — R/section_heading_map.R, R/extract_sections.R

# --- Heading map (#209) ---

test_that("headings found unmapped in the 2026-10-01 audit are mapped", {
  expect_equal(match_heading_to_section("Obligatorisk aktivitet"), "coursework_requirements")
  expect_equal(match_heading_to_section("Knowledge"), "learning_outcomes")
  expect_equal(match_heading_to_section("Skills"), "learning_outcomes")
  expect_equal(match_heading_to_section("General competence"), "learning_outcomes")
  expect_equal(match_heading_to_section("Form of assessment"), "assessment")
  expect_equal(match_heading_to_section("Arbeids- og undervisingsformer"), "teaching_methods")
  expect_equal(match_heading_to_section("Arbeidsform og organisering"), "teaching_methods")
  expect_equal(match_heading_to_section("Faget i praksis"), "teaching_methods")
  expect_equal(match_heading_to_section("Innleiing"), "course_content")
})

test_that("short English sub-headings only match exactly", {
  expect_equal(match_heading_to_section("Required prerequisite knowledge"), "prerequisites")
  expect_true(is.na(match_heading_to_section("Digital skills course")))
})

test_that("language metadata headings are not filed as assessment", {
  expect_true(is.na(match_heading_to_section("Language of instruction and examination")))
  expect_true(is.na(match_heading_to_section("Eksamensspråk")))
})

# --- Placeholder rows (#210) ---

test_that("placeholder and pointer-only rows are dropped", {
  drop <- c("Ingen", "Ingen.", "Ingen Ingen", "Ingen krav", "Ingen spesielle krav.",
            "None", "-", "...", ".", "- -", "x x",
            "Se fagplanen.", "Se fagplanen. Se fagplanen.", "Ingen. Se fagplanen.",
            "Sjå fagplanen.", "Se programplan", "Se emnearkivet",
            "Ingen emner i programmet",
            "Ingen pensumliste tilgjengelig for dette emnet",
            "No reading list available for this course",
            "Gjeldende litteraturliste for 2024 Høst finner du i Leganto",
            "Gjeldende litteraturliste for HØST 2022 finner du i Leganto.",
            "Litteratur og faglige ressurser finner du her.")
  for (t in drop) expect_false(.keep_section_row(t, "reading_list"), label = t)
})

test_that("rows with real content are kept", {
  keep <- c("Ingen utover opptakskravet.",
            "Ingen forkunnskaper, men emnet bygger på MGL101.",
            "Se fagplanen for detaljer. Studentene skriver en refleksjonslogg.",
            "Pensum er på opptil 500 sider.",
            "Forelesninger og seminarer.",
            "Litteraturliste finner du i Leganto. I tillegg: Imsen, G. (2020). Elevens verden.")
  for (t in keep) expect_true(.keep_section_row(t, "reading_list"), label = t)
})

# --- Sub-headings and .drop (#209, #211, #212) ---

html_cfg <- function(sub = NULL) {
  list(selector = "main", heading_level = "h2", subheading_selector = sub)
}
sections_of <- function(html, cfg) {
  out <- .clean_sections(extract_sections_html(list(html = html), cfg))
  stats::setNames(out$raw_text, out$section)
}

test_that("an emphasised paragraph that equals a heading starts a section", {
  html <- "<main>
    <h2>Innhold</h2><p>Motorisk utvikling.</p>
    <p><em>Faget i praksis</em></p><p>Studentene planlegger en økt.</p>
    <h2>Undervisningsformer</h2><p>Forelesninger.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["course_content"]], "Motorisk utvikling.")
  expect_match(s[["teaching_methods"]], "^Studentene planlegger en økt\\.")
  expect_match(s[["teaching_methods"]], "Forelesninger\\.$")
  # Without a sub-heading selector the old behaviour is unchanged.
  s0 <- sections_of(html, html_cfg())
  expect_match(s0[["course_content"]], "Faget i praksis")
})

test_that("sentences and list items never act as sub-headings", {
  html <- "<main><h2>Innhold</h2>
    <p>Vurdering skjer ved eksamen.</p><ul><li><p>Vurdering</p></li></ul>
    <p>Klasseledelse.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(names(s), "course_content")
  expect_match(s[["course_content"]], "Klasseledelse")
})

test_that("admission text is dropped and h3 prerequisites are kept (uio)", {
  html <- "<main>
    <h2>Opptak til emnet</h2>
    <p>Studenter må hvert semester søke og melde seg til eksamen i Studentweb.</p>
    <h3>Obligatoriske forkunnskaper</h3><p>MAT1100.</p>
    <h2>Undervisning</h2><p>Seminarer.</p>
    <p>Obligatoriske aktiviteter:</p><p>To innleveringer.</p>
    <h2>Eksamen</h2><p>Skriftlig eksamen, 4 timer.</p>
    <h3>Karakterskala</h3><p>A-F.</p></main>"
  s <- sections_of(html, html_cfg("h3, h4, p"))
  expect_equal(s[["prerequisites"]], "MAT1100.")
  expect_equal(s[["teaching_methods"]], "Seminarer.")
  expect_equal(s[["coursework_requirements"]], "To innleveringer.")
  # An unmapped h3 hands its text back to the enclosing section.
  expect_match(s[["assessment"]], "4 timer\\.\n\nA-F\\.$")
  expect_false(any(grepl("Studentweb", s)))
})

test_that("text_split ends a section at an admission heading and drops it", {
  txt <- "Forkunnskapskrav\nMAT1\n\nOpptakskrav\nGenerell studiekompetanse.\n\nVurdering\nSkriftlig eksamen."
  out <- .clean_sections(extract_sections_text(list(extracted_text = txt), list()))
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["prerequisites"]], "MAT1")
  expect_equal(s[["assessment"]], "Skriftlig eksamen.")
  expect_false(any(grepl("studiekompetanse", s)))
})

# --- details_mf (#213) ---

test_that("mf: intro is course_content, accordions are sections, contact card ignored", {
  html <- '<html><body><article class="template-study-subject"><div class="content-body">
    <div class="template-study-subject__details">Emneinfo Emnekode: X</div>
    <div class="wp-block-group">
      <p>Dette emnet gir en innføring i identitet.</p>
      <p>Arbeidsform og organisering:</p><p>Forelesninger og seminarer.</p>
      <p>Delemne C: Kirkekunnskap (2,5 studiepoeng)</p>
    </div>
    <div class="template-study-subject__accordion"><h2>Om studiet</h2>
      <details><summary>Obligatoriske aktiviteter</summary><p>Godkjent fremmøte.</p></details>
      <details><summary>Avsluttende vurdering/eksamen</summary><p>Skoleeksamen, 4 timer.</p></details>
      <details><summary>Eksamensdatoer</summary><p>12. desember.</p></details>
      <details><summary>Læringsutbytte</summary><p>Studenten har</p><ul><li>kunnskap om makt</li></ul></details>
    </div>
    <div class="template-study-subject__contact"><h2>Emneansvarlig</h2>Ola Nordmann</div>
  </div></article></body></html>'
  cfg <- .section_cfg("mf")
  out <- .clean_sections(extract_sections_mf(list(html = html), cfg))
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Dette emnet gir en innføring i identitet.")
  expect_match(s[["teaching_methods"]], "^Forelesninger og seminarer\\.\nDelemne C")
  expect_equal(s[["coursework_requirements"]], "Godkjent fremmøte.")
  expect_equal(s[["assessment"]], "Skoleeksamen, 4 timer.")
  expect_match(s[["learning_outcomes"]], "^Studenten har\\s+kunnskap om makt$")
  expect_false(any(grepl("Nordmann|desember|Emnekode", s)))
})

# --- Exam logistics and notices (#215) ---

test_that("exam-logistics headings end the assessment section", {
  expect_equal(match_heading_to_section("Mer om eksamen ved UiO"), ".drop")
  expect_equal(match_heading_to_section("Hjelpemidler"), ".drop")
  expect_equal(match_heading_to_section("Sensorordning"), ".drop")
  # exact rows only: a combined heading is still assessment
  expect_equal(match_heading_to_section("Eksamen og hjelpemidler"), "assessment")
})

test_that("notices are stripped from assessment but not from course content", {
  txt <- paste("Skriftlig eksamen, 4 timer.",
               "Å generere besvarelse ved hjelp av ChatGPT er å regne som fusk.",
               "På bakgrunn av Covid-19 epidemien blir eksamen endret.",
               "Oppgaver blir sjekket for plagiat.", sep = "\n")
  out <- tibble::tibble(section = c("assessment", "course_content"),
                        raw_text = c(txt, "Bruk av ChatGPT i skolen."))
  res <- .clean_sections(out, "nord")
  expect_equal(res$raw_text[1], "Skriftlig eksamen, 4 timer.")
  expect_equal(res$raw_text[2], "Bruk av ChatGPT i skolen.")
})

test_that("uib footer and error banner are removed", {
  txt <- paste("Mappevurdering.",
               "Vi opplever problemer med å hente inn eksamensinformasjon for dette emnet.",
               "Lukk", "Fagdidaktikk i spansk 3",
               "Dette bør du vite om eksamen", "Institutt for fremmedspråk", "Til toppen",
               sep = "\n")
  res <- .clean_sections(tibble::tibble(section = "assessment", raw_text = txt), "uib")
  expect_equal(res$raw_text, "Mappevurdering.")
})

# --- Inline coursework lines (#212) ---

test_that("nord-style gate lines move from assessment to coursework_requirements", {
  out <- tibble::tibble(section = "assessment", raw_text = paste(
    "Obligatorisk deltakelse (OD): Minst 80 % av undervisningen.",
    "Arbeidskrav (AK): Tre skriftlige arbeider. Må være godkjent.",
    "Eksamen",
    "Skriftlig individuell skoleeksamen, 4 timer.", sep = "\n"))
  res <- .split_inline_coursework(out)
  s <- stats::setNames(res$raw_text, res$section)
  expect_equal(s[["assessment"]], "Eksamen\nSkriftlig individuell skoleeksamen, 4 timer.")
  expect_match(s[["coursework_requirements"]], "^Obligatorisk deltakelse \\(OD\\).*\nArbeidskrav \\(AK\\)")
})

test_that("sentences that merely mention arbeidskrav stay in assessment", {
  out <- tibble::tibble(section = "assessment",
                        raw_text = "Mappen består av tre arbeidskrav og vurderes samlet.")
  expect_equal(.split_inline_coursework(out), out)
})

test_that("nmbu exam-table line is removed but a sentence before it is kept", {
  txt <- "Muntlig eksamen.\nMuntlig eksamen Karakterregel: Bokstavkarakterer Hjelpemiddelkode: A1"
  res <- .clean_sections(tibble::tibble(section = "assessment", raw_text = txt), "nmbu")
  expect_equal(res$raw_text, "Muntlig eksamen.")
  one <- "Skriftlig avsluttende prøve; 3,5 timer. Karakterregel: A-F."
  res <- .clean_sections(tibble::tibble(section = "assessment", raw_text = one), "nmbu")
  expect_equal(res$raw_text, one)
})

test_that("admission sentences are removed from prerequisites, real ones kept (#211)", {
  txt <- paste("Opptak skjer på bakgrunn av generell studiekompetanse eller realkompetanse.",
               "Kan tas som frittstående fag i lærerutdanning.",
               "Bestått MGL101 og opptak til lærerutdanningen.",
               "Emnet bygger på Norsk 1.", sep = "\n")
  res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt), "nord")
  expect_equal(res$raw_text, "Bestått MGL101 og opptak til lærerutdanningen.\nEmnet bygger på Norsk 1.")
})

test_that("recommendations that mention admission are kept (#211)", {
  txt <- paste("I tillegg til generell studiekompetanse bør studenten ha engelsk.",
               "Generell studiekompetanse inklusive lulesamisk fra videregående skole.",
               "Kan tas som frittstående fag i lærerutdanning. Bygger på 1A 1-7",
               "Generell studiekompetanse", sep = "\n")
  res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt), "nord")
  expect_equal(res$raw_text, paste(strsplit(txt, "\n")[[1]][1:3], collapse = "\n"))
})
