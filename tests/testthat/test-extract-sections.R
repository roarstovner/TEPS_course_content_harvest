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

test_that("paragraphs in an accordion <li> that holds a section heading split (#240)", {
  html <- "<main><ul>
    <li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div>
      <p>To innleveringer.</p><ul><li><p>Vurdering</p></li></ul></div></li>
    <li><h2>Vurdering og eksamen</h2><div>
      <p>Individuell munnleg eksamen.</p>
      <p><strong>Ny/utsatt eksamen</strong></p>
      <p>Ny eksamen blir arrangert som ved ordinær eksamen.</p></div></li>
    <li><h2>Hjelpemidler ved eksamen</h2><div><p>Ingen.</p></div></li></ul></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["assessment"]], "Individuell munnleg eksamen.")
  # A list item inside the section is still not a sub-heading.
  expect_match(s[["coursework_requirements"]], "To innleveringer\\.\\s+Vurdering$")
})

test_that("a bold label after a dropped sub-heading returns to the parent (#240)", {
  html <- "<main><ul><li><h2>Vurdering og eksamen</h2><div>
    <p><strong>Vurdering for studentar som tar faget 1. og 2. studieår</strong></p>
    <p>Individuell prøveforelesing.</p>
    <p><strong>Ny/utsett eksamen</strong></p><p>Som ved ordinær eksamen.</p>
    <p><strong>Vurdering for studentar som tar faget 3. studieår</strong></p>
    <p>Individuell FoU-oppgåve.</p></div></li></ul></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_false(grepl("ordinær", s[["assessment"]]))
  expect_match(s[["assessment"]], "^Vurdering for studentar som tar faget 1\\. og 2\\. studieår\n")
  expect_match(s[["assessment"]], "faget 3\\. studieår\nIndividuell FoU-oppgåve\\.$")
})

test_that("an emphasised lead that equals a heading splits its paragraph (#240)", {
  html <- "<main>
    <h2>Innhold</h2><p>Motorisk utvikling.</p>
    <p><em>Faget i praksis</em>I løpet av emnet planlegger studentene en økt.</p>
    <p><strong>Merk</strong> at dette ikke er en overskrift.</p>
    <h2>Vurdering</h2><p>Skriftlig eksamen.</p>
    <p><strong>Arbeidskrav:</strong> To innleveringer.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["course_content"]], "Motorisk utvikling.")
  expect_match(s[["teaching_methods"]], "^I løpet av emnet planlegger studentene en økt\\.")
  expect_match(s[["teaching_methods"]], "Merk at dette ikke er en overskrift\\.$")
  expect_equal(s[["assessment"]], "Skriftlig eksamen.")
  expect_equal(s[["coursework_requirements"]], "To innleveringer.")
})

test_that("a heading on its own line inside a <p> splits it (#240)", {
  html <- "<main><h2>Innhold</h2>
    <p>Bærekraftig utvikling. <br>Faget i praksis </p>
    <p>I praksisperioden vektlegges utforskende arbeidsmåter.</p>
    <h2>Innhold</h2><p>Geografi.</p>
    <p>Faget i praksis<br>Emnet har et fagdidaktisk perspektiv.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["course_content"]], "Bærekraftig utvikling.\n\nGeografi.")
  expect_match(s[["teaching_methods"]], "^I praksisperioden vektlegges")
  expect_match(s[["teaching_methods"]], "Emnet har et fagdidaktisk perspektiv\\.$")
})

test_that("the stock resit sentence is removed from assessment (#241)", {
  txt <- "Muntlig eksamen. Deleksamen 1: Ny/utsatt eksamen arrangeres som ved ordinær eksamen. Karakter A-F."
  out <- tibble::tibble(section = "assessment", raw_text = txt)
  expect_equal(.clean_sections(out, "oslomet")$raw_text, "Muntlig eksamen. Karakter A-F.")
})

test_that("an inline label for the open section stays as content (#240)", {
  html <- "<main><h2>Læringsutbytte</h2>
    <p><strong>Kunnskap</strong>Kandidaten har kunnskap om lesing.</p>
    <p><strong>Ferdigheter:</strong> Kandidaten kan planlegge.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_match(s[["learning_outcomes"]], "^Kunnskap\nKandidaten har kunnskap om lesing\\.")
  expect_match(s[["learning_outcomes"]], "\nFerdigheter:\nKandidaten kan planlegge\\.$")
})

test_that("sub-headings under an unmapped heading do not collect text (#240)", {
  html <- "<main><ul>
    <li><h2>Fagplan</h2><div><p>Læringsutbytte</p><p>Programmets mål.</p>
      <p>Arbeidskrav</p><p>Programmets krav.</p></div></li>
    <li><h2>Læringsutbytte</h2><div><p>Emnets mål.</p>
      <p>Kunnskap</p><p>Studenten kan lese.</p></div></li></ul></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(names(s), "learning_outcomes")
  # The group label naming the open section stays as content.
  expect_equal(s[["learning_outcomes"]], "Emnets mål.\nKunnskap\nStudenten kan lese.")
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
  expect_match(s[["assessment"]], "4 timer\\.\n\nKarakterskala\nA-F\\.$")
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

test_that("uis PDF: header skipped, intro kept, admin blocks and book titles handled (#243)", {
  txt <- paste(
    "MGL2050_1 Bokmål Versjon 20.Februar.2023", "",
    "Bevegelse og vurderinger i kroppsøvingsfaget",
    "Emnekode: MGL2050_1", "Vekting (SP): 30",
    "Tilbys av: Fakultet for utdanningsvitenskap og humaniora, Institutt for",
    "spesialpedagogikk", "",
    "Emnet gir innsikt i samfunnsfaget.", "",
    "Fagpersoner", "- Ola Nordmann (Faglærer)", "",
    "Arbeidsformer", "Seminarer.", "",
    "Emneevaluering", "Tidligdialog og sluttevaluering.", "",
    "Litteratur", "Nettside", "", "Kompetansemål og vurdering", "",
    "Utdanningsdirektoratet.", sep = "\n")
  cfg <- list(text_header = get_institution_config("uis")$section_text_header)
  out <- .clean_sections(extract_sections_text(list(extracted_text = txt), cfg))
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Emnet gir innsikt i samfunnsfaget.")
  expect_equal(s[["teaching_methods"]], "Seminarer.")
  expect_match(s[["reading_list"]], "Kompetansemål og vurdering\nUtdanningsdirektoratet\\.$")
  expect_false("assessment" %in% names(s))
  expect_false(any(grepl("Nordmann|Tidligdialog|Versjon", s)))
})

test_that("usn: table of contents skipped, stamp dropped, header 'litteratur' harmless (#244)", {
  txt <- paste(
    "Ansvarlig:", "", "Institutt for språk og litteratur", "",
    "Innholdsfortegnelse", "", "Faglig innhold i emnet", "", "Vurderingsformer", "",
    "Godkjent emneplan", "", "Litteratur", "",
    "Faglig innhold i emnet", "", "Språklæring.", "",
    "Vurderingsformer", "", "Muntlig eksamen.", "",
    "Godkjent emneplan", "", "Godkjent av dekan 31.01.2018", "",
    "Litteratur", "", "Kompetansemål og vurdering", sep = "\n")
  out <- .clean_sections(extract_sections_text(list(extracted_text = txt), list()))
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Språklæring.")
  expect_equal(s[["assessment"]], "Muntlig eksamen.")
  expect_equal(s[["reading_list"]], "Kompetansemål og vurdering")
})

test_that("ntnu: exam block gives assessment design, sessions and scripts dropped (#245)", {
  html <- "<main><h2>Om emnet</h2><h3>Læringsformer og aktiviteter</h3><p>Seminarer.</p>
    <script>function toggleRooms(id) { x(); }</script>
    <h3>Kontaktinformasjon</h3><h4>Faglærere</h4><p>Ola Nordmann</p>
    <h2>Eksamen</h2><p>Vurderingsordning: Skriftlig eksamen<br>Karakter: Bokstavkarakterer</p>
    <h4>Ordinær eksamen - Høst 2025</h4><p>Dato 29.11.2025, rom SL120</p></main>"
  cfg <- list(selector = "main", heading_selector = "h2, h3", subheading_selector = "h4")
  s <- sections_of(html, cfg)
  expect_equal(s[["teaching_methods"]], "Seminarer.")
  expect_equal(s[["assessment"]], "Vurderingsordning: Skriftlig eksamen\nKarakter: Bokstavkarakterer")
  expect_false(any(grepl("toggleRooms|Nordmann|SL120", s)))
  # form widgets (uib semester picker; #219) are not content either
  doc <- .read_doc("<div><label>Vel emnebeskrivelse for semester</label><select><option>2027 Vår</option></select><p>Mål.</p></div>")
  expect_equal(rvest::html_text2(doc), "Mål.")
})

test_that("more placeholders, exam-table headers and list stamps are removed (#248)", {
  for (t in c("Oppgis senere.", "Pensum-/litteraturliste er ikke publisert ennå.",
              "Litteratur vil være klart ved semesterstart.", "Pensumliste for høsten 2024.",
              "Pensumlista for høsten 2025-våren 2026.",
              "Litteraturlisten for høst 2026\n\nTilgang til litteratur\nNoe av litteraturen er digital.")) {
    expect_true(.is_placeholder_text(t), label = t)
  }
  out <- tibble::tibble(
    section = c("assessment", "assessment", "reading_list"),
    raw_text = c("Vurderingsform Gruppering Varighet Karakterskala Andel Kommentar Mappe Individuell A-F 100",
                 "Skriftlig eksamen.\nVurderingsform Vekting Varighet Karakter Hjelpemiddel\nSkoleeksamen 100/100 4 timer A - F",
                 "Litteraturlisten er sist oppdatert 16. august 2018.\nBotten, G. (2016). Matematikk med mening."))
  res <- .clean_sections(out, "uis")$raw_text
  expect_equal(res[1], "Mappe Individuell A-F 100")
  expect_equal(res[2], "Skriftlig eksamen.\n\nSkoleeksamen 100/100 4 timer A - F")
  expect_equal(res[3], "Botten, G. (2016). Matematikk med mening.")
  # A sentence that starts with header words is kept.
  expect_equal(.clean_sections(tibble::tibble(section = "assessment",
    raw_text = "Varighet og omfang avtales med veileder."), "inn")$raw_text,
    "Varighet og omfang avtales med veileder.")
})

test_that("first group label and one-line sections are kept, heading echo dropped (#249)", {
  out <- tibble::tibble(section = c("learning_outcomes", "learning_outcomes", "teaching_methods"),
                        raw_text = c("Kunnskap\nStudenten kan lese.", "Læringsutbytte\nStudenten kan lese.", "Praksis"))
  expect_equal(.clean_sections(out)$raw_text,
               c("Kunnskap\nStudenten kan lese.", "Studenten kan lese.", "Praksis"))
})

test_that("a colon-ended coursework lead-in starts coursework_requirements (#246)", {
  html <- "<main><h2>Undervisning</h2><p>Seminarer hver uke.</p>
    <p>Emnet inkluderer følgende obligatoriske aktiviteter, som må være godkjent før eksamen:</p>
    <ul><li>To innleveringer</li></ul>
    <h2>Eksamen</h2><p>Skriftlig eksamen.</p>
    <p>Merk at følgende gjelder:</p><p>Ingen hjelpemidler.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["teaching_methods"]], "Seminarer hver uke.")
  expect_match(s[["coursework_requirements"]], "^Emnet inkluderer følgende obligatoriske aktiviteter.*\nTo innleveringer$")
  expect_match(s[["assessment"]], "Ingen hjelpemidler\\.$")
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
  cfg <- .section_cfg(get_institution_config("mf"))
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

test_that("practicum headings are teaching methods (#247)", {
  expect_equal(match_heading_to_section("Praksis"), "teaching_methods")
  expect_equal(match_heading_to_section("Gjennomføring av praksis"), "teaching_methods")
  # exact rows only: other headings that mention praksis are unchanged
  expect_true(is.na(match_heading_to_section("Praksisrapport")))
})

test_that("resit heading variants are dropped (#241)", {
  for (h in c("Vilkår for ny/utsatt eksamen", "Ny/utsett eksamen",
              "Ny eller utsatt eksamen", "Kontinuasjonseksamen")) {
    expect_equal(match_heading_to_section(h), ".drop", label = h)
  }
})

test_that("uio: exam language and grading scale stay in assessment, labelled", {
  html <- "<main><h2>Eksamen</h2><p>Skriftlig eksamen, 4 timer.</p>
    <h3>Eksamensspråk</h3><p>Nynorsk.</p>
    <h3>Karakterskala</h3><p>Bestått/ikke bestått.</p></main>"
  s <- sections_of(html, html_cfg("h3, h4, p"))
  expect_equal(s[["assessment"]], paste0("Skriftlig eksamen, 4 timer.\n\nEksamensspråk\nNynorsk.",
                                         "\n\nKarakterskala\nBestått/ikke bestått."))
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

test_that("a COVID sentence is removed without the rest of its line (#241)", {
  txt <- paste("3 timers skoleeksamen erstattes av 3 timers hjemmeeksamen.",
               "Dette er et ekstraordinært tiltak i forbindelse med koronapandemien.",
               "Karakter A-F.")
  out <- tibble::tibble(section = "assessment", raw_text = txt)
  expect_equal(.clean_sections(out, "nord")$raw_text,
               "3 timers skoleeksamen erstattes av 3 timers hjemmeeksamen. Karakter A-F.")
  txt <- paste("Skriftlig eksamen.",
               "Pga. koronasituasjonen vil kravet om oppmøte ikkje bli handheva",
               "Grunna korona vert det følgande endringar:",
               "Koronatiltak gjeld ikkje lenger.", sep = "\n")
  out <- tibble::tibble(section = "assessment", raw_text = txt)
  expect_equal(.clean_sections(out, "uib")$raw_text, "Skriftlig eksamen.")
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

test_that("nla: sections are read from the EmneplanPage JSON in a <script>", {
  f <- test_path("../../data/raw/html_nla.RDS")
  skip_if_not(file.exists(f), "harvested nla data not available")
  d <- readRDS(f)
  d <- d[!is.na(d$html), ][1:20, ]
  out <- extract_sections(get_institution_config("nla"), d$html, d$extracted_text, d$course_id)
  expect_gt(nrow(out), 0)
})

test_that("extract_sections returns its columns when there are no pages", {
  out <- extract_sections(get_institution_config("samas"), character(), character(), character())
  expect_named(out, c("course_id", "institution", "section", "raw_text"))
  expect_equal(nrow(out), 0)
})
