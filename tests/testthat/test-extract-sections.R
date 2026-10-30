# Tests for section extraction — R/section_heading_map.R, R/extract_sections.R

# --- Heading map ---

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

test_that("headings of the English-only HVL pages are mapped", {
  expect_equal(match_heading_to_section("Compulsory learning activities"), "coursework_requirements")
  expect_equal(match_heading_to_section("Entry requirements"), "prerequisites")
  expect_equal(match_heading_to_section("Recommended previous knowledge"), "prerequisites")
})

test_that("headings of Nord's English pages are mapped", {
  expect_equal(match_heading_to_section("Course description"), "course_content")
  expect_equal(match_heading_to_section("Learning activities"), "teaching_methods")
  expect_equal(match_heading_to_section("Exam description"), "assessment")
  expect_true(is.na(match_heading_to_section("Course evaluation")))
})

test_that("headings of NTNU's English pages are mapped", {
  expect_equal(match_heading_to_section("Learning methods and activities"), "teaching_methods")
  expect_equal(match_heading_to_section("Further on evaluation"), "assessment")
  expect_equal(match_heading_to_section("Required previous knowledge"), "prerequisites")
  expect_equal(match_heading_to_section("Course materials"), "reading_list")
  expect_equal(match_heading_to_section("Compulsory assignments"), "coursework_requirements")
  expect_equal(match_heading_to_section("Department with academic responsibility"), ".drop")
  expect_true(is.na(match_heading_to_section("Contact information")))
})

test_that("headings of NLA's English pages are mapped like their Norwegian ones", {
  expect_equal(match_heading_to_section("Introduction"), "course_content")
  expect_equal(match_heading_to_section("Teaching and learning methods"), "teaching_methods")
  expect_equal(match_heading_to_section("Practice"), "teaching_methods")
  expect_equal(match_heading_to_section("Final assessment"), "assessment")
  expect_equal(match_heading_to_section("Assessment language"), "assessment")
  expect_equal(match_heading_to_section("Permitted aids under examination"), "assessment")
  expect_equal(match_heading_to_section("Syllabus"), "reading_list")
  expect_equal(match_heading_to_section("Progression requirements"), "prerequisites")
  expect_true(.is_label_heading("Assessment language"))
  expect_true(is.na(match_heading_to_section("Scope")))
})

test_that("headings of USN's English pages are mapped like their Norwegian ones", {
  expect_equal(match_heading_to_section("Course is part of these studies"), ".drop")
  expect_equal(match_heading_to_section("Approved course plan"), ".drop")
  expect_equal(match_heading_to_section("Change description"), ".drop")
  expect_equal(match_heading_to_section("Participation/Compulsory work"), "coursework_requirements")
  expect_equal(match_heading_to_section("Compulsory Activity"), "coursework_requirements")
  expect_equal(match_heading_to_section("Compulsory activity and compulsory attendance"),
               "coursework_requirements")
  expect_equal(match_heading_to_section("Forms of assessment"), "assessment")
  expect_equal(match_heading_to_section("Supervised professional training"), "teaching_methods")
  # like "Annet"
  expect_true(is.na(match_heading_to_section("Miscellaneous")))
})

test_that("short English sub-headings only match exactly", {
  expect_equal(match_heading_to_section("Required prerequisite knowledge"), "prerequisites")
  expect_true(is.na(match_heading_to_section("Digital skills course")))
})

test_that("a table of contents is not course content", {
  expect_equal(match_heading_to_section("Innholdsfortegnelse"), ".drop")
  expect_equal(match_heading_to_section("Innhold"), "course_content")
})

test_that("language metadata headings are not filed as assessment", {
  # uit's language label is facts, not plan text, in Norwegian and English
  expect_equal(match_heading_to_section("Undervisnings- og eksamensspråk"), ".drop")
  expect_equal(match_heading_to_section("Language of instruction and examination"), ".drop")
  expect_true(is.na(match_heading_to_section("Language of instruction")))
  expect_equal(match_heading_to_section("Eksamensspråk"), "assessment")
})

# --- Placeholder rows ---

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
            "Litteratur og faglige ressurser finner du her.",
            "Emnebeskrivelsen finnes kun på engelsk, gå til våre engelske nettsider ved å klikke \"English\" øverst i menyfeltet.",
            "Emnebeskrivelsen finnes kun på engelsk. Gå til våre engelske nettsider ved å klikke på \"English\" øverst i menyfeltet.",
            "Dette emnet er kun beskrevet på engelsk. Klikk på \"English\" for å få fram emneplanen",
            "Dette emner er kun beskrevet på engelsk. Klikk på \"English\" øverst til høgre for å få fram beskrivelsen.",
            "Dette emnet er berre skildra på engelsk. Klikk på knappen \"English\" oppe til høgre på sida, for å få fram den engelske visninga.",
            "Se engelske nettsider for mer informasjon",
            "Emnet foregår på engelsk. Se engelske nettsider ved å velge engelsk språk.",
            "See English version.", "See English Version", "se engelsk beskrivelse", "Se engelsk beskrivelse.",
            "Se engelsk tekst.")
  for (t in drop) expect_false(.keep_section_row(t, "reading_list"), label = t)
  # a pointer after plan text stays (ntnu MACS490)
  expect_true(.keep_section_row("Bestått MACS4900\n\nSe engelsk beskrivelse for mer utfyllende informasjon.",
                                "prerequisites"))
  # the teaching language is not a pointer (hvl MGUEN201)
  expect_true(.keep_section_row("Emnet foregår på engelsk.", "course_content"))
  expect_true(.keep_section_row(paste("All undervisning i emnet foregår på engelsk, og språkøvingsaspektet",
                                      "står sentralt i studentenes arbeid."), "course_content"))
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

# --- Sub-headings and .drop ---

html_cfg <- function(sub = NULL, heading = "h2") {
  list(reader = "html", container = "main", heading = heading, sub = sub)
}
sections_of <- function(html, cfg) {
  out <- page_sections(page_blocks(html, NA, cfg), NA, cfg)
  stats::setNames(out$raw_text, out$section)
}
text_sections <- function(txt, text_header = NULL) {
  cfg <- list(reader = "text", text_header = text_header)
  page_sections(page_blocks(NA, txt, cfg), txt, cfg)
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

test_that("paragraphs in an accordion <li> that holds a section heading split", {
  html <- "<main><ul>
    <li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div>
      <p>To innleveringer.</p><ul><li><p>Vurdering</p></li></ul></div></li>
    <li><h2>Vurdering og eksamen</h2><div>
      <p>Individuell munnleg eksamen.</p>
      <p><strong>Ny/utsatt eksamen</strong></p>
      <p>Ny eksamen blir arrangert som ved ordinær eksamen.</p></div></li>
    <li><h2>Hjelpemidler ved eksamen</h2><div><p>Ingen.</p></div></li></ul></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_equal(s[["assessment"]], "Individuell munnleg eksamen.\n\nHjelpemidler ved eksamen\nIngen.")
  # A list item inside the section is still not a sub-heading.
  expect_match(s[["coursework_requirements"]], "To innleveringer\\.\\s+Vurdering$")
})

test_that("a bold label after a dropped sub-heading returns to the parent", {
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

test_that("an emphasised lead that equals a heading splits its paragraph", {
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

test_that("a heading on its own line inside a <p> splits it", {
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

test_that("the stock resit sentence is removed from assessment", {
  txt <- "Muntlig eksamen. Deleksamen 1: Ny/utsatt eksamen arrangeres som ved ordinær eksamen. Karakter A-F."
  out <- tibble::tibble(section = "assessment", raw_text = txt)
  expect_equal(.clean_sections(out, "oslomet")$raw_text, "Muntlig eksamen. Karakter A-F.")
})

test_that("an inline label for the open section stays as content", {
  html <- "<main><h2>Læringsutbytte</h2>
    <p><strong>Kunnskap</strong>Kandidaten har kunnskap om lesing.</p>
    <p><strong>Ferdigheter:</strong> Kandidaten kan planlegge.</p></main>"
  s <- sections_of(html, html_cfg("p"))
  expect_match(s[["learning_outcomes"]], "^Kunnskap\nKandidaten har kunnskap om lesing\\.")
  expect_match(s[["learning_outcomes"]], "\nFerdigheter:\nKandidaten kan planlegge\\.$")
})

test_that("sub-headings under an unmapped heading do not collect text", {
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

test_that("a section that points to the Fagplan takes its text from it", {
  html <- "<main><ul>
    <li><h2>Fagplan</h2><div><p><strong>Innledning</strong></p><p>Faget handler om kart.</p>
      <p><strong>Fagets arbeids- og undervisningsformer</strong></p>
      <p>Forelesninger og seminarer.</p><p>Ekskursjoner.</p>
      <p><strong>Vurdering</strong></p><p>Vurderingsformene er varierte.</p></div></li>
    <li><h2>Innhold</h2><div><p>Kart og klima.</p></div></li>
    <li><h2>Arbeids- og undervisningsformer</h2><div><p>Se fagplanen.</p></div></li>
    <li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div><p>Se fagplanen.</p></div></li>
    </ul></main>"
  cfg <- c(html_cfg("p"), list(pointer = list(
    heading = "Fagplan", lead = "Fagplanen sier:",
    sections = c(teaching_methods = "Fagets arbeids- og undervisningsformer"))))
  s <- sections_of(html, cfg)
  expect_equal(s[["teaching_methods"]],
               "Se fagplanen. Fagplanen sier:\nForelesninger og seminarer.\nEkskursjoner.")
  # sections without a sub-heading to read stay placeholders and are dropped
  expect_false("coursework_requirements" %in% names(s))
  expect_equal(s[["course_content"]], "Kart og klima.")
  # real teaching methods are left alone
  own <- sub("Se fagplanen.", "Gruppearbeid.", html, fixed = TRUE)  # the first is teaching methods
  expect_equal(sections_of(own, cfg)[["teaching_methods"]], "Gruppearbeid.")
})

test_that("the Fagplan's coursework rules join a coursework section of its own", {
  html <- "<main><ul>
    <li><h2>Fagplan</h2><div><p><strong>Innledning</strong></p><p>Faget handler om kart.</p>
      <p><strong>Arbeidskrav</strong></p><p>Gyldig fravær gir ikke fritak for arbeidskrav.</p>
      <p>Obligatorisk deltakelse</p><p>Det stilles krav om 80 % tilstedeværelse.</p>
      <p>Skikkethetsvurdering</p><p>Studenten vurderes gjennom hele studiet.</p></div></li>
    <li><h2>Innhold</h2><div><p>Kart og klima.</p></div></li>
    <li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div><p>To innleveringer.</p></div></li>
    </ul></main>"
  cfg <- c(html_cfg("p"), list(pointer = list(
    heading = "Fagplan", lead = "Fagplanen sier:",
    append = list(coursework_requirements = "Arbeidskrav"), stop = "^Skikkethetsvurdering")))
  rules <- paste0("Fagplanen sier:\nGyldig fravær gir ikke fritak for arbeidskrav.\n",
                  "Obligatorisk deltakelse\nDet stilles krav om 80 % tilstedeværelse.")
  # up to the stop label, not the suitability text after it
  expect_equal(sections_of(html, cfg)[["coursework_requirements"]],
               paste0("To innleveringer.\n\n", rules))
  # a course without coursework of its own, or with only a pointer, gets none
  # (practicum courses)
  for (own in c("", "<li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div><p>Se fagplanen.</p></div></li>")) {
    other <- sub("<li><h2>Arbeidskrav og obligatoriske aktiviteter</h2><div><p>To innleveringer.</p></div></li>",
                 own, html, fixed = TRUE)
    expect_false("coursework_requirements" %in% names(sections_of(other, cfg)))
    expect_equal(sections_of(other, cfg)[["course_content"]], "Kart og klima.")
  }
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
  out <- text_sections(txt)
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["prerequisites"]], "MAT1")
  expect_equal(s[["assessment"]], "Skriftlig eksamen.")
  expect_false(any(grepl("studiekompetanse", s)))
})

test_that("uis PDF: header skipped, intro kept, admin blocks and book titles handled", {
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
  out <- text_sections(txt, get_institution_config("uis")$section_text_header)
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Emnet gir innsikt i samfunnsfaget.")
  expect_equal(s[["teaching_methods"]], "Seminarer.")
  expect_match(s[["reading_list"]], "Kompetansemål og vurdering\nUtdanningsdirektoratet\\.$")
  expect_false("assessment" %in% names(s))
  expect_false(any(grepl("Nordmann|Tidligdialog|Versjon", s)))
})

test_that("usn: table of contents skipped, stamp dropped, header 'litteratur' harmless", {
  txt <- paste(
    "Ansvarlig:", "", "Institutt for språk og litteratur", "",
    "Innholdsfortegnelse", "", "Faglig innhold i emnet", "", "Vurderingsformer", "",
    "Hjelpemidler til eksamen", "", "Annet", "", "Godkjent emneplan", "", "Litteratur", "",
    "Faglig innhold i emnet", "", "Språklæring.", "",
    "Vurderingsformer", "", "Muntlig eksamen.", "",
    "Hjelpemidler til eksamen", "", "Alle", "",
    "Annet", "", "Emnet evalueres jevnlig.", "",
    "Godkjent emneplan", "", "Godkjent av dekan 31.01.2018", "",
    "Litteratur", "", "Kompetansemål og vurdering", sep = "\n")
  out <- text_sections(txt)
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Språklæring.")
  # an unmapped entry of the table of contents ("Annet") ends the section
  expect_equal(s[["assessment"]], "Muntlig eksamen.\n\nHjelpemidler til eksamen\nAlle")
  expect_equal(s[["reading_list"]], "Kompetansemål og vurdering")
})

test_that("ntnu: one exam session in assessment; contacts, scripts, timetable, dates, rooms are out", {
  html <- "<section id='content'><div id='omEmnet'><h1 class='tabTitle'>Om</h1><h2>Om emnet</h2>
    <h3>Læringsformer og aktiviteter</h3><p>Seminarer.</p>
    <script>function toggleRooms(id) { x(); }</script>
    <h3>Mer om vurdering</h3><p>Hjemmeeksamen.</p>
    <h3>Kontaktinformasjon</h3><h4>Faglærere</h4><p>Ola Nordmann</p>
    <h3>Ansvarlig enhet</h3><p>Institutt for lærerutdanning</p></div>
    <div id='timeplan' class='tab'><h1 class='tabTitle'>Timeplan</h1><h2>Timeplan</h2><p>Vis detaljert timeplan</p></div>
    <div id='omEksamen' class='tab'><h1 class='tabTitle'>Eksamen</h1><h2 class='course-exam-heading'>Eksamen</h2>
    <div class='exam-assessment-form-general'>Vurderingsordning: Skriftlig eksamen</div>
    <div class='grade-rule-heading'>Karakter: Bokstavkarakterer</div>
    <div class='exam-element'><h4 class='course-exam-heading2'>Utsatt eksamen - Vår 2026</h4>
      <h5 class='exam-form'>Muntlig eksamen</h5><div class='exam-container'>
      <span class='exam-item exam-fact-label'>Vekting</span> <span class='exam-item'>100/100</span></div></div>
    <div class='exam-element'><h4 class='course-exam-heading2'>Ordinær eksamen - Høst 2025</h4>
      <h5 class='exam-form'>Skriftlig eksamen</h5><div class='exam-container'>
      <span class='exam-item exam-fact-label'>Vekting</span> <span class='exam-item'>100/100</span>
      <span class='exam-item exam-fact-label'> Dato </span> <span class='exam-item'>29.11.2025</span>
      <span class='exam-item exam-fact-label'>Tid</span> <span class='exam-item'>09:00</span>
      <span class='exam-item exam-fact-label'>Varighet</span> <span class='exam-item'>4 timer</span>
      <span class='exam-item exam-fact-label'>Eksamenssystem</span> <span class='exam-item'>Inspera Assessment</span>
      <div class='exam-rooms'>Sted og rom <div class='room-list'>Rom SL120</div></div></div>
      <ul class='portlet-msg-info'><li>Øvrige kommentarer</li><li>Individuelt tidspunkt kommer senere</li></ul></div>
    <div class='exam-link'><p><a>Alt om eksamen ved NTNU</a></p></div></div></section>"
  ic <- get_institution_config("ntnu")
  s <- sections_of(html, .block_cfg(ic))
  expect_equal(s[["teaching_methods"]], "Seminarer.")
  expect_equal(s[["assessment"]], paste0(
    "Hjemmeeksamen.\n\nVurderingsordning: Skriftlig eksamen\nKarakter: Bokstavkarakterer\n",
    "Skriftlig eksamen\nVekting 100/100 Varighet 4 timer"))
  expect_false(any(grepl("toggleRooms|Nordmann|SL120|29.11|09:00|Inspera|Alt om|Muntlig|Timeplan|kommentarer", s)))
  # course_plan keeps both sessions, the exam system and the unit; no
  # contacts, timetable, tab titles, dates, rooms or comments
  cp <- anonymize_text("ntnu", page_fulltext(page_blocks(html, NA, .block_cfg(ic)), ic), .progress = FALSE)
  expect_match(cp, "Ansvarlig enhet\n+Institutt for lærerutdanning\n+Eksamen\nVurderingsordning: Skriftlig eksamen")
  expect_match(cp, "Utsatt eksamen\nMuntlig eksamen\nVekting 100/100\nOrdinær eksamen\nSkriftlig eksamen\n")
  expect_match(cp, "Vekting 100/100 Varighet 4 timer Eksamenssystem Inspera Assessment$")
  expect_false(grepl("Nordmann|Timeplan|SL120|Dato|Tid|kommentarer|Alt om|^Om\n", cp))
  # form widgets (uib semester picker) are not content either
  doc <- .read_doc("<div><label>Vel emnebeskrivelse for semester</label><select><option>2027 Vår</option></select><p>Mål.</p></div>")
  expect_equal(rvest::html_text2(doc), "Mål.")
})

test_that("ntnu: the English page's exam tab is read like the Norwegian one", {
  html <- "<section id='content'><div id='omEmnet'><h3>Further on evaluation</h3><p>Portfolio.</p>
    <h3>Contact information</h3><h4>Course coordinator</h4><p>Kari Nordmann</p>
    <h4>Department with academic responsibility</h4><p>Department of Computer Science</p></div>
    <div id='omEksamen' class='tab'><h2 class='course-exam-heading'>Examination</h2>
    <div class='exam-assessment-form-general'>Examination arrangement: Written examination</div>
    <div class='grade-rule-heading'>Grade: Letter grades</div>
    <div class='exam-element'><h4 class='course-exam-heading2'>Ordinary examination - Autumn 2021</h4>
      <h5 class='exam-form'>Written examination</h5><div class='exam-container'>
      <span class='exam-item exam-fact-label'>Weighting</span> <span class='exam-item'>100/100</span>
      <span class='exam-item exam-fact-label'> Date </span> <span class='exam-item'>2021-12-01</span>
      <span class='exam-item exam-fact-label'>Time</span> <span class='exam-item'>09:00</span>
      <span class='exam-item exam-fact-label'>Exam system</span> <span class='exam-item'>Inspera Assessment</span></div></div>
    <div class='exam-element'><h4 class='course-exam-heading2'>Re-sit examination - Autumn 2021</h4>
      <h5 class='exam-form'>Oral examination</h5><div class='exam-container'>
      <span class='exam-item exam-fact-label'>Weighting</span> <span class='exam-item'>100/100</span></div></div>
    </div></section>"
  ic <- get_institution_config("ntnu")
  s <- sections_of(html, .block_cfg(ic))
  expect_equal(s[["assessment"]], paste0("Portfolio.\n\nExamination arrangement: Written examination\n",
                                         "Grade: Letter grades\nWritten examination\nWeighting 100/100"))
  cp <- anonymize_text("ntnu", page_fulltext(page_blocks(html, NA, .block_cfg(ic)), ic), .progress = FALSE)
  expect_false(grepl("Nordmann|2021-12-01|09:00|Date|Time", cp))
  expect_match(cp, "Re-sit examination\nOral examination")
})

test_that("ntnu: banners are out; admission, credit reductions and subject areas are facts", {
  html <- "<section id='content'><div id='course-details'><span class='course-code'>MA2012</span>
    <h1 class='course-name'>Geometri</h1><p class='portlet-msg-alert mt-3'>Nytt fra studieåret 2025/2026</p>
    <div class='course-facts'><span>Studiepoeng</span> <span>7,5</span></div></div>
    <div id='omEmnet'><h3>Faglig innhold</h3><p>Geometri.</p>
    <div id='requirements-toggler'><h3>Spesielle vilkår</h3><div><p>Krever opptak til studieprogram:<br>
      <a>Lektorutdanning i realfag (MLREAL)</a></p></div></div>
    <div id='credit-reductions-toggler'><h3>Studiepoengreduksjon</h3><table><tr><td>MA0012</td><td>7,5 sp</td></tr></table>
      <span>Dette emnet har faglig overlapp med emnet i tabellen over.</span></div>
    <div class='course-card'><div class='course-card-header'><h3>Fagområder</h3></div>
      <div class='course-card-body'><ul class='subject-area-list'><li>Matematikk</li></ul></div></div></div></section>"
  ic <- get_institution_config("ntnu")
  b <- page_blocks(html, NA, .block_cfg(ic))
  txt <- page_fulltext(b, ic)
  expect_match(txt, "Spesielle vilkår.*Lektorutdanning i realfag.*overlapp.*Fagområder\\s+Matematikk$")
  expect_false(grepl("Nytt fra", txt))
  expect_equal(page_fulltext(b, ic, facts = FALSE), "Faglig innhold\n\nGeometri.")
})

test_that("more placeholders and list stamps are removed, exam-table headers kept", {
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
  expect_equal(res[1:2], out$raw_text[1:2])
  expect_equal(res[3], "Botten, G. (2016). Matematikk med mening.")
})

test_that("first group label and one-line sections are kept, heading echo dropped", {
  out <- tibble::tibble(section = c("learning_outcomes", "learning_outcomes", "teaching_methods"),
                        raw_text = c("Kunnskap\nStudenten kan lese.", "Læringsutbytte\nStudenten kan lese.", "Seminarer"))
  expect_equal(.clean_sections(out)$raw_text,
               c("Kunnskap\nStudenten kan lese.", "Studenten kan lese.", "Seminarer"))
})

test_that("a colon-ended coursework lead-in starts coursework_requirements", {
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

# --- details_mf ---

test_that("mf: intro is course_content, accordions are sections, contact card ignored", {
  html <- '<html><body><article class="template-study-subject"><div class="content-body">
    <hgroup class="course_title"><p>RL5045</p><h1>Kirkehistorie</h1></hgroup>
    <div class="template-study-subject__details"><h3>Emneinfo</h3><p>Emnekode: X</p>
      <div class="wp-block-group"><p class="text-sm">Studieprogramtilhørighet:</p><a>Lektorprogram</a>
      <button>Vis flere</button></div><div class="wp-block-button"><a>Timeplan</a></div></div>
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
  cfg <- .block_cfg(get_institution_config("mf"))
  out <- page_sections(page_blocks(html, NA, cfg), NA, cfg)
  s <- stats::setNames(out$raw_text, out$section)
  expect_equal(s[["course_content"]], "Dette emnet gir en innføring i identitet.")
  expect_match(s[["teaching_methods"]], "^Forelesninger og seminarer\\.\nDelemne C")
  expect_equal(s[["coursework_requirements"]], "Godkjent fremmøte.")
  expect_equal(s[["assessment"]], "Skoleeksamen, 4 timer.")
  expect_match(s[["learning_outcomes"]], "^Studenten har\\s+kunnskap om makt$")
  expect_false(any(grepl("Nordmann|desember|Emnekode|RL5045|Studieprogram", s)))
  # the title and the facts box are in the text, their buttons are not
  # also in the 2026-04 layout (template-study-program__*)
  for (h in c(html, gsub("template-study-subject__", "template-study-program__", html))) {
    txt <- page_fulltext(page_blocks(h, NA, cfg), list())
    expect_match(txt, "^RL5045\\s+Kirkehistorie\\s+Emneinfo\\s+Emnekode: X\\s+Studieprogramtilhørighet:\\s+Lektorprogram")
    expect_false(grepl("Vis flere|Timeplan|Nordmann", txt))
    out <- page_sections(page_blocks(h, NA, cfg), NA, cfg)
    expect_equal(out$raw_text[out$section == "course_content"], "Dette emnet gir en innføring i identitet.")
  }
})

test_that("uis: the facts box is in the text without its links, and in no section", {
  html <- '<html><body><div id="block-page-content"><div class="article course">
    <div class="factbox factbox--course"><h2>Fakta</h2>
      <p class="factbox__content-item"><strong>Emnekode</strong> MGL3038</p>
      <p class="factbox__content-item"><strong>Timeplan</strong> <a>Vis timeplan</a></p></div>
    <div><h2>Eksamen / vurdering</h2></div>
    <div class="factbox factbox--course factbox--exam"><h3>Skriftlig eksamen</h3>
      <p class="factbox__content-item"><strong>Vekt</strong> <a>1/1</a></p></div>
    <div class="course-footer"><h2>Kontakt</h2><p>Ola Nordmann</p></div></div></div></body></html>'
  cfg <- .block_cfg(get_institution_config("uis"))
  txt <- page_fulltext(page_blocks(html, NA, cfg), list())
  expect_match(txt, "Fakta\\s+Emnekode MGL3038")
  expect_false(grepl("timeplan|Nordmann", txt, ignore.case = TRUE))
  out <- page_sections(page_blocks(html, NA, cfg), NA, cfg)
  expect_equal(out$section, "assessment")
  expect_match(out$raw_text, "Vekt 1/1")
})

test_that("nord: the short description is course content, title and code are not", {
  page <- function(intro) paste0('<html><body><div class="node__content">
    <div class="for-mobile"><h1>Matematikk 4</h1></div><div class="main-content">
    <h1>Matematikk 4</h1><div class="field--name-field-subject-code">MAT5006</div>
    <div class="placeholder-text">', intro, '</div>
    <div class="accordion-container"><div class="ac"><button class="ac-trigger">Beskrivelse av emnet</button>
    <div class="ac-panel"><p>Masteroppgaven er et selvstendig forskningsarbeid i matematikkdidaktikk.</p>
    <p>Studenten mottar veiledning.</p></div></div></div></div>
    <section class="sidebar"><div class="semester-chooser">Velg semester</div>
    <div class="field--name-field-credits"><div>Studiepoeng</div><div>30</div></div>
    <div class="field--name-field-p-employees"><h2>Emneansvarlig</h2><div>Ola Nordmann</div></div>
    </section></div></body></html>')
  cfg <- .block_cfg(get_institution_config("nord"))
  cc <- function(intro) {
    b <- page_blocks(page(intro), NA, cfg)
    out <- page_sections(b, .blocks_text(b), cfg)
    out$raw_text[out$section == "course_content"]
  }
  expect_equal(cc("Emnet gir innsikt i aktuell forskning."), paste(
    "Emnet gir innsikt i aktuell forskning.",
    "Masteroppgaven er et selvstendig forskningsarbeid i matematikkdidaktikk.",
    "Studenten mottar veiledning.", sep = "\n\n"))
  # a pointer goes; a description that the section repeats is kept once
  only_body <- cc("")
  expect_equal(cc("Se kursinnhold."), only_body)
  expect_equal(cc("Masteroppgaven er et selvstendig forskningsarbeid i matematikkdidaktikk."),
               only_body)
  # the sidebar's facts are in the text, once, and in no section; staff and
  # the semester picker are not
  txt <- .blocks_text(page_blocks(page(""), NA, cfg))
  expect_match(txt, "^Matematikk 4\nMAT5006.*Studiepoeng\n30$")
  expect_false(grepl("Nordmann|Velg semester", txt))
  expect_false(grepl("Studiepoeng", cc("")))
})

test_that("a prerequisites label in course content moves what it names", {
  uit <- paste("Emnet er et fagvalg i grunnskolelærerutdanning.", "",
               "Forkunnskapskrav og anbefalte forkunnskaper:", "",
               "Jf. opptakskrav og progresjonskrav i studieplanen.", "",
               "Emnet består av naturfagdidaktikk.", sep = "\n")
  res <- .split_inline_prerequisites(tibble::tibble(section = "course_content", raw_text = uit))
  expect_equal(res$raw_text, c("Emnet er et fagvalg i grunnskolelærerutdanning.\n\nEmnet består av naturfagdidaktikk.",
                               "Jf. opptakskrav og progresjonskrav i studieplanen."))
  expect_equal(res$section, c("course_content", "prerequisites"))
  # the admission pointer alone leaves no prerequisites row
  expect_equal(.clean_sections(res)$section, "course_content")
  mf <- tibble::tibble(section = c("course_content", "prerequisites"),
                       raw_text = c("Praksis i skolen.\nForkunnskapskrav: 140 stp. emner inkl PED1010",
                                    "Opptak til lektorprogrammet."))
  res <- .split_inline_prerequisites(mf)
  expect_equal(res$raw_text, c("Praksis i skolen.",
                               "Opptak til lektorprogrammet.\n\n140 stp. emner inkl PED1010"))
  # a sentence about prerequisites is not a label; a list of headings names nothing
  txt <- tibble::tibble(section = "course_content",
                        raw_text = "Emnet bygger på forkunnskaper fra Norsk 1.")
  expect_equal(.split_inline_prerequisites(txt), txt)
  toc <- tibble::tibble(section = "course_content", raw_text = paste(
    "Undervisningssted", "Krav til forkunnskaper", "Anbefalte forkunnskaper",
    "Studiepoengsreduksjon", "Arbeids- og undervisningsformer", sep = "\n"))
  expect_equal(.split_inline_prerequisites(toc), toc)
})

test_that("section leftovers of the 2026-10-05 audit", {
  # an inline prerequisites label after a sentence starts its own line (uit)
  cc <- tibble::tibble(section = "course_content", raw_text = paste0(
    "Emnet kan ikke tas som enkeltemne.ForkunnskapskravJf. opptakskrav i studieplanen.\n",
    "Emnet tas som enkeltemne. Forkunnskapskrav: Avlagt bachelorgrad i sosiologi."))
  res <- .clean_sections(.split_inline_prerequisites(cc))
  expect_equal(res$raw_text, c("Emnet kan ikke tas som enkeltemne.\nEmnet tas som enkeltemne.",
                               "Avlagt bachelorgrad i sosiologi."))
  expect_equal(res$section, c("course_content", "prerequisites"))
  # uit 2020s gate table: the grading cell goes, the gate names stay
  cw <- tibble::tibble(section = "coursework_requirements", raw_text = paste0(
    "Følgende arbeidskrav må være godkjent\nTilstedeværelse\tKarakterregel:\n",
    "Godkjent – ikke godkjent\t\nProsjektskisse\tKarakterregel:\nGodkjent – ikke godkjent\t"))
  expect_equal(.clean_sections(cw, "uit")$raw_text,
               "Følgende arbeidskrav må være godkjent\nTilstedeværelse\nProsjektskisse")
  # a Leganto tab bar goes; each list keeps its label
  rl <- tibble::tibble(section = "reading_list", raw_text = paste(
    "Norsk, emne 2 (MG1NO2) Drammen", "Norsk 1-7 (MG1NO2) Notodden",
    "Norsk, emne 2 (MG1NO2) Drammen", "Obligatorisk litteratur", "Bok A",
    "Norsk 1-7 (MG1NO2) Notodden", "Bok B", sep = "\n"))
  expect_equal(.clean_sections(rl)$raw_text, paste(
    "Norsk, emne 2 (MG1NO2) Drammen", "Obligatorisk litteratur", "Bok A",
    "Norsk 1-7 (MG1NO2) Notodden", "Bok B", sep = "\n"))
  # a row that only lists headings (uib shell page) is dropped
  shell <- tibble::tibble(section = "course_content", raw_text = paste(
    "Læringsutbytte", "Fulltid/deltid", "Krav til forkunnskaper", "Vurderingsformer",
    "Karakterskala", "Studienivå (studiesyklus)", sep = "\n"))
  expect_equal(nrow(.clean_sections(shell)), 0)
  expect_equal(nrow(.clean_sections(dplyr::mutate(shell, raw_text = paste0(
    raw_text, "\nEmnet gir en innføring i norsk språk.")))), 1)
})

# --- Exam logistics and notices ---

test_that("exam-logistics headings end the assessment section", {
  expect_equal(match_heading_to_section("Mer om eksamen ved UiO"), ".drop")
  expect_equal(match_heading_to_section("Sensorordning"), ".drop")
  # exact rows only: a combined heading is still assessment
  expect_equal(match_heading_to_section("Eksamen og sensorordning"), "assessment")
})

test_that("exam aids, exam language and absence rules keep their heading as a line", {
  expect_equal(match_heading_to_section("Hjelpemidler"), "assessment")
  expect_equal(match_heading_to_section("Hjelpemiddel ved eksamen"), "assessment")
  expect_equal(match_heading_to_section("Fravær fra obligatorisk aktivitet"),
               "coursework_requirements")
  # nord: an accordion of its own; hvl: a heading after the assessment
  html <- "<main><h2>Vurdering</h2><p>Skriftlig eksamen, 4 timer.</p>
    <h2>Hjelpemidler til eksamen</h2><p>Ingen</p>
    <h2>Eksamensspråk</h2><p>-</p>
    <h2>Innhold</h2><p>Algebra.</p></main>"
  s <- sections_of(html, html_cfg())
  expect_equal(s[["assessment"]], "Skriftlig eksamen, 4 timer.\n\nHjelpemidler til eksamen\nIngen")
  # a label with nothing under it, or a pointer, goes; a page of them has no section
  html <- "<main><h2>Hjelpemidler ved eksamen</h2><p>Se fagplanen.</p>
    <h2>Hjelpemiddel til eksamen</h2></main>"
  expect_length(sections_of(html, html_cfg()), 0)
  # hvl: a dash and the aids link go; "Ingen" stays without the link
  html <- "<main><h3>Vurdering</h3><p>Muntlig eksamen.</p>
    <h3>Hjelpemiddel ved eksamen</h3><p>-</p><p>Meir om hjelpemiddel</p>
    <h3>Hjelpemidler ved eksamen</h3><p>Ingen</p><p>Mer om hjelpemidler</p></main>"
  out <- page_sections(page_blocks(html, NA, html_cfg(heading = "h3")), NA,
                       c(html_cfg(heading = "h3"), institution = "hvl"))
  expect_equal(out$raw_text, "Muntlig eksamen.\n\nHjelpemidler ved eksamen\nIngen")
  # a label line in running text keeps its list after a blank line (uit)
  txt <- "Skoleeksamen.\n\nTillatte hjelpemidler til eksamen:\n\nKalkulator"
  expect_equal(.clean_sections(tibble::tibble(section = "assessment", raw_text = txt), "uit")$raw_text, txt)
  # text reader: the label line opens assessment and stays
  res <- text_sections("Innhold\nAlgebra.\n\nTillatte hjelpemidler\nKalkulator.")
  expect_equal(res$raw_text[res$section == "assessment"], "Tillatte hjelpemidler\nKalkulator.")
  # a label phrase in running text is no label heading (hiof)
  html <- "<main><h2>Eksamen</h2><p>Skriftlig eksamen, 5 timer<br>Tillatte hjelpemidler: Ordbok.</p></main>"
  expect_equal(sections_of(html, html_cfg())[["assessment"]],
               "Skriftlig eksamen, 5 timer\nTillatte hjelpemidler: Ordbok.")
})

test_that("practicum headings are teaching methods", {
  expect_equal(match_heading_to_section("Praksis"), "teaching_methods")
  expect_equal(match_heading_to_section("Gjennomføring av praksis"), "teaching_methods")
  # exact rows only: other headings that mention praksis are unchanged
  expect_true(is.na(match_heading_to_section("Praksisrapport")))
})

test_that("resit heading variants are dropped", {
  for (h in c("Vilkår for ny/utsatt eksamen", "Ny/utsett eksamen",
              "Ny eller utsatt eksamen", "Kontinuasjonseksamen")) {
    expect_equal(match_heading_to_section(h), ".drop", label = h)
  }
})

test_that("uio: exam language and the grading scale stay in assessment", {
  html <- "<main><h2>Eksamen</h2><p>Skriftlig eksamen, 4 timer.</p>
    <h3>Eksamensspråk</h3><p>Nynorsk.</p>
    <h3>Karakterskala</h3><p>Bestått/ikke bestått.</p>
    <p>Sensurordning:</p><p>To sensorer.</p><p>Sensurering:</p><p>Deleksamen 1 vektes 60 %.</p></main>"
  s <- sections_of(html, html_cfg("h3, h4, p"))
  expect_equal(s[["assessment"]], paste0("Skriftlig eksamen, 4 timer.\nEksamensspråk\nNynorsk.",
                                         "\n\nKarakterskala\nBestått/ikke bestått.",
                                         "\n\nDeleksamen 1 vektes 60 %."))
})

test_that("notices about the assessment stay in it", {
  txt <- paste("Skriftlig eksamen, 4 timer.",
               "Å generere besvarelse ved hjelp av ChatGPT er å regne som fusk.",
               "MERK: Skriftlig skoleeksamen endres til hjemmeeksamen.",
               "3 timers skoleeksamen erstattes av hjemmeeksamen pga. koronapandemien.",
               "Oppgaver blir sjekket for plagiat.",
               "Vurderingsform\tGruppering\tVarighet\tKarakterskala",
               "Mappe\tIndividuell\t\tA-F", sep = "\n")
  out <- tibble::tibble(section = "assessment", raw_text = txt)
  expect_equal(.clean_sections(out, "nord")$raw_text, txt)
  # a pointer to the AI page with no rule of its own goes
  out <- tibble::tibble(section = "assessment", raw_text = paste0(
    "Er å regne som fusk. Informasjon om ChatGPT/kunstig intelligens mm. finner du her.\n",
    "Alle hjelpemidler er tillatt. For informasjon om bruk av kunstig intelligens (KI) se ",
    "Innlevering av heimeeksamen og oppgåver - Høgskulen på Vestlandet (hvl.no)."))
  expect_equal(.clean_sections(out, "nord")$raw_text,
               sub(" Informasjon om ChatGPT[^\n]*", "", out$raw_text))
  # and the links to the aids page and the exam guidelines
  nd <- tibble::tibble(section = "assessment", raw_text = paste(
    "Hjelpemidler til eksamen", "Alle hjelpemidler tillatt. Generell informasjon om hjelpemidler til eksamen finner du her.",
    "", "Praktisk informasjon og retningslinjer for eksamen.", "Les meir om retningslinjer og praktisk informasjon om eksamen",
    sep = "\n"))
  expect_equal(.clean_sections(nd, "nord")$raw_text, "Hjelpemidler til eksamen\nAlle hjelpemidler tillatt.")
  nd$raw_text <- "Karakter A-F.\n\nHjelpemidler til eksamen\nGenerell informasjon om hjelpemidler til eksamen finner du her.\n\nÅ generere besvarelse ved hjelp av ChatGPT er fusk."
  expect_equal(.clean_sections(nd, "nord")$raw_text, "Karakter A-F.\n\nÅ generere besvarelse ved hjelp av ChatGPT er fusk.")
  expect_equal(.clean_sections(out, "hvl")$raw_text, paste0(
    "Er å regne som fusk. Informasjon om ChatGPT/kunstig intelligens mm. finner du her.\n",
    "Alle hjelpemidler er tillatt."))
  # a dated change-log entry goes, the text under it stays (hiof)
  for (s in c("assessment", "coursework_requirements")) {
    out <- tibble::tibble(section = s, raw_text = paste(
      "13.03.2023: Eksamensform våren 2023 er endret etter vurdering av programansvarlig.",
      "Muntlig eksamen, 30 minutter.", "26.09.2018: Endret arbeidskrav knyttet til praksis.",
      "Arbeidskrav må være godkjent.", sep = "\n"))
    expect_equal(.clean_sections(out, "hiof")$raw_text,
                 "Muntlig eksamen, 30 minutter.\nArbeidskrav må være godkjent.", info = s)
  }
})

test_that("hvl: exam logistics and costs leave every section, the rest of the line stays", {
  out <- tibble::tibble(
    section = c("assessment", "teaching_methods", "coursework_requirements", "course_content"),
    raw_text = c(
      paste("Skriftlig skoleeksamen, 5 timer. Tid og sted for eksamen blir opplyst på Studentweb.",
            "Tid og stad for eksamen blir gitt på Canvas.", "Karakterskala A-F.", sep = "\n"),
      paste("Forelesningar og reknetimar.",
            "Emnet undervisast i haustsemesteret. Eksamen blir avholdt i starten av Januar.",
            "Kr 400 i materialpenger betales inn ved studiestart (150 for naturfag).",
            "En ekskursjon om vinteren med to overnattinger. Pris ca. kr 1600.",
            "I forbindelse med ekskursjoner må studenter ved HVL være forberedt på å betale en egenandel. Det blir lagt til rette for kompensatorisk arbeidskrav.",
            sep = "\n"),
      # a sentence that also names a coursework requirement stays
      "Fag 3: Eit arbeidskrav om digital kompetanse I samband med ekskursjonar må studentar betale ein eigenandel.",
      "Studenten betalar materialpengar (sum framgår i semesterplanen)"))
  res <- .clean_sections(out, "hvl")
  expect_equal(res$raw_text, c(
    "Skriftlig skoleeksamen, 5 timer.\nKarakterskala A-F.",
    paste("Forelesningar og reknetimar.", "Emnet undervisast i haustsemesteret.",
          "En ekskursjon om vinteren med to overnattinger.",
          "Det blir lagt til rette for kompensatorisk arbeidskrav.", sep = "\n"),
    out$raw_text[3]))
  # other institutions keep them
  expect_equal(.clean_sections(out[1, ], "nla")$raw_text, out$raw_text[1])
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

# --- Inline coursework lines ---

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

test_that("uit 2008: 'Beskriving av arbeidskrav' and a numbered gate point move out", {
  split <- function(x) {
    res <- .split_inline_coursework(tibble::tibble(section = "assessment", raw_text = x))
    stats::setNames(res$raw_text, res$section)
  }
  s <- split(paste("Beskriving av arbeidskrav: Eit munnleg innlegg. Vurderast som godkjent/ikke godkjent.",
                   "Beskriving av eksamen: 1 essay på 10-12 sider. Vurderes etter karakterskalaen A-F."))
  expect_match(s[["coursework_requirements"]], "^Beskriving av arbeidskrav: Eit munnleg innlegg")
  expect_equal(s[["assessment"]], "Beskriving av eksamen: 1 essay på 10-12 sider. Vurderes etter karakterskalaen A-F.")
  s <- split(paste("1) Ved semesterstart får studenter beskjed om krav til antall godkjente oppgaver",
                   "for å gå opp til eksamen. 2) Skriftlig eksamen utgjør 100 % av sluttkarakter."))
  expect_match(s[["coursework_requirements"]], "godkjente oppgaver for å gå opp til eksamen\\.$")
  expect_equal(s[["assessment"]], "2) Skriftlig eksamen utgjør 100 % av sluttkarakter.")
})

test_that("a gate label takes the paragraph it names along", {
  split <- function(...) {
    res <- .split_inline_coursework(tibble::tibble(section = "assessment",
                                                   raw_text = paste(..., sep = "\n")))
    stats::setNames(res$raw_text, res$section)
  }
  # uit: colon-ended lead-in, then a list of short items
  s <- split("Følgende arbeidskrav må være godkjent før man kan framstille seg for eksamen:", "",
             "70 % deltakelse på all undervisning", "25 minutters framlegg", "",
             "Eksamen består av:", "", "Individuell muntlig eksamen.")
  expect_equal(s[["coursework_requirements"]], paste(
    "Følgende arbeidskrav må være godkjent før man kan framstille seg for eksamen:",
    "70 % deltakelse på all undervisning", "25 minutters framlegg", sep = "\n"))
  expect_match(s[["assessment"]], "^\\s*Eksamen består av:\n\nIndividuell muntlig eksamen\\.$")
  # nord: a label without a colon takes a body paragraph, not another label
  s <- split("Sammensatt vurdering.", "", "Obligatorisk deltakelse (OD)", "",
             "Det kreves 80 prosent deltakelse. Teller 0/100.", "", "Eksamen", "",
             "Inntil 4 arbeidskrav (AK). Teller 0/100 av karakter.", "",
             "Obligatorisk arbeid (OA)", "", "Mappe (MA)", "", "Skriftlig eksamen.")
  expect_equal(s[["coursework_requirements"]], paste(
    "Obligatorisk deltakelse (OD)", "Det kreves 80 prosent deltakelse. Teller 0/100.",
    "Inntil 4 arbeidskrav (AK). Teller 0/100 av karakter.", "Obligatorisk arbeid (OA)",
    sep = "\n"))
  expect_match(s[["assessment"]], "Eksamen\n+Mappe \\(MA\\)\n+Skriftlig eksamen\\.$")
  # a label stops at the next assessment component; "Arbeidskrav 3: x" is no label
  s <- split("Arbeidskrav (AK):", "AK1 Skriftlig arbeid. Teller 0/100.",
             "Praktisk eksamen (PE): Individuell. Teller 100/100.", "",
             "Arbeidskrav 3: utvikle undervisningsdesign", "",
             "Individuell presentasjon (MU), teller 100/100 av karakteren")
  expect_equal(s[["coursework_requirements"]], paste(
    "Arbeidskrav (AK):", "AK1 Skriftlig arbeid. Teller 0/100.",
    "Arbeidskrav 3: utvikle undervisningsdesign", sep = "\n"))
  expect_match(s[["assessment"]], "^Praktisk eksamen \\(PE\\).*\n+Individuell presentasjon")
  # gate and assessment on one line (uit master theses)
  s <- split("Arbeidskrav: Prosjektskissen skal godkjennes. Vurdering: Oppgaven vurderes A-F.")
  expect_equal(s[["coursework_requirements"]], "Arbeidskrav: Prosjektskissen skal godkjennes.")
  expect_equal(s[["assessment"]], "Vurdering: Oppgaven vurderes A-F.")
  # ... but the gate's own pass/fail stays with it (nord)
  s <- split("Obligatorisk deltakelse (OD): Krav om 80 %. Vurdering: Godkjent/ikke godkjent.",
             "Obligatorisk arbeid (OA): Tre oppgaver. Vurdering:", "", "Godkjent/ikke godkjent.",
             "", "Skriftlig eksamen (S).")
  expect_equal(trimws(s[["assessment"]]), "Skriftlig eksamen (S).")
  # ... and the paragraphs after it that still speak of a gate, no more
  s <- split("Obligatorisk deltakelse (OD)", "", "Aktivt samarbeid er nødvendig.", "",
             "Det er derfor obligatorisk oppmøte.", "", "Skriftlig skoleeksamen, 6 timer.")
  expect_equal(s[["coursework_requirements"]], paste("Obligatorisk deltakelse (OD)",
    "Aktivt samarbeid er nødvendig.", "Det er derfor obligatorisk oppmøte.", sep = "\n"))
  expect_equal(trimws(s[["assessment"]]), "Skriftlig skoleeksamen, 6 timer.")
  s <- split("ARBEIDSKRAV: En presentasjon.EKSAMEN: Semesteroppgave.")
  expect_equal(s[["assessment"]], "EKSAMEN: Semesteroppgave.")
  s <- split("Arbeidskrav: 12 av 18 oppgaver. Emnet evalueres med skoleeksamen.")
  expect_equal(s[["assessment"]], "Emnet evalueres med skoleeksamen.")
})

test_that("a gate heading takes its block, up to a heading or the exam (ntnu)", {
  split <- function(...) {
    res <- .split_inline_coursework(tibble::tibble(section = "assessment",
                                                   raw_text = paste(..., sep = "\n")))
    stats::setNames(res$raw_text, res$section)
  }
  # numbered requirements across blank lines, up to the next heading
  s <- split("OBLIGATORISKE ARBEIDSKRAV:", "1. Delta på innføring i kartlegging.", "",
             "2. Prøve ut materiell i gruppe. Arbeidskravet skal utføres i gruppe.", "",
             "Obligatoriske arbeidskrav vurderes som godkjent/ikke godkjent.", "",
             "Individuell fordypningsoppgave",
             "Oppgaven skal være et individuelt faglig arbeid.", "",
             "Vurderingsordning: Oppgave", "Karakter: Bokstavkarakterer")
  expect_equal(s[["coursework_requirements"]], paste(
    "OBLIGATORISKE ARBEIDSKRAV:", "1. Delta på innføring i kartlegging.",
    "2. Prøve ut materiell i gruppe. Arbeidskravet skal utføres i gruppe.",
    "Obligatoriske arbeidskrav vurderes som godkjent/ikke godkjent.", sep = "\n"))
  expect_match(s[["assessment"]], "^\\s*Individuell fordypningsoppgave\nOppgaven skal.*Karakter: Bokstavkarakterer$")
  # without a blank line, a paragraph on the thesis ends the block (MGLU5222-A)
  s <- split("Obligatoriske arbeidskrav:", "- Midtveisseminar",
             "Obligatoriske arbeidskrav må være godkjent før en kan framstille seg til eksamen.",
             "Masteroppgaven skrives normalt i monografiform.", "Vurderingsordning: Masteroppgave")
  expect_equal(s[["assessment"]], "Masteroppgaven skrives normalt i monografiform.\nVurderingsordning: Masteroppgave")
  # short items without blank lines, then the exam with no blank line before it
  s <- split("Obligatoriske aktiviteter", "Muntlig gruppepresentasjon", "Inntil 8 skriftlige oppgaver",
             "Alle må være godkjent før eksamen.", "Inntil 2 obligatoriske arbeidskrav",
             "Muntlig eksamen.", "Evaluering", "Skriftlig hjemmeeksamen (5 dager).")
  expect_equal(s[["coursework_requirements"]], paste(
    "Obligatoriske aktiviteter", "Muntlig gruppepresentasjon", "Inntil 8 skriftlige oppgaver",
    "Alle må være godkjent før eksamen.", "Inntil 2 obligatoriske arbeidskrav", sep = "\n"))
  expect_equal(s[["assessment"]], "Muntlig eksamen.\nEvaluering\nSkriftlig hjemmeeksamen (5 dager).")
  # over a colon lead-in the heading is a label: the lead-in takes its list (uit)
  s <- split("Arbeidskrav:", "", "Følgende arbeidskrav må være godkjent før eksamen:", "",
             "Et muntlig framlegg", "En kortere skriftlig oppgave", "",
             "Alle skriftlige arbeider skal ha 1,5 linjeavstand.", "", "Eksamen består av:")
  expect_equal(s[["coursework_requirements"]], paste("Arbeidskrav:",
    "Følgende arbeidskrav må være godkjent før eksamen:", "Et muntlig framlegg",
    "En kortere skriftlig oppgave", sep = "\n"))
  # a colon lead-in stops at the assessment proper
  s <- split("Følgende arbeidskrav må være godkjent før eksamen:", "To innleveringer",
             "Eksamen: Mappe", "Vurdering gis med bokstavkarakter.")
  expect_equal(s[["assessment"]], "Eksamen: Mappe\nVurdering gis med bokstavkarakter.")
})

test_that("nord abbreviations, number words and English labels are gates", {
  split <- function(...) {
    res <- .split_inline_coursework(tibble::tibble(section = "assessment",
                                                   raw_text = paste(..., sep = "\n")))
    stats::setNames(res$raw_text, res$section)
  }
  # "OD:"/"AK:" and "• OD"/"• AK" without a number (NOR5005, AKV6000)
  s <- split("OD: Obligatorisk deltakelse, min. 80% oppmøte. Godkjent/ikke godkjent.", "",
             "AK: Studenten skal ha godkjent inntil tre faglige arbeidskrav.", "",
             "• AK Arbeidskrav, godkjent / ikke godkjent.", "",
             "Oppgave med justerende muntlig (OMU). A-F. Vektes 100 % av karakteren.")
  expect_equal(s[["coursework_requirements"]], paste(
    "OD: Obligatorisk deltakelse, min. 80% oppmøte. Godkjent/ikke godkjent.",
    "AK: Studenten skal ha godkjent inntil tre faglige arbeidskrav.",
    "• AK Arbeidskrav, godkjent / ikke godkjent.", sep = "\n"))
  expect_match(s[["assessment"]], "^\\s*Oppgave med justerende muntlig")
  # a number word (ORG2007)
  s <- split("To arbeidskrav. Godkjent/Ikke godkjent.", "", "Oppgave, individuelt. Karakter A-F.")
  expect_equal(s[["coursework_requirements"]], "To arbeidskrav. Godkjent/Ikke godkjent.")
  expect_equal(trimws(s[["assessment"]]), "Oppgave, individuelt. Karakter A-F.")
  # English: the label takes its requirement, not the graded part (PRO9011)
  s <- split("Coursework requirements:", "",
             "Individual presentation; represents 0/100 of the grade. Approved/not approved.",
             "Individual written assignment, represents 100/100 of the grade. Pass/fail")
  expect_equal(s[["coursework_requirements"]], paste("Coursework requirements:",
    "Individual presentation; represents 0/100 of the grade. Approved/not approved.", sep = "\n"))
  expect_equal(trimws(s[["assessment"]]), "Individual written assignment, represents 100/100 of the grade. Pass/fail")
  # an English heading stops at the evaluation (ntnu)
  s <- split("Compulsory assignments", "A one-week field course.", "Further on evaluation",
             "Oral exam (30%): Presentation.")
  expect_equal(s[["coursework_requirements"]], "Compulsory assignments\nA one-week field course.")
  expect_equal(s[["assessment"]], "Further on evaluation\nOral exam (30%): Presentation.")
  # a line naming a gate and an exam part stays in assessment too (SOS5003)
  s <- split("Arbeidskrav og hjemmeeksamen (3 dager)")
  expect_equal(s[["assessment"]], "Arbeidskrav og hjemmeeksamen (3 dager)")
  expect_equal(s[["coursework_requirements"]], "Arbeidskrav og hjemmeeksamen (3 dager)")
  # ... as does a gate line that counts towards the grade
  s <- split("AK og refleksjonsnotat. Bestått/ikke bestått. Vektes 50 %.")
  expect_equal(s[["assessment"]], "AK og refleksjonsnotat. Bestått/ikke bestått. Vektes 50 %.")
  # not a gate: lowercase "od", a word that starts with AK, a gate merely before the exam
  out <- tibble::tibble(section = "assessment", raw_text = paste(
    "AKV6000 vurderes med mappe.", "od: ikke en etikett", "Eksamen etter arbeidskrav og seminar.",
    sep = "\n"))
  expect_equal(.split_inline_coursework(out), out)
})

test_that("a gate sentence moves to coursework_requirements, at every institution", {
  split <- function(..., labels = TRUE) {
    res <- .split_inline_coursework(tibble::tibble(section = "assessment",
                                                   raw_text = paste(..., sep = "\n")), labels)
    stats::setNames(res$raw_text, res$section)
  }
  # one sentence among others moves alone (uit 2008)
  s <- split("Kurset avsluttes med 4 t skriftlig eksamen. Tilgang til slutteksamen forutsetter godkjente øvelser. Bokstavkarakterer (A-F).")
  expect_equal(s[["assessment"]], "Kurset avsluttes med 4 t skriftlig eksamen. Bokstavkarakterer (A-F).")
  expect_equal(s[["coursework_requirements"]], "Tilgang til slutteksamen forutsetter godkjente øvelser.")
  # without the label split (uib), only the sentence moves
  s <- split("Skriftleg skuleeksamen, 4 timar.", "Godkjend obligatorisk arbeid er ein føresetnad for å gå opp til eksamen.",
             labels = FALSE)
  expect_equal(s[["assessment"]], "Skriftleg skuleeksamen, 4 timar.")
  expect_equal(s[["coursework_requirements"]], "Godkjend obligatorisk arbeid er ein føresetnad for å gå opp til eksamen.")
  for (x in c("Arbeidskravene må være utført innen fastsatt frist og godkjent av faglærer før studenten kan fremstille seg til eksamen.",
              "Alle definerte arbeidskrav i emnet, må være forevist og godkjent av faglærer før avsluttende vurdering.",
              "Arbeidskrav inngår ikke i karakteren, men må utføres eller være godkjent innen satte frister for at emnet skal være bestått.",
              "For å kunne gå opp til eksamen må alle obligatoriske arbeidskrav være bestått.",
              "Obligatoriske innleveringer og terminprøve (juleprøve) må være bestått for å få adgang til avsluttende eksamen.",
              "Approval of this assignment is required in order to take the final exam.",
              "1 individual assignment must be approved in order to be allowed to take the written examination.",
              # a condition for handing in, presenting or grading the assessed work
              "For å kunne fremstille prosjektoppgaven til vurdering må deltakerne ha fått godkjent på mellomperiodearbeidet.",
              "Arbeidskrav må være godkjent for å få endelig karakter i emnet.",
              "For å kunne få endelig vurdering i emnet, må både OD og AK være vurdert til godkjent.",
              "Dei obligatoriske arbeidskrava må vere godkjende før ein kan få oppgåva vurdert.",
              "Alle oppgaver i kurset må være gjennomført og godkjent for at eksamensoppgaven skal kunne innleveres.",
              "All obligatorisk aktivitet må være bestått for å kunne levere bacheloroppgaven.",
              "Arbeidskrav og obligatorisk deltakelse må være bestått for fremstilling til hjemmeeksamen.",
              "The teacher must approve all three work requirements before the student can submit the semester paper.")) {
    expect_equal(split("Skriftlig eksamen.", x, labels = FALSE)[["coursework_requirements"]], x)
  }
  # usn's hard-wrapped sentence moves whole, line break and all
  s <- split("For at studenten skal kunne gå opp til eksamen, må obligatoriske skriftlige og muntlige",
             "arbeidskrav være godkjent. Omfang og form på arbeidskravene oppgis ved semesterstart.",
             "Skriftlig eksamen, 4 timer.")
  expect_equal(s[["coursework_requirements"]], paste(
    "For at studenten skal kunne gå opp til eksamen, må obligatoriske skriftlige og muntlige",
    "arbeidskrav være godkjent.", sep = "\n"))
  expect_equal(s[["assessment"]], "Omfang og form på arbeidskravene oppgis ved semesterstart.\nSkriftlig eksamen, 4 timer.")
  # uit English: "Assignments:" heads the paragraphs that speak of the gate
  s <- split("Assignments:", "", "Online students: Assignments to teacher-supplied questions. The assignments have to be approved by the teacher(s) before students are allowed to sit the final exam.",
             "", "Bachelor students: 3 written assignments. The assignments have to be approved before students are allowed to sit the final exam.",
             "", "Exam:", "", "A final written, home exam (3 days).")
  expect_match(s[["coursework_requirements"]], "^Assignments:\nOnline students: .*\nBachelor students: ")
  expect_match(s[["assessment"]], "^\\s*Exam:\n+A final written, home exam \\(3 days\\)\\.$")
  # a gate sentence ending in a colon takes its list across blank lines, and
  # "Requirements" goes with it
  s <- split("Requirements", "", "The following coursework requirements must be approved in order to take the exam:", "",
             "- A presentation.", "", "- An analysis of a text.", "", "Assessment method:", "A 4-hour school exam.")
  expect_equal(s[["coursework_requirements"]], paste("Requirements",
    "The following coursework requirements must be approved in order to take the exam:",
    "- A presentation.", "- An analysis of a text.", sep = "\n"))
  # ... and stops at a label naming other information (ntnu)
  s <- split("Obligatoriske aktiviteter:",
             "De følgende obligatoriske aktivitetene må være godkjent for å kunne levere eksamensoppgaven:",
             "En skriftlig refleksjon", "Annen relevant informasjon", "Samarbeid med en institusjon.")
  expect_equal(s[["assessment"]], "Annen relevant informasjon\nSamarbeid med en institusjon.")
  # ... but no line about the exam
  s <- split("Følgende arbeidskerav må være godkjent før man kan fremstille seg til eksamen:", "",
             "Eksamen og vurdering", "Muntlig eksamen.")
  expect_equal(s[["assessment"]], "\nEksamen og vurdering\nMuntlig eksamen.")
})

test_that("a gate sentence moved after the FS timestamp is kept (uia)", {
  out <- tibble::tibble(section = c("assessment", "coursework_requirements"), raw_text = c(
    "Vurderes til Bestått/Ikke bestått.\nFor at de kliniske studiene skal vurderes til Bestått, må studenten ha oppfylt kravet om tilstedeværelse.",
    "Studenten må ha fått godkjent to refleksjonsnotat.\nSist hentet fra FS (Felles studentsystem) 16. okt. 2026 11:44:59"))
  res <- .clean_sections(.split_inline_coursework(out, FALSE), "uia")
  expect_equal(res$raw_text[2], paste("Studenten må ha fått godkjent to refleksjonsnotat.",
    "For at de kliniske studiene skal vurderes til Bestått, må studenten ha oppfylt kravet om tilstedeværelse.", sep = "\n\n"))
})

test_that("requirements under the teaching or content heading move to coursework", {
  split <- function(section, ...) {
    out <- tibble::tibble(section = c(section, "assessment"),
                          raw_text = c(paste(..., sep = "\n"), "Skriftlig eksamen."))
    res <- .split_inline_coursework(out, "heading", section)
    stats::setNames(res$raw_text, res$section)
  }
  # a gate sentence, and the sentence a pointing-back gate names (nord)
  s <- split("teaching_methods", "Forelesninger og øvinger. Det blir gitt 2 øvinger som skal leveres i Excel. Begge disse kreves godkjent for adgang til eksamen.")
  expect_equal(s[["teaching_methods"]], "Forelesninger og øvinger.")
  expect_equal(s[["coursework_requirements"]],
               "Det blir gitt 2 øvinger som skal leveres i Excel. Begge disse kreves godkjent for adgang til eksamen.")
  s <- split("teaching_methods", "Studenten skal skrive ei seminaroppgåve. Dei obligatoriske arbeidskrava må vere godkjende før ein kan få oppgåva vurdert.")
  expect_equal(s[["teaching_methods"]], "Studenten skal skrive ei seminaroppgåve.")
  s <- split("teaching_methods", "Lectures: 40 h. Three mandatory homework sets. A passing grade is required of two homework sets for permission to take the exam.")
  expect_equal(s[["coursework_requirements"]], "A passing grade is required of two homework sets for permission to take the exam.")
  s <- split("teaching_methods", "Seminars and supervision. Mandatory course work: Attendance is mandatory in all seminars. Teaching and exam terms: A thesis seminar is offered.")
  expect_equal(s[["teaching_methods"]], "Seminars and supervision. Teaching and exam terms: A thesis seminar is offered.")
  expect_equal(s[["coursework_requirements"]], "Mandatory course work: Attendance is mandatory in all seminars.")
  s <- split("course_content", "Statistikk.", "The student must pass the project assignment before (s)he can take the exam.")
  expect_equal(s[["course_content"]], "Statistikk.")
  # a gate heading takes its block (ntnu), a line with nord's abbreviation moves
  s <- split("teaching_methods", "Forelesninger.", "", "Obligatoriske aktiviteter:", "Deltakelse på tre seminar",
             "75% oppmøte på seminar", "", "Undervisningen foregår på campus.")
  expect_equal(s[["coursework_requirements"]],
               "Obligatoriske aktiviteter:\nDeltakelse på tre seminar\n75% oppmøte på seminar")
  expect_match(s[["teaching_methods"]], "^Forelesninger\\.\n+Undervisningen foregår på campus\\.$")
  s <- split("teaching_methods", "Laboratoriekurs.", "Obligatorisk deltakelse (OD) på laboratorieøvelser")
  expect_equal(s[["coursework_requirements"]], "Obligatorisk deltakelse (OD) på laboratorieøvelser")
  # a line stating the assessment form moves to assessment, with the exam
  # labels under it (ntnu masters, nord)
  s <- split("teaching_methods", "Forelesninger, seminar, framlegg av deltakerne.",
             "Vurderingsform: Deltakelse, paper.", "Karakter: Bokstavkarakter (A-F)", "Undervisningen er på engelsk.")
  expect_equal(s[["teaching_methods"]], "Forelesninger, seminar, framlegg av deltakerne.\nUndervisningen er på engelsk.")
  expect_equal(s[["assessment"]], "Skriftlig eksamen.\n\nVurderingsform: Deltakelse, paper.\nKarakter: Bokstavkarakter (A-F)")
  expect_false("coursework_requirements" %in% names(s))
  res <- .split_inline_coursework(tibble::tibble(section = "teaching_methods",
    raw_text = "Vurderingsform: Masteroppgaven, samt 1 times muntlig eksamen."), "heading", "teaching_methods")
  expect_equal(res$raw_text[res$section == "assessment"], "Vurderingsform: Masteroppgaven, samt 1 times muntlig eksamen.")
  s <- split("teaching_methods", "Veiledning i apoteket.", "Vurderingsinnhold:  Kriterier for praksis",
             "Vurderingsform:    Midtveisvurdering og sluttvurdering")
  expect_equal(s[["teaching_methods"]], "Veiledning i apoteket.")
  # not: a teaching form named "Assignments", a mention of obligatory subjects,
  # a description of the arbeidskrav, a gate label that only says something
  out <- tibble::tibble(section = "teaching_methods", raw_text = paste(
    "Lectures", "Assignments", "Project work", "Det forutsettes at studentene deltar aktivt.",
    "Arbeidskravene har som forutsetning at studentene arbeider selvstendig.",
    "Arbeidskrav: 20 timer", "Vurdering: Godkjend/ ikkje godkjend.", "Eksamen: 6 timer", sep = "\n"))
  expect_equal(.split_inline_coursework(out, "heading", "teaching_methods"), out)
  out <- tibble::tibble(section = "course_content", raw_text = paste(
    "Studentene skal utvikle kompetanse for å kunne undervise og veilede elever i de obligatoriske programfagene."))
  expect_equal(.split_inline_coursework(out, "heading", "course_content"), out)
})

test_that("compulsory attendance stated alone moves from assessment, praksis attendance stays", {
  out <- tibble::tibble(section = "assessment", raw_text = paste(
    "Deltakelse på 80 % av undervisning. Deltakelse telles time for time.",
    "70 % obligatorisk deltakelse på seminarundervisning",
    "Hjemmeeksamen, 1 uke.", sep = "\n"))
  res <- .split_inline_coursework(out, FALSE)
  s <- stats::setNames(res$raw_text, res$section)
  expect_equal(s[["assessment"]], "Deltakelse telles time for time.\nHjemmeeksamen, 1 uke.")
  expect_equal(s[["coursework_requirements"]],
               "Deltakelse på 80 % av undervisning.\n70 % obligatorisk deltakelse på seminarundervisning")
  for (x in c("For å bestå praksis må studenten ha 100 % oppmøte.", "Det er krav om 100 % tilstedeværelse i praksis.")) {
    out <- tibble::tibble(section = "assessment", raw_text = x)
    expect_equal(.split_inline_coursework(out, FALSE), out)
  }
})

test_that("a uit lead-in takes its list though an item names the exam", {
  res <- .split_inline_coursework(tibble::tibble(section = "assessment", raw_text = paste(
    "Følgende arbeidskrav må være godkjent før man kan framstille seg til eksamen:",
    "framlegg av utkast til eksamensarbeid (hjemmeeksamen) i emnet", "utkast til bacheloroppgaven",
    "70 % deltakelse på undervisning", "", "Eksamen i emnet består av:", "en hjemmeoppgave", sep = "\n")))
  s <- stats::setNames(res$raw_text, res$section)
  expect_match(s[["coursework_requirements"]], "framlegg av utkast.*\n70 % deltakelse på undervisning$")
  expect_match(s[["assessment"]], "^\\s*Eksamen i emnet består av:\nen hjemmeoppgave$")
})

test_that("exam parts, exam tables and graded components stay in assessment", {
  keep <- function(x, labels = FALSE) {
    out <- tibble::tibble(section = "assessment", raw_text = x)
    expect_equal(.split_inline_coursework(out, labels), out)
  }
  # the exam's own parts or a pass of the exam, not a gate
  keep("Begge deleksamener må være bestått for å få samlet karakter i emnet (A-F).")
  keep("Mappen må som helhet være vurdert til bestått før studenten kan fremstille seg til muntlig eksamen.")
  keep("Studenten leverer ein tekst. Begge må vere bestått for å få godkjent eksamen.")
  keep("Alle delene må være godkjent/bestått for å få endelig karakter i emnet.")
  keep("Praksis må være bestått før eksamen kan avlegges.")
  keep("Emnet har en sammensatt vurdering som betyr at alle delene må være godkjent og bestått for å få endelig karakter i emnet.")
  keep("Friluftsliv, skikurs og kanotur er deleksamen som må være godkjent for å få karakter i emnet.")
  keep("Alle de 4 elementene i vurderingsmappen må vurderes som bestått for at kandidaten skal få godkjent vurderingsmappen.")
  keep("For å bestå en modul må man bestå modulens test og levere et godkjent inkrement av prosjektet.")
  # a sentence without a subject names its gate in the sentence before (uis)
  keep("Mappe bestående av 6 mindre arbeider. Må være godkjent for å kunne gå opp til eksamen.")
  # a row of an exam table (hivolda)
  keep(paste("Praktisk eksamen\tIndividuell\t4-6 Veker\tA-F\t40\tFramsyning. Framsyninga og",
             "forskingsoppgåva må begge vere godkjend for å få gjennomføre den munnlege eksamenen."))
  # timing, not a condition
  keep("48 timer før eksamen trekker kandidaten en undervisningsøkt, utarbeidet i arbeidskrav.")
  # usn: "Obligatorisk arbeid (40%)" counts towards the grade: it stays (and is copied)
  s <- .split_inline_coursework(tibble::tibble(section = "assessment", raw_text = paste(
    "Skriftlig eksamen 3 timer (60%)",
    "Obligatorisk arbeid (40%) bestående av fire like store deler gjennom semesteret.", sep = "\n")))
  expect_equal(s$raw_text[s$section == "assessment"], paste("Skriftlig eksamen 3 timer (60%)",
    "Obligatorisk arbeid (40%) bestående av fire like store deler gjennom semesteret.", sep = "\n"))
  expect_true(grepl(.graded_component_regex, "Obligatorisk arbeid (40%)", perl = TRUE))
  expect_false(grepl(.graded_component_regex, "Obligatorisk frammøte (80%)", perl = TRUE))
  expect_false(grepl(.graded_component_regex, "Obligatorisk deltakelse i laboratoriearbeid (100%).", perl = TRUE))
  # a gate sentence that counts towards the grade stays as well
  s <- .split_inline_coursework(tibble::tibble(section = "assessment", raw_text =
    "Hjemmeeksamen. Arbeidskravet må være godkjent før eksamen og teller 40 % av karakteren."), FALSE)
  expect_equal(s$raw_text[1], "Hjemmeeksamen. Arbeidskravet må være godkjent før eksamen og teller 40 % av karakteren.")
  expect_equal(s$raw_text[2], "Arbeidskravet må være godkjent før eksamen og teller 40 % av karakteren.")
})

test_that("usn's reading list loses the stock line after a list or a note", {
  out <- tibble::tibble(section = c("reading_list", "teaching_methods"),
                        raw_text = c("Egenvalgt litteratur\n\nPensum-/litteraturliste er ikke publisert ennå. ",
                                     "Forelesninger."))
  expect_equal(.clean_sections(out, "usn")$raw_text[1], "Egenvalgt litteratur")
  out$raw_text[1] <- "Ikke bestemt.\n\nPensum-/litteraturliste er ikke publisert ennå."
  expect_equal(.clean_sections(out, "usn")$section, "teaching_methods")
})

test_that("leading placeholders and praksis pointers go, content stays", {
  out <- tibble::tibble(
    section = c("prerequisites", "teaching_methods", "coursework_requirements"),
    raw_text = c("Ingen\n\nNOLI102 er tilrådd.",
                 paste("Forelesninger og seminarer.", "Praksis",
                       "For nærmere informasjon om praksis, se egen praksisplan.",
                       "Det er tre uker praksis i 6. semester. Se programplanen.", sep = "\n"),
                 "Ingen arbeidskrav."))
  s <- stats::setNames(.clean_sections(out)$raw_text, .clean_sections(out)$section)
  expect_equal(s[["prerequisites"]], "NOLI102 er tilrådd.")
  expect_equal(s[["teaching_methods"]], paste("Forelesninger og seminarer.",
    "Det er tre uker praksis i 6. semester. Se programplanen.", sep = "\n"))
  expect_false("coursework_requirements" %in% names(s))
})

test_that("uio: a lead-in naming obligatory parts opens coursework, admin labels close it", {
  html <- "<main><h2>Undervisning</h2><p>Seminarer hver uke.</p>
    <p>Obligatoriske forhold i undervisningen:</p><ul><li>Deltagelse i undervisning</li></ul>
    <p>Undervisningssted:</p><p>Universitetet i Oslo.</p></main>"
  s <- sections_of(html, html_cfg("h3, h4, p"))
  expect_equal(s[["teaching_methods"]], "Seminarer hver uke.")
  expect_equal(s[["coursework_requirements"]],
               "Obligatoriske forhold i undervisningen:\nDeltagelse i undervisning")
})

test_that("sentences that merely mention arbeidskrav stay in assessment", {
  out <- tibble::tibble(section = "assessment",
                        raw_text = "Mappen består av tre arbeidskrav og vurderes samlet.")
  expect_equal(.split_inline_coursework(out), out)
})

test_that("nmbu exam-table line stays in assessment", {
  txt <- "Muntlig eksamen.\nMuntlig eksamen Karakterregel: Bokstavkarakterer Hjelpemiddelkode: A1"
  res <- .clean_sections(tibble::tibble(section = "assessment", raw_text = txt), "nmbu")
  expect_equal(res$raw_text, txt)
})

test_that("an admission line that continues an either/or entry condition stays (oslomet)", {
  txt <- paste("Bestått første og andre studieår eller tilsvarende i bachelorstudium i bioingeniørfag, eller",
               "Opptak til kompletterende utdanning for bioingeniører", sep = "\n")
  expect_equal(.strip_admission_lines(txt), txt)
  # an admission line on its own still goes
  expect_equal(.strip_admission_lines("Opptak til lærerutdanningen.\nEmnet bygger på Norsk 1."),
               "Emnet bygger på Norsk 1.")
  # a conjunction left dangling by a dropped paragraph goes; one before a
  # kept paragraph stays
  expect_equal(trimws(.strip_admission_lines("Bestått MGL101, eller\n\nOpptak til lærerutdanningen.")), "Bestått MGL101")
  expect_equal(trimws(.strip_admission_lines("Bestått MGL101\n\neller\n\nOpptak til lærerutdanningen.")), "Bestått MGL101")
  expect_equal(trimws(.strip_admission_lines("Opptak til lærerutdanningen.\n\neller\n\nBestått MGL101")), "Bestått MGL101")
  txt <- "Bestått MGL101, eller\n\nBestått MGL102\n\nOpptak til lærerutdanningen."
  expect_equal(trimws(.strip_admission_lines(txt)), "Bestått MGL101, eller\n\nBestått MGL102")
  # a line after a comma is its own line (nord: a graded exam after a gate)
  lines <- c("Obligatorisk deltakelse (OD): Minimum 80% deltagelse. Teller 0/100 av karakteren,",
             "Praktisk eksamen (PE): Individuell, varighet 30 minutter. Teller 100/100 av karakteren.")
  expect_equal(.unwrap_lines(lines), lines)
})

test_that("admission sentences are removed from prerequisites, real ones kept", {
  txt <- paste("Opptak skjer på bakgrunn av generell studiekompetanse eller realkompetanse.",
               "Kan tas som frittstående fag i lærerutdanning.",
               "Bestått MGL101 og opptak til lærerutdanningen.",
               "Emnet bygger på Norsk 1.", sep = "\n")
  res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt), "nord")
  expect_equal(res$raw_text, "Bestått MGL101 og opptak til lærerutdanningen.\nEmnet bygger på Norsk 1.")
})

test_that("recommendations that mention admission are kept", {
  txt <- paste("I tillegg til generell studiekompetanse bør studenten ha engelsk.",
               "Generell studiekompetanse inklusive lulesamisk fra videregående skole.",
               "Kan tas som frittstående fag i lærerutdanning. Bygger på 1A 1-7",
               "Generell studiekompetanse", sep = "\n")
  res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt), "nord")
  expect_equal(res$raw_text, paste(c(strsplit(txt, "\n")[[1]][1:2], "Bygger på 1A 1-7"),
                                   collapse = "\n"))
})

test_that("admission, study right and pointers to them leave prerequisites", {
  clean <- function(txt) {
    res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt))
    if (nrow(res) == 0) NA_character_ else res$raw_text
  }
  for (t in c("Forutsetter opptak til femårig lektorutdanning med master i språkfag, studieretning tysk.",
              "Opptak på 5-årig lærerutdanning i samfunnsfag",
              "Se fagplanen under Opptakskrav.",
              "Jf. Opptakskrav og progresjonskrav i studieplanen.",
              "Som opptakskrav til studieprogrammet, se studiets studieplan.",
              "Det er ingen forkunnskaper utover opptakskrav i studieplanen for Master grunnskolelærerutdanning, 5-10.",
              "Se forkunnskapskrav\n\nEmnet et forbeholdt studenter på femårig lektorutdanning i kroppsøving og idrettsfag",
              paste("Emnet er forebeholdt studenter på følgende studieprogram:", "",
                    "Lektorutdanning i kroppsøving og idrettsfag", "Grunnskolelærerutdanning 1-7", "",
                    "Det kreves ingen forkunnskaper utover opptakskravet.", sep = "\n"))) {
    expect_true(is.na(clean(t)), label = t)
  }
  # the prerequisite stays, with its spacing; a label over a removed pointer goes
  expect_equal(clean("Fullført 52,5 sp. geografi. Emnet er forbeholdt studenter tatt opp til femårig lektorutdanning i geografi."),
               "Fullført 52,5 sp. geografi.")
  expect_equal(clean("Bestått PEL 1 eller tilsvarende.Gjennomført betyr eksamensklar. Emnet er forbeholdt studenter med studierett ved grunnskolelærerutdanningen"),
               "Bestått PEL 1 eller tilsvarende.Gjennomført betyr eksamensklar.")
  expect_equal(clean("Gode engelskkunnskaper\n\nForkunnskapskrav:\nSom opptakskrav til studieprogrammet, se studiets studieplan."),
               "Gode engelskkunnskaper")
  expect_equal(clean("Nei. Forutsetter opptak på grunnskolelærerutdanningen.\n\nMå ha bestått sikkerhetskurs."),
               "Må ha bestått sikkerhetskurs.")
  expect_equal(clean("Se forkunnskapskrav\n\nIDR3038 eller tilsvarende."), "IDR3038 eller tilsvarende.")
  expect_equal(clean("Det anbefales Engelsk 1.\n\nKrever opptak til studieprogram: Grunnskolelærerutdanning 5.-10. trinn (MGLU5-10)"),
               "Det anbefales Engelsk 1.")
  # mixed sentences and recommendations stay
  for (t in c("Opptak til femårig lektorutdanning og beståtte eksamener i henhold til forskrift.",
              "For opptak til emnet kreves fullført minst del 1 av praktisk-pedagogisk utdanning.",
              "Ingen utover opptakskravet, men det er en fordel å kunne lese noter.",
              "Generell studiekompetanse Det blir venta at studentane meistrar begge målformer.")) {
    expect_equal(clean(t), t, label = t)
  }
})

test_that("nla: sections are read from the EmneplanPage JSON in a <script>", {
  f <- harvest_files("nla", test_path("../../data/raw"))
  skip_if_not(length(f) > 0, "harvested nla data not available")
  d <- read_raw(f)
  d <- d[!is.na(d$html), ][1:20, ]
  out <- extract_sections(get_institution_config("nla"), d$html, d$extracted_text, d$course_id)
  expect_gt(nrow(out), 0)
})

test_that("extract_sections returns its columns when there are no pages", {
  out <- extract_sections(get_institution_config("samas"), character(), character(), character())
  expect_named(out, c("course_id", "institution", "section", "raw_text"))
  expect_equal(nrow(out), 0)
})

test_that("facts: the title and facts box are in the text, not in a section or the plan text", {
  html <- "<main><h1>MAT101 Algebra</h1><aside class='f'><p>Studiepoeng: 10</p><h2>Innhold</h2>
    <span class='c'>Campus Bodø</span></aside><h2>Innhold</h2><p>Grupper og ringer.</p></main>"
  cfg <- c(html_cfg(), list(facts = c("main > h1", "aside.f")))
  b <- page_blocks(html, NA, cfg)
  expect_equal(b$facts, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_match(page_fulltext(b, list()), "^MAT101 Algebra\\s+Studiepoeng: 10\\s+Innhold\\s+Campus Bodø\\s+Innhold")
  expect_equal(page_fulltext(b, list(), facts = FALSE), "Innhold\n\nGrupper og ringer.")
  # a heading inside the facts box opens no section
  expect_equal(sections_of(html, cfg), c(course_content = "Grupper og ringer."))
})

test_that("the text fallback stands only with a section that no heading on the page names", {
  # ntnu's terse plans: the html reader saw "Om emnet" and "Eksamen"; under
  # "Om emnet" there are only admission and subject areas
  html <- "<main><h2>Om emnet</h2><h3>Spesielle vilkår</h3><div>Krever opptak.</div>
    <h2>Eksamen</h2><div>Avhandling</div></main>"
  cfg <- html_cfg(heading = "h2, h3")
  text <- "Om emnet\nSpesielle vilkår\nKrever opptak.\n\nEksamen\nAvhandling"
  out <- page_sections(page_blocks(html, NA, cfg), text, cfg)
  expect_equal(out$section, "assessment")
  # a heading the html reader missed: the fallback's sections
  html <- "<main><p>Innhold</p><p>Tall.</p><p>Vurdering</p><p>Eksamen.</p></main>"
  out <- page_sections(page_blocks(html, NA, cfg), "Innhold\nTall.\n\nVurdering\nEksamen.", cfg)
  expect_equal(out$section, c("course_content", "assessment"))
})

test_that("usn: reading-list placeholder and the generic fitness notice are dropped", {
  out <- tibble::tibble(
    section = c("reading_list", "coursework_requirements", "assessment"),
    raw_text = c("The reading list is not published yet.",
                 paste0("Arbeidskrav 1: Praksis.\nSkikkethetsvurdering\nKunnskapsdepartementet fastsatte 30. juni 2006 Forskrift.\n",
                        "Kriterier ved vurdering av om en student ikke er skikket i utdanningen er at studenten:\n",
                        "a) viser manglende vilje\nh) viser manglende vilje til å endre adferd\n"),
                 "Mappevurdering."))
  res <- .clean_sections(out, "usn")
  expect_equal(res$section, c("coursework_requirements", "assessment"))
  expect_equal(trimws(res$raw_text[1]), "Arbeidskrav 1: Praksis.")
})

test_that("pointers to the admission rules and admission statements leave prerequisites", {
  clean <- function(txt) {
    res <- .clean_sections(tibble::tibble(section = "prerequisites", raw_text = txt))
    if (nrow(res) == 0) NA_character_ else res$raw_text
  }
  for (t in c("Studenter må oppfylle gjeldene opptakskrav ved tilhørende studieprogram.",   # nord
              "Studiets opptakskrav.", "Generelle opptakskrav.",                              # hvl, usn
              "Ingen. Sjå opptakskrav i studieplanen.",                                       # hivolda
              "View the admission requirements for the MA programme in Global Journalism.",   # nla
              "Admission to the MA Programme in Global Journalism",
              "Studenten må være tatt opp på studiet.",
              "Det kreves generell studiekompetanse for å tas opp på innføringsstudiet.\n\nIngen",
              # a line that opens with the admission rule or the applicants goes whole (oslomet)
              paste("Opptakskrav er bestått lærerutdanning eller tilsvarende (15 studiepoeng).",
                    "I tillegg kreves minst tre års yrkeserfaring.\nSøkere som mangler fag kan også søke.",
                    "Slike søkere må i tillegg oppfylle kravet til generell studiekompetanse."))) {
    expect_true(is.na(clean(t)), label = t)
  }
  expect_equal(clean("Studiets opptakskrav.\n\nØAO002 Markedsføring I"), "ØAO002 Markedsføring I")
  expect_equal(clean(paste("No formal requirements over and above the admission requirements.",
                           "Some knowledge of building physics is an advantage.")),
               "Some knowledge of building physics is an advantage.")
  # uia: study places, ranking and conditional admission go; the recommendation stays
  expect_equal(clean(paste("Utdanningsdirektoratet fordeler studieplassene, og du søker opptak via udir.no.",
                           "Søkere til studiet kan rangeres av oppdragsgiver.",
                           "Det gis ikke betinget opptak til studiet.", "",
                           "Det anbefales gode kunnskaper i engelsk.", sep = "\n")),
               "Det anbefales gode kunnskaper i engelsk.")
  # a requirement next to the admission pointer stays
  for (t in c("Studiets opptakskrav, med vekt på termodynamikk og grunnleggende kjemi",
              "Studenten må være tatt opp på masterstudiet i helsevitenskap og ha autorisasjon som fysioterapeut.",
              "Admission to the programme is based upon a completed bachelor's degree.")) {
    expect_equal(clean(t), t, label = t)
  }
})

test_that("parts that only say they are empty or elsewhere are not sections", {
  drop <- c("_", "Ingen info", "Ja", "Ja, vert spesifisert i semesterplan ved semesterstart",
            "Ja (spesifiseres i undervisningsplanen ved semesterstart).",
            "Sjå pensumliste for emnet her", "Litteratur og faglige ressurser finnes her.",
            "Follow this link for reading list and academic resources.",
            "Ta kontakt med instituttet/fakultetet", "Se hjemmeside.", "Available on request",
            "Ny pensumbok vurderes.", "Ingen arbeidskrav i emnet.",
            "Det er ingen arbeidskrav knyttet til emnet.", "Ingen arbeidskrav eller obligatorisk aktivitet.",
            "Ingen arbeidskrav/obligatorisk aktivitet.", "No coursework requirements or compulsory activities.")
  for (t in drop) expect_false(.keep_section_row(t, "reading_list"), label = t)
  expect_true(.keep_section_row("Ingen arbeidskrav, men 80 % oppmøte.", "coursework_requirements"))
  # uit: Leganto's notice and the list's title, with no list
  rl <- "Du kan se og få tilgang til deler av pensum via Leganto.\nPensumliste for LER-3101 Masteremne: norskdidaktikk"
  expect_equal(nrow(.clean_sections(tibble::tibble(section = "reading_list", raw_text = rl), "uit")), 0)
  # nla: an empty part after the section's own text goes, not in assessment
  out <- tibble::tibble(section = c("teaching_methods", "assessment", "coursework_requirements"),
                        raw_text = c("Forelesninger og seminarer.\n\nIngen.",
                                     "Muntlig eksamen.\n\nIngen.",
                                     "Obligatorisk deltakelse\n\nIngen"))
  expect_equal(.clean_sections(out)$raw_text,
               c("Forelesninger og seminarer.", "Muntlig eksamen.\n\nIngen.",
                 "Obligatorisk deltakelse\n\nIngen"))
})

test_that("a learning-outcome group label as a heading of its own stays (inn)", {
  html <- "<main><h2>Læringsutbytte</h2><p>Ved bestått emne:</p>
    <div class=\"label\">Kunnskap</div><p>Studenten</p><p>har kunnskap om regi</p>
    <div class=\"label\">Ferdigheter</div><p>Studenten</p><p>kan regissere</p>
    <div class=\"label\">Arbeids- og undervisningsformer</div><p>Seminarer.</p></main>"
  s <- sections_of(html, html_cfg(heading = "h2, div.label"))
  expect_equal(s[["learning_outcomes"]],
               "Ved bestått emne:\n\nKunnskap\nStudenten\nhar kunnskap om regi\n\nFerdigheter\nStudenten\nkan regissere")
  expect_equal(s[["teaching_methods"]], "Seminarer.")
})

test_that("resit rights, the fitness pointer, the form reservation and approval stamps leave the sections", {
  a <- paste0("Muntlig eksamen.\nStudentens retter og plikter ved ny/utsatt eksamen fremgår av forskrift ",
              "om studier og eksamen ved OsloMet. Studenter er selv ansvarlige for å melde seg opp til ",
              "eventuell ny/utsatt eksamen.\n\nSkikkethetsvurdering\nSe programplanen for bachelorstudium i ",
              "barnehagelærerutdanning (180 studiepoeng) heltid eller https://student.oslomet.no/skikkethetsvurdering\n\n",
              "Gradert skala A-F.")
  cc <- paste0("Vedtatt i avdelingsstyret. Godkjent av rektor. Siste revisjon godkjent av studieutvalget ved ",
               "fakultet LUI. Redaksjonell endring foretatt\nEmnet gir en innføring i tolking.")
  out <- .clean_sections(tibble::tibble(section = c("assessment", "course_content"), raw_text = c(a, cc)), "oslomet")
  expect_equal(out$raw_text, c("Muntlig eksamen.\n\nGradert skala A-F.", "Emnet gir en innføring i tolking."))
  n <- paste0("Skoleeksamen, 4 timer, karakterregel Bokstavkarakterer. Det tas forbehold om endringer i ",
              "vurderingsform. Korrekte vurderingsform vises i StudentWeb ved oppmelding til aktuelt emne.")
  expect_equal(.clean_sections(tibble::tibble(section = "assessment", raw_text = n), "nord")$raw_text,
               "Skoleeksamen, 4 timer, karakterregel Bokstavkarakterer.")
  st <- paste0("Godkjent av studieutvalget ved fakultet LUI. Etablert av fakultetsstyret 10. april 2014. ",
               "Revidert versjon godkjent av studieutvalget ved LUI. Pensum oppdatert. Gjeldende fra 1. august 2020.")
  expect_equal(.strip_sentences(paste0(st, "\nEmnet gir kunnskap."), .approval_sentence_regex), "Emnet gir kunnskap.")
  # an approval in coursework stays
  cw <- "Arbeidskravet må være godkjent av faglærer."
  expect_equal(.clean_sections(tibble::tibble(section = "coursework_requirements", raw_text = cw), "oslomet")$raw_text, cw)
})

test_that("the course evaluation leaves the teaching methods, uit's coursework item stays", {
  tm <- paste0("Studenten arbeider i bedriften.\n\nEmneevaluering – kvalitetssystem\n\n",
               "Til vanlig skal det gjennomføres evaluering av alle emner.")
  expect_equal(.clean_sections(tibble::tibble(section = "teaching_methods", raw_text = tm), "inn")$raw_text,
               "Studenten arbeider i bedriften.")
  cw <- "Emneevaluering\tKarakterregel: Godkjent – ikke godkjent\nGodkjent labjournal."
  expect_equal(.clean_sections(tibble::tibble(section = "coursework_requirements", raw_text = cw), "uit")$raw_text, cw)
})

test_that("a reading list that only says it comes later or elsewhere is not a section", {
  drop <- c("Endelig pensumliste legges ut på Blackboard før studiestart.",
            "Endelig pensumliste legges ut på Blackboard før studiestart. Den kan også søkes opp på Innsida.",
            "Endelig semesterplan og pensumliste legges ut på den digitale læringsplattformen før studiestart.",
            "Oppgis ved semesterstart", "Pensumliste oppgis ved semesterstart.", "Se pensumliste. Oppgis ved semesterstart")
  for (t in drop) expect_false(.keep_section_row(t, "reading_list"), label = t)
  keep <- c("Litteraturen er basert på matematikkdidaktiske forskningsartikler og oppgis ved semesterstart.",
            "Kursmaterialet er hovedsakelig vitenskapelige artikler skrevet på engelsk. Endelig pensumliste legges ut på Blackboard før studiestart.",
            "Oppgis ved semesterstart. Læreboka må leses.")
  for (t in keep) expect_true(.keep_section_row(t, "reading_list"), label = t)
})
