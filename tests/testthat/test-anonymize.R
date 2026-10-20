# Tests for anonymize_text() — R/anonymize.R

source(here::here("R/anonymize.R"))

# --- Core behavior ---

test_that("NA and empty input returns NA", {
  expect_equal(anonymize_text("ntnu", NA_character_, .progress = FALSE), NA_character_)
  expect_equal(anonymize_text("ntnu", "", .progress = FALSE), NA_character_)
})

test_that("vectorized input works", {
  result <- anonymize_text(
    c("ntnu", "hivolda"),
    c("Some course text about programming", NA_character_),
    .progress = FALSE
  )
  expect_length(result, 2)
  expect_type(result, "character")
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("mismatched lengths error", {
  expect_error(anonymize_text("ntnu", c("a", "b"), .progress = FALSE))
})

test_that("whitespace-only input returns NA", {
  expect_true(is.na(anonymize_text("ntnu", "   \n\n  ", .progress = FALSE)))
})

# --- Generic anonymization ---

test_that("email addresses are removed", {
  input <- "Kontakt oss på kari.nordmann@university.no for info"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("kari\\.nordmann@university\\.no", result))
  expect_true(grepl("Kontakt oss", result))
})

test_that("IP addresses in OpenURL links are removed, also when wrapped", {
  input <- paste0("rft.pub=Cappelen&svc_dat=viewit&user_ip=10.16.56.12\n5&req.skin=primo\n",
                  "view=UBIS&us\ner_ip=109.189.203.220&req.skin=primo\n",
                  "Kapittel 6.4.6.1 og doi=10.1.1.462.7279")
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("10\\.16\\.56|109\\.189|user_ip", result))
  expect_match(result, "svc_dat=viewit&&req.skin=primo", fixed = TRUE)
  expect_match(result, "Kapittel 6.4.6.1 og doi=10.1.1.462.7279", fixed = TRUE)
})

test_that("page furniture: OpenURL links, uis PDF header, usn Leganto labels", {
  uis <- anonymize_text("uis", paste0(
    "Bracke, Sarah, 2008-11, 51-67,\nhttps://bibsys-ur.userservices.exlibrisgroup.com/view?ctx=1&rft.a=\n",
    "b&req.skin=primoVi\new online\n\nBok\nNeste tittel\n",
    "Emne LENG116_1, BOKMÅL, 2015 HØST, versjon 01.02.2015\nfortsatt"), .progress = FALSE)
  expect_equal(uis, "Bracke, Sarah, 2008-11, 51-67,\n\nBok\nNeste tittel\nfortsatt")
  usn <- anonymize_text("usn", paste(
    "Click to view interactive reading list in LegantoObligatorisk litteratur",
    "BookMøter med barnelitteratur", "View online", "Booking av rom", sep = "\n"), .progress = FALSE)
  expect_equal(usn, "Obligatorisk litteratur\nMøter med barnelitteratur\nBooking av rom")
})

test_that("uis PDF footer and a last steiner page number are removed", {
  expect_equal(anonymize_text("uis", paste(
    "Kunnskap om litteratur", "   LENG116_1 - Innføring i britisk og irsk litteratur",
    "og kultur.", "Se LENG115 - Introduksjon", sep = "\n"), .progress = FALSE),
    "Kunnskap om litteratur\nog kultur.\nSe LENG115 - Introduksjon")
  expect_equal(anonymize_text("steiner", "Eksamen tas opp igjen.\n15", .progress = FALSE),
               "Eksamen tas opp igjen.")
})

test_that("phone numbers with +47 are removed", {
  input <- "Ring oss: +47 22 85 50 00 for hjelp"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("22 85 50 00", result))
})

test_that("phone numbers with Tlf prefix are removed", {
  input <- "Tlf: 22855000 eller telefon: 22 85 50 00"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("22855000", result))
  expect_false(grepl("22 85 50 00", result))
})

test_that("administrative years after keywords are removed", {
  result <- anonymize_text("hivolda", "Opprettet 2020 og oppdatert 2023", .progress = FALSE)
  expect_false(grepl("2020|2023", result))
  expect_true(grepl("Opprettet", result))
  expect_true(grepl("oppdatert", result))
})

test_that("years after various admin keywords are removed", {
  cases <- list(
    c("Revidert 2022", "Revidert"),
    c("Vedtatt 2021", "Vedtatt"),
    c("Godkjent 2023", "Godkjent"),
    c("Gjeldende fra 2024", "Gjeldende fra"),
    c("Gyldig fra 2023", "Gyldig fra"),
    c("Sist endret 2022", "Sist endret"),
    c("Sist revidert 2021", "Sist revidert")
  )
  for (case in cases) {
    result <- anonymize_text("hivolda", case[1], .progress = FALSE)
    expect_false(grepl("\\d{4}", result), info = paste("Year not removed from:", case[1]))
    expect_true(grepl(case[2], result, ignore.case = TRUE),
                info = paste("Keyword lost from:", case[1]))
  }
})

test_that("academic year ranges are removed", {
  result <- anonymize_text("hivolda", "Pensum for 2023/2024 og 2024/25", .progress = FALSE)
  expect_false(grepl("2023/2024", result))
  expect_false(grepl("2024/25", result))
})

test_that("content years are preserved", {
  cases <- c(
    "Studenten skal kunne de viktigste utviklingene i Europa etter 1945",
    "Finanskrisen i 2008 er sentral",
    "Litteratur fra 2000-tallet",
    "Se NOU 2015:2 for bakgrunn",
    "Jf. LOV-2005-04-01-15",
    "Oppgaven skal være på 2000 ord"
  )
  for (input in cases) {
    result <- anonymize_text("hivolda", input, .progress = FALSE)
    expect_true(grepl("(19|20)\\d{2}", result), info = paste("Content year removed from:", input))
  }
})

test_that("dates dd.mm.yyyy are removed", {
  result <- anonymize_text("hivolda", "Eksamen 15.12.2023 kl 09:00", .progress = FALSE)
  expect_false(grepl("15\\.12\\.2023", result))
})

test_that("links, DOIs, citations and content ranges keep their numbers", {
  keep <- c("https://www.utdanningsnytt.no/files/2019/06/27/Bedre.pdf",
            "https://www.journals.uio.no/index.php/adno/article/view/1126/1005",
            "Piaget (1980/2005) og Vygotsky (1934/1986).",
            "Svartedauden 1349-50 og krigen 1944-45.",
            "Forskrift 28.10.2020 nr. 2474 om rammeplan.")
  for (x in keep) expect_equal(anonymize_text("uit", x, .progress = FALSE), x, info = x)
  expect_equal(anonymize_text("uit", "Studieåret 2023/24, gyldig 01.08.2023.", .progress = FALSE),
               "Studieåret, gyldig.")
})

test_that("Norwegian date-time format is removed", {
  result <- anonymize_text("hivolda", "Hentet 12. feb. 2026 02:50:04", .progress = FALSE)
  expect_false(grepl("feb", result))
  expect_false(grepl("02:50:04", result))
})

test_that("times HH:MM are removed", {
  result <- anonymize_text("hivolda", "Forelesning 08:15 til 10:00", .progress = FALSE)
  expect_false(grepl("08:15|10:00", result))
})

test_that("Sist hentet fra FS timestamp is removed", {
  input <- "Kursinnhold\nSist hentet fra FS (Felles studentsystem) 9. feb. 2026 11:30:35"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("Sist hentet", result))
  expect_true(grepl("Kursinnhold", result))
})

test_that("Sist henta frå FS (nynorsk) is removed", {
  input <- "Kursinnhold\nSist henta frå FS (Felles studentsystem) 9. feb. 2026 11:30:35"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("Sist henta", result))
})

test_that("season words with year are removed", {
  result <- anonymize_text("hivolda", "Oppstart Høst 2024 og Vår 2025", .progress = FALSE)
  expect_false(grepl("(?i)\\bhøst\\b", result, perl = TRUE))
  expect_false(grepl("(?i)\\bvår\\b", result, perl = TRUE))
})

test_that("a season after a semester label stays in course_plan", {
  # a fact of the course, in every language (course_plan_spec.md)
  keep <- c("Undervisningssemester: Vår\nEksamen: Høst", "Semester of instruction: Autumn",
            "Undervisningssemester:\nOppstart høst, undervisning over to semestere",
            "Teaching semester:\nStart in the autumn, teaching over to semesters")
  for (x in keep) expect_equal(anonymize_text("hivolda", x, .progress = FALSE), x)
})

test_that("the plan id and the sections leave a season after a semester label out", {
  an <- function(x) .drop_semester_values(anonymize_text("uib", x, .progress = FALSE))
  expect_equal(an("Undervisningssemester: Vår\nEksamen: Høst"), "Undervisningssemester:\nEksamen:")
  expect_equal(an("Undervisningssemester\nVår og haust\nInnhald"), "Undervisningssemester\n\nInnhald")
  expect_equal(an("Vurderingssemester\nHaustsemesteret, 6. semester i lektorutdanninga"),
               "Vurderingssemester\n6. semester i lektorutdanninga")
  expect_equal(an("Undervisningssemester\nHaustEmnet går første gong"), "Undervisningssemester\nEmnet går første gong")
  expect_equal(an("Gjeldende fra høstsemesteret 2021."), "Gjeldende fra.")
  # the start of another word stays
  expect_equal(an("Eksamen\nHøstpraksis: ny praksis. Semester\nVåre dokumenter"),
               "Eksamen\nHøstpraksis: ny praksis. Semester\nVåre dokumenter")
  expect_equal(.drop_semester_values(c("Undervisningssemester: Haust", NA, "Vår")),
               c("Undervisningssemester:", NA, "Vår"))
})

test_that("an academic year that sets a condition stays", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  keep <- c("Semester of education: Autumn. Exception for academic year 2020-2021: Spring.",
            "IKF132 Innføring i kvalitative forskningsmetoder, gjennomført studieåret 2019-2020.",
            "15 studiepoeng mot P301, når eksamen i P301 er avlagt studieåret 2013-2014 eller senere.",
            "studenter som ikke fullførte emnet i løpet av studieåret 2013/2014.")
  for (x in keep) expect_equal(an("nla", x), x)
  # an administrative academic year still goes
  expect_equal(an("nla", "Studieår: 2019 — 2020\nEmneplan for studieåret 2024/2025."),
               "Studieår:\nEmneplan for studieåret.")
})

test_that("vår meaning 'our' is preserved in prose", {
  result <- anonymize_text("hivolda", "Vår tilnærming til pedagogikk er viktig", .progress = FALSE)
  expect_true(grepl("tilnærming", result))
  expect_true(grepl("(?i)\\bvår\\b", result, perl = TRUE))
})

test_that("høst in prose context is preserved", {
  result <- anonymize_text("hivolda", "I løpet av høst og vinter jobber studentene", .progress = FALSE)
  expect_true(grepl("(?i)\\bhøst\\b", result, perl = TRUE))
})

test_that("2-digit season+year removed", {
  result <- anonymize_text("hivolda", "Oppstart Høst23 og Vår 22", .progress = FALSE)
  expect_false(grepl("Høst23|Vår 22", result))
})

test_that("JS artifacts are removed", {
  input <- 'Info function(x) { return y; } mer tekst'
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("function", result))
  expect_true(grepl("Info", result))
})

test_that("3+ newlines collapsed to 2", {
  input <- "Avsnitt 1\n\n\n\n\nAvsnitt 2"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("\n{3,}", result))
  expect_true(grepl("Avsnitt 1\n\nAvsnitt 2", result))
})

# --- NTNU ---

test_that("NTNU: contacts go; the unit and the exam tab stay", {
  an <- function(x) anonymize_text("ntnu", x, .progress = FALSE)
  input <- paste("Viktig tekst", "Kontaktinformasjon", "Emneansvarlig/koordinator", "Kari Nordmann",
                 "Faglærere", "Ola Nordmann", "Ansvarlig enhet", "", "Institutt for lærerutdanning", sep = "\n")
  expect_equal(an(input), "Viktig tekst\nAnsvarlig enhet\n\nInstitutt for lærerutdanning")
  # without a unit the contact block ends at the exam tab
  expect_equal(an("Tekst\nKontaktinformasjon\nFaglærere\nOla Nordmann\nEksamen\nVurderingsordning: Oppgave"),
               "Tekst\nEksamen\nVurderingsordning: Oppgave")
  expect_equal(an("Tekst\nKontaktinformasjon\nFaglærere\nOla Nordmann"), "Tekst")
  # the English page (`english_page`)
  expect_equal(an(paste("Duration 1 semester", "Course start Spring 2023", "Text", "Contact information",
                        "Course coordinator", "Kari Nordmann", "Department with academic responsibility",
                        "", "Department of Computer Science", "Examination", sep = "\n")),
               paste("Duration 1 semester", "", "Text", "Department with academic responsibility", "",
                     "Department of Computer Science", "Examination", sep = "\n"))
  # not a line that only starts so
  expect_equal(an("The course starts in week 2."), "The course starts in week 2.")
})

test_that("NTNU: course-details-portlet removed", {
  input <- "course-details-portlet ENG3901 Masteroppgave"
  result <- anonymize_text("ntnu", input, .progress = FALSE)
  expect_false(grepl("course-details-portlet", result))
  expect_true(grepl("Masteroppgave", result))
})

test_that("NTNU: Undervisningsstart line removed", {
  input <- "Nivå Master\nUndervisningsstart Høst 2024 / Vår 2025\nVarighet 2 semestre"
  result <- anonymize_text("ntnu", input, .progress = FALSE)
  expect_false(grepl("Undervisningsstart", result))
  expect_true(grepl("Varighet", result))
})

test_that("NTNU: Blackboard links removed", {
  input <- "Andre sider om emnet Blackboard - VÅR-2024 Fagområder"
  result <- anonymize_text("ntnu", input, .progress = FALSE)
  expect_false(grepl("Blackboard", result))
})

# --- UiT ---

test_that("UiT, UiS: HTML written out as text loses its tags, not its links", {
  expect_equal(anonymize_text("uit", paste0("<p>Kapittel 3</p>\n<div class=\"x\">Kap. 9 &amp; 10<br/>s. 1-25</div>\n",
                                            "Se <http://www.sami.uit.no/girji/n02/no/111dunfl.html>"), .progress = FALSE),
               "Kapittel 3\n\nKap. 9 & 10\ns. 1-25\n\nSe <http://www.sami.uit.no/girji/n02/no/111dunfl.html>")
  expect_equal(anonymize_text("uis", "Bok&nbsp;A p&aring; norsk", .progress = FALSE), "Bok A på norsk")
  expect_equal(anonymize_text("nmbu", "Kategori 2.\nHer finner du KI-kategoriene beskrevet.\nNeste", .progress = FALSE),
               "Kategori 2.\nNeste")
})

test_that("UiT: an exam part's date goes with its label", {
  an <- function(x) anonymize_text("uit", x, .progress = FALSE)
  expect_equal(an("Oppgave\tInnlevering: 15.05.2023 14:00 Vekting: 0/1\tKarakterregel: A–E, stryk F"),
               "Oppgave\tVekting: 0/1\tKarakterregel: A–E, stryk F")
  expect_equal(an("Oppgave\tInnlevering: 15.05.2023 14:00\tKarakterregel: A–E, stryk F"),
               "Oppgave\t\tKarakterregel: A–E, stryk F")
  expect_equal(an("Hjemmeeksamen\tInnlevering: 01.12.2023 12:00 Utlevering: 24.11.2023 09:00 Varighet: 1 Uker"),
               "Hjemmeeksamen\tVarighet: 1 Uker")
  expect_equal(an("Skoleeksamen\tDato: 02.12.2024 –06.12.2024 Varighet: 4 Timer"),
               "Skoleeksamen\tVarighet: 4 Timer")
})

test_that("UiT: Error rendering component removed", {
  input <- "Læringsutbytte Error rendering component Innhold"
  result <- anonymize_text("uit", input, .progress = FALSE)
  expect_false(grepl("Error rendering", result))
})

test_that("UiT: Kontaktperson + name line stripped", {
  input <- "Innhold\nKontaktperson:\nOla Nordmann\nPensum"
  result <- anonymize_text("uit", input, .progress = FALSE)
  expect_false(grepl("Ola Nordmann", result))
  expect_true(grepl("Pensum", result))
})

test_that("UiT: Foreleser + name line stripped", {
  input <- "Innhold\nForeleser:\nKari Hansen\nPensum"
  result <- anonymize_text("uit", input, .progress = FALSE)
  expect_false(grepl("Kari Hansen", result))
  expect_true(grepl("Pensum", result))
})

# --- UiA ---

test_that("UiA: breadcrumb removed without eating content", {
  input <- "Forside > Studier > Emner > 2025 > Høst 2025 > EN-148 Fagdidaktikk (Høst 2025) Studiepoeng: 7.5"
  result <- anonymize_text("uia", input, .progress = FALSE)
  expect_false(grepl("Forside", result))
  expect_true(grepl("Studiepoeng", result))
})

test_that("UiA: (Høst/Haust YYYY) removed from title", {
  input <- "EN-148 Fagdidaktikk (Høst 2025) Studiepoeng"
  result <- anonymize_text("uia", input, .progress = FALSE)
  expect_false(grepl("\\(Høst", result))
})

test_that("UiA: Emneansvarlig + name on next line stripped", {
  input <- "Innhold\nEmneansvarlig:\nKari Nordmann\nUndervisningssemester:\nHøst"
  result <- anonymize_text("uia", input, .progress = FALSE)
  expect_false(grepl("Kari Nordmann", result))
})

test_that("UiA: Emneansvarlige + several name lines stripped", {
  input <- paste0("Ansvarlig avdeling:\nFakultet for humaniora og pedagogikk\nEmneansvarlige:\n",
                  "Kari Nordmann\nOla Smith-Hansen\nUndervisningssemester:\n\nUndervisningsspråk:\nNorsk")
  result <- anonymize_text("uia", input, .progress = FALSE)
  expect_false(grepl("Nordmann|Smith-Hansen|Emneansvarlige", result))
  expect_match(result, "pedagogikk\nUndervisningssemester:")
  # the role named in a sentence is not a label
  s <- "Emneansvarlig fastsetter evalueringsform.\nTilbys som enkeltemne"
  expect_equal(anonymize_text("uia", s, .progress = FALSE), s)
})

test_that("USN: 'Godkjent av' with the approver's name and title stripped", {
  input <- "Alle hjelpemidler tillatt.\n\nGodkjent av\nKari Nordmann\nInstituttnestleder\n\nLitteratur"
  result <- anonymize_text("usn", input, .progress = FALSE)
  expect_false(grepl("Nordmann|Godkjent av|Instituttnestleder", result))
  expect_match(result, "Litteratur")
})

# --- HiOF ---

test_that("HiOF: Sist hentet fra FS removed", {
  input <- "Kursinnhold\nSist hentet fra FS (Felles studentsystem) 12. feb. 2026 02:50:04"
  result <- anonymize_text("hiof", input, .progress = FALSE)
  expect_false(grepl("Sist hentet fra FS", result))
})

test_that("HiOF: Litteraturlista sist oppdatert removed", {
  input <- "Pensum\nLitteraturlista er sist oppdatert 16.08.2016\nAnnet"
  result <- anonymize_text("hiof", input, .progress = FALSE)
  expect_false(grepl("Litteraturlista er sist oppdatert", result))
})

test_that("HiOF: multiline Emneansvarlige block stripped", {
  input <- "Studiested:\nHalden\nEmneansvarlige:\nRagnhild Louise Næsje\nChristian Bjørn Bjerke\nUndervisningsspråk:\nNorsk"
  result <- anonymize_text("hiof", input, .progress = FALSE)
  expect_false(grepl("Ragnhild|Næsje|Bjerke", result))
  expect_true(grepl("Undervisningsspråk", result))
})

# --- HIVOLDA ---

test_that("HIVOLDA: Emneansvarleg + name on next line stripped", {
  input <- "Innhold\nEmneansvarleg:\nIngeborg Katrin Lid Berget\nPensum"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("Ingeborg|Berget", result))
  expect_true(grepl("Pensum", result))
})

test_that("HIVOLDA: Godkjent av + name on next line stripped", {
  input <- "Innhold\nGodkjent av:\nSilje Ims Lied"
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("Silje|Lied", result))
  expect_true(grepl("Innhold", result))
})

# --- INN ---

test_that("INN: placeholder pages return NA", {
  input <- "Emnesøket gjelder kun fra 2022. Noe annet"
  result <- anonymize_text("inn", input, .progress = FALSE)
  expect_true(is.na(result))
  # the notice that the page shows the latest version goes; the version's
  # start semester is `page_term`
  latest <- paste0("Statusmelding\nEmnebeskrivelsen for valgt semester er ikke publisert enda. ",
                   "Her er den siste versjonen vi har tilgjengelig.\nEmnets innhold\nTekst")
  expect_equal(anonymize_text("inn", latest, .progress = FALSE), "Emnets innhold\nTekst")
  # and so does the banner over a plan in English only, with or without the language
  english <- c("Statusmelding\nKun tilgjengelig på engelsk\nCourse content\nText",
               "Statusmelding\nKun tilgjengelig på\nCourse content\nText")
  for (x in english) expect_equal(anonymize_text("inn", x, .progress = FALSE), "Course content\nText")
})

test_that("INN: structural headings preserved for readability", {
  input <- "Emnekode\nABC123\nStudiepoeng\n15\nEmnets innhold\nViktig faglig innhold"
  result <- anonymize_text("inn", input, .progress = FALSE)
  expect_true(grepl("Emnekode", result))
  expect_true(grepl("Studiepoeng", result))
  expect_true(grepl("Emnets innhold", result))
  expect_true(grepl("Viktig faglig innhold", result))
})

test_that("INN: English structural headings preserved for readability", {
  input <- "Course code\nABC123\nNumber of credits\n15\nCourse content\nImportant content"
  result <- anonymize_text("inn", input, .progress = FALSE)
  expect_true(grepl("Course code", result))
  expect_true(grepl("Number of credits", result))
  expect_true(grepl("Course content", result))
  expect_true(grepl("Important content", result))
})

# --- OsloMet ---

test_that("OsloMet: error pages return NA", {
  input <- "Siden du leter etter finnes ikke, eller det har oppstått en feil."
  result <- anonymize_text("oslomet", input, .progress = FALSE)
  expect_true(is.na(result))
})

test_that("OsloMet: runs of semicolons are rendering artifacts", {
  expect_equal(anonymize_text("oslomet", "Klasseledelse -;;;; Klasseledelse og fag\n1);;; Kropp; og læring",
                              .progress = FALSE),
               "Klasseledelse - Klasseledelse og fag\n1) Kropp; og læring")
})

test_that("OsloMet: the Emneansvarlig block goes whole, up to Emneoverlapp", {
  an <- function(x) anonymize_text("oslomet", x, .progress = FALSE)
  overlap <- "\n\nEmneoverlapp\n\nIngen kjente emner gir full overlapp."
  for (block in c("Kari Nordmann", "Professor Kari Nordmann", "x",
                  "Kari Nordmann og Ola Dahl-Nordli.",
                  "Delemne Praksisstudium: Kari Nordmann\n\nDelemne Sosialt arbeid og helse: Ola Dahl",
                  paste0("Master thesis coordinator: assistant professor Kari Nordmann\n\n",
                         "Writing workshop coordinator: professor Ola Dahl"))) {
    input <- paste0("Sensorordning\n\nTo interne sensorer.\n\nEmneansvarlig\n\n", block)
    expect_equal(an(input), "Sensorordning\n\nTo interne sensorer.", label = block)
    expect_equal(an(paste0(input, overlap)),
                 paste0("Sensorordning\n\nTo interne sensorer.", overlap), label = block)
  }
  # the coordinators named in a sentence: the names and their roles go
  expect_equal(an("Emnet er for sosionomer.\n\nEmneansvarlige er: Kari Nordmann (sosionom) og Ola Dahl (pedagog).\n\nForkunnskapskrav"),
               "Emnet er for sosionomer.\n\nEmneansvarlige er:.\n\nForkunnskapskrav")
})

test_that("OsloMet: bare semicolons (2022 artifacts) are removed", {
  input <- "Innhold\n;\nMer tekst\n ; \nSlutt"
  result <- anonymize_text("oslomet", input, .progress = FALSE)
  expect_false(grepl("(?m)^\\s*;\\s*$", result, perl = TRUE))
  expect_true(grepl("Innhold", result))
  expect_true(grepl("Slutt", result))
})

test_that("OsloMet: content semicolons are preserved", {
  input <- "Tema1; Tema2 og ulike tilnærminger; Tema3"
  result <- anonymize_text("oslomet", input, .progress = FALSE)
  expect_true(grepl(";", result))
  expect_true(grepl("Tema1; Tema2", result))
})

# --- MF ---

test_that("MF: Emneansvarlig section stripped to end", {
  input <- "Kursinnhold viktig\nEmneansvarlig\nOla Nordmann\nola@mf.no\nStudentlivet"
  result <- anonymize_text("mf", input, .progress = FALSE)
  expect_true(grepl("viktig", result))
  expect_false(grepl("Ola Nordmann", result))
  expect_false(grepl("Studentlivet", result))
})

# --- UiS ---

test_that("UiS: Kontakt section stripped to end (HTML)", {
  input <- "Innhold\nKontakt\nEmneansvarlig:\nBerit Aarrestad\nFaglærer:\nSara Sedberg"
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("Kontakt|Aarrestad|Sedberg", result))
  expect_true(grepl("Innhold", result))
})

test_that("UiS: EMNE Versjon header stripped (PDF)", {
  input <- "EMNE LFYBAC_1 BOKMÅL Versjon 14.Jun.2019 15:15\nInnhold"
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("EMNE LFYBAC", result))
  expect_true(grepl("Innhold", result))
})

test_that("UiS: Fagpersoner section stripped (PDF)", {
  input <- "Innhold\nFagpersoner\n-Diana Lucia Quintero Castro (Faglærer)\n\nAnnet"
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("Diana|Quintero", result))
})

test_that("UiS: Powered by TCPDF stripped", {
  input <- "Innhold\nPowered by TCPDF (www.tcpdf.org)"
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("TCPDF", result))
})

test_that("UiS: Emnebeskrivelsen er hentet fra stripped", {
  input <- "Innhold\nEmnebeskrivelsen er hentet fra Felles studentsystem Versjon 1"
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_false(grepl("Emnebeskrivelsen er hentet", result))
})

# --- Steiner ---

# --- USN ---

test_that("USN: Godkjent emneplan heading stripped", {
  input <- "Innhold\nGodkjent emneplan\nLitteratur"
  result <- anonymize_text("usn", input, .progress = FALSE)
  expect_false(grepl("Godkjent emneplan", result))
})

test_that("USN: Godkjent date line stripped", {
  input <- "Innhold\nGodkjent 22.11.2022.\nLitteratur"
  result <- anonymize_text("usn", input, .progress = FALSE)
  expect_false(grepl("Godkjent", result))
})

# --- UiB ---

test_that("UiB: Studierettleiar line stripped", {
  input <- "Innhold\nStudierettleiar: info@uib.no\nPensum"
  result <- anonymize_text("uib", input, .progress = FALSE)
  expect_false(grepl("Studierettleiar", result))
  expect_true(grepl("Pensum", result))
})

test_that("UiB: the date the reading list is published goes with its sentence", {
  an <- function(x) anonymize_text("uib", x, .progress = FALSE)
  expect_equal(an("Pensum er henta. Litteraturlista vil vere klar innan 01.06. for haustsemesteret og 01.12. for vårsemesteret."),
               "Pensum er henta.")
  expect_equal(an("Litteraturlista vil vere klar innan 01.12. Pensum utgjer om lag 750 sider."),
               "Pensum utgjer om lag 750 sider.")
  expect_true(is.na(an("Litteraturlistene vil vere klare innan 1. juni for haustsemesteret og 1. desember for vårsemesteret.")))
})

test_that("UiB: Kontakt line stripped", {
  input <- "Innhold\nKontakt: studieinfo@uib.no\nPensum"
  result <- anonymize_text("uib", input, .progress = FALSE)
  expect_false(grepl("Kontakt", result))
})

test_that("UiB: Eksamensadministrasjon line stripped", {
  input <- "Innhold\nEksamensadministrasjon: eksamen@uib.no\nPensum"
  result <- anonymize_text("uib", input, .progress = FALSE)
  expect_false(grepl("Eksamensadministrasjon", result))
})

# --- UiO ---

test_that("UiO: person name before parenthesized email stripped", {
  input <- "Ta kontakt med Per Hansen (per.hansen@math.uio.no) for info"
  result <- anonymize_text("uio", input, .progress = FALSE)
  expect_false(grepl("Per Hansen", result))
  expect_true(grepl("Ta kontakt med", result))
})

test_that("UiO: multi-word person name before email stripped", {
  input <- "kontakt med Ola Kristian Nordmann (o.k.nordmann@geo.uio.no) eller studieadmin"
  result <- anonymize_text("uio", input, .progress = FALSE)
  expect_false(grepl("Ola Kristian Nordmann", result))
  expect_true(grepl("studieadmin", result))
})

test_that("UiO: organizational name before email preserved", {
  input <- "studieadministrasjonen ved Matematisk institutt (studieinfo@math.uio.no) for info"
  result <- anonymize_text("uio", input, .progress = FALSE)
  expect_true(grepl("Matematisk institutt", result))
})

# --- NMBU ---

test_that("NMBU: Emneansvarlig:Name stripped", {
  input <- "Innhold\nEmneansvarlig: Kari Nordmann\nPensum"
  result <- anonymize_text("nmbu", input, .progress = FALSE)
  expect_false(grepl("Kari Nordmann", result))
  expect_true(grepl("Pensum", result))
})

# --- Unknown institution ---

test_that("unknown institution applies only generic anonymization", {
  input <- "Kurs med info@test.no oppdatert 2023"
  result <- anonymize_text("unknown_inst", input, .progress = FALSE)
  expect_false(grepl("info@test\\.no", result))
  expect_false(grepl("2023", result))
  expect_true(grepl("Kurs med", result))
})

# --- Smoke tests: no PII leaks ---

test_that("smoke test: no email-shaped strings in output", {
  inputs <- c(
    "Kontakt ola.nordmann@ntnu.no for info",
    "Send til student@hivolda.no eller veileder@uia.no",
    "Email: test.person@usn.no, backup: admin@example.com"
  )
  institutions <- c("ntnu", "hivolda", "uia")

  for (i in seq_along(inputs)) {
    result <- anonymize_text(institutions[i], inputs[i], .progress = FALSE)
    expect_false(
      grepl("[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}", result, perl = TRUE),
      info = paste("Email leaked for", institutions[i])
    )
  }
})

test_that("smoke test: no phone numbers in output", {
  inputs <- c(
    "Ring +47 22 85 50 00",
    "Tlf: 73595000",
    "telefon: 55 58 00 00"
  )
  institutions <- c("ntnu", "hivolda", "uib")

  for (i in seq_along(inputs)) {
    result <- anonymize_text(institutions[i], inputs[i], .progress = FALSE)
    expect_false(
      grepl("\\d{8}", result),
      info = paste("Phone number leaked for", institutions[i])
    )
  }
})

test_that("smoke test: no dates in output", {
  input <- "Eksamen 15.12.2023 kl 09:00. Sist oppdatert 01.01.2024."
  result <- anonymize_text("hivolda", input, .progress = FALSE)
  expect_false(grepl("\\d{1,2}\\.\\d{1,2}\\.\\d{4}", result))
  expect_false(grepl("\\d{1,2}:\\d{2}", result))
})

# --- Staff name lists ---

test_that("uis Fagperson(er) staff list is removed, surrounding text kept", {
  input <- paste(
    "adgang til prøving nektes.",
    "Fagperson(er)",
    "- Per Olav Hansen Berg (Faglærer)",
    "- Kari Elisabeth Nordmann Lie (Studiekoordinator)",
    "- Åse Ødegård (Emneansvarlig)",
    "Arbeidsformer",
    "Forelesninger og seminar.",
    sep = "\n"
  )
  result <- anonymize_text("uis", input, .progress = FALSE)
  expect_equal(result, "adgang til prøving nektes.\nArbeidsformer\nForelesninger og seminar.")
})

test_that("Name (Role) lines are removed at any institution", {
  result <- anonymize_text("hvl", "Innhald\nKari Anne van der Nordmann (Faglærar)\nTekst.",
                           .progress = FALSE)
  expect_equal(result, "Innhald\nTekst.")
})

test_that("a role label followed by a name is removed", {
  expect_equal(anonymize_text("uit", "Pensum\nEmneleder Kari Nordmann kari.nordmann@uit.no\nFire fagbøker.",
                              .progress = FALSE), "Pensum\nFire fagbøker.")
  expect_equal(anonymize_text("uis", "Emneansvarlig Ola Nordmann\nInnhold", .progress = FALSE),
               "Innhold")
  expect_equal(anonymize_text("uib", "Emneansvarlig\nThe Programme Committee", .progress = FALSE),
               "Emneansvarlig\nThe Programme Committee")
})

test_that("parenthesised non-roles and prose are kept", {
  for (t in c("Vekting (SP): 15", "Reduksjon (SP)",
              "Studentene leser Ibsen og Hamsun (various episodes)",
              "Emneansvarlig informerer om detaljer knyttet til eksamen.")) {
    expect_equal(anonymize_text("uis", t, .progress = FALSE), t, label = t)
  }
})

test_that("approver name is removed but the role is kept", {
  result <- anonymize_text("usn", "Godkjent av dekan Ola Nordmann", .progress = FALSE)
  expect_equal(result, "Godkjent av dekan")
  expect_equal(anonymize_text("usn", "Godkjent av Dekan", .progress = FALSE),
               "Godkjent av Dekan")
})

# --- Signature lines ---

test_that("signature lines 'Name, dekan' are removed with their date stamp", {
  for (line in c("17.03.20 Ola Kristian Nordmann, dekan",
                 "19.11 2020 Ola Nordmann, dekan",
                 "21.03.2021\u00a0Ola Nordmann, dekan",
                 "Kari Nordmann Lie, prodekan",
                 "Per Hansen, instituttleder ")) {
    input <- paste("Vurdering skjer i samråd med student.", line, "", "Endringer", sep = "\n")
    result <- anonymize_text("usn", input, .progress = FALSE)
    expect_equal(result, "Vurdering skjer i samråd med student.\n\nEndringer", label = line)
  }
})

test_that("prose and references with a role after a comma are kept", {
  for (t in c("eit felles ansvarsområde for praksislærar, rektor og faglærar",
              "Nordmann, O. (2024). Rektor som rettsanvender. Oslo: Universitetsforlaget.",
              "Ordningen er godkjent av fakultetet, dekan og prodekan.")) {
    expect_equal(anonymize_text("usn", t, .progress = FALSE), t, label = t)
  }
})

test_that("steiner: bare PDF page numbers are removed, numbers in text kept", {
  input <- "kan gi tilbakemelding\n10\nkan anvende kunnskaper\nOmfang: 10 studiepoeng"
  expect_equal(anonymize_text("steiner", input, .progress = FALSE),
               "kan gi tilbakemelding\nkan anvende kunnskaper\nOmfang: 10 studiepoeng")
})

# --- Seasons, academic years, brackets, mf and uio tails, phone numbers ---

test_that("inflected and run-together season + year are removed", {
  expect_equal(anonymize_text("nih", "Pensumliste for høsten 2025 og våren2026.", .progress = FALSE),
               "Pensumliste for.")
})

test_that("academic-year ranges go, content ranges stay", {
  expect_equal(anonymize_text("nih", "Se Studiehåndbok 2023-2024 og 2023- 24.", .progress = FALSE),
               "Se Studiehåndbok og.")
  expect_equal(anonymize_text("nih", "Perioden 1945-1970 og 1914-18.", .progress = FALSE),
               "Perioden 1945-1970 og 1914-18.")
})

test_that("brackets emptied by year removal go, code brackets stay", {
  expect_equal(anonymize_text("hiof", "Studiehåndbok (2016-2017) og print() i Python.", .progress = FALSE),
               "Studiehåndbok og print() i Python.")
})

test_that("a bracketed e-mail leaves no empty brackets", {
  expect_equal(anonymize_text("uio", "Ta kontakt med Per Hansen (per.hansen@uio.no) eller oss.", .progress = FALSE),
               "Ta kontakt med eller oss.")
})

test_that("mf exam-dates block and library notice are removed", {
  input <- paste("Skriftlig eksamen.", "", "Eksamensdatoer", "Eksamensdato:", "18. november 2026",
                 "", "Læringsutbytte", "Kunnskap", "", "Litteraturliste", "Litteraturlisten for vår 2026", "",
                 "Tilgang til litteratur", "Noe er digitalt.", "via ditt lokale folkebibliotek.", sep = "\n")
  expect_equal(anonymize_text("mf", input, .progress = FALSE),
               "Skriftlig eksamen.\n\nLæringsutbytte\nKunnskap\n\nLitteraturliste")
})

test_that("uio exam-links tail is cut, up to the facts box", {
  expect_equal(anonymize_text("uio", "Skriftlig eksamen.\nMer om eksamen ved UiO\nHvordan bruke KI som student", .progress = FALSE),
               "Skriftlig eksamen.")
  expect_equal(anonymize_text("uio", paste0("Skriftlig eksamen.\nMer om eksamen ved UiO\nTrekk fra eksamen\n\n",
                                            "Fakta om emnet\nNivå\nMaster"), .progress = FALSE),
               "Skriftlig eksamen.\n\nFakta om emnet\nNivå\nMaster")
})

test_that("phone numbers after 'Tlf.' are removed", {
  expect_equal(anonymize_text("uib", "Studieadministrasjonen. Tlf. 55 58 27 10", .progress = FALSE),
               "Studieadministrasjonen.")
})

# --- The shareable files (README "Data Files: Published and Internal") ---

test_that("the personal-data check finds what the anonymizer removes", {
  f <- tempfile(fileext = ".RDS")
  saveRDS(tibble::tibble(id = 1:4, text = c(
    "Kontakt kari.nordmann@uit.no", "- Ola Nordmann (Emneansvarlig)\nInnhold", "Ingen",
    "Emneansvarlige:\nKari Nordmann\nUndervisningssemester:")), f)
  found <- personal_data_in(f)
  expect_setequal(found$pattern, c("email", "staff", "role_name", "role_heading_name"))
  saveRDS(tibble::tibble(text = anonymize_text(c("uit", "uit"), c(
    "Kontakt kari.nordmann@uit.no", "- Ola Nordmann (Emneansvarlig)\nInnhold"),
    .progress = FALSE)), f)
  expect_equal(nrow(personal_data_in(f)), 0)
})

test_that("a role and names inside a line: the names go, the role stays", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  expect_equal(an("usn", "Bøkene må avklares med faglærer Kari Nordmann."),
               "Bøkene må avklares med faglærer.")
  expect_equal(an("usn", "Godkjent emneplan av dekan Ola Nordmann"), "Godkjent emneplan av dekan")
  expect_equal(an("uis", "Studiepoeng 15 Emneansvarlig Ola Nordmann\nInnhold"),
               "Studiepoeng 15 Emneansvarlig\nInnhold")
  expect_equal(an("uio", "Ta kontakt med Kari K. Nordmann eller Ola Berg Dahl for mer informasjon."),
               "Ta kontakt med for mer informasjon.")
  expect_equal(an("usn", "Spørsmål til emneansvarlig: Kari Nordmann-Dahl"), "Spørsmål til emneansvarlig:")
  # a role in brackets after a name goes with it, an affiliation stays
  expect_equal(an("usn", "Endringer ønsket av faglærer Kari Nordmann (UiA), meddelt via epost."),
               "Endringer ønsket av faglærer (UiA), meddelt via epost.")
  # a role before a lower-case word or one capitalised word is prose
  expect_equal(an("usn", "Faglærer Kari gir veiledning. Veileder Universitetet i Oslo."),
               "Faglærer Kari gir veiledning. Veileder Universitetet i Oslo.")
  # uit 2008-2011 timetable block; hvl campus contact lines
  expect_equal(an("uit", "Pensum\nBok.\nUndervisning Høst 2010\tForeleserKari Nordmann\nGruppe 1\tOla Dahl"),
               "Pensum\nBok.")
  expect_equal(an("hvl", "Hjelpemidler ved eksamen\nBergen: Kari Nordmann\nStord: Ola Dahl\nSogndal: Oppgåve i to delar."),
               "Hjelpemidler ved eksamen\nSogndal: Oppgåve i to delar.")
  f <- tempfile(fileext = ".RDS")
  saveRDS(tibble::tibble(text = "Ta kontakt med Kari Nordmann for mer."), f)
  expect_equal(personal_data_in(f)$pattern, "role_name_inline")
})

test_that("usn approval block and name + leader-role lines lose the name", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  expect_equal(an("usn", "Hjelpemidler.\n\nGodkjent\nNotodden 06.03.20\nKari Nordmann\nInstituttnestleder\n\nLitteratur"),
               "Hjelpemidler.\n\nGodkjent\nInstituttnestleder\n\nLitteratur")
  expect_equal(an("usn", "Godkjent\nNotodden 08.11.19\nKari Berg Nordmann (instiuttleder)\nLitteratur"),
               "Godkjent\nLitteratur")
  expect_equal(an("hvl", "Tekst\nKari Nordmann\nDekan\nMer tekst"), "Tekst\nMer tekst")
  # a committee or a title over a role heading stays (uib)
  uib <- "The Programme Committee for The Study of Religion\nEmneansvarlig"
  expect_equal(an("uib", uib), uib)
  # a role heading over a name line, also without a colon: both lines go;
  # a committee, a unit or a sentence under the heading stays
  expect_equal(an("uib", "Emneansvarlig\nOla Nordmann, Forskarskolen i klinisk medisin\nAdministrativt ansvarlig\nFakultetet"),
               "Administrativt ansvarlig\nFakultetet")
  expect_equal(an("uia", "Tekst.\nContact persons:\nKari Nordmann and Ola Dahl"), "Tekst.")
  for (t in c("Emneansvarlig\nThe Programme Committee", "Emneansvarlig\nInstitutt for pedagogikk",
              "Emneansvarlig\nProgramstyret for nordisk", "Programansvarlig\nEmneansvarlig\nFagmiljøet i nordisk",
              "Sensor\n\nEkstern Sensor brukes jevnlig.")) {
    expect_equal(an("uib", t), t, label = t)
  }
  f <- tempfile(fileext = ".RDS")
  saveRDS(tibble::tibble(text = "Godkjent\nKari Nordmann\nInstituttnestleder"), f)
  expect_true("name_role_lines" %in% personal_data_in(f)$pattern)
})

test_that("the personal-data check finds coordinator blocks and sentences", {
  forms <- c(heading = "Emneansvarlig\n\nKari Nordmann",
             part = "Emneansvarlig\n\nDelemne Sosialt arbeid og helse: Kari Nordmann",
             title = "Emneansvarlig\n\nMaster thesis coordinator: assistant professor Kari Nordmann",
             unit = "Emneansvarlig\nOla Nordmann, Forskarskolen i klinisk medisin",
             english = "Contact persons:\nKari Nordmann and Ola Dahl",
             sentence = "Emneansvarlige er: Kari Nordmann (sosionom) og Ola Dahl (pedagog).")
  f <- tempfile(fileext = ".RDS")
  for (k in names(forms)) {
    saveRDS(tibble::tibble(text = forms[[k]]), f)
    expect_gt(nrow(personal_data_in(f)), 0, label = k)
  }
  saveRDS(tibble::tibble(text = anonymize_text(rep("oslomet", length(forms)), unname(forms),
                                               .progress = FALSE)), f)
  expect_equal(nrow(personal_data_in(f)), 0)
})

test_that("admin dates go, government documents keep theirs", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  keep <- c("Meld. St. 28 (2015-2016) og St.meld. nr. 35 (2007–2008).",
            "Prop. 57 L (2022 – 2023)", "Kunnskapsdepartementet, 2015-2016,",
            "NB: Ny opplæringslov fra 01.08.24",
            "LOV-1998-07-17-61 (sist endret 1.1.2018)",
            "forskrift om rammeplan, fastsatt av Kunnskapsdepartementet 7. juni 2016",
            "Ny læreplan som trer i kraft høsten 2020.",
            "https://www.stortinget.no/dokumentserien/2020-2021/dok3.pdf",
            "rapporter/2016/ungkost-rapport-24.06.16.pdf")
  for (x in keep) expect_equal(an("usn", x), x, info = x)
  expect_equal(an("nla", "Studieår: 2019 — 2020"), "Studieår:")
  expect_equal(an("usn", "Godkjent 9.3.21.\nEmneplan godkjent 25.03.25\nGodkjent av instituttleder 18.10.22"),
               "Godkjent\nEmneplan godkjent\nGodkjent av instituttleder")
  expect_equal(an("nord", "Oppsatt skoleeksamen 16.12.20 gjøres om."), "Oppsatt skoleeksamen gjøres om.")
  expect_equal(an("oslomet", "Fagplanen ble godkjent i studieutvalget 14. november 2016.\nRedaksjonelle endringar lagt inn 3. juli 2017, 19. april og 7. juni 2018"),
               "Fagplanen ble godkjent i studieutvalget\nRedaksjonelle endringar lagt inn")
  expect_equal(an("usn", "Naturfag (MG1NA1) H2018 Notodden\nPensumliste for H2025-V2026\ndidaktisk arbeid- H23"),
               "Naturfag (MG1NA1) Notodden\nPensumliste for\ndidaktisk arbeid")
  expect_equal(an("uis", "Tekst\nLENG145_1 Bokmål Versjon 20.Februar.2023\nMer"), "Tekst\nMer")
  expect_equal(an("usn", "Godkjent desember 2017.\nEmneplan godkjent mars 2022."),
               "Godkjent\nEmneplan godkjent")
  expect_equal(an("usn", paste("Endringsbeskrivelse", "November 2020: Endret karakterskala.",
                               "November 2021 - Endret på vurderingsform", "27.08.2020: Endring i forkunnskapskrav.",
                               sep = "\n")),
               "Endringsbeskrivelse\nEndret karakterskala.\nEndret på vurderingsform\nEndring i forkunnskapskrav.")
  # cohort and plan-version years are content
  for (x in c("April 1940: Tyskland angriper Norge", "Studieplan som gjelder fra og med 2013 (kull 2010-2012)"))
    expect_equal(an("uit", x), x, info = x)
})

test_that("the guard looks back from the date's start; ISO and reference dates stay", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  keep <- c("Utdanningsdirektoratets spørreundersøkelse til lærere høsten\n2020",
            paste("nasjonale retningslinjer for grunnskolelærerutdanningen trinn 1-7 av",
                  "1. september 2016, revidert 17. oktober 2018, og"),
            "Teaching children mathematics, 6, 7, 2001-02-01, Sidespenn 346-349,",
            "Oversettelse fra Russisk. Razsviet, 4.11.1923.", "Sabima, Sabima, 30.4.2018,",
            "utfyllende regler for realfagsstudiene av 01.08.2015 for gjentak")
  for (x in keep) expect_equal(an("usn", x), x, info = x)
  expect_equal(an("uit", "Studieår 2023-2024\nGodkjent 15.03.2024\nEksamen 15.12.2023"),
               "Studieår\nGodkjent\nEksamen")
})

test_that("handlers follow the pages: mf notice, inn titles, hvl notice, usn types", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  mf <- paste("Litteraturliste", "Litteraturlisten for dette emnet", "",
              "Litteraturlisten for emnet finner du via lenken under.", "",
              "Tilgang til litteratur:", "", "Noe er digitalt.", "via ditt lokale folkebibliotek.", "",
              "MERK: Sjekk at litteraturlisten viser riktig semester (oppdatering kan ta litt tid).",
              "Vurdering", sep = "\n")
  expect_equal(an("mf", mf), "Litteraturliste\n\nVurdering")
  expect_equal(an("inn", "2MEN171-1 Engelsk 1, emne 1\nUndervisnings- og eksamensspråk\nEngelsk"),
               "2MEN171-1 Engelsk 1, emne 1\nUndervisnings- og eksamensspråk\nEngelsk")
  expect_equal(an("hvl", "Om emnet\nEmnet tilbys ikke studieåret 2025/2026.\nInnhald"), "Om emnet\nInnhald")
  expect_equal(an("usn", "Government DocumentLæreplan i norsk\nVideoAntz\nOpenStreetMap"),
               "Læreplan i norsk\nAntz\nOpenStreetMap")
})

test_that("handler variants: hvl Nynorsk notice, usn labels, oslomet ';' glue and stamps", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  expect_equal(an("hvl", "Om emnet\nEmnet vert ikkje tilbydd studieåret 2025/2026.\nInnhald"), "Om emnet\nInnhald")
  # the English notice, and the notice in the semester field, go whole: without
  # the year they would say the course is never taught
  expect_equal(an("hvl", "Om emnet\nThe module is not offered in the academic year 2024/2025.\nInnhald"),
               "Om emnet\nInnhald")
  expect_equal(an("hvl", "Undervisningssemester: Vår. Emnet har ikke undervisning våren 2025.\nSpråk: Norsk"),
               "Undervisningssemester: Vår.\nSpråk: Norsk")
  expect_equal(an("usn", "ArticleiPed: Pedagogy\nArticle“Translanguaging”\nWebsitehttps://www.unhcr.org/"),
               "iPed: Pedagogy\n“Translanguaging”\nhttps://www.unhcr.org/")
  keep <- "Mappeinnlevering med to deler:\nVideoen vil bli lagt ut.\nArticles: se Canvas."
  expect_equal(an("usn", keep), keep)
  expect_equal(an("oslomet", "En;viktig del;av frister.;Det\n;Tema 2: Læreplanteori;;;\nomfang 1;000 ord. Godt; men"),
               "En viktig del av frister. Det\nTema 2: Læreplanteori\nomfang 1 000 ord. Godt; men")
  expect_equal(an("oslomet", paste("Revisjon godkjent av utdanningsutvalget 3. mai 2018 og på fullmakt av",
                                   "leder i utdanningsutvalget 4. mars 2019\nEmneplanen er godkjent 21. juni2021.")),
               paste("Revisjon godkjent av utdanningsutvalget og på fullmakt av leder i utdanningsutvalget",
                     "Emneplanen er godkjent", sep = "\n"))
})

test_that("a removal takes the space and separators that joined it", {
  an <- function(inst, x) anonymize_text(inst, x, .progress = FALSE)
  expect_equal(an("uio", "meldt til studieadministrasjonen studieinfo@ibv.uio.no, før forelesningen"),
               "meldt til studieadministrasjonen, før forelesningen")
  expect_equal(an("uit", "Emneplan godkjent 15.03.2024.\nFaget går høsten 2024."),
               "Emneplan godkjent.\nFaget går.")
  expect_equal(an("uit", "Pensumliste for LER-1402, 5-10, 15 stp, høst 2019 og vår 2020\nNaturfag 5-10 - høst 2020"),
               "Pensumliste for LER-1402, 5-10, 15 stp\nNaturfag 5-10")
  expect_equal(an("nord", "Muntlig eksamen\tDato: 27.05.2024 –31.05.2024 Varighet: 30 Minutter"),
               "Muntlig eksamen\tDato: Varighet: 30 Minutter")
  expect_equal(an("nla", "Avsluttende vurdering\nHøst 2024:\nMappe"), "Avsluttende vurdering\n\nMappe")
  expect_equal(an("uit", "MERK: Våren 2021: Skriftlig eksamen\nmasteroppgave vår 2025: For"),
               "MERK: Skriftlig eksamen\nmasteroppgave: For")
})

test_that("clock times go; durations, verses, ratios and credits stay", {
  an <- function(x) anonymize_text("uis", x, .progress = FALSE)
  keep <- c("A Monster Calls (2016). Lengde: 2:19", "22. juli-terroren [20:28]",
            "Masterstudenter i historie, 10:27, 2020,", "Sure 2:177, Sure 3:30-32",
            "ISBN-10:82-05-34802-8", "2018;24:40–45.", "Vektingen er 60:40.",
            "LER-2303 Naturfag 5-10 Høst 10 stp", "Modern Learners, Total Pages 1:00:09,")
  for (x in keep) expect_equal(an(x), x, info = x)
  expect_equal(an("Tid Utlevering 09:00 Innlevering 23:59. Onsdager 14:30-15:30."),
               "Tid Utlevering Innlevering. Onsdager.")
})

test_that("data/processed holds no personal data", {
  files <- list.files(here::here("data/processed"), "\\.RDS$", recursive = TRUE, full.names = TRUE)
  skip_if(length(files) == 0, "built data not available")
  found <- personal_data_in(files)
  expect(nrow(found) == 0, paste0(
    "Personal data in data/processed (every text there must come from ",
    "anonymize_text()):\n",
    paste(utils::capture.output(print(as.data.frame(found))), collapse = "\n")))
})
