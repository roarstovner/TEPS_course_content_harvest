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

test_that("page furniture: OpenURL links, uis PDF header, usn Leganto labels (#287)", {
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

test_that("season words after semester keyword are removed", {
  result <- anonymize_text("hivolda", "Undervisningssemester: Vår\nEksamen: Høst", .progress = FALSE)
  expect_false(grepl("(?i)\\bvår\\b", result, perl = TRUE))
  expect_false(grepl("(?i)\\bhøst\\b", result, perl = TRUE))
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

test_that("NTNU: Kontaktinformasjon section stripped to end", {
  input <- "Faglig innhold\nViktig tekst\nKontaktinformasjon\nNavn: Ola\nEksamen: dato"
  result <- anonymize_text("ntnu", input, .progress = FALSE)
  expect_true(grepl("Viktig tekst", result))
  expect_false(grepl("Kontaktinformasjon", result))
  expect_false(grepl("Ola", result))
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

test_that("INN: status banner removed", {
  input <- "Statusmelding\nEmnebeskrivelsen for valgt semester er ikke publisert enda. Her er siste versjon\nKursinnhold"
  result <- anonymize_text("inn", input, .progress = FALSE)
  expect_false(grepl("Statusmelding", result))
  expect_true(grepl("Kursinnhold", result))
})

test_that("INN: Engelsk normalized to English", {
  input <- "Språk\nEngelsk\nInnhold her"
  result <- anonymize_text("inn", input, .progress = FALSE)
  expect_false(grepl("Engelsk", result))
  expect_true(grepl("English", result))
})

# --- OsloMet ---

test_that("OsloMet: error pages return NA", {
  input <- "Siden du leter etter finnes ikke, eller det har oppstått en feil."
  result <- anonymize_text("oslomet", input, .progress = FALSE)
  expect_true(is.na(result))
})

test_that("OsloMet: Emneansvarlig + name stripped", {
  input <- "Studiepoeng 15\nEmneansvarlig\n\nKari Nordmann\nInnhold"
  result <- anonymize_text("oslomet", input, .progress = FALSE)
  expect_false(grepl("Kari Nordmann", result))
  expect_true(grepl("Innhold", result))
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

# --- Staff name lists (#237) ---

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

test_that("a role label followed by a name is removed (#260)", {
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

# --- Signature lines (#238) ---

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

test_that("steiner: bare PDF page numbers are removed, numbers in text kept (#248)", {
  input <- "kan gi tilbakemelding\n10\nkan anvende kunnskaper\nOmfang: 10 studiepoeng"
  expect_equal(anonymize_text("steiner", input, .progress = FALSE),
               "kan gi tilbakemelding\nkan anvende kunnskaper\nOmfang: 10 studiepoeng")
})

# --- #224, #227-#230 ---

test_that("inflected and run-together season + year are removed (#227)", {
  expect_equal(anonymize_text("nih", "Pensumliste for høsten 2025 og våren2026.", .progress = FALSE),
               "Pensumliste for  og .")
})

test_that("academic-year ranges go, content ranges stay (#230)", {
  expect_equal(anonymize_text("nih", "Se Studiehåndbok 2023-2024 og 2023- 24.", .progress = FALSE),
               "Se Studiehåndbok  og .")
  expect_equal(anonymize_text("nih", "Perioden 1945-1970 og 1914-18.", .progress = FALSE),
               "Perioden 1945-1970 og 1914-18.")
})

test_that("brackets emptied by year removal go, code brackets stay (#229)", {
  expect_equal(anonymize_text("hiof", "Meld. St. 16 (2016-2017) og print() i Python.", .progress = FALSE),
               "Meld. St. 16 og print() i Python.")
})

test_that("a bracketed e-mail leaves no empty brackets (#229)", {
  expect_equal(anonymize_text("uio", "Ta kontakt med Per Hansen (per.hansen@uio.no) eller oss.", .progress = FALSE),
               "Ta kontakt med eller oss.")
})

test_that("mf exam-dates block and library notice are removed (#228)", {
  input <- paste("Skriftlig eksamen.", "", "Eksamensdatoer", "Eksamensdato:", "18. november 2026",
                 "", "Læringsutbytte", "Kunnskap", "", "Litteraturliste", "Litteraturlisten for vår 2026", "",
                 "Tilgang til litteratur", "Noe er digitalt.", "via ditt lokale folkebibliotek.", sep = "\n")
  expect_equal(anonymize_text("mf", input, .progress = FALSE),
               "Skriftlig eksamen.\n\nLæringsutbytte\nKunnskap\n\nLitteraturliste")
})

test_that("uio exam-links tail is cut (#224)", {
  expect_equal(anonymize_text("uio", "Skriftlig eksamen.\nMer om eksamen ved UiO\nHvordan bruke KI som student", .progress = FALSE),
               "Skriftlig eksamen.")
})

test_that("phone numbers after 'Tlf.' are removed", {
  expect_equal(anonymize_text("uib", "Studieadministrasjonen. Tlf. 55 58 27 10", .progress = FALSE),
               "Studieadministrasjonen.")
})

# --- The shareable files (README "Data Files: Published and Internal") ---

test_that("the personal-data check finds what the anonymizer removes", {
  f <- tempfile(fileext = ".RDS")
  saveRDS(tibble::tibble(id = 1:3, text = c(
    "Kontakt kari.nordmann@uit.no", "- Ola Nordmann (Emneansvarlig)\nInnhold", "Ingen")), f)
  found <- personal_data_in(f)
  expect_setequal(found$pattern, c("email", "staff"))
  saveRDS(tibble::tibble(text = anonymize_text(c("uit", "uit"), c(
    "Kontakt kari.nordmann@uit.no", "- Ola Nordmann (Emneansvarlig)\nInnhold"),
    .progress = FALSE)), f)
  expect_equal(nrow(personal_data_in(f)), 0)
})

test_that("data/processed holds no personal data", {
  files <- Sys.glob(here::here("data/processed/*.RDS"))
  skip_if(length(files) == 0, "built data not available")
  found <- personal_data_in(files)
  expect(nrow(found) == 0, paste0(
    "Personal data in data/processed (every text there must come from ",
    "anonymize_text()):\n",
    paste(utils::capture.output(print(as.data.frame(found))), collapse = "\n")))
})
