# Tests for normalize_plan_text.R and deduplicate_plans.R

source(here::here("R/normalize_plan_text.R"))
source(here::here("R/deduplicate_plans.R"))

# --- normalize_plan_text ---

test_that("NA and empty input returns NA", {
  expect_equal(normalize_plan_text("ntnu", NA_character_), NA_character_)
  expect_equal(normalize_plan_text("ntnu", ""), NA_character_)
})

test_that("vectorized input works", {
  result <- normalize_plan_text(
    c("ntnu", "hivolda"),
    c("Some course text", NA_character_)
  )
  expect_length(result, 2)
  expect_type(result, "character")
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("mismatched lengths error", {
  expect_error(normalize_plan_text("ntnu", c("a", "b")))
})

# --- Generic normalization ---

test_that("4-digit years are removed", {
  result <- normalize_plan_text("hivolda", "Kurs opprettet 2020 og oppdatert 2023")
  expect_false(grepl("2020|2023", result))
  expect_true(grepl("kurs opprettet", result))
})

test_that("dates dd.mm.yyyy are removed", {
  result <- normalize_plan_text("hivolda", "Eksamen 15.12.2023 kl 09:00")
  expect_false(grepl("15\\.12\\.2023", result))
})

test_that("times HH:MM are removed", {
  result <- normalize_plan_text("hivolda", "Forelesning starter 08:15 og slutter 10:00")
  expect_false(grepl("08:15|10:00", result))
})

test_that("Norwegian date-time format is removed", {
  result <- normalize_plan_text("hivolda", "Hentet 12. feb. 2026 02:50:04")
  expect_false(grepl("feb", result))
  expect_false(grepl("2026", result))
})

test_that("Sist hentet fra FS timestamp is removed", {
  input <- "Kursinnhold her. Sist hentet fra FS (Felles studentsystem) 9. feb. 2026 11:30:35"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("Sist hentet", result, ignore.case = TRUE))
  expect_true(grepl("kursinnhold", result))
})

test_that("whitespace is normalized", {
  result <- normalize_plan_text("hivolda", "  Mye   ekstra    mellomrom  ")
  expect_equal(result, "mye ekstra mellomrom")
})

test_that("tolower: KUNNSKAP and Kunnskap normalize identically", {
  a <- normalize_plan_text("hivolda", "KUNNSKAP om faget")
  b <- normalize_plan_text("hivolda", "Kunnskap om faget")
  expect_equal(a, b)
  expect_true(grepl("kunnskap", a))
})

test_that("semester words are removed generically", {
  result <- normalize_plan_text("hivolda", "Undervisning Vår og Høst og Haust")
  expect_false(grepl("vår|høst|haust", result))
  expect_true(grepl("undervisning", result))
})

test_that("2-digit season+year removed", {
  result <- normalize_plan_text("hivolda", "Oppstart Høst23 og Vår 22")
  expect_false(grepl("23|22", result))
  expect_true(grepl("oppstart", result))
})

test_that("Emneansvarlige (plural) is removed", {
  input <- "Intro Emneansvarlige: Ola Nordmann Undervisning her"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("ola nordmann", result))
  expect_true(grepl("undervisning", result))
})

test_that("Godkjent av: Name is removed", {
  input <- "Intro Godkjent av: Kari Nordmann Undervisning her"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("kari nordmann", result))
  expect_true(grepl("undervisning", result))
})

test_that("Nynorsk Sist henta frå FS is removed", {
  input <- "Kursinnhold\nSist henta frå FS (Felles studentsystem) 9. feb. 2026 11:30:35"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("sist henta", result))
  expect_true(grepl("kursinnhold", result))
})

# --- NTNU pre-processing ---

test_that("NTNU: Kontaktinformasjon section stripped to end", {
  input <- "Faglig innhold\nViktig tekst\nKontaktinformasjon\nNavn: Ola\nEksamen: dato"
  result <- normalize_plan_text("ntnu", input)
  expect_true(grepl("viktig tekst", result))
  expect_false(grepl("kontaktinformasjon", result))
  expect_false(grepl("ola", result))
})

test_that("NTNU: course-details-portlet removed", {
  input <- "course-details-portlet ENG3901 Masteroppgave"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("course-details-portlet", result))
  expect_true(grepl("masteroppgave", result))
})

test_that("NTNU: moment.locale removed", {
  input <- 'moment.locale("nb_NO"); ENG3901'
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("moment", result))
})

test_that("NTNU: year dropdown removed", {

  input <- "Velg studieår Studieår 2024/2025 Studieår 2023/2024 Studiepoeng 30"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("velg studieår", result))
  expect_false(grepl("studieår", result))
  expect_true(grepl("studiepoeng", result))
})

test_that("NTNU: Undervisningsstart line removed", {
  input <- "Nivå Master\nUndervisningsstart Høst 2024 / Vår 2025\nVarighet 2 semestre"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("undervisningsstart", result))
  expect_true(grepl("varighet", result))
})

test_that("NTNU: Blackboard links removed", {
  input <- "Andre sider om emnet Blackboard - VÅR-2024 Fagområder"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("blackboard", result))
})

# --- UiT pre-processing ---

test_that("UiT: breadcrumbs removed", {
  input <- "Startsida\nEmnekatalog\nKursnavn og innhold"
  result <- normalize_plan_text("uit", input)
  expect_false(grepl("startsida", result))
  expect_false(grepl("emnekatalog", result))
  expect_true(grepl("kursnavn", result))
})

test_that("UiT: Error rendering component removed", {
  input <- "Læringsutbytte Error rendering component Innhold"
  result <- normalize_plan_text("uit", input)
  expect_false(grepl("error rendering", result))
  expect_true(grepl("læringsutbytte", result))
})

# --- UiA pre-processing ---

test_that("UiA: breadcrumb removed without eating content", {
  input <- "Forside > Studier > Emner > 2025 > Høst 2025 > EN-148 Fagdidaktikk (Høst 2025) Studiepoeng: 7.5"
  result <- normalize_plan_text("uia", input)
  expect_false(grepl("forside", result))
  expect_true(grepl("studiepoeng", result))
})

test_that("UiA: (Høst YYYY) removed from title", {
  input <- "EN-148 Fagdidaktikk (Høst 2025) EN-148 Fagdidaktikk (Høst 2025) Studiepoeng"
  result <- normalize_plan_text("uia", input)
  expect_false(grepl("\\(høst", result))
})

test_that("UiA: Haust breadcrumb handled", {
  input <- "Forside > Studier > Emner > 2025 > Haust 2025 > NO-503 Nynorsk (Haust 2025) Studiepoeng"
  result <- normalize_plan_text("uia", input)
  expect_false(grepl("forside", result))
  expect_false(grepl("\\(haust", result, ignore.case = TRUE))
  expect_true(grepl("studiepoeng", result))
})

# --- HiOF pre-processing ---

test_that("HiOF: Sist hentet fra FS removed", {
  input <- "Kursinnhold\nSist hentet fra FS (Felles studentsystem) 12. feb. 2026 02:50:04"
  result <- normalize_plan_text("hiof", input)
  expect_false(grepl("sist hentet", result))
})

test_that("HiOF: Litteraturlista sist oppdatert removed", {
  input <- "Pensum\nLitteraturlista er sist oppdatert 16.08.2016\nAnnet innhold"
  result <- normalize_plan_text("hiof", input)
  expect_false(grepl("litteraturlista er sist oppdatert", result))
  expect_true(grepl("annet innhold", result))
})

test_that("HiOF: space inserted after heading before uppercase", {
  input <- "KunnskapStudenten skal lære"
  result <- normalize_plan_text("hiof", input)
  expect_true(grepl("kunnskap studenten", result))
})

# --- HIVOLDA pre-processing ---

test_that("HIVOLDA: Emneansvarleg + name on next line stripped", {
  input <- "Innhold her\nEmneansvarleg:\nIngeborg Katrin Lid Berget\nPensum\nListe"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("ingeborg|berget", result))
  expect_true(grepl("innhold|pensum", result))
})

test_that("HIVOLDA: Godkjent av + name on next line stripped", {
  input <- "Innhold her\nGodkjent av:\nSilje Ims Lied"
  result <- normalize_plan_text("hivolda", input)
  expect_false(grepl("silje|lied", result))
  expect_true(grepl("innhold", result))
})

test_that("HiOF: multiline Emneansvarlige block stripped", {
  input <- "Studiested:\nHalden\nEmneansvarlige:\nRagnhild Louise Næsje\nChristian Bjørn Bjerke\nUndervisningsspråk:\nNorsk"
  result <- normalize_plan_text("hiof", input)
  expect_false(grepl("ragnhild|næsje|bjerke", result))
  expect_true(grepl("undervisningsspråk", result))
})

# --- INN pre-processing ---

test_that("INN: status banner removed", {
  input <- "Statusmelding\nEmnebeskrivelsen for valgt semester er ikke publisert enda. Her er siste versjon\nKursinnhold"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("statusmelding", result))
  expect_true(grepl("kursinnhold", result))
})

test_that("INN: Startsemestre metadata removed", {
  input <- "Startsemestre\n2026 Høst\nEmnekode\nABC123"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("startsemestre", result))
  expect_true(grepl("abc123", result))
})

test_that("INN: placeholder pages return NA", {
  input <- "Startsemester 2029 Emnesøket gjelder kun fra 2022. Some course list"
  result <- normalize_plan_text("inn", input)
  expect_true(is.na(result))
})

test_that("INN: Norwegian field labels stripped", {
  input <- "ABC123 Testkurs\nEmnekode\nABC123\nStudiepoeng\n15\nEmnets innhold\nViktig faglig innhold"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("emnekode", result))
  expect_false(grepl("studiepoeng", result))
  expect_false(grepl("emnets innhold", result))
  expect_true(grepl("viktig faglig innhold", result))
})

test_that("INN: English field labels stripped", {
  input <- "ABC123 Test\nCourse code\nABC123\nNumber of credits\n15\nCourse content\nImportant content"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("course code", result, ignore.case = TRUE))
  expect_false(grepl("number of credits", result, ignore.case = TRUE))
  expect_false(grepl("course content", result, ignore.case = TRUE))
  expect_true(grepl("important content", result))
})

test_that("INN: additional English labels stripped", {
  input <- "ABC123\nCompulsory activities\nSome requirement\nAssessment methods\nWritten exam\nWork requirements\nAttendance"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("compulsory activities", result, ignore.case = TRUE))
  expect_false(grepl("assessment methods", result, ignore.case = TRUE))
  expect_false(grepl("work requirements", result, ignore.case = TRUE))
})

test_that("INN: Engelsk normalized to English", {
  input <- "Språk\nEngelsk\nInnhold her"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("engelsk", result))
  expect_true(grepl("english", result))
})

test_that("INN: semester value lines stripped", {
  input <- "ABC123 Test\n2023 Høst, 2024 Høst\nInnhold her"
  result <- normalize_plan_text("inn", input)
  expect_true(grepl("innhold her", result))
})

# --- MF pre-processing ---

test_that("MF: Emneansvarlig section stripped to end", {
  input <- "Kursinnhold viktig tekst\nEmneansvarlig\nOla Nordmann\nForsteamanuensis\nola@mf.no\nStudentlivet på MF"
  result <- normalize_plan_text("mf", input)
  expect_true(grepl("viktig tekst", result))
  expect_false(grepl("ola nordmann", result))
  expect_false(grepl("studentlivet", result))
})

test_that("MF: Kontakt studieveileder removed", {
  input <- "Emneinfo Kontakt studieveileder Innholdsfortegnelse"
  result <- normalize_plan_text("mf", input)
  expect_false(grepl("kontakt studieveileder", result))
})

# --- OsloMet pre-processing ---

test_that("OsloMet: error pages return NA", {
  input <- "EPN-V2\nFjern Feil:Siden du leter etter finnes ikke, eller det har oppstått en feil. Prøv igjen senere."
  result <- normalize_plan_text("oslomet", input)
  expect_true(is.na(result))
})

test_that("OsloMet: real content is preserved", {
  input <- "EPN-V2\nMGVM4100 Vitenskapsteori og metode Emneplan\nStudiepoeng 15"
  result <- normalize_plan_text("oslomet", input)
  expect_false(is.na(result))
  expect_true(grepl("vitenskapsteori", result))
})

# --- UiS pre-processing ---

test_that("UiS: Kontakt section stripped to end (HTML pages)", {
  input <- "Innhold\nKontakt\nEmneansvarlig:\nBerit Aarrestad\nFaglærer:\nSara Sedberg"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("kontakt|aarrestad|sedberg", result))
  expect_true(grepl("innhold", result))
})

test_that("UiS: EMNE Versjon header stripped (PDF pages)", {
  input <- "EMNE LFYBAC_1 BOKMÅL Versjon 14.Jun.2019 15:15\nInnhold"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("emne lfybac|versjon", result))
  expect_true(grepl("innhold", result))
})

test_that("UiS: Fagpersoner section stripped (PDF pages)", {
  input <- "Innhold\nFagpersoner\n-Diana Lucia Quintero Castro (Faglærer)\n\nAnnet"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("fagpersoner|diana|quintero", result))
  expect_true(grepl("innhold", result))
})

test_that("UiS: Powered by TCPDF stripped", {
  input <- "Innhold\nPowered by TCPDF (www.tcpdf.org)"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("tcpdf", result))
  expect_true(grepl("innhold", result))
})

test_that("UiS: 'side N' page numbers stripped (PDF pages)", {
  input <- "Innhold\n                                   side 2\nMer innhold"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("side 2", result))
  expect_true(grepl("innhold", result))
})

test_that("UiS: Emnebeskrivelsen er hentet fra stripped", {
  input <- "Innhold\nEmnebeskrivelsen er hentet fra Felles studentsystem Versjon 1"
  result <- normalize_plan_text("uis", input)
  expect_false(grepl("emnebeskrivelsen er hentet", result))
  expect_true(grepl("innhold", result))
})

# --- USN pre-processing ---

test_that("USN: Godkjent emneplan heading stripped", {
  input <- "Innhold\nGodkjent emneplan\nLitteratur"
  result <- normalize_plan_text("usn", input)
  expect_false(grepl("godkjent emneplan", result))
  expect_true(grepl("innhold|litteratur", result))
})

test_that("USN: Godkjent date line stripped", {
  input <- "Innhold\nGodkjent 22.11.2022.\nLitteratur"
  result <- normalize_plan_text("usn", input)
  expect_false(grepl("godkjent \\.", result))
  expect_true(grepl("innhold|litteratur", result))
})

# --- Unknown institution passes through ---

test_that("unknown institution applies only generic normalization", {
  result <- normalize_plan_text("unknown_inst", "Kurs i 2023 med eksamen 15.12.2023")
  expect_false(grepl("2023", result))
  expect_true(grepl("kurs i", result))
})

# --- build_plan_id ---

test_that("same input produces same hash", {
  expect_equal(build_plan_id("hello"), build_plan_id("hello"))
})

test_that("different input produces different hash", {
  expect_false(build_plan_id("hello") == build_plan_id("world"))
})

test_that("NA input returns NA", {
  expect_true(is.na(build_plan_id(NA_character_)))
})

test_that("empty string returns NA", {
  expect_true(is.na(build_plan_id("")))
})

test_that("vectorized hashing works", {
  ids <- build_plan_id(c("a", "b", NA, "a"))
  expect_length(ids, 4)
  expect_equal(ids[1], ids[4])
  expect_true(is.na(ids[3]))
  expect_false(ids[1] == ids[2])
})

# --- deduplicate_plans ---

test_that("deduplicate_plans returns correct structure", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    fulltext = c("Same plan text", "Same plan text", "Updated plan", "Updated plan")
  )

  result <- deduplicate_plans(df)

  expect_type(result, "list")
  expect_named(result, c("plans", "courses"))
  expect_s3_class(result$plans, "tbl_df")
  expect_s3_class(result$courses, "tbl_df")
})

test_that("deduplicate_plans reduces identical plans", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    fulltext = c("Same plan text", "Same plan text", "Updated plan", "Updated plan")
  )

  result <- deduplicate_plans(df)

  # Should have 2 unique plans, not 4
  expect_equal(nrow(result$plans), 2)
  # Courses should keep all 4 rows with plan_content_id
  expect_equal(nrow(result$courses), 4)
  expect_true("plan_content_id" %in% names(result$courses))
})

test_that("deduplicate_plans computes correct year ranges", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    fulltext = c("Plan A", "Plan A", "Plan B", "Plan B")
  )

  result <- deduplicate_plans(df)
  plans <- result$plans |> dplyr::arrange(year_from)

  expect_equal(plans$year_from, c(2020, 2022))
  expect_equal(plans$year_to, c(2021, 2023))
})

test_that("deduplicate_plans handles NA fulltext", {
  df <- tibble::tibble(
    institution_short = c("hivolda", "hivolda"),
    Emnekode = c("TEST101", "TEST102"),
    Årstall = c(2020, 2020),
    fulltext = c("Real plan", NA_character_)
  )

  result <- deduplicate_plans(df)

  # Plan lookup should only have the non-NA one
  expect_equal(nrow(result$plans), 1)
  # Courses should have both rows
  expect_equal(nrow(result$courses), 2)
  expect_true(is.na(result$courses$plan_content_id[2]))
})

test_that("deduplicate_plans errors on missing columns", {
  df <- tibble::tibble(x = 1)
  expect_error(deduplicate_plans(df))
})
