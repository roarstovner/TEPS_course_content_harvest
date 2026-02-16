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
  expect_true(grepl("Kurs opprettet", result))
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
  expect_false(grepl("Sist hentet", result))
  expect_true(grepl("Kursinnhold", result))
})

test_that("whitespace is normalized", {
  result <- normalize_plan_text("hivolda", "  Mye   ekstra    mellomrom  ")
  expect_equal(result, "Mye ekstra mellomrom")
})

# --- NTNU pre-processing ---

test_that("NTNU: Kontaktinformasjon section stripped to end", {
  input <- "Faglig innhold\nViktig tekst\nKontaktinformasjon\nNavn: Ola\nEksamen: dato"
  result <- normalize_plan_text("ntnu", input)
  expect_true(grepl("Viktig tekst", result))
  expect_false(grepl("Kontaktinformasjon", result))
  expect_false(grepl("Ola", result))
})

test_that("NTNU: course-details-portlet removed", {
  input <- "course-details-portlet ENG3901 Masteroppgave"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("course-details-portlet", result))
  expect_true(grepl("Masteroppgave", result))
})

test_that("NTNU: moment.locale removed", {
  input <- 'moment.locale("nb_NO"); ENG3901'
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("moment", result))
})

test_that("NTNU: year dropdown removed", {

  input <- "Velg studieår Studieår 2024/2025 Studieår 2023/2024 Studiepoeng 30"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("Velg studieår", result))
  expect_false(grepl("Studieår", result))
  expect_true(grepl("Studiepoeng", result))
})

test_that("NTNU: Undervisningsstart line removed", {
  input <- "Nivå Master\nUndervisningsstart Høst 2024 / Vår 2025\nVarighet 2 semestre"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("Undervisningsstart", result))
  expect_true(grepl("Varighet", result))
})

test_that("NTNU: Blackboard links removed", {
  input <- "Andre sider om emnet Blackboard - VÅR-2024 Fagområder"
  result <- normalize_plan_text("ntnu", input)
  expect_false(grepl("Blackboard", result))
})

# --- UiT pre-processing ---

test_that("UiT: breadcrumbs removed", {
  input <- "Startsida\nEmnekatalog\nKursnavn og innhold"
  result <- normalize_plan_text("uit", input)
  expect_false(grepl("Startsida", result))
  expect_false(grepl("Emnekatalog", result))
  expect_true(grepl("Kursnavn", result))
})

test_that("UiT: Error rendering component removed", {
  input <- "Læringsutbytte Error rendering component Innhold"
  result <- normalize_plan_text("uit", input)
  expect_false(grepl("Error rendering", result))
  expect_true(grepl("Læringsutbytte", result))
})

# --- UiA pre-processing ---

test_that("UiA: breadcrumb removed without eating content", {
  input <- "Forside > Studier > Emner > 2025 > Høst 2025 > EN-148 Fagdidaktikk (Høst 2025) Studiepoeng: 7.5"
  result <- normalize_plan_text("uia", input)
  expect_false(grepl("Forside", result))
  expect_true(grepl("Studiepoeng", result))
})

test_that("UiA: (Høst YYYY) removed from title", {
  input <- "EN-148 Fagdidaktikk (Høst 2025) EN-148 Fagdidaktikk (Høst 2025) Studiepoeng"
  result <- normalize_plan_text("uia", input)
  expect_false(grepl("\\(Høst", result))
})

# --- HiOF pre-processing ---

test_that("HiOF: Sist hentet fra FS removed", {
  input <- "Kursinnhold\nSist hentet fra FS (Felles studentsystem) 12. feb. 2026 02:50:04"
  result <- normalize_plan_text("hiof", input)
  expect_false(grepl("Sist hentet", result))
})

test_that("HiOF: Litteraturlista sist oppdatert removed", {
  input <- "Pensum\nLitteraturlista er sist oppdatert 16.08.2016\nAnnet innhold"
  result <- normalize_plan_text("hiof", input)
  expect_false(grepl("Litteraturlista er sist oppdatert", result))
  expect_true(grepl("Annet innhold", result))
})

# --- INN pre-processing ---

test_that("INN: status banner removed", {
  input <- "Statusmelding\nEmnebeskrivelsen for valgt semester er ikke publisert enda. Her er siste versjon\nKursinnhold"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("Statusmelding", result))
  expect_true(grepl("Kursinnhold", result))
})

test_that("INN: Startsemestre metadata removed", {
  input <- "Startsemestre\n2026 Høst\nEmnekode\nABC123"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("Startsemestre", result))
  expect_true(grepl("ABC123", result))
})

test_that("INN: placeholder pages return NA", {
  input <- "Startsemester 2029 Emnesøket gjelder kun fra 2022. Some course list"
  result <- normalize_plan_text("inn", input)
  expect_true(is.na(result))
})

test_that("INN: Norwegian field labels stripped", {
  input <- "ABC123 Testkurs\nEmnekode\nABC123\nStudiepoeng\n15\nEmnets innhold\nViktig faglig innhold"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("Emnekode", result))
  expect_false(grepl("Studiepoeng", result))
  expect_false(grepl("Emnets innhold", result))
  expect_true(grepl("Viktig faglig innhold", result))
})

test_that("INN: English field labels stripped", {
  input <- "ABC123 Test\nCourse code\nABC123\nNumber of credits\n15\nCourse content\nImportant content"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("Course code", result))
  expect_false(grepl("Number of credits", result))
  expect_false(grepl("Course content", result))
  expect_true(grepl("Important content", result))
})

test_that("INN: Engelsk normalized to English", {
  input <- "Språk\nEngelsk\nInnhold her"
  result <- normalize_plan_text("inn", input)
  expect_false(grepl("Engelsk", result))
  expect_true(grepl("English", result))
})

test_that("INN: semester value lines stripped", {
  input <- "ABC123 Test\n2023 Høst, 2024 Høst\nInnhold her"
  result <- normalize_plan_text("inn", input)
  expect_true(grepl("Innhold her", result))
})

# --- MF pre-processing ---

test_that("MF: Emneansvarlig section stripped to end", {
  input <- "Kursinnhold viktig tekst\nEmneansvarlig\nOla Nordmann\nForsteamanuensis\nola@mf.no\nStudentlivet på MF"
  result <- normalize_plan_text("mf", input)
  expect_true(grepl("viktig tekst", result))
  expect_false(grepl("Ola Nordmann", result))
  expect_false(grepl("Studentlivet", result))
})

test_that("MF: Kontakt studieveileder removed", {
  input <- "Emneinfo Kontakt studieveileder Innholdsfortegnelse"
  result <- normalize_plan_text("mf", input)
  expect_false(grepl("Kontakt studieveileder", result))
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
  expect_true(grepl("Vitenskapsteori", result))
})

# --- Unknown institution passes through ---

test_that("unknown institution applies only generic normalization", {
  result <- normalize_plan_text("unknown_inst", "Kurs i 2023 med eksamen 15.12.2023")
  expect_false(grepl("2023", result))
  expect_true(grepl("Kurs i", result))
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
