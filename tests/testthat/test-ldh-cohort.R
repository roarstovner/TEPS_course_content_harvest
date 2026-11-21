# Tests for LDH's cohort plans: a programme study plan's text split by course
# heading, with its cohort and each course's semesters, and the cohort an
# offering takes (cohort_plan_match()).

ldh_plan_pages <- c(
  paste("                     Studieplan", "         Master i sykepleie innen psykisk helse",
        "                               Kull V2025", "Innholdsfortegnelse",
        "      MAPSY-100: Innføring i sykepleie ...... 16", sep = "\n"),
  paste("MAPSY-100: Innføring i sykepleie innen psykisk helse, rus og", "avhengighet",
        "Engelsk emnenavn: Introduction to nursing", "Studiepoeng: 10", "Semester: 1", "",
        "Hensikten med emnet", "Hensikten er å gi innføring.", "",
        "          Master i sykepleie innen psykisk helse V2025. Sist oppdatert 10.06.26   16",
        "                                       Lovisenberg diakonale høgskole",
        "Vurderingsform", "Muntlig fremlegg i gruppe", sep = "\n"),
  paste("MAPNA-100: Samsykelighet", "Engelsk navn: Comorbidity", "Semester: 2", "", "Emnets innhold", "Rusmidler",
        "", "MFSY-500: Masteroppgave", "Engelsk emnenavn: Master thesis", "Semester: 5 og 6", "", "Hensikten med emnet",
        "Et vitenskapelig arbeid.", "", "STUDIETS OVERORDNEDE LÆRINGSUTBYTTER", "Kandidaten", sep = "\n"))

test_that("a study plan is split into its courses, with cohort and semesters", {
  x <- ldh_pdf_courses(ldh_plan_pages, "https://ldh.no/x/Studieplan%20MAPRA%202025.pdf")
  expect_equal(x$code, c("MAPSY-100", "MAPNA-100", "MFSY-500"))   # not the table of contents' line
  expect_equal(x$start, rep(semester_term(2025L, "Vår"), 3))       # "Kull V2025", not the file name's 2025
  expect_equal(x$k1, c(1L, 2L, 5L))
  expect_equal(x$k2, c(1L, 2L, 6L))
  expect_match(x$text[1], "^MAPSY-100: Innføring")
  expect_match(x$text[1], "Muntlig fremlegg i gruppe$")             # across the page break
  expect_no_match(x$text[1], "Lovisenberg diakonale høgskole|Sist oppdatert")
  expect_no_match(x$text[3], "OVERORDNEDE|Kandidaten")              # the plan's own part after the last course
  old <- ldh_pdf_courses(c("  Studieplan\nBachelor i sykepleie\n               BIS21\n",
                           paste("BSY-301 Sykepleie ved akutte sykdommer", " Engelsk emnenavn     Acute care nursing",
                                 " Semester             Tredje semester, høst", "", "Hensikt", "Tekst.", sep = "\n")),
                         "https://ldh.no/x/BIS21%20Studieplan.pdf")
  expect_equal(c(old$start, old$k1, old$k2), c(semester_term(2021L, "Høst"), 3L, 3L))
  expect_equal(nrow(ldh_pdf_courses("Studieplan uten kull\n", "https://ldh.no/x/plan.pdf")), 0)
})

test_that("a course ends at the plan's mixed-case heading, without page numbers and footers", {
  x <- ldh_pdf_courses(c(
    "Studieplan\nBachelor i sykepleie\n            Kull BIS23\n",
    paste("BSYP-503 Sykepleier som fagutvikler", "Engelsk emnenavn: Leadership", "Semester: Femte (høst)", "",
          "Læringsutbytte", "Etter avsluttede praksisstudier skal studenten kunne", "", "Antall: 30",
          "                                 120", "", "                                    38",
          "    Master i klinisk intensivsykepleie H2025. Sist oppdatert 07.09.26",
          "                               Lovisenberg diakonale høgskole", sep = "\n"),
    paste("Vurderings-/eksamensform og vurderingsuttrykk", "Bestått/ikke bestått.", "",
          "Studiets overordnede læringsutbytter", "", "Forskrift om nasjonal retningslinje", "Kandidaten", sep = "\n")),
    "https://ldh.no/x/Studieplan%20BIS23.pdf")
  expect_equal(x$code, "BSYP-503")
  expect_match(x$text, "Læringsutbytte\nEtter avsluttede")         # a course's own heading does not end it
  expect_match(x$text, "\n {2,}120\n")                            # a number within the page stays
  expect_match(x$text, "Bestått/ikke bestått.$")                  # across the page break, to the plan's heading
  expect_no_match(x$text, "\\b38\\b|Sist oppdatert|Lovisenberg|overordnede|Kandidaten")
})

test_that("an offering takes the cohort that has the course in its term", {
  t <- function(y, s) semester_term(y, s)
  index <- tibble::tibble(
    code = c("BSY-321", "BSY-321", "BSY-301", "BSY-301", "BSY-321", "MFSY-500"),
    start = c(t(2025L, "Høst"), t(2024L, "Høst"), t(2021L, "Høst"), t(2022L, "Høst"), t(2024L, "Høst"), t(2025L, "Vår")),
    k1 = c(3L, 3L, 3L, 3L, 3L, 5L), k2 = c(3L, 3L, 3L, 3L, 3L, 6L),
    url = c("html25", "html24", "bis21.pdf", "bis22.pdf", "bis24.pdf", "mapra.pdf"),
    text = c(NA, NA, "a", "b", "c", "d"))
  code <- c("BSY-321", "BSY-321", "BSY-321", "BSY-301", "BSY-301", "MFSY-500", "MFSY-500", "MFSY-500", "BSY-999")
  term <- c(t(2026L, "Høst"), t(2025L, "Høst"), t(2024L, "Høst"), t(2022L, "Høst"), t(2025L, "Vår"),
            t(2027L, "Vår"), t(2027L, "Høst"), t(2026L, "Høst"), t(2025L, "Høst"))
  expect_equal(index$url[cohort_plan_match(code, term, index)], c(
    "html25",     # BIS25's third semester is autumn 2026
    "html24",     # the page before the PDF of the same cohort
    NA,           # before any cohort had it
    "bis21.pdf",
    "bis22.pdf",  # after its last cohort: the cohort that had it last
    "mapra.pdf", "mapra.pdf",   # semesters 5 and 6 of a cohort from spring 2025
    NA,           # the cohort reaches it in spring 2027
    NA))
})

test_that("LDH's config reads its cohort plans", {
  cfg <- get_institution_config("ldh")
  expect_equal(cfg$strategy, "cohort_plans")
  expect_equal(institution_from_code("8202"), "ldh")
  expect_equal(cfg$page_term("<p><strong>3. semester: </strong>høst 2026</p>"), semester_term(2026L, "Høst"))
  expect_equal(cfg$page_end_term("<p><strong>2. semester: </strong>vår 2026</p>"), semester_term(2026L, "Vår"))
  df <- tibble::tibble(institution = "ldh", course_code = "BSY-321", course_code_raw = "BSY-321",
                       year = 2026L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
})
