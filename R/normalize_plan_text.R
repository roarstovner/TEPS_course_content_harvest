# R/normalize_plan_text.R

#' Normalize fulltext for deduplication
#'
#' Removes year-varying noise (dates, teacher names, JS artifacts, etc.)
#' while preserving substantive course plan content.
#'
#' @param institution_short Character vector of institution short names.
#' @param fulltext Character vector of raw fulltext.
#' @param .progress Passed to purrr::map2_chr for progress reporting.
#' @return Character vector of normalized text. NA input -> NA output.
normalize_plan_text <- function(institution_short, fulltext, .progress = "Normalize plan texts") {
  stopifnot(length(institution_short) == length(fulltext))

  purrr::map2_chr(institution_short, fulltext, \(inst, txt) {
    if (is.na(txt) || !nzchar(txt)) return(NA_character_)

    # Phase 1: Institution-specific pre-processing
    txt <- .pre_normalize(inst, txt)

    # Phase 2: Generic normalization
    txt <- .normalize_generic(txt)

    txt
  }, .progress = .progress)
}


#' Generate a content hash for normalized plan text
#'
#' @param normalized_text Character vector of normalized text (from normalize_plan_text).
#' @return Character vector of hash strings. NA input -> NA output.
build_plan_id <- function(normalized_text, .progress = "Building plan IDs") {
  purrr::map_chr(normalized_text, \(txt) {
    if (is.na(txt) || !nzchar(txt)) return(NA_character_)
    digest::digest(txt, algo = "sha256", serialize = FALSE)
  })
}


# --- Institution-specific pre-processing ---

.pre_normalize <- function(inst, txt) {
  switch(inst,
    ntnu    = .pre_ntnu(txt),
    uit     = .pre_uit(txt),
    uia     = .pre_uia(txt),
    hiof    = .pre_hiof(txt),
    inn     = .pre_inn(txt),
    mf      = .pre_mf(txt),
    oslomet = .pre_oslomet(txt),
    txt
  )
}

.pre_ntnu <- function(txt) {
  txt |>
    # Strip from Kontaktinformasjon to end (teacher names, exam dates, JS, timetable)
    stringr::str_remove("Kontaktinformasjon[\\s\\S]*$") |>
    # Strip header boilerplate
    stringr::str_remove_all("course-details-portlet\\s*") |>
    stringr::str_remove_all('moment\\.locale\\([^)]+\\);?\\s*') |>
    stringr::str_remove_all("Velg studieår\\s*") |>
    stringr::str_remove_all("Studieår \\d{4}/\\d{4}\\s*") |>
    stringr::str_remove_all("Undervisningsstart[^\n]+") |>
    # Strip LMS links and misc
    stringr::str_remove_all("Blackboard\\s*-\\s*\\S+") |>
    stringr::str_remove_all("Andre sider om emnet\\s*") |>
    stringr::str_remove_all("Alt om eksamen ved NTNU\\s*")
}

.pre_uit <- function(txt) {
  txt |>
    stringr::str_remove_all("Startsida\\s*\\n\\s*Emnekatalog\\s*") |>
    stringr::str_remove_all("Error rendering component\\s*") |>
    stringr::str_remove_all("Se timeplan\\s*")
}

.pre_uia <- function(txt) {
  txt |>
    # Remove breadcrumb: "Forside > Studier > Emner > YYYY > Høst YYYY > TITLE"
    # Ends at second occurrence of course code pattern or title in parens
    stringr::str_remove("Forside\\s*>\\s*Studier\\s*>\\s*Emner\\s*>\\s*\\d{4}\\s*>\\s*(?:Høst|Vår|Haust)\\s+\\d{4}\\s*>\\s*") |>
    # Remove "(Høst/Vår/Haust YYYY)" from title
    stringr::str_remove_all("\\((Høst|Vår|Haust)\\s+\\d{4}\\)")
}

.pre_hiof <- function(txt) {
  txt |>
    stringr::str_remove_all("Sist hentet fra FS[^\n]*") |>
    stringr::str_remove_all("Litteraturlista er sist oppdatert[^\n]*") |>
    # Insert missing space when heading runs into uppercase content (e.g. "KunnskapStudenten")
    stringr::str_replace_all(
      "(Kunnskap|Ferdigheter|Generell kompetanse|Kompetanse)(?=[A-ZÆØÅ])",
      "\\1 "
    )
}

.pre_inn <- function(txt) {
  # Filter placeholder/error pages as NA
  if (grepl("Emnesøket gjelder kun fra", txt, fixed = TRUE)) return(NA_character_)

  # Field labels in both Norwegian and English to strip
  inn_labels <- c(
    # Norwegian
    "Startsemestre", "Startsemester", "Emnekode", "Studiepoeng",
    "Undervisningssemestre", "Undervisnings- og eksamensspråk",
    "Undervisningssted", "Forkunnskapskrav", "Anbefalte forkunnskaper",
    "Krav til forkunnskaper",
    "Emnets innhold", "Læringsutbytte", "Kunnskap", "Ferdigheter",
    "Generell kompetanse", "Arbeids- og undervisningsformer",
    "Eksamen", "Vurderingsordning", "Vurderingsformer",
    "Pensum", "Arbeidskrav", "Emneansvarlig", "Sist revidert",
    # English
    "Course code", "Number of credits", "Teaching semester",
    "Language of instruction and examination",
    "Campus", "Required prerequisite knowledge",
    "Course content", "Learning outcomes", "Learning outcome",
    "Knowledge", "Skills",
    "General competence", "Teaching and working methods",
    "Examination", "Reading list", "Course coordinator",
    "Compulsory activities", "Work requirements",
    "Assessment methods", "Teaching and learning activities",
    "Obligatoriske aktiviteter"
  )

  for (label in inn_labels) {
    txt <- gsub(label, "", txt, fixed = TRUE)
  }

  txt |>
    # Remove status banner
    stringr::str_remove("Statusmelding\\s*\\n?Emnebeskrivelsen for valgt semester er ikke publisert enda\\.[^\n]*") |>
    # Remove semester value lines (e.g. "2023 Spring, 2024 Spring") before season stripping
    stringr::str_remove_all("(?m)^\\d{4}\\s+(?:Høst|Vår|Autumn|Spring)(?:,\\s*\\d{4}\\s+(?:Høst|Vår|Autumn|Spring))*\\s*$") |>
    # Normalize language metadata value: "Engelsk" -> "English"
    stringr::str_replace_all("\\bEngelsk\\b", "English")
}

.pre_oslomet <- function(txt) {
  # Filter error pages as NA
  if (grepl("Siden du leter etter finnes ikke", txt, fixed = TRUE)) return(NA_character_)

  txt |>
    # Replace semicolons with spaces - OsloMet 2022 pages used semicolons as
    # section separators, causing false diffs with other years. Replace (not
    # remove) to avoid concatenating words that had no surrounding space.
    stringr::str_replace_all(";", " ")
}

.pre_mf <- function(txt) {
  txt |>
    # Strip from Emneansvarlig to end (names + emails + marketing)
    stringr::str_remove("Emneansvarlig\\s*\\n[\\s\\S]*$") |>
    # Also strip "Kontakt studieveileder" and "Vis flere" nav links
    stringr::str_remove_all("Kontakt studieveileder\\s*") |>
    stringr::str_remove_all("Vis flere\\s*")
}


# --- Generic normalization (all institutions) ---

.normalize_generic <- function(txt) {
  txt |>
    # Remove "Sist hentet/henta fra/frå FS..." timestamp (UiA, HiOF) - before date patterns
    stringr::str_remove_all("Sist hent(?:et|a) fr(?:a|å) FS \\(Felles studentsystem\\)[^\n]*") |>
    # Remove Norwegian date-time format: "12. feb. 2026 02:50:04" - before year removal
    stringr::str_remove_all("\\d{1,2}\\.\\s*(?:jan|feb|mar|apr|mai|jun|jul|aug|sep|okt|nov|des)\\.?\\s*\\d{4}\\s*\\d{2}:\\d{2}(?::\\d{2})?") |>
    # Remove dates: dd.mm.yyyy
    stringr::str_remove_all("\\d{1,2}\\.\\d{1,2}\\.\\d{4}") |>
    # Remove 4-digit years
    stringr::str_remove_all("\\b(19|20)\\d{2}\\b") |>
    # Remove times HH:MM(:SS)
    stringr::str_remove_all("\\b\\d{1,2}:\\d{2}(?::\\d{2})?\\b") |>
    # Remove "Emneansvarlig(e): Name" (captures name up to next known heading)
    # Use \\p{L} (Unicode letter) to match accented names like Müller, Gjesdal
    stringr::str_remove_all("Emneansvarlige?:?\\s*\\p{Lu}[\\p{L} .,-]+(?=\\s+(?:Undervisning|Varighet|Studiepoeng|Semester|Faglærer|Ansvarlig|$))") |>
    # Remove "Faglærer(e): Name" (same approach)
    stringr::str_remove_all("Faglærer[e]?:?\\s*\\p{Lu}[\\p{L} .,-]+(?=\\s+(?:Undervisning|Varighet|Studiepoeng|Semester|Ansvarlig|$))") |>
    # Remove "Godkjent av: Name" (same approach)
    stringr::str_remove_all("Godkjent av:?\\s*\\p{Lu}[\\p{L} .,-]+(?=\\s+(?:Undervisning|Varighet|Studiepoeng|Semester|Kunnskap|Læringsutbytte|Ansvarlig|$))") |>
    # Remove JS artifacts
    stringr::str_remove_all("function\\s*\\([^)]*\\)\\s*\\{[^}]*\\}") |>
    stringr::str_remove_all("\\$\\([^)]+\\)\\.[^;]+;") |>
    # Case-fold to lowercase (fixes USN KUNNSKAP/Kunnskap etc.)
    tolower() |>
    # Normalize heading synonyms (USN renamed "Vurderingsformer" → "Eksamensformer" in 2025)
    stringr::str_replace_all("\\beksamensformer\\b", "vurderingsformer") |>
    # Remove 2-digit season+year patterns (e.g. "Høst23", "Vår 22") - before standalone season removal
    stringr::str_remove_all("(høst|vår|haust|autumn|spring)\\s*\\d{2}\\b") |>
    # Remove standalone semester words
    stringr::str_remove_all("\\b(høst|vår|haust|autumn|spring|sommer|summer)\\b") |>
    # Normalize whitespace
    stringr::str_squish()
}
