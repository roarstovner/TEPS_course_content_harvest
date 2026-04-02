# R/anonymize.R

#' Anonymize fulltext to produce readable course_plan text
#'
#' Removes PII (names, emails, phone numbers), dates, years, seasons,
#' and institution-specific boilerplate. Preserves case and paragraph structure.
#'
#' @param institution_short Character vector of institution short names.
#' @param fulltext Character vector of raw fulltext.
#' @param .progress Passed to purrr::map2_chr for progress reporting.
#' @return Character vector of anonymized text. NA input -> NA output.
anonymize_fulltext <- function(institution_short, fulltext,
                               .progress = "Anonymizing fulltext") {
  stopifnot(length(institution_short) == length(fulltext))

  purrr::map2_chr(institution_short, fulltext, \(inst, txt) {
    if (is.na(txt) || !nzchar(txt)) return(NA_character_)

    txt <- .anon_institution(inst, txt)
    txt <- .anon_generic(txt)

    if (is.na(txt) || !nzchar(trimws(txt))) return(NA_character_)
    txt
  }, .progress = .progress)
}


# --- Institution-specific anonymization ---

.anon_institution <- function(inst, txt) {
  switch(inst,
    ntnu    = .anon_ntnu(txt),
    uit     = .anon_uit(txt),
    uia     = .anon_uia(txt),
    hiof    = .anon_hiof(txt),
    hivolda = .anon_hivolda(txt),
    inn     = .anon_inn(txt),
    mf      = .anon_mf(txt),
    oslomet = .anon_oslomet(txt),
    steiner = .anon_steiner(txt),
    uis     = .anon_uis(txt),
    usn     = .anon_usn(txt),
    uib     = .anon_uib(txt),
    nmbu    = .anon_nmbu(txt),
    uio     = .anon_uio(txt),
    txt
  )
}

.anon_ntnu <- function(txt) {
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

.anon_uit <- function(txt) {
  txt |>
    stringr::str_remove_all("Startsida\\s*\\n\\s*Emnekatalog\\s*") |>
    stringr::str_remove_all("Error rendering component\\s*") |>
    stringr::str_remove_all("Se timeplan\\s*") |>
    # Strip "Kontaktperson:" + name line
    stringr::str_remove_all("(?m)^Kontaktperson:?\\s*\\n[^\\n]*") |>
    # Strip "Foreleser:" + name line
    stringr::str_remove_all("(?m)^Foreleser:?\\s*\\n[^\\n]*")
}

.anon_uia <- function(txt) {
  txt |>
    # Remove breadcrumb
    stringr::str_remove("Forside\\s*>\\s*Studier\\s*>\\s*Emner\\s*>\\s*\\d{4}\\s*>\\s*(?:Høst|Vår|Haust)\\s+\\d{4}\\s*>\\s*") |>
    # Remove "(Høst/Vår/Haust YYYY)" from title
    stringr::str_remove_all("\\((Høst|Vår|Haust)\\s+\\d{4}\\)") |>
    # Strip "Emneansvarlig:\nName\n" (name is on the next line, before "Undervisningssemester:")
    stringr::str_remove_all("(?m)^Emneansvarlig:\\s*\\n[^\\n]+(?=\\n)")
}

.anon_hiof <- function(txt) {
  txt |>
    stringr::str_remove_all("Sist hentet fra FS[^\n]*") |>
    stringr::str_remove_all("Litteraturlista er sist oppdatert[^\n]*") |>
    # Strip "Emneansvarlig(e):" + name lines until next "Heading:" line
    stringr::str_remove("(?m)^Emneansvarlige?:\\s*\\n(?:(?![A-ZÆØÅ][\\w ]+:)[^\\n]*\\n?)*") |>
    # Insert missing space when heading runs into uppercase content
    stringr::str_replace_all(
      "(Kunnskap|Ferdigheter|Generell kompetanse|Kompetanse)(?=[A-ZÆØÅ])",
      "\\1 "
    )
}

.anon_inn <- function(txt) {
  # Filter placeholder/error pages as NA
  if (grepl("Emnesøket gjelder kun fra", txt, fixed = TRUE)) return(NA_character_)

  txt |>
    stringr::str_remove_all("NameCreditsDateComment") |>
    stringr::str_remove_all("(?m)^Name\\s*$") |>
    stringr::str_remove_all("(?m)^Credits\\s*$") |>
    stringr::str_remove_all("(?m)^Date\\s*$") |>
    stringr::str_remove_all("(?m)^Comment\\s*$") |>
    stringr::str_remove_all("Statusmelding\\s*\\n?Emnebeskrivelsen for valgt semester er ikke publisert enda\\.[^\n]*") |>
    stringr::str_remove_all("(?m)^\\d{4}\\s+(?:Høst|Vår|Autumn|Spring)(?:,\\s*\\d{4}\\s+(?:Høst|Vår|Autumn|Spring))*\\s*$") |>
    stringr::str_replace_all("\\bEngelsk\\b", "English")
}

.anon_oslomet <- function(txt) {
  if (grepl("Siden du leter etter finnes ikke", txt, fixed = TRUE)) return(NA_character_)

  txt |>
    stringr::str_replace_all(";", " ") |>
    # Strip "Emneansvarlig\n\nName" (label line + following name line)
    stringr::str_remove("(?m)^Emneansvarlig[ \\t]*\\n+\\p{Lu}[\\p{L} .,-]+(?=\\n|$)")
}

.anon_mf <- function(txt) {
  txt |>
    # Strip from Emneansvarlig to end (names + emails + marketing)
    stringr::str_remove("Emneansvarlig\\s*\\n[\\s\\S]*$") |>
    stringr::str_remove_all("Kontakt studieveileder\\s*") |>
    stringr::str_remove_all("Vis flere\\s*")
}

.anon_hivolda <- function(txt) {
  txt |>
    # Strip "Emneansvarleg:" + following name line
    stringr::str_remove("(?m)^Emneansvarleg:\\s*\\n[^\\n]*") |>
    # Strip "Godkjent av:" + following name line
    stringr::str_remove("(?m)^Godkjent av:\\s*\\n[^\\n]*")
}

.anon_uis <- function(txt) {
  txt |>
    # HTML pages: strip from "Kontakt" heading to end (names + version line)
    stringr::str_remove("(?m)^Kontakt\\s*\\n[\\s\\S]*$") |>
    stringr::str_remove_all("Emnebeskrivelsen er hentet fra[^\n]*") |>
    # PDF pages: strip "EMNE ... Versjon ..." header line
    stringr::str_remove_all("(?m)^\\s*EMNE\\s+\\S+\\s+\\S+\\s+Versjon[^\n]*") |>
    # PDF pages: strip "Fagpersoner" section (heading + name lines with roles)
    stringr::str_remove("(?s)Fagpersoner\\s*\\n.*?(?=\\n\\n|$)") |>
    stringr::str_remove_all("Powered by TCPDF[^\n]*") |>
    stringr::str_remove_all("(?m)^\\s*side\\s+\\d+\\s*$")
}

.anon_steiner <- function(txt) {
  txt |>
    stringr::str_remove_all("(?m)^\\s*Side\\s+\\d+\\s+av\\s+\\d+\\s*$")
}

.anon_usn <- function(txt) {
  txt |>
    stringr::str_remove_all("(?m)^Godkjent emneplan\\s*$") |>
    stringr::str_remove_all("(?m)^Godkjent\\s+\\d{1,2}\\.\\d{1,2}\\.\\d{4}[^\n]*")
}

.anon_uib <- function(txt) {
  txt |>
    # Strip "Studierettleiar:"/"Studieveileder:" + email/content lines
    stringr::str_remove_all("(?m)^Studierettleiar:?\\s*[^\n]*") |>
    stringr::str_remove_all("(?m)^Studieveileder:?\\s*[^\n]*") |>
    # Strip "Eksamensadministrasjon:" lines
    stringr::str_remove_all("(?m)^Eksamensadministrasjon:?\\s*[^\n]*") |>
    # Strip "Studierettleiar kan kontaktast her:" boilerplate
    stringr::str_remove_all("Studierettleiar kan kontaktast her:\\s*") |>
    # Strip "Kontakt:" section lines
    stringr::str_remove_all("(?m)^Kontakt:?\\s*[^\n]*")
}

.anon_nmbu <- function(txt) {
  txt |>
    # Strip "Emneansvarlig:Name" (colon directly followed by name, same line)
    stringr::str_remove_all("Emneansvarlig:?\\s*\\p{Lu}[\\p{L} .,-]+(?=\\n|$)")
}

.anon_uio <- function(txt) {
  txt |>
    # Strip person name before parenthesized email: "Tom Lindstrøm (lindstro@math.uio.no)"
    # Keeps organizational names + emails (handled by generic email removal)
    stringr::str_remove_all("\\p{Lu}\\p{Ll}+(?:\\s+\\p{Lu}\\p{Ll}+)+\\s*(?=\\([\\w.+-]+@)")
}


# --- Generic anonymization (all institutions) ---

.anon_generic <- function(txt) {
  txt |>
    # Remove "Sist hentet/henta fra/frå FS..." timestamp
    stringr::str_remove_all("Sist hent(?:et|a) fr(?:a|å) FS \\(Felles studentsystem\\)[^\n]*") |>
    # Remove email addresses
    stringr::str_remove_all("\\b[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\b") |>
    # Remove phone numbers: +47 XX XX XX XX, Tlf: XXXXXXXX, telefon: XX XX XX XX
    stringr::str_remove_all("(?i)(?:tlf|telefon)\\s*:?\\s*(?:\\+47\\s*)?\\d[\\d ]{6,}") |>
    stringr::str_remove_all("\\+47\\s*\\d[\\d ]{6,}") |>
    # Remove Norwegian date-time format: "12. feb. 2026 02:50:04"
    stringr::str_remove_all("\\d{1,2}\\.\\s*(?:jan|feb|mar|apr|mai|jun|jul|aug|sep|okt|nov|des)\\.?\\s*\\d{4}\\s*\\d{2}:\\d{2}(?::\\d{2})?") |>
    # Remove 2-digit season+year patterns (e.g. "Høst23", "Vår 22")
    stringr::str_remove_all("(?i)(høst|vår|haust|autumn|spring)\\s*\\d{2}\\b") |>
    # Remove season words in semester context (next to year or semester keywords)
    # e.g. "Høst 2024", "2024 Vår", "Undervisningssemester: Vår", "Semester: Autumn"
    # Must run BEFORE year removal so "Høst 2024" matches as a unit
    # Preserves "vår" meaning "our" in normal prose
    stringr::str_remove_all("(?i)(høst|vår|haust|autumn|spring|sommer|summer)\\s+(\\d{4})") |>
    stringr::str_remove_all("(?i)(\\d{4})\\s+(høst|vår|haust|autumn|spring|sommer|summer)") |>
    stringr::str_remove_all("(?i)(?<=(?:semester|undervisning|oppstart|startsemester|eksamen)[:\\s]{0,3})(høst|vår|haust|autumn|spring|sommer|summer)") |>
    # Remove dates: dd.mm.yyyy
    stringr::str_remove_all("\\d{1,2}\\.\\d{1,2}\\.\\d{4}") |>
    # Remove years in administrative contexts only (content years like "etter 1945" preserved)
    # Blanket year removal for dedup lives in normalize_plan_text()
    # Admin keywords + year: "Opprettet 2020" → "Opprettet", "Gyldig fra 2023" → "Gyldig fra"
    stringr::str_replace_all(
      "(?i)(opprettet|oppdatert|revidert|vedtatt|godkjent|gjeldende(?: fra)?|gyldig(?: fra)?|sist (?:endret|revidert|oppdatert))\\s+\\d{4}\\b",
      "\\1"
    ) |>
    # Remove academic year ranges: "2023/2024", "2023/24"
    stringr::str_remove_all("\\b\\d{4}/\\d{2,4}\\b") |>
    # Remove times HH:MM(:SS)
    stringr::str_remove_all("\\b\\d{1,2}:\\d{2}(?::\\d{2})?\\b") |>
    # Remove JS artifacts
    stringr::str_remove_all("function\\s*\\([^)]*\\)\\s*\\{[^}]*\\}") |>
    stringr::str_remove_all("\\$\\([^)]+\\)\\.[^;]+;") |>
    # Light whitespace cleanup: collapse 3+ newlines to 2, trim trailing spaces per line
    stringr::str_replace_all("(?m)[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}
