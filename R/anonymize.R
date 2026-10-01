# R/anonymize.R

#' Anonymize course text
#'
#' Removes PII (names, emails, phone numbers), dates, years, seasons,
#' and institution-specific boilerplate. Preserves case and paragraph structure.
#' Used on extracted_text (-> course_plan) and on section text (sections_raw).
#'
#' @param institution Character vector of institution short names.
#' @param text Character vector of raw text (extracted_text or a section).
#' @param .progress Passed to purrr::map2_chr for progress reporting.
#' @return Character vector of anonymized text. NA input -> NA output.
anonymize_text <- function(institution, text,
                           .progress = "Anonymizing text") {
  stopifnot(length(institution) == length(text))

  purrr::map2_chr(institution, text, \(inst, txt) {
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
    # Remove bare semicolons (2022 website rendering artifacts); preserve real punctuation
    stringr::str_remove_all("(?m)^\\s*;\\s*$") |>
    stringr::str_remove_all("(?<=\\s);(?=\\s)") |>
    # Strip "Emneansvarlig\n\nName" (label line + following name line)
    stringr::str_remove("(?m)^Emneansvarlig[ \\t]*\\n+\\p{Lu}[\\p{L} .,-]+(?=\\n|$)")
}

.anon_mf <- function(txt) {
  txt |>
    # Strip from Emneansvarlig to end (names + emails + marketing)
    stringr::str_remove("Emneansvarlig\\s*\\n[\\s\\S]*$") |>
    stringr::str_remove_all("Kontakt studieveileder\\s*") |>
    stringr::str_remove_all("Vis flere\\s*") |>
    # Exam dates block up to the learning outcomes, library notice (#228)
    stringr::str_remove("(?s)\\nEksamensdatoer\\n.*?(?=\\nLæringsutbytte\\n)") |>
    stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)?Tilgang til litteratur\\n.*?folkebibliotek\\.")
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
    # HTML pages head the staff list "Fagperson(er)"; the "Name (Role)" lines
    # under it are removed in .anon_generic() (#237)
    stringr::str_remove_all("(?m)^Fagperson\\(er\\)[ \\t]*\\n?") |>
    stringr::str_remove_all("Powered by TCPDF[^\n]*") |>
    stringr::str_remove_all("(?m)^\\s*side\\s+\\d+\\s*$")
}

.anon_steiner <- function(txt) {
  txt |>
    stringr::str_remove_all("(?m)^\\s*Side\\s+\\d+\\s+av\\s+\\d+\\s*$") |>
    # bare PDF page numbers (#248, #225)
    stringr::str_remove_all("(?m)^[ \\t]*\\d{1,3}[ \\t]*\\n")
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
    # Strip person name before parenthesized email: "Per Hansen (per.hansen@math.uio.no)"
    # Keeps organizational names + emails (handled by generic email removal)
    stringr::str_remove_all("\\p{Lu}\\p{Ll}+(?:\\s+\\p{Lu}\\p{Ll}+)+\\s*(?=\\([\\w.+-]+@)") |>
    # Generic exam-links tail "Mer om eksamen ved UiO ... Andre veiledninger" (#224)
    stringr::str_remove("(?s)\\n(?:Mer|Meir) om eksamen ved UiO\\b.*$|(?s)\\nMore about examinations at UiO\\b.*$")
}


# --- Generic anonymization (all institutions) ---

# Staff roles that follow a person's name in parentheses in staff lists.
.staff_roles <- c(
  "emneansvarlig", "emneansvarleg", "faglærer", "faglærar",
  "studiekoordinator", "praksiskoordinator", "emnekoordinator", "koordinator",
  "studieprogramleder", "studieprogramleiar", "programansvarlig",
  "instituttleder", "instituttleiar", "veileder", "rettleiar",
  "timelærer", "timelærar", "kontaktperson", "sensor"
)
# A whole line that is only a capitalised name (particles like "van" allowed)
# followed by "(Role)", optionally bulleted.
.staff_line_regex <- paste0(
  "(?m)^[ \\t]*[-•*]?[ \\t]*",
  "\\p{Lu}[\\p{L}.'’-]*(?:[ \\t]+(?:\\p{Lu}[\\p{L}.'’-]*|van|von|de|der|den|da|di|af))+",
  "[ \\t]*\\((?i:", paste(.staff_roles, collapse = "|"), ")\\)[ \\t]*(?:\\n|$)"
)
# A signature line "17.03.20 Ola Nordmann, dekan": optional date stamp, a
# capitalised name, then ", role" ending the line (usn approval stamps; #238).
# \h, not [ \t]: some stamps have a no-break space after the date.
.signature_roles <- c("dekan", "prodekan", "studiedekan", "instituttleder",
                      "instituttleiar", "rektor", "prorektor",
                      "studieprogramleder", "studieprogramleiar")
.signature_line_regex <- paste0(
  "(?m)^[\\h]*(?:\\d[\\d.\\h]*[\\h]+)?",
  "\\p{Lu}[\\p{L}.'’-]*(?:[\\h]+(?:\\p{Lu}[\\p{L}.'’-]*|van|von|de|der|den|da|di|af))+",
  "[\\h]*,[\\h]*(?i:", paste(.signature_roles, collapse = "|"), ")[\\h]*(?:\\n|$)"
)
.approved_by_name_regex <- paste0(
  "([Gg]odkjent av (?:[Dd]ekan(?:en)?|[Pp]rodekan|[Ii]nstitutt(?:nest)?leder|",
  "[Ss]tudieprogramleder))[ \\t]+\\p{Lu}\\p{Ll}+(?:[ \\t]+\\p{Lu}[\\p{L}-]+)+"
)

# "2023-2024" / "2023-24" (consecutive years) is an academic year and goes;
# a content range such as "1945-1970" stays (#230).
.drop_academic_year <- function(m) {
  y <- stringr::str_match(m, "(\\d{4})\\s?[-–]\\s?(\\d{2,4})")
  nxt <- as.integer(y[, 2]) + 1L
  m[!is.na(nxt) & (y[, 3] == nxt | y[, 3] == sprintf("%02d", nxt %% 100L))] <- ""
  m
}

.anon_generic <- function(txt) {
  txt |>
    # Remove "Sist hentet/henta fra/frå FS..." timestamp
    stringr::str_remove_all("Sist hent(?:et|a) fr(?:a|å) FS \\(Felles studentsystem\\)[^\n]*") |>
    # Remove email addresses, with their brackets: "Ta kontakt med (x@uio.no)" (#229)
    stringr::str_remove_all("\\s*\\(\\s*[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\s*\\)") |>
    stringr::str_remove_all("\\b[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\b") |>
    # Remove staff list lines "- Ola Nordmann (Emneansvarlig)" (#237)
    stringr::str_remove_all(.staff_line_regex) |>
    # Remove signature lines "17.03.20 Ola Nordmann, dekan" (#238)
    stringr::str_remove_all(.signature_line_regex) |>
    # Keep the role, drop the name: "Godkjent av dekan Ola Nordmann" (#237)
    stringr::str_replace_all(.approved_by_name_regex, "\\1") |>
    # Remove phone numbers: +47 XX XX XX XX, Tlf: XXXXXXXX, telefon: XX XX XX XX
    stringr::str_remove_all("(?i)(?:tlf|telefon)\\.?\\s*:?\\s*(?:\\+47\\s*)?\\d[\\d ]{6,}") |>
    stringr::str_remove_all("\\+47\\s*\\d[\\d ]{6,}") |>
    # Remove Norwegian date-time format: "12. feb. 2026 02:50:04"
    stringr::str_remove_all("\\d{1,2}\\.\\s*(?:jan|feb|mar|apr|mai|jun|jul|aug|sep|okt|nov|des)\\.?\\s*\\d{4}\\s*\\d{2}:\\d{2}(?::\\d{2})?") |>
    # Remove 2-digit season+year patterns (e.g. "Høst23", "Vår 22")
    stringr::str_remove_all("(?i)(høst|vår|haust|autumn|spring)\\s*\\d{2}\\b") |>
    # Remove season words in semester context (next to year or semester keywords)
    # e.g. "Høst 2024", "2024 Vår", "Undervisningssemester: Vår", "Semester: Autumn"
    # Must run BEFORE year removal so "Høst 2024" matches as a unit
    # Preserves "vår" meaning "our" in normal prose
    # also inflected / run together: "høsten 2025", "våren2026" (#227)
    stringr::str_remove_all("(?i)\\b(?:(?:høst|vår|haust|sommer)(?:en|a)?|autumn|spring|summer)\\s*\\d{4}\\b") |>
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
    stringr::str_replace_all("\\b\\d{4}\\s?[-–]\\s?\\d{2,4}\\b", .drop_academic_year) |>
    # Brackets emptied by the removals above: "Meld. St. 16 ()" (#229);
    # code such as "print()" has no space before and stays
    stringr::str_remove_all("[ \\t]+\\([ \\t]*\\)") |>
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
