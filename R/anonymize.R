# R/anonymize.R

#' Anonymize course text
#'
#' Removes PII (names, emails, phone numbers), dates, years, seasons,
#' and institution-specific boilerplate. Preserves case and paragraph structure.
#' Used on extracted_text (-> course_plan) and on section text (plan_sections).
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
    hvl     = .anon_hvl(txt),
    txt
  )
}

.anon_ntnu <- function(txt) {
  txt |>
    # The contact block (course coordinator, teachers) up to "Ansvarlig
    # enhet" or the exam tab; the unit stays. The timetable tab is not read.
    stringr::str_remove("(?ms)^Kontaktinformasjon\\n.*?(?=^(?:Ansvarlig enhet|Eksamen)$|\\z)") |>
    # Strip header boilerplate
    stringr::str_remove_all("course-details-portlet\\s*") |>
    stringr::str_remove_all("Undervisningsstart[^\n]+") |>
    # Strip LMS links and misc
    stringr::str_remove_all("Blackboard\\s*-\\s*\\S+") |>
    stringr::str_remove_all("Andre sider om emnet\\s*")
}

# HTML written out as text in a plan's fields (uit, uis reading-list notes:
# "<p>Kapittel 3</p>", "Kap. 6,7, 9 &amp; 10"): the tags go, a line break
# stays one, and the entities are decoded. Not "<http://...>" links.
.literal_markup <- function(txt) {
  txt |>
    stringr::str_replace_all("(?i)<br\\s*/?>|</(?:p|div)>", "\n") |>
    stringr::str_remove_all("(?i)<(?:p|div|span|strong|em|b|i|u)\\b[^<>\n]*>|</(?:span|strong|em|b|i|u)>") |>
    stringr::str_replace_all(c("&nbsp;" = " ", "&aring;" = "å", "&oslash;" = "ø", "&aelig;" = "æ",
                               "&Aring;" = "Å", "&Oslash;" = "Ø", "&AElig;" = "Æ", "&lt;" = "<",
                               "&gt;" = ">", "&quot;" = "\"", "&amp;" = "&"))
}

.anon_uit <- function(txt) {
  txt |>
    .literal_markup() |>
    # a plan component that failed to render: post_fn cuts the fulltext there,
    # but sections read past it (1,013 sections on 2026-10-05)
    stringr::str_remove_all("Error rendering component\\s*") |>
    stringr::str_remove_all("Se timeplan\\s*") |>
    # An exam part's date with its label ("Innlevering: 15.05.2023 14:00",
    # "Dato: 02.12.2024 –06.12.2024", "Utlevering:"); some semesters have
    # none, and the table cell is then empty
    stringr::str_remove_all(paste0("(?:Innlevering|Utlevering|Dato):[ \\t]*\\d{1,2}\\.(?:\\d{1,2}\\.\\d{4}|[ \\t]*\\p{L}+)",
                                   "(?:[ \\t]*[-–][ \\t]*\\d{1,2}\\.\\d{1,2}\\.\\d{4})?(?:[ \\t]+\\d{1,2}:\\d{2})? *")) |>
    # Strip "Kontaktperson:" + name line
    stringr::str_remove_all("(?m)^Kontaktperson:?\\s*\\n[^\\n]*") |>
    # Strip "Foreleser:" + name line
    stringr::str_remove_all("(?m)^Foreleser:?\\s*\\n[^\\n]*") |>
    # 2008-2011 layout: the closing timetable block ("Undervisning Høst 2010"
    # + teachers glued to their roles, rooms, first lectures)
    stringr::str_remove("(?s)(?:^|\\n)Undervisning (?:Høst|Vår|Haust) \\d{4}.*$")
}

.anon_hvl <- function(txt) {
  txt |>
    # a campus label with a name: the contact for exam aids
    stringr::str_remove_all(paste0("(?m)^(?:Bergen|Stord|Haugesund|Sogndal|Førde)[ \\t]*:[ \\t]*",
                                   .name_regex, "[ \\t]*(?:\\n|$)")) |>
    # "Emnet tilbys ikke studieåret 2025/2026.", "Emnet vert ikkje tilbydd
    # 2025/2026.": without the year it would read "not offered"
    stringr::str_remove_all(paste0("(?m)^Emnet (?:tilbys ikke|vert ikkje tilbydd) (?:studieåret )?",
                                   "\\d{4}/\\d{4}\\.?[ \\t]*(?:\\n|$)"))
}

.anon_uia <- function(txt) {
  txt |>
    # Remove breadcrumb
    stringr::str_remove("Forside\\s*>\\s*Studier\\s*>\\s*Emner\\s*>\\s*\\d{4}\\s*>\\s*(?:Høst|Vår|Haust)\\s+\\d{4}\\s*>\\s*") |>
    # Remove "(Høst/Vår/Haust YYYY)" from title
    stringr::str_remove_all("\\((Høst|Vår|Haust)\\s+\\d{4}\\)") |>
    # "Emneansvarlig(e):" and the name lines under it, up to the next
    # "Label:" line (Undervisningssemester:); several names are common
    stringr::str_remove_all("(?m)^Emneansvarlige?:[ \\t]*\\n(?:(?![\\p{Lu}][\\p{L} ()-]*:[ \\t]*$)[^\\n]*\\n)*")
}

.anon_hiof <- function(txt) {
  txt |>
    stringr::str_remove_all("Sist hentet fra FS[^\n]*") |>
    # "Litteraturlista er sist oppdatert ...", "Litteraturliste sist oppdatert"
    stringr::str_remove_all("(?m)^[ \\t]*Litteratur\\w*(?: er)? sist oppdater[^\n]*") |>
    # Strip "Emneansvarlig(e):" + name lines until next "Heading:" line
    stringr::str_remove("(?m)^Emneansvarlige?:\\s*\\n(?:(?![A-ZÆØÅ][\\w ]+:)[^\\n]*\\n?)*")
}

.anon_inn <- function(txt) {
  # Filter placeholder/error pages as NA
  if (grepl("Emnesøket gjelder kun fra", txt, fixed = TRUE)) return(NA_character_)
  # "... ikke publisert enda. Her er den siste versjonen vi har tilgjengelig.":
  # the page shows a later plan, not the requested semester's
  if (grepl("Emnebeskrivelsen for valgt semester er ikke publisert enda", txt, fixed = TRUE))
    return(NA_character_)

  txt |>
    stringr::str_remove_all("(?m)^\\d{4}\\s+(?:Høst|Vår|Autumn|Spring)(?:,\\s*\\d{4}\\s+(?:Høst|Vår|Autumn|Spring))*\\s*$")
}

.anon_oslomet <- function(txt) {
  if (grepl("Siden du leter etter finnes ikke", txt, fixed = TRUE)) return(NA_character_)

  txt |>
    # and so is a run of them before a space ("Klasseledelse -;;;; Klasseledelse
    # og fag", "1);;; Kropp og læring")
    stringr::str_remove_all(";{2,}(?=[ \\t])") |>
    # A ";" not followed by a space is a 2021-2022 rendering artifact for a
    # space or line break ("En;viktig del;av", "frister.;Det", "1;000 ord",
    # "fordypning;;;"); a real semicolon has a space after it
    stringr::str_remove_all("(?m)^[ \\t]*;++(?![ \\t])") |>
    stringr::str_replace_all("[ \\t]*;++(?![ \\t])[ \\t]*", " ") |>
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
    # Exam dates block up to the learning outcomes, library notice
    stringr::str_remove("(?s)\\nEksamensdatoer\\n.*?(?=\\nLæringsutbytte\\n)") |>
    # with the link labels before it; the list itself is on another page
    # (colon and closing "MERK: Sjekk at litteraturlisten ..." since 2026)
    stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\n.*?folkebibliotek\\.(?:\\s*MERK: Sjekk at litteraturlisten[^\\n]*)?")
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
    .literal_markup() |>
    # HTML pages: strip from "Kontakt" heading to end (names + version line)
    stringr::str_remove("(?m)^Kontakt\\s*\\n[\\s\\S]*$") |>
    stringr::str_remove_all("Emnebeskrivelsen er hentet fra[^\n]*") |>
    # PDF pages: strip "EMNE ... Versjon ..." header line
    stringr::str_remove_all("(?m)^\\s*EMNE\\s+\\S+\\s+\\S+\\s+Versjon[^\n]*") |>
    # and the stamp "LENG145_1 Bokmål Versjon 20.Februar.2023"
    stringr::str_remove_all("(?m)^[ \\t]*\\S+_\\d+[ \\t]+(?:Bokmål|Nynorsk|Engelsk|English)[ \\t]+Versjon[^\n]*(?:\n|$)") |>
    # PDF pages: strip "Fagpersoner" section (heading + name lines with roles)
    stringr::str_remove("(?s)Fagpersoner\\s*\\n.*?(?=\\n\\n|$)") |>
    # HTML pages head the staff list "Fagperson(er)"; the "Name (Role)" lines
    # under it are removed in .anon_generic()
    stringr::str_remove_all("(?m)^Fagperson\\(er\\)[ \\t]*\\n?") |>
    stringr::str_remove_all("Powered by TCPDF[^\n]*") |>
    # PDF running header "Emne LENG116_1, BOKMÅL, 2015 HØST, versjon ..."
    stringr::str_remove_all("(?m)^[ \\t]*Emne [A-ZÆØÅ0-9_-]+, (?:BOKMÅL|NYNORSK|ENGELSK),[^\n]*(?:\n|$)") |>
    # PDF running footer "LENG116_1 - Innføring i britisk og irsk litteratur",
    # the course's own code and title
    stringr::str_remove_all("(?m)^[ \\t]*\\p{Lu}[\\p{Lu}0-9-]*\\d[\\p{Lu}0-9-]*_\\d+ - [^\n]*(?:\n|$)") |>
    stringr::str_remove_all("(?m)^\\s*side\\s+\\d+\\s*$")
}

.anon_steiner <- function(txt) {
  txt |>
    # bare PDF page numbers, also the last line
    stringr::str_remove_all("(?m)^[ \\t]*\\d{1,3}[ \\t]*(?:\\n|$)")
}

.anon_usn <- function(txt) {
  txt |>
    stringr::str_remove_all("(?m)^Godkjent emneplan\\s*$") |>
    stringr::str_remove_all("(?m)^Godkjent\\s+\\d{1,2}\\.\\d{1,2}\\.\\d{4}[^\n]*") |>
    # "Godkjent av" with the approver's name and title on the next lines
    stringr::str_remove_all("(?m)^Godkjent av[ \\t]*\\n[^\\n]*(?:\\n(?:Institutt|Fakultet|Studie|Prorektor|Dekan)[^\\n]*)?") |>
    # "Godkjent\nNotodden 06.03.20\nKari Nordmann (instituttleder)": the campus
    # and date line and the approver's name line under it
    stringr::str_replace_all(paste0("(?m)^(Godkjent)[ \\t]*\\n[^\\n]*\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}[ \\t]*",
                                    "(?:\\n[ \\t]*", .name_regex, "[^\\n]*)?(?=\\n|$)"), "\\1") |>
    # Leganto reading-list widget: link label, "View online", and the
    # item type glued to the title ("BookMøter med barnelitteratur")
    stringr::str_remove_all("Click to view interactive reading list in Leganto") |>
    stringr::str_remove_all("(?m)^[ \\t]*View online[ \\t]*(?:\\n|$)") |>
    stringr::str_remove_all(paste0(
      "(?m)^(?:Book Chapter|Book Extract|Electronic Book|E-book|Electronic Article|",
      "Newspaper Article|Audio-Visual Document|Government Document|Legal Document|Legislation|",
      "Technical Report|Working Paper|Conference Paper|Conference|Presentation|Dissertation|",
      "Streamed Video|Video|Audio Recording|Music|Text Resource|Research Dataset|Database|",
      "Reference Entry|Computer Program|Anthology|Journal|Series|Review|Poem|Map|Note|Other|",
      "Book|Article|Website|Document|Thesis|Report)",
      # a title, quote, dash, bullet or link; lowercase too ("ArticleiPed",
      # "Book Chapterkapittel 10"), but not a word going on ("Booking",
      # "Mappeinnlevering", "Videoen", "Articles:", "Documentation")
      "(?=[\\p{Lu}\\d\"«(“”‘’'–•-]|(?<!Book|Map|Video|Note|Music|Poem|Review|Report|Series|",
      "Thesis|Conference|Presentation|Database|Anthology)(?!s(?!\\p{L})|ation|ist|wise|ing)\\p{Ll})"))
}

.anon_uib <- function(txt) {
  txt |>
    # Strip "Studierettleiar:"/"Studieveileder:" + email/content lines
    stringr::str_remove_all("(?m)^Studierettleiar:?\\s*[^\n]*") |>
    stringr::str_remove_all("(?m)^Studieveileder:?\\s*[^\n]*") |>
    # Strip "Eksamensadministrasjon:" lines
    stringr::str_remove_all("(?m)^Eksamensadministrasjon:?\\s*[^\n]*") |>
    # Strip "Kontakt:" section lines
    stringr::str_remove_all("(?m)^Kontakt:?\\s*[^\n]*") |>
    # When the reading list is published, a date that changes between years:
    # "Litteraturlista vil vere klar innan 01.06. for haustsemesteret og
    # 01.12. for vårsemesteret.", "Litteraturlistene vil vere klare innan 1. juni"
    stringr::str_remove_all(paste0("Litteraturlist\\w*(?: for [\\p{L} ]{1,30}?)? vil ver[ae] klare? inn[ae]n ",
                                   "(?:\\d\\.(?=\\d|[ \\t]+\\p{Ll})|[^.\n])*\\.[ \\t]*"))
}

.anon_nmbu <- function(txt) {
  txt |>
    # the label of a link to the AI categories, under "Om bruk av KI"
    stringr::str_remove_all("(?m)^[ \\t]*Her finner du KI-kategoriene beskrevet\\.?[ \\t]*(?:\n|$)") |>
    # Strip "Emneansvarlig:Name" (colon directly followed by name, same line)
    stringr::str_remove_all("Emneansvarlig:?\\s*\\p{Lu}[\\p{L} .,-]+(?=\\n|$)")
}

.anon_uio <- function(txt) {
  txt |>
    # Strip person name before parenthesized email: "Per Hansen (per.hansen@math.uio.no)"
    # Keeps organizational names + emails (handled by generic email removal)
    stringr::str_remove_all("\\p{Lu}\\p{Ll}+(?:\\s+\\p{Lu}\\p{Ll}+)+\\s*(?=\\([\\w.+-]+@)") |>
    # Generic exam-links tail "Mer om eksamen ved UiO ... Andre veiledninger",
    # up to the facts box
    stringr::str_remove(paste0("(?s)\\n(?:(?:Mer|Meir) om eksamen ved UiO|More about examinations at UiO)\\b",
                               ".*?(?=\\n+(?:Fakta om emnet|Facts about this course)\\n|$)"))
}


# --- Generic anonymization (all institutions) ---

# Staff roles that follow a person's name in parentheses in staff lists.
.staff_roles <- c(
  "emneansvarlig", "emneansvarleg", "faglærer", "faglærar",
  "emneleder", "emneleiar",
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
# A role label followed by a capitalised name, on the same line or (after a
# colon) the next, and possibly an e-mail: "Emneleder Kari Nordmann
# kari@uit.no" (uit), "Emneansvarlig Ola Nordmann" (uis PDF plans),
# "Emneansvarlige:\nKari Nordmann" (uia). "Emneansvarlig informerer ..." is prose, and uib's
# "Emneansvarlig\nThe Programme Committee" names no person.
.role_name_line_regex <- paste0(
  "(?m)^[ \\t]*(?i:", paste(.staff_roles, collapse = "|"), ")(?i:e|er|ar)?",
  "[ \\t]*(?::[ \\t]*\\n[ \\t]*|:?[ \\t]+)",   # the next line only after a colon
  "\\p{Lu}\\p{Ll}+(?:[ \\t]+\\p{Lu}[\\p{L}.'’-]*)+[^\\n]*(?:\\n|$)"
)
# A signature line "17.03.20 Ola Nordmann, dekan": optional date stamp, a
# capitalised name, then ", role" ending the line (usn approval stamps).
# \h, not [ \t]: some stamps have a no-break space after the date.
.signature_roles <- c("dekan", "prodekan", "studiedekan", "instituttleder",
                      "instituttleiar", "rektor", "prorektor",
                      "studieprogramleder", "studieprogramleiar",
                      "instituttnestleder", "instituttnestleiar")
.signature_line_regex <- paste0(
  "(?m)^[\\h]*(?:\\d[\\d.\\h]*[\\h]+)?",
  "\\p{Lu}[\\p{L}.'’-]*(?:[\\h]+(?:\\p{Lu}[\\p{L}.'’-]*|van|von|de|der|den|da|di|af))+",
  "[\\h]*,[\\h]*(?i:", paste(.signature_roles, collapse = "|"), ")[\\h]*(?:\\n|$)"
)
# A capitalised name, with initials and hyphens ("Ola K. Nordmann",
# "Anne-Kari Nordmann-Dahl").
.name_part <- "\\p{Lu}[\\p{Ll}'’]+(?:-\\p{Lu}?[\\p{Ll}'’]+)*"
.name_regex <- paste0(.name_part, "(?:[ \\t]+\\p{Lu}\\.)*(?:[ \\t]+", .name_part, ")+")
# A role word followed by names anywhere in a line: "... avklares med
# faglærer Ola Nordmann" (usn), "Godkjent emneplan av dekan Ola Nordmann"
# (usn), "Studiepoeng 15 Emneansvarlig Ola Nordmann" (uis PDF), "Ta kontakt
# med Ola K. Nordmann eller Kari Nordmann" (uio), "Kontaktpersoner er A, B og
# C" (uit). The role stays, the names go.
.role_name_inline_regex <- paste0(
  "(?<!\\p{L})((?i:", paste(c(.staff_roles, .signature_roles), collapse = "|"),
  ")(?i:e|er|ar|en)?:?|(?i:kontakt(?:e|er)? med|kontaktpersone?r? er))[ \\t]+",
  .name_regex, "(?:[ \\t]*(?:,|og|eller)[ \\t]+", .name_regex, ")*"
)
# A name line followed by a line that is only a leader's role:
# "Kari Nordmann\nInstituttnestleder" (usn approval blocks). Other roles
# head the next line ("The Programme Committee\nEmneansvarlig", uib).
.name_role_lines_regex <- paste0(
  "(?m)^[ \\t]*", .name_regex, "[ \\t]*\\n[ \\t]*(?i:", paste(.signature_roles, collapse = "|"),
  ")[ \\t]*(?:\\n|$)"
)
.approved_by_name_regex <- paste0(
  "([Gg]odkjent av (?:[Dd]ekan(?:en)?|[Pp]rodekan|[Ii]nstitutt(?:nest)?leder|",
  "[Ss]tudieprogramleder))[ \\t]+\\p{Lu}\\p{Ll}+(?:[ \\t]+\\p{Lu}[\\p{L}-]+)+"
)
# PDF text wraps long links anywhere: `s` with an optional line break
# between any two characters.
.wrapped <- function(s) paste(strsplit(s, "")[[1]], collapse = "\\n?")
# The reader's IP address in library OpenURL links: "...&user_ip=10.16.56.125&"
# (uis reading lists).
.user_ip_regex <- paste0(.wrapped("user_ip="), "[\\d.\\n]*\\d")
# The library's OpenURL link itself, up to its "View online" label: hundreds
# of characters of rft.* parameters (uis). It holds no space, so it never
# runs into the next reference.
.openurl_regex <- paste0("https?://\\S*exlibrisgroup[^ \\t]*?", .wrapped("View"),
                         "\\s+", .wrapped("online"))

# Government documents: laws, regulations, white papers, propositions, NOUs,
# curricula, conventions and what ministries and directorates publish. Their
# dates and session years are content and stay (user decision 2026-10-08): "Meld. St. 21 (2016–2017)", "Ny opplæringslov fra 01.08.24",
# "fastsatt av Kunnskapsdepartementet 7. juni 2016".
.gov_doc_regex <- paste0(
  "(?:(?i:lov(?:en|a|er|ene|data|verk|verket)?s?(?!\\p{L})|forskrift|departement|",
  "direktorat|storting|regjering|meld\\.[ \\t]?st|st\\.[ \\t]?meld|[os]t\\.[ \\t]?prp|",
  "prop\\.[ \\t]?\\d|innst\\.[ \\t]?\\d|rundskriv|rammeplan|læreplan|kunnskapsløftet|",
  "nasjonale retningslinjer|udir|konvensjon|forente nasjoner|regler|reglement)|NOU)")
# The date stays when such a document is named up to 80 characters before it
# on the same line. The guard goes after a date rule's first token: after the
# whole date it looked back from the date's end and stopped at a line break
# inside it; at the very start of a rule it ran at every character.
.not_gov <- paste0("(?<!", .gov_doc_regex, "[^\\n]{0,80})")
# A date rule `first` + `rest` with the guard between them, run only where
# the rest follows.
.not_gov_rule <- function(first, rest) paste0(first, "(?=", rest, ")", .not_gov, rest)

# "2023-2024" / "2023/24" (consecutive years from 1990) is an academic year
# and goes; a content range such as "1945-1970", "1349-50" or a reprint
# "(1980/2005)" stays.
.drop_academic_year <- function(m) {
  y <- stringr::str_match(m, "(\\d{4})\\s?[-–—/]\\s?(\\d{2,4})")
  from <- as.integer(y[, 2])
  nxt <- from + 1L
  m[!is.na(nxt) & from >= 1990L & (y[, 3] == nxt | y[, 3] == sprintf("%02d", nxt %% 100L))] <- ""
  m
}

# A season word, also inflected or in a compound: "høsten", "Vårsemesteret".
.season_regex <- "(?:(?:høst|vår|haust|sommer)(?:semester(?:et)?|en|a)?|autumn|spring|summer)"

# A long-form date "7. juni 2016"; in a list the earlier ones may lack the
# year ("3. juli 2017, 19. april og 7. juni 2018").
.month_regex <- paste0("(?i:januar|februar|mars|april|mai|juni|juli|august|september|",
                       "oktober|november|desember|january|february|march|may|june|july|october|december)")
.long_date_regex <- paste0("\\d{1,2}\\.?[ \\t]*", .month_regex)
.long_dates_regex <- paste0("(?:", .long_date_regex, "(?:[ \\t]*\\d{4})?[ \\t]*(?:,|og|and)[ \\t]*)*",
                            .long_date_regex, "[ \\t]*\\d{4}")
# A full stop ending the line, removed with the date before it.
.eol_stop <- "(?:\\.(?=[ \\t]*(?:\\n|$)))?"
# The space before a removed date, season or e-mail goes with it, so no
# "Faget går ." or "studieadministrasjonen , før" is left.
.gap <- "[ \\t]*"
.clock_regex <- "(?:[01]\\d|2[0-3]):[0-5]\\d(?::[0-5]\\d)?"
# Approval and update dates in words: "godkjent i studieutvalget 14.
# november 2016", "endringer lagt inn 3. juli 2017 og 7. mai 2024"
.approval_date_regex <- paste0(
  "((?<!\\p{L})(?i:godkjent|vedtatt|revidert|revisjon|foretatt|lagt inn|oppdatert|",
  "gjort tilgjengelig|sist endret)", .not_gov, "[^\\n.]{0,100}?)", .gap, "\\b", .long_dates_regex, .eol_stop)

.anon_generic <- function(txt) {
  txt |>
    # Remove "Sist hentet/henta fra/frå FS..." timestamp
    stringr::str_remove_all("Sist hent(?:et|a) fr(?:a|å) FS \\(Felles studentsystem\\)[^\n]*") |>
    # Remove email addresses, with their brackets: "Ta kontakt med (x@uio.no)"
    stringr::str_remove_all("\\s*\\(\\s*[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\s*\\)") |>
    stringr::str_remove_all(paste0(.gap, "\\b[\\w.+-]+@[\\w.-]+\\.[a-zA-Z]{2,}\\b")) |>
    stringr::str_remove_all(.openurl_regex) |>
    stringr::str_remove_all(.user_ip_regex) |>
    # Remove staff list lines "- Ola Nordmann (Emneansvarlig)"
    stringr::str_remove_all(.staff_line_regex) |>
    # Remove role + name lines "Emneleder Ola Nordmann"
    stringr::str_remove_all(.role_name_line_regex) |>
    # Remove signature lines "17.03.20 Ola Nordmann, dekan"
    stringr::str_remove_all(.signature_line_regex) |>
    # and "Ola Nordmann\nInstituttnestleder"
    stringr::str_remove_all(.name_role_lines_regex) |>
    # Keep the role, drop the name: "Godkjent av dekan Ola Nordmann"
    stringr::str_replace_all(.approved_by_name_regex, "\\1") |>
    # "faglærer Ola Nordmann" anywhere in a line
    stringr::str_replace_all(.role_name_inline_regex, "\\1") |>
    # Remove phone numbers: +47 XX XX XX XX, Tlf: XXXXXXXX, telefon: XX XX XX XX
    stringr::str_remove_all("(?i)(?:tlf|telefon)\\.?\\s*:?\\s*(?:\\+47\\s*)?\\d[\\d ]{6,}") |>
    stringr::str_remove_all("\\+47\\s*\\d[\\d ]{6,}") |>
    # Remove Norwegian date-time format: "12. feb. 2026 02:50:04"
    stringr::str_remove_all("\\d{1,2}\\.\\s*(?:jan|feb|mar|apr|mai|jun|jul|aug|sep|okt|nov|des)\\.?\\s*\\d{4}\\s*\\d{2}:\\d{2}(?::\\d{2})?") |>
    # Date and year rules keep a government document's dates (.not_gov).
    # Remove 2-digit season+year patterns (e.g. "Høst23", "Vår 22"), not
    # credits ("Høst 10 stp")
    stringr::str_remove_all(.not_gov_rule(paste0(.gap, "(?i:høst|vår|haust|autumn|spring)"),
      "\\s*\\d{2}\\b(?![ \\t]*(?i:stp|sp|studiepoeng|ects|credits)\\b)")) |>
    # Remove season words in semester context (next to year or semester keywords)
    # e.g. "Høst 2024", "2024 Vår", "Undervisningssemester: Vår", "Semester: Autumn"
    # Must run BEFORE year removal so "Høst 2024" matches as a unit
    # Preserves "vår" meaning "our" in normal prose
    # also inflected / run together: "høsten 2025", "våren2026",
    # "høstsemesteret 2019"; with a dash, comma or colon before it and
    # a list after it ("15 stp, høst 2019 og vår 2020", "Naturfag 5-10 -
    # høst 2020", "MERK: Våren 2021: Skriftlig") and a colon closing the line
    # ("Høst 2024:")
    stringr::str_remove_all(.not_gov_rule(
      paste0("(?:[ \\t]*[-–,:])?", .gap, "\\b(?i:", .season_regex, ")"),
      paste0("\\s*\\d{4}\\b(?:[ \\t]*(?:,|og|and|[-–/])[ \\t]*(?i:", .season_regex, ")\\s*\\d{4}\\b)*",
             "(?::(?=[ \\t]*(?:\\n|$)))?"))) |>
    stringr::str_remove_all(.not_gov_rule(paste0(.gap, "\\d{4}"), "\\s+(?i:høst|vår|haust|autumn|spring|sommer|summer)")) |>
    # after a semester label: the whole word and a list ("Vår og haust",
    # "Haustsemesteret"), not the start of another word ("Våre", "Høstpraksis")
    #; a glued next word ("HaustEmnet") starts with a capital
    stringr::str_remove_all(paste0(
      "(?<=(?i:semester|undervisning|oppstart|startsemester|eksamen)[:\\s]{0,3})(?i:",
      .season_regex, "(?:[ \\t]*(?:,|og|and|eller|or|/|&)[ \\t]*", .season_regex,
      ")*)(?!\\p{Ll})[.,]?[ \\t]*")) |>
    # Semester codes: "Pensumliste for H2025-V2026", "Engelskdidaktikk - H23",
    # "(MG1NA1) H2018 Notodden", "H23: Undervisningen ..."
    stringr::str_remove_all(.not_gov_rule(
      "[ \\t]*[-–]?[ \\t]*\\b[HV]", "(?:19|20)?\\d{2}(?:[-/][HV](?:19|20)?\\d{2})?\\b:?")) |>
    # Approval and update dates in words, twice for a second date on a
    # stamp line ("godkjent av utdanningsutvalget 3. mai 2018 og på fullmakt
    # av leder i utdanningsutvalget 4. mars 2019")
    stringr::str_replace_all(.approval_date_regex, "\\1") |>
    stringr::str_replace_all(.approval_date_regex, "\\1") |>
    # dd.mm.yy or month and year after an approval: "Godkjent 9.3.21.",
    # "Emneplan godkjent 25.03.25", "Godkjent av instituttleder 18.10.22",
    # "Godkjent desember 2017." (usn), and any zero-padded one ("Oppsatt
    # skoleeksamen 16.12.20"), not in a file name ("rapport-24.06.16.pdf")
    stringr::str_replace_all(paste0(
      "((?<!\\p{L})(?i:godkjent)", .not_gov, "(?:[ \\t]+(?i:emneplan|av [^\\n]{0,30}?))?:?)[ \\t]+",
      "(?:\\d{1,2}\\.\\d{1,2}\\.\\d{2}(?![\\d/]|\\.\\w)|", .month_regex, "[ \\t]+\\d{4}\\b)", .eol_stop), "\\1") |>
    # A change-log entry's date with its colon or dash: "November 2020:
    # Endring i forkunnskapskrav", "November 2021 - Endret på ...",
    # "27.08.2020: Endring ..." (usn, hiof); not "April 1940:" in a timeline
    stringr::str_remove_all(paste0(
      "(?m)^[ \\t]*(?:", .month_regex, "[ \\t]+20\\d{2}|\\d{1,2}\\.\\d{1,2}\\.(?:\\d{4}|\\d{2}))",
      "[ \\t]*(?::|[-–](?=[ \\t]))[ \\t]*")) |>
    stringr::str_remove_all(.not_gov_rule(
      paste0(.gap, "(?<![\\d./])(?:0[1-9]|[12]\\d|3[01])"), paste0("\\.(?:0[1-9]|1[0-2])\\.\\d{2}(?![\\d/]|\\.\\w)", .eol_stop))) |>
    # Remove dates: dd.mm.yyyy; not inside a path or number, not a
    # regulation's date ("forskrift 28.10.2020 nr. 2474"), and not a
    # reference's publication date after a comma ("Razsviet, 4.11.1923";
    # "SSB, 07.06.2023,"); a range is one date ("27.05.2024 –31.05.2024")
    stringr::str_remove_all(.not_gov_rule(paste0(.gap, "(?<![\\d./]|,[ \\t])\\d{1,2}"), paste0(
      "\\.\\d{1,2}\\.\\d{4}(?:[ \\t]*[-–][ \\t]*\\d{1,2}\\.\\d{1,2}\\.\\d{4})?",
      "(?![\\d/]|[ \\t]*nr\\.?[ \\t]*\\d)"))) |>
    # Remove years in administrative contexts only (content years like "etter 1945" preserved)
    # Blanket year removal for dedup lives in normalize_plan_text()
    # Admin keywords + year: "Opprettet 2020" → "Opprettet", "Gyldig fra 2023" → "Gyldig fra"
    stringr::str_replace_all(.not_gov_rule(paste0(
      "((?i:opprettet|oppdatert|revidert|vedtatt|godkjent|gjeldende(?: fra)?|gyldig(?: fra)?|",
      "sist (?:endret|revidert|oppdatert)))"), "\\s+\\d{4}\\b"), "\\1") |>
    # Remove academic year ranges: "2023/2024", "2023/24", "2019 — 2020" (nla);
    # not year paths in links ("files/2019/06/27/", "dokumentserien/2020-2021/")
    # or DOIs, or ISO dates ("2001-02-01")
    stringr::str_replace_all(.not_gov_rule(paste0(.gap, "(?<![/\\w.])\\d{4}"), "[ \\t]?/[ \\t]?\\d{2,4}\\b(?!/|[-–]\\d)"),
                             .drop_academic_year) |>
    stringr::str_replace_all(.not_gov_rule(paste0(.gap, "(?<![/\\w.])\\d{4}"), "\\s?[-–—]\\s?\\d{2,4}\\b(?!/|[-–]\\d)"),
                             .drop_academic_year) |>
    # Brackets emptied by the removals above: "Meld. St. 16 ()";
    # code such as "print()" has no space before and stays
    stringr::str_remove_all("[ \\t]+\\([ \\t]*\\)") |>
    # Remove clock times "09:00", "23:59:59"; not running times ("Lengde:
    # 2:19", "[20:28]", "Mer Film, 2:30, 2022,", "1:00:09"), verses ("Sure 3:30"),
    # volume:pages, ISBN-10 or ratios ("60:40"); a range is one time
    # ("14:30-15:30")
    stringr::str_remove_all(paste0(.gap, "(?<!\\[|Total Pages |[\\d:])", .clock_regex,
                                   "(?:[ \\t]*[-–][ \\t]*", .clock_regex, ")?\\b",
                                   "(?!\\]|,[ \\t]*(?:19|20)\\d{2}\\b)")) |>
    # Remove JS artifacts
    stringr::str_remove_all("function\\s*\\([^)]*\\)\\s*\\{[^}]*\\}") |>
    stringr::str_remove_all("\\$\\([^)]+\\)\\.[^;]+;") |>
    # Light whitespace cleanup: collapse 3+ newlines to 2, trim trailing spaces per line
    stringr::str_replace_all("(?m)[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}


# --- Check of the shareable files ---

# What the anonymizer removes, as patterns to look for in its output. A hit in
# data/processed/ means a text there did not pass through anonymize_text().
.personal_data_patterns <- c(
  email     = "[\\w.+-]+@[\\w-]+(?:\\.[\\w-]+)*\\.[a-zA-Z]{2,}",
  phone     = "(?i)(?:\\+47[ \\t]*\\d[\\d \\t]{6,}|(?:tlf|telefon)\\.?[ \\t]*:?[ \\t]*\\d[\\d \\t]{6,})",
  ip        = .user_ip_regex,
  staff     = .staff_line_regex,
  role_name = .role_name_line_regex,
  role_name_inline = .role_name_inline_regex,
  name_role_lines = .name_role_lines_regex,
  signature = .signature_line_regex
)

#' Personal data left in data files
#'
#' @param files RDS files holding a data frame; every character column is
#'   scanned.
#' @return Tibble `file`, `column`, `pattern`, `n` (rows with a match) and
#'   `example` (the first match); no rows when nothing is found.
personal_data_in <- function(files) {
  found <- list()
  for (f in files) {
    df <- readRDS(f)
    for (col in names(df)[vapply(df, is.character, logical(1))]) {
      for (p in names(.personal_data_patterns)) {
        hit <- which(grepl(.personal_data_patterns[[p]], df[[col]], perl = TRUE))
        if (length(hit) == 0) next
        found[[length(found) + 1]] <- tibble::tibble(
          file = f, column = col, pattern = p, n = length(hit),
          example = stringr::str_extract(df[[col]][hit[1]], .personal_data_patterns[[p]]))
      }
    }
  }
  dplyr::bind_rows(tibble::tibble(file = character(), column = character(),
                                  pattern = character(), n = integer(),
                                  example = character()), found)
}
