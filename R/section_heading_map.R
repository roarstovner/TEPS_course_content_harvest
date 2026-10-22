# R/section_heading_map.R
# Heading-to-section mapping table and matcher function for section extraction.

#' Heading pattern table
#'
#' Each row maps a pattern (substring) to a canonical section name, or to
#' ".drop" for headings whose text is not course content.
#' Patterns are matched case-insensitively against heading text.
#' Ordered by specificity (longer/more specific patterns first within each section)
#' so that the first match wins.
section_heading_patterns <- tibble::tribble(
  ~pattern,                                ~section,                 ~exact,
  # --- coursework_requirements ---
  # NB: placed BEFORE assessment on purpose. The matcher returns the first
  # substring match scanning top-to-bottom, so gate headings that also contain
  # "eksamen" (e.g. "Arbeidskrav - vilkår for å avlegge eksamen" (hiof),
  # "Vilkår for å gå opp til eksamen" (uis, uia)) must be tested before the
  # assessment "eksamen" pattern, otherwise they leak into assessment.
  "vilkår for å gå opp til eksamen",       "coursework_requirements", FALSE,
  "vilkår for å avlegge eksamen",          "coursework_requirements", FALSE,
  "vilkår for å framstille seg til eksamen", "coursework_requirements", FALSE,
  "faglige krav for å kunne avlegge eksamen", "coursework_requirements", FALSE,
  "obligatorisk læringsaktivitet",         "coursework_requirements", FALSE,
  "obligatorisk undervisningsaktivitet",   "coursework_requirements", FALSE,
  "obligatoriske aktiviteter",             "coursework_requirements", FALSE,
  "compulsory activity",                   "coursework_requirements", TRUE,   # usn English pages
  "fravær fra obligatorisk aktivitet",     "coursework_requirements", FALSE,  # uio label
  "obligatorisk aktivitet",                "coursework_requirements", FALSE,
  "compulsory activity and compulsory attendance", "coursework_requirements", TRUE, # usn English pages
  "compulsory learning activities",        "coursework_requirements", FALSE,  # hvl English pages
  "compulsory assignments",                "coursework_requirements", TRUE,   # ntnu English pages
  "obligatoriske arbeidskrav",             "coursework_requirements", TRUE,   # uit label
  "participation/compulsory work",         "coursework_requirements", TRUE,   # usn English pages
  "arbeidskrav",                           "coursework_requirements", FALSE,
  "studiekrav",                            "coursework_requirements", FALSE,
  "compulsory activities",                 "coursework_requirements", FALSE,
  "work requirements",                     "coursework_requirements", FALSE,
  "coursework requirements",               "coursework_requirements", FALSE,

  # --- .drop: exam logistics (before assessment, which "eksamen" would hit) ---
  # The codebook keeps assessment design and excludes exam logistics.
  # Most rows are exact (whole heading only), and exact matches win over
  # substrings, so "Eksamen og sensorordning" still maps to assessment.
  "undervisnings- og eksamensspråk",       ".drop",                  TRUE,   # uit 2018+ (language, facts)
  "eksamensdato",                          ".drop",                  FALSE,
  "dato for eksamen",                      ".drop",                  TRUE,   # uit 2008-2011
  "dato for skoleeksamen",                 ".drop",                  TRUE,
  "date for examination",                  ".drop",                  TRUE,
  "mer om eksamen ved uio",                ".drop",                  TRUE,
  "meir om eksamen ved uio",               ".drop",                  TRUE,
  "more about examinations at uio",        ".drop",                  TRUE,
  "adgang til ny eller utsatt eksamen",    ".drop",                  TRUE,
  "resit an examination",                  ".drop",                  TRUE,
  "ny/utsatt eksamen",                     ".drop",                  TRUE,
  "ny/utsett eksamen",                     ".drop",                  TRUE,
  "ny og utsatt eksamen",                  ".drop",                  TRUE,
  "ny og utsett eksamen",                  ".drop",                  TRUE,
  "ny eller utsatt eksamen",               ".drop",                  TRUE,
  "vilkår for ny/utsatt eksamen",          ".drop",                  TRUE,
  "syk på eksamen / utsatt eksamen",       ".drop",                  TRUE,
  "kontinuasjonseksamen",                  ".drop",                  TRUE,
  "dette bør du vite om eksamen",          ".drop",                  TRUE,
  "sensorordning",                         ".drop",                  TRUE,
  "sensurordning",                         ".drop",                  TRUE,   # uio
  "vurderingssemester",                    ".drop",                  TRUE,   # uib: exam timing
  # a table of contents ("innhold" would map it to course_content; hiof, uia)
  "innholdsfortegnelse",                   ".drop",                  TRUE,
  "innhaldsliste",                         ".drop",                  TRUE,   # uia nynorsk
  "innholdsliste",                         ".drop",                  TRUE,
  "table of contents",                     ".drop",                  TRUE,

  # --- assessment ---
  # Exam aids and exam language are assessment design. These headings stay
  # as a line of the section (`label` below).
  "hjelpemidler til eksamen",              "assessment",             FALSE,
  "hjelpemiddel til eksamen",              "assessment",             FALSE,
  "hjelpemidler ved eksamen",              "assessment",             FALSE,
  "hjelpemiddel ved eksamen",              "assessment",             FALSE,
  "tillatte hjelpemidler",                 "assessment",             FALSE,  # nla
  "tillatte hjelpemiddel",                 "assessment",             FALSE,
  "permitted aids under examination",      "assessment",             TRUE,   # nla English pages
  "hjelpemidler",                          "assessment",             TRUE,
  "hjelpemiddel",                          "assessment",             TRUE,
  "examination support material",          "assessment",             TRUE,   # uio
  "eksamensspråk",                         "assessment",             TRUE,   # uio, nla
  "language of examination",               "assessment",             TRUE,
  "assessment language",                   "assessment",             TRUE,   # nla English pages
  "vurdering og eksamen",                  "assessment",             FALSE,
  "avsluttende vurdering",                 "assessment",             FALSE,
  "final assessment",                      "assessment",             TRUE,   # nla English pages
  "mer om vurdering",                      "assessment",             FALSE,
  "vurderingsordning",                     "assessment",             FALSE,
  "vurderingsformer",                      "assessment",             FALSE,
  "forms of assessment",                   "assessment",             TRUE,   # usn English pages
  "vurderingsform",                        "assessment",             FALSE,
  "eksamensformer",                        "assessment",             FALSE,
  "vurdering",                             "assessment",             FALSE,
  "eksamen",                               "assessment",             FALSE,
  "assessment methods",                    "assessment",             FALSE,
  "form of assessment",                    "assessment",             FALSE,
  "examination",                           "assessment",             FALSE,
  "assessment",                            "assessment",             TRUE,
  "exam description",                      "assessment",             TRUE,   # nord English pages
  "further on evaluation",                 "assessment",             TRUE,   # ntnu English pages
  "sensurering",                           "assessment",             TRUE,   # uio: weighting

  # --- prerequisites ---
  "krav til forkunnskaper",                "prerequisites",          FALSE,
  "anbefalte forkunnskaper",               "prerequisites",          FALSE,
  "forkunnskapskrav",                      "prerequisites",          FALSE,
  "forkunnskaper",                         "prerequisites",          FALSE,
  "forkunnskap",                           "prerequisites",          FALSE,
  "forkrav",                               "prerequisites",          FALSE,
  "progresjonskrav",                       "prerequisites",          FALSE,  # nla
  "progression requirements",              "prerequisites",          TRUE,   # nla English pages
  "required prerequisite knowledge",       "prerequisites",          FALSE,
  "recommended prerequisite knowledge",    "prerequisites",          FALSE,
  "formal prerequisite knowledge",         "prerequisites",          TRUE,
  "recommended previous knowledge",        "prerequisites",          TRUE,
  "required previous knowledge",           "prerequisites",          TRUE,   # ntnu English pages
  # hvl English pages: their "Krav til forkunnskapar". Exact, since elsewhere
  # "entry requirements" can be admission text.
  "entry requirements",                    "prerequisites",          TRUE,
  "prerequisites",                         "prerequisites",          FALSE,

  # --- .drop: headings that end a section but carry no course content ---
  # Text under them is discarded by .clean_sections(). Admission and
  # study-right headings: the codebook excludes admission text from
  # prerequisites. Placed after prerequisites so a mixed heading such as
  # "Opptakskrav og forkunnskaper" stays prerequisites.
  "opptakskrav",                           ".drop",                  FALSE,
  "opptak til emnet",                      ".drop",                  FALSE,
  "admission to the course",               ".drop",                  FALSE,
  "hvem kan ta dette emnet",               ".drop",                  FALSE,
  "krav til studierett",                   ".drop",                  FALSE,
  # Programme list in the page footer (usn, hivolda).
  "emnet inngår i følgende studier",       ".drop",                  FALSE,
  "course is part of these studies",       ".drop",                  TRUE,   # usn English pages
  "emnet inngår i følgande studieprogram", ".drop",                  FALSE,
  # Admin blocks (uis, usn): evaluation, staff, who may take the
  # course, overlapping courses, approval stamp, change log.
  "emneevaluering",                        ".drop",                  TRUE,
  "fagperson(er)",                         ".drop",                  TRUE,
  "fagpersoner",                           ".drop",                  TRUE,
  "fagpersonar",                           ".drop",                  TRUE,
  "åpent for",                             ".drop",                  TRUE,
  "åpen for",                              ".drop",                  TRUE,
  "ope for",                               ".drop",                  TRUE,
  "overlapping",                           ".drop",                  TRUE,
  "fakta",                                 ".drop",                  TRUE,
  "kontakt",                               ".drop",                  TRUE,
  "ansvarlig enhet",                       ".drop",                  TRUE,   # ntnu, uit: the unit
  "responsible unit",                      ".drop",                  TRUE,
  "department with academic responsibility", ".drop",                TRUE,   # ntnu English pages
  # usn approval stamp and change log
  "godkjent emneplan",                     ".drop",                  TRUE,
  "approved course plan",                  ".drop",                  TRUE,   # usn English pages
  "endringsbeskrivelse",                   ".drop",                  TRUE,
  "change description",                    ".drop",                  TRUE,   # usn English pages
  "utgifter i emnet",                      ".drop",                  TRUE,   # usn
  "undervisningssted",                     ".drop",                  TRUE,   # uio labels
  "eventuelle utgifter i forbindelse med undervisning", ".drop",    TRUE,
  "arbeidsmengd",                          ".drop",                  TRUE,   # uis workload boilerplate
  "arbeidsmengde",                         ".drop",                  TRUE,

  # --- learning_outcomes ---
  "læringsutbytte",                        "learning_outcomes",      FALSE,
  "læringsmål",                            "learning_outcomes",      FALSE,
  "lærer du",                              "learning_outcomes",      FALSE,
  # læringsutbytte sub-headings (knowledge/skills/general competence) — these
  # appear as their own heading nodes at some institutions (e.g. inn's
  # div.label), so map them to learning_outcomes rather than dropping them.
  # NB: ordered AFTER prerequisites so "forkunnskap" still wins over "kunnskap".
  "generell kompetanse",                   "learning_outcomes",      FALSE,
  "kunnskaper",                            "learning_outcomes",      FALSE,
  "kunnskapar",                            "learning_outcomes",      FALSE,
  "kunnskap",                              "learning_outcomes",      FALSE,
  "ferdigheter",                           "learning_outcomes",      FALSE,
  "ferdigheiter",                          "learning_outcomes",      FALSE,
  "ferdighetsmål",                         "learning_outcomes",      FALSE,  # inn
  "ferdigheitsmål",                        "learning_outcomes",      FALSE,
  "learning outcomes",                     "learning_outcomes",      FALSE,
  "learning outcome",                      "learning_outcomes",      FALSE,
  "objectives of the course",              "learning_outcomes",      TRUE,
  "objective of the course",               "learning_outcomes",      TRUE,   # uit 2008-2011
  "mål",                                   "learning_outcomes",      TRUE,
  # English sub-headings (inn English plans). Exact, so "Prerequisite
  # knowledge" and the like are not caught.
  "knowledge",                             "learning_outcomes",      TRUE,
  "skills",                                "learning_outcomes",      TRUE,
  "general competence",                    "learning_outcomes",      FALSE,

  # --- teaching_methods ---
  "arbeids- og undervisningsformer",       "teaching_methods",       FALSE,
  "undervisnings- og arbeidsformer",       "teaching_methods",       FALSE,
  "undervisnings- og læringsformer",       "teaching_methods",       FALSE,
  "læringsformer og aktiviteter",          "teaching_methods",       FALSE,
  "læringsaktiviteter og undervisningsmetoder", "teaching_methods",  FALSE,
  "undervisningsformer",                   "teaching_methods",       FALSE,
  "undervisningsform",                     "teaching_methods",       TRUE,   # uit 2008-2011
  "undervisningsopplegg",                  "teaching_methods",       FALSE,
  "læringsaktiviteter",                    "teaching_methods",       FALSE,
  "læringsformer",                         "teaching_methods",       FALSE,
  "undervisingsformer",                    "teaching_methods",       FALSE,
  # Whole phrases too, so <p> sub-headings (exact match only) are caught (mf).
  "arbeidsform og organisering",           "teaching_methods",       FALSE,
  "arbeidsform",                           "teaching_methods",       FALSE,
  "arbeidsmåter",                          "teaching_methods",       FALSE,
  "arbeidsmåtar",                          "teaching_methods",       FALSE,
  "praktisk organisering",                 "teaching_methods",       FALSE,
  # uia: what students do with the subject during placement. Practicum is a
  # teaching method in the codebook, so praksis stays there by default.
  "faget i praksis",                       "teaching_methods",       FALSE,
  "praksis",                               "teaching_methods",       TRUE,
  "supervised professional training",      "teaching_methods",       TRUE,   # usn English pages
  "practice",                              "teaching_methods",       TRUE,   # nla English pages
  "gjennomføring av praksis",              "teaching_methods",       TRUE,
  "undervisningstider",                    "teaching_methods",       TRUE,   # nmbu
  "læringsstøtte",                         "teaching_methods",       TRUE,
  "undervisning",                          "teaching_methods",       TRUE,
  "teaching",                              "teaching_methods",       TRUE,
  "teaching and working methods",          "teaching_methods",       FALSE,
  "teaching and learning activities",      "teaching_methods",       FALSE,
  "teaching and learning methods",         "teaching_methods",       TRUE,   # nla English pages
  "teaching methods",                      "teaching_methods",       FALSE,
  "teaching and organization",             "teaching_methods",       FALSE,
  "learning activities",                   "teaching_methods",       TRUE,   # nord English pages
  "learning methods and activities",       "teaching_methods",       TRUE,   # ntnu English pages
  # uit: teaching and language, then an h3 "Pensum" (reading_list)
  "undervisning og pensum",                "teaching_methods",       TRUE,

  # --- reading_list ---
  "pensumlitteratur",                      "reading_list",           FALSE,
  "pensum",                                "reading_list",           FALSE,
  # nla English pages: their "Pensum" (a reading list, not course content)
  "syllabus",                              "reading_list",           TRUE,
  "kursmateriell",                         "reading_list",           FALSE,
  "lesestoff",                             "reading_list",           FALSE,
  "læremidler",                            "reading_list",           FALSE,
  "litteratur",                            "reading_list",           FALSE,
  "reading list",                          "reading_list",           FALSE,
  "recommended reading",                   "reading_list",           FALSE,  # uit 2008-2011
  "course materials",                      "reading_list",           TRUE,   # ntnu English pages

  # --- course_content ---
  "mål og innhold",                        "course_content",         FALSE,
  "emnets innhold",                        "course_content",         FALSE,
  "innhald og oppbygging",                 "course_content",         FALSE,
  "beskrivelse av emnet",                  "course_content",         FALSE,
  "detaljert emnebeskrivelse",             "course_content",         FALSE,  # usn, under "Annet"
  "emnebeskrivelse",                       "course_content",         FALSE,
  "kort om emnet",                         "course_content",         FALSE,
  "om dette emnet",                        "course_content",         FALSE,
  "om studiet",                            "course_content",         FALSE,
  "faginnhold",                            "course_content",         FALSE,
  "innhold",                               "course_content",         FALSE,
  "innhald",                               "course_content",         FALSE,
  "innledning",                            "course_content",         FALSE,
  "innleiing",                             "course_content",         FALSE,
  "introduksjon",                          "course_content",         TRUE,
  "introduction",                          "course_content",         TRUE,   # nla English pages
  "om emnet",                              "course_content",         TRUE,   # uit
  "about the course",                      "course_content",         TRUE,
  "course content",                        "course_content",         FALSE,
  "course description",                    "course_content",         TRUE,   # nord English pages
  "content",                               "course_content",         FALSE
)

# Headings that name one part of their section (exam aids, exam language, rules
# for absence) stay as a line of it (sectionize()), since the text under them
# ("Ingen", "Alle", "Norsk.") says nothing without them.
section_heading_patterns$label <- section_heading_patterns$pattern %in% c(
  "hjelpemidler til eksamen", "hjelpemiddel til eksamen", "hjelpemidler ved eksamen",
  "hjelpemiddel ved eksamen", "tillatte hjelpemidler", "tillatte hjelpemiddel",
  "hjelpemidler", "hjelpemiddel", "examination support material", "eksamensspråk",
  "language of examination", "permitted aids under examination", "assessment language",
  "fravær fra obligatorisk aktivitet")


#' Match a heading text to a canonical section name
#'
#' Performs case-insensitive substring matching against the pattern table.
#' Returns the section name for the first matching pattern, or NA if no match.
#'
#' @param heading Character string — the heading text to match.
#' @param word_start If TRUE, a substring pattern must start at a word
#'   boundary, so "elevkunnskap" does not match "kunnskap" (the text reader
#'   uses this on plain-text lines).
#' @return Character string — canonical section name, or NA_character_.
match_heading_to_section <- function(heading, word_start = FALSE) {
  section_heading_patterns$section[heading_pattern(heading, word_start)]
}

# Whether a heading (with or without a closing colon) is a `label` heading;
# heading_pattern() only for a heading that holds a label pattern.
.is_label_heading <- function(heading) {
  h <- sub("[:：]\\s*$", "", tolower(trimws(heading)))
  lab <- section_heading_patterns$pattern[section_heading_patterns$label]
  any(stringr::str_detect(h, stringr::fixed(lab))) &&
    isTRUE(section_heading_patterns$label[heading_pattern(h)])
}

#' The row of section_heading_patterns a heading matches
#'
#' An exact match first, then the first substring pattern in table order;
#' NA when none matches. Used by match_heading_to_section() and to count how
#' often each pattern is used (target `heading_use`).
#'
#' @inheritParams match_heading_to_section
#' @return Integer row index, or NA_integer_.
heading_pattern <- function(heading, word_start = FALSE) {
  if (is.na(heading) || !nzchar(trimws(heading))) return(NA_integer_)
  heading_lower <- tolower(trimws(heading))

  eq <- which(section_heading_patterns$pattern == heading_lower)
  if (length(eq)) return(eq[1])

  # Denylist: language/expression metadata fields whose text collides with a
  # real pattern (e.g. "Undervisnings- og eksamensspråk" contains "eksamen")
  # but which are NOT the section (nla json; inn's "Language of
  # instruction and examination" would hit "examination"). A bare
  # "Eksamensspråk" is the exact .drop row above.
  if (grepl("eksamensspråk|vurderingsspråk|language of instruction", heading_lower)) {
    return(NA_integer_)
  }

  exact <- section_heading_patterns$exact %||% rep(FALSE, nrow(section_heading_patterns))
  for (i in seq_len(nrow(section_heading_patterns))) {
    if (isTRUE(exact[i])) next
    pattern <- section_heading_patterns$pattern[i]
    hit <- if (word_start) {
      grepl(paste0("(?<!\\p{L})\\Q", pattern, "\\E"), heading_lower, perl = TRUE)
    } else {
      grepl(pattern, heading_lower, fixed = TRUE)
    }
    if (hit) return(i)
  }
  NA_integer_
}
