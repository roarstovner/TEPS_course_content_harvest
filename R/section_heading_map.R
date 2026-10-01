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
  # assessment "eksamen" pattern, otherwise they leak into assessment (#198).
  "vilkår for å gå opp til eksamen",       "coursework_requirements", FALSE,
  "vilkår for å avlegge eksamen",          "coursework_requirements", FALSE,
  "vilkår for å framstille seg til eksamen", "coursework_requirements", FALSE,
  "faglige krav for å kunne avlegge eksamen", "coursework_requirements", FALSE,
  "obligatorisk læringsaktivitet",         "coursework_requirements", FALSE,
  "obligatorisk undervisningsaktivitet",   "coursework_requirements", FALSE,
  "obligatoriske aktiviteter",             "coursework_requirements", FALSE,
  "obligatorisk aktivitet",                "coursework_requirements", FALSE,
  "arbeidskrav",                           "coursework_requirements", FALSE,
  "studiekrav",                            "coursework_requirements", FALSE,
  "compulsory activities",                 "coursework_requirements", FALSE,
  "work requirements",                     "coursework_requirements", FALSE,
  "coursework requirements",               "coursework_requirements", FALSE,

  # --- .drop: exam logistics (before assessment, which "eksamen" would hit) ---
  # The codebook keeps assessment design and excludes exam logistics (#215).
  # Most rows are exact (whole heading only), and exact matches win over
  # substrings, so "Eksamen og hjelpemidler" still maps to assessment.
  "eksamensdato",                          ".drop",                  FALSE,
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
  "hjelpemidler til eksamen",              ".drop",                  TRUE,
  "hjelpemiddel til eksamen",              ".drop",                  TRUE,
  "hjelpemidler ved eksamen",              ".drop",                  TRUE,
  "hjelpemiddel ved eksamen",              ".drop",                  TRUE,
  "examination support material",          ".drop",                  TRUE,
  "language of examination",               ".drop",                  TRUE,
  "dette bør du vite om eksamen",          ".drop",                  TRUE,
  "hjelpemidler",                          ".drop",                  TRUE,
  "hjelpemiddel",                          ".drop",                  TRUE,
  "tillatte hjelpemidler",                 ".drop",                  TRUE,
  "tillatte hjelpemiddel",                 ".drop",                  TRUE,
  "sensorordning",                         ".drop",                  TRUE,

  # --- assessment ---
  "vurdering og eksamen",                  "assessment",             FALSE,
  "avsluttende vurdering",                 "assessment",             FALSE,
  "mer om vurdering",                      "assessment",             FALSE,
  "vurderingsordning",                     "assessment",             FALSE,
  "vurderingsformer",                      "assessment",             FALSE,
  "vurderingsform",                        "assessment",             FALSE,
  "eksamensformer",                        "assessment",             FALSE,
  "vurdering",                             "assessment",             FALSE,
  "eksamen",                               "assessment",             FALSE,
  "assessment methods",                    "assessment",             FALSE,
  "form of assessment",                    "assessment",             FALSE,
  "examination",                           "assessment",             FALSE,

  # --- prerequisites ---
  "krav til forkunnskaper",                "prerequisites",          FALSE,
  "anbefalte forkunnskaper",               "prerequisites",          FALSE,
  "forkunnskapskrav",                      "prerequisites",          FALSE,
  "forkunnskaper",                         "prerequisites",          FALSE,
  "forkunnskap",                           "prerequisites",          FALSE,
  "forkrav",                               "prerequisites",          FALSE,
  "required prerequisite knowledge",       "prerequisites",          FALSE,
  "recommended prerequisite knowledge",    "prerequisites",          FALSE,
  "formal prerequisite knowledge",         "prerequisites",          TRUE,
  "recommended previous knowledge",        "prerequisites",          TRUE,
  "prerequisites",                         "prerequisites",          FALSE,

  # --- .drop: headings that end a section but carry no course content ---
  # Text under them is discarded by .clean_sections(). Admission and
  # study-right headings (#211): the codebook excludes admission text from
  # prerequisites. Placed after prerequisites so a mixed heading such as
  # "Opptakskrav og forkunnskaper" stays prerequisites.
  "opptakskrav",                           ".drop",                  FALSE,
  "opptak til emnet",                      ".drop",                  FALSE,
  "admission to the course",               ".drop",                  FALSE,
  "hvem kan ta dette emnet",               ".drop",                  FALSE,
  "krav til studierett",                   ".drop",                  FALSE,
  # Programme list in the page footer (usn, hivolda).
  "emnet inngår i følgende studier",       ".drop",                  FALSE,
  "emnet inngår i følgande studieprogram", ".drop",                  FALSE,

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
  "learning outcomes",                     "learning_outcomes",      FALSE,
  "learning outcome",                      "learning_outcomes",      FALSE,
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
  # teaching method in the codebook (#201 keeps praksis there by default).
  "faget i praksis",                       "teaching_methods",       FALSE,
  "undervisning",                          "teaching_methods",       TRUE,
  "teaching",                              "teaching_methods",       TRUE,
  "teaching and working methods",          "teaching_methods",       FALSE,
  "teaching and learning activities",      "teaching_methods",       FALSE,
  "teaching methods",                      "teaching_methods",       FALSE,
  "teaching and organization",             "teaching_methods",       FALSE,

  # --- reading_list ---
  "pensumlitteratur",                      "reading_list",           FALSE,
  "pensum",                                "reading_list",           FALSE,
  "kursmateriell",                         "reading_list",           FALSE,
  "lesestoff",                             "reading_list",           FALSE,
  "læremidler",                            "reading_list",           FALSE,
  "litteratur",                            "reading_list",           FALSE,
  "reading list",                          "reading_list",           FALSE,

  # --- course_content ---
  "mål og innhold",                        "course_content",         FALSE,
  "emnets innhold",                        "course_content",         FALSE,
  "innhald og oppbygging",                 "course_content",         FALSE,
  "beskrivelse av emnet",                  "course_content",         FALSE,
  "emnebeskrivelse",                       "course_content",         FALSE,
  "kort om emnet",                         "course_content",         FALSE,
  "om dette emnet",                        "course_content",         FALSE,
  "om studiet",                            "course_content",         FALSE,
  "faginnhold",                            "course_content",         FALSE,
  "innhold",                               "course_content",         FALSE,
  "innhald",                               "course_content",         FALSE,
  "innledning",                            "course_content",         FALSE,
  "innleiing",                             "course_content",         FALSE,
  "course content",                        "course_content",         FALSE,
  "content",                               "course_content",         FALSE
)


#' Match a heading text to a canonical section name
#'
#' Performs case-insensitive substring matching against the pattern table.
#' Returns the section name for the first matching pattern, or NA if no match.
#'
#' @param heading Character string — the heading text to match.
#' @param word_start If TRUE, a substring pattern must start at a word
#'   boundary, so "elevkunnskap" does not match "kunnskap" (text_split uses
#'   this on plain-text lines).
#' @return Character string — canonical section name, or NA_character_.
match_heading_to_section <- function(heading, word_start = FALSE) {
  if (is.na(heading) || !nzchar(trimws(heading))) return(NA_character_)
  heading_lower <- tolower(trimws(heading))

  # Denylist: language/expression metadata fields whose text collides with a
  # real pattern (e.g. "eksamensspråk" contains "eksamen") but which are NOT
  # the section (#198, nla json maps "Eksamensspråk" into assessment; inn's
  # "Language of instruction and examination" would hit "examination"). As a
  # sub-heading, NA keeps uio's "Eksamensspråk" block in assessment.
  if (grepl("eksamensspråk|vurderingsspråk|language of instruction", heading_lower)) {
    return(NA_character_)
  }

  exact <- section_heading_patterns$exact %||% rep(FALSE, nrow(section_heading_patterns))
  eq <- section_heading_patterns$pattern == heading_lower
  if (any(eq)) return(section_heading_patterns$section[which(eq)[1]])

  for (i in seq_len(nrow(section_heading_patterns))) {
    if (isTRUE(exact[i])) next
    pattern <- section_heading_patterns$pattern[i]
    hit <- if (word_start) {
      grepl(paste0("(?<!\\p{L})\\Q", pattern, "\\E"), heading_lower, perl = TRUE)
    } else {
      grepl(pattern, heading_lower, fixed = TRUE)
    }
    if (hit) return(section_heading_patterns$section[i])
  }
  NA_character_
}
