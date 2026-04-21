# R/section_heading_map.R
# Heading-to-section mapping table and matcher function for section extraction.

#' Heading pattern table
#'
#' Each row maps a pattern (substring) to a canonical section name.
#' Patterns are matched case-insensitively against heading text.
#' Ordered by specificity (longer/more specific patterns first within each section)
#' so that the first match wins.
section_heading_patterns <- tibble::tribble(
  ~pattern,                                ~section,                 ~exact,
  # --- assessment ---
  "vurdering og eksamen",                  "assessment",             FALSE,
  "mer om vurdering",                      "assessment",             FALSE,
  "vurderingsordning",                     "assessment",             FALSE,
  "vurderingsformer",                      "assessment",             FALSE,
  "vurderingsform",                        "assessment",             FALSE,
  "eksamensformer",                        "assessment",             FALSE,
  "eksamen",                               "assessment",             FALSE,
  "assessment methods",                    "assessment",             FALSE,
  "examination",                           "assessment",             FALSE,

  # --- prerequisites ---
  "krav til forkunnskaper",                "prerequisites",          FALSE,
  "anbefalte forkunnskaper",               "prerequisites",          FALSE,
  "forkunnskapskrav",                      "prerequisites",          FALSE,
  "forkunnskap",                           "prerequisites",          FALSE,
  "forkrav",                               "prerequisites",          FALSE,
  "opptakskrav",                           "prerequisites",          FALSE,
  "opptak til emnet",                      "prerequisites",          FALSE,
  "admission to the course",               "prerequisites",          FALSE,
  "hvem kan ta dette emnet",               "prerequisites",          FALSE,
  "krav til studierett",                   "prerequisites",          FALSE,
  "required prerequisite knowledge",       "prerequisites",          FALSE,
  "recommended prerequisite knowledge",    "prerequisites",          FALSE,
  "prerequisites",                         "prerequisites",          FALSE,

  # --- learning_outcomes ---
  "læringsutbytte",                        "learning_outcomes",      FALSE,
  "læringsmål",                            "learning_outcomes",      FALSE,
  "lærer du",                              "learning_outcomes",      FALSE,
  "learning outcomes",                     "learning_outcomes",      FALSE,
  "learning outcome",                      "learning_outcomes",      FALSE,

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
  "undervisning",                          "teaching_methods",       TRUE,
  "teaching",                              "teaching_methods",       TRUE,
  "teaching and working methods",          "teaching_methods",       FALSE,
  "teaching and learning activities",      "teaching_methods",       FALSE,
  "teaching methods",                      "teaching_methods",       FALSE,

  # --- coursework_requirements ---
  "obligatorisk læringsaktivitet",         "coursework_requirements", FALSE,
  "obligatorisk undervisningsaktivitet",   "coursework_requirements", FALSE,
  "obligatoriske aktiviteter",             "coursework_requirements", FALSE,
  "arbeidskrav",                           "coursework_requirements", FALSE,
  "compulsory activities",                 "coursework_requirements", FALSE,
  "work requirements",                     "coursework_requirements", FALSE,
  "coursework requirements",               "coursework_requirements", FALSE,

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
  "course content",                        "course_content",         FALSE,
  "content",                               "course_content",         FALSE
)


#' Match a heading text to a canonical section name
#'
#' Performs case-insensitive substring matching against the pattern table.
#' Returns the section name for the first matching pattern, or NA if no match.
#'
#' @param heading Character string — the heading text to match.
#' @return Character string — canonical section name, or NA_character_.
match_heading_to_section <- function(heading) {
  if (is.na(heading) || !nzchar(trimws(heading))) return(NA_character_)
  heading_lower <- tolower(trimws(heading))

  exact <- section_heading_patterns$exact %||% rep(FALSE, nrow(section_heading_patterns))
  eq <- section_heading_patterns$pattern == heading_lower
  if (any(eq)) return(section_heading_patterns$section[which(eq)[1]])

  for (i in seq_len(nrow(section_heading_patterns))) {
    if (isTRUE(exact[i])) next
    if (grepl(section_heading_patterns$pattern[i], heading_lower, fixed = TRUE)) {
      return(section_heading_patterns$section[i])
    }
  }
  NA_character_
}
