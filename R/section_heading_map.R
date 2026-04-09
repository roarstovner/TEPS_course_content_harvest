# R/section_heading_map.R
# Heading-to-section mapping table and matcher function for section extraction.

#' Heading pattern table
#'
#' Each row maps a pattern (substring) to a canonical section name.
#' Patterns are matched case-insensitively against heading text.
#' Ordered by specificity (longer/more specific patterns first within each section)
#' so that the first match wins.
section_heading_patterns <- tibble::tribble(
  ~pattern,                                ~section,
  # --- assessment ---
  "vurdering og eksamen",                  "assessment",
  "vurderingsordning",                     "assessment",
  "vurderingsformer",                      "assessment",
  "eksamensformer",                        "assessment",
  "eksamen",                               "assessment",
  "assessment methods",                    "assessment",
  "examination",                           "assessment",

  # --- prerequisites ---
  "krav til forkunnskaper",                "prerequisites",
  "anbefalte forkunnskaper",               "prerequisites",
  "forkunnskapskrav",                      "prerequisites",
  "opptakskrav",                           "prerequisites",
  "required prerequisite knowledge",       "prerequisites",
  "recommended prerequisite knowledge",    "prerequisites",
  "prerequisites",                         "prerequisites",


  # --- learning_outcomes ---
  "læringsutbytte",                        "learning_outcomes",
  "læringsmål",                            "learning_outcomes",
  "learning outcomes",                     "learning_outcomes",
  "learning outcome",                      "learning_outcomes",

  # --- teaching_methods ---
  "arbeids- og undervisningsformer",       "teaching_methods",
  "undervisnings- og arbeidsformer",       "teaching_methods",
  "undervisningsformer",                   "teaching_methods",
  "undervisningsopplegg",                  "teaching_methods",
  "teaching and working methods",          "teaching_methods",
  "teaching and learning activities",      "teaching_methods",
  "teaching methods",                      "teaching_methods",

  # --- coursework_requirements ---
  "obligatoriske aktiviteter",             "coursework_requirements",
  "arbeidskrav",                           "coursework_requirements",
  "compulsory activities",                 "coursework_requirements",
  "work requirements",                     "coursework_requirements",
  "coursework requirements",              "coursework_requirements",

  # --- reading_list ---
  "pensumlitteratur",                      "reading_list",
  "pensum",                                "reading_list",
  "lesestoff",                             "reading_list",
  "læremidler",                            "reading_list",
  "litteratur",                            "reading_list",
  "reading list",                          "reading_list",

  # --- course_content ---
  "mål og innhold",                        "course_content",
  "emnets innhold",                        "course_content",
  "faginnhold",                            "course_content",
  "innhold",                               "course_content",
  "course content",                        "course_content",
  "content",                               "course_content"
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

  for (i in seq_len(nrow(section_heading_patterns))) {
    if (grepl(section_heading_patterns$pattern[i], heading_lower, fixed = TRUE)) {
      return(section_heading_patterns$section[i])
    }
  }
  NA_character_
}
