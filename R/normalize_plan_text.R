# R/normalize_plan_text.R

#' Normalize course_plan text for deduplication
#'
#' Applies lossy transforms (case-folding, heading synonyms, whitespace squishing)
#' on top of already-anonymized course_plan text. The result is suitable for
#' content hashing but not for human reading.
#'
#' @param course_plan Character vector of anonymized text (from anonymize_fulltext).
#' @param .progress Passed to purrr::map_chr for progress reporting.
#' @return Character vector of normalized text. NA input -> NA output.
normalize_plan_text <- function(course_plan, .progress = "Normalize plan texts") {
  purrr::map_chr(course_plan, \(txt) {
    if (is.na(txt) || !nzchar(txt)) return(NA_character_)

    txt |>
      tolower() |>
      stringr::str_replace_all("\\beksamensformer\\b", "vurderingsformer") |>
      stringr::str_squish()
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
