# R/checkpoint.R

#' Read checkpoint data
#'
#' @param checkpoint_path Path to checkpoint RDS file
#' @return A tibble with course_id, html, html_error, html_success columns, or NULL if no checkpoint exists
read_checkpoint <- function(checkpoint_path) {
  if (!file.exists(checkpoint_path)) return(NULL)
  readRDS(checkpoint_path)
}

#' Write checkpoint data
#'
#' @param checkpoint_df A tibble with course_id and html columns
#' @param checkpoint_path Path to checkpoint RDS file
write_checkpoint <- function(checkpoint_df, checkpoint_path) {
  dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(checkpoint_df, checkpoint_path)
  invisible(checkpoint_df)
}

#' Fetch HTML with checkpoint support
#'
#' @param courses A tibble with course_id and url columns
#' @param checkpoint_path Path to checkpoint RDS file
#' @param .progress Show progress bar
#' @return courses tibble with html, html_error, html_success columns added
fetch_html_with_checkpoint <- function(courses,
                                       checkpoint_path = "data/checkpoint/checkpoint_html.RDS",
                                       .progress = TRUE) {
  # 0) Guardrails: vi forventer disse kolonnene
  if (!all(c("course_id", "url", "institution_short") %in% names(courses))) {
    stop("courses must contain: course_id, url, institution_short")
  }
  
  # 1) Les eksisterende checkpoint (hvis finnes)
  checkpoint <- read_checkpoint(checkpoint_path)
  
  # 2) Finn hvilke vi må hente (ikke i checkpoint enda)
  if (is.null(checkpoint)) {
    to_fetch <- courses
  } else {
    to_fetch <- dplyr::anti_join(courses, checkpoint, by = "course_id")
  }
  
  # 3) Dropp rader uten URL (ellers sløser du tid og får NA-feil)
  to_fetch <- dplyr::filter(to_fetch, !is.na(url), nzchar(url))
  
  # Hvis ingenting gjenstår: returner courses + checkpoint-data
  if (nrow(to_fetch) == 0) {
    if (!is.null(checkpoint)) {
      return(dplyr::left_join(
        courses,
        dplyr::select(checkpoint, course_id, html, html_error, html_success),
        by = "course_id"
      ))
    } else {
      # Ingen checkpoint og ingenting å hente -> returner bare NA-kolonner
      return(dplyr::mutate(courses,
                           html = NA_character_,
                           html_error = list(NULL),
                           html_success = NA
      ))
    }
  }
  
  # 4) Hent HTML-kolonner for rest
  html_cols <- fetch_html_cols(
    urls        = to_fetch$url,
    institution = to_fetch$institution_short,
    .progress   = .progress
  )
  
  fetched <- dplyr::bind_cols(
    dplyr::select(to_fetch, course_id),
    html_cols
  ) |>
    dplyr::select(course_id, html, html_error, html_success)
  
  # 5) Oppdater checkpoint og lagre
  updated <- if (is.null(checkpoint)) fetched else dplyr::bind_rows(checkpoint, fetched)
  updated <- dplyr::distinct(updated, course_id, .keep_all = TRUE) # ekstra sikkerhet
  
  write_checkpoint(updated, checkpoint_path)
  
  # 6) Returner full courses med html-kolonner joinet inn
  dplyr::left_join(
    courses,
    dplyr::select(updated, course_id, html, html_error, html_success),
    by = "course_id"
  )
}
