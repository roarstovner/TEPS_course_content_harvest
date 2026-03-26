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

#' Append a single row to checkpoint (for incremental updates)
#'
#' Reads existing checkpoint, appends row, writes back.
#' Useful for crash recovery during long-running operations.
#'
#' @param row A single-row tibble to append
#' @param path Path to checkpoint RDS file
#' @return Invisible updated checkpoint
checkpoint_append_row <- function(row, path) {
  existing <- read_checkpoint(path)
  updated <- dplyr::bind_rows(existing, row)
  write_checkpoint(updated, path)
}

#' Fetch HTML with checkpoint support
#'
#' @param courses A tibble with course_id and url columns
#' @param checkpoint_path Path to checkpoint RDS file
#' @param .progress Show progress bar
#' @return courses tibble with html, html_error, html_success columns added
fetch_html_with_checkpoint <- function(courses,
                                       checkpoint_path = "data/checkpoint/checkpoint_html.RDS",
                                       .progress = TRUE,
                                       config = NULL) {
  # 0) Guardrails: vi forventer disse kolonnene
  if (!all(c("course_id", "url", "institution_short") %in% names(courses))) {
    stop("courses must contain: course_id, url, institution_short")
  }
  
  # 1) Les eksisterende checkpoint (hvis finnes)
  checkpoint <- read_checkpoint(checkpoint_path)

  # 2) Finn hvilke vi må hente (ikke i checkpoint enda)
  if (is.null(checkpoint)) {
    message("No checkpoint found at ", checkpoint_path)
    to_fetch <- courses
  } else {
    message("Checkpoint: ", nrow(checkpoint), " courses already fetched")
    to_fetch <- dplyr::anti_join(courses, checkpoint, by = "course_id")
  }

  # 3) Dropp rader uten URL (ellers sløser du tid og får NA-feil)
  to_fetch <- dplyr::filter(to_fetch, !is.na(url), nzchar(url))
  message("To fetch: ", nrow(to_fetch), " courses with URLs")

  # Hvis ingenting gjenstår: returner courses + checkpoint-data
  if (nrow(to_fetch) == 0) {
    if (!is.null(checkpoint)) {
      message("All courses in checkpoint, skipping fetch")
      return(dplyr::left_join(
        courses,
        dplyr::select(checkpoint, course_id, html, html_error, html_success),
        by = "course_id"
      ))
    } else {
      return(dplyr::mutate(courses,
                           html = NA_character_,
                           html_error = list(NULL),
                           html_success = NA
      ))
    }
  }
  
  # 4) Hent HTML én om gangen med inkrementell checkpoint
  n <- nrow(to_fetch)
  if (.progress) pb <- progress::progress_bar$new(
    format = "  Fetching [:bar] :current/:total (:percent) ETA: :eta",
    total = n, clear = FALSE
  )

  for (i in seq_len(n)) {
    row <- to_fetch[i, ]
    html_cols <- fetch_html_cols(
      urls        = row$url,
      institution = row$institution_short,
      config      = config,
      .progress   = FALSE
    )
    fetched_row <- dplyr::bind_cols(
      dplyr::select(row, course_id),
      html_cols
    ) |>
      dplyr::select(course_id, html, html_error, html_success)

    checkpoint <- dplyr::bind_rows(checkpoint, fetched_row)
    write_checkpoint(checkpoint, checkpoint_path)

    if (.progress) pb$tick()
  }

  checkpoint <- dplyr::distinct(checkpoint, course_id, .keep_all = TRUE)
  
  # 6) Returner full courses med html-kolonner joinet inn
  dplyr::left_join(
    courses,
    dplyr::select(checkpoint, course_id, html, html_error, html_success),
    by = "course_id"
  )
}
