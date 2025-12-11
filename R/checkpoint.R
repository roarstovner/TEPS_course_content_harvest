# R/checkpoint.R

#' Read checkpoint data
#'
#' @param checkpoint_path Path to checkpoint RDS file
#' @return A tibble with course_id, html, html_error, html_success columns, or NULL if no checkpoint exists
read_checkpoint <- function(checkpoint_path = "data/checkpoint_html.RDS") {
  if (file.exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    NULL
  }
}

#' Write checkpoint data
#'
#' @param checkpoint_df A tibble with course_id and html columns
#' @param checkpoint_path Path to checkpoint RDS file
write_checkpoint <- function(checkpoint_df, checkpoint_path = "data/checkpoint_html.RDS") {
  dir.create(dirname(checkpoint_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(checkpoint_df, checkpoint_path)
  invisible(checkpoint_df)
}

#' Fetch HTML with checkpoint support
#'
#' @param courses A tibble with course_id and url columns
#' @param checkpoint_path Path to checkpoint RDS file
#' @param .progress Show progress bar
#' @return courses tibble with html, html_error, html_success columns added
fetch_html_with_checkpoint <- function(courses, checkpoint_path = "data/checkpoint_html.RDS", .progress = TRUE) {
  # Read existing checkpoint
  checkpoint <- read_checkpoint(checkpoint_path)
  
  if (is.null(checkpoint)) {
    # No checkpoint - fetch everything
    courses_to_fetch <- courses
  } else {
    # Filter out courses already in checkpoint
    courses_to_fetch <- courses |>
      dplyr::anti_join(checkpoint, by = "course_id")
    
    if (nrow(courses_to_fetch) == 0) {
      cli::cli_alert_success("All {nrow(courses)} courses found in checkpoint")
      return(courses |>
        dplyr::left_join(checkpoint |> dplyr::select(course_id, html, html_error, html_success), by = "course_id"))
    }
    
    cli::cli_alert_info("Found {nrow(checkpoint)} courses in checkpoint, fetching {nrow(courses_to_fetch)} remaining")
  }
  
  # Fetch HTML for remaining courses
  html_cols <- fetch_html_cols(courses_to_fetch$url, courses_to_fetch$institution_short, .progress = .progress)
  courses_fetched <- dplyr::bind_cols(
    courses_to_fetch |> dplyr::select(course_id),
    html_cols
  )
  
  # Combine with checkpoint and save
  checkpoint_updated <- if (is.null(checkpoint)) {
    courses_fetched
  } else {
    dplyr::bind_rows(checkpoint, courses_fetched)
  }
  
  write_checkpoint(checkpoint_updated, checkpoint_path)
  cli::cli_alert_success("Checkpoint saved with {nrow(checkpoint_updated)} courses")
  
  # Return full dataset
  courses |>
    dplyr::left_join(checkpoint_updated |> dplyr::select(course_id, html, html_error, html_success), by = "course_id")
}