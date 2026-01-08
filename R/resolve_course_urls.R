#' Resolve course URLs that can't be determined from metadata alone
#'
#' Some institutions (like USN) require trial-and-error to discover valid URLs.
#' This function takes courses with NA urls and attempts to discover them.
#'
#' @param df A tibble with course_id and url columns
#' @param checkpoint_path Path to checkpoint RDS file for URL resolution
#' @param .progress Show progress bar
#' @return df with url column filled in where possible
resolve_course_urls <- function(df,
                                checkpoint_path = "data/checkpoint/url_resolution.RDS",
                                .progress = TRUE) {
  # Guardrails
  if (!all(c("course_id", "url", "institution_short") %in% names(df))) {
    stop("df must contain: course_id, url, institution_short")
  }

  # Only process rows where url is NA
  needs_resolution <- df |> dplyr::filter(is.na(url))

  if (nrow(needs_resolution) == 0) {
    return(df)
  }

  # Load checkpoint if exists
  checkpoint <- read_url_checkpoint(checkpoint_path)

  # Skip courses already resolved in checkpoint
  if (!is.null(checkpoint)) {
    needs_resolution <- dplyr::anti_join(needs_resolution, checkpoint, by = "course_id")
  }

  if (nrow(needs_resolution) == 0) {
    # All already resolved - merge checkpoint back
    return(df |>
      dplyr::rows_update(
        dplyr::select(checkpoint, course_id, url, url_version),
        by = "course_id",
        unmatched = "ignore"
      ))
  }

  # Dispatch to institution-specific resolvers
  resolved <- needs_resolution |>
    dplyr::mutate(
      resolution = dplyr::case_match(
        institution_short,
        "usn" ~ purrr::pmap(
          list(Emnekode, Årstall, Semesternavn),
          resolve_url_usn,
          .progress = .progress
        ),
        .default = list(list(url = NA_character_, version = NA_integer_))
      )
    ) |>
    tidyr::unnest_wider(resolution, names_sep = "_") |>
    dplyr::mutate(
      url = resolution_url,
      url_version = resolution_version
    ) |>
    dplyr::select(-dplyr::starts_with("resolution_"))

  # Update checkpoint
  updated_checkpoint <- if (is.null(checkpoint)) {
    resolved |> dplyr::select(course_id, url, url_version)
  } else {
    dplyr::bind_rows(
      checkpoint,
      resolved |> dplyr::select(course_id, url, url_version)
    )
  }

  write_url_checkpoint(updated_checkpoint, checkpoint_path)

  # Merge back into original df
  df |>
    dplyr::rows_update(
      resolved |> dplyr::select(course_id, url, url_version),
      by = "course_id",
      unmatched = "ignore"
    )
}

#' Resolve URL for a single USN course by trying version numbers
#'
#' @param course_code Course code (e.g., "MG1PE2")
#' @param year Academic year (e.g., 2026)
#' @param semester Semester name (e.g., "Vår", "Høst")
#' @param max_version Maximum version number to try (default: 5)
#' @return Named list with url and version, or NA if no valid URL found
resolve_url_usn <- function(course_code, year, semester, max_version = 5) {
  # Convert semester to USN format
  sem <- dplyr::case_match(
    semester,
    "Vår" ~ "VÅR",
    "Høst" ~ "HØST",
    .default = toupper(semester)
  )

  # Try version numbers from 1 to max_version
  for (version in seq_len(max_version)) {
    url <- glue::glue("https://www.usn.no/studier/studie-og-emneplaner/#/emne/{course_code}_{version}_{year}_{sem}")

    if (validate_usn_url(url)) {
      return(list(url = as.character(url), version = version))
    }
  }

  # No valid version found
  list(url = NA_character_, version = NA_integer_)
}

#' Validate if a USN URL points to a valid course page
#'
#' Since USN uses hash fragments (#/emne/...), the server always returns 200.
#' We need to check the HTML content to see if it's a valid course page.
#'
#' @param url URL to validate
#' @return TRUE if valid course page, FALSE otherwise
validate_usn_url <- function(url) {
  tryCatch({
    resp <- url |>
      httr2::request() |>
      httr2::req_user_agent(
        "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
      ) |>
      httr2::req_perform()

    html_content <- httr2::resp_body_string(resp)

    # Check for indicators of a valid course page
    # USN course pages contain specific elements/text that 404 or error pages don't
    # We check for the presence of course-related content
    has_course_content <- stringr::str_detect(html_content, "emnebeskrivelse|course description|studiepoeng|ECTS") ||
                         stringr::str_detect(html_content, "læringsutbytte|learning outcome")

    # Check it's NOT an error page
    not_error <- !stringr::str_detect(html_content, "404|side ikke funnet|page not found", negate = FALSE)

    has_course_content && not_error

  }, error = function(e) {
    FALSE
  })
}

#' Read URL resolution checkpoint
#'
#' @param checkpoint_path Path to checkpoint RDS file
#' @return A tibble with course_id, url, url_version columns, or NULL if no checkpoint exists
read_url_checkpoint <- function(checkpoint_path) {
  if (!file.exists(checkpoint_path)) return(NULL)
  readRDS(checkpoint_path)
}

#' Write URL resolution checkpoint
#'
#' @param checkpoint_df A tibble with course_id, url, url_version columns
#' @param checkpoint_path Path to checkpoint RDS file
write_url_checkpoint <- function(checkpoint_df, checkpoint_path) {
  dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(checkpoint_df, checkpoint_path)
  invisible(checkpoint_df)
}
