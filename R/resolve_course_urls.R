#' Resolve course URLs that can't be determined from metadata alone
#'
#' Some institutions (like USN) require trial-and-error to discover valid URLs.
#' This function takes courses with NA urls and attempts to discover them.
#'
#' @param df A tibble with course_id and url columns
#' @param checkpoint_path Path to checkpoint RDS file for URL resolution.
#'   Set to NULL to disable checkpointing (useful for testing).
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

  # Load checkpoint if exists (unless checkpointing disabled)
  checkpoint <- if (!is.null(checkpoint_path)) {
    read_url_checkpoint(checkpoint_path)
  } else {
    NULL
  }

  # Skip courses already resolved in checkpoint
  if (!is.null(checkpoint)) {
    needs_resolution <- dplyr::anti_join(needs_resolution, checkpoint, by = "course_id")
  }

  if (nrow(needs_resolution) == 0) {
    # All already resolved - merge checkpoint back
    # Ensure url_version column exists first
    if (!"url_version" %in% names(df)) {
      df <- df |> dplyr::mutate(url_version = NA_integer_)
    }

    return(df |>
      dplyr::rows_update(
        dplyr::select(checkpoint, course_id, url, url_version),
        by = "course_id",
        unmatched = "ignore"
      ))
  }

  # Dispatch to institution-specific batch resolvers (enables reuse of chromium sessions)

  resolve_batch <- function(df, inst) {
    if (inst == "usn") {
      resolve_urls_usn_batch(df)
    } else {
      df |> dplyr::mutate(url = NA_character_, url_version = NA_integer_)
    }
  }
  
  resolved <- needs_resolution |>
    dplyr::group_by(institution_short) |>
    dplyr::group_modify(~ resolve_batch(.x, .y$institution_short)) |>
    dplyr::ungroup()

  resolved <- needs_resolution |>
    dplyr::group_by(institution_short) |>
    dplyr::group_modify(~ resolve_batch(.x, .y$institution_short)) |>
    dplyr::ungroup()

  # Update checkpoint (only if checkpointing enabled)
  if (!is.null(checkpoint_path)) {
    updated_checkpoint <- if (is.null(checkpoint)) {
      resolved |> dplyr::select(course_id, url, url_version)
    } else {
      dplyr::bind_rows(
        checkpoint,
        resolved |> dplyr::select(course_id, url, url_version)
      )
    }

    write_url_checkpoint(updated_checkpoint, checkpoint_path)
  }

  # Ensure url_version column exists in df before merging
  if (!"url_version" %in% names(df)) {
    df <- df |> dplyr::mutate(url_version = NA_integer_)
  }

  # Merge back into original df
  df |>
    dplyr::rows_update(
      resolved |> dplyr::select(course_id, url, url_version),
      by = "course_id",
      unmatched = "ignore"
    )
}

#' Resolve URLs for a batch of USN courses using a single browser session
#'
#' This function reuses a single Chrome session for all URL validations,
#' navigating via hash changes instead of creating new sessions. This is
#' much faster than creating a new session per URL.
#'
#' @param df A tibble with Emnekode, Årstall, Semesternavn columns
#' @param max_version Maximum version number to try per course (default: 5)
#' @param .progress Show progress bar
#' @return df with url and url_version columns added
resolve_urls_usn_batch <- function(df, max_version = 5, .progress = TRUE) {
  if (nrow(df) == 0) {
    return(df |> dplyr::mutate(url = NA_character_, url_version = NA_integer_))
  }

  # Start a single browser session at the base URL
  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  session <- rvest::read_html_live(base_url)

  # Initial wait for page to fully load
  Sys.sleep(3)

  # Setup progress bar
  if (.progress) {
    pb <- progress::progress_bar$new(
      format = "  resolving [:bar] :current/:total (:percent) eta: :eta",
      total = nrow(df),
      clear = FALSE
    )
  }

  # Process each course
  results <- purrr::pmap(
    list(df$Emnekode, df$Årstall, df$Semesternavn),
    function(course_code, year, semester) {
      if (.progress) pb$tick()

      # Convert semester to USN format
      sem <- dplyr::case_match(
        semester,
        "Vår" ~ "VÅR",
        "Høst" ~ "HØST",
        .default = toupper(semester)
      )

      # Normalize expected semester for comparison
      expected_sem_norm <- dplyr::case_match(
        tolower(semester),
        "vår" ~ "vår",
        "høst" ~ "høst",
        .default = tolower(semester)
      )

      # Try version numbers
      for (version in seq_len(max_version)) {
        hash <- glue::glue("#/emne/{course_code}_{version}_{year}_{sem}")
        full_url <- paste0(base_url, hash)

        tryCatch({
          # Navigate by changing the hash (much faster than new session)
          session$session$Runtime$evaluate(
            sprintf("window.location.hash = '%s';", hash)
          )

          # Wait for content to render (shorter wait since page is already loaded)
          Sys.sleep(2)

          # Extract Shadow DOM content
          html_content <- read_usn_live_html(session)

          if (!is.null(html_content)) {
            # Check for the expected year/semester pattern
            pattern <- "Undervisningsstart\\s+(høst|vår)\\s+(\\d{4})"
            match <- stringr::str_match(
              html_content,
              stringr::regex(pattern, ignore_case = TRUE)
            )

            if (!is.na(match[1])) {
              displayed_year <- as.integer(match[3])
              displayed_sem <- tolower(match[2])

              if (displayed_year == year && displayed_sem == expected_sem_norm) {
                return(list(url = full_url, version = version))
              }
            }
          }
        }, error = function(e) {
          message("Error checking version ", version, " for ", course_code, ": ", e$message)
        })
      }

      # No valid version found
      list(url = NA_character_, version = NA_integer_)
    }
  )

  # Clean up the browser session
  tryCatch(
    session$session$close(),
    error = function(e) NULL
  )

  # Add results to dataframe
  df |>
    dplyr::mutate(
      url = purrr::map_chr(results, ~ .x$url %||% NA_character_),
      url_version = purrr::map_int(results, ~ .x$version %||% NA_integer_)
    )
}

#' Resolve URL for a single USN course by trying version numbers
#'
#' USN course URLs include a version number that indicates a revision of the course plan.
#' Each version is valid for a specific range of years. When requesting a version+year
#' that doesn't exist, USN silently redirects to another page (often the most recent).
#'
#' This function tries versions 1 through max_version and validates that the page
#' actually displays the requested year/semester (not a redirect).
#'
#' Note: For batch processing, use resolve_urls_usn_batch() which reuses a single
#' browser session for better performance.
#'
#' @param course_code Course code (e.g., "MG1PE2")
#' @param year Academic year (e.g., 2026)
#' @param semester Semester name (e.g., "Vår", "Høst")
#' @param max_version Maximum version number to try (default: 5)
#' @return Named list with url and version, or NA if no valid URL found
resolve_url_usn <- function(course_code, year, semester, max_version = 5) {
  # Convert semester to USN format for URL
  sem <- dplyr::case_match(
    semester,
    "Vår" ~ "VÅR",
    "Høst" ~ "HØST",
    .default = toupper(semester)
  )

  # Try version numbers from 1 to max_version
  for (version in seq_len(max_version)) {
    url <- glue::glue("https://www.usn.no/studier/studie-og-emneplaner/#/emne/{course_code}_{version}_{year}_{sem}")

    # Validate checks that the displayed year matches the requested year
    # This detects silent redirects to other year/semester pages
    if (validate_usn_url(url, year, semester)) {
      return(list(url = as.character(url), version = version))
    }
  }

  # No valid version found - course likely wasn't offered this semester
  list(url = NA_character_, version = NA_integer_)
}

#' Read rendered content from a USN LiveHTML session
#'
#' USN renders course content inside Shadow DOM (web components), which
#' standard html_text() cannot access. This function executes JavaScript
#' to recursively traverse shadow roots and extract text content.
#'
#' The caller is responsible for:
#' 1. Creating the session with rvest::read_html_live()
#' 2. Waiting for content to render before calling this function
#' 3. Closing the session when done
#'
#' @param session A LiveHTML session from rvest::read_html_live()
#' @return Character string with rendered page content, or NULL on error
#' @export
read_usn_live_html <- function(session) {

  shadow_js <- "
  function getTextWithShadow(node) {
    let text = '';
    if (node.shadowRoot) {
      text += getTextWithShadow(node.shadowRoot);
    }
    for (const child of node.childNodes) {
      if (child.nodeType === Node.TEXT_NODE) {
        text += child.textContent;
      } else if (child.nodeType === Node.ELEMENT_NODE) {
        text += getTextWithShadow(child);
      }
    }
    return text;
  }
  getTextWithShadow(document.body);
  "

  tryCatch({
    js_result <- session$session$Runtime$evaluate(shadow_js)
    content <- js_result$result$value

    if (is.null(content) || content == "") {
      return(NULL)
    }

    content
  }, error = function(e) {
    message("Error in read_usn_live_html: ", conditionMessage(e))
    NULL
  })
}

#' Validate if a USN URL points to a valid course page for the expected year/semester
#'
#' Since USN uses hash fragments (#/emne/...) and JavaScript routing,
#' we need to use a live browser session to render the page and check content.
#' Crucially, we verify that the displayed year matches the requested year,
#' because invalid version+year combos silently redirect to other pages.
#'
#' @param url URL to validate
#' @param expected_year The year we expect to see displayed (integer)
#' @param expected_semester The semester we expect ("Vår" or "Høst")
#' @return TRUE if valid course page showing the correct year/semester, FALSE otherwise
validate_usn_url <- function(url, expected_year, expected_semester) {
  tryCatch(
    {
      session <- rvest::read_html_live(url)
      Sys.sleep(5)

      html_content <- read_usn_live_html(session)

      tryCatch(session$session$close(), error = function(e) NULL)

      if (is.null(html_content)) {
        return(FALSE)
      }

      # Extract "Planen gjelder for: Undervisningsstart [semester] [year]"
      pattern <- "Undervisningsstart\\s+(høst|vår)\\s+(\\d{4})"
      match <- stringr::str_match(
        html_content,
        stringr::regex(pattern, ignore_case = TRUE)
      )

      if (is.na(match[1])) {
        return(FALSE)
      }

      displayed_semester_raw <- match[2]
      displayed_year <- as.integer(match[3])

      expected_sem_norm <- dplyr::case_match(
        tolower(expected_semester),
        "vår" ~ "vår",
        "høst" ~ "høst",
        .default = tolower(expected_semester)
      )

      displayed_year == expected_year &&
        tolower(displayed_semester_raw) == expected_sem_norm
    },
    error = function(e) {
      message("Error in validate_usn_url: ", conditionMessage(e))
      FALSE
    }
  )
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
