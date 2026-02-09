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
    read_checkpoint(checkpoint_path)
  } else {
    NULL
  }

  # Skip courses already resolved in checkpoint
  n_needs_resolution <- nrow(needs_resolution)
  if (!is.null(checkpoint)) {
    needs_resolution <- dplyr::anti_join(needs_resolution, checkpoint, by = "course_id")
  }

  # Return early if nothing left to resolve (all in checkpoint)
  if (nrow(needs_resolution) == 0) {
    cli::cli_alert_success("All {n_needs_resolution} courses found in checkpoint")
    # Merge checkpoint data back into df
    update_cols <- intersect(c("course_id", "url", "html"), names(checkpoint))
    if ("html" %in% names(checkpoint) && !"html" %in% names(df)) {
      df <- df |> dplyr::mutate(html = NA_character_)
    }
    return(df |>
      dplyr::rows_update(
        checkpoint |> dplyr::select(dplyr::all_of(update_cols)),
        by = "course_id",
        unmatched = "ignore"
      ))
  }

  # Dispatch to institution-specific batch resolvers (enables reuse of chromium sessions)
  n_from_checkpoint <- n_needs_resolution - nrow(needs_resolution)
  if (n_from_checkpoint > 0) {
    cli::cli_alert_info("Found {n_from_checkpoint} courses in checkpoint, resolving {nrow(needs_resolution)} remaining")
  }

  resolve_batch <- function(df, inst, checkpoint_path, .progress) {
    if (inst == "usn") {
      resolve_urls_usn_batch(df, checkpoint_path = checkpoint_path, .progress = .progress)
    } else {
      df |> dplyr::mutate(url = NA_character_)
    }
  }

  resolved <- needs_resolution |>
    dplyr::group_by(institution_short) |>
    dplyr::group_modify(~ resolve_batch(.x, .y$institution_short, checkpoint_path, .progress)) |>
    dplyr::ungroup()

  # Update checkpoint (only if checkpointing enabled)
  if (!is.null(checkpoint_path)) {
    # Select columns that exist in resolved (USN includes html, others may not)
    checkpoint_cols <- intersect(c("course_id", "url", "html"), names(resolved))

    updated_checkpoint <- if (is.null(checkpoint)) {
      resolved |> dplyr::select(dplyr::all_of(checkpoint_cols))
    } else {
      # Ensure checkpoint has same columns as new data
      if ("html" %in% names(resolved) && !"html" %in% names(checkpoint)) {
        checkpoint <- checkpoint |> dplyr::mutate(html = NA_character_)
      }
      dplyr::bind_rows(
        checkpoint,
        resolved |> dplyr::select(dplyr::all_of(checkpoint_cols))
      )
    }

    write_checkpoint(updated_checkpoint, checkpoint_path)
    cli::cli_alert_success("Checkpoint saved with {nrow(updated_checkpoint)} courses")
  }

  # Merge back into original df
  # Select columns that exist in resolved (USN includes html, others may not)
  update_cols <- intersect(c("course_id", "url", "html"), names(resolved))

  # Ensure df has html column if resolved has it
  if ("html" %in% names(resolved) && !"html" %in% names(df)) {
    df <- df |> dplyr::mutate(html = NA_character_)
  }

  df |>
    dplyr::rows_update(
      resolved |> dplyr::select(dplyr::all_of(update_cols)),
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
resolve_urls_usn_batch <- function(df, max_version = 3, checkpoint_path = NULL, .progress = TRUE) {
  if (nrow(df) == 0) {
    return(df |> dplyr::mutate(url = NA_character_, html = NA_character_))
  }

  # Start a single browser session at the base URL
  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  session <- rvest::read_html_live(base_url)

  # Initial wait for page to fully load
  Sys.sleep(3)

  # Initialize results storage
  results <- vector("list", nrow(df))
  if (.progress) pb <- cli::cli_progress_bar(total = nrow(df))

  for (i in seq_len(nrow(df))) {
    course_id <- df$course_id[i]
    course_code <- df$Emnekode[i]
    year <- df$Årstall[i]
    semester <- df$Semesternavn[i]

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

    # Default result
    result_url <- NA_character_
    result_html <- NA_character_

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
          # Early exit: if course code not in content, version doesn't exist.
          # Versions are sequential, so higher versions won't exist either.
          if (!stringr::str_detect(html_content, stringr::fixed(course_code))) {
            break
          }

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
              result_url <- full_url
              result_html <- html_content
              break
            }

            # Early exit: if version 1's teaching start is after requested year,
            # the course didn't exist yet. No need to check higher versions.
            if (version == 1 && displayed_year > year) {
              break
            }
          }
        }
      }, error = function(e) {
        message("Error checking version ", version, " for ", course_code, ": ", e$message)
      })
    }

    results[[i]] <- list(url = result_url, html = result_html)

    # Checkpoint after each course
    if (!is.null(checkpoint_path)) {
      row <- tibble::tibble(course_id = course_id, url = result_url, html = result_html)
      checkpoint_append_row(row, checkpoint_path)
    }

    if (.progress) cli::cli_progress_update(id = pb)
  }

  # Clean up the browser session
  tryCatch(
    session$session$close(),
    error = function(e) NULL
  )

  # Add results to dataframe
  df |>
    dplyr::mutate(
      url = purrr::map_chr(results, ~ .x$url %||% NA_character_),
      html = purrr::map_chr(results, ~ .x$html %||% NA_character_)
    )
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

  # Wrap in IIFE to avoid const re-declaration errors on repeated calls
  shadow_js <- "(function() {
  // Block-level elements that should have newlines after them (like html_text2)
  const blockElements = new Set([
    'P', 'DIV', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6',
    'LI', 'TR', 'BR', 'SECTION', 'ARTICLE', 'HEADER', 'FOOTER',
    'BLOCKQUOTE', 'PRE', 'HR', 'DT', 'DD'
  ]);

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
        // Add newline after block elements for readable formatting
        if (blockElements.has(child.tagName)) {
          text += '\\n';
        }
      }
    }
    return text;
  }

  // Navigate to the course content in nested shadow DOMs
  var usnStudy = document.querySelector('usn-study');
  if (usnStudy && usnStudy.shadowRoot) {
    var subjectEl = usnStudy.shadowRoot.querySelector('usn-study-subject');
    if (subjectEl && subjectEl.shadowRoot) {
      return getTextWithShadow(subjectEl.shadowRoot);
    }
  }
  return '';
})();"

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
