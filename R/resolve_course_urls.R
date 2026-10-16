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
                                checkpoint_path = file.path(RAW_DIR, "checkpoint", "url_resolution.RDS"),
                                .progress = TRUE) {
  # Guardrails
  if (!all(c("course_id", "url", "institution") %in% names(df))) {
    stop("df must contain: course_id, url, institution")
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
    } else if (inst == "hivolda") {
      resolve_urls_hivolda_batch(df, .progress = .progress)
    } else if (inst == "uit") {
      resolve_urls_uit_batch(df, .progress = .progress)
    } else if (inst == "uio") {
      resolve_urls_uio_batch(df)
    } else {
      df |> dplyr::mutate(url = NA_character_)
    }
  }

  resolved <- needs_resolution |>
    dplyr::group_by(institution) |>
    dplyr::group_modify(~ resolve_batch(.x, .y$institution, checkpoint_path, .progress)) |>
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

#' Resolve URLs for a batch of Hivolda courses
#'
#' For each unique Emnekode, fetches the base page /emne/{CODE} and parses
#' the "Semestre" section to find semester-specific version URLs.
#' Link text uses Nynorsk ("Haust") which is mapped to Bokmål ("Høst").
#'
#' @param df A tibble with Emnekode, Årstall, Semesternavn columns
#' @param .progress Show progress bar
#' @return df with url column filled in where possible
resolve_urls_hivolda_batch <- function(df, .progress = TRUE) {
  if (nrow(df) == 0) return(df |> dplyr::mutate(url = NA_character_))

  unique_codes <- unique(df$Emnekode)
  cli::cli_alert_info("Discovering URLs for {length(unique_codes)} unique Hivolda course codes")

  if (.progress) pb <- cli::cli_progress_bar(total = length(unique_codes))

  # Build lookup: Emnekode + year + semester -> URL
  lookup_rows <- list()

  for (code in unique_codes) {
    tryCatch({
      base_url <- paste0("https://www.hivolda.no/emne/", code)
      polite_pause()
      page <- rvest::read_html(base_url)

      # Find all links under the course page that match /emne/{CODE}/{ID}
      links <- page |>
        rvest::html_elements("a") |>
        purrr::keep(~ grepl(paste0("/emne/", code, "/\\d+"), rvest::html_attr(.x, "href")))

      for (link in links) {
        href <- rvest::html_attr(link, "href")
        text <- trimws(rvest::html_text2(link))

        # Parse "2024 Haust" or "2023 Vår" from link text
        m <- stringr::str_match(text, "(\\d{4})\\s+(\\S+)")
        if (!is.na(m[1])) {
          year <- as.integer(m[2])
          # Map Nynorsk to Bokmål: Haust -> Høst
          sem_raw <- m[3]
          sem <- dplyr::case_match(
            tolower(sem_raw),
            "haust" ~ "Høst",
            "vår"   ~ "Vår",
            .default = sem_raw
          )

          full_url <- paste0("https://www.hivolda.no", href)
          lookup_rows <- c(lookup_rows, list(tibble::tibble(
            Emnekode = code, Årstall = year, Semesternavn = sem, url = full_url
          )))
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch listing for {code}: {e$message}")
    })

    if (.progress) cli::cli_progress_update(id = pb)
  }

  if (length(lookup_rows) == 0) {
    cli::cli_alert_warning("No semester URLs discovered")
    return(df |> dplyr::mutate(url = NA_character_))
  }

  lookup <- dplyr::bind_rows(lookup_rows) |>
    dplyr::distinct(Emnekode, Årstall, Semesternavn, .keep_all = TRUE)
  cli::cli_alert_success("Discovered {nrow(lookup)} semester URLs across {length(unique_codes)} course codes")

  # Join lookup to df, replacing the NA url column
  df |>
    dplyr::select(-url) |>
    dplyr::left_join(lookup, by = c("Emnekode", "Årstall", "Semesternavn"))
}

#' Resolve URLs for a batch of UiO courses from the sitemap
#'
#' A UiO course page sits in a faculty and department folder
#' (/studier/emner/hf/ilos/ENG2165/) that DBH does not give. The sitemap lists
#' the folder of every course taught now (the same courses as the listing
#' /studier/emner/alle/). A course not taught now keeps its page but is not
#' listed: it gets the folder most listed courses with its code prefix
#' ("ENG") have, else the one most listed courses of its DBH department
#' (Avdelingskode) have, and a wrong guess is a 404. The URL is always the
#' base URL, never a semester page (/h24/: logistics, not the plan).
#'
#' @param df A tibble with Emnekode and Avdelingskode columns
#' @param folders Tibble of code, folder and name, from uio_course_folders()
#' @return df with url filled in
resolve_urls_uio_batch <- function(df, folders = uio_course_folders()) {
  code <- chartr("Ø", "O", toupper(df$Emnekode))   # MØNA2504's folder is MONA2504
  prefix <- function(code) sub("[0-9].*", "", code)
  common <- function(key, folder) {   # the folder most courses with the key have
    dplyr::count(tibble::tibble(key, folder), key, folder) |>
      dplyr::slice_max(n, n = 1, with_ties = FALSE, by = key)
  }
  listed <- dplyr::distinct(tibble::tibble(code, dept = df$Avdelingskode)) |>
    dplyr::inner_join(folders, by = "code")
  by_prefix <- common(prefix(folders$code), folders$folder)
  by_dept <- common(listed$dept, listed$folder)
  i <- match(code, folders$code)
  guess <- dplyr::coalesce(by_prefix$folder[match(prefix(code), by_prefix$key)],
                           by_dept$folder[match(df$Avdelingskode, by_dept$key)])
  path <- dplyr::coalesce(paste(folders$folder, folders$name, sep = "/")[i],
                          dplyr::if_else(is.na(guess), NA, paste(guess, code, sep = "/")))
  cli::cli_alert_info(paste("UiO: {sum(!is.na(i))} of {nrow(df)} offerings in the sitemap,",
                            "{sum(!is.na(path))} with a URL"))
  df$url <- dplyr::if_else(is.na(path), NA, paste0("https://www.uio.no/studier/emner/", path, "/"))
  df
}

#' UiO's course folders, from its sitemap
#'
#' A few folders are named in lower case (sv/iss/sos9009), and the URL must
#' be too.
#'
#' @param index URL of the sitemap index
#' @return Tibble of code (upper case), folder ("hf/ilos") and name (the
#'   course folder's own name), one row per code
uio_course_folders <- function(index = "https://www.uio.no/sitemapindex.xml") {
  get <- function(url) httr2::resp_body_string(fetch_html_cols_single(url))
  maps <- stringr::str_match_all(get(index), "<loc>(/sitemap-www\\.uio\\.no-\\d+\\.xml)</loc>")[[1]][, 2]
  if (!length(maps)) stop("no www.uio.no sitemap in ", index)
  m <- do.call(rbind, lapply(paste0("https://www.uio.no", maps), function(url) {
    polite_pause()
    stringr::str_match_all(get(url), "<loc>https://www\\.uio\\.no/studier/emner/([a-z]+/[^/<]+)/([A-Za-z0-9-]+)/")[[1]]
  }))
  tibble::tibble(code = toupper(m[, 3]), folder = m[, 2], name = m[, 3]) |>
    dplyr::distinct(code, .keep_all = TRUE)
}

#' Resolve URLs for a batch of UiT courses via document ID discovery
#'
#' For each unique Emnekode, fetches the active page /utdanning/aktivt/emne/{CODE}
#' and parses the <select> dropdown to find historical document IDs.
#' Option text format: "{CODE}: {H|V} {YEAR}" where H=Høst, V=Vår.
#'
#' @param df A tibble with Emnekode, Årstall, Semesternavn columns
#' @param .progress Show progress bar
#' @return df with url column filled in where possible
resolve_urls_uit_batch <- function(df, .progress = TRUE) {
  if (nrow(df) == 0) return(df |> dplyr::mutate(url = NA_character_))

  unique_codes <- unique(df$Emnekode)
  cli::cli_alert_info("Discovering URLs for {length(unique_codes)} unique UiT course codes")

  if (.progress) pb <- cli::cli_progress_bar(total = length(unique_codes))

  # Build lookup: Emnekode + year + semester -> URL
  lookup_rows <- list()

  for (code in unique_codes) {
    tryCatch({
      base_url <- paste0("https://uit.no/utdanning/aktivt/emne/", toupper(code))
      polite_pause()
      page <- rvest::read_html(base_url)

      # Find <option> elements with p_document_id in value
      options <- page |>
        rvest::html_elements("option[value*='p_document_id']")

      for (opt in options) {
        href <- rvest::html_attr(opt, "value")
        text <- trimws(rvest::html_text2(opt))

        # Parse "{CODE}: {H|V} {YEAR}" from option text
        m <- stringr::str_match(text, ":\\s+([HV])\\s+(\\d{4})")
        if (!is.na(m[1])) {
          year <- as.integer(m[3])
          sem <- dplyr::case_match(m[2], "H" ~ "Høst", "V" ~ "Vår", .default = NA_character_)

          full_url <- paste0("https://uit.no", href)
          lookup_rows <- c(lookup_rows, list(tibble::tibble(
            Emnekode = code, Årstall = year, Semesternavn = sem, url = full_url
          )))
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch listing for {code}: {e$message}")
    })

    if (.progress) cli::cli_progress_update(id = pb)
  }

  if (length(lookup_rows) == 0) {
    cli::cli_alert_warning("No semester URLs discovered")
    return(df |> dplyr::mutate(url = NA_character_))
  }

  lookup <- dplyr::bind_rows(lookup_rows) |>
    dplyr::semi_join(df, by = c("Emnekode", "Årstall", "Semesternavn")) |>
    uit_one_per_semester()
  cli::cli_alert_success("Discovered {nrow(lookup)} semester URLs across {length(unique_codes)} course codes")

  # Join lookup to df, replacing the NA url column
  df |>
    dplyr::select(-url) |>
    dplyr::left_join(lookup, by = c("Emnekode", "Årstall", "Semesternavn"))
}

#' One UiT document per semester
#'
#' The semester picker can list a semester twice: an English document (heading
#' "Course code:", for ENG-2980 V 2017 an empty page) beside the Norwegian one,
#' or two Norwegian revisions. Only for such a semester are the documents
#' fetched; the first Norwegian one is kept.
#'
#' @param lookup Tibble with Emnekode, Årstall, Semesternavn, url, in picker order
#' @param is_english Function of a URL: TRUE for an English document
#' @return lookup with one row per semester
uit_one_per_semester <- function(lookup, is_english = uit_page_is_english) {
  lookup |>
    dplyr::mutate(english = if (dplyr::n() > 1) purrr::map_lgl(url, is_english) else FALSE,
                  .by = c(Emnekode, Årstall, Semesternavn)) |>
    dplyr::arrange(english) |>
    dplyr::distinct(Emnekode, Årstall, Semesternavn, .keep_all = TRUE) |>
    dplyr::select(-english)
}

uit_page_is_english <- function(url) {
  polite_pause()
  tryCatch(grepl("Course code:", as.character(rvest::read_html(url)), fixed = TRUE),
           error = function(e) FALSE)
}

#' Resolve URLs for a batch of USN courses using a single browser session
#'
#' This function reuses a single Chrome session for all URL validations,
#' navigating via hash changes instead of creating new sessions. This is
#' much faster than creating a new session per URL.
#'
#' @param df A tibble with Emnekode, Årstall, Semesternavn columns
#' @param max_version Maximum version number to try per course (default: 3; every
#'   URL found so far is version 1 or 2)
#' @param .progress Show progress bar
#' @return df with url and url_version columns added
resolve_urls_usn_batch <- function(df, max_version = 3, checkpoint_path = NULL, .progress = TRUE) {
  if (nrow(df) == 0) {
    return(df |> dplyr::mutate(url = NA_character_, html = NA_character_))
  }

  # Start a single browser session at the base URL. read_html_live() needs
  # chromote, which rvest only suggests; naming it here puts it in renv.lock
  rlang::check_installed("chromote")
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
              # The notice that the semester's plan is not ready stays from the
              # page shown before when that page showed this same plan (a
              # semester without one); a reload clears it
              if (grepl("ikke klar ennå", html_content, fixed = TRUE)) {
                session$session$Page$reload()
                Sys.sleep(5)
                html_content <- read_usn_live_html(session) %||% html_content
              }
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
