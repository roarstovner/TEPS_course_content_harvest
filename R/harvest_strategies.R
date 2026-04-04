# R/harvest_strategies.R
# Strategy implementations for the config-driven harvest pipeline.
# Each function receives (df, config, refetch) and returns df with
# url, html, html_error, html_success, extracted_text columns.

# --- Helpers ---

#' Build checkpoint file path for an institution
#' @param config Institution config (must have $name)
#' @param type Checkpoint type: "html", "urls", "discovery", "pdf"
strategy_checkpoint_path <- function(config, type = "html") {
  file.path("data", "checkpoint", paste0(type, "_", config$name, ".RDS"))
}

#' Clear checkpoint if refetch is requested
#' @param path Checkpoint file path
#' @param refetch Logical — if TRUE, delete the checkpoint
clear_if_refetch <- function(path, refetch) {
  if (refetch && file.exists(path)) {
    message("Refetch requested — clearing checkpoint: ", path)
    file.remove(path)
  }
  invisible(NULL)
}

#' Ensure uniform output columns exist on df
#' Adds missing columns with appropriate NA types.
ensure_output_columns <- function(df) {
  if (!"url" %in% names(df)) df$url <- NA_character_
  if (!"html" %in% names(df)) df$html <- NA_character_
  if (!"html_error" %in% names(df)) df$html_error <- vector("list", nrow(df))
  if (!"html_success" %in% names(df)) df$html_success <- NA
  if (!"extracted_text" %in% names(df)) df$extracted_text <- NA_character_
  df
}

# --- Strategy: standard ---
# URL -> HTTP fetch -> CSS extract
# Used by: oslomet, uia, ntnu, inn, hiof, hvl, mf, nih, nmbu, uib, uio, nord

harvest_standard <- function(df, config, refetch = FALSE) {
  cp <- strategy_checkpoint_path(config, "html")
  clear_if_refetch(cp, refetch)

  df <- fetch_html_with_checkpoint(df, checkpoint_path = cp, config = config)

  df$extracted_text <- extract_fulltext_css(
    df$html,
    config$selector,
    config$selector_mode,
    pre_fn = config$pre_fn,
    post_fn = config$post_fn
  )
  df
}

# --- Strategy: url_discovery ---
# URL -> discover real URL -> HTTP fetch -> CSS extract
# Used by: hivolda, uit

harvest_url_discovery <- function(df, config, refetch = FALSE) {
  url_cp <- strategy_checkpoint_path(config, "urls")
  html_cp <- strategy_checkpoint_path(config, "html")
  clear_if_refetch(url_cp, refetch)
  clear_if_refetch(html_cp, refetch)

  # Resolve URLs (hivolda, uit have institution-specific batch resolvers)
  df <- resolve_course_urls(df, checkpoint_path = url_cp)

  # Fetch HTML
  df <- fetch_html_with_checkpoint(df, checkpoint_path = html_cp, config = config)

  # Extract fulltext
  df$extracted_text <- extract_fulltext_css(
    df$html,
    config$selector,
    config$selector_mode,
    pre_fn = config$pre_fn,
    post_fn = config$post_fn
  )
  df
}

# --- Strategy: shadow_dom ---
# URL -> Chrome + Shadow DOM extraction (URL discovery + fetch combined)
# Used by: usn

harvest_shadow_dom <- function(df, config, refetch = FALSE) {
  url_cp <- strategy_checkpoint_path(config, "urls")
  clear_if_refetch(url_cp, refetch)

  # resolve_course_urls for USN also fetches HTML (combined step)
  df <- resolve_course_urls(df, checkpoint_path = url_cp)

  # Add html_success/html_error for output consistency
  df <- df |>
    dplyr::mutate(
      html_success = !is.na(html) & html != "",
      html_error = vector("list", dplyr::n())
    )

  # USN returns pre-rendered text, not raw HTML — use cleanup, not CSS extraction
  df$extracted_text <- .cleanup_usn_text(df$html)
  df
}

# --- Strategy: html_pdf_discovery ---
# URL -> discover dropdown -> HTML or PDF fetch -> extract
# Used by: uis

harvest_html_pdf_discovery <- function(df, config, refetch = FALSE) {
  discovery_cp <- strategy_checkpoint_path(config, "discovery")
  html_cp <- strategy_checkpoint_path(config, "html")
  pdf_cp <- strategy_checkpoint_path(config, "pdf")
  clear_if_refetch(discovery_cp, refetch)
  clear_if_refetch(html_cp, refetch)
  clear_if_refetch(pdf_cp, refetch)

  # --- Discovery phase: fetch base pages and parse dropdowns ---
  unique_codes <- df |>
    dplyr::distinct(Emnekode_raw, url) |>
    dplyr::filter(!is.na(url))

  existing_discovery <- read_checkpoint(discovery_cp)

  if (is.null(existing_discovery)) {
    codes_to_discover <- unique_codes
  } else {
    codes_to_discover <- dplyr::anti_join(unique_codes, existing_discovery,
                                           by = "Emnekode_raw")
  }

  if (nrow(codes_to_discover) > 0) {
    message("Discovering dropdowns for ", nrow(codes_to_discover), " course codes...")

    discovery_results <- purrr::map2(
      codes_to_discover$Emnekode_raw,
      codes_to_discover$url,
      \(code, base_url) {
        tryCatch({
          resp <- base_url |>
            httr2::request() |>
            httr2::req_user_agent(
              "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
            ) |>
            httr2::req_perform()
          html <- httr2::resp_body_string(resp)
          parsed <- .parse_uis_semester_dropdown(html)
          parsed$Emnekode_raw <- code
          parsed
        }, error = \(e) {
          tibble::tibble(label = character(), url = character(),
                         type = character(), year = integer(),
                         Emnekode_raw = character())
        })
      },
      .progress = TRUE
    ) |>
      dplyr::bind_rows()

    updated_discovery <- dplyr::bind_rows(existing_discovery, discovery_results)
    write_checkpoint(updated_discovery, discovery_cp)
    message("Discovery complete: ", nrow(discovery_results), " semester entries found")
  } else {
    message("All course codes already discovered (checkpoint)")
    updated_discovery <- existing_discovery
  }

  # --- Match discovered URLs with course rows ---
  discovered <- updated_discovery |> dplyr::filter(!is.na(year))

  df_matched <- df |>
    dplyr::left_join(
      discovered |> dplyr::select(Emnekode_raw, year, discovered_url = url, url_type = type),
      by = c("Emnekode_raw", "Årstall" = "year")
    ) |>
    dplyr::mutate(
      url = dplyr::coalesce(discovered_url, url),
      url_type = dplyr::coalesce(url_type, "html")
    )

  message("Matched: ",
          sum(df_matched$url_type == "html", na.rm = TRUE), " HTML, ",
          sum(df_matched$url_type == "pdf", na.rm = TRUE), " PDF, ",
          sum(is.na(df_matched$url_type) | is.na(df_matched$url)), " unmatched")

  # --- Process HTML subset ---
  df_html <- df_matched |> dplyr::filter(url_type == "html", !is.na(url))

  if (nrow(df_html) > 0) {
    df_html <- fetch_html_with_checkpoint(df_html, checkpoint_path = html_cp,
                                           config = config)
    df_html$extracted_text <- extract_fulltext_css(
      df_html$html,
      config$selector,
      config$selector_mode,
      pre_fn = config$pre_fn,
      post_fn = config$post_fn
    )
  } else {
    df_html <- df_html |>
      dplyr::mutate(html = NA_character_, html_error = list(NULL),
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Process PDF subset ---
  df_pdf <- df_matched |> dplyr::filter(url_type == "pdf", !is.na(url))
  existing_pdf <- read_checkpoint(pdf_cp)

  if (nrow(df_pdf) > 0) {
    if (!is.null(existing_pdf)) {
      to_fetch_pdf <- dplyr::anti_join(df_pdf, existing_pdf, by = "course_id")
    } else {
      to_fetch_pdf <- df_pdf
    }

    if (nrow(to_fetch_pdf) > 0) {
      message("Downloading ", nrow(to_fetch_pdf), " PDFs...")

      pdf_results <- purrr::map(
        to_fetch_pdf$url,
        \(pdf_url) {
          tryCatch({
            resp <- pdf_url |>
              httr2::request() |>
              httr2::req_user_agent(
                "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
              ) |>
              httr2::req_perform()
            list(raw = httr2::resp_body_raw(resp), error = NULL, success = TRUE)
          }, error = \(e) {
            list(raw = NULL, error = conditionMessage(e), success = FALSE)
          })
        },
        .progress = TRUE
      )

      extracted_text <- extract_fulltext_pdf(purrr::map(pdf_results, "raw"))

      fetched_pdf <- tibble::tibble(
        course_id = to_fetch_pdf$course_id,
        html = NA_character_,
        html_error = purrr::map(pdf_results, \(r) r$error),
        html_success = purrr::map_lgl(pdf_results, \(r) r$success),
        extracted_text = extracted_text
      )

      updated_pdf <- dplyr::bind_rows(existing_pdf, fetched_pdf)
      write_checkpoint(updated_pdf, pdf_cp)
    } else {
      updated_pdf <- existing_pdf
    }

    df_pdf <- df_pdf |>
      dplyr::left_join(
        updated_pdf |> dplyr::select(course_id, html, html_error, html_success, extracted_text),
        by = "course_id"
      )
  } else {
    df_pdf <- df_pdf |>
      dplyr::mutate(html = NA_character_, html_error = list(NULL),
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Handle unmatched rows ---
  df_none <- df_matched |> dplyr::filter(is.na(url) | (!url_type %in% c("html", "pdf")))
  if (nrow(df_none) > 0) {
    df_none <- df_none |>
      dplyr::mutate(html = NA_character_, html_error = list(NULL),
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Combine and clean up ---
  dplyr::bind_rows(df_html, df_pdf, df_none) |>
    dplyr::select(-dplyr::any_of(c("discovered_url", "url_type")))
}

# --- Strategy: pdf_split ---
# Fetch wrapper page -> download PDF -> split by emne section
# Used by: steiner

harvest_pdf_split <- function(df, config, refetch = FALSE) {
  dir.create("temp/steiner", showWarnings = FALSE, recursive = TRUE)

  # --- Define wrapper pages for each subject PDF ---
  wrapper_pages <- tibble::tibble(
    wrapper_url = c(
      "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-norsk-1",
      "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-matematikk-1",
      "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-naturfag-1",
      "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-pedagogikk-og-elevkunnskap-1",
      "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-samfunnsfag-1"
    ),
    subject = c("Norsk", "Matematikk", "Naturfag",
                "Pedagogikk og elevkunnskap", "Samfunnsfag")
  )

  # --- Extract PDF URLs from wrapper pages ---
  wrapper_pages <- wrapper_pages |>
    dplyr::mutate(pdf_url = purrr::map_chr(wrapper_url, .steiner_get_pdf_url)) |>
    dplyr::filter(!is.na(pdf_url))

  message("Found ", nrow(wrapper_pages), " PDF URLs")

  # --- Download PDFs ---
  wrapper_pages <- wrapper_pages |>
    dplyr::mutate(
      local_path = purrr::map2_chr(pdf_url, subject, .steiner_download_pdf)
    )

  # --- Extract emne sections ---
  pdf_emner <- purrr::pmap_dfr(
    list(wrapper_pages$local_path, wrapper_pages$subject, wrapper_pages$wrapper_url),
    .steiner_extract_emner_from_pdf
  )

  message("Extracted ", nrow(pdf_emner), " emne sections from PDFs")

  # --- Build join key and match to courses ---
  pdf_emner <- pdf_emner |>
    dplyr::mutate(
      join_key = .steiner_normalize_emne_name(pdf_emne_name) |>
        purrr::map_chr(.steiner_remap_pel_emne)
    )

  df <- df |>
    dplyr::mutate(join_key = .steiner_normalize_emne_name(Emnenavn))

  # Diagnostics
  pdf_keys <- unique(pdf_emner$join_key)
  course_keys <- unique(df$join_key)
  matched <- intersect(pdf_keys, course_keys)
  unmatched_pdf <- setdiff(pdf_keys, course_keys)
  unmatched_course <- setdiff(course_keys, pdf_keys)

  message("Join keys: ", length(matched), " matched, ",
          length(unmatched_pdf), " PDF-only, ",
          length(unmatched_course), " courses-only")
  if (length(unmatched_pdf) > 0) {
    message("  Unmatched PDF emner: ", paste(unmatched_pdf, collapse = "; "))
  }

  # --- Join fulltext ---
  pdf_lookup <- pdf_emner |> dplyr::select(join_key, url, extracted_text)

  df <- df |>
    dplyr::left_join(pdf_lookup, by = "join_key") |>
    dplyr::select(-join_key)

  message(sum(!is.na(df$extracted_text)), "/", nrow(df), " rows with extracted text")
  df
}

# Steiner helper: extract PDF URL from wrapper page
.steiner_get_pdf_url <- function(wrapper_url) {
  message("  Fetching wrapper: ", basename(wrapper_url))
  html <- tryCatch(rvest::read_html(wrapper_url), error = function(e) NULL)
  if (is.null(html)) return(NA_character_)

  page_text <- html |> rvest::html_text()
  pdf_urls <- stringr::str_extract_all(
    page_text, "https?://[^\"'\\s]+\\.pdf[^\"'\\s]*"
  )[[1]]

  pdf_from_attrs <- html |>
    rvest::html_nodes("iframe, embed, object") |>
    rvest::html_attr("src") |>
    stringr::str_subset("\\.pdf")

  all_pdfs <- unique(c(pdf_urls, pdf_from_attrs))
  if (length(all_pdfs) == 0) return(NA_character_)
  all_pdfs[1]
}

# Steiner helper: download PDF to temp directory
.steiner_download_pdf <- function(pdf_url, subject) {
  filename <- subject |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_-]", "") |>
    tolower()
  local_path <- file.path("temp/steiner", paste0(filename, ".pdf"))

  if (!file.exists(local_path)) {
    message("  Downloading: ", subject)
    download.file(pdf_url, local_path, mode = "wb", quiet = TRUE)
  }
  local_path
}

# Steiner helper: extract emne sections from a PDF
.steiner_extract_emner_from_pdf <- function(pdf_path, subject, wrapper_url) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages)) return(tibble::tibble())

  emne_pattern <- paste0(
    "^\\s*",
    stringr::str_replace_all(subject, "\\s+", "\\\\s+"),
    "\\s+\\d+\\s*,\\s*emne\\s+\\d+"
  )

  page_is_emne_start <- purrr::map_lgl(pages, function(p) {
    first_line <- stringr::str_split(p, "\n")[[1]][1]
    stringr::str_detect(first_line, stringr::regex(emne_pattern, ignore_case = TRUE))
  })

  page_is_overview <- purrr::map_lgl(pages, function(p) {
    first_line <- stringr::str_split(p, "\n")[[1]][1]
    stringr::str_detect(first_line, stringr::regex("emne\\s+\\d+\\s*-\\s*\\d+", ignore_case = TRUE))
  })

  emne_start_pages <- which(page_is_emne_start & !page_is_overview)
  if (length(emne_start_pages) == 0) return(tibble::tibble())

  message("  ", subject, ": ", length(emne_start_pages), " emner at pages ",
          paste(emne_start_pages, collapse = ", "))

  purrr::map_dfr(seq_along(emne_start_pages), function(i) {
    start <- emne_start_pages[i]
    all_starts <- which(page_is_emne_start | page_is_overview)
    next_starts <- all_starts[all_starts > start]
    end <- if (length(next_starts) > 0) next_starts[1] - 1 else length(pages)

    emne_text <- paste(pages[start:end], collapse = "\n\n") |>
      stringr::str_remove_all("Side\\s+\\d+\\s+av\\s+\\d+") |>
      stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
      stringr::str_replace_all("\\n{3,}", "\n\n") |>
      stringr::str_trim()
    first_line <- stringr::str_trim(stringr::str_split(pages[start], "\n")[[1]][1])

    tibble::tibble(
      pdf_emne_name = first_line,
      url = wrapper_url,
      extracted_text = emne_text
    )
  })
}

# Steiner helper: normalize emne name for joining
.steiner_normalize_emne_name <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace("^(.+?)\\s+(\\d+)\\s*,", "\\1\\2,")
}

# Steiner helper: remap Pedagogikk emne numbers
# PDF has "pedagogikk og elevkunnskap1, emne 4" and "emne 5"
# but courses.RDS has M-PEL2 "pedagogikk og elevkunnskap2, emne 1" and "emne 2"
.steiner_remap_pel_emne <- function(name) {
  if (stringr::str_detect(name, "^pedagogikk og elevkunnskap1, emne 4$")) {
    return("pedagogikk og elevkunnskap2, emne 1")
  }
  if (stringr::str_detect(name, "^pedagogikk og elevkunnskap1, emne 5$")) {
    return("pedagogikk og elevkunnskap2, emne 2")
  }
  name
}

# --- Strategy: json_extract ---
# URL -> HTTP fetch -> JSON extraction from <script> tag
# Used by: nla

harvest_json_extract <- function(df, config, refetch = FALSE) {
  html_cp <- strategy_checkpoint_path(config, "html")
  clear_if_refetch(html_cp, refetch)

  # NLA embeds all years in one page per course code — fetch once per unique Emnekode
  df <- df |>
    dplyr::mutate(academic_year = nla_academic_year(Årstall, Semesternavn))

  unique_codes <- df |>
    dplyr::distinct(Emnekode, .keep_all = TRUE) |>
    dplyr::select(Emnekode, url, institution_short) |>
    dplyr::filter(!is.na(url)) |>
    dplyr::mutate(course_id = paste0("nla_", Emnekode))

  unique_fetched <- fetch_html_with_checkpoint(unique_codes, checkpoint_path = html_cp,
                                                 config = config)

  # Join HTML back to all rows by Emnekode
  html_lookup <- unique_fetched |>
    dplyr::select(Emnekode, html, html_error, html_success)

  df <- dplyr::left_join(df, html_lookup, by = "Emnekode")

  # Extract year-specific fulltext from JSON
  df$extracted_text <- extract_nla_json(df$html, df$academic_year)
  df
}

# --- Strategy: noop ---
# Set extracted_text = NA (no harvestable course plan content)
# Used by: samas

harvest_noop <- function(df, config) {
  df$url <- NA_character_
  df$extracted_text <- NA_character_
  message("0/", nrow(df), " rows with extracted text (noop strategy)")
  df
}
