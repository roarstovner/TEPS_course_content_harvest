# R/extract_fulltext.R

#' Config-driven CSS extraction (replaces institution-specific dispatching)
#'
#' @param html Character vector of raw HTML strings
#' @param selector CSS selector string
#' @param mode "single" (html_element) or "multi" (html_elements, collapsed)
#' @param pre_fn Optional function applied to HTML string before parsing
#' @param post_fn Optional function applied to extracted text after parsing
#' @return Character vector of extracted text (NA where extraction fails)
extract_fulltext_css <- function(html, selector, mode = "single",
                                 pre_fn = NULL, post_fn = NULL) {
  safe_extract <- purrr::possibly(function(h) {
    if (!is.null(pre_fn)) h <- pre_fn(h)
    doc <- .read_doc(h)
    text <- if (mode == "single") {
      node <- rvest::html_element(doc, selector)
      if (length(node) == 0) return(NA_character_)
      rvest::html_text2(node)
    } else {
      nodes <- rvest::html_elements(doc, selector)
      if (length(nodes) == 0) return(NA_character_)
      txt <- rvest::html_text2(nodes)
      txt <- txt[nzchar(txt)]
      if (length(txt) == 0) return(NA_character_)
      paste(txt, collapse = "\n")
    }
    if (!is.null(post_fn)) text <- post_fn(text)
    if (is.na(text) || !nzchar(text)) NA_character_ else text
  }, otherwise = NA_character_)

  purrr::map_chr(html, function(h) {
    if (is.na(h) || !nzchar(h)) return(NA_character_)
    safe_extract(h)
  })
}

# Parse HTML without script/style text (ntnu's "function toggleRooms(...)"
# ended up in assessment, #245; uit's page scripts, #218) or form widgets
# (uib's semester picker "Vel emnebeskrivelse for semester 2027 Vår ...",
# #219). Used by both fulltext and section extraction (#259).
.read_doc <- function(html) {
  doc <- rvest::read_html(html)
  xml2::xml_remove(rvest::html_elements(doc, "script, style, noscript, select, label"))
  doc
}

#' Extract course plan text from the raw harvest
#'
#' One place for how `extracted_text` is made from what the harvest stored
#' (#256), used by the harvest strategies and by the {targets} pipeline:
#' the CSS selector and pre/post functions on `html` (standard, url_discovery,
#' uis web pages), the year's JSON in nla's page, cleanup of the text USN
#' renders in Chrome. Rows the harvest filled from a PDF (uis archive plans,
#' steiner) have no `html`: the PDF itself is not kept, so their stored text is
#' the raw data and is returned as is.
#'
#' @param df Harvested rows of one institution (`html`, and `academic_year`
#'   for nla; `extracted_text` for PDF rows).
#' @param config Institution config from get_institution_config().
#' @return Character vector of extracted text, one per row.
extract_fulltext_from_raw <- function(df, config) {
  stored <- df$extracted_text %||% rep(NA_character_, nrow(df))
  switch(config$strategy,
    noop         = rep(NA_character_, nrow(df)),
    pdf_split    = stored,
    shadow_dom   = .cleanup_usn_text(df$html),
    json_extract = extract_nla_json(df$html, df$academic_year),
    dplyr::if_else(
      is.na(df$html),
      stored,
      extract_fulltext_css(df$html, config$selector, config$selector_mode,
                           pre_fn = config$pre_fn, post_fn = config$post_fn)
    )
  )
}

#' Harvested rows with the current extracted_text
#'
#' Reads the raw harvest (data/raw/html_{inst}.RDS) and takes `extracted_text`
#' from data/interim/extracted_text.RDS, which the {targets} pipeline rebuilds
#' with the current config. The `extracted_text` stored in html_{inst}.RDS at
#' harvest time is ignored.
#'
#' @param institutions Institutions to read; all when empty.
read_harvest <- function(institutions = NULL,
                         text_file = "data/interim/extracted_text.RDS") {
  if (!file.exists(text_file)) {
    stop(text_file, " is missing: run targets::tar_make() first")
  }
  found <- harvested_institutions()
  if (length(institutions)) found <- intersect(found, institutions)
  text <- readRDS(text_file)[, c("course_id", "extracted_text")]
  harvest_file(found) |>
    lapply(readRDS) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of(c("extracted_text", "fulltext"))) |>
    dplyr::left_join(text, by = "course_id")
}

.add_table_cell_breaks <- function(html) {
  # Insert newlines before closing </td> and </th> so html_text2() treats
  # cells as block-level content instead of squashing them together.
  html |>
    stringr::str_replace_all("</td>", "\n</td>") |>
    stringr::str_replace_all("</th>", "\n</th>")
}

.post_ntnu <- function(txt) {
  # Strip JS artifacts from timetable widget (toggleRooms, etc.)
  txt <- stringr::str_remove(txt, "(?s)Vis detaljert timeplan.*$")
  stringr::str_trim(txt)
}

.pre_uit <- function(txt) {
  txt |>
    # Banner on plans from earlier semesters
    stringr::str_remove(paste0("^\\s*(?:OBS! Dette emnet tilhører et tidligere semester / år|",
                               "NOTE! This course belongs to a previous semester/year)\\s*")) |>
    # From a plan component that failed to render, or the year picker and the
    # contact block (staff names, titles, e-mails), to the end (#218)
    stringr::str_remove(paste0("(?s)\\n?(?:Error rendering component|Andre år og semester|",
                               "Previous years and semesters|Kontakt oss)\\b.*$")) |>
    # A heading left alone means the plan itself did not render
    stringr::str_remove("^(?:Om emnet|About the course)\\s*$") |>
    stringr::str_trim()
}

extract_nla_json <- function(raw_html, academic_year) {
  safe <- purrr::possibly(.extract_nla_json_one, otherwise = NA_character_)
  purrr::map2_chr(raw_html, academic_year, safe)
}

.extract_nla_json_one <- function(raw_html, academic_year) {
  if (is.na(raw_html) || !nzchar(raw_html)) return(NA_character_)
  if (is.na(academic_year)) return(NA_character_)

  doc <- rvest::read_html(raw_html)
  scripts <- rvest::html_elements(doc, "script")
  script_texts <- rvest::html_text(scripts)

  idx <- grep("EmneplanPage", script_texts, fixed = TRUE)
  if (length(idx) == 0) return(NA_character_)

  script_text <- script_texts[idx[1]]

  # Extract the JSON object from the script tag
  json_match <- regmatches(script_text, regexpr("\\{.*\\}", script_text))
  if (length(json_match) == 0) return(NA_character_)

  parsed <- jsonlite::fromJSON(json_match, simplifyVector = FALSE)
  items <- parsed$props$items
  if (is.null(items)) return(NA_character_)

  year_data <- items[[academic_year]]
  if (is.null(year_data)) return(NA_character_)

  parts <- character()

  # Extract title
  if (!is.null(year_data$title) && nzchar(year_data$title)) {
    parts <- c(parts, year_data$title)
  }

  # Extract table items: "title: content" lines
  if (!is.null(year_data$table)) {
    for (item in year_data$table) {
      if (!is.null(item$title) && !is.null(item$content) && nzchar(item$content)) {
        parts <- c(parts, paste0(item$title, ": ", item$content))
      }
    }
  }

  # Extract accordion items: title + html_text of content
  if (!is.null(year_data$accordions)) {
    for (item in year_data$accordions) {
      section_parts <- character()
      if (!is.null(item$title) && nzchar(item$title)) {
        section_parts <- c(section_parts, item$title)
      }
      if (!is.null(item$content) && nzchar(item$content)) {
        content_doc <- rvest::read_html(paste0("<div>", item$content, "</div>"))
        content_text <- rvest::html_text2(rvest::html_element(content_doc, "div"))
        if (!is.na(content_text) && nzchar(content_text)) {
          section_parts <- c(section_parts, content_text)
        }
      }
      if (length(section_parts) > 0) {
        parts <- c(parts, paste(section_parts, collapse = "\n"))
      }
    }
  }

  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = "\n\n")
}

#' Parse UiS semester dropdown to discover available years and their URLs
#'
#' @param raw_html Character string of the base course page HTML
#' @return A tibble with columns: label, url, type ('html' or 'pdf'), year (integer)
.parse_uis_semester_dropdown <- function(raw_html) {
  if (is.na(raw_html) || !nzchar(raw_html)) {
    return(tibble::tibble(label = character(), url = character(),
                          type = character(), year = integer()))
  }

  doc <- rvest::read_html(raw_html)
  sel <- rvest::html_element(doc, "select#fs-semester-select")
  if (length(sel) == 0) {
    return(tibble::tibble(label = character(), url = character(),
                          type = character(), year = integer()))
  }

  options <- rvest::html_elements(sel, "option")
  if (length(options) == 0) {
    return(tibble::tibble(label = character(), url = character(),
                          type = character(), year = integer()))
  }

  values <- rvest::html_attr(options, "value")
  labels <- trimws(rvest::html_text(options))

  # Determine type: absolute URLs starting with http are PDFs, relative are HTML
  type <- dplyr::if_else(grepl("^https?://", values), "pdf", "html")

  # For HTML options, prepend base URL
  urls <- dplyr::if_else(
    type == "html",
    paste0("https://www.uis.no", values),
    values
  )

  # Extract year from label: "2025 - 2026" → 2025 (autumn start year)
  year <- as.integer(stringr::str_extract(labels, "^\\d{4}"))

  # Parse data-older JSON for even older entries
  older_json <- rvest::html_attr(sel, "data-older")
  if (!is.na(older_json) && older_json != "[]" && nzchar(older_json)) {
    older <- jsonlite::fromJSON(older_json, simplifyDataFrame = FALSE)
    if (length(older) > 0) {
      older_labels <- purrr::map_chr(older, "label", .default = NA_character_)
      older_urls   <- purrr::map_chr(older, "url", .default = NA_character_)
      older_years  <- as.integer(stringr::str_extract(older_labels, "^\\d{4}"))
      older_type   <- rep("pdf", length(older))

      labels <- c(labels, older_labels)
      urls   <- c(urls, older_urls)
      type   <- c(type, older_type)
      year   <- c(year, older_years)
    }
  }

  tibble::tibble(label = labels, url = urls, type = type, year = year)
}

#' Extract text from a PDF file
#'
#' @param pdf_raw Raw bytes of a PDF file (from httr2 response body)
#' @return Character string of extracted text
extract_fulltext_pdf <- function(pdf_raw) {
  safe <- purrr::possibly(.extract_fulltext_pdf_one, otherwise = NA_character_)
  purrr::map_chr(pdf_raw, safe)
}

.extract_fulltext_pdf_one <- function(pdf_raw) {
  if (is.null(pdf_raw) || length(pdf_raw) == 0) return(NA_character_)

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(pdf_raw, tmp)

  pages <- pdftools::pdf_text(tmp)
  if (length(pages) == 0) return(NA_character_)

  txt <- paste(pages, collapse = "\n")
  txt <- stringr::str_replace_all(txt, "\\n{3,}", "\n\n")
  txt <- stringr::str_trim(txt)
  if (!nzchar(txt)) NA_character_ else txt
}

.cleanup_usn_text <- function(raw_html) {
  raw_html |>
    stringr::str_remove_all("keyboard_backspace") |>
    stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}
