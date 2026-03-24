# R/extract_fulltext.R

# CSS selectors by extraction mode
.selectors <- list(
  single = list(
    oslomet  = "#main-content",
    uia      = "#right-main",
    ntnu     = "#content",
    inn      = ".content-inner",
    hvl      = ".l-2-col__main-content",
    hivolda  = "article.content-emweb",
    mf       = "main",

    nih      = ".fs-body",
    nmbu     = ".layout",
    hiof     = "#vrtx-fs-emne-content, main .entry-content, .entry-content",
    uio      = "#vrtx-course-content"
  ),
  many = list(
    nord = "#ac-trigger-0, #ac-trigger-1, #ac-trigger-2, #ac-trigger-3, #ac-trigger-4,
            #ac-trigger-5, #ac-trigger-6, #ac-trigger-7, #ac-trigger-8,
            .ac-panel--inner, #ac-panel-2 .field__item, #ac-panel-0 li, p, .placeholder-text",
    uib  = ".accordion, .accordion__main, .vertical-reset-children .vertical-reset-children div,
            summary, #main-content li, p, .vertical-reset-children .vertical-reset-children .mt-12",
    uis  = "#block-page-content .link--, #block-page-content .paragraph--with-title",
    uit  = ".hovedfelt > main > div.col-md-12"
  )
)

extract_fulltext <- function(institution_short, raw_html) {
  safe_extract_one  <- purrr::possibly(.extract_one,  otherwise = NA_character_)
  safe_extract_many <- purrr::possibly(.extract_many, otherwise = NA_character_)

  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
    if (is.na(html) || !nzchar(html)) return(NA_character_)

    if (inst == "usn") return(.cleanup_usn_text(html))

    if (inst %in% names(.selectors$single)) {
      if (inst %in% c("hivolda", "inn")) html <- .add_table_cell_breaks(html)
      txt <- safe_extract_one(html, .selectors$single[[inst]])
      if (inst == "ntnu" && !is.na(txt)) txt <- .post_ntnu(txt)
      txt
    } else if (inst %in% names(.selectors$many)) {
      txt <- safe_extract_many(html, .selectors$many[[inst]])
      if (inst == "uit" && !is.na(txt)) txt <- .pre_uit(txt)
      txt
    } else {
      NA_character_
    }
  }, .progress = "Extracting fulltext")
}

.extract_one <- function(raw_html, css) {
  doc <- rvest::read_html(raw_html)
  node <- rvest::html_element(doc, css)
  if (length(node) == 0) return(NA_character_)

  txt <- rvest::html_text2(node)
  if (!nzchar(txt)) NA_character_ else txt
}

.extract_many <- function(raw_html, css) {
  doc <- rvest::read_html(raw_html)
  nodes <- rvest::html_elements(doc, css)
  if (length(nodes) == 0) return(NA_character_)

  txt <- rvest::html_text2(nodes)
  txt <- txt[nzchar(txt)]
  if (length(txt) == 0) return(NA_character_)

  paste(txt, collapse = "\n")
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
  # Strip "Error rendering component" artifact from broken UI widget
  txt <- stringr::str_remove(txt, "(?s)Error rendering component.*$")
  stringr::str_trim(txt)
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
