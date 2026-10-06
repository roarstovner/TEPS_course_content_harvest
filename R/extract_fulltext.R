# R/extract_fulltext.R
# The plan text (`extracted_text`) of each harvested page: the page's blocks
# (R/blocks.R) as text, or for pages without HTML the text the harvest kept.

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
#' (#256), used by the harvest strategies and by the {targets} pipeline. A page
#' with HTML is read into blocks (R/blocks.R) and the blocks are its text
#' (#276), so the text and the sections come from one reading of the page. USN
#' renders its pages in Chrome and keeps the text; rows the harvest filled from
#' a PDF (uis archive plans, steiner) have no `html`: the PDF itself is not
#' kept, so their stored text is the raw data and is returned as is.
#'
#' @param df Harvested rows of one institution (`html`, `course_id`;
#'   `extracted_text` for PDF rows).
#' @param config Institution config from get_institution_config().
#' @return Character vector of extracted text, one per row.
extract_fulltext_from_raw <- function(df, config) {
  stored <- df$extracted_text %||% rep(NA_character_, nrow(df))
  html <- df$html %||% rep(NA_character_, nrow(df))
  switch(config$strategy,
    noop       = rep(NA_character_, nrow(df)),
    pdf_split  = stored,
    shadow_dom = .cleanup_usn_text(html),
    {
      cfg <- .block_cfg(config)
      vapply(seq_len(nrow(df)), function(i) {
        if (is.na(html[i]) || !nzchar(html[i])) return(stored[i])
        page_fulltext(page_blocks(html[i], NA, cfg, df$course_id[i]), config)
      }, character(1))
    }
  )
}

#' The extracted_text of a page from its blocks
#'
#' @param blocks Block table from page_blocks().
#' @param config Institution config; its `post_fn` cuts what the blocks
#'   cannot leave out (ntnu's timetable, uit's year picker and contact block).
#' @return Character(1), NA when there is no text.
page_fulltext <- function(blocks, config) {
  text <- .blocks_text(blocks)
  if (!is.na(text) && !is.null(config$post_fn)) text <- config$post_fn(text)
  if (is.na(text) || !nzchar(trimws(text))) NA_character_ else text
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
