# R/extract_fulltext.R
# The plan text (`extracted_text`) of each harvested page: the page's blocks
# (R/blocks.R) as text, or for pages without HTML the text the harvest kept.

# Parse HTML without script/style text (ntnu's "function toggleRooms(...)"
# ended up in assessment; uit's page scripts) or form widgets
# (uib's semester picker "Vel emnebeskrivelse for semester 2027 Vår ...").
# Used by both fulltext and section extraction.
# html_text2() and the block walker drop a <br> inside an inline element and
# glue the words around it (uit's "Karakterregel:A–E", uib's
# "KunnskaparStudenten"), so such a <br> becomes a space.
# A table is one line per row, cells separated by tabs (an exam table: one
# line per exam). html_text2() reads a table whose rows sit in
# thead/tbody/tfoot as inline text and runs it together
# ("FormAndelHjemmeeksamen40%"), so the wrappers go; a cell's <div>s and
# <br>s would break its row, so each cell becomes its text on one line. No
# section heading sits in a table cell.
.read_doc <- function(html) {
  doc <- rvest::read_html(html)
  xml2::xml_remove(rvest::html_elements(doc, "script, style, noscript, select, label"))
  # MF's editor writes <list><listitem> for lists, which html_text2() runs
  # together ("Luthersk prestetjeneste i dagPastoralteologi")
  xml2::xml_set_name(xml2::xml_find_all(doc, "//list"), "ul")
  xml2::xml_set_name(xml2::xml_find_all(doc, "//listitem"), "li")
  inline_br <- xml2::xml_find_all(doc, paste0("//", .inline_tags, "/br", collapse = " | "))
  xml2::xml_text(inline_br) <- " "
  for (w in rvest::html_elements(doc, "thead, tbody, tfoot")) {
    for (k in xml2::xml_contents(w)) xml2::xml_add_sibling(w, k, .where = "before")
    xml2::xml_remove(w)
  }
  for (cell in rvest::html_elements(doc, "td, th")) {
    txt <- stringr::str_squish(rvest::html_text2(cell))
    xml2::xml_remove(xml2::xml_contents(cell))
    xml2::xml_text(cell) <- txt
  }
  doc
}

#' Extract course plan text from the raw harvest
#'
#' One place for how `extracted_text` is made from what the harvest stored,
#' used by the harvest strategies and by the {targets} pipeline. A page
#' with HTML is read into blocks (R/blocks.R) and the blocks are its text,
#' so the text and the sections come from one reading of the page. USN
#' renders its pages in Chrome and keeps the text; rows the harvest filled from
#' a PDF (uis archive plans, steiner) have no `html`: their stored text is the
#' raw data and is returned as is (the PDF itself was not kept until
#' 2026-10-16; uis keeps it since, in the raw store's `pdf` column).
#'
#' @param df Harvested rows of one institution (`html`, `course_id`;
#'   `extracted_text` for PDF rows).
#' @param config Institution config from get_institution_config().
#' @return Character vector of extracted text, one per row.
extract_fulltext_from_raw <- function(df, config) {
  stored <- df[["extracted_text"]] %||% rep(NA_character_, nrow(df))
  html <- df[["html"]] %||% rep(NA_character_, nrow(df))
  switch(config$strategy,
    noop       = rep(NA_character_, nrow(df)),
    pdf_split  = stored,
    shadow_dom = .cleanup_usn_text(html),
    {
      cfg <- .block_cfg(config)
      # a parsed page is freed (C memory, libxml2) only when R collects its
      # garbage, which it does seldom with a year's pages in memory: a
      # collection every 500 pages keeps the parsed pages from piling up
      vapply(seq_len(nrow(df)), function(i) {
        if (i %% 500L == 0L) gc()
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
#'   cannot leave out (uit's year picker and contact block).
#' @param facts Whether the title and facts box (`facts` blocks) are in the
#'   text. Without them it is the plan text for the plan id, the shell rule
#'   and the sections' text fallback.
#' @return Character(1), NA when there is no text.
page_fulltext <- function(blocks, config, facts = TRUE) {
  if (!facts) blocks <- blocks[!blocks$facts %in% TRUE, ]
  text <- .blocks_text(blocks)
  if (!is.na(text) && !is.null(config$post_fn)) text <- config$post_fn(text)
  if (is.na(text) || !nzchar(trimws(text))) NA_character_ else text
}

.pre_uit <- function(txt) {
  # the responsible unit in the contact block, without its e-mail, stays,
  # on two lines as ntnu has it (a .drop heading, so in no section)
  unit <- stringr::str_match(txt, paste0("\\n(Ansvarlig enhet|Responsible unit):[ \\t]*",
                                         "([^\\n]*?)[ \\t]*(?:E-post:[^\\n]*)?(?:\\n|$)"))
  txt <- txt |>
    # Banner on plans from earlier semesters
    stringr::str_remove(paste0("(?m)^[ \\t]*(?:OBS! Dette emnet tilhører et tidligere semester / år|",
                               "NOTE! This course belongs to a previous semester/year)\\s*")) |>
    # From a plan component that failed to render, or the year picker and the
    # contact block (staff names, titles, e-mails), to the end
    stringr::str_remove(paste0("(?s)\\n?(?:Error rendering component|Andre år og semester|",
                               "Previous years and semesters|Kontakt oss)\\b.*$")) |>
    # A heading left alone means the plan itself did not render
    stringr::str_remove("^(?:Om emnet|About the course)\\s*$") |>
    stringr::str_trim()
  if (is.na(unit[, 3]) || !nzchar(unit[, 3]) || !nzchar(txt)) return(txt)
  paste0(txt, "\n\n", unit[, 2], "\n", unit[, 3])
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
    # Banner left from the page shown before: the resolver keeps only pages
    # whose "Undervisningsstart" is the requested semester, and these show
    # that semester's plan without the banner when loaded again (6 pages)
    stringr::str_remove(paste0("MERK:\\s*Emneplanen for dette semesteret er ikke klar ennå\\.\\s*",
                               "For at du skal få et inntrykk av hva emnet inneholder, viser vi ",
                               "derfor emneplanen fra et tidligere semester\\.\\s*")) |>
    # the same banner on a page in English
    stringr::str_remove(paste0("Note:\\s*The course plan for this semester is not available yet\\.\\s*",
                               "To give you an idea about the course content, here.s the course ",
                               "plan from a previous semester\\.\\s*")) |>
    stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}
