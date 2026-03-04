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
      safe_extract_one(html, .selectors$single[[inst]])
    } else if (inst %in% names(.selectors$many)) {
      txt <- safe_extract_many(html, .selectors$many[[inst]])
      if (inst == "uit" && !is.na(txt)) txt <- .pre_uit(txt)
      txt
    } else {
      NA_character_
    }
  }, .progress = TRUE)
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

.cleanup_usn_text <- function(raw_html) {
  raw_html |>
    stringr::str_remove_all("keyboard_backspace") |>
    stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}
