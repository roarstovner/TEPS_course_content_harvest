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
    mf       = "#main",
    nla      = "#content",
    nih      = ".fs-body",
    uit      = ".hovedfelt",
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
    uis  = "#block-page-content .link--, #block-page-content .paragraph--with-title"
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
      safe_extract_many(html, .selectors$many[[inst]])
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

.cleanup_usn_text <- function(raw_html) {
  raw_html |>
    stringr::str_remove_all("keyboard_backspace") |>
    stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    stringr::str_trim()
}
