# R/extract_fulltext.R

extract_fulltext <- function(institution_short, raw_html) {
  
  safe_extract_oslomet <- purrr::possibly(extract_fulltext_oslomet, otherwise = NA_character_)
  safe_extract_uia     <- purrr::possibly(extract_fulltext_uia,     otherwise = NA_character_)
  safe_extract_ntnu    <- purrr::possibly(extract_fulltext_ntnu,    otherwise = NA_character_)
  safe_extract_inn     <- purrr::possibly(extract_fulltext_inn,     otherwise = NA_character_)
  
  safe_extract_hivolda <- purrr::possibly(extract_fulltext_hivolda, otherwise = NA_character_)
  safe_extract_hiof    <- purrr::possibly(extract_fulltext_hiof,    otherwise = NA_character_)
  safe_extract_hvl     <- purrr::possibly(extract_fulltext_hvl,     otherwise = NA_character_)
  
  safe_extract_mf      <- purrr::possibly(extract_fulltext_mf,      otherwise = NA_character_)
  safe_extract_nla     <- purrr::possibly(extract_fulltext_nla,     otherwise = NA_character_)
  
  safe_extract_nord    <- purrr::possibly(extract_fulltext_nord,    otherwise = NA_character_)
  safe_extract_nih     <- purrr::possibly(extract_fulltext_nih,     otherwise = NA_character_)
  
  safe_extract_uib     <- purrr::possibly(extract_fulltext_uib,     otherwise = NA_character_)
  safe_extract_uio     <- purrr::possibly(extract_fulltext_uio,     otherwise = NA_character_)
  
  safe_extract_uis     <- purrr::possibly(extract_fulltext_uis,     otherwise = NA_character_)
  safe_extract_usn     <- purrr::possibly(extract_fulltext_usn,     otherwise = NA_character_)
  safe_extract_uit     <- purrr::possibly(extract_fulltext_uit,     otherwise = NA_character_)
  safe_extract_nmbu    <- purrr::possibly(extract_fulltext_nmbu,    otherwise = NA_character_)
  
  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
    if (is.na(html) || !nzchar(html)) return(NA_character_)
    
    switch(
      inst,
      "oslomet" = safe_extract_oslomet(html),
      "uia"     = safe_extract_uia(html),
      "ntnu"    = safe_extract_ntnu(html),
      "inn"     = safe_extract_inn(html),
      
      "hivolda" = safe_extract_hivolda(html),
      "hiof"    = safe_extract_hiof(html),
      "hvl"     = safe_extract_hvl(html),
      
      "mf"      = safe_extract_mf(html),
      "nla"     = safe_extract_nla(html),
      
      "nord"    = safe_extract_nord(html),
      "nih"     = safe_extract_nih(html),
      
      "uib"     = safe_extract_uib(html),
      "uio"     = safe_extract_uio(html),

      "uis"     = safe_extract_uis(html),
      "usn"     = extract_fulltext_usn(html),
      "uit"     = safe_extract_uit(html),
      "nmbu"    = safe_extract_nmbu(html),

      NA_character_
    )
  }, .progress = TRUE)
}

.extract_one <- function(raw_html, css) {
  doc <- rvest::read_html(raw_html)
  node <- rvest::html_element(doc, css)
  if (length(node) == 0) return(NA_character_)
  
  txt <- rvest::html_text2(node)
  txt <- stringr::str_squish(txt)
  if (!nzchar(txt)) NA_character_ else txt
}

.extract_many <- function(raw_html, css) {
  doc <- rvest::read_html(raw_html)
  nodes <- rvest::html_elements(doc, css)
  if (length(nodes) == 0) return(NA_character_)
  
  txt <- rvest::html_text2(nodes)
  txt <- stringr::str_squish(txt)
  txt <- txt[nzchar(txt)]
  if (length(txt) == 0) return(NA_character_)
  
  paste(txt, collapse = "\n")
}

extract_fulltext_oslomet <- function(raw_html) .extract_one(raw_html, "#main-content")
extract_fulltext_uia     <- function(raw_html) .extract_one(raw_html, ".main-text")
extract_fulltext_ntnu    <- function(raw_html) .extract_one(raw_html, "#content")
extract_fulltext_inn     <- function(raw_html) .extract_one(raw_html, ".content-inner")
extract_fulltext_hvl     <- function(raw_html) .extract_one(raw_html, ".l-2-col__main-content")

extract_fulltext_mf      <- function(raw_html) .extract_one(raw_html, "#main")
extract_fulltext_nla     <- function(raw_html) .extract_one(raw_html, "#content")

extract_fulltext_nih     <- function(raw_html) .extract_one(raw_html, ".fs-body")
extract_fulltext_uit     <- function(raw_html) .extract_one(raw_html, ".hovedfelt")
extract_fulltext_nmbu    <- function(raw_html) .extract_one(raw_html, ".layout")

extract_fulltext_nord <- function(raw_html) {
  .extract_many(
    raw_html,
    "#ac-trigger-0, #ac-trigger-1, #ac-trigger-2, #ac-trigger-3, #ac-trigger-4,
     #ac-trigger-5, #ac-trigger-6, #ac-trigger-7, #ac-trigger-8,
     .ac-panel--inner, #ac-panel-2 .field__item, #ac-panel-0 li, p, .placeholder-text"
  )
}

extract_fulltext_uib <- function(raw_html) {
  .extract_many(
    raw_html,
    ".accordion, .accordion__main, .vertical-reset-children .vertical-reset-children div,
     summary, #main-content li, p, .vertical-reset-children .vertical-reset-children .mt-12"
  )
}

extract_fulltext_uis <- function(raw_html) {
  .extract_many(
    raw_html,
    "#block-page-content .link--, #block-page-content .paragraph--with-title"
  )
}

extract_fulltext_hiof <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    # Alekandras
    rvest::html_elements(paste(
      "#vrtx-fs-emne-content",
      "main .entry-content",
      ".entry-content",
      sep = ", "
    )) |>
    # Min
    # rvest::html_elements("#vrtx-fs-emne-content") |>
    rvest::html_text2() |>
    purrr::pluck(1, .default = NA_character_)
}

# THIS WAS LOST IN A MERGE. POSSIBLY THE CORRECT HIVOLDA VERSION?
# extract_fulltext_hivolda <- function(raw_html) {
#   raw_html |>
#     rvest::read_html() |>
#     rvest::html_elements("article.content-emweb") |>

extract_fulltext_hivolda <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#main-content") |>
    rvest::html_text2() |>
    purrr::pluck(1, .default = NA_character_)
}

extract_fulltext_uio <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#vrtx-course-content") |>
    rvest::html_text2() |>
    purrr::pluck(1, .default = NA_character_)
}

extract_fulltext_usn <- function(raw_html) {
  # USN "html" is pre-extracted text from the course content shadow DOM.
  if (is.na(raw_html) || !nzchar(raw_html)) {
    return(NA_character_)
  }

  raw_html |>
    # Remove UI artifacts (Material Icons text)
    stringr::str_remove_all("keyboard_backspace") |>
    # Trim whitespace from each line
    stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
    # Collapse multiple blank lines to single blank line
    stringr::str_replace_all("\\n{3,}", "\n\n") |>
    # Trim leading/trailing whitespace
    stringr::str_trim()
}
