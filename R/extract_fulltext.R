# R/extract_fulltext.R

extract_fulltext <- function(institution_short, raw_html) {
  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
    if (is.na(html) || !nzchar(html)) {
      return(NA_character_)
    }
    
    switch(
      inst,
      "ntnu"    = extract_fulltext_ntnu(html),
      "uia"     = extract_fulltext_uia(html),
      "uit"     = extract_fulltext_uit(html),
      "uib"     = extract_fulltext_uib(html),
      "nord"    = extract_fulltext_nord(html),
      "hvl"     = extract_fulltext_hvl(html),
      "hiof"    = extract_fulltext_hiof(html),
      "hivolda" = extract_fulltext_hivolda(html),
      "uio"     = extract_fulltext_uio(html),
      
      # fallback
      NA_character_
    )
  })
}

# ----------------------------
# INDIVIDUELLE EXTRACT-FUNKSJONER
# ----------------------------

extract_fulltext_ntnu <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#content") |>
    rvest::html_text2()
}

extract_fulltext_uia <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements(".main-text") |>
    rvest::html_text2()
}

extract_fulltext_uit <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements(".hovedfelt") |>
    rvest::html_text2()
}

extract_fulltext_uib <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements(".accordion , .accordion__main , .vertical-reset-children .vertical-reset-children div , summary , #main-content li , p , .vertical-reset-children .vertical-reset-children .mt-12") |>
    rvest::html_text2()
}

extract_fulltext_nord <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#ac-trigger-0 , #ac-trigger-1 , #ac-trigger-5 , #ac-trigger-8 , #ac-trigger-6 , #ac-trigger-7 , #ac-trigger-2 , #ac-trigger-4 , #ac-trigger-3 , .ac-panel--inner , #ac-panel-2 .field__item , #ac-panel-0 li , p , .placeholder-text") |>
    rvest::html_text2()
}

extract_fulltext_hvl <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements(".l-2-col__main-content") |>
    rvest::html_text2()
}

extract_fulltext_hiof <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#vrtx-fs-emne-content") |>
    rvest::html_text2()
}

extract_fulltext_hivolda <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#main-content") |>
    rvest::html_text2()
}

extract_fulltext_uio <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements("#vrtx-course-content") |>
    rvest::html_text2()
}
