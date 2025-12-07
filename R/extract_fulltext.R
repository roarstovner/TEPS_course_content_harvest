extract_fulltext <- function(institution_short, raw_html) {
  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
         if(is.na(html)) {
          return(NA_character_)
         }

    switch(
      inst,
      "hivolda" = extract_fulltext_hivolda(html),
      "hiof"     = extract_fulltext_hiof(html),
      "uio"     = extract_fulltext_uio(html),
      "unsupported institution"
    )
  })
}

extract_fulltext_hivolda <- function(raw_html) {
  raw_html |> 
    rvest::read_html() |> 
    rvest::html_elements("article.content-emweb") |> 
    rvest::html_text2()
}

extract_fulltext_hiof <- function(raw_html) {
  raw_html |>
    rvest::read_html() |>
    rvest::html_elements(paste(
      "#vrtx-fs-emne-content", 
      "main .entry-content",    
      ".entry-content",         
      sep = ", "
    )) |>
    rvest::html_text2()
}

extract_fulltext_uio <- function(raw_html) {
  raw_html |> 
    rvest::read_html() |> 
    rvest::html_elements("#vrtx-course-content") |> 
    rvest::html_text2()
}

