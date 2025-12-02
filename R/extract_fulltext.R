extract_fulltext <- function(institution_short, raw_html) {
  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
         if(is.na(html)) {
          return(NA_character_)
         }

    switch(
      inst,
      "hivolda" = extract_fulltext_hivolda(html),
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
