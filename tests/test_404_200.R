# Mini-sjekk av to UiA-URLer med h1
suppressPackageStartupMessages({
  library(httr); library(xml2); library(rvest)
})

urls <- c(
  "https://www.uia.no/studier/emner/2025/host/rel416.html",
  "https://www.uia.no/studier/emner/2026/var/rel416.html"
)

ua  <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36")
hdr <- httr::add_headers(`Accept-Language` = "nb-NO,nb;q=0.9")

check_one <- function(u, code_pattern = "rel\\s*-?416") {
  out <- list(url = u, status = NA_integer_, type = NA_character_,
              h1 = NA_character_, soft_404 = NA, bytes = NA_integer_,
              mentions_code = NA, ok = FALSE)
  resp <- try(httr::GET(u, ua, hdr, httr::timeout(20)), silent = TRUE)
  if (inherits(resp, "try-error")) return(out)
  
  out$status <- httr::status_code(resp)
  out$type   <- httr::http_type(resp)
  
  # Krev HTML + 2xx/3xx for videre sjekk
  if (!(out$status >= 200 && out$status < 400 && identical(out$type, "text/html"))) return(out)
  
  # Parse fra RAW for minst mulig encoding-tr??bbel
  doc <- try(xml2::read_html(httr::content(resp, "raw")), silent = TRUE)
  if (inherits(doc, "try-error")) return(out)
  
  # Hent <h1>
  h1_node <- rvest::html_element(doc, "h1")
  out$h1  <- if (!is.na(h1_node)) rvest::html_text2(h1_node) else NA_character_
  
  # ???Soft-404??? deteksjon basert p?? h1
  h1_lc <- tolower(trimws(ifelse(is.na(out$h1), "", out$h1)))
  out$soft_404 <- grepl("siden ble ikke funnet|page not found|404", h1_lc, fixed = FALSE)
  
  # Ta hovedinnhold og m??l litt lengde
  xml2::xml_find_all(doc, ".//script|.//style|.//nav|.//footer") |> xml2::xml_remove()
  body <- rvest::html_element(doc, "main, #content, body")
  txt  <- if (!is.na(body)) trimws(rvest::html_text2(body)) else ""
  out$bytes <- nchar(txt, type = "bytes")
  
  # Sjekk om teksten nevner emnekoden (tillater ???rel416??? / ???rel-416??? / ???REL416???)
  out$mentions_code <- grepl(code_pattern, txt, ignore.case = TRUE)
  
  # Enkel OK-regel: ikke soft-404, og enten passende lengde eller koden nevnes
  MIN_BYTES <- 120L
  out$ok <- (!out$soft_404) && (out$bytes >= MIN_BYTES || isTRUE(out$mentions_code))
  out
}

res <- do.call(rbind, lapply(urls, function(u) {
  as.data.frame(check_one(u), optional = TRUE, stringsAsFactors = FALSE)
}))

print(res[, c("url","status","h1","soft_404","bytes","mentions_code","ok")], row.names = FALSE)
