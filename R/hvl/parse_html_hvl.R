# ============================================================
# R/hvl/parse_html_hvl.R
# Parser for Høgskulen på Vestlandet (HVL)
# ------------------------------------------------------------
# Leser hovedinnhold fra <main> / .emneplan og
# konverterer h2/h3/p/ul/ol til Markdown.
# ============================================================

library(rvest)
library(xml2)

parse_html_hvl <- function(file_path, cfg = NULL) {
  # --- 1. Les HTML ---
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Klarte ikkje lese HTML:", file_path)
    return("")
  }
  
  # --- 2. Finn hovedinnhold ---
  main <- html_element(page, "main, .emneplan, .emnepage")
  if (is.na(main) || length(main) == 0) main <- page
  
  # --- 3. Fjern uønskede deler ---
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".related", ".share")
  for (sel in exclude) {
    nodes <- html_elements(main, sel)
    if (length(nodes)) xml_remove(nodes)
  }
  
  # --- 4. Hent tittel ---
  title_node <- html_element(main, "h1")
  title <- if (length(title_node)) html_text2(title_node) else basename(file_path)
  out <- c(paste0("# ", title))
  
  # --- 5. Hent innholdsnoder ---
  nodes <- html_elements(main, xpath = ".//*")
  
  for (node in nodes) {
    tag <- xml_name(node)
    
    if (tag == "h2") {
      out <- c(out, paste0("## ", html_text2(node)))
      
    } else if (tag == "h3") {
      out <- c(out, paste0("### ", html_text2(node)))
      
    } else if (tag == "p") {
      txt <- html_text2(node)
      if (nchar(txt) > 0) out <- c(out, txt)
      
    } else if (tag %in% c("ul", "ol")) {
      items <- html_elements(node, "li") |> html_text2()
      if (length(items)) out <- c(out, paste0("- ", items))
      
    } else if (tag == "table") {
      out <- c(out, "[Tabell utelatt]")
      
    } else if (tag == "div") {
      # fang opp fritekst-diver som ikke allerede har struktur
      txt <- html_text2(node)
      if (nchar(txt) > 50 && !grepl("\\{\\{", txt)) { # unngå Angular
        out <- c(out, txt)
      }
    }
  }
  
  # --- 6. Rydd whitespace ---
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # --- 7. Fjern støy (cookie, personvern etc.) ---
  rm_pats <- c(
    "(?i)personvern", "(?i)cookies", "(?i)tilgjengelegheit",
    "(?i)høgskulen på vestlandet"
  )
  for (p in rm_pats) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
