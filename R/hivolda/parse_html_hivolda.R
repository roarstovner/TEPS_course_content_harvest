# ============================================================
# R/hivolda/parse_html_hivolda.R
# Parser for Hogskulen i Volda (Drupal-basert CMS)
# ------------------------------------------------------------
# Leser HTML fra <article>, henter field-seksjoner og
# konverterer til Markdown.
# ============================================================

library(rvest)
library(xml2)

parse_html_hivolda <- function(file_path, cfg) {
  
  # --- 1. Les HTML ---
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Kunne ikkje lese HTML:", file_path)
    return("")
  }
  
  # --- 2. Finn hovedinnhold (Drupal-article) ---
  main <- html_element(page, "article, main, #content")
  if (is.na(main) || length(main) == 0) {
    warning("Fann ikkje article/main i:", basename(file_path))
    main <- page
  }
  
  # --- 3. Fjern element (navigasjon, footer, osv.) ---
  exclude_selectors <- c("nav", "footer", "header", ".breadcrumbs", ".pager")
  for (sel in exclude_selectors) {
    nodes <- html_elements(main, sel)
    if (length(nodes)) xml_remove(nodes)
  }
  
  # --- 4. Hent tittel ---
  title_node <- html_element(main, "h1, .page-title")
  title <- if (length(title_node)) html_text2(title_node) else basename(file_path)
  out <- c(paste0("# ", title))
  
  # --- 5. Finn alle seksjoner (div.field eller section) ---
  sections <- html_elements(main, ".field, section")
  
  for (sec in sections) {
    # 5a. Hent label
    label_node <- html_element(sec, ".field__label, .label, h2, h3")
    label <- if (length(label_node)) html_text2(label_node) else ""
    
    # 5b. Hent innhold
    content_nodes <- html_elements(sec, ".field__item, .item, p, ul, ol")
    content_texts <- character()
    for (cn in content_nodes) {
      tag <- xml_name(cn)
      if (tag %in% c("ul", "ol")) {
        items <- html_elements(cn, "li") |> html_text2()
        content_texts <- c(content_texts, paste0("- ", items))
      } else {
        txt <- html_text2(cn)
        if (nchar(txt) > 0) content_texts <- c(content_texts, txt)
      }
    }
    
    # 5c. Append til utdata
    if (nchar(label) > 0) out <- c(out, paste0("## ", label))
    if (length(content_texts)) out <- c(out, paste(content_texts, collapse = "\n\n"))
  }
  
  # --- 6. Rydd whitespace og unødige linjer ---
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # --- 7. Fjern vanlige footer-stoy dersom det finnes ---
  remove_patterns <- c(
    "(?i)kontakt oss",
    "(?i)tilgjengelegheitserklæring",
    "(?i)personvern",
    "(?i)informasjonskapslar"
  )
  for (pat in remove_patterns) {
    txt <- gsub(pat, "", txt, perl = TRUE)
  }
  
  return(trimws(txt))
}
