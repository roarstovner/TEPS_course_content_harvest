# ============================================================
# R/hiof/parse_html_hiof.R
# Parser for hiof
# Bruker Vortex-struktur (#vrtx-fs-emne-content)
# ============================================================

library(rvest)
library(xml2)

# ------------------------------------------------------------
# parse_html_hiof()
# ------------------------------------------------------------
parse_html_hiof <- function(file_path, cfg) {
  # --- 1. Les HTML ---
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Kunne ikke lese HTML: ", file_path)
    return("")
  }
  
  # --- 2. Finn hovedinnhold (Vortex container) ---
  main <- html_element(page, "#vrtx-fs-emne-content")
  if (is.na(main) || length(main) == 0) {
    warning("Fant ikke hovedinnhold i: ", basename(file_path))
    main <- page
  }
  
  # --- 3. Fjern uonskede elementer ---
  exclude_selectors <- c(
    "div.vrtx-date-info",
    "div.vrtx-breadcrumbs",
    "header",
    "footer",
    "nav",
    "#vrtx-fs-emne-right-column",
    "#toc"
  )
  for (sel in exclude_selectors) {
    nodes <- html_elements(main, sel)
    if (length(nodes)) xml_remove(nodes)
  }
  
  # --- 4. Hent alle noder i riktig rekkefolge ---
  nodes <- html_elements(main, xpath = "./*")
  out <- character()
  
  for (node in nodes) {
    tag <- xml_name(node)
    
    if (tag %in% c("h1")) {
      out <- c(out, paste0("# ", html_text2(node)))
      
    } else if (tag %in% c("h2")) {
      # mange viktige delseksjoner ligger i H2
      out <- c(out, paste0("## ", html_text2(node)))
      
    } else if (tag %in% c("h3")) {
      out <- c(out, paste0("### ", html_text2(node)))
      
    } else if (tag == "p") {
      text <- html_text2(node)
      if (nchar(text) > 0) out <- c(out, text)
      
    } else if (tag %in% c("ul", "ol")) {
      items <- html_elements(node, "li") |> html_text2()
      if (length(items)) out <- c(out, paste0("- ", items))
      
    } else if (tag == "table") {
      out <- c(out, "[Tabell utelatt]")
      
    } else if (tag == "div") {
      # fallback for div-blokker som inneholder tekst
      txt <- html_text2(node)
      if (nchar(txt) > 0) out <- c(out, txt)
    }
  }
  
  # --- 5. Sett sammen og rydd whitespace ---
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # --- 6. Fjern footer-stoy ---
  remove_patterns <- c(
    "(?i)informasjon om cookies",
    "(?i)tilgjengelighetserklæring",
    "(?i)høgskolen i østfold"
  )
  for (pat in remove_patterns) {
    txt <- gsub(pat, "", txt, perl = TRUE)
  }
  
  # --- 7. Returner ren tekst ---
  return(trimws(txt))
}
