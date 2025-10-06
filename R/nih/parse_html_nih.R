# ============================================================
# R/nih/parse_html_nih.R
# Parser for Norges idrettshøgskole (NIH)
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_nih <- function(file_path, cfg = NULL) {
  # 1. Read HTML safely
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Find main content container
  main <- html_element(page, "#vrtx-fs-emne-content")
  if (length(main) == 0) main <- html_element(page, "main") # fallback
  
  # 3. Remove irrelevant elements
  for (sel in c("nav", "footer", "header", ".breadcrumbs", ".vrtx-login-manage-component"))
    xml_remove(html_elements(main, sel))
  
  # 4. Extract title
  title <- html_text2(html_element(main, "h1"))
  out <- c(paste0("# ", title))
  
  # 5. Extract key facts from dl > div.dg
  facts <- html_elements(main, "#vrtx-fs-emne-facts .dg")
  if (length(facts)) {
    fact_lines <- sapply(facts, function(div) {
      dt <- html_text2(html_element(div, "dt"))
      dd <- html_text2(html_element(div, "dd"))
      if (nchar(dt) > 0 && nchar(dd) > 0) paste0(dt, " ", dd) else NULL
    })
    fact_section <- paste0("## Course facts\n\n", paste(fact_lines, collapse = "\n"))
    out <- c(out, fact_section)
  }
  
  # 6. Extract each content section (h2 + paragraphs/lists)
  fs_body <- html_element(main, ".fs-body")
  if (length(fs_body)) {
    h2_nodes <- html_elements(fs_body, "h2")
    for (h in h2_nodes) {
      header <- html_text2(h)
      sibs <- xml_find_all(h, "following-sibling::*[preceding-sibling::h2[1]=current()]")
      section <- character()
      for (node in sibs) {
        tag <- xml_name(node)
        if (tag %in% c("p", "ul", "ol")) {
          if (tag == "p") {
            section <- c(section, html_text2(node))
          } else {
            items <- html_elements(node, "li") |> html_text2()
            section <- c(section, paste0("- ", items))
          }
        } else if (tag == "h2") break
      }
      if (length(section)) {
        out <- c(out, paste0("## ", header), paste(section, collapse = "\n\n"))
      }
    }
  }
  
  # 7. Clean whitespace and remove footer junk
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengelighet")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
