# ============================================================
# R/nord/parse_html_nord.R
# Parser for Nord University (Drupal-based CMS)
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_nord <- function(file_path, cfg = NULL) {
  # 1. Read HTML safely
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Identify main content area (article or main)
  main <- html_element(page, "article, main, #content")
  if (length(main) == 0) main <- page
  
  # 3. Remove irrelevant elements
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".menu", ".pager", ".byline")
  for (sel in exclude) xml_remove(html_elements(main, sel))
  
  # 4. Extract course title
  title <- html_text2(html_element(main, "h1"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract metadata (key facts often inside .field__items or <ul> lists)
  fact_nodes <- html_elements(main, ".field--name-field-course-facts li, .field__items li")
  if (length(fact_nodes)) {
    fact_text <- html_text2(fact_nodes)
    fact_section <- paste0("## Course details\n\n", paste(fact_text, collapse = "\n"))
    out <- c(out, fact_section)
  }
  
  # 6. Extract main structured sections (h2/h3 + paragraphs/lists)
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
      out <- c(out, "[Table omitted]")
    } else if (tag == "div") {
      # sometimes content lives directly in <div class="field__item">
      txt <- html_text2(node)
      if (nchar(txt) > 150 && !grepl("\\{\\{", txt)) {
        out <- c(out, txt)
      }
    }
  }
  
  # 7. Clean whitespace and remove junk
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # 8. Remove footer or boilerplate content
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengelighet", "(?i)studentweb")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
