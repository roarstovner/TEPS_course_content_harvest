# ============================================================
# R/uib/parse_html_uib.R
# Parser for University of Bergen (UiB)
# Focuses only on main course content (no sidebar)
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_uib <- function(file_path, cfg = NULL) {
  # 1. Read HTML
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Find main container
  main <- html_element(page, "main.grid-span-main, .box__main, article, #content")
  if (length(main) == 0) main <- page
  
  # 3. Remove irrelevant blocks (menus, footer, etc.)
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".sidebar", ".course__aside")
  for (sel in exclude) xml_remove(html_elements(main, sel))
  
  # 4. Extract title
  title <- html_text2(html_element(main, "h1"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract short summary if available (<p> before first <h2>)
  first_para <- html_element(main, "p")
  if (length(first_para)) {
    txt <- html_text2(first_para)
    if (nchar(txt) > 0) out <- c(out, txt)
  }
  
  # 6. Parse all <h2> and <details> sections
  nodes <- html_elements(main, xpath = ".//*")
  for (node in nodes) {
    tag <- xml_name(node)
    
    if (tag == "h2") {
      out <- c(out, paste0("## ", html_text2(node)))
      
    } else if (tag == "details") {
      summary_node <- html_element(node, "summary, h3")
      summary_text <- if (length(summary_node)) html_text2(summary_node) else "Section"
      content <- html_text2(node)
      content <- gsub(summary_text, "", content, fixed = TRUE)
      if (nchar(content) > 0) {
        out <- c(out, paste0("## ", summary_text), trimws(content))
      }
      
    } else if (tag == "p") {
      txt <- html_text2(node)
      if (nchar(txt) > 0) out <- c(out, txt)
      
    } else if (tag %in% c("ul", "ol")) {
      items <- html_elements(node, "li") |> html_text2()
      if (length(items)) out <- c(out, paste0("- ", items))
    }
  }
  
  # 7. Clean up whitespace
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # 8. Remove noise patterns
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengeligheit")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
