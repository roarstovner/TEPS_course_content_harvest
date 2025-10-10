# ============================================================
# R/nmbu/parse_html_nmbu.R
# Parser for NMBU course pages (Next.js-based site)
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_nmbu <- function(file_path, cfg = NULL) {
  # 1. Read HTML
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Locate the main content block
  main <- html_element(page, "main")
  if (length(main) == 0) main <- html_element(page, "#__next")
  if (length(main) == 0) main <- page
  
  # 3. Remove scripts, styles, nav, footer
  exclude <- c("script", "style", "nav", "footer", "header", ".breadcrumbs", ".menu")
  for (sel in exclude) {
    xml_remove(html_elements(main, sel))
  }
  
  # 4. Title
  title <- html_text2(html_element(main, "h1"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract course metadata (if any <dl> or <div> with labels)
  facts <- html_elements(main, "dl, .course-facts, .emnefakta")
  if (length(facts)) {
    dt_nodes <- html_elements(facts, "dt")
    dd_nodes <- html_elements(facts, "dd")
    if (length(dt_nodes) && length(dd_nodes)) {
      dt_texts <- html_text2(dt_nodes)
      dd_texts <- html_text2(dd_nodes)
      lines <- paste(dt_texts, dd_texts, sep = ": ")
      out <- c(out, "## Course details", paste(lines, collapse = "\n"))
    }
  }
  
  # 6. Extract main content sections
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
      txt <- html_text2(node)
      if (nchar(txt) > 100 && !grepl("\\{\\{", txt)) { # ignore templating placeholders
        out <- c(out, txt)
      }
    }
  }
  
  # 7. Clean whitespace
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # 8. Remove noise patterns
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengelighet")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
