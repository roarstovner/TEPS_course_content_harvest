# ============================================================
# R/uis/parse_html_uis.R
# Parser for University of Stavanger (UiS)
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_uis <- function(file_path, cfg = NULL) {
  # 1. Read HTML safely
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Identify main article container
  main <- html_element(page, "article.course, main, #content, .region-content")
  if (length(main) == 0) main <- page
  
  # 3. Remove non-relevant elements
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".menu", ".pager", ".byline")
  for (sel in exclude) xml_remove(html_elements(main, sel))
  
  # 4. Extract course title
  title <- html_text2(html_element(main, "h1, .page-title"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract key metadata (course facts table or list)
  facts <- html_elements(main, "dl.course-facts, .course-facts, table")
  if (length(facts)) {
    fact_lines <- html_elements(facts, "tr, div, dt") |> 
      lapply(function(n) {
        dt <- html_text2(html_element(n, "th, dt"))
        dd <- html_text2(html_element(n, "td, dd"))
        if (nchar(dt) > 0 && nchar(dd) > 0) paste0(dt, ": ", dd) else NULL
      }) |> unlist()
    if (length(fact_lines)) {
      out <- c(out, "## Course details", paste(fact_lines, collapse = "\n"))
    }
  }
  
  # 6. Extract main content sections (headings + text)
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
    } else if (tag == "div") {
      # sometimes UiS stores free text in generic <div>
      txt <- html_text2(node)
      if (nchar(txt) > 150 && !grepl("\\{\\{", txt)) out <- c(out, txt)
    }
  }
  
  # 7. Clean and normalize whitespace
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # 8. Remove legal/footer text
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengelighet", "(?i)universitetet i stavanger")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
