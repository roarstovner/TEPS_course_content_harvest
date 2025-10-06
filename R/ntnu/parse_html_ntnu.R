# ============================================================
# R/ntnu/parse_html_ntnu.R
# Parser for NTNU (Vortex-based course pages)
# Structure similar to HiOF, UiO, NIH
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_ntnu <- function(file_path, cfg = NULL) {
  # 1. Read HTML safely
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Identify main content
  main <- html_element(page, "#vrtx-course-content, #course-content, article, main, #content")
  if (length(main) == 0) main <- page
  
  # 3. Remove irrelevant elements (nav/footer etc.)
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".menu", ".vrtx-login-manage-component")
  for (sel in exclude) xml_remove(html_elements(main, sel))
  
  # 4. Extract course title
  title <- html_text2(html_element(main, "h1"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract metadata (facts) if present
  facts <- html_elements(main, "#vrtx-course-facts .fs-table, .courseFacts, .vrtx-coursefacts .dg")
  if (length(facts)) {
    fact_lines <- html_elements(facts, "tr, div.dg") |> 
      lapply(function(n) {
        dt <- html_text2(html_element(n, "th, dt"))
        dd <- html_text2(html_element(n, "td, dd"))
        if (nchar(dt) > 0 && nchar(dd) > 0) paste0(dt, ": ", dd) else NULL
      }) |> unlist()
    if (length(fact_lines)) {
      out <- c(out, "## Course details", paste(fact_lines, collapse = "\n"))
    }
  }
  
  # 6. Extract structured sections (h2/h3 + paragraphs/lists)
  content_nodes <- html_elements(main, xpath = ".//*")
  for (node in content_nodes) {
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
      if (nchar(txt) > 150 && !grepl("\\{\\{", txt)) {
        out <- c(out, txt)
      }
    }
  }
  
  # 7. Clean whitespace
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  # 8. Remove footer noise
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)tilgjengelighet", "(?i)studentweb")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
