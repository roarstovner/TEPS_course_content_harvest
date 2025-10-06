# ============================================================
# R/mf/parse_html_mf.R
# Parser for MF (Norwegian School of Theology, Religion and Society)
# Drupal 10 structure with accordion sections
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_mf <- function(file_path, cfg = NULL) {
  # 1. Read HTML
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML:", file_path)
    return("")
  }
  
  # 2. Select the main article container
  main <- html_element(page, "article.template-study-subject, main, #content")
  if (is.na(main) || length(main) == 0) main <- page
  
  # 3. Remove unwanted parts (navigation, footer, etc.)
  exclude <- c("nav", "footer", "header", ".breadcrumbs", ".related", ".pager")
  for (sel in exclude) {
    nodes <- html_elements(main, sel)
    if (length(nodes)) xml_remove(nodes)
  }
  
  # 4. Extract title
  title_node <- html_element(main, "h1 span, h1")
  title <- if (length(title_node)) html_text2(title_node) else basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Extract course metadata (code, credits, semester, language)
  facts <- html_elements(main, "ul.template-study-subject__details li")
  if (length(facts)) {
    fact_text <- facts |> html_text2() |> trimws()
    if (length(fact_text)) {
      fact_section <- paste0("## Course details\n\n", paste(fact_text, collapse = "\n"))
      out <- c(out, fact_section)
    }
  }
  
  # 6. Extract introductory description (before accordions)
  intro_pars <- html_elements(main, "div.field--name-field-study-description p")
  if (length(intro_pars)) {
    intro_text <- intro_pars |> html_text2() |> paste(collapse = "\n\n")
    if (nchar(intro_text) > 0) {
      out <- c(out, "## Description", intro_text)
    }
  }
  
  # 7. Extract accordion sections (each collapsible block)
  accordions <- html_elements(main, ".accordion, .accordion-item")
  for (acc in accordions) {
    heading_node <- html_element(acc, ".accordion-header, h2, h3")
    heading <- if (length(heading_node)) html_text2(heading_node) else ""
    content_node <- html_element(acc, ".accordion-body, .accordion-content, .accordion__content")
    if (!length(content_node)) content_node <- acc
    paragraphs <- html_elements(content_node, "p, ul, ol")
    section_text <- character()
    for (p in paragraphs) {
      tag <- xml_name(p)
      if (tag %in% c("ul", "ol")) {
        items <- html_elements(p, "li") |> html_text2()
        section_text <- c(section_text, paste0("- ", items))
      } else {
        txt <- html_text2(p)
        if (nchar(txt) > 0) section_text <- c(section_text, txt)
      }
    }
    if (nchar(heading) > 0 && length(section_text)) {
      out <- c(out, paste0("## ", heading), paste(section_text, collapse = "\n\n"))
    }
  }
  
  # 8. Clean whitespace and remove footer noise
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  remove_patterns <- c("(?i)privacy", "(?i)cookies", "(?i)personvern", "(?i)tilgjengelighet")
  for (pat in remove_patterns) txt <- gsub(pat, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
