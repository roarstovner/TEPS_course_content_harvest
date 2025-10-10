# ============================================================
# R/nla/parse_html_nla.R
# Parser for NLA Høgskolen course descriptions
# ============================================================

library(rvest)
library(xml2)
library(stringr)

parse_html_nla <- function(file_path, cfg = NULL) {
  # 1. Read HTML safely
  page <- try(read_html(file_path), silent = TRUE)
  if (inherits(page, "try-error")) {
    warning("Could not read HTML: ", file_path)
    return("")
  }
  
  # 2. Identify main container
  main <- html_element(page, "main.l-site__main-content, article.l-article")
  if (length(main) == 0) main <- page
  
  # 3. Clean unwanted blocks (nav, footer, etc.)
  for (sel in c("header", "footer", "nav", ".breadcrumbs", ".byline", ".social-link-list"))
    xml_remove(html_elements(main, sel))
  
  # 4. Title
  title <- html_text2(html_element(main, "h1.l-article__title"))
  if (!nchar(title)) title <- basename(file_path)
  out <- c(paste0("# ", title))
  
  # 5. Basic metadata fields (<p><strong>…</strong> …</p>)
  meta_nodes <- html_elements(main, "p strong")
  if (length(meta_nodes)) {
    meta_text <- sapply(meta_nodes, function(n) {
      label <- html_text2(n)
      parent <- xml_parent(n)
      val <- html_text2(parent)
      val <- gsub(label, "", val, fixed = TRUE)
      paste0(label, " ", str_trim(val))
    })
    meta_text <- meta_text[nzchar(meta_text)]
    out <- c(out, "## Course details", paste(meta_text, collapse = "\n"))
  }
  
  # 6. Main sections (<h1>…</h1> followed by <p>/<ul>)
  body_divs <- html_elements(main, "div, section")
  sections <- html_elements(body_divs, "h1")
  if (length(sections)) {
    for (h in sections) {
      header <- html_text2(h)
      sibs <- xml_find_all(h, "following-sibling::*[preceding-sibling::h1[1]=current()]")
      content <- character()
      for (node in sibs) {
        tag <- xml_name(node)
        if (tag %in% c("p", "ul", "ol")) {
          if (tag == "p") {
            txt <- html_text2(node)
            if (nchar(txt) > 0) content <- c(content, txt)
          } else {
            items <- html_elements(node, "li") |> html_text2()
            content <- c(content, paste0("- ", items))
          }
        } else if (tag == "h1") break
      }
      if (length(content)) {
        out <- c(out, paste0("## ", header), paste(content, collapse = "\n\n"))
      }
    }
  }
  
  # 7. Cleanup
  txt <- paste(out, collapse = "\n\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- gsub("[ \t]+$", "", txt)
  txt <- trimws(txt)
  
  remove_patterns <- c("(?i)personvern", "(?i)cookies", "(?i)vipps", "(?i)facebook")
  for (p in remove_patterns) txt <- gsub(p, "", txt, perl = TRUE)
  
  return(trimws(txt))
}
