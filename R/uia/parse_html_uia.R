# R/uia/parse_html_uia.R
# ======================
# Parser for Universitetet i Agder (UiA) course descriptions
# Extracts main content from HTML and structures it with Markdown-style headers

parse_html_uia <- function(html_path) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Package 'rvest' required")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' required")
  
  doc <- xml2::read_html(html_path)
  
  # Title
  title <- rvest::html_text(rvest::html_element(doc, "h1"))
  title <- trimws(title)
  
  # Extract main course content
  main <- rvest::html_element(doc, "div.main-text")
  if (is.na(main)) return(paste("#", title, "\n\n(No content found)"))
  
  # Clean unwanted elements
  rvest::html_elements(main, ".vrtx-date-info, .toc, script, style, nav, footer") |>
    xml2::xml_remove()
  
  # Replace <h2> with ## headers
  for (h in rvest::html_elements(main, "h2")) {
    header_text <- trimws(rvest::html_text(h))
    xml2::xml_replace(h, xml2::read_html(paste0("<p>## ", header_text, "</p>")))
  }
  
  # Convert lists to text bullets
  for (ul in rvest::html_elements(main, "ul")) {
    items <- rvest::html_text(rvest::html_elements(ul, "li"))
    if (length(items)) {
      bullet <- paste0("- ", trimws(items), collapse = "\n")
      xml2::xml_replace(ul, xml2::read_html(paste0("<p>", bullet, "</p>")))
    }
  }
  
  # Convert ordered lists (e.g., <ol>) to numbered lists
  for (ol in rvest::html_elements(main, "ol")) {
    items <- rvest::html_text(rvest::html_elements(ol, "li"))
    if (length(items)) {
      numbered <- paste0(seq_along(items), ". ", trimws(items), collapse = "\n")
      xml2::xml_replace(ol, xml2::read_html(paste0("<p>", numbered, "</p>")))
    }
  }
  
  # Remove tables but keep text if present
  for (tbl in rvest::html_elements(main, "table")) {
    txt <- rvest::html_text(tbl)
    xml2::xml_replace(tbl, xml2::read_html(paste0("<p>", txt, "</p>")))
  }
  
  # Extract clean text
  txt <- rvest::html_text(main)
  txt <- gsub("\r", "", txt)
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\n{3,}", "\n\n", txt)
  txt <- trimws(txt)
  
  paste0("# ", title, "\n\n", txt)
}
