library(readr)
library(rvest)
library(stringr)
library(glue)


# m?? integrere css-selectors.xlsx i dette
# === Scraping function for OsloMet ===
scrape_oslomet_course <- function(html_string) {
  tryCatch({
    page <- read_html(html_string)
    
    # Get all headers (e.g., "Innhold", "L??ringsutbytte", etc.)
    headers <- page %>% html_elements("h2.title-text") %>% html_text2()
    
    # Get all corresponding content blocks
    bodies <- page %>% html_elements("div.panel-body") %>% html_text2()
    
    # Check if lengths match
    if (length(headers) != length(bodies)) {
      warning("Mismatch between number of headers and content blocks.")
    }
    
    # Combine sections
    sections <- map2_chr(headers, bodies, ~ glue("### {.x}\n\n{.y}"))
    
    # Join all sections together
    full_text <- paste(sections, collapse = "\n\n---\n\n")
    return(full_text)
    
  }, error = function(e) {
    warning(glue("Failed to scrape OsloMet HTML: {e$message}"))
    return(NA_character_)
  })
}


# m?? integrere generate_courses_urls i dette url'n
url <- "https://student.oslomet.no/studier/-/studieinfo/emne/MGVM4100/2025/H%C3%98ST"
html_raw <- read_file(url)
scraped_text <- scrape_oslomet_course(html_raw)

cat(substr(scraped_text, 1, 1000))  # Forhaandsvisning

writeLines(scraped_text, "oslomet_MGVM4100.txt")

