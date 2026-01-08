library(rvest); library(glue); library(readr)

scrape_oslomet_course <- function(html_string) {
  tryCatch({
    page <- read_html(html_string)
    headers <- page %>% html_elements("h2.title-text") %>% html_text2()
    bodies  <- page %>% html_elements("div.panel-body") %>% html_text2()
    if (length(headers) != length(bodies)) warning("Mismatch mellom antall headere og blokker.")
    sections <- mapply(function(h,b) paste0("### ", h, "\n\n", b), headers, bodies)
    list(
      course_name_no = page %>% html_element(".page-title") %>% html_text2(),
      full_text = paste(sections, collapse = "\n\n---\n\n")
    )
  }, error = function(e) {
    warning(glue("Failed to scrape OsloMet HTML: {e$message}"))
    list(course_name_no = NA_character_, full_text = NA_character_)
  })
}
