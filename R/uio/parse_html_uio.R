# R/uio/parse_html_uio.R
library(rvest)
library(xml2)
library(stringr)

parse_html_uio <- function(file_path, cfg = NULL) {
  html <- read_html(file_path, encoding = "UTF-8")
  
  title <- html %>%
    html_element("h1") %>%
    html_text(trim = TRUE)
  
  body_nodes <- html %>%
    html_elements("#vrtx-content, #vrtx-main-content, #vrtx-course-content")
  
  text <- body_nodes %>%
    html_text2() %>%
    str_replace_all("\\r|\\n", "\n") %>%
    str_replace_all("\\n{3,}", "\n\n") %>%
    str_squish()
  
  text <- str_replace_all(text, "([A-ZÆØÅ]{2,}\\s*\\d{3,})", "# \\1")
  text <- str_replace_all(text, "(?<=\\n)(Kort om emnet|Hva lærer du|Kva lærer du|Opptak til emnet|Undervisning|Eksamen|Litteratur|Overlapp|Obligatoriske forkunnskaper)", "\n## \\1")
  text <- str_replace_all(text, "\\n{2,}", "\n\n")
  
  paste0("# ", title, "\n\n", text, "\n")
}
