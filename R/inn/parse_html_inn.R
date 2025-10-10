# ================================
# parse_html_inn.R
# ================================
# Parser emnebeskrivelser fra INN (Edutorium / Drupal 7)
# ================================

library(rvest)
library(xml2)
library(stringr)

parse_html_inn <- function(file_path) {
  # Les HTML
  html <- read_html(file_path)
  
  # Hent tittel
  title <- html %>%
    html_element("h1#page-title") %>%
    html_text(trim = TRUE)
  
  # Hent alle seksjoner i hovedinnholdet
  content_nodes <- html %>%
    html_elements("div.content > div.field-course-content, 
                  div.content > div[class^='field-learning-outcome'],
                  div.content > div[class^='field-work-'],
                  div.content > div[class^='field-assessment'],
                  div.content > div.field-workload,
                  div.content > div.field-faculties-ref,
                  div.content > div.field-study-programs-ref,
                  div.content > div.field-reading-list-link")
  
  # Rydd opp tekst
  sections <- lapply(content_nodes, function(node) {
    heading <- html_element(node, "div.label, h2.label") %>%
      html_text(trim = TRUE)
    body <- html_text(node, trim = TRUE)
    body <- gsub("\\s{3,}", "\n", body)
    paste0("## ", heading, "\n\n", body)
  })
  
  # Hent faktaboks ("Emnekode", "Studiepoeng", etc.)
  facts <- html %>%
    html_elements("#facts-toc .facts-label, #facts-toc .facts-item") %>%
    html_text(trim = TRUE)
  
  if (length(facts) > 0) {
    fact_pairs <- paste(facts[c(TRUE, FALSE)], facts[c(FALSE, TRUE)], sep=": ", collapse="\n")
    fact_section <- paste0("## Emnefakta\n\n", fact_pairs)
  } else {
    fact_section <- ""
  }
  
  # Sett sammen alt
  text_out <- paste(
    paste0("# ", title),
    fact_section,
    paste(sections, collapse = "\n\n"),
    sep = "\n\n"
  )
  
  # Fjern overflodig whitespace
  text_out <- str_squish(text_out)
  text_out <- gsub("\n{3,}", "\n\n", text_out)
  
  return(text_out)
}
