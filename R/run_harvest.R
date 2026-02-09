library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

# Institutions that need special handling (have their own run_harvest_*.R scripts)
skip_institutions <- c("usn")  # USN requires URL version discovery

for (inst in setdiff(unique(courses$institution_short), skip_institutions)) {

  df <- courses |>
    filter(
      institution_short == inst
      # evt. år:
      # Årstall %in% 2004:2007
    ) |>
    add_course_id() |>
    validate_courses("initial") |>
    add_course_url() |>
    validate_courses("with_url")
  
  df <- fetch_html_with_checkpoint(
    df,
    checkpoint_path = paste0("data/checkpoint/html_", inst, ".RDS")
  )
  
  df$fulltext <- extract_fulltext(df$institution_short, df$html)
  
  saveRDS(df, file = paste0("data/html_", inst, ".RDS"))
}
