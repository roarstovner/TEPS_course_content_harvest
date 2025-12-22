library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

for (inst in unique(courses$institution_short)) {
  
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
