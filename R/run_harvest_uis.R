# run_harvest_uis.R
# Harvest pipeline for UiS (University of Stavanger)

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "uis", Årstall == max(Årstall)) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

message("UiS: ", sum(!is.na(df$url)), "/", nrow(df), " URLs generated")

df <- fetch_html_with_checkpoint(
  df,
  checkpoint_path = "data/checkpoint/html_uis.RDS"
)

df$fulltext <- extract_fulltext(df$institution_short, df$html)
saveRDS(df, "data/html_uis.RDS")

message("UiS done: ", sum(!is.na(df$fulltext)), "/", nrow(df), " with fulltext")
