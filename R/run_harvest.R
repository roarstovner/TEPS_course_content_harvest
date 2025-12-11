library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

ntnu <- courses |>
  filter(
         institution_short == "ntnu",
    # evt. år:
    # Årstall %in% 2004:2007
  ) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |> 
  validate_courses("with_url")

ntnu <- fetch_html_with_checkpoint(
  ntnu,
  checkpoint_path = "data/checkpoint/html_ntnu.RDS"
)

ntnu$fulltext <- extract_fulltext(ntnu$institution_short, ntnu$html)

saveRDS(ntnu, file = "data/html_ntnu.RDS")

