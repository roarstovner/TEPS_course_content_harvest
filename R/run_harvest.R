library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

courses <- courses |> 
  filter(
    institution_short == "hivolda",
    Årstall %in% c(2017)
  ) |> 
  add_course_id() |> 
  validate_courses("initial") |>
  add_course_url() |> 
  validate_courses("with_url")


courses <- fetch_html_with_checkpoint(
  courses,
  checkpoint_path = "data/checkpoint/html_hivolda.RDS"
)

fulltext <- extract_fulltext(courses$institution_short, courses$html)
