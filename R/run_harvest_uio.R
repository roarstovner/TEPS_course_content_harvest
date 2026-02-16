library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

# UiO only publishes current course plans (no historical versions).
# Filter to current year only to avoid applying current content to old rows.
uio <- courses |>
  filter(institution_short == "uio", Årstall == max(Årstall)) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

uio <- fetch_html_with_checkpoint(
  uio,
  checkpoint_path = "data/checkpoint/html_uio.RDS"
)

uio$fulltext <- extract_fulltext(uio$institution_short, uio$html)

saveRDS(uio, "data/html_uio.RDS")

message("UiO done: ", sum(!is.na(uio$fulltext)), "/", nrow(uio), " with fulltext")