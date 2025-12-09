library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

uio_all <- courses |>
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
    institution_short == "uio",
  ) |>
  add_course_id() |>
  add_course_url()


# UiO does not store old versions of their Emneplan, so you only need the distinct urls created.
# This makes historical UiO data rather limited, and looking up their Emneplan might give the wrong result.
# I therefore only keep the latest version of each individual url.
uio <- uio_all |>
  slice_max(Årstall, n = 1, with_ties = FALSE, by = "url")

uio <- fetch_html_with_checkpoint(
  uio,
  checkpoint_path = "data/checkpoint/html_uio.RDS"
)

uio$fulltext <- extract_fulltext(uio$institution_short, uio$html)

uio <- uio |>
  select(course_id, html, html_error, html_success, fulltext) |> 
  right_join(uio_all, by = "course_id") |> 
  relocate(contains("html"), fulltext, .after = last_col())

saveRDS(
  uio,
  file = "data/html_uio.RDS"
)
