## code to prepare `test_courses` dataset goes here
source("R/utils.R", local = TRUE)
source("R/extract_fulltext.R", local = TRUE)
source("R/fetch_html_cols.R", local = TRUE)
source("R/institution_config.R", local = TRUE)

courses <- dbh_courses("teacher_ed")

test_courses <- courses |> 
  dplyr::filter(
    year >= 2015,
    !is.na(institution_from_code(institution_code)),
    ) |> 
  dplyr::distinct(institution_name, year, .keep_all = TRUE) |> 
  dplyr::arrange(institution_name, year)


saveRDS(test_courses, "data/input/test_courses.RDS")

