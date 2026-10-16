## code to prepare `test_courses` dataset goes here
source("R/utils.R", local = TRUE)
source("R/extract_fulltext.R", local = TRUE)
source("R/fetch_html_cols.R", local = TRUE)
source("R/institution_config.R", local = TRUE)

courses <- dbh_courses("teacher_ed")

test_courses <- courses |> 
  dplyr::filter(
    Årstall >= 2015,
    !is.na(institution_from_code(Institusjonskode)),
    ) |> 
  dplyr::distinct(Institusjonsnavn, Årstall, .keep_all = TRUE) |> 
  dplyr::arrange(Institusjonsnavn, Årstall)


saveRDS(test_courses, "data/input/test_courses.RDS")

