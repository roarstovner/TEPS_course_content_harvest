## code to prepare `test_courses` dataset goes here
courses <- readRDS("data/input/courses.RDS")

source("R/extract_fulltext.R", local = TRUE)
source("R/fetch_html_cols.R", local = TRUE)
source("R/institution_config.R", local = TRUE)

test_courses <- courses |> 
  dplyr::filter(
    Årstall >= 2015,
    !is.na(institution_from_code(Institusjonskode)),
    ) |> 
  dplyr::distinct(Institusjonsnavn, Årstall, .keep_all = TRUE) |> 
  dplyr::arrange(Institusjonsnavn, Årstall)


saveRDS(test_courses, "data/input/test_courses.RDS")

