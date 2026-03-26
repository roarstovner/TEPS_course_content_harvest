## code to prepare `test_courses` dataset goes here
courses <- readRDS("data/courses.RDS")

source("R/institution_config.R", local = TRUE)
institusjonskoder <- setNames(
  names(institution_configs),
  vapply(institution_configs, \(x) x$code, character(1))
)

test_courses <- courses |> 
  dplyr::filter(
    Årstall >= 2015,
    Institusjonskode %in% names(institusjonskoder),
    ) |> 
  dplyr::distinct(Institusjonsnavn, Årstall, .keep_all = TRUE) |> 
  dplyr::arrange(Institusjonsnavn, Årstall)


saveRDS(test_courses, "data/test_courses.RDS")

