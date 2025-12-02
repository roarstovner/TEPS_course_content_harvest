## code to prepare `test_courses` dataset goes here
courses <- readRDS("data/courses.RDS")

cfg <- yaml::yaml.load_file(input = "config/institutions.yaml")
institusjonskoder <- cfg$aliases |> unlist()

test_courses <- courses |> 
  dplyr::filter(
    Årstall >= 2015,
    Institusjonskode %in% names(institusjonskoder),
    ) |> 
  dplyr::distinct(Institusjonsnavn, Årstall, .keep_all = TRUE) |> 
  dplyr::arrange(Institusjonsnavn, Årstall)


saveRDS(test_courses, "data/test_courses.RDS")

