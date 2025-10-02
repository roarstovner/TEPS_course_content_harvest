## code to prepare `test_courses` dataset goes here
library(dplyr)

courses <- readRDS("data/courses.RDS")

institusjonskoder <- c(
  0236,
  0238,
  0241,
  0256,
  0259,
  0262,
  1110,
  1120,
  1130,
  1150,
  1160,
  1170,
  1171,
  1173,
  1174,
  1175,
  1176,
  1260,
  8221,
  8223
)

test_courses <- courses |> 
  filter(
    Årstall >= 2015,
    Institusjonskode %in% institusjonskoder,
    ) |> 
  slice(which.min(Årstall), which.max(Årstall), .by = "Institusjonsnavn") |> 
  arrange(Institusjonsnavn, Årstall)


saveRDS(test_courses, "data/test_courses.RDS")
