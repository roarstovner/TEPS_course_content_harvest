## code to prepare `courses` dataset goes here

library(rdbhapi)
library(tidyverse)

studieprogram <- dbh_data(
  347, # dbh-tabell: Studieprogram
  filters = list(
    "Studiumkode" = c("INTMASTER", "IMALU5-10", "IMALU1-7", "LUPE")
  )
)

studieprogramkode <- studieprogram |>
  group_by(Studieprogramkode) |>
  slice(1) |>
  pull(Studieprogramkode)

emner_1 <- dbh_data(
  208, # dbh-tabell: Emner
  filters = list(
    "Studieprogramkode" = studieprogramkode[1:80] #A hack? Throws an error if more than 80 study program
  )
)

emner_2 <- dbh_data(
  208,
  filters = list(
    "Studieprogramkode" = studieprogramkode[80:length(studieprogramkode)]
  )
)

courses <- bind_rows(emner_1, emner_2)

usethis::use_data(courses, overwrite = TRUE)
