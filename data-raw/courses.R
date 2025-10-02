## code to prepare `courses` dataset goes here

library(rdbhapi)
library(tidyverse)

studieprogram <- dbh_data(
  347, # dbh-tabell: Studieprogram
  filters = list(
    "Studiumkode" = c("INTMASTER", "IMALU5-10", "IMALU1-7", "LUPE", "GLU1-7", "GLU5-10") # de to siste er fireårig
  )
)

studieprogramkode <- studieprogram |>
  distinct(Studieprogramkode)

emner_1 <- dbh_data(
  208, # dbh-tabell: Emner
  filters = list(
    "Studieprogramkode" = studieprogramkode[1:80] #A hack? Throws an error if more than 80 study programs
  )
)

emner_2 <- dbh_data(
  208,
  filters = list(
    "Studieprogramkode" = studieprogramkode[81:160]
  )
)

emner_3 <- dbh_data(
  208,
  filters = list(
    "Studieprogramkode" = studieprogramkode[161:length(studieprogramkode)]
  )
)

courses <- reduce(list(emner_1, emner_2, emner_3), bind_rows)

saveRDS(courses, "data/courses.RDS")
