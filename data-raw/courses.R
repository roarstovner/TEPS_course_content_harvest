## code to prepare `courses` dataset goes here

institution_short <- function(institution_code, cfg = NULL){
  if(is.null(cfg)) cfg <- yaml::yaml.load_file(input = "config/institutions.yaml")
  
  lookup <- unlist(cfg$aliases)
  
  lookup[institution_code]
}

studieprogram <- rdbhapi::dbh_data(
  347, # dbh-tabell: Studieprogram
  filters = list(
    "Studiumkode" = c("INTMASTER", "IMALU5-10", "IMALU1-7", "LUPE")#, "GLU1-7", "GLU5-10") # de to siste er fireårig
  )
)

studieprogramkode <- unique(studieprogram$Studieprogramkode)

# rdbhapi doesn't handle larger chunks than 80
studieprogramkode_chunks <- split(
  studieprogramkode, 
  ceiling(seq_along(studieprogramkode) / 80)
)

courses_list <- lapply(studieprogramkode_chunks, function(chunk) {
  rdbhapi::dbh_data(
    208,
    filters = list("Studieprogramkode" = chunk)
  )
})

courses <- do.call(rbind, emner_list)

courses <- courses |> 
  dplyr::mutate(
    institution_short = unname(institution_short(Institusjonskode)),
    Emnekode_raw = Emnekode,
    Emnekode = canon_remove_trailing_num(Emnekode),
  ) |> 
  dplyr::relocate(institution_short) |> 
  dplyr::relocate(Emnekode_raw, .before = Emnekode)

saveRDS(courses, "data/courses.RDS")
