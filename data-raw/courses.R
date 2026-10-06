## code to prepare `courses` dataset goes here

source("R/utils.R", local = TRUE)
# institution_config.R references pre/post and fetch functions defined here
source("R/extract_fulltext.R", local = TRUE)
source("R/fetch_html_cols.R", local = TRUE)
source("R/institution_config.R", local = TRUE)

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

courses <- do.call(rbind, courses_list)

# Studieprogramkode is only unique within an institution: the same code (e.g.
# "LUPE", "LREAL") can exist at several institutions. Keep only courses whose
# (Institusjonskode, Studieprogramkode) pair matches a teacher education
# programme from table 347.
studieprogram_keys <- dplyr::distinct(studieprogram, Institusjonskode, Studieprogramkode)

courses <- courses |>
  dplyr::semi_join(studieprogram_keys, by = c("Institusjonskode", "Studieprogramkode"))

courses <- courses |>
  dplyr::mutate(
    institution = institution_from_code(Institusjonskode),
    Emnekode_raw = Emnekode,
    Emnekode = canon_remove_trailing_num(Emnekode),
  ) |> 
  dplyr::relocate(institution) |> 
  dplyr::relocate(Emnekode_raw, .before = Emnekode)

saveRDS(courses, "data/input/courses.RDS")
