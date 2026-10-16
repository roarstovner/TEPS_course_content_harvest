## The DBH input: a dated snapshot of every course of every institution
##
## Writes data/input/dbh_courses_{date}.parquet (DBH table 208, Emner: every
## course x year x semester, all levels and statuses) and
## data/input/dbh_programmes_{date}.parquet (table 347, Studieprogram: the
## programmes, with Studiumkode). Both are DBH's bulk files, read in full, so
## the two tables come from the same day. dbh_courses() (R/utils.R) reads the
## latest snapshot; dbh_courses("teacher_ed") is the teacher education frame,
## a filter of it. Run it from the repository root; a new snapshot gets a new
## date and the old ones stay (methods.qmd, "Population and sampling frame").

date <- format(Sys.Date())
options(timeout = 3600)   # table 208's bulk file is ~700 MB

snapshot <- function(table) {
  df <- rdbhapi::dbh_data(table)   # no filters: the bulk file
  # DBH pads codes with spaces (trimmed when read) and a few names end in a
  # newline
  df <- dplyr::mutate(df, dplyr::across(dplyr::where(is.character), trimws))
  tibble::as_tibble(as.data.frame(df))   # without readr's attributes
}

write_snapshot <- function(df, name) {
  file <- file.path("data/input", sprintf("dbh_%s_%s.parquet", name, date))
  if (file.exists(file)) stop(file, " exists: a snapshot is not written twice")
  arrow::write_parquet(df, file, compression = "zstd", compression_level = 9)
  message(file, ": ", nrow(df), " rows")
}

courses <- snapshot(208)
programmes <- snapshot(347)
write_snapshot(courses, "courses")
write_snapshot(programmes, "programmes")
