## The DBH input: dated snapshots of every course of every institution, and
## of the student counts per course
##
##   Rscript data-raw/courses.R           both
##   Rscript data-raw/courses.R courses   the course list
##   Rscript data-raw/courses.R counts    the student counts
##
## The course list: data/input/dbh_courses_{date}.parquet (DBH table 208,
## Emner: every course x year x semester, all levels and statuses) and
## data/input/dbh_programmes_{date}.parquet (table 347, Studieprogram: the
## programmes, with Studiumkode), DBH's bulk files, read in full, so the two
## tables come from the same day. dbh_courses() (R/utils.R) reads the latest;
## dbh_courses("teacher_ed") is the teacher education frame, a filter of it.
## The student counts per course (code with version, as 208's) and semester at
## the institutions with a config, from the API: data/input/dbh_exams_{date}.parquet
## (table 905, Eksamensdata: exam registrations) and dbh_credits_{date}.parquet
## (table 900, Studiepoeng: credits earned), which dbh_offered() reads, the
## latest apart from the course list's date. Run it from the repository root;
## a new snapshot gets a new date and the old ones stay (methods.qmd,
## "Population and sampling frame").

for (f in c("R/utils.R", "R/fetch_html_cols.R", "R/extract_fulltext.R", "R/institution_config.R")) source(f)
what <- commandArgs(TRUE)
if (!length(what)) what <- c("courses", "counts")
stopifnot(what %in% c("courses", "counts"))
date <- format(Sys.Date())
options(timeout = 3600)   # table 208's bulk file is ~700 MB

write_snapshot <- function(df, name) {
  file <- file.path("data/input", sprintf("dbh_%s_%s.parquet", name, date))
  if (file.exists(file)) stop(file, " exists: a snapshot is not written twice")
  arrow::write_parquet(df, file, compression = "zstd", compression_level = 9)
  message(file, ": ", nrow(df), " rows")
}

if ("courses" %in% what) {
  courses <- dbh_fetch(208)
  programmes <- dbh_fetch(347)
  write_snapshot(courses, "courses")
  write_snapshot(programmes, "programmes")
}
if ("counts" %in% what) {
  codes <- sort(unique(unlist(lapply(institution_configs, `[[`, "code"))))
  exams <- dbh_fetch(905, codes)
  credits <- dbh_fetch(900, codes)
  write_snapshot(exams, "exams")
  write_snapshot(credits, "credits")
}
