library(stringi)

standardize_columns <- function(courses_data) {
  orig <- names(courses_data)
  names_map <- c(
    "Institusjonskode"="institution_code","Institusjonsnavn"="institution_name",
    "Avdelingskode"="faculty_code","Avdelingsnavn"="faculty_name",
    "Avdelingskode_SSB"="faculty_code_ssb","Semester"="semester_int",
    "Semesternavn"="semester_name","Studieprogramkode"="program_code",
    "Studieprogramnavn"="program_name","Emnekode"="course_code","Emnenavn"="course_name",
    "Studiepoeng"="ects","NUS-kode"="nus_code","Status"="status_code",
    "Statusnavn"="status_name","Navn"="instruction_language_name",
    "Fagkode"="field_code","Fagnavn"="field_name","Oppgave (ny fra h2012)"="thesis_flag"
  )
  idx_year <- which(grepl("??rstall|Arstall|\\u00C5rstall|.rstall", orig, ignore.case=TRUE, useBytes=TRUE))
  if (length(idx_year)==1) names_map[ orig[idx_year] ] <- "year"
  idx_lang <- which(grepl("Underv.*spr", orig, ignore.case=TRUE, useBytes=TRUE))
  if (length(idx_lang)==1) names_map[ orig[idx_lang] ] <- "instruction_language_code"
  idx_level_code <- which(grepl("Niv.*kod", orig, ignore.case=TRUE, useBytes=TRUE))
  idx_level_name <- which(grepl("Niv.*navn", orig, ignore.case=TRUE, useBytes=TRUE))
  if (length(idx_level_code)==1) names_map[ orig[idx_level_code] ] <- "level_code"
  if (length(idx_level_name)==1) names_map[ orig[idx_level_name] ] <- "level_name"
  
  new_names <- unname(names_map[orig])
  new_names[is.na(new_names)] <- orig[is.na(new_names)]
  names(courses_data) <- new_names
  courses_data
}

pick_fields <- function(courses_data, restrict_years = NULL) {
  needed <- c("institution_name","year","semester_int","semester_name","course_code","course_name")
  miss <- setdiff(needed, names(courses_data))
  if (length(miss)) stop("Mangler kolonner: ", paste(miss, collapse=", "))
  year_raw <- courses_data[["year"]]
  sem_raw  <- courses_data[["semester_int"]]
  year_num <- ifelse(grepl("^[0-9]+$", year_raw), as.integer(year_raw), NA_integer_)
  sem_num  <- ifelse(grepl("^[0-9]+$", sem_raw),  as.integer(sem_raw),  NA_integer_)
  
  df <- data.frame(
    institution_name = enc2utf8(courses_data[["institution_name"]]),
    year             = year_num,
    semester_int     = sem_num,
    semester_name    = enc2utf8(courses_data[["semester_name"]]),
    course_code      = trimws(enc2utf8(courses_data[["course_code"]])),
    course_name      = enc2utf8(courses_data[["course_name"]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(restrict_years)) df <- subset(df, year %in% restrict_years)
  df
}
