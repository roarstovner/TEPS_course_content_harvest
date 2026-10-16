# Data folders (README "Data Files: Published and Internal"): data/input holds
# the DBH snapshots (in git); data/raw the harvest, which only harvesting
# writes; data/interim and data/processed are rebuilt by targets::tar_make(),
# interim with raw page text (personal data, internal), processed anonymized
# (can be shared).
INPUT_DIR <- "data/input"
RAW_DIR <- "data/raw"

# DBH study types (Studiumkode, table 347) of the five-year teacher education
# programmes: the teacher-ed frame (methods.qmd, "Population and sampling frame").
TEACHER_ED_STUDIUM <- c("INTMASTER", "IMALU5-10", "IMALU1-7", "LUPE")

# A DBH snapshot written by data-raw/courses.R, of `date` or the latest:
# data/input/dbh_{table}_{YYYY-MM-DD}.parquet.
dbh_snapshot_file <- function(table = c("courses", "programmes"), date = NULL,
                              input_dir = INPUT_DIR) {
  table <- match.arg(table)
  files <- if (is.null(date)) sort(Sys.glob(file.path(input_dir, paste0("dbh_", table, "_*.parquet")))) else
    file.path(input_dir, sprintf("dbh_%s_%s.parquet", table, format(as.Date(date))))
  files <- files[file.exists(files)]
  if (!length(files)) stop("No DBH ", table, " snapshot in ", input_dir, ": run data-raw/courses.R")
  files[length(files)]
}

#' DBH course offerings from a snapshot
#'
#' DBH table 208 holds every course of every institution, year and semester
#' (all levels and statuses): the all-courses frame. `frame = "teacher_ed"`
#' keeps the courses of the teacher education programmes, those whose
#' programme has a study type in `TEACHER_ED_STUDIUM` in table 347 in some year,
#' matched on institution and programme code (a programme code is unique only
#' within an institution). Both come from the same snapshot, so the
#' teacher-ed offerings have the same course ids in both frames.
#'
#' @param frame "all" or "teacher_ed"
#' @param date Snapshot date; NULL for the latest
#' @param input_dir Folder of the snapshots
#' @return One row per course, year and semester, with DBH's columns plus
#'   `institution` (NA for a code no config has), `Emnekode_raw` (DBH's code)
#'   and `Emnekode` (without a trailing version number); the input of
#'   `harvest_all(courses = ...)`
dbh_courses <- function(frame = c("all", "teacher_ed"), date = NULL, input_dir = INPUT_DIR) {
  frame <- match.arg(frame)
  courses <- arrow::read_parquet(dbh_snapshot_file("courses", date, input_dir))
  if (frame == "teacher_ed") {
    programmes <- arrow::read_parquet(dbh_snapshot_file("programmes", date, input_dir),
                                      col_select = c("Institusjonskode", "Studieprogramkode", "Studiumkode"))
    keys <- programmes |>
      dplyr::filter(Studiumkode %in% TEACHER_ED_STUDIUM) |>
      dplyr::distinct(Institusjonskode, Studieprogramkode)
    courses <- dplyr::semi_join(courses, keys, by = c("Institusjonskode", "Studieprogramkode"))
  }
  courses |>
    # one row per offering: 2 of the 2.16 million rows of 2026-10-16 (2007)
    # repeat a course and semester
    dplyr::distinct(Institusjonskode, Emnekode, Årstall, Semester, .keep_all = TRUE) |>
    dplyr::mutate(institution = institution_from_code(Institusjonskode),
                  Emnekode_raw = Emnekode,
                  Emnekode = canon_remove_trailing_num(Emnekode)) |>
    dplyr::relocate(institution) |>
    dplyr::relocate(Emnekode_raw, .before = Emnekode)
}

harvest_file <- function(institution, raw_dir = RAW_DIR) {
  file.path(raw_dir, paste0("html_", institution, ".RDS"))
}

# Institutions with a raw file (a first harvest or a dated one).
harvested_institutions <- function(raw_dir = RAW_DIR) {
  files <- c(list.files(raw_dir, "^html_.*\\.RDS$"),
             basename(Sys.glob(file.path(raw_dir, "harvests", "*", "html_*.RDS"))))
  sort(unique(sub("^html_(.*)\\.RDS$", "\\1", files)))
}

# The raw store is append-only: an institution's first harvest is
# data/raw/html_{inst}.RDS, and every later harvest adds a file
# data/raw/harvests/{YYYY-MM-DD}/html_{inst}.RDS with what it fetched (a
# second harvest the same day: {YYYY-MM-DD}_{HHMMSS}/, which sorts after it).
# No raw file is written twice; read_harvest() combines them.
dated_harvest_file <- function(institution, date = Sys.Date(), raw_dir = RAW_DIR) {
  file.path(raw_dir, "harvests", format(date), paste0("html_", institution, ".RDS"))
}

# All raw files of an institution: the first harvest, then the dated ones.
harvest_files <- function(institution, raw_dir = RAW_DIR) {
  first <- harvest_file(institution, raw_dir)
  dated <- Sys.glob(file.path(raw_dir, "harvests", "*", paste0("html_", institution, ".RDS")))
  # byte order: the locale's ignores punctuation and puts {date}_{time}/ first
  c(first[file.exists(first)], sort(dated, method = "radix"))
}

# Date of a raw harvest: its harvested_at column (written since 2026-10-06),
# else the name of its dated directory. Never the file's modification time,
# which a copy changes.
harvest_date <- function(file, df = readRDS(file)) {
  if ("harvested_at" %in% names(df) && any(!is.na(df$harvested_at))) {
    return(max(as.Date(df$harvested_at), na.rm = TRUE))
  }
  d <- as.Date(basename(dirname(file)), optional = TRUE)
  if (is.na(d)) stop("No harvest date for ", file, ": no harvested_at, not in a dated directory")
  d
}

# Academic year ("2025-2026", August to July) of a date.
academic_year_of_date <- function(date) {
  y <- as.integer(format(date, "%Y")) - (as.integer(format(date, "%m")) < 8)
  out <- sprintf("%d-%d", y, y + 1)
  out[is.na(y)] <- NA_character_
  out
}

canon_remove_trailing_num <- function(x) {
  sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)
}

canon_semester_name <- function(semester_name) {
  dplyr::case_match(
    semester_name,
    "Vår" ~ "spring",
    "Høst" ~ "autumn",
    "Sommer" ~ "summer",
    .default = semester_name
  )
}

# The year an offering's academic year (August-July) starts: autumn Y -> Y,
# spring and summer Y -> Y - 1
academic_start_year <- function(year, semester) {
  year - (semester %in% c("Vår", "Sommer"))
}

nla_academic_year <- function(year, semester) {
  dplyr::case_match(semester,
    "Høst"            ~ paste0(year, "-", year + 1),
    c("Vår", "Sommer") ~ paste0(year - 1, "-", year)
  )
}

semester_to_url <- function(semester) {
  dplyr::case_match(semester,
    "Vår"  ~ "var",
    "Høst" ~ "host",
    .default = tolower(semester)
  )
}


#' Add Course ID
#'
#' Creates a unique course identifier by combining institution, course code,
#' year, semester, and status information.
#'
#' @param dbh_df A data frame containing course information with columns:
#'   `institution`, `Emnekode_raw`, `Årstall`, `Semesternavn`, and `Status`.
#'
#' @return A data frame with an additional `course_id` column placed first.
#'   The course_id format is: `{institution}_{code}_{year}_{semester}_{status}`.
#'
#' @details
#' Status codes: 1 = Aktivt, 2 = Nytt, 3 = Avviklet, 4 = Avviklet, men tas eksamen.
#' Uses `Emnekode_raw` instead of `Emnekode` which may result in fewer duplicates
#' if the raw code is more granular, but could miss normalization benefits.
add_course_id <- function(dbh_df) {
  dbh_df |>
    dplyr::mutate(
      course_id = paste(
        institution,
        Emnekode_raw, # Using `Emnekode_raw` instead of `Emnekode` may result in fewer duplicates if the raw code is more granular, but could miss normalization or grouping benefits provided by `Emnekode`.
        Årstall,
        canon_semester_name(Semesternavn),
        Status, # 1: Aktivt, 2: Nytt, 3: Avviklet, 4: Avviklet, men tas eksamen
        sep = "_"
      )
    ) |> 
    dplyr::relocate(course_id, .before = 1)
}

# An offering keeps the course_id it was first harvested under. The id ends
# with DBH's Status, which a later DBH snapshot can change; the rest
# (institution, code, year, semester) is the offering, one row in DBH.
offering_key <- function(course_id) sub("_[^_]*$", "", course_id)

first_course_id <- function(course_id, known) {
  i <- match(offering_key(course_id), offering_key(known))
  course_id[!is.na(i)] <- known[i[!is.na(i)]]
  course_id
}


# Removes duplicate courses from the dataframe by arranging rows by course_id and Status,
# then keeping only the first occurrence of each course_id. Status helps prioritize which duplicate to keep.
remove_dupes <- function(dbh_df) {
  dbh_df |> 
    dplyr::arrange(course_id, Status) |> #Status: 1 Aktivt; 2 Nytt; 3 Avviklet; 4 Avviklet, men tas eksamen
    distinct(course_id, .keep_all = TRUE)
}

validate_courses <- function(df, stage = c("initial", "with_url", "with_html")) {
  stage <- match.arg(stage)

  required <- switch(stage,
    initial = c("institution", "Emnekode", "Årstall"),
    with_url = c("institution", "course_id", "url"),
    with_html = c("institution", "course_id", "url", "html", "html_success"),
    c() # default case: returns empty character vector if stage does not match
  )
  
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns at {stage} stage: {paste(missing, collapse = ', ')}")
  }
  invisible(df)
}
