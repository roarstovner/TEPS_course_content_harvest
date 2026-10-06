# Data folders (README "Data Files: Published and Internal"): data/input holds
# the DBH course list (in git); data/raw the harvest, which only harvesting
# writes; data/interim and data/processed are rebuilt by targets::tar_make(),
# interim with raw page text (personal data, internal), processed anonymized
# (can be shared).
RAW_DIR <- "data/raw"

harvest_file <- function(institution, raw_dir = RAW_DIR) {
  file.path(raw_dir, paste0("html_", institution, ".RDS"))
}

harvested_institutions <- function(raw_dir = RAW_DIR) {
  sub("^html_(.*)\\.RDS$", "\\1", list.files(raw_dir, "^html_.*\\.RDS$"))
}

# The raw store is append-only (#296): an institution's first harvest is
# data/raw/html_{inst}.RDS, and every later harvest adds a file
# data/raw/harvests/{YYYY-MM-DD}/html_{inst}.RDS with what it fetched. No raw
# file is written twice; read_harvest() combines them.
dated_harvest_file <- function(institution, date = Sys.Date(), raw_dir = RAW_DIR) {
  file.path(raw_dir, "harvests", format(date), paste0("html_", institution, ".RDS"))
}

# All raw files of an institution: the first harvest, then the dated ones.
harvest_files <- function(institution, raw_dir = RAW_DIR) {
  dated <- Sys.glob(file.path(raw_dir, "harvests", "*", paste0("html_", institution, ".RDS")))
  c(harvest_file(institution, raw_dir), sort(dated))
}

# Date of a raw harvest: its harvested_at column, else the dated directory
# name, else the file's modification date (harvests before 2026-10-06 have no
# harvested_at).
harvest_date <- function(file, df = readRDS(file)) {
  if ("harvested_at" %in% names(df) && any(!is.na(df$harvested_at))) {
    return(max(as.Date(df$harvested_at), na.rm = TRUE))
  }
  d <- as.Date(basename(dirname(file)), optional = TRUE)
  if (!is.na(d)) d else as.Date(file.mtime(file))
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
