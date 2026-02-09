canon_remove_trailing_num <- function(x) {
  sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)
}

canon_semester_name <- function(semester_name) {
  case_match(
    semester_name,
    "Vår" ~ "spring",
    "Høst" ~ "autumn",
    "Sommer" ~ "summer",
    .default = semester_name
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
#'   `institution_short`, `Emnekode_raw`, `Årstall`, `Semesternavn`, and `Status`.
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
        institution_short,
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
    initial = c("institution_short", "Emnekode", "Årstall"),
    with_url = c("institution_short", "course_id", "url"),
    with_html = c("institution_short", "course_id", "url", "html", "html_success"),
    c() # default case: returns empty character vector if stage does not match
  )
  
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns at {stage} stage: {paste(missing, collapse = ', ')}")
  }
  invisible(df)
}
