# R/deduplicate_plans.R

#' Deduplicate course plans across years
#'
#' Takes combined harvested data and produces a deduplicated plan lookup table
#' plus the original data with plan_content_id added.
#'
#' @param df Combined data frame from all html_*.RDS files.
#'   Must include a `course_plan` column (from anonymize_fulltext).
#' @return A list with two elements:
#'   - `plans`: Tibble of unique plans (plan_content_id, institution_short, Emnekode,
#'              course_plan, course_plan_normalized, year_from, year_to)
#'   - `courses`: The original df with plan_content_id column added.
deduplicate_plans <- function(df) {
  stopifnot(
    all(c("institution_short", "Emnekode", "Årstall", "course_plan") %in% names(df))
  )

  # Normalize anonymized text and build plan ID
  courses <- df |>
    dplyr::mutate(
      course_plan_normalized = normalize_plan_text(course_plan),
      plan_content_id = build_plan_id(course_plan_normalized)
    )

  # Build plan lookup: one row per unique plan per course code per institution
  plans <- courses |>
    dplyr::filter(!is.na(plan_content_id)) |>
    dplyr::arrange(institution_short, Emnekode, Årstall) |>
    dplyr::group_by(plan_content_id, institution_short, Emnekode) |>
    dplyr::summarise(
      year_from = min(Årstall),
      year_to = max(Årstall),
      course_plan = dplyr::first(course_plan),
      course_plan_normalized = dplyr::first(course_plan_normalized),
      .groups = "drop"
    )

  list(
    plans = plans,
    courses = courses
  )
}
