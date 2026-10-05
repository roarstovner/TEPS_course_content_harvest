# R/deduplicate_plans.R

#' Deduplicate course plans across years
#'
#' Takes combined harvested data and produces a deduplicated plan lookup table
#' plus the original data with plan_content_id added.
#'
#' @param df Combined data frame from all html_*.RDS files.
#'   Must include a `course_plan` column (from anonymize_text).
#' @return A list with two elements:
#'   - `plans`: Tibble of unique plans (plan_content_id, institution, Emnekode,
#'              year_from, year_to, source_course_id, course_plan,
#'              course_plan_normalized). The text is that of the plan's
#'              earliest offering, `source_course_id`, whose page its
#'              sections are cut from too (#273).
#'   - `courses`: The original df with plan_content_id column added.
deduplicate_plans <- function(df) {
  stopifnot(
    all(c("course_id", "institution", "Emnekode", "Årstall", "course_plan") %in% names(df))
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
    dplyr::arrange(institution, Emnekode, Årstall) |>
    dplyr::group_by(plan_content_id, institution, Emnekode) |>
    dplyr::summarise(
      year_from = min(Årstall),
      year_to = max(Årstall),
      source_course_id = dplyr::first(course_id),
      course_plan = dplyr::first(course_plan),
      course_plan_normalized = dplyr::first(course_plan_normalized),
      .groups = "drop"
    )

  list(
    plans = plans,
    courses = courses
  )
}
