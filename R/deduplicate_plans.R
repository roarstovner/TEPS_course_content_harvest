# R/deduplicate_plans.R

#' Deduplicate course plans across years
#'
#' Takes combined harvested data and produces a deduplicated plan lookup table
#' plus the original data with plan_content_id added.
#'
#' @param df Combined data frame from all html_*.RDS files.
#' @param .progress Passed to normalize_plan_text for progress reporting.
#' @return A list with two elements:
#'   - `plans`: Tibble of unique plans (plan_content_id, institution_short, Emnekode,
#'              fulltext_normalized, year_from, year_to)
#'   - `courses`: The original df with plan_content_id column added.
deduplicate_plans <- function(df) {
  stopifnot(
    all(c("institution_short", "Emnekode", "Årstall", "fulltext") %in% names(df))
  )

  # Add normalized text and plan ID
  courses <- df |>
    dplyr::mutate(
      fulltext_normalized = normalize_plan_text(institution_short, fulltext),
      plan_content_id = build_plan_id(fulltext_normalized)
    )

  # Build plan lookup: one row per unique plan per course code per institution
  plans <- courses |>
    dplyr::filter(!is.na(plan_content_id)) |>
    dplyr::arrange(institution_short, Emnekode, Årstall) |>
    dplyr::group_by(plan_content_id, institution_short, Emnekode) |>
    dplyr::summarise(
      year_from = min(Årstall),
      year_to = max(Årstall),
      fulltext = dplyr::first(fulltext),
      fulltext_normalized = dplyr::first(fulltext_normalized),
      .groups = "drop"
    )

  list(
    plans = plans,
    courses = courses
  )
}
