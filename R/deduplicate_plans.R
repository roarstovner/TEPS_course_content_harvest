# R/deduplicate_plans.R

#' Plan ids of texts
#'
#' @param id_text The text a plan id is made from (the anonymized plan text
#'   without the title and facts box, where known).
#' @param exam_sessions The pages list the exam parts once per exam session
#'   (ntnu's exam tab): the plan id keeps one session (.exam_tab_design()),
#'   so plans do not split by the sessions a semester lists.
#' @return Tibble `course_plan_normalized`, `plan_content_id` (a hash of it)
#'   and `plan_nchar` (the characters of the id text, for the shell rule).
plan_ids <- function(id_text, exam_sessions = FALSE) {
  normalized <- normalize_plan_text(if (exam_sessions)
    vapply(id_text, .exam_tab_design, character(1), USE.NAMES = FALSE) else id_text)
  tibble::tibble(course_plan_normalized = normalized,
                 plan_content_id = build_plan_id(normalized),
                 plan_nchar = nchar(id_text))
}

#' The plans of a set of offerings
#'
#' One row per unique plan per course code per institution: offerings with the
#' same plan_content_id share it. The text is that of the plan's earliest
#' offering, `source_course_id`, whose page its sections are cut from too.
#' Plans are keyed by institution, so the table of several institutions is
#' that of each.
#'
#' @param courses Offerings with `course_id`, `institution`, `Emnekode`,
#'   `Årstall`, `course_plan` and the columns of plan_ids().
#' @return Tibble plan_content_id, institution, Emnekode, year_from, year_to,
#'   source_course_id, course_plan, course_plan_normalized, plan_nchar.
plan_table <- function(courses) {
  courses |>
    dplyr::filter(!is.na(plan_content_id)) |>
    dplyr::arrange(institution, Emnekode, Årstall) |>
    dplyr::group_by(plan_content_id, institution, Emnekode) |>
    dplyr::summarise(
      # double, as in the releases so far (the type of a table without
      # plans), so the release schema changes only when its variables are
      # renamed and documented
      year_from = as.double(min(Årstall)),
      year_to = as.double(max(Årstall)),
      source_course_id = dplyr::first(course_id),
      course_plan = dplyr::first(course_plan),
      course_plan_normalized = dplyr::first(course_plan_normalized),
      plan_nchar = dplyr::first(plan_nchar),
      .groups = "drop"
    )
}

#' Deduplicate course plans across years
#'
#' @param df Offerings with a `course_plan` column (from anonymize_text()) and,
#'   where present, `id_text`, the text the plan id is made from.
#' @param exam_sessions See plan_ids().
#' @return A list: `plans`, plan_table(); `courses`, `df` with the columns of
#'   plan_ids() added.
deduplicate_plans <- function(df, exam_sessions = FALSE) {
  stopifnot(
    all(c("course_id", "institution", "Emnekode", "Årstall", "course_plan") %in% names(df))
  )
  ids <- plan_ids(df[["id_text"]] %||% df$course_plan, exam_sessions)
  courses <- dplyr::select(df, -dplyr::any_of("id_text"))
  for (col in names(ids)) courses[[col]] <- ids[[col]]
  list(plans = plan_table(courses), courses = courses)
}
