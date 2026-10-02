# R/pipeline_metrics.R
# Regression snapshot for the built data (#255).
#
# pipeline_metrics() counts offerings, plans and sections per institution in
# the built files; check_pipeline_metrics() compares them with the snapshot in
# tests/snapshots/pipeline_metrics.csv and prints every change beyond
# tolerance. A rebuild that silently empties an institution (all 142 nla
# courses lost their sections in 06db3ac) then shows up at once, and the test
# in tests/testthat/test-pipeline-metrics.R fails. After an intended change,
# update the snapshot and commit it with the change that explains it:
#
#   Rscript -e 'source("R/pipeline_metrics.R"); check_pipeline_metrics(update = TRUE)'

METRICS_SNAPSHOT <- "tests/snapshots/pipeline_metrics.csv"

#' Per-institution metrics of the built data
#'
#' @param offerings course_offerings_full: one row per offering with
#'   `extracted_text`, `course_plan` and `plan_content_id`.
#' @param sections sections_raw (`course_id`, `institution`, `section`,
#'   `raw_text`), or NULL to skip the section metrics.
#' @return Long tibble `institution`, `section` ("(all)" for offering-level
#'   metrics), `metric`, `value`. Metrics named n_* are counts; median_chars
#'   is the median length of `course_plan` or of the section text.
pipeline_metrics <- function(offerings, sections = NULL) {
  has <- function(x) !is.na(x) & nzchar(x)
  course_years <- offerings |>
    dplyr::group_by(institution, Emnekode_raw, Årstall) |>
    dplyr::summarise(plan = any(has(course_plan)), .groups = "drop") |>
    dplyr::count(institution, wt = plan, name = "n_course_years_with_plan")
  if (is.null(sections)) {
    sections <- tibble::tibble(course_id = character(), institution = character(),
                               section = character(), raw_text = character())
  }
  with_sections <- sections |>
    dplyr::distinct(institution, course_id) |>
    dplyr::count(institution, name = "n_with_sections")

  per_institution <- offerings |>
    dplyr::group_by(institution) |>
    dplyr::summarise(
      n_rows         = dplyr::n(),
      n_with_text    = sum(has(extracted_text)),
      n_with_plan    = sum(has(course_plan)),
      n_unique_plans = dplyr::n_distinct(plan_content_id, na.rm = TRUE),
      median_chars   = stats::median(nchar(course_plan[has(course_plan)])),
      .groups = "drop"
    ) |>
    dplyr::left_join(course_years, by = "institution") |>
    dplyr::left_join(with_sections, by = "institution") |>
    tidyr::pivot_longer(-institution, names_to = "metric", values_to = "value") |>
    dplyr::mutate(section = "(all)")

  per_section <- sections |>
    dplyr::group_by(institution, section) |>
    dplyr::summarise(n_courses = dplyr::n_distinct(course_id),
                     median_chars = stats::median(nchar(raw_text)),
                     .groups = "drop") |>
    tidyr::pivot_longer(c(n_courses, median_chars),
                        names_to = "metric", values_to = "value")

  dplyr::bind_rows(per_institution, per_section) |>
    dplyr::mutate(value = dplyr::coalesce(as.numeric(value), 0)) |>
    dplyr::select(institution, section, metric, value) |>
    dplyr::arrange(institution, section, metric)
}

#' Metrics that changed beyond tolerance between two metric tables
#'
#' A count (n_*) is flagged when it changes by more than `tol_n` (relative)
#' and `min_n` (absolute), median_chars by more than `tol_chars` and
#' `min_chars`. A metric that appears, disappears, or goes to or from zero is
#' always flagged.
compare_metrics <- function(old, new, tol_n = 0.05, min_n = 10,
                            tol_chars = 0.10, min_chars = 50) {
  dplyr::full_join(old, new, by = c("institution", "section", "metric"),
                   suffix = c("_old", "_new")) |>
    dplyr::mutate(
      old = dplyr::coalesce(value_old, 0),
      new = dplyr::coalesce(value_new, 0),
      diff = new - old,
      chars = metric == "median_chars",
      big = abs(diff) > ifelse(chars, min_chars, min_n) &
        abs(diff) > ifelse(chars, tol_chars, tol_n) * old,
      zero = (old == 0) != (new == 0)
    ) |>
    dplyr::filter(big | zero) |>
    dplyr::select(institution, section, metric, old, new, diff) |>
    dplyr::arrange(institution, section, metric)
}

#' Compare the built data with the snapshot, or update the snapshot
#'
#' @param update If TRUE, write the current metrics as the new snapshot.
#' @return The changes beyond tolerance (invisibly; empty after an update).
check_pipeline_metrics <- function(update = FALSE, snapshot = METRICS_SNAPSHOT,
                                   offerings = "data/course_offerings_full.RDS",
                                   sections = "data/sections_raw.RDS") {
  current <- pipeline_metrics(
    readRDS(offerings),
    if (file.exists(sections)) readRDS(sections)
  )
  if (update || !file.exists(snapshot)) {
    utils::write.csv(current, snapshot, row.names = FALSE)
    message("Wrote ", nrow(current), " metrics to ", snapshot)
    return(invisible(compare_metrics(current, current)))
  }
  changes <- compare_metrics(utils::read.csv(snapshot), current)
  if (nrow(changes) == 0) {
    message("Pipeline metrics match ", snapshot)
  } else {
    message(nrow(changes), " metric(s) differ from ", snapshot,
            " beyond tolerance:")
    print(as.data.frame(changes), row.names = FALSE)
    message("If intended: check_pipeline_metrics(update = TRUE), and commit ",
            "the snapshot with the change.")
  }
  invisible(changes)
}
