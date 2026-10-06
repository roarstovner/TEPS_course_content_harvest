# R/harvest.R
# Single entry point for harvesting course content from all institutions.
# Dispatches to strategy functions in R/harvest_strategies.R via config
# from R/institution_config.R.

#' Harvest one institution
#'
#' @param institution Character, e.g. "oslomet"
#' @param courses Data frame from courses.RDS (pre-filtered or not)
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @return Data frame with DBH columns plus course_id, url, html, html_error,
#'   html_success, extracted_text
harvest_institution <- function(institution, courses, year = NULL,
                                refetch = FALSE) {
  config <- get_institution_config(institution)
  if (identical(config$plan_years, "current")) prepare_current_harvest(institution)

  df <- courses |>
    dplyr::filter(institution == !!institution) |>
    apply_year_filter(config, year) |>
    add_course_id() |>
    validate_courses("initial") |>
    add_course_url() |>
    validate_courses("with_url")

  message(institution, ": ", sum(!is.na(df$url)), "/", nrow(df), " URLs")

  result <- switch(config$strategy,
    standard           = harvest_standard(df, config, refetch),
    url_discovery      = harvest_url_discovery(df, config, refetch),
    shadow_dom         = harvest_shadow_dom(df, config, refetch),
    html_pdf_discovery = harvest_html_pdf_discovery(df, config, refetch),
    pdf_split          = harvest_pdf_split(df, config, refetch),
    json_extract       = harvest_json_extract(df, config, refetch),
    noop               = harvest_noop(df, config),
    stop("Unknown strategy: ", config$strategy)
  )

  result <- ensure_output_columns(result)

  message(institution, ": ", sum(!is.na(result$extracted_text)), "/",
          nrow(result), " with extracted text")
  result
}

#' Harvest all institutions
#'
#' Loops through all configured institutions, harvests each, and saves
#' the result to data/raw/html_{inst}.RDS.
#'
#' @param courses Data frame from courses.RDS. If NULL, reads from disk.
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @param institutions Optional character vector of institution names to harvest.
#'   If NULL, harvests all configured institutions.
harvest_all <- function(courses = NULL, year = NULL, refetch = FALSE,
                        institutions = NULL) {
  if (is.null(courses)) courses <- readRDS("data/input/courses.RDS")
  if (!is.data.frame(courses)) {
    stop("`courses` must be a data frame, not ", class(courses)[1], ". ",
         "Did you mean harvest_all(institutions = ...)?", call. = FALSE)
  }

  configs <- load_all_configs()
  inst_names <- if (!is.null(institutions)) institutions else names(configs)

  for (inst in inst_names) {
    message("\n=== ", inst, " ===")
    tryCatch({
      result <- harvest_institution(inst, courses, year, refetch)
      result$harvested_at <- Sys.Date()
      if (identical(configs[[inst]]$plan_years, "current")) archive_harvest(inst)
      saveRDS(result, harvest_file(inst))
      log_summary(inst, result)
    }, error = function(e) {
      message("ERROR harvesting ", inst, ": ", conditionMessage(e))
    })
  }
}

#' Before harvesting a "current" site (plan_years; #293)
#'
#' Such a site shows only the plan in force, so a harvest counts for its own
#' academic year. Checkpoints from an earlier academic year would return last
#' year's pages as if fetched now, so they are removed; in June-August the site
#' may already show next year's plan, so a warning is given.
#'
#' @param institution Character, institution short name
prepare_current_harvest <- function(institution) {
  if (as.integer(format(Sys.Date(), "%m")) %in% 6:8) {
    warning(institution, " shows only the current plan: a harvest in June-August ",
            "may give next year's plan (#293)", call. = FALSE)
  }
  now <- academic_year_of_date(Sys.Date())
  for (cp in Sys.glob(file.path(RAW_DIR, "checkpoint", paste0("*_", institution, ".RDS")))) {
    if (academic_year_of_date(as.Date(file.mtime(cp))) != now) {
      message("Checkpoint from an earlier academic year, removed: ", cp)
      file.remove(cp)
    }
  }
}

#' Keep the previous harvest of a "current" site
#'
#' Moves data/raw/html_{inst}.RDS to data/raw/archive/{harvest date}/ when it
#' is from an earlier academic year, so a new harvest does not overwrite the
#' only copy of last year's plans (#293).
#'
#' @param institution Character, institution short name
archive_harvest <- function(institution) {
  file <- harvest_file(institution)
  if (!file.exists(file)) return(invisible())
  date <- harvest_date(file)
  if (academic_year_of_date(date) == academic_year_of_date(Sys.Date())) return(invisible())
  dir <- file.path(RAW_DIR, "archive", format(date))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  message("Earlier harvest kept in ", dir)
  file.rename(file, file.path(dir, basename(file)))
  invisible()
}

#' Apply year filter based on config
#'
#' A "current" site (`plan_years`) shows only the plan in force, so without an
#' explicit year only the latest DBH year is harvested. "url" and "page" sites
#' give every year its own plan. An explicit year always filters to that year.
#'
#' @param df Data frame with Årstall column
#' @param config Institution config list
#' @param year Optional explicit year to filter to
#' @return Filtered data frame
apply_year_filter <- function(df, config, year = NULL) {
  if (!is.null(year)) {
    message("Filtering to year ", year)
    return(dplyr::filter(df, Årstall == year))
  }
  if (identical(config$plan_years, "current")) {
    max_year <- max(df$Årstall, na.rm = TRUE)
    message("plan_years = \"current\" — filtering to max year: ", max_year)
    return(dplyr::filter(df, Årstall == max_year))
  }
  df
}

#' Log harvest summary for one institution
#'
#' @param inst Institution short name
#' @param result Harvested data frame
log_summary <- function(inst, result) {
  n <- nrow(result)
  n_url <- sum(!is.na(result$url))
  n_html <- sum(result$html_success, na.rm = TRUE)
  n_text <- sum(!is.na(result$extracted_text))
  message(sprintf("  %s: %d courses, %d URLs, %d HTML, %d extracted text",
                  inst, n, n_url, n_html, n_text))
}
