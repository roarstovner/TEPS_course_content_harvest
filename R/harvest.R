# R/harvest.R
# Single entry point for harvesting course content from all institutions.
# Dispatches to strategy functions in R/harvest_strategies.R via config
# from R/institution_config.R.

#' Harvest one institution
#'
#' @param institution_short Character, e.g. "oslomet"
#' @param courses Data frame from courses.RDS (pre-filtered or not)
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @return Data frame with DBH columns plus course_id, url, html, html_error,
#'   html_success, extracted_text
harvest_institution <- function(institution_short, courses, year = NULL,
                                refetch = FALSE) {
  config <- get_institution_config(institution_short)

  df <- courses |>
    dplyr::filter(institution_short == !!institution_short) |>
    apply_year_filter(config, year) |>
    add_course_id() |>
    validate_courses("initial") |>
    add_course_url() |>
    validate_courses("with_url")

  message(institution_short, ": ", sum(!is.na(df$url)), "/", nrow(df), " URLs")

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

  message(institution_short, ": ", sum(!is.na(result$extracted_text)), "/",
          nrow(result), " with extracted text")
  result
}

#' Harvest all institutions
#'
#' Loops through all configured institutions, harvests each, and saves
#' the result to data/html_{inst}.RDS.
#'
#' @param courses Data frame from courses.RDS. If NULL, reads from disk.
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @param institutions Optional character vector of institution names to harvest.
#'   If NULL, harvests all configured institutions.
harvest_all <- function(courses = NULL, year = NULL, refetch = FALSE,
                        institutions = NULL) {
  if (is.null(courses)) courses <- readRDS("data/courses.RDS")

  configs <- load_all_configs()
  inst_names <- if (!is.null(institutions)) institutions else names(configs)

  for (inst in inst_names) {
    message("\n=== ", inst, " ===")
    tryCatch({
      result <- harvest_institution(inst, courses, year, refetch)
      saveRDS(result, file.path("data", paste0("html_", inst, ".RDS")))
      log_summary(inst, result)
    }, error = function(e) {
      message("ERROR harvesting ", inst, ": ", conditionMessage(e))
    })
  }
}

#' Apply year filter based on config
#'
#' If year_in_url is FALSE and no explicit year is provided, filters to
#' max(Årstall) — fetching the same timeless page for every year is waste.
#' If an explicit year is given, always filters to that year.
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
  if (!isTRUE(config$year_in_url)) {
    max_year <- max(df$Årstall, na.rm = TRUE)
    message("year_in_url=FALSE — filtering to max year: ", max_year)
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
