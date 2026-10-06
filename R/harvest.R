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
#' @param skip Course ids not to harvest (already in a raw file)
#' @return Data frame with DBH columns plus course_id, url, html, html_error,
#'   html_success, extracted_text
harvest_institution <- function(institution, courses, year = NULL,
                                refetch = FALSE, skip = character()) {
  config <- get_institution_config(institution)
  if (!identical(config$plan_years, "url")) refetch <- prepare_fresh_harvest(config)

  df <- courses |>
    dplyr::filter(institution == !!institution) |>
    apply_year_filter(config, year) |>
    add_course_id() |>
    dplyr::filter(!course_id %in% skip)
  if (nrow(df) == 0) return(df)
  df <- df |>
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
#' Loops through all configured institutions and harvests each. The raw store
#' is append-only (#296): the first harvest of an institution is saved to
#' data/raw/html_{inst}.RDS; a later one fetches only offerings that no raw
#' file holds yet (a "current" site: a new snapshot of its latest DBH year)
#' and saves them to data/raw/harvests/{date}/html_{inst}.RDS. An existing raw
#' file is never written again.
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
      first <- length(harvest_files(inst)) == 0
      current <- identical(configs[[inst]]$plan_years, "current")
      skip <- if (first || current) character() else
        unlist(lapply(harvest_files(inst), function(f) readRDS(f)$course_id))
      result <- harvest_institution(inst, courses, year, refetch, skip)
      file <- if (first) harvest_file(inst) else dated_harvest_file(inst)
      if (nrow(result) == 0) {
        message(inst, ": nothing new to harvest")
      } else if (file.exists(file)) {
        stop("raw files are not written twice (#296): ", file)
      } else {
        result$harvested_at <- Sys.Date()
        dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
        saveRDS(result, file)
        log_summary(inst, result)
      }
    }, error = function(e) {
      message("ERROR harvesting ", inst, ": ", conditionMessage(e))
    })
  }
}

#' Before harvesting a site whose pages change over time
#'
#' A "page" site (one page, several years) adds new years to its pages and a
#' "current" site replaces its plan, so a page fetched earlier must not stand
#' in for one fetched now: their checkpoints are removed and every page is
#' fetched again (#296). A "current" site harvested in June-August may already
#' show next year's plan, so that gives a warning (#293).
#'
#' @param config Institution config
#' @return TRUE, the `refetch` for the strategy
prepare_fresh_harvest <- function(config) {
  if (identical(config$plan_years, "current") &&
      as.integer(format(Sys.Date(), "%m")) %in% 6:8) {
    warning(config$name, " shows only the current plan: a harvest in June-August ",
            "may give next year's plan (#293)", call. = FALSE)
  }
  cps <- Sys.glob(file.path(RAW_DIR, "checkpoint", paste0("*_", config$name, ".RDS")))
  if (length(cps)) message("Pages are fetched again; removed: ", paste(cps, collapse = ", "))
  file.remove(cps)
  TRUE
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
