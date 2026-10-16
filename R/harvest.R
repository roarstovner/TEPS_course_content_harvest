# R/harvest.R
# Single entry point for harvesting course content from all institutions.
# Dispatches to strategy functions in R/harvest_strategies.R via config
# from R/institution_config.R.

#' Harvest one institution
#'
#' @param institution Character, e.g. "oslomet"
#' @param courses Data frame from dbh_courses() (pre-filtered or not)
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @param skip Course ids not to harvest (already in a raw file)
#' @param known Course ids in the raw files: an offering among them keeps its
#'   id, also when a later DBH snapshot gives it another Status
#' @return Data frame with DBH columns plus course_id, url, html, html_error,
#'   html_success, extracted_text
harvest_institution <- function(institution, courses, year = NULL,
                                refetch = FALSE, skip = character(), known = skip) {
  config <- get_institution_config(institution)
  if (!identical(config$plan_years, "url")) refetch <- prepare_fresh_harvest(config)

  df <- courses |>
    dplyr::filter(institution == !!institution) |>
    apply_year_filter(config, year) |>
    add_course_id() |>
    dplyr::mutate(course_id = first_course_id(course_id, known)) |>
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
#' is append-only: a harvest fetches only offerings that no raw file holds yet
#' (a "current" site: a new snapshot of its latest DBH year) and saves them as
#' a new run, data/raw/{inst}/{year}/{date}.parquet (a second harvest the same
#' day: {date}_{HHMMSS}.parquet; write_raw_run()), with its fetch ledger,
#' data/raw/{inst}/ledger/{run}.parquet (fetch_ledger()). An existing raw file
#' is never written again. An offering in a raw file keeps its course_id when
#' a later DBH snapshot gives it another Status (first_course_id()).
#'
#' With `retry`, a site with the year in the URL also fetches again the
#' offerings whose page gave no plan in the current build
#' (data/interim/course_offerings_full.RDS), by how their URL answered last
#' (offering_outcome()): `retry = TRUE` all but those whose URL was not found
#' (a 404 is final), or only the outcome classes named, e.g. `retry =
#' c("timeout", "server_error")`, or `retry = "not_found"` for pages
#' published late. They are saved with `retry = TRUE`, and read_harvest()
#' takes their new page. Run targets::tar_make() first, and narrow it with `year`.
#'
#' @param courses Data frame from dbh_courses(). If NULL, the teacher-ed frame
#'   of the latest DBH snapshot: until tar_make() filters a frame, everything
#'   in data/raw enters the teacher-ed outputs.
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
#' @param institutions Optional character vector of institution names to harvest.
#'   If NULL, harvests all configured institutions.
#' @param retry FALSE, TRUE or outcome classes (FETCH_OUTCOMES): also fetch
#'   offerings whose page gave no plan
harvest_all <- function(courses = NULL, year = NULL, refetch = FALSE,
                        institutions = NULL, retry = FALSE) {
  if (is.null(courses)) courses <- dbh_courses("teacher_ed")
  if (!is.data.frame(courses)) {
    stop("`courses` must be a data frame, not ", class(courses)[1], ". ",
         "Did you mean harvest_all(institutions = ...)?", call. = FALSE)
  }
  if (!isFALSE(retry) && !isTRUE(retry) && !all(retry %in% FETCH_OUTCOMES)) {
    stop("`retry` must be TRUE, FALSE or outcome classes: ", paste(FETCH_OUTCOMES, collapse = ", "),
         call. = FALSE)
  }
  snapshot <- attr(courses, "dbh_snapshot") %||% NA_character_

  configs <- load_all_configs()
  inst_names <- if (!is.null(institutions)) institutions else names(configs)

  for (inst in inst_names) {
    message("\n=== ", inst, " ===")
    tryCatch({
      current <- identical(configs[[inst]]$plan_years, "current")
      known <- raw_course_ids(inst)
      skip <- if (current) character() else known
      again <- if (!isFALSE(retry) && length(skip)) intersect(skip, offerings_to_retry(inst, retry)) else character()
      if (length(again)) message(inst, ": ", length(again), " offerings without a plan to retry")
      # a retry fetches again, so the checkpoint's old pages are not reused
      result <- harvest_institution(inst, courses, year, refetch || length(again) > 0,
                                    setdiff(skip, again), known)
      if (nrow(result) == 0) {
        message(inst, ": nothing new to harvest")
      } else {
        result$harvested_at <- Sys.Date()
        if (length(again)) result$retry <- result$course_id %in% again
        run <- new_harvest_run(inst)
        write_raw_run(dplyr::select(result, -dplyr::any_of(REQUEST_COLUMNS)), inst, run)
        ledger <- fetch_ledger(result, inst, run, snapshot)
        write_fetch_ledger(ledger, inst, run)
        log_summary(inst, result)
        message("  outcomes: ", paste(names(table(ledger$outcome)), table(ledger$outcome), collapse = ", "))
      }
    }, error = function(e) {
      message("ERROR harvesting ", inst, ": ", conditionMessage(e))
    })
  }
}

# Offerings of an institution that harvest_all(retry = ) fetches again: no
# plan in the current build, and an outcome retry_wanted() takes.
offerings_to_retry <- function(institution, retry = TRUE,
                               file = "data/interim/course_offerings_full.RDS") {
  if (!file.exists(file)) stop(file, " is missing: run targets::tar_make() first")
  o <- readRDS(file)
  o <- o[o$institution == institution & is.na(o$plan_content_id), ]
  outcome <- offering_outcome(o$course_id, as.character(o$url), raw_ledger(harvest_files(institution)))
  o$course_id[retry_wanted(outcome, retry)]
}

#' Before harvesting a site whose pages change over time
#'
#' A "page" site (one page, several years) adds new years to its pages and a
#' "current" site replaces its plan, so a page fetched earlier must not stand
#' in for one fetched now: their checkpoints are removed and every page is
#' fetched again. A "current" site harvested in June-August may already
#' show next year's plan, so that gives a warning.
#'
#' @param config Institution config
#' @return TRUE, the `refetch` for the strategy
prepare_fresh_harvest <- function(config) {
  if (identical(config$plan_years, "current") &&
      as.integer(format(Sys.Date(), "%m")) %in% 6:8) {
    warning(config$name, " shows only the current plan: a harvest in June-August ",
            "may give next year's plan", call. = FALSE)
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
