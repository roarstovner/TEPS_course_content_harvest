# R/harvest.R
# Single entry point for harvesting course content from all institutions.
# Dispatches to strategy functions in R/harvest_strategies.R via config
# from R/institution_config.R.

#' Harvest one institution
#'
#' @param institution Character, e.g. "oslomet"
#' @param courses Data frame from dbh_courses() (pre-filtered or not)
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, remove the institution's checkpoints and
#'   re-download everything
#' @param skip Course ids not to harvest (already in a raw file)
#' @param known Course ids in the raw files: an offering among them keeps its
#'   id, also when a later DBH snapshot gives it another status
#' @return Data frame with DBH columns plus course_id, url, html, html_error,
#'   html_success, extracted_text. A URL robots.txt disallows is not
#'   requested (robots_allows(); its html_error says so), and a host whose
#'   robots.txt has not been read (no row in data/robots_txt.csv) is an
#'   error, before any request to it.
harvest_institution <- function(institution, courses, year = NULL,
                                refetch = FALSE, skip = character(), known = skip) {
  config <- get_institution_config(institution)
  if (identical(config$plan_years, "current")) prepare_fresh_harvest(config, refetch)
  else if (refetch) remove_checkpoints(institution)

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
  # robots.txt (robots_allows()): a host not read stops the harvest here,
  # before any request; a URL it disallows is not requested
  robots <- sum(!robots_allows(df$url), na.rm = TRUE)
  if (robots) message(institution, ": robots.txt disallows ", robots, " of them: not requested")

  result <- switch(config$strategy,
    standard           = harvest_standard(df, config),
    url_discovery      = harvest_url_discovery(df, config),
    shadow_dom         = harvest_shadow_dom(df, config),
    html_pdf_discovery = harvest_html_pdf_discovery(df, config),
    pdf_split          = harvest_pdf_split(df, config),
    json_extract       = harvest_json_extract(df, config),
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
#' Loops through all configured institutions and harvests each, one DBH year
#' at a time, newest first (harvest_years()). The raw store is append-only: a
#' harvest fetches only offerings that no raw file holds yet (a "current"
#' site: a new snapshot of its latest DBH year) and saves each year's as a new
#' run, data/raw/{inst}/{year}/{date}.parquet (a later run the same day:
#' {date}_{HHMMSS}.parquet; write_raw_run()), with its fetch ledger,
#' data/raw/{inst}/ledger/{run}.parquet (fetch_ledger()). An existing raw file
#' is never written again. An offering in a raw file keeps its course_id when
#' a later DBH snapshot gives it another status (first_course_id()).
#'
#' Memory holds one year's pages, the html checkpoints at most two years':
#' the year before's pages can serve this year's offerings (an autumn and the
#' next spring share an academic-year page), so a year's html checkpoint is
#' removed once its run and the next year's are written. The institution's
#' URL, discovery and PDF checkpoints serve every year: they are removed once
#' all its years are written. An error (a site that refuses the harvest)
#' stops the institution's harvest with its checkpoints kept, and the next
#' harvest resumes there.
#'
#' With `retry`, a site with the year in the URL also fetches again the
#' offerings whose page gave no plan in the current build
#' (data/interim/course_offerings_full.RDS), by how their URL answered last
#' (offering_outcome()): `retry = TRUE` all but those whose URL was not found
#' (a 404 is final), answered 403 (forbidden) or robots.txt disallows, or only
#' the outcome classes named, e.g. `retry = c("timeout", "server_error")`, or
#' `retry = "not_found"` for pages published late, `retry = "robots"` after a
#' permission (`robots_permission`). They are saved with `retry = TRUE`, and read_harvest()
#' takes their new page. Run targets::tar_make() first, and narrow it with `years`.
#'
#' @param courses Data frame from dbh_courses(). If NULL, every course of the
#'   latest DBH snapshot (all years: weeks at some sites; the teacher-ed
#'   outputs of tar_make() keep only the teacher-ed frame).
#' @param years Optional integer vector: only harvest these years
#' @param refetch Logical — if TRUE, remove the checkpoints and re-download everything
#' @param institutions Optional character vector of institution names to harvest.
#'   If NULL, harvests all configured institutions.
#' @param retry FALSE, TRUE or outcome classes (FETCH_OUTCOMES): also fetch
#'   offerings whose page gave no plan
#' @param year One year, as `years` (the argument until 2026-10-28)
harvest_all <- function(courses = NULL, years = NULL, refetch = FALSE,
                        institutions = NULL, retry = FALSE, year = NULL) {
  if (is.null(courses)) courses <- dbh_courses()
  if (!is.data.frame(courses)) {
    stop("`courses` must be a data frame, not ", class(courses)[1], ". ",
         "Did you mean harvest_all(institutions = ...)?", call. = FALSE)
  }
  if (!isFALSE(retry) && !isTRUE(retry) && !all(retry %in% FETCH_OUTCOMES)) {
    stop("`retry` must be TRUE, FALSE or outcome classes: ", paste(FETCH_OUTCOMES, collapse = ", "),
         call. = FALSE)
  }
  snapshot <- attr(courses, "dbh_snapshot") %||% NA_character_
  years <- years %||% year

  configs <- load_all_configs()
  inst_names <- if (!is.null(institutions)) institutions else names(configs)
  # only the offerings of these institutions are kept: DBH's 2.16 million
  # offerings (2026-10-16) take some 0.5 GB for the whole harvest
  courses <- courses[courses$institution %in% inst_names, ]
  gc()

  for (inst in inst_names) {
    message("\n=== ", inst, " ===")
    tryCatch({
      config <- get_institution_config(inst)
      current <- identical(config$plan_years, "current")
      known <- raw_course_ids(inst)
      skip <- if (current) character() else known
      again <- if (!isFALSE(retry) && length(skip)) intersect(skip, offerings_to_retry(inst, retry)) else character()
      if (length(again)) message(inst, ": ", length(again), " offerings without a plan to retry")
      take_probes(inst)   # none from an earlier harvest that stopped
      # a retry fetches again, so the checkpoints' old pages are not reused
      if (refetch || length(again)) remove_checkpoints(inst)
      offerings <- courses[courses$institution %in% inst, ]
      inst_years <- harvest_years(offerings, config, years)
      if (!length(inst_years)) message(inst, ": nothing new to harvest")
      for (k in seq_along(inst_years)) {
        result <- harvest_institution(inst, offerings, inst_years[k], skip = setdiff(skip, again), known = known)
        if (nrow(result) == 0) {
          message(inst, " ", inst_years[k], ": nothing new to harvest")
        } else {
          result$harvested_at <- Sys.Date()
          if (length(again)) result$retry <- result$course_id %in% again
          run <- new_harvest_run(inst)
          write_raw_run(dplyr::select(result, -dplyr::any_of(REQUEST_COLUMNS)), inst, run)
          ledger <- fetch_ledger(result, inst, run, snapshot, probes = take_probes(inst))
          write_fetch_ledger(ledger, inst, run)
          log_summary(inst, result)
          message("  outcomes: ", paste(names(table(ledger$outcome)), table(ledger$outcome), collapse = ", "))
        }
        # the year before's pages, which could serve this year's offerings,
        # are in its run and this one now
        if (k > 1) remove_checkpoints(inst, inst_years[k - 1])
        # not held while the next year is fetched: a year's pages are some
        # 1 GB at NTNU (11,000 pages of 90 KB in 2025)
        result <- ledger <- NULL
        gc()
      }
      remove_checkpoints(inst, c(inst_years, "urls", "discovery", "pdf"))
    }, error = function(e) {
      message("ERROR harvesting ", inst, ": ", conditionMessage(e))
    })
  }
}

#' The years harvest_all() harvests of an institution
#'
#' @param courses The institution's DBH offerings
#' @param config Institution config
#' @param years The years asked for; NULL for every year of `courses` (a
#'   "current" site: the latest, apply_year_filter())
#' @return The years, newest first
harvest_years <- function(courses, config, years = NULL) {
  y <- years %||% unique(courses$year)
  if (is.null(years) && identical(config$plan_years, "current") && length(y)) y <- max(y)
  sort(unique(y), decreasing = TRUE)
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

# The pilot sample (docs/new-institution.md "Piloting an Institution";
# methods.qmd "Pilot protocol and gate"): course-years in the pilot years, with
# at least PILOT_COVER course-years of each level group, language group and
# department.
PILOT_YEARS <- c(2019L, 2022L, 2025L)
PILOT_N <- 100L
PILOT_COVER <- c(level = 5, language = 10, department_name = 1)
PILOT_SEED <- 42L

# Continuing education: a programme named so, in Norwegian or English.
PILOT_CONTINUING_RX <- paste0("videreutdanning|vidareutdanning|etter- og vid|etterutdanning|",
                              "\\bEVU\\b|continuing education|experience-based|erfaringsbasert")

#' The level group of DBH offerings, for the pilot sample
#'
#' "continuing" (a continuing-education programme, PILOT_CONTINUING_RX, or
#' DBH's experience-based master, ME), "phd" (FU), "higher" (master level:
#' HN, M2, MX), "lower" (bachelor level: LN, B3, HK, AR, YU), "other"
#' (upper-secondary level, VS, or none).
#'
#' @param level_code,programme_name DBH's level and programme name.
pilot_level <- function(level_code, programme_name) {
  dplyr::case_when(
    level_code %in% "ME" | grepl(PILOT_CONTINUING_RX, programme_name, ignore.case = TRUE) ~ "continuing",
    level_code %in% "FU" ~ "phd",
    level_code %in% c("HN", "M2", "MX") ~ "higher",
    level_code %in% c("LN", "B3", "HK", "AR", "YU") ~ "lower",
    .default = "other")
}

#' A pilot sample of an institution's DBH offerings
#'
#' Step 1 of the pilot protocol (docs/new-institution.md "Piloting an
#' Institution"): a stratified sample of the institution's course-years
#' (course_code_raw x year, with every semester DBH lists for it: a course
#' taught in one semester has no page in the other) in `oldest_year` and the
#' `years` after it (a site has no plans before its oldest year). Drawn in a
#' random order that `seed` fixes, in two steps: (1) cover: PILOT_COVER
#' course-years of each level group (pilot_level()), of each language group
#' (English-taught, language_code ENG in some semester, or not) and of each
#' department (department_name: a faculty at a college, an institute at a
#' university); (2) fill up to `n` course-years, in equal numbers per year.
#' Step 1 can take more than `n` at an institution with many departments.
#' A site that shows only the current plan (`plan_years = "current"`) is
#' sampled in the latest DBH year only.
#'
#' @param institution The institution (a config name).
#' @param oldest_year The oldest year the site has plans for (read on the
#'   site), or NULL.
#' @param n Course-years to draw.
#' @param years The pilot years.
#' @param seed The seed: the same DBH snapshot, arguments and seed give the
#'   same sample.
#' @param courses DBH offerings (dbh_courses()).
#' @param within Course ids: draw only among these offerings (by
#'   offering_key()), e.g. raw_course_ids() to try the gate on data already
#'   harvested; NULL for all.
#' @param file Where to write the sample: course_id, the course-year and its
#'   strata, the DBH snapshot and the seed (pilot_file(); in git). NULL for
#'   nowhere.
#' @return The sampled offerings: rows of `courses` with its columns, the
#'   input of harvest_all(courses = ).
pilot_sample <- function(institution, oldest_year = NULL, n = PILOT_N, years = PILOT_YEARS,
                         seed = PILOT_SEED, courses = dbh_courses(), within = NULL,
                         file = pilot_file(institution)) {
  x <- add_course_id(courses[courses$institution %in% institution, ])
  if (!is.null(within)) x <- x[offering_key(x$course_id) %in% offering_key(within), ]
  if (identical(get_institution_config(institution)$plan_years, "current")) {
    years <- max(x$year)
  } else years <- sort(unique(c(oldest_year, years[years > max(oldest_year, 0)])))
  x <- x[x$year %in% years, ]
  if (!nrow(x)) stop(institution, ": no DBH offerings in ", paste(years, collapse = ", "), call. = FALSE)
  x$unit <- paste(x$course_code_raw, x$year)
  english <- tapply(x$language_code %in% "ENG", x$unit, any)
  x$language <- ifelse(english[x$unit], "english", "other")
  x$level <- pilot_level(x$level_code, x$programme_name)

  set.seed(seed)
  units <- x[!duplicated(x$unit), c("unit", "year", names(PILOT_COVER))]
  units <- units[sample.int(nrow(units)), ]
  pick <- rep(FALSE, nrow(units))
  for (dim in names(PILOT_COVER)) for (v in unique(units[[dim]])) {
    have <- which(units[[dim]] %in% v)
    pick[utils::head(have[!pick[have]], max(0, PILOT_COVER[[dim]] - sum(pick[have])))] <- TRUE
  }
  # each year's next course-year in the random order, the years with the
  # fewest so far first
  rest <- which(!pick)
  picked <- tapply(pick, as.character(units$year), sum)
  k <- stats::ave(rest, units$year[rest], FUN = seq_along) + picked[as.character(units$year[rest])]
  pick[utils::head(rest[order(k)], max(0, n - sum(pick)))] <- TRUE

  out <- x[x$unit %in% units$unit[pick], ]
  out <- out[order(out$year, out$course_code_raw, out$course_id, method = "radix"), ]
  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(data.frame(
      out[c("institution", "course_id", "course_code_raw", "year", "semester_name",
            "department_name", "level", "language")],
      dbh_snapshot = attr(courses, "dbh_snapshot") %||% NA_character_, seed = seed),
      file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  out <- out[names(courses)]   # harvest_all() keeps every column in the raw files
  attr(out, "dbh_snapshot") <- attr(courses, "dbh_snapshot")
  out
}

#' Before harvesting a site that shows only the current plan
#'
#' A "current" site replaces its plan, so a page fetched in an earlier
#' academic year must not stand in for one fetched now: the checkpoints are
#' removed unless they were started in this academic year (from 1 August),
#' recorded in data/raw/checkpoint/{inst}/academic_year. A harvest that
#' stopped then resumes where it was. A harvest in June-August may already
#' show next year's plan, so that gives a warning.
#'
#' @param config Institution config
#' @param refetch TRUE removes the checkpoints in any case
prepare_fresh_harvest <- function(config, refetch = FALSE) {
  if (as.integer(format(Sys.Date(), "%m")) %in% 6:8) {
    warning(config$name, " shows only the current plan: a harvest in June-August ",
            "may give next year's plan", call. = FALSE)
  }
  marker <- file.path(RAW_DIR, "checkpoint", config$name, "academic_year")
  this <- as.character(as.integer(format(Sys.Date(), "%Y")) - (as.integer(format(Sys.Date(), "%m")) < 8))
  if (refetch || !file.exists(marker) || !identical(readLines(marker, warn = FALSE), this)) {
    remove_checkpoints(config$name)
  }
  dir.create(dirname(marker), recursive = TRUE, showWarnings = FALSE)
  writeLines(this, marker)
  invisible(this)
}

#' Apply year filter based on config
#'
#' A "current" site (`plan_years`) shows only the plan in force, so without an
#' explicit year only the latest DBH year is harvested. A "url" site gives
#' every year its own plan. An explicit year always filters to that year.
#'
#' @param df Data frame with year column
#' @param config Institution config list
#' @param year Optional explicit year to filter to
#' @return Filtered data frame
apply_year_filter <- function(df, config, year = NULL) {
  if (!is.null(year)) {
    message("Filtering to year ", year)
    return(dplyr::filter(df, .data$year == .env$year))
  }
  if (identical(config$plan_years, "current")) {
    max_year <- max(df$year, na.rm = TRUE)
    message("plan_years = \"current\" — filtering to max year: ", max_year)
    return(dplyr::filter(df, year == max_year))
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
