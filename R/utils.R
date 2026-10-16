# Data folders (README "Data Files: Published and Internal"): data/input holds
# the DBH snapshots (in git); data/raw the harvest, which only harvesting
# writes; data/interim and data/processed are rebuilt by targets::tar_make(),
# interim with raw page text (personal data, internal), processed anonymized
# (can be shared). data/pilot holds the pilot samples (pilot_sample(), in git).
INPUT_DIR <- "data/input"
RAW_DIR <- "data/raw"
PILOT_DIR <- "data/pilot"

# The pilot sample of an institution (pilot_sample()).
pilot_file <- function(institution, pilot_dir = PILOT_DIR) {
  file.path(pilot_dir, paste0(institution, ".csv"))
}

# DBH study types (Studiumkode, table 347) of the five-year teacher education
# programmes: the teacher-ed frame (methods.qmd, "Population and sampling frame").
TEACHER_ED_STUDIUM <- c("INTMASTER", "IMALU5-10", "IMALU1-7", "LUPE")

# The frames the pipeline writes data files for: every harvested offering
# ("all", the default) and the teacher-ed filter of it (README "Frames").
FRAMES <- c("all", "teacher_ed")

# The folder of a frame's data files: data/{stage}/ for all courses,
# data/{stage}/{frame}/ for a filter.
frame_dir <- function(frame, stage = "processed") {
  ifelse(frame == "all", file.path("data", stage), file.path("data", stage, frame))
}

# A DBH snapshot written by data-raw/courses.R, of `date` or the latest:
# data/input/dbh_{table}_{YYYY-MM-DD}.parquet.
dbh_snapshot_file <- function(table = c("courses", "programmes"), date = NULL,
                              input_dir = INPUT_DIR) {
  table <- match.arg(table)
  files <- if (is.null(date)) sort(Sys.glob(file.path(input_dir, paste0("dbh_", table, "_*.parquet")))) else
    file.path(input_dir, sprintf("dbh_%s_%s.parquet", table, format(as.Date(date))))
  files <- files[file.exists(files)]
  if (!length(files)) stop("No DBH ", table, " snapshot in ", input_dir, ": run data-raw/courses.R")
  files[length(files)]
}

# The pipeline's names of DBH's columns (table 208), name = DBH's name. DBH's
# snapshots keep DBH's names; the raw files of the harvests until 2026-10-16
# and the releases up to data-2026-10-16 have them too, with `Emnekode_raw`
# for DBH's code and `Emnekode` for the code without its version number.
# dbh_rename() renames them where such data are read. data/variables.csv
# documents every variable of the data files (README "Data Structure").
DBH_COLUMNS <- c(
  institution_code = "Institusjonskode", institution_name = "Institusjonsnavn",
  department_code = "Avdelingskode", department_name = "Avdelingsnavn",
  department_code_ssb = "Avdelingskode_SSB", year = "Årstall", semester = "Semester",
  semester_name = "Semesternavn", programme_code = "Studieprogramkode",
  programme_name = "Studieprogramnavn", course_code_raw = "Emnekode_raw",
  course_code = "Emnekode", course_name = "Emnenavn", level_code = "Nivåkode",
  level_name = "Nivånavn", credits = "Studiepoeng", nus_code = "NUS-kode",
  status = "Status", status_name = "Statusnavn", language_code = "Underv.språk",
  language_name = "Navn", subject_code = "Fagkode", subject_name = "Fagnavn",
  thesis = "Oppgave (ny fra h2012)")

# Data with DBH's column names (or the old `institution_short`) under the
# pipeline's names; other columns as they are.
dbh_rename <- function(df) {
  dplyr::rename(df, dplyr::any_of(c(institution = "institution_short", DBH_COLUMNS)))
}

#' DBH course offerings from a snapshot
#'
#' DBH table 208 holds every course of every institution, year and semester
#' (all levels and statuses): the all-courses frame. `frame = "teacher_ed"`
#' keeps the courses of the teacher education programmes, those whose
#' programme has a study type in `TEACHER_ED_STUDIUM` in table 347 in some year,
#' matched on institution and programme code (a programme code is unique only
#' within an institution). Both come from the same snapshot, so the
#' teacher-ed offerings have the same course ids in both frames.
#'
#' @param frame "all" or "teacher_ed"
#' @param date Snapshot date; NULL for the latest
#' @param input_dir Folder of the snapshots
#' @return One row per course, year and semester, with DBH's columns under
#'   the pipeline's names (DBH_COLUMNS), DBH's code as `course_code_raw` and
#'   without a trailing version number as `course_code`, and `institution`
#'   (NA for a code no config has); the input of `harvest_all(courses =
#'   ...)`. Attribute `dbh_snapshot`: the snapshot's date, which harvest_all()
#'   records in the fetch ledger.
dbh_courses <- function(frame = c("all", "teacher_ed"), date = NULL, input_dir = INPUT_DIR) {
  frame <- match.arg(frame)
  file <- dbh_snapshot_file("courses", date, input_dir)
  courses <- dbh_rename(dplyr::rename(arrow::read_parquet(file), course_code_raw = "Emnekode"))
  if (frame == "teacher_ed") {
    programmes <- arrow::read_parquet(dbh_snapshot_file("programmes", date, input_dir),
                                      col_select = c("Institusjonskode", "Studieprogramkode", "Studiumkode"))
    keys <- dbh_rename(programmes) |>
      dplyr::filter(Studiumkode %in% TEACHER_ED_STUDIUM) |>
      dplyr::distinct(institution_code, programme_code)
    courses <- dplyr::semi_join(courses, keys, by = c("institution_code", "programme_code"))
  }
  courses <- courses |>
    # one row per offering: 2 of the 2.16 million rows of 2026-10-16 (2007)
    # repeat a course and semester
    dplyr::distinct(institution_code, course_code_raw, year, semester, .keep_all = TRUE) |>
    dplyr::mutate(institution = institution_from_code(institution_code),
                  course_code = canon_remove_trailing_num(course_code_raw)) |>
    dplyr::relocate(institution) |>
    dplyr::relocate(course_code, .after = course_code_raw)
  attr(courses, "dbh_snapshot") <- dbh_snapshot_date(file)
  courses
}

# The date of a DBH snapshot file, "YYYY-MM-DD".
dbh_snapshot_date <- function(file) sub("^dbh_[a-z]+_(.*)\\.parquet$", "\\1", basename(file))

# --- The raw store -----------------------------------------------------------
# Append-only Parquet (README "Raw Store and Finalized Releases"): one file per
# institution, DBH year and harvest run,
# data/raw/{inst}/{year}/{run}.parquet. A run is named by its date, a second
# run of the institution the same day by date and time ({YYYY-MM-DD}_{HHMMSS},
# which sorts after it). No raw file is written twice; read_harvest()
# combines them.

raw_file <- function(institution, year, run, raw_dir = RAW_DIR) {
  file.path(raw_dir, institution, year, paste0(run, ".parquet"))
}

# The harvest run of a raw file: its name.
raw_run <- function(file) sub("\\.parquet$", "", basename(file))

.raw_files <- function(raw_dir, institution = "*", year = "*") {
  f <- Sys.glob(file.path(raw_dir, institution, year, "*.parquet"))
  f[grepl("^\\d{4}$", basename(dirname(f)))]
}

# Institutions with a raw file.
harvested_institutions <- function(raw_dir = RAW_DIR) {
  sort(unique(basename(dirname(dirname(.raw_files(raw_dir))))))
}

# The partitions of the raw store: one row per institution and DBH year
# (`year`, as in the folder name) with a raw file.
raw_partitions <- function(raw_dir = RAW_DIR) {
  f <- .raw_files(raw_dir)
  p <- unique(tibble::tibble(institution = basename(dirname(dirname(f))), year = basename(dirname(f))))
  p[order(p$institution, p$year, method = "radix"), ]
}

# All raw files of an institution (or of one of its years), run by run, each
# run's years in order.
harvest_files <- function(institution, raw_dir = RAW_DIR, year = "*") {
  f <- .raw_files(raw_dir, institution, year)
  # byte order: the locale's ignores punctuation and puts {date}_{time} first
  f[order(raw_run(f), basename(dirname(f)), method = "radix")]
}

# The name of a new harvest run: today's date, with the time when the
# institution has a run of that name already.
new_harvest_run <- function(institution, raw_dir = RAW_DIR, time = Sys.time()) {
  run <- format(time, "%Y-%m-%d")
  if (run %in% raw_run(harvest_files(institution, raw_dir))) run <- format(time, "%Y-%m-%d_%H%M%S")
  run
}

#' Write a harvest run to the raw store
#'
#' One Parquet file (zstd) per DBH year of the rows. The rows keep every column
#' the harvest gave them, except that `html_error` is kept as text
#' (fetch_error_text()), `url` as text, and `extracted_text` only where there
#' is no `html` (text read from a PDF, which is the raw data there; elsewhere
#' it is rebuilt from the html). A `pdf` column (the PDF's bytes, uis since
#' 2026-10-16) is stored as binary. `harvest_row` is each row's position in
#' the run, so read_raw() gives the rows in the order the harvest returned them.
#'
#' @param rows Harvested rows of one institution (harvest_institution()).
#' @param institution,run The institution and the name of the run
#'   (new_harvest_run()).
#' @return The files written, invisibly.
write_raw_run <- function(rows, institution, run, raw_dir = RAW_DIR) {
  html <- rows$html %||% rep(NA_character_, nrow(rows))
  rows <- dplyr::mutate(rows, dplyr::across(dplyr::where(~ inherits(.x, "glue")), as.character))
  rows$html_error <- fetch_error_text(rows$html_error %||% vector("list", nrow(rows)))
  if ("extracted_text" %in% names(rows)) rows$extracted_text[!is.na(html) & nzchar(html)] <- NA
  if ("pdf" %in% names(rows)) {
    rows$pdf <- structure(lapply(rows$pdf, function(x) x), class = c("arrow_binary", "vctrs_vctr", "list"))
  }
  rows$harvest_row <- seq_len(nrow(rows))
  files <- raw_file(institution, rows$year, run, raw_dir)
  if (any(file.exists(files)) || run %in% raw_run(harvest_files(institution, raw_dir))) {
    stop("raw files are not written twice: run ", run, " of ", institution, " exists")
  }
  for (f in unique(files)) .write_plain_parquet(rows[files == f, ], f)
  invisible(unique(files))
}

.write_plain_parquet <- function(df, file) {
  tab <- arrow::Table$create(df)
  tab$metadata$r <- NULL   # plain Parquet, without R's attributes
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(tab, file, compression = "zstd", compression_level = 9)
}

#' Rows of raw files in the order they were harvested
#'
#' Run by run, each run's rows in the order its harvest returned them, with
#' the pipeline's column names (dbh_rename()): the harvests until 2026-10-16
#' have DBH's, those of 2026-04-04 `institution_short` for `institution`.
#'
#' @param files Raw files of one institution (harvest_files()).
#' @param run Keep `.run`, the run of each row.
#' @param content FALSE reads the rows without their pages, for choosing
#'   between them: `html` and `extracted_text` hold the row's page id
#'   (raw_page_id()) where they hold a page, and `pdf` is left out. The pages
#'   are not read into R.
#' @param page_id Add `page_id`, the row's page id.
read_raw <- function(files, run = FALSE, content = TRUE, page_id = FALSE) {
  if (!length(files)) return(tibble::tibble())
  rows <- dplyr::bind_rows(lapply(files, function(f) {
    df <- if (content) arrow::read_parquet(f) else .raw_without_pages(f)
    df <- dbh_rename(df)
    if (page_id) df$page_id <- raw_page_id(f, df$harvest_row)
    df$.run <- raw_run(f)
    df
  }))
  rows <- rows[order(rows$.run, rows$harvest_row, method = "radix"), ]
  rows$harvest_row <- NULL
  if (!run) rows$.run <- NULL
  rows
}

# The id of a raw row's page: its raw file and its place in the run,
# "{institution}/{year}/{run}/{harvest_row}".
raw_page_id <- function(file, harvest_row) {
  if (!length(harvest_row)) return(character())
  paste(basename(dirname(dirname(file))), basename(dirname(file)), raw_run(file), harvest_row, sep = "/")
}

# A raw file's rows with the page id in place of each page (read_raw()).
.raw_without_pages <- function(f) {
  cols <- arrow::ParquetFileReader$create(f)$GetSchema()$names
  page <- intersect(c("html", "extracted_text"), cols)
  df <- arrow::read_parquet(f, col_select = dplyr::all_of(setdiff(cols, c(page, "pdf"))))
  if (length(page)) {
    stored <- arrow::read_parquet(f, col_select = dplyr::all_of(page), as_data_frame = FALSE)
    for (p in page) {
      id <- raw_page_id(f, df$harvest_row)
      id[as.vector(is.na(stored[[p]]))] <- NA_character_
      df[[p]] <- id
    }
  }
  df[setdiff(cols, "pdf")]
}

# The course ids in an institution's raw files.
raw_course_ids <- function(institution, raw_dir = RAW_DIR) {
  as.character(unlist(lapply(harvest_files(institution, raw_dir), function(f)
    arrow::read_parquet(f, col_select = "course_id")$course_id)))
}

# A fetch error as text: NA for none, else the message, with its cause
# (httr2's "Failed to perform HTTP request." of a timeout) since 2026-10-16
# (the raw store keeps text; a harvest gives conditions or messages, NULL for none).
fetch_error_text <- function(err) {
  if (is.character(err)) return(err)
  vapply(err, function(e) {
    if (is.null(e)) NA_character_ else if (is.character(e)) e[1]
    else if (is.null(e$message)) NA_character_
    else paste(c(e$message, e$parent$message), collapse = " ")
  }, character(1))
}

# Date of each row's harvest run: the run's harvested_at (stamped since
# 2026-10-06), else the date its name holds. Never a file's modification
# time, which a copy changes.
harvest_date <- function(run, harvested_at = NULL) {
  stamped <- as.Date(harvested_at %||% rep(NA_real_, length(run)))
  out <- as.Date(rep(NA_real_, length(run)))
  for (r in unique(run)) {
    i <- run == r
    d <- if (any(!is.na(stamped[i]))) max(stamped[i], na.rm = TRUE) else
      as.Date(substr(r, 1, 10), optional = TRUE)
    if (is.na(d)) stop("No harvest date for run ", r, ": no harvested_at, no date in its name")
    out[i] <- d
  }
  out
}

# --- The fetch ledger --------------------------------------------------------
# How each offering of a harvest run was fetched (README "Fetch Ledger and
# Retries"). harvest_all() writes it with the run, as
# data/raw/{inst}/ledger/{run}.parquet (write_fetch_ledger()), append-only
# like the raw files; raw_ledger() reads it and reconstructs it for a run
# without one (the harvests before 2026-10-16).

# The outcome classes of a fetch (fetch_outcome()), and those that
# harvest_all(retry = TRUE) fetches again: all but not_found.
FETCH_OUTCOMES <- c("ok", "not_found", "shell", "server_error", "timeout", "blocked", "error", "no_url")
RETRY_OUTCOMES <- setdiff(FETCH_OUTCOMES, "not_found")
# A site that answers 403 or 429 refuses the harvest: its harvest stops.
BLOCKED_STATUS <- c(403L, 429L)
# Columns of a harvest's rows that only the ledger keeps.
REQUEST_COLUMNS <- c("http_status", "requested_at")

#' The outcome class of a fetch
#'
#' "ok": a page (or a PDF's text) was stored; "not_found": HTTP 404 or 410,
#' HTTP 400 (UiB's answer for a semester before its first), or the site's
#' own not-found page (HVL's "Course not found (soft 404)"); "shell": the
#' site answers that it has nothing for that year (NTNU's "No course
#' information available for this academic year"); "server_error": HTTP 5xx;
#' "timeout": no answer (a timeout or a failed connection); "blocked": HTTP
#' 403 or 429; "error": anything else; "no_url": no URL to request.
#'
#' @param url,status,error The URL, HTTP status (NA if unknown) and fetch
#'   error text (fetch_error_text()) of each fetch.
#' @param page Whether a page was stored.
fetch_outcome <- function(url, page, status, error) {
  dplyr::case_when(
    page ~ "ok",
    is.na(url) ~ "no_url",
    status %in% c(400L, 404L, 410L) | grepl("soft 404", error, fixed = TRUE) ~ "not_found",
    grepl("No course information available", error, fixed = TRUE) ~ "shell",
    status %in% BLOCKED_STATUS ~ "blocked",
    status >= 500L ~ "server_error",
    status %in% 408L | grepl("Failed to perform HTTP request|[Tt]imeout|timed out", error) ~ "timeout",
    .default = "error")
}

#' The fetch ledger of a harvest run
#'
#' One row per offering of the run, in the run's order: `run`,
#' `institution`, `course_id`, `url`, `requested_at` (when the request was
#' sent), `http_status`, `outcome` (fetch_outcome()), `bytes` and `sha256` of
#' the page as stored (`html`, else the text read from a PDF), `raw_file` (the
#' raw file of the row) and `dbh_snapshot` (the date of the DBH snapshot the
#' offerings came from). `requested_at` and `http_status` come from the fetch
#' (fetch_html_cols()); where the rows lack them they are NA, and
#' `http_status` is then read from the error message ("HTTP 404 Not Found.").
#'
#' @param rows The run's rows (harvest_institution(), or read_raw()).
#' @param institution,run The institution and the run's name.
#' @param dbh_snapshot The DBH snapshot's date, if known.
fetch_ledger <- function(rows, institution, run, dbh_snapshot = NA_character_, raw_dir = RAW_DIR) {
  n <- nrow(rows)
  col <- function(x, na) rows[[x]] %||% rep(na, n)
  html <- col("html", NA_character_)
  page <- dplyr::if_else(!is.na(html) & nzchar(html), html, as.character(col("extracted_text", NA_character_)))
  error <- fetch_error_text(rows[["html_error"]] %||% vector("list", n))
  status <- dplyr::coalesce(as.integer(col("http_status", NA_integer_)),
                            as.integer(stringr::str_match(error, "^HTTP (\\d{3})")[, 2]))
  url <- as.character(col("url", NA_character_))
  tibble::tibble(
    run = rep(run, n), institution = rep(institution, n), course_id = rows$course_id, url = url,
    requested_at = rows[["requested_at"]] %||% .POSIXct(rep(NA_real_, n)),
    http_status = status,
    outcome = fetch_outcome(url, !is.na(page), status, error),
    bytes = ifelse(is.na(page), NA_integer_, nchar(page, "bytes")),
    sha256 = cli::hash_sha256(page),
    raw_file = raw_file(institution, rows$year, run, raw_dir),
    dbh_snapshot = rep(as.character(dbh_snapshot), n))
}

ledger_file <- function(institution, run, raw_dir = RAW_DIR) {
  file.path(raw_dir, institution, "ledger", paste0(run, ".parquet"))
}

ledger_files <- function(institution, raw_dir = RAW_DIR) {
  sort(Sys.glob(file.path(raw_dir, institution, "ledger", "*.parquet")), method = "radix")
}

# Write a run's fetch ledger (fetch_ledger()); like a raw file, never twice.
write_fetch_ledger <- function(ledger, institution, run, raw_dir = RAW_DIR) {
  f <- ledger_file(institution, run, raw_dir)
  if (file.exists(f)) stop("raw files are not written twice: ", f, " exists")
  .write_plain_parquet(ledger, f)
  invisible(f)
}

#' The fetch ledger of raw files
#'
#' Run by run: the ledger harvest_all() wrote with the run (`recorded`), or,
#' for a run without one (harvests before 2026-10-16), the ledger
#' reconstructed from its raw rows (fetch_ledger(): no `requested_at` or
#' `dbh_snapshot`, `http_status` only from an error message). Derived data:
#' the {targets} pipeline rebuilds it (target `ledger`).
#'
#' @param files Raw files of one institution (harvest_files()).
raw_ledger <- function(files) {
  if (!length(files)) {
    none <- fetch_ledger(tibble::tibble(course_id = character(), year = integer()), "", "")
    return(dplyr::mutate(none, recorded = logical()))
  }
  dir <- dirname(dirname(files))
  runs <- unique(data.frame(dir = dir, run = raw_run(files)))
  dplyr::bind_rows(lapply(seq_len(nrow(runs)), function(i) {
    inst <- basename(runs$dir[i])
    lf <- ledger_file(inst, runs$run[i], dirname(runs$dir[i]))
    if (file.exists(lf)) return(dplyr::mutate(arrow::read_parquet(lf), recorded = TRUE))
    rows <- read_raw(files[dir == runs$dir[i] & raw_run(files) == runs$run[i]])
    dplyr::mutate(fetch_ledger(rows, inst, runs$run[i], raw_dir = dirname(runs$dir[i])), recorded = FALSE)
  }))
}

#' How the URL of each offering answered, last
#'
#' The outcome of the latest request of the offering's URL, whichever
#' offering it was made for (an academic-year site shares a page between
#' offerings); for an offering without a URL, its own latest outcome. "not
#' requested": nobody requested the URL; "not harvested": the offering is in
#' no raw run.
#'
#' @param course_id,url Offerings and the URL the build gives them
#'   (course_offerings_full).
#' @param ledger The ledger of the institutions' raw files (raw_ledger()).
offering_outcome <- function(course_id, url, ledger) {
  own <- ledger[!duplicated(ledger$course_id, fromLast = TRUE), ]
  hit <- ledger[!is.na(ledger$url), ]
  hit <- hit[!duplicated(hit$url, fromLast = TRUE), ]
  out <- dplyr::if_else(is.na(url), own$outcome[match(course_id, own$course_id)],
                        dplyr::coalesce(hit$outcome[match(url, hit$url)], "not requested"))
  out[!course_id %in% ledger$course_id] <- "not harvested"
  out
}

# Whether harvest_all(retry = ) fetches an offering without a plan again,
# by its offering_outcome(): `retry` TRUE takes every outcome but not_found,
# a character vector the outcomes it names; a URL nobody requested always.
retry_wanted <- function(outcome, retry = TRUE) {
  outcome %in% c(if (isTRUE(retry)) RETRY_OUTCOMES else retry, "not requested")
}

# Academic year ("2025-2026", August to July) of a date.
academic_year_of_date <- function(date) {
  y <- as.integer(format(date, "%Y")) - (as.integer(format(date, "%m")) < 8)
  out <- sprintf("%d-%d", y, y + 1)
  out[is.na(y)] <- NA_character_
  out
}

canon_remove_trailing_num <- function(x) {
  sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)
}

canon_semester_name <- function(semester_name) {
  dplyr::case_match(
    semester_name,
    "Vår" ~ "spring",
    "Høst" ~ "autumn",
    "Sommer" ~ "summer",
    .default = semester_name
  )
}

# The year an offering's academic year (August-July) starts: autumn Y -> Y,
# spring and summer Y -> Y - 1
academic_start_year <- function(year, semester) {
  year - (semester %in% c("Vår", "Sommer"))
}

nla_academic_year <- function(year, semester) {
  dplyr::case_match(semester,
    "Høst"            ~ paste0(year, "-", year + 1),
    c("Vår", "Sommer") ~ paste0(year - 1, "-", year)
  )
}

semester_to_url <- function(semester) {
  dplyr::case_match(semester,
    "Vår"  ~ "var",
    "Høst" ~ "host",
    .default = tolower(semester)
  )
}


#' Add Course ID
#'
#' Creates a unique course identifier by combining institution, course code,
#' year, semester, and status information.
#'
#' @param dbh_df A data frame containing course information with columns:
#'   `institution`, `course_code_raw`, `year`, `semester_name`, and `status`.
#'
#' @return A data frame with an additional `course_id` column placed first.
#'   The course_id format is: `{institution}_{code}_{year}_{semester}_{status}`.
#'
#' @details
#' status codes: 1 = Aktivt, 2 = Nytt, 3 = Avviklet, 4 = Avviklet, men tas eksamen.
#' Uses `course_code_raw` instead of `course_code` which may result in fewer duplicates
#' if the raw code is more granular, but could miss normalization benefits.
add_course_id <- function(dbh_df) {
  dbh_df |>
    dplyr::mutate(
      course_id = paste(
        institution,
        course_code_raw, # Using `course_code_raw` instead of `course_code` may result in fewer duplicates if the raw code is more granular, but could miss normalization or grouping benefits provided by `course_code`.
        year,
        canon_semester_name(semester_name),
        status, # 1: Aktivt, 2: Nytt, 3: Avviklet, 4: Avviklet, men tas eksamen
        sep = "_"
      )
    ) |> 
    dplyr::relocate(course_id, .before = 1)
}

# An offering keeps the course_id it was first harvested under. The id ends
# with DBH's status, which a later DBH snapshot can change; the rest
# (institution, code, year, semester) is the offering, one row in DBH.
offering_key <- function(course_id) sub("_[^_]*$", "", course_id)

first_course_id <- function(course_id, known) {
  i <- match(offering_key(course_id), offering_key(known))
  course_id[!is.na(i)] <- known[i[!is.na(i)]]
  course_id
}


validate_courses <- function(df, stage = c("initial", "with_url", "with_html")) {
  stage <- match.arg(stage)

  required <- switch(stage,
    initial = c("institution", "course_code", "year"),
    with_url = c("institution", "course_id", "url"),
    with_html = c("institution", "course_id", "url", "html", "html_success"),
    c() # default case: returns empty character vector if stage does not match
  )
  
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns at {stage} stage: {paste(missing, collapse = ', ')}")
  }
  invisible(df)
}
