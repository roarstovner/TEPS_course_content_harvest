# Data files of the static course plan browser (app/browser/README.md, "Data
# Contract"). Built from the anonymized files of a frame (data/processed/), the
# source URL and harvest date of each plan (data/interim/, url and
# harvested_at only), the DBH programmes snapshot (study types) and the plan
# gaps (why a course-year has no plan).

# The sections of plan_sections.RDS in the browser: `code` as in the data,
# `query` the name of the query syntax ("outcomes:dybdelæring"), and the
# labels. The order is the codebook's (section_codebook.yml).
BROWSER_SECTIONS <- tibble::tribble(
  ~code,                     ~query,          ~label_no,                         ~label_en,
  "course_content",          "content",       "Innhold",                         "Course content",
  "learning_outcomes",       "outcomes",      "Læringsutbytte",                  "Learning outcomes",
  "teaching_methods",        "teaching",      "Undervisnings- og læringsformer", "Teaching and learning methods",
  "assessment",              "assessment",    "Vurdering",                       "Assessment",
  "coursework_requirements", "coursework",    "Arbeidskrav",                     "Coursework requirements",
  "prerequisites",           "prerequisites", "Forkunnskapskrav",                "Prerequisites",
  "reading_list",            "reading",       "Pensum",                          "Reading list",
  "practice",                "practice",      "Praksis",                         "Practice"
)

#' Write the data files of the course plan browser
#'
#' Reads a frame's `course_plans.RDS`, `plan_sections.RDS` and
#' `course_offerings.RDS` (data/processed/), the `url` and `harvested_at` of
#' each plan's source offering from the frame's `course_offerings_full.RDS`
#' (data/interim/; no other column, so neither `html` nor `extracted_text`
#' gets in), the study types of the latest DBH programmes snapshot and the
#' reasons of data/interim/plan_gaps.RDS, and writes the files of
#' app/browser/README.md "Data Contract" to `out_dir`.
#'
#' @param frame A frame of `FRAMES`
#' @param out_dir Folder to write to (created; files there are replaced)
#' @return The paths written, invisibly. Stops before writing anything when a
#'   text column holds personal data (the patterns of target privacy_check).
build_browser_data <- function(frame = "teacher_ed", out_dir = "app/browser/public/data") {
  stopifnot(frame %in% FRAMES)
  processed <- frame_dir(frame)
  plans_file <- file.path(processed, "course_plans.RDS")
  full <- readRDS(file.path(frame_dir(frame, "interim"), "course_offerings_full.RDS"))
  sources <- full[c("course_id", "url", "harvested_at")]
  rm(full)
  programmes <- arrow::read_parquet(dbh_snapshot_file("programmes"),
                                    col_select = c("Institusjonskode", "Studieprogramkode", "Årstall",
                                                   "Studiumkode", "Studnavn"))
  plans <- readRDS(plans_file)
  plans$course_plan_normalized <- NULL
  tables <- browser_tables(
    plans = plans, sections = readRDS(file.path(processed, "plan_sections.RDS")),
    offerings = readRDS(file.path(processed, "course_offerings.RDS")), sources = sources,
    programmes = programmes, gaps = readRDS(file.path(frame_dir("all", "interim"), "plan_gaps.RDS")),
    version = paste(frame, "processed", format(file.mtime(plans_file), "%Y-%m-%dT%H:%M")),
    frame = frame)
  write_browser_data(tables, out_dir)
}

#' The browser's tables from the frame's data
#'
#' A course-year belongs to every programme DBH lists it under that year and
#' has every plan of its offerings that year, so `course_years` holds each
#' course-year x programme x plan once (DBH lists a course once per semester
#' and programme), with one NA-plan row per programme when the course-year has
#' no plan. `had_students` is TRUE when any of the course-year's offerings had
#' students.
#'
#' @param plans,sections,offerings The frame's course_plans, plan_sections and
#'   course_offerings
#' @param sources `course_id`, `url`, `harvested_at` of the offerings
#' @param programmes DBH programmes snapshot (DBH's column names):
#'   Institusjonskode, Studieprogramkode, Årstall, Studiumkode, Studnavn
#' @param gaps plan_gaps(): institution, course_code, year, has_plan, reason
#' @param version,frame For meta.json
#' @return List of tibbles `courses`, `course_years`, `programmes`, `plans`,
#'   `sections` and the list `meta`
browser_tables <- function(plans, sections, offerings, sources, programmes, gaps, version, frame) {
  cy_key <- c("institution", "course_code", "year")
  # the study type of each programme-year: the one of most snapshot rows
  study_types <- programmes |>
    dplyr::count(institution_code = Institusjonskode, programme_code = Studieprogramkode,
                 year = Årstall, study_type = Studiumkode) |>
    dplyr::arrange(dplyr::desc(n), study_type) |>
    dplyr::distinct(institution_code, programme_code, year, .keep_all = TRUE) |>
    dplyr::select(-n)

  course_years <- offerings |>
    dplyr::distinct(institution, institution_code, course_code, year, programme_code) |>
    dplyr::left_join(dplyr::filter(offerings, !is.na(plan_content_id)) |>
                       dplyr::distinct(institution, course_code, year, plan_content_id),
                     by = cy_key, relationship = "many-to-many") |>
    dplyr::left_join(study_types, by = c("institution_code", "programme_code", "year")) |>
    dplyr::left_join(dplyr::summarise(offerings, had_students = any(had_students), .by = dplyr::all_of(cy_key)),
                     by = cy_key) |>
    dplyr::left_join(dplyr::filter(gaps, !has_plan) |> dplyr::select(dplyr::all_of(cy_key), reason),
                     by = cy_key) |>
    dplyr::mutate(no_plan_reason = dplyr::if_else(is.na(plan_content_id), reason, NA_character_)) |>
    dplyr::select(institution, course_code, year, programme_code, study_type, plan_content_id,
                  had_students, no_plan_reason) |>
    dplyr::arrange(institution, course_code, year, programme_code, plan_content_id)

  latest <- offerings |>
    dplyr::arrange(dplyr::desc(year), dplyr::desc(semester))
  courses <- latest |>
    dplyr::distinct(institution, course_code, .keep_all = TRUE) |>
    dplyr::select(institution, course_code, course_name, level_code, credits, language_code,
                  subject_code, subject_name, nus_code, department_name) |>
    dplyr::left_join(dplyr::summarise(offerings, year_first = min(year), year_last = max(year),
                                      .by = c(institution, course_code)),
                     by = c("institution", "course_code")) |>
    dplyr::arrange(institution, course_code)

  programme_table <- latest |>
    dplyr::distinct(institution, institution_code, programme_code, .keep_all = TRUE) |>
    dplyr::left_join(study_types, by = c("institution_code", "programme_code", "year")) |>
    dplyr::select(institution, programme_code, programme_name, study_type) |>
    dplyr::arrange(institution, programme_code)

  plan_key <- c("plan_content_id", "institution", "course_code")
  plan_table <- plans |>
    dplyr::left_join(dplyr::rename(sources, source_url = url), by = c(source_course_id = "course_id")) |>
    dplyr::select(dplyr::all_of(plan_key), year_from, year_to, source_url, harvested_at, course_plan) |>
    dplyr::arrange(institution, course_code, year_from, year_to, plan_content_id)
  # sections in the plans' order, each plan's in their own order
  section_table <- sections |>
    dplyr::mutate(.plan = match(paste(plan_content_id, institution, course_code),
                                do.call(paste, plan_table[plan_key])),
                  .row = dplyr::row_number()) |>
    dplyr::filter(!is.na(.plan)) |>
    dplyr::arrange(.plan, .row) |>
    dplyr::select(dplyr::all_of(plan_key), section, text)

  institutions <- latest |>   # the latest name (Høgskolen i Innlandet became a university)
    dplyr::distinct(code = institution, .keep_all = TRUE) |>
    dplyr::select(code, institution_name, plan_year_basis) |>
    dplyr::arrange(code)
  type_names <- dplyr::distinct(programmes, code = Studiumkode, name = Studnavn) |>
    dplyr::distinct(code, .keep_all = TRUE)
  used_types <- sort(unique(stats::na.omit(c(course_years$study_type, programme_table$study_type))))
  meta <- list(
    version = version,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    frame = frame,
    counts = list(
      courses = nrow(courses),
      course_years = nrow(dplyr::distinct(course_years, institution, course_code, year)),
      course_years_with_plan = nrow(dplyr::distinct(dplyr::filter(course_years, !is.na(plan_content_id)),
                                                    institution, course_code, year)),
      programmes = nrow(programme_table),
      plans = nrow(plan_table),
      sections = nrow(section_table)),
    institutions = institutions,
    study_types = dplyr::left_join(tibble::tibble(code = used_types), type_names, by = "code"),
    sections = BROWSER_SECTIONS)
  list(courses = courses, course_years = course_years, programmes = programme_table,
       plans = plan_table, sections = section_table, meta = meta)
}

#' Write browser_tables() to `out_dir`
#'
#' Scans every character column of every table with the patterns of target
#' privacy_check (personal_data_in_frame()) and stops on a hit, before
#' writing. Parquet files are zstd-compressed (level 19).
#'
#' @param tables browser_tables()
#' @param out_dir Folder to write to
#' @return The paths written, invisibly
write_browser_data <- function(tables, out_dir) {
  parquet <- setdiff(names(tables), "meta")
  found <- dplyr::bind_rows(lapply(parquet, function(n) personal_data_in_frame(tables[[n]], n)))
  if (nrow(found) > 0) {
    stop("personal data in the browser data: ",
         paste(sprintf("%s$%s (%s, %d rows)", found$file, found$column, found$pattern, found$n),
               collapse = "; "), call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(out_dir, paste0(parquet, ".parquet"))
  for (i in seq_along(parquet)) {
    # level 19: the teacher-ed plans 5.6 MB, 7.8 MB at the default level 3 (2026-11-19)
    arrow::write_parquet(tables[[parquet[i]]], paths[i], compression = "zstd", compression_level = 19)
  }
  meta_path <- file.path(out_dir, "meta.json")
  jsonlite::write_json(tables$meta, meta_path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(c(meta_path, paths))
}
