# Data files of the static course plan browser (app/browser/README.md, "Data
# Contract"). Built from the anonymized files of a frame (data/processed/), the
# source URL and harvest date of each plan (data/interim/, url and
# harvested_at only), the study type tree (data/study_types.csv), the plan
# gaps (why a course-year has no plan) and, from the DBH input, the courses of
# each study type at institutions not harvested.

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
#' gets in), the study type tree of `types_file` and the reasons of
#' data/interim/plan_gaps.RDS and the courses of each study type at the
#' institutions not harvested (DBH tables 208 and 347, uncovered_study_types()),
#' and writes the files of app/browser/README.md "Data Contract" to `out_dir`.
#'
#' @param frame A frame of `FRAMES`
#' @param out_dir Folder to write to (created; files there are replaced)
#' @param types_file The study types (data/study_types.csv)
#' @param dbh_date Date of the DBH snapshot (courses and programmes); NULL for
#'   the latest
#' @param covered The institutions harvested (harvested_institutions())
#' @return The paths written, invisibly. Stops before writing anything when a
#'   text column holds personal data (the patterns of target privacy_check).
build_browser_data <- function(frame = "teacher_ed", out_dir = "app/browser/public/data",
                               types_file = "data/study_types.csv", dbh_date = NULL,
                               covered = harvested_institutions()) {
  stopifnot(frame %in% FRAMES)
  types <- utils::read.csv(types_file, encoding = "UTF-8")
  dbh_date <- dbh_date %||% dbh_snapshot_date(dbh_snapshot_file("courses"))
  programmes <- arrow::read_parquet(dbh_snapshot_file("programmes", dbh_date),
                                    col_select = c("Institusjonskode", "Studieprogramkode", "Årstall",
                                                   "Semester", "Studiumkode", "Studnavn"))
  uncovered <- uncovered_study_types(dbh_courses("all", dbh_date), programme_study_types(programmes, types),
                                     covered)
  rm(programmes)
  processed <- frame_dir(frame)
  plans_file <- file.path(processed, "course_plans.RDS")
  full <- readRDS(file.path(frame_dir(frame, "interim"), "course_offerings_full.RDS"))
  sources <- full[c("course_id", "url", "harvested_at")]
  rm(full)
  plans <- readRDS(plans_file)
  plans$course_plan_normalized <- NULL
  tables <- browser_tables(
    plans = plans, sections = readRDS(file.path(processed, "plan_sections.RDS")),
    offerings = readRDS(file.path(processed, "course_offerings.RDS")), sources = sources,
    types = types, gaps = readRDS(file.path(frame_dir("all", "interim"), "plan_gaps.RDS")),
    version = paste(frame, "processed", format(file.mtime(plans_file), "%Y-%m-%dT%H:%M")),
    frame = frame, uncovered = uncovered)
  write_browser_data(tables, out_dir)
}

#' Courses of each study type at the institutions not harvested
#'
#' So that a study type filter can say which institutions it leaves out (as
#' Lovisenberg for nursing, BI and Kristiania for law): every DBH course
#' (table 208) of an institution code that no harvested institution has,
#' under a programme of each study type (table 347) that semester. Former
#' institutions (merged into a harvested one under a code of their own) count
#' too, for the years they had courses.
#'
#' @param courses dbh_courses("all"): institution (NA without a config),
#'   institution_code, institution_name, course_code, year, semester,
#'   programme_code
#' @param study_types programme_study_types()
#' @param covered The institutions harvested (harvested_institutions())
#' @return List of `year`, the last year of `courses` (the current one), and
#'   `by_type`: one row per study type and institution code with a course of
#'   the type, `study_type`, `institution_code`, `institution_name` (its
#'   latest), `courses` (all years), `courses_current` (courses offered in
#'   `year`) and `year_last`, sorted by type, `courses_current` and `courses`
#'   (most first)
uncovered_study_types <- function(courses, study_types, covered) {
  year <- max(courses$year)
  x <- courses[is.na(courses$institution) | !courses$institution %in% covered,
               c("institution_code", "institution_name", "course_code", "year", "semester", "programme_code")]
  x <- add_study_types(x, study_types)
  x <- dplyr::rename(x[!is.na(x$study_type_code), ], study_type = study_type_code)
  names <- x |>
    dplyr::arrange(dplyr::desc(year), dplyr::desc(semester)) |>
    dplyr::distinct(institution_code, institution_name)
  names <- names[!duplicated(names$institution_code), ]
  by_type <- x |>
    dplyr::summarise(courses = dplyr::n_distinct(course_code),
                     courses_current = dplyr::n_distinct(course_code[year == .env$year]),
                     year_last = max(year),
                     .by = c(study_type, institution_code)) |>
    dplyr::left_join(names, by = "institution_code") |>
    dplyr::relocate(institution_name, .after = institution_code) |>
    dplyr::arrange(study_type, dplyr::desc(courses_current), dplyr::desc(courses), institution_code)
  list(year = year, by_type = by_type)
}

#' The browser's tables from the frame's data
#'
#' A course-year belongs to every programme DBH lists it under that year and
#' has every plan of its offerings that year, so `course_years` holds each
#' course-year x programme x plan once (DBH lists a course once per semester
#' and programme), with one NA-plan row per programme when the course-year has
#' no plan. `had_students` is TRUE when any of the course-year's offerings had
#' students. `plans`, `sections` and `course_years` carry `plan_id`, the
#' plan's key (plan_key_id()); it is made here for input built before
#' 2026-11-19, which does not have it.
#'
#' @param plans,sections,offerings The frame's course_plans, plan_sections and
#'   course_offerings (with `study_type_code` and `study_type_name`)
#' @param sources `course_id`, `url`, `harvested_at` of the offerings
#' @param types data/study_types.csv: study_type, name_no, name_en, group,
#'   group_label_en, group_label_no, te_frame. `meta$study_types` has every
#'   row, in its order, and `courses`, the number of courses with a course-year
#'   of the type (0 for a type not in the data); a type of the data missing from
#'   it is added at the end with DBH's name in group "other".
#' @param gaps plan_gaps(): institution, course_code, year, has_plan, reason
#' @param version,frame For meta.json
#' @param uncovered uncovered_study_types(), `meta$uncovered`; NULL for none
#' @return List of tibbles `courses`, `course_years`, `programmes`, `plans`,
#'   `sections` and the list `meta`
browser_tables <- function(plans, sections, offerings, sources, types, gaps, version, frame, uncovered = NULL) {
  with_plan_id <- function(x) {
    if (!"plan_id" %in% names(x)) x$plan_id <- plan_key_id(x$institution, x$course_code, x$plan_content_id)
    x
  }
  plans <- with_plan_id(plans)
  sections <- with_plan_id(sections)
  offerings <- with_plan_id(offerings)
  cy_key <- c("institution", "course_code", "year")
  # the study type of each programme-year (DBH's is per semester): the one of
  # most offerings
  study_types <- offerings |>
    dplyr::filter(!is.na(study_type_code)) |>
    dplyr::count(institution_code, programme_code, year, study_type = study_type_code) |>
    dplyr::arrange(dplyr::desc(n), study_type) |>
    dplyr::distinct(institution_code, programme_code, year, .keep_all = TRUE) |>
    dplyr::select(-n)

  course_years <- offerings |>
    dplyr::distinct(institution, institution_code, course_code, year, programme_code) |>
    dplyr::left_join(dplyr::filter(offerings, !is.na(plan_content_id)) |>
                       dplyr::distinct(institution, course_code, year, plan_id, plan_content_id),
                     by = cy_key, relationship = "many-to-many") |>
    dplyr::left_join(study_types, by = c("institution_code", "programme_code", "year")) |>
    dplyr::left_join(dplyr::summarise(offerings, had_students = any(had_students), .by = dplyr::all_of(cy_key)),
                     by = cy_key) |>
    dplyr::left_join(dplyr::filter(gaps, !has_plan) |> dplyr::select(dplyr::all_of(cy_key), reason),
                     by = cy_key) |>
    dplyr::mutate(no_plan_reason = dplyr::if_else(is.na(plan_content_id), reason, NA_character_)) |>
    dplyr::select(institution, course_code, year, programme_code, study_type, plan_id, plan_content_id,
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

  plan_key <- c("plan_id", "plan_content_id", "institution", "course_code")
  plan_table <- plans |>
    dplyr::left_join(dplyr::rename(sources, source_url = url), by = c(source_course_id = "course_id")) |>
    dplyr::select(dplyr::all_of(plan_key), year_from, year_to, source_url, harvested_at, course_plan) |>
    dplyr::arrange(institution, course_code, year_from, year_to, plan_content_id)
  # sections in the plans' order, each plan's in their own order
  section_table <- sections |>
    dplyr::mutate(.plan = match(plan_id, plan_table$plan_id),
                  .row = dplyr::row_number()) |>
    dplyr::filter(!is.na(.plan)) |>
    dplyr::arrange(.plan, .row) |>
    dplyr::select(dplyr::all_of(plan_key), section, text)

  institutions <- latest |>   # the latest name (Høgskolen i Innlandet became a university)
    dplyr::distinct(code = institution, .keep_all = TRUE) |>
    dplyr::select(code, institution_name, plan_year_basis) |>
    dplyr::arrange(code)
  type_names <- dplyr::distinct(offerings, code = study_type_code, name = study_type_name) |>
    dplyr::distinct(code, .keep_all = TRUE)
  type_courses <- dplyr::filter(course_years, !is.na(study_type)) |>
    dplyr::distinct(institution, course_code, code = study_type) |>
    dplyr::count(code, name = "courses")
  tree <- tibble::as_tibble(types) |>
    dplyr::transmute(code = study_type, name_no, name_en, group, group_label_en, group_label_no,
                     te_frame = as.logical(te_frame))
  missing <- dplyr::filter(type_courses, !code %in% tree$code) |>
    dplyr::left_join(type_names, by = "code") |>
    dplyr::transmute(code, name_no = dplyr::coalesce(name, code), name_en = name_no, group = "other",
                     group_label_en = "Other", group_label_no = "Annet", te_frame = FALSE)
  tree <- dplyr::bind_rows(tree, missing) |>
    dplyr::left_join(type_courses, by = "code") |>
    dplyr::mutate(courses = dplyr::coalesce(courses, 0L))
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
    study_types = tree,
    uncovered = uncovered %||% list(year = max(offerings$year), by_type = tibble::tibble(
      study_type = character(), institution_code = character(), institution_name = character(),
      courses = integer(), courses_current = integer(), year_last = integer())),
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
