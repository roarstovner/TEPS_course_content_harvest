# R/release.R
# Hand the published data to another project: the coding project
# (../TEPS_course_content_coding) develops and checks its code on it, and
# draws its samples from it. A development hand-over (dev-YYYY-MM-DD) is
# release_data() alone; a frozen release is finalize_release() too. No
# release is finalized yet: the first comes when coding starts (the releases
# of 2026-10-15 and 2026-10-16 were retired on 2026-11-04).

# Every variable of the data files: its file, type, source and meaning, and
# its name until 2026-10-16 (DBH's; docs/data-files.md "Data
# Structure").
VARIABLES_FILE <- "data/variables.csv"
# DBH's study types with the project's groups (study_type_group of
# course_offerings; docs/data-files.md "Study Types of the Offerings")
STUDY_TYPES_FILE <- "data/study_types.csv"
# The value labels of the coded columns (docs/codebook.md); with
# STUDY_TYPES_FILE, release_codes()
CODES_FILE <- "data/codes.csv"
# The human codebook, rendered from VARIABLES_FILE and release_codes()
CODEBOOK_FILE <- "docs/codebook.md"

# The keys of the published files (docs/data-files.md "Keys of the Published
# Files"): plan_id is unique per plan; plan_content_id is a text
# fingerprint, not a key
RELEASE_KEYS <- list(
  course_offerings = list(primaryKey = "course_id",
                          foreignKeys = list(list(fields = "plan_id",
                                                  reference = list(resource = "course_plans", fields = "plan_id")))),
  course_plans = list(primaryKey = "plan_id"),
  plan_sections = list(primaryKey = c("plan_id", "section"),
                       foreignKeys = list(list(fields = "plan_id",
                                               reference = list(resource = "course_plans", fields = "plan_id"))))
)

# Table Schema types of the types in VARIABLES_FILE, and the R type a
# column of each has
TABLE_SCHEMA_TYPES <- c(character = "string", integer = "integer", double = "number",
                        logical = "boolean", Date = "date")

#' Copy the published data files to another project, with a manifest
#'
#' Writes to `dest`: course_offerings.parquet, course_plans.parquet and
#' plan_sections.parquet of the frame (the teacher-ed frame by default: the
#' coding project's) with `datapackage.json`, their Data Package (v2)
#' descriptor (write_datapackage(); it stops if the tables do not match it);
#' methods.md as built, variables.csv (VARIABLES_FILE: every variable of
#' the files, with its name until 2026-10-16) and study_types.csv
#' (STUDY_TYPES_FILE: the project's groups of study types, study_type_group);
#' codes.csv (CODES_FILE: the value labels of the coded columns) and
#' codebook.md (CODEBOOK_FILE, rendered from the variables and codes);
#' `manifest.yml` (release tag, whether it is finalized, harvest repo commit,
#' date, files with row counts and md5 sums); `section_coverage.csv` (share of
#' plans per institution with each section; where a section is missing the
#' coders use the whole plan); and, when `dest` already holds a release,
#' `plan_id_crosswalk.csv`: plan_content_id is a hash of the cleaned text, so a
#' cleaning change renames plans, and the crosswalk carries codes to the new
#' ids through the offerings (course_id) both releases share; and
#' `sections_changed.csv`: a section fix keeps the plan's id, so the crosswalk
#' does not show it, yet units coded on the old section text no longer match
#' (see section_changes()). The release in `dest` is read from Parquet, or
#' from RDS as written until 2026-11-26 (read_release_table()); one with the
#' old names (the data handed to the coding project on 2026-10-15) is read
#' with the new ones (dbh_rename()).
#'
#' A release to be finalized: tag the commit first (`git tag
#' data-YYYY-MM-DD`) so the manifest names it, then finalize_release(). A
#' development hand-over (`dev-YYYY-MM-DD`: data to build and check code on,
#' not frozen; `finalized: false` in the manifest) needs no tag: the manifest
#' names the commit.
#'
#' @param tag Release name, e.g. "data-2026-10-09" (a git tag) or
#'   "dev-2026-11-26".
#' @param dest Directory to write to.
#' @param frame The frame whose files are released (FRAMES).
#' @return `dest`, invisibly.
release_data <- function(tag, dest = "../TEPS_course_content_coding/data", frame = "teacher_ed") {
  data_files <- file.path(frame_dir(frame), paste0(names(RELEASE_KEYS), ".RDS"))
  files <- c("methods.md", VARIABLES_FILE, STUDY_TYPES_FILE, CODES_FILE, CODEBOOK_FILE)
  stopifnot(all(file.exists(c(data_files, files))), dir.exists(dest))
  commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  if (length(system2("git", c("status", "--porcelain", "--", "R", "_targets.R"), stdout = TRUE))) {
    warning("Uncommitted code changes: the manifest's commit does not describe the data")
  }
  if (!startsWith(tag, "dev-") && !tag %in% system2("git", c("tag", "--points-at", "HEAD"), stdout = TRUE)) {
    warning("HEAD is not tagged ", tag)
  }

  off <- readRDS(data_files[1])
  plans <- readRDS(data_files[2])
  secs <- readRDS(data_files[3])

  # crosswalk from the release already in dest, before it is overwritten
  old_off <- read_release_table(dest, "course_offerings")
  if (!is.null(old_off)) {
    old_tag <- tryCatch(yaml::read_yaml(file.path(dest, "manifest.yml"))$release,
                        error = function(e) NA_character_)
    crosswalk <- old_off |>
      dplyr::filter(!is.na(plan_content_id)) |>
      dplyr::select(course_id, institution, course_code, old_plan_content_id = plan_content_id) |>
      dplyr::left_join(dplyr::select(off, course_id, new_plan_content_id = plan_content_id),
                       by = "course_id") |>
      dplyr::distinct(institution, course_code, old_plan_content_id, new_plan_content_id) |>
      dplyr::mutate(old_release = old_tag %||% NA_character_, new_release = tag)
    utils::write.csv(crosswalk, file.path(dest, "plan_id_crosswalk.csv"), row.names = FALSE)
    changed <- section_changes(read_release_table(dest, "plan_sections"), secs,
                               read_release_table(dest, "course_plans"), plans) |>
      dplyr::mutate(old_release = old_tag %||% NA_character_, new_release = tag)
    utils::write.csv(changed, file.path(dest, "sections_changed.csv"), row.names = FALSE)
  }

  # Parquet and the descriptor first: it stops before any file is copied if
  # the tables do not match the descriptor
  tables <- list(course_offerings = off, course_plans = plans, plan_sections = secs)
  parquet <- write_datapackage(tables, dest, tag, frame)
  rows <- vapply(tables, nrow, 1L)
  file.copy(files, dest, overwrite = TRUE)
  old_rds <- file.path(dest, paste0(names(RELEASE_KEYS), ".RDS"))
  if (any(file.exists(old_rds))) {
    warning("dest still holds ", toString(basename(old_rds[file.exists(old_rds)])),
            " of an earlier release (RDS until 2026-11-26): not this release")
  }

  key <- c("plan_content_id", "institution", "course_code")
  coverage <- secs |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "section")))) |>
    dplyr::count(institution, section, name = "n_plans_with_section") |>
    dplyr::left_join(dplyr::count(plans, institution, name = "n_plans"), by = "institution") |>
    dplyr::mutate(share = round(n_plans_with_section / n_plans, 3))
  utils::write.csv(coverage, file.path(dest, "section_coverage.csv"), row.names = FALSE)

  by_inst <- off |>
    dplyr::group_by(institution) |>
    dplyr::summarise(years = paste(range(year), collapse = "-"), offerings = dplyr::n(),
                     offerings_with_plan = sum(!is.na(plan_content_id)), .groups = "drop") |>
    dplyr::left_join(dplyr::count(plans, institution, name = "plans"), by = "institution")
  manifest <- list(
    release = tag,
    # finalize_release() keeps a finalized release's files in data/releases/
    finalized = dir.exists(file.path("data/releases", tag)),
    harvest_repo_commit = commit,
    created = format(Sys.time(), "%Y-%m-%d %H:%M"),
    methods = "methods.md (the choices behind the data, with numbers for this release)",
    variables = "variables.csv (every variable of the files, and its name until 2026-10-16)",
    study_types = "study_types.csv (DBH's study types and the project's groups of them, study_type_group)",
    codes = "codes.csv (the value labels of the coded columns; the study types are in study_types.csv)",
    codebook = "codebook.md (the variables, codes and keys of the files)",
    datapackage = "datapackage.json (Data Package v2 descriptor of the Parquet files: types, codes, keys)",
    files = lapply(stats::setNames(nm = c(basename(files), basename(parquet))), function(f) {
      table <- tools::file_path_sans_ext(f)
      c(list(md5 = unname(tools::md5sum(file.path(dest, f)))),
        if (table %in% names(rows)) list(rows = rows[[table]]))
    }),
    institutions = lapply(split(by_inst[-1], by_inst$institution), as.list)
  )
  # true/false, not yaml 1.1's yes/no, which YAML 1.2 readers take as strings
  yaml::write_yaml(manifest, file.path(dest, "manifest.yml"), handlers = list(logical = yaml::verbatim_logical))
  message("Release ", tag, " written to ", dest)
  invisible(dest)
}

#' A table of the release in a directory, from Parquet or RDS
#'
#' @param dir The release's directory.
#' @param name The table (names of RELEASE_KEYS).
#' @return The table with the current names (dbh_rename()), or NULL when
#'   `dir` holds neither `{name}.parquet` nor `{name}.RDS` (as written until
#'   2026-11-26).
read_release_table <- function(dir, name) {
  f <- file.path(dir, paste0(name, c(".parquet", ".RDS")))
  if (file.exists(f[1])) return(dbh_rename(tibble::as_tibble(arrow::read_parquet(f[1]))))
  if (file.exists(f[2])) dbh_rename(readRDS(f[2]))
}

#' Value labels of the coded columns of the published files
#'
#' CODES_FILE (one row per allowed value of a variable, in every file that has
#' it) and, from STUDY_TYPES_FILE, the study types (study_type_code, DBH's,
#' with the project's English name) and their groups (study_type_group, the
#' project's), so the study types are kept in one file.
#'
#' @param codes_file,study_types_file The two tables.
#' @return Data frame `variable`, `code` (character), `label`, `source` (DBH
#'   or project), `description`.
release_codes <- function(codes_file = CODES_FILE, study_types_file = STUDY_TYPES_FILE) {
  codes <- utils::read.csv(codes_file, colClasses = "character", na.strings = NULL)
  st <- utils::read.csv(study_types_file, colClasses = "character")
  groups <- st[!duplicated(st$group), ]
  rbind(codes,
        data.frame(variable = "study_type_code", code = st$study_type, label = st$name_en,
                   source = "DBH", description = paste0("DBH: ", st$name_no, "; group ", st$group)),
        data.frame(variable = "study_type_group", code = groups$group, label = groups$group_label_en,
                   source = "project", description = groups$group_label_no))
}

#' The Data Package (v2) descriptor of a release's tables
#'
#' One resource per published file, `{file}.parquet`, with a Table Schema:
#' the fields of VARIABLES_FILE (type, description, source), an enum
#' constraint and labelled categories for each coded variable
#' (release_codes()), and the keys of RELEASE_KEYS.
#'
#' @param tag Release name (the package version).
#' @param frame The frame of the files (FRAMES).
#' @param variables,codes VARIABLES_FILE and release_codes().
#' @param files Optional named vector of the written Parquet files, by
#'   resource, to add their bytes and md5 hash.
#' @return The descriptor, a list for jsonlite::write_json(auto_unbox = TRUE).
datapackage_descriptor <- function(tag, frame, variables = utils::read.csv(VARIABLES_FILE),
                                   codes = release_codes(), files = NULL) {
  resource <- function(name) {
    v <- variables[variables$file == name, ]
    fields <- lapply(seq_len(nrow(v)), function(i) {
      type <- unname(TABLE_SCHEMA_TYPES[v$type[i]])
      f <- list(name = v$variable[i], type = type, description = v$description[i], source = v$source[i])
      cd <- codes[codes$variable == v$variable[i], ]
      if (nrow(cd)) {
        value <- if (type == "integer") as.integer(cd$code) else cd$code
        f$constraints <- list(enum = I(value))
        f$categories <- lapply(seq_along(value), function(j) list(value = value[j], label = cd$label[j]))
      }
      f
    })
    keys <- RELEASE_KEYS[[name]]
    r <- list(name = name, path = paste0(name, ".parquet"), format = "parquet",
              mediatype = "application/vnd.apache.parquet")
    if (!is.null(files)) {
      r$bytes <- unname(file.size(files[[name]]))
      r$hash <- unname(tools::md5sum(files[[name]]))
    }
    r$schema <- c(list(fields = fields, primaryKey = I(keys$primaryKey)),
                  if (length(keys$foreignKeys)) list(foreignKeys = lapply(keys$foreignKeys, function(k)
                    list(fields = I(k$fields), reference = list(resource = k$reference$resource,
                                                                fields = I(k$reference$fields))))))
    r
  }
  list(`$schema` = "https://datapackage.org/profiles/2.0/datapackage.json",
       name = paste0("teps-course-plans-", gsub("_", "-", frame)),
       title = paste0("Norwegian course plans (", frame, " frame)"),
       description = paste("Course offerings from DBH, their course plans (anonymized) and the plans'",
                           "sections; codebook.md describes the variables and codes."),
       version = tag,
       created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       resources = lapply(names(RELEASE_KEYS), resource))
}

#' Write a release's tables as Parquet with their datapackage.json
#'
#' @param tables Named list of the published tables (names of RELEASE_KEYS).
#' @param dest Directory to write to.
#' @inheritParams datapackage_descriptor
#' @return The paths written, invisibly. Stops if the tables do not match
#'   the descriptor (check_datapackage()).
write_datapackage <- function(tables, dest, tag, frame, variables = utils::read.csv(VARIABLES_FILE),
                              codes = release_codes()) {
  desc <- datapackage_descriptor(tag, frame, variables, codes)
  problems <- check_datapackage(desc, tables)
  if (length(problems)) stop("The tables do not match the descriptor:\n", paste(problems, collapse = "\n"))
  files <- stats::setNames(file.path(dest, paste0(names(RELEASE_KEYS), ".parquet")), names(RELEASE_KEYS))
  for (n in names(files)) arrow::write_parquet(tables[[n]], files[[n]])
  desc <- datapackage_descriptor(tag, frame, variables, codes, files)
  json <- file.path(dest, "datapackage.json")
  jsonlite::write_json(desc, json, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(c(files, datapackage = json))
}

#' Check tables against a Data Package descriptor
#'
#' Fields (names and order), types, enum constraints, primary keys (unique, no
#' NA) and foreign keys (every non-NA value in the referenced table).
#'
#' @param desc The descriptor (datapackage_descriptor(), or
#'   jsonlite::read_json() of a datapackage.json).
#' @param tables Named list of data frames, by resource name.
#' @return Character vector of problems; empty when the tables match.
check_datapackage <- function(desc, tables) {
  r_type <- function(col) if (inherits(col, "Date")) "Date" else typeof(col)
  key_of <- function(x, cols) do.call(paste, c(unname(as.list(x[unlist(cols)])), sep = "\r"))
  out <- character()
  for (r in desc$resources) {
    x <- tables[[r$name]]
    if (is.null(x)) { out <- c(out, paste(r$name, "missing")); next }
    fields <- r$schema$fields
    names_f <- vapply(fields, `[[`, "", "name")
    if (!identical(names(x), names_f)) {
      out <- c(out, sprintf("%s: columns %s, descriptor %s", r$name, toString(names(x)), toString(names_f)))
      next
    }
    for (f in fields) {
      col <- x[[f$name]]
      want <- names(TABLE_SCHEMA_TYPES)[TABLE_SCHEMA_TYPES == f$type]
      if (r_type(col) != want) out <- c(out, sprintf("%s.%s: %s, not %s", r$name, f$name, r_type(col), f$type))
      enum <- unlist(f$constraints$enum)
      bad <- setdiff(unique(col[!is.na(col)]), enum)
      if (length(enum) && length(bad)) {
        out <- c(out, sprintf("%s.%s: values not in the codes: %s", r$name, f$name, toString(utils::head(bad, 10))))
      }
    }
    pk <- unlist(r$schema$primaryKey)
    if (anyNA(x[pk]) || anyDuplicated(key_of(x, pk))) {
      out <- c(out, sprintf("%s: primary key (%s) not unique or NA", r$name, toString(pk)))
    }
    for (fk in r$schema$foreignKeys) {
      ref <- tables[[fk$reference$resource]]
      if (is.null(ref)) next
      v <- x[stats::complete.cases(x[unlist(fk$fields)]), , drop = FALSE]
      miss <- !key_of(v, fk$fields) %in% key_of(ref, fk$reference$fields)
      if (any(miss)) {
        out <- c(out, sprintf("%s: %d row(s) with %s not in %s", r$name, sum(miss),
                              toString(unlist(fk$fields)), fk$reference$resource))
      }
    }
  }
  out
}

#' Sections that differ between two releases, for the plans both hold
#'
#' @param was_secs,now_secs plan_sections of the old and the new release.
#' @param was_plans,now_plans course_plans of the old and the new release.
#' @return Tibble with the plan key, `section` and `change` ("changed",
#'   "added", "removed"), for plans with the same id, institution and code in
#'   both releases.
section_changes <- function(was_secs, now_secs, was_plans, now_plans) {
  key <- c("plan_content_id", "institution", "course_code")
  both <- dplyr::semi_join(was_plans[key], now_plans, by = key)
  dplyr::full_join(dplyr::semi_join(was_secs, both, by = key)[c(key, "section", "text")],
                   dplyr::semi_join(now_secs, both, by = key)[c(key, "section", "text")],
                   by = c(key, "section"), suffix = c("_was", "_now")) |>
    dplyr::filter(!mapply(identical, text_was, text_now)) |>
    dplyr::transmute(dplyr::across(dplyr::all_of(c(key, "section"))),
                     change = dplyr::case_when(is.na(text_was) ~ "added",
                                               is.na(text_now) ~ "removed",
                                               .default = "changed"))
}

# --- Finalizing -----------------------------------------------------------

RAW_MANIFEST <- "tests/snapshots/raw_manifest.csv"

#' Finalize a release: lock its raw files and keep its plans
#'
#' Runs targets::tar_make() first (a no-op when the build is current). Makes
#' every raw file (harvest_files() and the fetch ledgers, ledger_files(), of
#' all institutions) read-only, so a
#' later write fails loudly; lists them with md5 sums in RAW_MANIFEST (commit
#' it); and keeps the release's offerings, plans and sections in
#' data/releases/{tag}/: both frames, so a release cuts nothing (users filter
#' by frame or study type), the teacher-ed frame's in the folder itself, all
#' courses' in all/, with STUDY_TYPES_FILE, the definition of
#' study_type_group. frozen_changes() then checks on every build that nothing
#' of the teacher-ed frame's files has changed. A later harvest only adds raw
#' files (harvest_all()), so a new DBH year leaves the release as it was.
#'
#' @param tag Release name, the git tag of the release.
#' @param raw_dir,processed,processed_all,releases,manifest Locations
#'   (defaults: the repo's); `processed` the teacher-ed frame's files,
#'   `processed_all` all courses'.
#' @return The manifest, invisibly.
finalize_release <- function(tag, raw_dir = RAW_DIR, processed = frame_dir("teacher_ed"),
                             processed_all = frame_dir("all"), releases = "data/releases",
                             manifest = RAW_MANIFEST) {
  # build first, so the release is what the code makes; tar_outdated() cannot
  # tell: html_file runs on every build, so all after it always count as outdated
  if (file.exists("_targets.R")) targets::tar_make()
  files <- unlist(lapply(harvested_institutions(raw_dir), function(i)
    c(harvest_files(i, raw_dir), ledger_files(i, raw_dir))))
  m <- tibble::tibble(release = tag, file = files, md5 = unname(tools::md5sum(files)))
  dir <- file.path(releases, tag)
  data_files <- c("course_offerings.RDS", "course_plans.RDS", "plan_sections.RDS")
  from <- c(file.path(processed, data_files), file.path(processed_all, data_files), STUDY_TYPES_FILE)
  kept <- c(file.path(dir, data_files), file.path(dir, "all", data_files),
            file.path(dir, basename(STUDY_TYPES_FILE)))
  stopifnot(all(file.exists(from)))
  dir.create(file.path(dir, "all"), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, kept)
  Sys.chmod(c(files, kept), "0444")
  utils::write.csv(m, manifest, row.names = FALSE)
  message(length(files), " raw files locked for ", tag, "; commit ", manifest)
  invisible(m)
}

#' Changes to the raw store and to the finalized release
#'
#' Checks that every raw file listed in RAW_MANIFEST is unchanged (check "raw
#' file changed or missing"): the guard of the append-only raw store. When the
#' release named in the manifest is kept in `releases` (finalize_release()),
#' also lists an offering of it gone or with another plan, a plan of it gone or
#' with other text and a section of it changed. No release is finalized yet
#' (the releases of 2026-10-15 and 2026-10-16 were retired on 2026-11-04; the
#' first comes when coding starts), so until then only the raw files are
#' checked. No rows without a manifest.
#'
#' @param manifest The raw manifest file.
#' @param offerings,plans,sections The built course_offerings, course_plans and
#'   plan_sections files of the release's frame.
#' @param releases Where finalize_release() keeps the plans.
#' @return Tibble `check`, `institution`, `what`.
frozen_changes <- function(manifest, offerings, plans, sections, releases = "data/releases") {
  row <- function(check, institution, what) tibble::tibble(check = check, institution = institution, what = what)
  out <- row(character(), character(), character())
  if (!file.exists(manifest)) return(out)
  m <- utils::read.csv(manifest)
  md5 <- unname(tools::md5sum(m$file))
  bad <- is.na(md5) | md5 != m$md5
  out <- row("raw file changed or missing", NA_character_, m$file[bad])
  dir <- file.path(releases, m$release[1])
  if (dir.exists(dir)) {
    o <- dplyr::left_join(readRDS(file.path(dir, "course_offerings.RDS"))[c("course_id", "institution", "plan_content_id")],
                          readRDS(offerings)[c("course_id", "plan_content_id")],
                          by = "course_id", suffix = c("", "_now"))
    moved <- !o$course_id %in% readRDS(offerings)$course_id |
      !mapply(identical, o$plan_content_id, o$plan_content_id_now)
    key <- c("plan_content_id", "institution", "course_code")
    was <- readRDS(file.path(dir, "course_plans.RDS"))[c(key, "course_plan")]
    p <- dplyr::left_join(was, readRDS(plans)[c(key, "course_plan")], by = key, suffix = c("", "_now"))
    diff <- is.na(p$course_plan_now) | p$course_plan_now != p$course_plan
    cols <- c(key, "section", "text")
    was_s <- readRDS(file.path(dir, "plan_sections.RDS"))[cols]
    now_s <- dplyr::semi_join(readRDS(sections)[cols], was, by = key)
    s <- dplyr::bind_rows(dplyr::anti_join(was_s, now_s, by = cols),
                          dplyr::anti_join(now_s, was_s, by = cols)) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "section"))))
    out <- dplyr::bind_rows(
      out, row("offering gone or with another plan", o$institution[moved], o$course_id[moved]),
      row("plan gone or text changed", p$institution[diff], paste(p$course_code[diff], p$plan_content_id[diff])),
      row("section changed", s$institution, paste(s$course_code, s$plan_content_id, s$section)))
  }
  if (nrow(out) > 0) {
    warning(nrow(out), " change(s) to the raw store",
            if (dir.exists(dir)) paste(" or the finalized release", m$release[1]),
            ": see tar_read(frozen_check)", call. = FALSE)
  }
  out
}
