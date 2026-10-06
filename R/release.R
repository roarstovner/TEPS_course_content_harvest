# R/release.R
# Hand a frozen release of the published data to another project (#294): the
# coding project (../TEPS_course_content_coding) draws its samples from it.

#' Copy the published data files to another project, with a manifest
#'
#' Writes to `dest`: course_offerings.RDS, course_plans.RDS, plan_sections.RDS
#' and methods.md as built; `manifest.yml` (release tag, harvest repo commit,
#' date, files with row counts and md5 sums); `section_coverage.csv` (share of
#' plans per institution with each section; where a section is missing the
#' coders use the whole plan); and, when `dest` already holds a release,
#' `plan_id_crosswalk.csv`: plan_content_id is a hash of the cleaned text, so a
#' cleaning change renames plans, and the crosswalk carries codes to the new
#' ids through the offerings (course_id) both releases share.
#'
#' Tag the commit first (`git tag data-YYYY-MM-DD`) so the manifest names it.
#'
#' @param tag Release name, e.g. "data-2026-10-09"; should be a git tag.
#' @param dest Directory to write to.
#' @return `dest`, invisibly.
release_data <- function(tag, dest = "../TEPS_course_content_coding/data") {
  files <- c("data/processed/course_offerings.RDS", "data/processed/course_plans.RDS",
             "data/processed/plan_sections.RDS", "methods.md")
  stopifnot(all(file.exists(files)), dir.exists(dest))
  commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  if (length(system2("git", c("status", "--porcelain", "--", "R", "_targets.R"), stdout = TRUE))) {
    warning("Uncommitted code changes: the manifest's commit does not describe the data")
  }
  if (!tag %in% system2("git", c("tag", "--points-at", "HEAD"), stdout = TRUE)) {
    warning("HEAD is not tagged ", tag)
  }

  off <- readRDS(files[1])
  plans <- readRDS(files[2])
  secs <- readRDS(files[3])

  # crosswalk from the release already in dest, before it is overwritten
  old_file <- file.path(dest, "course_offerings.RDS")
  if (file.exists(old_file)) {
    old_tag <- tryCatch(yaml::read_yaml(file.path(dest, "manifest.yml"))$release,
                        error = function(e) NA_character_)
    crosswalk <- readRDS(old_file) |>
      dplyr::filter(!is.na(plan_content_id)) |>
      dplyr::select(course_id, institution, Emnekode, old_plan_content_id = plan_content_id) |>
      dplyr::left_join(dplyr::select(off, course_id, new_plan_content_id = plan_content_id),
                       by = "course_id") |>
      dplyr::distinct(institution, Emnekode, old_plan_content_id, new_plan_content_id) |>
      dplyr::mutate(old_release = old_tag %||% NA_character_, new_release = tag)
    utils::write.csv(crosswalk, file.path(dest, "plan_id_crosswalk.csv"), row.names = FALSE)
  }

  file.copy(files, dest, overwrite = TRUE)

  key <- c("plan_content_id", "institution", "Emnekode")
  coverage <- secs |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "section")))) |>
    dplyr::count(institution, section, name = "n_plans_with_section") |>
    dplyr::left_join(dplyr::count(plans, institution, name = "n_plans"), by = "institution") |>
    dplyr::mutate(share = round(n_plans_with_section / n_plans, 3))
  utils::write.csv(coverage, file.path(dest, "section_coverage.csv"), row.names = FALSE)

  by_inst <- off |>
    dplyr::group_by(institution) |>
    dplyr::summarise(years = paste(range(Årstall), collapse = "-"), offerings = dplyr::n(),
                     offerings_with_plan = sum(!is.na(plan_content_id)), .groups = "drop") |>
    dplyr::left_join(dplyr::count(plans, institution, name = "plans"), by = "institution")
  manifest <- list(
    release = tag,
    harvest_repo_commit = commit,
    created = format(Sys.time(), "%Y-%m-%d %H:%M"),
    methods = "methods.md (the choices behind the data, with numbers for this release)",
    files = lapply(stats::setNames(basename(files), basename(files)), function(f) {
      path <- file.path(dest, f)
      list(md5 = unname(tools::md5sum(path)),
           rows = if (grepl("\\.RDS$", f)) nrow(readRDS(path)) else NULL)
    }),
    institutions = lapply(split(by_inst[-1], by_inst$institution), as.list)
  )
  yaml::write_yaml(manifest, file.path(dest, "manifest.yml"))
  message("Release ", tag, " written to ", dest)
  invisible(dest)
}
