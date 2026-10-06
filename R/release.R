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
      c(list(md5 = unname(tools::md5sum(path))),
        if (grepl("\\.RDS$", f)) list(rows = nrow(readRDS(path))))
    }),
    institutions = lapply(split(by_inst[-1], by_inst$institution), as.list)
  )
  yaml::write_yaml(manifest, file.path(dest, "manifest.yml"))
  message("Release ", tag, " written to ", dest)
  invisible(dest)
}

# --- Finalizing (#296) -----------------------------------------------------------

RAW_MANIFEST <- "tests/snapshots/raw_manifest.csv"

#' Finalize a release: lock its raw files and keep its plans
#'
#' Makes every raw file (harvest_files() of all institutions) read-only, so a
#' later write fails loudly; lists them with md5 sums in RAW_MANIFEST (commit
#' it); and keeps the release's offerings, plans and sections in
#' data/releases/{tag}/. frozen_changes() then checks on every build that
#' nothing of the release has changed. A later harvest only adds raw files
#' (harvest_all()), so a new DBH year leaves the release as it was.
#'
#' @param tag Release name, the git tag of the release.
#' @param raw_dir,processed,releases,manifest Locations (defaults: the repo's).
#' @return The manifest, invisibly.
finalize_release <- function(tag, raw_dir = RAW_DIR, processed = "data/processed",
                             releases = "data/releases", manifest = RAW_MANIFEST) {
  if (file.exists("_targets.R") && length(targets::tar_outdated())) {
    stop("Run targets::tar_make() first: the built data are not up to date")
  }
  files <- unlist(lapply(harvested_institutions(raw_dir), harvest_files, raw_dir = raw_dir))
  m <- tibble::tibble(release = tag, file = files, md5 = unname(tools::md5sum(files)))
  dir <- file.path(releases, tag)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  kept <- file.path(dir, c("course_offerings.RDS", "course_plans.RDS", "plan_sections.RDS"))
  file.copy(file.path(processed, basename(kept)), kept)
  Sys.chmod(c(files, kept), "0444")
  utils::write.csv(m, manifest, row.names = FALSE)
  message(length(files), " raw files locked for ", tag, "; commit ", manifest)
  invisible(m)
}

#' Changes to the finalized release
#'
#' Compares with RAW_MANIFEST and the data kept by finalize_release(): a raw
#' file changed or missing, an offering of the release gone or with another
#' plan, a plan of the release gone or with other text, a section of it
#' changed. No rows before anything is finalized.
#'
#' @param manifest The raw manifest file.
#' @param offerings,plans,sections The built course_offerings, course_plans and
#'   plan_sections files.
#' @param releases Where finalize_release() keeps the plans.
#' @return Tibble `check`, `institution`, `what`.
frozen_changes <- function(manifest, offerings, plans, sections, releases = "data/releases") {
  out <- tibble::tibble(check = character(), institution = character(), what = character())
  if (!file.exists(manifest)) return(out)
  m <- utils::read.csv(manifest)
  md5 <- unname(tools::md5sum(m$file))
  bad <- is.na(md5) | md5 != m$md5
  out <- dplyr::bind_rows(out, tibble::tibble(check = "raw file changed or missing",
                                              institution = NA_character_, what = m$file[bad]))
  dir <- file.path(releases, m$release[1])
  o <- dplyr::left_join(readRDS(file.path(dir, "course_offerings.RDS"))[c("course_id", "institution", "plan_content_id")],
                        readRDS(offerings)[c("course_id", "plan_content_id")],
                        by = "course_id", suffix = c("", "_now"))
  now_ids <- readRDS(offerings)$course_id
  moved <- !o$course_id %in% now_ids | !mapply(identical, o$plan_content_id, o$plan_content_id_now)
  out <- dplyr::bind_rows(out, tibble::tibble(check = "offering gone or with another plan",
                                              institution = o$institution[moved],
                                              what = o$course_id[moved]))
  key <- c("plan_content_id", "institution", "Emnekode")
  was <- readRDS(file.path(dir, "course_plans.RDS"))[c(key, "course_plan")]
  now <- readRDS(plans)[c(key, "course_plan")]
  p <- dplyr::left_join(was, now, by = key, suffix = c("", "_now"))
  diff <- is.na(p$course_plan_now) | p$course_plan_now != p$course_plan
  out <- dplyr::bind_rows(out, tibble::tibble(check = "plan gone or text changed",
                                              institution = p$institution[diff],
                                              what = paste(p$Emnekode[diff], p$plan_content_id[diff])))
  cols <- c(key, "section", "text")
  was_s <- readRDS(file.path(dir, "plan_sections.RDS"))[cols]
  now_s <- dplyr::semi_join(readRDS(sections)[cols], was, by = key)
  s <- dplyr::bind_rows(dplyr::anti_join(was_s, now_s, by = cols),
                        dplyr::anti_join(now_s, was_s, by = cols)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "section"))))
  out <- dplyr::bind_rows(out, tibble::tibble(check = "section changed",
                                              institution = s$institution,
                                              what = paste(s$Emnekode, s$plan_content_id, s$section)))
  if (nrow(out) > 0) {
    warning(nrow(out), " change(s) to the finalized release ", m$release[1],
            ": see tar_read(frozen_check)", call. = FALSE)
  }
  out
}
