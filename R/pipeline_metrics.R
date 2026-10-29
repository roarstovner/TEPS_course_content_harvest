# R/pipeline_metrics.R
# Regression snapshot for the built data.
#
# pipeline_metrics() counts offerings, plans and sections per institution in
# the built files; check_pipeline_metrics() compares them with the snapshot of
# each frame in tests/snapshots/pipeline_metrics_{frame}.csv and prints every
# change beyond tolerance. A rebuild that silently empties an institution (all
# 142 nla courses lost their sections in 06db3ac) then shows up at once, and
# the test in tests/testthat/test-pipeline-metrics.R fails. The teacher-ed
# snapshot is stable; the all-courses one changes with each harvest. After an
# intended change, update the snapshot of the frames it changed and commit it
# with the change that explains it:
#
#   Rscript -e 'source("R/utils.R"); source("R/pipeline_metrics.R");
#               check_pipeline_metrics(update = TRUE, frames = "all")'

METRICS_SNAPSHOT <- c(all = "tests/snapshots/pipeline_metrics_all.csv",
                      teacher_ed = "tests/snapshots/pipeline_metrics_teacher_ed.csv")

#' Per-institution metrics of the built data
#'
#' @param offerings course_offerings_full: one row per offering with
#'   `extracted_text`, `course_plan` and `plan_content_id`.
#' @param sections plan_sections (`plan_content_id`, `institution`,
#'   `course_code`, `source_course_id`, `section`, `text`), or NULL to skip the
#'   section metrics.
#' @return Long tibble `institution`, `section` ("(all)" for offering-level
#'   metrics), `metric`, `value`. Metrics named n_* are counts (a plan is a
#'   plan_content_id + course_code); median_chars is the median length of
#'   `course_plan` or of the section text; pct_text_in_sections is the median
#'   share (%) of a plan's text that its sections hold.
pipeline_metrics <- function(offerings, sections = NULL) {
  has <- function(x) !is.na(x) & nzchar(x)
  course_years <- offerings |>
    dplyr::group_by(institution, course_code_raw, year) |>
    dplyr::summarise(plan = any(has(course_plan)), .groups = "drop") |>
    dplyr::count(institution, wt = plan, name = "n_course_years_with_plan")
  if (is.null(sections)) {
    sections <- tibble::tibble(plan_content_id = character(), institution = character(),
                               course_code = character(), source_course_id = character(),
                               section = character(), text = character())
  }
  in_sections <- sections |>
    dplyr::group_by(institution, source_course_id) |>
    dplyr::summarise(chars = sum(nchar(text)), .groups = "drop") |>
    dplyr::inner_join(dplyr::select(offerings, source_course_id = course_id, course_plan),
                      by = "source_course_id") |>
    dplyr::group_by(institution) |>
    dplyr::summarise(pct_text_in_sections = round(100 * stats::median(
      pmin(chars / nchar(course_plan), 1))), .groups = "drop")
  with_sections <- sections |>
    dplyr::distinct(institution, plan_content_id, course_code) |>
    dplyr::count(institution, name = "n_plans_with_sections")

  per_institution <- offerings |>
    dplyr::group_by(institution) |>
    dplyr::summarise(
      n_rows         = dplyr::n(),
      n_with_text    = sum(has(extracted_text)),
      n_with_plan    = sum(has(course_plan)),
      n_unique_plans = dplyr::n_distinct(plan_content_id, na.rm = TRUE),
      median_chars   = stats::median(nchar(course_plan[has(course_plan)])),
      .groups = "drop"
    ) |>
    dplyr::left_join(course_years, by = "institution") |>
    dplyr::left_join(with_sections, by = "institution") |>
    dplyr::left_join(in_sections, by = "institution") |>
    tidyr::pivot_longer(-institution, names_to = "metric", values_to = "value") |>
    dplyr::mutate(section = "(all)")

  per_section <- sections |>
    dplyr::group_by(institution, section) |>
    dplyr::summarise(n_plans = dplyr::n_distinct(plan_content_id, course_code),
                     median_chars = stats::median(nchar(text)),
                     .groups = "drop") |>
    tidyr::pivot_longer(c(n_plans, median_chars),
                        names_to = "metric", values_to = "value")

  dplyr::bind_rows(per_institution, per_section) |>
    dplyr::mutate(value = dplyr::coalesce(as.numeric(value), 0)) |>
    dplyr::select(institution, section, metric, value) |>
    dplyr::arrange(institution, section, metric)
}

#' Metrics that changed beyond tolerance between two metric tables
#'
#' A count (n_*) is flagged when it changes by more than `tol_n` (relative)
#' and `min_n` (absolute), median_chars by more than `tol_chars` and
#' `min_chars`. A metric that appears, disappears, or goes to or from zero is
#' always flagged.
compare_metrics <- function(old, new, tol_n = 0.05, min_n = 10,
                            tol_chars = 0.10, min_chars = 50) {
  dplyr::full_join(old, new, by = c("institution", "section", "metric"),
                   suffix = c("_old", "_new")) |>
    dplyr::mutate(
      old = dplyr::coalesce(value_old, 0),
      new = dplyr::coalesce(value_new, 0),
      diff = new - old,
      chars = metric == "median_chars",
      big = abs(diff) > ifelse(chars, min_chars, min_n) &
        abs(diff) > ifelse(chars, tol_chars, tol_n) * old,
      zero = (old == 0) != (new == 0)
    ) |>
    dplyr::filter(big | zero) |>
    dplyr::select(institution, section, metric, old, new, diff) |>
    dplyr::arrange(institution, section, metric)
}

#' Compare the built data with the snapshot, or update the snapshot
#'
#' @param update If TRUE, write the current metrics as the new snapshot.
#' @param frames The frames (FRAMES) to compare or update.
#' @param root The repository's folder.
#' @return The changes beyond tolerance, with their `frame` (invisibly; empty
#'   after an update).
check_pipeline_metrics <- function(update = FALSE, frames = names(METRICS_SNAPSHOT), root = ".") {
  changes <- dplyr::bind_rows(lapply(frames, function(f) {
    snapshot <- file.path(root, METRICS_SNAPSHOT[[f]])
    current <- pipeline_metrics(
      readRDS(file.path(root, frame_dir(f, "interim"), "course_offerings_full.RDS")),
      readRDS(file.path(root, frame_dir(f), "plan_sections.RDS")))
    if (update || !file.exists(snapshot)) {
      utils::write.csv(current, snapshot, row.names = FALSE)
      message("Wrote ", nrow(current), " metrics to ", snapshot)
      return(NULL)
    }
    changes <- compare_metrics(utils::read.csv(snapshot), current)
    if (nrow(changes) == 0) {
      message("Pipeline metrics match ", snapshot)
    } else {
      message(nrow(changes), " metric(s) differ from ", snapshot, " beyond tolerance:")
      print(as.data.frame(changes), row.names = FALSE)
      message("If intended: check_pipeline_metrics(update = TRUE, frames = \"", f,
              "\"), and commit the snapshot with the change.")
    }
    tibble::tibble(frame = rep(f, nrow(changes)), changes)
  }))
  invisible(changes)
}

# --- Pilot gate ---------------------------------------------------------------
# Review flags of an institution's pilot (README "Piloting an Institution";
# methods.qmd "Pilot protocol and gate"): a share of the pilot sample's
# course-years below its value, or `unmapped_headings` or `privacy_hits`
# above it, is reviewed in the pilot audits. A flag does not block the full
# harvest on its own; the audits' verdict does, and a flagged number needs a
# fix or a reason there (decided 2026-10-17). Values from what the configured
# institutions reach in the teacher-ed frame (2026-10-16, methods.qmd).
PILOT_GATE <- c(fetched = 0.80, plan = 0.80, assessment = 0.90, coursework = 0.70,
                unmapped_headings = 0.65, privacy_hits = 0)
PILOT_GATE_MAX <- c("unmapped_headings", "privacy_hits")

#' Gate numbers of a pilot sample
#'
#' The sample's course-years (course_code_raw x year, pilot_sample()) in the
#' built data of all courses: `fetched`, the share with a page in some
#' semester (its URL answered "ok", fetch_coverage(), or it has a plan: a
#' current site's plan comes from an earlier snapshot); `plan`, the share
#' with a plan; `assessment` and `coursework`, the share of those with a plan
#' whose plan has an `assessment` or a `coursework_requirements` section;
#' `no_sections`, the course-years whose plan has no section at all (a page
#' layout the section extractor does not read: accepted, but counted; no
#' threshold); `unmapped_headings`, the share of the headings on the
#' sample's pages that map to no section (target page_headings);
#' `privacy_hits`, the course-years whose plan or sections hold personal
#' data by the patterns of target privacy_check.
#'
#' @param sample The pilot sample (its file, pilot_file()): `institution`,
#'   `course_id`, `course_code_raw`, `year`.
#' @param coverage Target coverage, frame "all".
#' @param offerings,sections course_offerings_full and plan_sections of all
#'   courses.
#' @param headings,page_map Targets page_headings and page_map.
#' @param thresholds PILOT_GATE.
#' @return Tibble `metric`, `value`, `n` (its denominator), `threshold`,
#'   `review` (TRUE when the value is beyond its threshold: the pilot audit
#'   finds its reason or a fix; NA without a threshold), `examples` (list: a
#'   course id of each course-year that counts against the metric, or the most
#'   frequent unmapped headings).
gate_numbers <- function(sample, coverage, offerings, sections, headings, page_map,
                         thresholds = PILOT_GATE) {
  key <- offering_key(sample$course_id)
  cov <- coverage[match(key, offering_key(coverage$course_id)), ]
  off <- offerings[match(key, offering_key(offerings$course_id)), ]
  planned <- cov$has_plan %in% TRUE & !is.na(off$plan_content_id)
  plan_key <- paste(off$plan_content_id, off$course_code)
  sec <- sections[sections$institution %in% sample$institution, ]
  sec <- sec[paste(sec$plan_content_id, sec$course_code) %in% plan_key[planned], ]
  sec_key <- paste(sec$plan_content_id, sec$course_code)
  has <- function(s) planned & plan_key %in% sec_key[sec$section %in% s]
  personal <- function(x) Reduce(`|`, lapply(.personal_data_patterns, grepl, x = x, perl = TRUE))
  unit <- paste(sample$course_code_raw, sample$year)
  by_unit <- function(x) unname(tapply(x, unit, any)[unique(unit)])
  u <- tibble::tibble(
    id = sample$course_id[!duplicated(unit)], fetched = by_unit(cov$outcome %in% "ok" | planned),
    plan = by_unit(planned), assessment = by_unit(has("assessment")),
    coursework = by_unit(has("coursework_requirements")), sectioned = by_unit(has(sec$section)),
    personal = by_unit(planned & (personal(off$course_plan) | plan_key %in% sec_key[personal(sec$text)])))

  pages <- unique(stats::na.omit(page_map$page_id[match(off$course_id, page_map$course_id)]))
  h <- headings[headings$page_id %in% pages, ]
  unmapped <- h$value[h$what == "unmapped"]
  out <- tibble::tibble(
    metric = c("fetched", "plan", "assessment", "coursework", "no_sections",
               "unmapped_headings", "privacy_hits"),
    value = c(mean(u$fetched), mean(u$plan), mean(u$assessment[u$plan]), mean(u$coursework[u$plan]),
              sum(u$plan & !u$sectioned), if (nrow(h)) length(unmapped) / nrow(h) else NA,
              sum(u$personal)),
    n = c(nrow(u), nrow(u), rep(sum(u$plan), 3), nrow(h), sum(u$plan)),
    examples = c(lapply(list(!u$fetched, !u$plan, u$plan & !u$assessment, u$plan & !u$coursework,
                             u$plan & !u$sectioned), function(x) u$id[x]),
                 list(names(utils::head(sort(table(unmapped), decreasing = TRUE), 10)), u$id[u$personal])))
  out$threshold <- unname(thresholds[out$metric])
  out$review <- ifelse(out$metric %in% PILOT_GATE_MAX, out$value > out$threshold,
                       out$value < out$threshold)
  out[c("metric", "value", "n", "threshold", "review", "examples")]
}

#' The gate numbers of an institution's pilot
#'
#' gate_numbers() of its pilot sample in the current build, an input to the
#' pilot audits (/audit-institutions --pilot): run targets::tar_make() first,
#' and read this with the pipeline's functions loaded
#' (targets::tar_load_globals()).
#'
#' @param institution The institution.
#' @param thresholds PILOT_GATE.
#' @param sample Its pilot sample; by default its file (pilot_file()).
pilot_gate <- function(institution, thresholds = PILOT_GATE,
                       sample = utils::read.csv(pilot_file(institution), colClasses = "character")) {
  coverage <- targets::tar_read(coverage)
  gate_numbers(sample, coverage[coverage$frame == "all", ],
               readRDS(file.path(frame_dir("all", "interim"), "course_offerings_full.RDS")),
               readRDS(file.path(frame_dir("all"), "plan_sections.RDS")),
               targets::tar_read(page_headings), targets::tar_read(page_map), thresholds)
}

#' What a code change does to plans and sections, without tar_make()
#'
#' Runs the per-partition steps (partition_blocks(), partition_text(),
#' partition_sections()) on a sample of the raw store twice, with the code of
#' `ref` and with the working tree, and returns where they differ. For trying
#' an extraction, heading-map or anonymizer fix in a minute: the full
#' rebuild and metrics_check stay the check before a commit (README
#' "Rebuilding Derived Data"). Each version's code (the files _targets.R
#' sources) is loaded into an environment of its own; the session's is not
#' changed.
#'
#' @param institutions Institutions to sample.
#' @param n Pages per raw partition (institution x DBH year), drawn with a
#'   seed among those with a page, so every year is in.
#' @param seed The seed of the sample: another seed, other pages.
#' @param course_ids Instead of a sample, the pages of these offerings.
#' @param ref The git ref to compare with.
#' @param raw_dir The raw store.
#' @return One row per page and `what` ("course_plan" or a section) whose
#'   text differs: institution, year, course_id, what, old, new (NA: none).
preview_change <- function(institutions, n = 5, seed = 42, course_ids = NULL, ref = "HEAD",
                           raw_dir = RAW_DIR) {
  tmp <- tempfile("preview_raw_")
  on.exit(unlink(tmp, recursive = TRUE))
  withr::local_seed(seed)
  for (f in unlist(lapply(institutions, harvest_files, raw_dir = raw_dir))) {
    ds <- arrow::open_dataset(f)
    has <- intersect(c("html", "extracted_text"), names(ds))
    rows <- dplyr::collect(dplyr::select(ds, harvest_row, course_id))
    # whether a row has a page, without reading the pages into R
    page <- Reduce(`|`, lapply(has, function(x) dplyr::pull(dplyr::transmute(ds, v = !is.na(.data[[x]])), v)))
    pick <- if (is.null(course_ids)) rows$harvest_row[page] else rows$harvest_row[page & rows$course_id %in% course_ids]
    if (!length(pick)) next
    keep <- sort(if (is.null(course_ids)) pick[sample.int(length(pick), min(n, length(pick)))] else pick)
    .write_plain_parquet(dplyr::collect(dplyr::filter(ds, harvest_row %in% keep)),
                         file.path(tmp, sub(paste0("^", raw_dir, "/"), "", f)))
  }
  parts <- raw_partitions(tmp)
  run <- function(env) dplyr::bind_rows(lapply(seq_len(nrow(parts)), function(i) {
    files <- harvest_files(parts$institution[i], tmp, parts$year[i])
    config <- env$get_institution_config(parts$institution[i])
    blocks <- env$partition_blocks(files, config)
    text <- env$partition_text(files, blocks, config)
    sections <- env$partition_sections(blocks, text, config, text$page_id)
    dplyr::bind_rows(tibble::tibble(page_id = text$page_id, what = "course_plan", text = text$course_plan),
                     dplyr::rename(sections, what = section)) |>
      dplyr::summarise(text = paste(text, collapse = "\n\n"), .by = c(page_id, what))
  }))
  old <- run(.code_env(ref))
  new <- run(.code_env(NULL))
  ids <- read_raw(harvest_files(parts$institution, tmp), page_id = TRUE)
  dplyr::full_join(old, new, by = c("page_id", "what"), suffix = c("_old", "_new")) |>
    dplyr::filter(is.na(text_old) != is.na(text_new) | text_old != text_new) |>
    dplyr::left_join(dplyr::distinct(ids[c("page_id", "institution", "year", "course_id")], page_id, .keep_all = TRUE),
                     by = "page_id") |>
    dplyr::select(institution, year, course_id, what, old = text_old, new = text_new) |>
    dplyr::arrange(institution, year, course_id, what)
}

# The pipeline's code (the files _targets.R sources) at a git ref, or in the
# working tree (NULL), loaded into a new environment.
.code_env <- function(ref = NULL) {
  show <- function(f) if (is.null(ref)) readLines(f, warn = FALSE) else system2("git", c("show", paste0(ref, ":", f)), stdout = TRUE)
  sources <- sub("(?s).*?tar_source\\((.*?)\\)\\).*", "\\1", paste(show("_targets.R"), collapse = "\n"), perl = TRUE)
  files <- regmatches(sources, gregexpr("R/\\w+\\.R", sources))[[1]]
  env <- new.env(parent = globalenv())
  for (f in files) eval(parse(text = show(f), keep.source = FALSE), envir = env)
  env
}
