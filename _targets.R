# _targets.R
# The post-harvest pipeline: targets::tar_make() rebuilds whatever is
# outdated, from the raw store data/raw/{inst}/{year}/ to the data files of
# each frame, the browsers' data and the data notes. See docs/rebuilding.md
# "Rebuilding Derived Data".

library(targets)

tar_option_set(
  packages = "dplyr",
  # Branches are independent, so they run on 4 local worker processes.
  # Workers read and write the store themselves, so large results do not
  # pass through the main process.
  controller = crew::crew_controller_local(workers = 4, seconds_idle = 60),
  storage = "worker",
  retrieval = "worker"
)

tar_source(c(
  "R/utils.R", "R/add_course_url.R", "R/fetch_html_cols.R", "R/extract_fulltext.R",
  "R/institution_config.R", "R/section_heading_map.R", "R/blocks.R",
  "R/extract_sections.R",
  "R/anonymize.R", "R/normalize_plan_text.R", "R/deduplicate_plans.R",
  "R/pipeline_metrics.R", "R/pipeline.R", "R/release.R"
))

# Read when the pipeline is loaded; {targets} tracks these globals, so a new
# institution or DBH year in data/raw adds branches without invalidating the
# others.
partitions <- raw_partitions()
institutions <- harvested_institutions()

list(
  # Per raw partition (institution x DBH year, pattern = map(partition)):
  # every stored page is read once, so a new harvest of one year, or a change
  # to one institution's config, rebuilds only those partitions. The
  # partition's raw files are listed on every build, so a new run is seen.
  tar_target(partition, partitions),
  tar_target(part_file, harvest_files(partition$institution, year = partition$year),
             pattern = map(partition), format = "file", cue = tar_cue(mode = "always"),
             deployment = "main"),
  tar_target(part_config, institution_config_target(partition$institution),
             pattern = map(partition), iteration = "list", deployment = "main"),
  tar_target(blocks, partition_blocks(part_file, part_config),
             pattern = map(part_file, part_config)),
  tar_target(page_text, partition_text(part_file, blocks, part_config),
             pattern = map(part_file, blocks, part_config)),
  tar_target(page_headings, partition_headings(blocks, page_text, part_config),
             pattern = map(blocks, page_text, part_config)),

  # Per institution: the page each offering takes (read_harvest() without
  # the pages), how each offering was fetched
  tar_target(institution, institutions),
  tar_target(html_file, harvest_files(institution), pattern = map(institution),
             format = "file", cue = tar_cue(mode = "always"), deployment = "main"),
  tar_target(config, institution_config_target(institution),
             pattern = map(institution), iteration = "list", deployment = "main"),
  tar_target(pages, read_harvest(html_file, config, content = FALSE),
             pattern = map(html_file, config), iteration = "list"),
  tar_target(fetch_status, institution_fetch_status(pages), pattern = map(pages)),
  # how each offering of each run was fetched (docs/harvesting.md "Fetch Ledger and Retries")
  tar_target(ledger, raw_ledger(html_file), pattern = map(html_file)),
  tar_target(changed_pages, page_changes(ledger, html_file, config),
             pattern = map(ledger, html_file, config)),

  # All offerings, their plans, and the frames (docs/data-files.md "Frames"): plans are
  # deduplicated within each frame, and sections cut from the pages their
  # plans keep, per partition
  tar_target(page_map, offering_pages(pages)),
  tar_target(fulltext, offering_fulltext(pages, page_text, config)),
  tar_target(courses, plan_offerings(pages, page_text, config)),
  tar_target(dbh_file, dbh_snapshot_file("courses"), format = "file",
             cue = tar_cue(mode = "always")),
  tar_target(frames, frame_members(courses$course_id, dbh_snapshot_date(dbh_file))),
  tar_target(plans, frame_plans(courses, frames)),
  tar_target(source_pages, plan_source_pages(plans, page_map)),
  tar_target(part_sources, partition_page_ids(source_pages, partition),
             pattern = map(partition), deployment = "main"),
  tar_target(source_sections, partition_sections(blocks, page_text, part_config, part_sources),
             pattern = map(blocks, page_text, part_config, part_sources)),
  tar_target(sections, lapply(plans, frame_sections, source_sections, page_map, institution)),
  tar_target(offerings, frame_offerings(courses, frames, plans, sections)),
  tar_target(unmapped, heading_counts(page_headings, page_map, "unmapped", institution)),
  tar_target(heading_hits, heading_counts(page_headings, page_map, "pattern", institution)),
  tar_target(heading_use, heading_pattern_use(heading_hits)),

  # Data files (docs/data-files.md "Data Files: Published and Internal"): all offerings
  # in data/interim/ and data/processed/, the teacher-ed frame (te_*) in
  # their teacher_ed/ folders
  tar_target(extracted_text_file,
             write_rds_file(fulltext, "data/interim/extracted_text.RDS"), format = "file"),
  tar_target(offerings_full_file,
             write_rds_file(offerings$all, "data/interim/course_offerings_full.RDS"), format = "file"),
  tar_target(offerings_file,
             write_rds_file(slim_offerings(offerings$all), "data/processed/course_offerings.RDS"),
             format = "file"),
  tar_target(course_plans_file,
             write_rds_file(combine_plans(plans$all, sections$all), "data/processed/course_plans.RDS"),
             format = "file"),
  tar_target(sections_file,
             write_rds_file(sections$all, "data/processed/plan_sections.RDS"), format = "file"),
  tar_target(te_offerings_full_file,
             write_rds_file(offerings$teacher_ed,
                            file.path(frame_dir("teacher_ed", "interim"), "course_offerings_full.RDS")),
             format = "file"),
  tar_target(te_offerings_file,
             write_rds_file(slim_offerings(offerings$teacher_ed),
                            file.path(frame_dir("teacher_ed"), "course_offerings.RDS")),
             format = "file"),
  tar_target(te_course_plans_file,
             write_rds_file(combine_plans(plans$teacher_ed, sections$teacher_ed),
                            file.path(frame_dir("teacher_ed"), "course_plans.RDS")),
             format = "file"),
  tar_target(te_sections_file,
             write_rds_file(sections$teacher_ed, file.path(frame_dir("teacher_ed"), "plan_sections.RDS")),
             format = "file"),
  tar_target(gaps, plan_gaps(offerings$all, fetch_status)),
  tar_target(gaps_file, write_rds_file(gaps, "data/interim/plan_gaps.RDS"), format = "file"),
  # how far the harvest got with each offering of each frame of the latest
  # DBH snapshot
  tar_target(coverage, frame_coverage(dbh_snapshot_date(dbh_file), ledger, offerings)),

  # Personal data left in the shareable files
  tar_target(privacy_check,
             check_personal_data(c(offerings_file, course_plans_file, sections_file,
                                   te_offerings_file, te_course_plans_file, te_sections_file))),

  # Regression check against the metrics snapshot of each frame
  # (tests/snapshots/pipeline_metrics_{frame}.csv)
  tar_target(metrics, frame_metrics(offerings, sections)),
  tar_target(snapshot_file, unname(METRICS_SNAPSHOT), format = "file"),
  tar_target(metrics_check, metrics_vs_snapshot(snapshot_file, metrics)),

  # The finalized (teacher-ed) release is unchanged: raw files and plans
  tar_target(frozen_check,
             frozen_changes(RAW_MANIFEST, te_offerings_file, te_course_plans_file, te_sections_file),
             cue = tar_cue(mode = "always")),

  # The browsers (teacher-ed frame), the methods notes (teacher-ed frame, the
  # release's) and the data notes (all offerings)
  tar_target(browser_script, "app/course_browser/build_data.R", format = "file"),
  tar_target(browser_data_file,
             run_r_script(browser_script, "app/course_browser/data/browser_data.RDS",
                          c(te_course_plans_file, te_offerings_full_file, te_sections_file),
                          chdir = TRUE),
             format = "file"),
  tar_target(ojs_script, "app/course_browser_ojs/build_data.R", format = "file"),
  tar_target(ojs_data_files,
             run_r_script(ojs_script,
                          c("app/course_browser_ojs/data/plans.parquet",
                            "app/course_browser_ojs/data/offerings.parquet"),
                          c(te_course_plans_file, te_offerings_file), chdir = TRUE),
             format = "file"),
  tar_target(methods_qmd, "methods.qmd", format = "file"),
  tar_target(methods_file,
             render_quarto(methods_qmd, "methods.md",
                           c(te_offerings_file, te_offerings_full_file, te_course_plans_file,
                             te_sections_file)),
             format = "file"),
  tar_target(data_notes_qmd, "data/data_notes.qmd", format = "file"),
  tar_target(data_notes_file,
             render_quarto(data_notes_qmd, "data/data_notes.md",
                           c(offerings_file, course_plans_file, gaps_file)),
             format = "file")
)
