# _targets.R
# The post-harvest pipeline (#263): targets::tar_make() rebuilds whatever is
# outdated, from data/raw/html_{inst}.RDS to the data files, the browsers' data and
# the data notes. See README "Rebuilding Derived Data".

library(targets)

tar_option_set(
  packages = "dplyr",
  # The institutions' branches are independent, so they run on 4 local worker
  # processes (#268). Workers read and write the store themselves, so large
  # results do not pass through the main process.
  controller = crew::crew_controller_local(workers = 4, seconds_idle = 60),
  storage = "worker",
  retrieval = "worker"
)

tar_source(c(
  "R/utils.R", "R/fetch_html_cols.R", "R/extract_fulltext.R",
  "R/institution_config.R", "R/section_heading_map.R", "R/blocks.R",
  "R/extract_sections.R",
  "R/anonymize.R", "R/normalize_plan_text.R", "R/deduplicate_plans.R",
  "R/pipeline_metrics.R", "R/pipeline.R"
))

# Read when the pipeline is loaded; {targets} tracks this global, so a new
# data/raw/html_{inst}.RDS adds a branch without invalidating the others.
institutions <- harvested_institutions()

list(
  # One branch per harvested institution (pattern = map(...)): a change to one
  # institution's raw data or config rebuilds only that institution.
  tar_target(institution, institutions),
  # the latest harvest, plus earlier ones of "current" sites (#293)
  tar_target(html_file, harvest_files(institution), pattern = map(institution),
             format = "file"),
  tar_target(config, institution_config_target(institution),
             pattern = map(institution), iteration = "list"),
  tar_target(blocks, institution_blocks(html_file, config),
             pattern = map(html_file, config)),
  tar_target(fulltext, institution_fulltext(html_file, blocks, config),
             pattern = map(html_file, blocks, config)),
  tar_target(plans, institution_plans(html_file, fulltext, config),
             pattern = map(html_file, fulltext, config), iteration = "list"),
  tar_target(sections, institution_sections(blocks, fulltext, config, plans),
             pattern = map(blocks, fulltext, config, plans)),
  tar_target(unmapped, unmapped_headings(blocks, config),
             pattern = map(blocks, config)),
  tar_target(heading_hits, institution_heading_hits(blocks, fulltext, config),
             pattern = map(blocks, fulltext, config)),
  tar_target(heading_use, heading_pattern_use(heading_hits)),

  # Data files (README "Data Files: Published and Internal")
  tar_target(extracted_text_file,
             write_rds_file(fulltext, "data/interim/extracted_text.RDS"), format = "file"),
  tar_target(offerings, combine_offerings(plans, sections)),
  tar_target(offerings_full_file,
             write_rds_file(offerings, "data/interim/course_offerings_full.RDS"), format = "file"),
  tar_target(offerings_file,
             write_rds_file(slim_offerings(offerings), "data/processed/course_offerings.RDS"),
             format = "file"),
  tar_target(course_plans_file,
             write_rds_file(combine_plans(plans, sections), "data/processed/course_plans.RDS"), format = "file"),
  tar_target(sections_file,
             write_rds_file(sections, "data/processed/plan_sections.RDS"), format = "file"),

  # Personal data left in the shareable files (#271)
  tar_target(privacy_check,
             check_personal_data(c(offerings_file, course_plans_file, sections_file))),

  # Regression check against tests/snapshots/pipeline_metrics.csv (#255)
  tar_target(metrics, pipeline_metrics(offerings, sections)),
  tar_target(snapshot_file, METRICS_SNAPSHOT, format = "file"),
  tar_target(metrics_check, metrics_vs_snapshot(snapshot_file, metrics)),

  # Browser data and data notes
  tar_target(browser_script, "app/course_browser/build_data.R", format = "file"),
  tar_target(browser_data_file,
             run_r_script(browser_script, "app/course_browser/data/browser_data.RDS",
                          c(course_plans_file, offerings_full_file, sections_file),
                          chdir = TRUE),
             format = "file"),
  tar_target(ojs_script, "app/course_browser_ojs/build_data.R", format = "file"),
  tar_target(ojs_data_files,
             run_r_script(ojs_script,
                          c("app/course_browser_ojs/data/plans.parquet",
                            "app/course_browser_ojs/data/offerings.parquet"),
                          c(course_plans_file, offerings_file), chdir = TRUE),
             format = "file"),
  tar_target(methods_qmd, "methods.qmd", format = "file"),
  tar_target(methods_file,
             render_quarto(methods_qmd, "methods.md",
                           c(offerings_file, offerings_full_file, course_plans_file, sections_file)),
             format = "file"),
  tar_target(data_notes_qmd, "data/data_notes.qmd", format = "file"),
  tar_target(data_notes_file,
             render_quarto(data_notes_qmd, "data/data_notes.md",
                           c(offerings_file, course_plans_file)),
             format = "file")
)
