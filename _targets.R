# _targets.R
# The post-harvest pipeline (#263): targets::tar_make() rebuilds whatever is
# outdated, from data/html_{inst}.RDS to the data files, the browsers' data and
# the data notes. See README "Rebuilding Derived Data".

library(targets)

tar_option_set(packages = "dplyr")

tar_source(c(
  "R/utils.R", "R/fetch_html_cols.R", "R/extract_fulltext.R",
  "R/institution_config.R", "R/section_heading_map.R", "R/extract_sections.R",
  "R/anonymize.R", "R/normalize_plan_text.R", "R/deduplicate_plans.R",
  "R/pipeline_metrics.R", "R/pipeline.R"
))

# Read when the pipeline is loaded; {targets} tracks this global, so a new
# data/html_{inst}.RDS adds a branch without invalidating the others.
institutions <- harvested_institutions()

list(
  # One branch per harvested institution (pattern = map(...)): a change to one
  # institution's raw data or config rebuilds only that institution.
  tar_target(institution, institutions),
  tar_target(html_file, harvest_file(institution), pattern = map(institution),
             format = "file"),
  tar_target(config, institution_config_target(institution),
             pattern = map(institution), iteration = "list"),
  tar_target(fulltext, institution_fulltext(html_file, config),
             pattern = map(html_file, config)),
  tar_target(plans, institution_plans(html_file, fulltext),
             pattern = map(html_file, fulltext), iteration = "list"),
  tar_target(sections, institution_sections(html_file, fulltext, config),
             pattern = map(html_file, fulltext, config)),
  tar_target(unmapped, unmapped_headings(html_file, config),
             pattern = map(html_file, config)),

  # Data files (README "Data Files: Published and Internal")
  tar_target(extracted_text_file,
             write_rds_file(fulltext, "data/extracted_text.RDS"), format = "file"),
  tar_target(offerings, combine_offerings(plans)),
  tar_target(offerings_full_file,
             write_rds_file(offerings, "data/course_offerings_full.RDS"), format = "file"),
  tar_target(offerings_file,
             write_rds_file(slim_offerings(offerings), "data/course_offerings.RDS"),
             format = "file"),
  tar_target(course_plans_file,
             write_rds_file(combine_plans(plans), "data/course_plans.RDS"), format = "file"),
  tar_target(sections_file,
             write_rds_file(sections, "data/sections_raw.RDS"), format = "file"),

  # Regression check against tests/snapshots/pipeline_metrics.csv (#255)
  tar_target(metrics, pipeline_metrics(offerings, sections)),
  tar_target(snapshot_file, METRICS_SNAPSHOT, format = "file"),
  tar_target(metrics_check, metrics_vs_snapshot(snapshot_file, metrics)),

  # Browser data and data notes
  tar_target(browser_script, "R/build_browser_data.R", format = "file"),
  tar_target(browser_data_file,
             run_r_script(browser_script, "data/browser_data.RDS",
                          c(course_plans_file, offerings_full_file, sections_file)),
             format = "file"),
  tar_target(ojs_script, "app/course_browser_ojs/build_data.R", format = "file"),
  tar_target(ojs_data_files,
             run_r_script(ojs_script,
                          c("app/course_browser_ojs/data/plans.parquet",
                            "app/course_browser_ojs/data/offerings.parquet"),
                          c(course_plans_file, offerings_file), chdir = TRUE),
             format = "file"),
  tar_target(data_notes_qmd, "data/data_notes.qmd", format = "file"),
  tar_target(data_notes_file,
             render_quarto(data_notes_qmd, "data/data_notes.md",
                           c(offerings_file, course_plans_file)),
             format = "file")
)
