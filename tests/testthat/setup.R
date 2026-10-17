# Shared setup for testthat tests
# This file is automatically sourced before tests run

library(dplyr)
library(rvest)
library(glue)
library(stringr)
library(httr)
library(xml2)

# Source project files
source(here::here("R/utils.R"))
source(here::here("R/add_course_url.R"))
source(here::here("R/resolve_course_urls.R"))
source(here::here("R/anonymize.R"))
source(here::here("R/normalize_plan_text.R"))
source(here::here("R/deduplicate_plans.R"))
source(here::here("R/fetch_html_cols.R"))
source(here::here("R/extract_fulltext.R"))
source(here::here("R/institution_config.R"))
source(here::here("R/section_heading_map.R"))
source(here::here("R/blocks.R"))
source(here::here("R/extract_sections.R"))
source(here::here("R/pipeline_metrics.R"))
source(here::here("R/pipeline.R"))
source(here::here("R/checkpoint.R"))
source(here::here("R/harvest_strategies.R"))
source(here::here("R/harvest.R"))
source(here::here("R/release.R"))
source(here::here("R/wayback.R"))

# The robots.txt table (data/robots_txt.csv), also from the tests' folder and
# the temporary folders the harvest tests run in
assign("ROBOTS_FILE", here::here(ROBOTS_FILE), envir = globalenv())

# A robots.txt table of the given hosts and rules (ROBOTS_FILE) for one test
local_robots <- function(host, disallow = NA, allow = NA, institution = NA, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  utils::write.csv(data.frame(institution, host, disallow, allow), f, row.names = FALSE, na = "")
  old <- ROBOTS_FILE
  assign("ROBOTS_FILE", f, envir = globalenv())
  withr::defer(assign("ROBOTS_FILE", old, envir = globalenv()), envir = env)
}
