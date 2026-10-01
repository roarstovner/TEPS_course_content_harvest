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
source(here::here("R/fetch_html_cols.R"))
source(here::here("R/extract_fulltext.R"))
source(here::here("R/institution_config.R"))
source(here::here("R/section_heading_map.R"))
source(here::here("R/extract_sections.R"))
