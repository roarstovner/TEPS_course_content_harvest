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
