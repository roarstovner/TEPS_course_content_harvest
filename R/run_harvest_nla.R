# run_harvest_nla.R
# Harvest pipeline for NLA (NLA Høgskolen)
#
# NLA course pages contain all years on one page, so the URL has no year
# component. We deduplicate URLs before fetching to avoid redundant requests,
# then join the HTML back to all course rows.

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "nla", Årstall == max(Årstall)) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

message("NLA: ", sum(!is.na(df$url)), "/", nrow(df), " URLs generated")
message("NLA: ", n_distinct(df$url, na.rm = TRUE), " unique URLs to fetch")

# Fetch only unique URLs to avoid hammering NLA's server
unique_urls <- df |>
  distinct(url, institution_short) |>
  filter(!is.na(url)) |>
  mutate(course_id = paste0("nla_url_", row_number()))

unique_fetched <- fetch_html_with_checkpoint(
  unique_urls,
  checkpoint_path = "data/checkpoint/html_nla.RDS"
)

# Join HTML back to all rows by URL
html_lookup <- unique_fetched |>
  select(url, html, html_error, html_success)

df <- left_join(df, html_lookup, by = "url")

df$fulltext <- extract_fulltext(df$institution_short, df$html)
saveRDS(df, "data/html_nla.RDS")

message("NLA done: ", sum(!is.na(df$fulltext)), "/", nrow(df), " with fulltext")
