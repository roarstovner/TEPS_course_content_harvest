# run_harvest_nla.R
# Harvest pipeline for NLA (NLA Høgskolen)
#
# NLA course pages embed all historical year variants as JSON in a <script> tag
# (jsxPath: "EmneplanPage"). The props.items object is keyed by academic year
# (e.g., "2023-2024"), each containing title, table, and accordions with full
# course plan HTML. We fetch once per unique course code and extract
# year-specific content from the JSON.

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "nla") |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url") |>
  mutate(academic_year = nla_academic_year(Årstall, Semesternavn))

message("NLA: ", nrow(df), " total course rows")
message("NLA: ", n_distinct(df$Emnekode), " unique course codes to fetch")

# Fetch once per unique Emnekode (all years share the same page)
unique_codes <- df |>
  distinct(Emnekode, .keep_all = TRUE) |>
  select(Emnekode, url, institution_short) |>
  filter(!is.na(url)) |>
  mutate(course_id = paste0("nla_", Emnekode))

unique_fetched <- fetch_html_with_checkpoint(
  unique_codes,
  checkpoint_path = "data/checkpoint/html_nla.RDS"
)

# Join HTML back to all rows by Emnekode
html_lookup <- unique_fetched |>
  select(Emnekode, html, html_error, html_success)

df <- left_join(df, html_lookup, by = "Emnekode")

# Extract year-specific fulltext from JSON embedded in HTML
df$fulltext <- extract_nla_json(df$html, df$academic_year)

saveRDS(df, "data/html_nla.RDS")

message("NLA done: ", sum(!is.na(df$fulltext)), "/", nrow(df), " with fulltext")
