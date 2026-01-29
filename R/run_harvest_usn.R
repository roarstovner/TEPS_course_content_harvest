library(dplyr)

source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(
    institution_short == "usn",
  ) |>
  add_course_id() |>
  add_course_url() |> 
  resolve_course_urls(
    checkpoint_path = "data/checkpoint/usn_urls.RDS"
  )

# Add html_success and html_error columns for consistency with other institutions
df <- df |>
  mutate(
    html_success = !is.na(html) & html != "",
    html_error = vector("list", n())
  )

# Extract fulltext
df$fulltext <- extract_fulltext(df$institution_short, df$html)

# Save results
saveRDS(df, "data/html_usn.RDS")

# Print summary
cat("\nUSN Harvest Summary:\n")
cat("Total courses:", nrow(df), "\n")
cat("URLs discovered:", sum(!is.na(df$url)), "\n")
cat("URLs not found:", sum(is.na(df$url)), "\n")
cat("HTML fetched successfully:", sum(df$html_success, na.rm = TRUE), "\n")
cat("Fulltext extracted:", sum(!is.na(df$fulltext)), "\n")

# test

usn_urls <- read_url_checkpoint("data/checkpoint/usn_urls.RDS")
usn_urls <- usn_urls |> left_join(courses |> add_course_id(), by = "course_id")
fulltext <- usn_urls  |> mutate(fulltext = extract_fulltext(institution_short, html))

