# =====================================================
# UiA Course Harvest Script with Verification
# =====================================================

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

# Load course data
courses <- readRDS("data/courses.RDS")

# Filter for UiA courses (only 2020+ available on website)
df <- courses |>
  filter(
    institution_short == "uia",
    Årstall >= 2020  # UiA website only has courses from 2020 onwards
  ) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

cat("============================================\n")
cat("UiA Course Harvest\n")
cat("============================================\n")
cat("Total courses:", nrow(df), "\n")
cat("Year range:", min(df$Årstall), "-", max(df$Årstall), "\n")
cat("Semesters:", paste(unique(df$Semesternavn), collapse=", "), "\n\n")

# Show sample URLs for verification
cat("Sample URLs (first 5):\n")
sample_urls <- df |>
  select(Emnekode, Årstall, Semesternavn, url) |>
  head(5)
print(sample_urls)
cat("\n")

# Verify URL accessibility with a small test sample
cat("Testing URL accessibility (first 3 courses)...\n")
test_sample <- df |>
  head(3) |>
  select(course_id, Emnekode, Årstall, Semesternavn, url)

for (i in 1:nrow(test_sample)) {
  url <- test_sample$url[i]
  cat(sprintf("Testing: %s\n", url))

  # Try to fetch the page
  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_user_agent("TEPS course content harvest (research project); https://github.com/teps-project") |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    return(NULL)
  })

  if (!is.null(resp)) {
    status <- httr2::resp_status(resp)
    cat(sprintf("  Status: %d %s\n", status, httr2::resp_status_desc(resp)))

    if (status == 200) {
      # Try extracting content with the CSS selector
      html_content <- httr2::resp_body_string(resp)
      parsed_html <- rvest::read_html(html_content)

      # Test the selector
      fulltext <- tryCatch({
        text <- rvest::html_elements(parsed_html, ".main-text") |>
          rvest::html_text2() |>
          paste(collapse = "\n\n")

        if (nchar(text) > 0) {
          cat(sprintf("  ✓ Extracted %d characters\n", nchar(text)))
          # Show snippet
          snippet <- substr(text, 1, 100)
          cat(sprintf("  Snippet: %s...\n", snippet))
        } else {
          cat("  ⚠ WARNING: Selector found no content\n")
        }
        text
      }, error = function(e) {
        cat(sprintf("  ⚠ Extraction error: %s\n", e$message))
        NA_character_
      })
    }
  }
  cat("\n")
}

# Proceed with full harvest
cat("============================================\n")
cat("Starting full harvest with checkpointing...\n")
cat("============================================\n\n")

df <- fetch_html_with_checkpoint(
  df,
  checkpoint_path = "data/checkpoint/html_uia.RDS"
)

cat("\nExtracting fulltext...\n")
df$fulltext <- extract_fulltext(df$institution_short, df$html)

# Summary statistics
cat("\n============================================\n")
cat("Harvest Complete - Summary\n")
cat("============================================\n")
cat("Total courses:", nrow(df), "\n")
cat("Successful HTML fetches:", sum(df$html_success, na.rm = TRUE), "\n")
cat("Failed HTML fetches:", sum(!df$html_success, na.rm = TRUE), "\n")
cat("Non-empty fulltext:", sum(!is.na(df$fulltext) & nchar(df$fulltext) > 0, na.rm = TRUE), "\n")
cat("Empty/NA fulltext:", sum(is.na(df$fulltext) | nchar(df$fulltext) == 0, na.rm = TRUE), "\n")

# Show fulltext length distribution
if (any(!is.na(df$fulltext))) {
  text_lengths <- nchar(df$fulltext[!is.na(df$fulltext)])
  cat("\nFulltext length statistics:\n")
  cat("  Min:", min(text_lengths, na.rm = TRUE), "\n")
  cat("  Median:", median(text_lengths, na.rm = TRUE), "\n")
  cat("  Mean:", round(mean(text_lengths, na.rm = TRUE)), "\n")
  cat("  Max:", max(text_lengths, na.rm = TRUE), "\n")
}

# Show any errors
errors <- df |> filter(!is.na(html_error))
if (nrow(errors) > 0) {
  cat("\nErrors encountered:\n")
  error_summary <- errors |>
    group_by(html_error) |>
    summarise(count = n()) |>
    arrange(desc(count))
  print(error_summary)
}

# Save results
output_file <- "data/html_uia.RDS"
saveRDS(df, file = output_file)
cat("\nResults saved to:", output_file, "\n")
