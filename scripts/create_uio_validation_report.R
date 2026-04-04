#!/usr/bin/env Rscript
# Create UiO validation report from existing harvest data

library(dplyr)
library(stringr)

# Load the harvested data
uio_data <- readRDS("data/html_uio.RDS")

cat("=== UiO Validation Report Data Collection ===\n\n")

# Basic statistics
total_courses <- nrow(uio_data)
url_success <- sum(!is.na(uio_data$url))
html_success <- sum(uio_data$html_success, na.rm = TRUE)
text_success <- sum(!is.na(uio_data$extracted_text))

cat("Total courses:", total_courses, "\n")
cat("URL generation success:", url_success, "/", total_courses,
    sprintf(" (%.1f%%)\n", 100 * url_success / total_courses))
cat("HTML fetch success:", html_success, "/", total_courses,
    sprintf(" (%.1f%%)\n", 100 * html_success / total_courses))
cat("Text extraction success:", text_success, "/", total_courses,
    sprintf(" (%.1f%%)\n", 100 * text_success / total_courses))

# Text length statistics
if (text_success > 0) {
  text_lengths <- nchar(uio_data$extracted_text[!is.na(uio_data$extracted_text)])
  cat("\nText length statistics:\n")
  cat("  Min:", min(text_lengths), "chars\n")
  cat("  Max:", max(text_lengths), "chars\n")
  cat("  Mean:", round(mean(text_lengths)), "chars\n")
  cat("  Median:", median(text_lengths), "chars\n")
}

# Year distribution
cat("\nYear distribution:\n")
year_table <- table(uio_data$Årstall)
print(year_table)

# Semester distribution
cat("\nSemester distribution:\n")
semester_table <- table(uio_data$Semesternavn)
print(semester_table)

# Faculty coverage
cat("\nFaculty coverage analysis:\n")
faculty_stats <- uio_data %>%
  group_by(Avdelingsnavn) %>%
  summarise(
    total = n(),
    url_success = sum(!is.na(url)),
    url_pct = round(100 * url_success / n(), 1),
    html_success = sum(html_success, na.rm = TRUE),
    html_pct = round(100 * html_success / n(), 1),
    text_success = sum(!is.na(fulltext)),
    text_pct = round(100 * text_success / n(), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

print(faculty_stats, n = 25)

# NA URL analysis
na_urls <- uio_data %>% filter(is.na(url))
if (nrow(na_urls) > 0) {
  cat("\n\nCourses with NA URLs (faculty not in mapping):\n")
  na_faculty_table <- table(na_urls$Avdelingsnavn)
  print(na_faculty_table)
  cat("\nSample courses with NA URLs:\n")
  na_sample <- na_urls %>%
    select(Emnekode, Årstall, Semesternavn, Avdelingsnavn) %>%
    slice_head(n = 5)
  print(na_sample)
}

# Course code verification
cat("\n\nCourse code verification (for successful HTML fetches):\n")
verified <- uio_data %>%
  filter(!is.na(html)) %>%
  mutate(code_in_html = str_detect(html, fixed(toupper(Emnekode))))

code_verify_success <- sum(verified$code_in_html, na.rm = TRUE)
cat("Courses with code in HTML:", code_verify_success, "/",
    nrow(verified),
    sprintf(" (%.1f%%)\n", 100 * code_verify_success / nrow(verified)))

# Error analysis
if ("html_error" %in% names(uio_data)) {
  errors <- uio_data %>%
    filter(!is.na(html_error)) %>%
    count(html_error, sort = TRUE)

  if (nrow(errors) > 0) {
    cat("\n\nHTML fetch error distribution:\n")
    print(errors, n = 10)
  }
}

# Sample successful extractions
cat("\n\nSample successful courses:\n")
successful_sample <- uio_data %>%
  filter(!is.na(fulltext)) %>%
  mutate(text_length = nchar(fulltext)) %>%
  select(Emnekode, Årstall, Semesternavn, Avdelingsnavn, url, text_length) %>%
  slice_head(n = 10)

print(successful_sample, n = 10, width = 150)

# URL pattern analysis
cat("\n\nURL pattern analysis (sample):\n")
url_sample <- uio_data %>%
  filter(!is.na(url)) %>%
  select(Emnekode, Avdelingsnavn, url) %>%
  slice_head(n = 5)

print(url_sample, width = 150)

# Save summary statistics for markdown report
summary_stats <- list(
  total_courses = total_courses,
  url_success = url_success,
  url_pct = round(100 * url_success / total_courses, 1),
  html_success = html_success,
  html_pct = round(100 * html_success / total_courses, 1),
  text_success = text_success,
  text_pct = round(100 * text_success / total_courses, 1),
  text_length_min = if (text_success > 0) min(text_lengths) else NA,
  text_length_max = if (text_success > 0) max(text_lengths) else NA,
  text_length_mean = if (text_success > 0) round(mean(text_lengths)) else NA,
  text_length_median = if (text_success > 0) median(text_lengths) else NA,
  na_url_count = nrow(na_urls),
  code_verify_success = code_verify_success,
  code_verify_total = nrow(verified),
  faculty_stats = faculty_stats,
  year_table = year_table,
  semester_table = semester_table
)

saveRDS(summary_stats, "data/uio_validation_summary.RDS")

cat("\n\n=== Analysis Complete ===\n")
cat("Summary statistics saved to data/uio_validation_summary.RDS\n")
