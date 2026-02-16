#!/usr/bin/env Rscript

# Source required functions
source('R/utils.R')
source('R/add_course_url.R')
source('R/fetch_html_cols.R')
source('R/extract_fulltext.R')

# Load test courses
test_courses <- readRDS('data/test_courses.RDS')

cat('=== NIH Validation Pipeline ===\n\n')
cat('Starting with', nrow(test_courses), 'test courses\n\n')

# Step 1: Add course IDs
cat('Step 1: Adding course IDs...\n')
test_courses <- add_course_id(test_courses)
cat('Course IDs added.\n\n')

# Step 2: Generate URLs
cat('Step 2: Generating URLs...\n')
test_courses <- add_course_url(test_courses)

# Count successful URL generation
url_success <- sum(!is.na(test_courses$url))
cat('URLs generated:', url_success, '/', nrow(test_courses), '\n')

if (url_success > 0) {
  cat('\nSample URLs:\n')
  sample_idx <- head(which(!is.na(test_courses$url)), 3)
  for (i in sample_idx) {
    cat('  ', test_courses$url[i], '\n')
  }
}
cat('\n')

# Step 3: Fetch HTML
cat('Step 3: Fetching HTML...\n')
for (i in 1:nrow(test_courses)) {
  if (!is.na(test_courses$url[i])) {
    cat(sprintf('  [%2d/%2d] Fetching %s...', i, nrow(test_courses), test_courses$Emnekode_raw[i]))

    result <- tryCatch({
      fetch_html(test_courses$url[i])
    }, error = function(e) {
      list(html = NA, success = FALSE, error = as.character(e))
    })

    test_courses$html[i] <- result$html
    test_courses$html_success[i] <- result$success

    if (result$success) {
      cat(' OK\n')
    } else {
      cat(' FAILED\n')
      test_courses$html_error[i] <- if (!is.null(result$error)) result$error else 'Unknown error'
    }

    # Be polite
    Sys.sleep(0.5)
  } else {
    cat(sprintf('  [%2d/%2d] Skipping %s (no URL)\n', i, nrow(test_courses), test_courses$Emnekode_raw[i]))
  }
}

fetch_success <- sum(test_courses$html_success, na.rm = TRUE)
cat('\nHTML fetch success:', fetch_success, '/', url_success, '\n\n')

# Step 4: Verify page content (check if course code appears)
cat('Step 4: Verifying page content...\n')
test_courses$has_course_code <- FALSE
for (i in 1:nrow(test_courses)) {
  if (test_courses$html_success[i]) {
    # Check if course code (without version suffix) appears in HTML
    course_code_base <- sub('-.*$', '', test_courses$Emnekode_raw[i])
    test_courses$has_course_code[i] <- grepl(course_code_base, test_courses$html[i], ignore.case = TRUE)
  }
}

verified <- sum(test_courses$has_course_code, na.rm = TRUE)
cat('Pages with course code:', verified, '/', fetch_success, '\n\n')

# Step 5: Extract fulltext
cat('Step 5: Extracting fulltext...\n')
test_courses$fulltext <- extract_fulltext(test_courses$institution_short, test_courses$html)
test_courses$text_length <- nchar(test_courses$fulltext)

extraction_success <- sum(!is.na(test_courses$fulltext) & test_courses$fulltext != '', na.rm = TRUE)
cat('Fulltext extracted:', extraction_success, '/', fetch_success, '\n\n')

# Calculate statistics
if (extraction_success > 0) {
  valid_lengths <- test_courses$text_length[!is.na(test_courses$fulltext) & test_courses$fulltext != '']
  cat('Text length statistics (valid extractions):\n')
  cat('  Min:', min(valid_lengths), 'characters\n')
  cat('  Max:', max(valid_lengths), 'characters\n')
  cat('  Mean:', round(mean(valid_lengths)), 'characters\n')
  cat('  Median:', median(valid_lengths), 'characters\n')
}

# Save results
saveRDS(test_courses, 'data/nih_validation_results.RDS')
cat('\nResults saved to data/nih_validation_results.RDS\n')

# Summary report
cat('\n=== SUMMARY ===\n')
cat('Total courses tested:', nrow(test_courses), '\n')
cat('URL generation:', url_success, '/', nrow(test_courses),
    sprintf(' (%.0f%%)\n', 100 * url_success / nrow(test_courses)))
cat('HTML fetch:', fetch_success, '/', url_success,
    sprintf(' (%.0f%%)\n', 100 * fetch_success / url_success))
cat('Page verification:', verified, '/', fetch_success,
    sprintf(' (%.0f%%)\n', 100 * verified / fetch_success))
cat('Text extraction:', extraction_success, '/', fetch_success,
    sprintf(' (%.0f%%)\n', 100 * extraction_success / fetch_success))

# Show sample extraction
if (extraction_success > 0) {
  cat('\n=== SAMPLE EXTRACTION ===\n')
  sample_idx <- which(!is.na(test_courses$fulltext) & test_courses$fulltext != '')[1]
  cat('Course:', test_courses$Emnekode_raw[sample_idx], '\n')
  cat('Text length:', test_courses$text_length[sample_idx], 'characters\n')
  cat('First 500 characters:\n')
  cat(substr(test_courses$fulltext[sample_idx], 1, 500), '\n...\n')
}

cat('\nValidation complete!\n')
