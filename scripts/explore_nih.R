#!/usr/bin/env Rscript
library(dplyr)
library(tidyr)

# Load courses
courses <- readRDS('data/courses.RDS')

# Filter for NIH
nih_courses <- courses |>
  filter(institution_short == 'nih')

# Show summary
cat('Total NIH courses:', nrow(nih_courses), '\n')
cat('\nYear distribution:\n')
print(table(nih_courses$year))
cat('\nSemester distribution:\n')
print(table(nih_courses$semester))
cat('\nStatus distribution:\n')
print(table(nih_courses$status))

# Show unique course codes
cat('\nUnique course codes:', n_distinct(nih_courses$Emnekode_raw), '\n')

# Select diverse test set (20 courses)
# Strategy: Sample across years, semesters, and course codes
set.seed(42)
test_courses <- nih_courses |>
  group_by(year, semester) |>
  slice_sample(n = 2, replace = FALSE) |>
  ungroup() |>
  slice_head(n = 20) |>
  arrange(year, semester, Emnekode_raw)

cat('\n\nTest set (20 courses):\n')
cat('Year distribution:\n')
print(table(test_courses$year))
cat('\nSemester distribution:\n')
print(table(test_courses$semester))

# Save test courses
saveRDS(test_courses, 'data/test_courses.RDS')

# Display test courses
cat('\n\nSelected test courses:\n')
test_courses |>
  select(year, semester, Emnekode_raw, status) |>
  print(n = 20)
