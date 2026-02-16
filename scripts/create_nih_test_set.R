#!/usr/bin/env Rscript

# Load courses
courses <- readRDS('data/courses.RDS')

# Filter for NIH using which()
idx <- which(courses$institution_short == 'nih')
nih_courses <- courses[idx, ]

# Show summary
cat('Total NIH courses:', nrow(nih_courses), '\n')

# Year column is Årstall (with encoding issues, use column index)
year_col <- which(names(nih_courses) == names(courses)[7])
semester_col <- which(names(nih_courses) == 'Semester')
status_col <- which(names(nih_courses) == 'Status')

cat('\nYear distribution:\n')
print(table(nih_courses[, year_col]))
cat('\nSemester distribution:\n')
print(table(nih_courses[, semester_col]))
cat('\nStatus distribution:\n')
print(table(nih_courses[, status_col]))

# Show unique course codes
cat('\nUnique course codes:', length(unique(nih_courses$Emnekode_raw)), '\n')
cat('\nSample course codes:\n')
print(head(unique(nih_courses$Emnekode_raw), 20))

# Select diverse test set (20 courses)
# Create a year-semester grouping key
year_vals <- nih_courses[, year_col]
semester_vals <- nih_courses[, semester_col]
nih_courses$year_sem_key <- paste(year_vals, semester_vals, sep = '_')

# Get unique combinations
combos <- unique(nih_courses$year_sem_key)

set.seed(42)
test_courses <- data.frame()

for (combo in combos) {
  subset_idx <- which(nih_courses$year_sem_key == combo)
  n_sample <- min(2, length(subset_idx))
  sampled_idx <- sample(subset_idx, n_sample)
  test_courses <- rbind(test_courses, nih_courses[sampled_idx, ])

  if (nrow(test_courses) >= 20) break
}

# Limit to 20
test_courses <- test_courses[1:min(20, nrow(test_courses)), ]

# Order by year, semester
test_courses <- test_courses[order(test_courses[, year_col], test_courses[, semester_col], test_courses$Emnekode_raw), ]

cat('\n\nTest set (', nrow(test_courses), ' courses):\n')
cat('Year distribution:\n')
print(table(test_courses[, year_col]))
cat('\nSemester distribution:\n')
print(table(test_courses[, semester_col]))

# Save test courses
saveRDS(test_courses, 'data/test_courses.RDS')

# Display test courses
cat('\n\nSelected test courses:\n')
for (i in 1:nrow(test_courses)) {
  cat(sprintf('%2d. %4d  Sem:%d  Code:%-15s  Status:%d\n',
              i,
              test_courses[i, year_col],
              test_courses[i, semester_col],
              test_courses$Emnekode_raw[i],
              test_courses[i, status_col]))
}

cat('\nTest courses saved to data/test_courses.RDS\n')
