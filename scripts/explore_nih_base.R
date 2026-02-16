#!/usr/bin/env Rscript

# Load courses
courses <- readRDS('data/courses.RDS')

# Filter for NIH
nih_courses <- courses[courses$institution_short == 'nih', ]

# Show summary
cat('Total NIH courses:', nrow(nih_courses), '\n')
cat('\nYear distribution:\n')
print(table(nih_courses$year))
cat('\nSemester distribution:\n')
print(table(nih_courses$semester))
cat('\nStatus distribution:\n')
print(table(nih_courses$status))

# Show unique course codes
cat('\nUnique course codes:', length(unique(nih_courses$Emnekode_raw)), '\n')

# Select diverse test set (20 courses)
# Strategy: Sample across years and semesters
set.seed(42)

# Get samples from different year/semester combinations
test_courses <- data.frame()
year_sem_combos <- unique(nih_courses[, c('year', 'semester')])

for (i in 1:nrow(year_sem_combos)) {
  y <- year_sem_combos$year[i]
  s <- year_sem_combos$semester[i]
  subset_data <- nih_courses[nih_courses$year == y & nih_courses$semester == s, ]

  if (nrow(subset_data) > 0) {
    n_sample <- min(2, nrow(subset_data))
    sampled <- subset_data[sample(nrow(subset_data), n_sample), ]
    test_courses <- rbind(test_courses, sampled)
  }

  if (nrow(test_courses) >= 20) break
}

# Limit to 20
test_courses <- test_courses[1:min(20, nrow(test_courses)), ]

# Order by year, semester
test_courses <- test_courses[order(test_courses$year, test_courses$semester, test_courses$Emnekode_raw), ]

cat('\n\nTest set (', nrow(test_courses), ' courses):\n')
cat('Year distribution:\n')
print(table(test_courses$year))
cat('\nSemester distribution:\n')
print(table(test_courses$semester))

# Save test courses
saveRDS(test_courses, 'data/test_courses.RDS')

# Display test courses
cat('\n\nSelected test courses:\n')
print(test_courses[, c('year', 'semester', 'Emnekode_raw', 'status')])
