# Tests for data/study_types.csv: DBH's study types (Studiumkode, table 347)
# with English names and the groups of the browser's study-type tree

study_types <- function() {
  read.csv(here::here("data/study_types.csv"), encoding = "UTF-8", check.names = FALSE)
}

test_that("every study type in the latest programmes snapshot has one row", {
  file <- tryCatch(dbh_snapshot_file("programmes", input_dir = here::here(INPUT_DIR)),
                   error = function(e) NULL)
  skip_if(is.null(file), "no DBH programmes snapshot")
  codes <- unique(arrow::read_parquet(file, col_select = "Studiumkode")$Studiumkode)
  st <- study_types()
  expect_false(anyDuplicated(st$study_type) > 0)
  expect_setequal(st$study_type, codes)
})

test_that("study types have a group from the tree, labels and names", {
  st <- study_types()
  expect_true(all(st$group %in% c("teacher_education", "health_welfare", "law_police_military",
                                  "technology_engineering", "arts_design_architecture",
                                  "primary_industries", "other_professional", "broad_fields")))
  # one label per group
  expect_equal(nrow(unique(st[c("group", "group_label_en", "group_label_no")])), length(unique(st$group)))
  expect_false(any(is.na(st$name_en) | st$name_en == "" | is.na(st$name_no) | st$name_no == ""))
  expect_true(all(st$first_year <= st$last_year))
})

test_that("te_frame marks the study types of the teacher-ed frame", {
  st <- study_types()
  expect_type(st$te_frame, "logical")
  expect_equal(st$te_frame, st$study_type %in% TEACHER_ED_STUDIUM)
  expect_true(all(st$group[st$te_frame] == "teacher_education"))
})
