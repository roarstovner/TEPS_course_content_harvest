# Tests for normalize_plan_text.R and deduplicate_plans.R

source(here::here("R/normalize_plan_text.R"))
source(here::here("R/deduplicate_plans.R"))

# --- normalize_plan_text ---

test_that("NA and empty input returns NA", {
  expect_equal(normalize_plan_text(NA_character_), NA_character_)
  expect_equal(normalize_plan_text(""), NA_character_)
})

test_that("vectorized input works", {
  result <- normalize_plan_text(c("Some course text", NA_character_))
  expect_length(result, 2)
  expect_type(result, "character")
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
})

test_that("tolower is applied", {
  expect_equal(normalize_plan_text("KUNNSKAP om Faget"), "kunnskap om faget")
})

test_that("KUNNSKAP and Kunnskap normalize identically", {
  a <- normalize_plan_text("KUNNSKAP om faget")
  b <- normalize_plan_text("Kunnskap om faget")
  expect_equal(a, b)
})

test_that("whitespace is squished", {
  expect_equal(normalize_plan_text("  Mye   ekstra    mellomrom  "), "mye ekstra mellomrom")
})

test_that("eksamensformer replaced with vurderingsformer", {
  result <- normalize_plan_text("Eksamensformer og vurdering")
  expect_false(grepl("eksamensformer", result))
  expect_true(grepl("vurderingsformer", result))
})

test_that("season words are removed (lossy)", {
  result <- normalize_plan_text("Undervisning Vår og Høst og Haust")
  expect_false(grepl("vår|høst|haust", result))
  expect_true(grepl("undervisning", result))
})

test_that("vår meaning 'our' is removed in normalize (lossy is expected)", {
  result <- normalize_plan_text("Vår tilnærming til pedagogikk")
  expect_false(grepl("vår", result))
  expect_true(grepl("tilnærming til pedagogikk", result))
})

test_that("english season words removed", {
  result <- normalize_plan_text("Teaching in autumn and spring")
  expect_false(grepl("autumn|spring", result))
  expect_true(grepl("teaching in", result))
})

test_that("sommer/summer removed", {
  result <- normalize_plan_text("Sommerkurs sommer og summer")
  expect_false(grepl("\\bsommer\\b", result))
  expect_false(grepl("\\bsummer\\b", result))
  # "sommerkurs" should remain (not a standalone word boundary match)
  expect_true(grepl("sommerkurs", result))
})

# --- build_plan_id ---

test_that("same input produces same hash", {
  expect_equal(build_plan_id("hello"), build_plan_id("hello"))
})

test_that("different input produces different hash", {
  expect_false(build_plan_id("hello") == build_plan_id("world"))
})

test_that("NA input returns NA", {
  expect_true(is.na(build_plan_id(NA_character_)))
})

test_that("empty string returns NA", {
  expect_true(is.na(build_plan_id("")))
})

test_that("vectorized hashing works", {
  ids <- build_plan_id(c("a", "b", NA, "a"))
  expect_length(ids, 4)
  expect_equal(ids[1], ids[4])
  expect_true(is.na(ids[3]))
  expect_false(ids[1] == ids[2])
})

# --- deduplicate_plans ---

test_that("deduplicate_plans returns correct structure", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    course_plan = c("Same plan text", "Same plan text", "Updated plan", "Updated plan")
  )

  result <- deduplicate_plans(df)

  expect_type(result, "list")
  expect_named(result, c("plans", "courses"))
  expect_s3_class(result$plans, "tbl_df")
  expect_s3_class(result$courses, "tbl_df")
})

test_that("deduplicate_plans reduces identical plans", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    course_plan = c("Same plan text", "Same plan text", "Updated plan", "Updated plan")
  )

  result <- deduplicate_plans(df)

  # Should have 2 unique plans, not 4
  expect_equal(nrow(result$plans), 2)
  # Courses should keep all 4 rows with plan_content_id
  expect_equal(nrow(result$courses), 4)
  expect_true("plan_content_id" %in% names(result$courses))
})

test_that("deduplicate_plans computes correct year ranges", {
  df <- tibble::tibble(
    institution_short = rep("hivolda", 4),
    Emnekode = rep("TEST101", 4),
    Årstall = c(2020, 2021, 2022, 2023),
    course_plan = c("Plan A", "Plan A", "Plan B", "Plan B")
  )

  result <- deduplicate_plans(df)
  plans <- result$plans |> dplyr::arrange(year_from)

  expect_equal(plans$year_from, c(2020, 2022))
  expect_equal(plans$year_to, c(2021, 2023))
})

test_that("deduplicate_plans handles NA course_plan", {
  df <- tibble::tibble(
    institution_short = c("hivolda", "hivolda"),
    Emnekode = c("TEST101", "TEST102"),
    Årstall = c(2020, 2020),
    course_plan = c("Real plan", NA_character_)
  )

  result <- deduplicate_plans(df)

  # Plan lookup should only have the non-NA one
  expect_equal(nrow(result$plans), 1)
  # Courses should have both rows
  expect_equal(nrow(result$courses), 2)
  expect_true(is.na(result$courses$plan_content_id[2]))
})

test_that("deduplicate_plans errors on missing columns", {
  df <- tibble::tibble(x = 1)
  expect_error(deduplicate_plans(df))
})
