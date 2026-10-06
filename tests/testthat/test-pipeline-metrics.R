# Tests for R/pipeline_metrics.R (#255)

test_that("pipeline_metrics counts offerings, plans and sections per institution", {
  offerings <- tibble::tibble(
    course_id = c("1", "2", "3"),
    institution = "a", Emnekode_raw = c("X", "X", "Y"), Årstall = 2025,
    extracted_text = c("t", NA, "u"), course_plan = c("abc", NA, "abcde"),
    plan_content_id = c("p1", NA, "p2")
  )
  sections <- tibble::tibble(
    plan_content_id = c("p1", "p1", "p2"), institution = "a", Emnekode = c("X", "X", "Y"),
    source_course_id = c("1", "1", "3"),
    section = c("assessment", "reading_list", "assessment"),
    text = c("xx", "yyyy", "zz")
  )
  m <- pipeline_metrics(offerings, sections)
  value <- function(section, metric) m$value[m$section == section & m$metric == metric]
  expect_equal(value("(all)", "n_rows"), 3)
  expect_equal(value("(all)", "n_with_plan"), 2)
  expect_equal(value("(all)", "n_course_years_with_plan"), 2)
  expect_equal(value("(all)", "n_plans_with_sections"), 2)
  expect_equal(value("assessment", "n_plans"), 2)
  expect_equal(value("reading_list", "median_chars"), 4)
  # plan 1: 6 of 3 chars (capped at 100 %), plan 3: 2 of 5 -> median 70
  expect_equal(value("(all)", "pct_text_in_sections"), 70)
})

test_that("drops, vanished and new metrics are flagged; small changes are not", {
  old <- tibble::tribble(
    ~institution, ~section,     ~metric,           ~value,
    "nla",        "assessment", "n_courses",       142,
    "uia",        "(all)",      "n_rows",          2881,
    "uia",        "(all)",      "median_chars",    4000,
    "uis",        "(all)",      "n_with_sections", 1600,
    "usn",        "(all)",      "median_chars",    3000
  )
  new <- tibble::tribble(
    ~institution, ~section,     ~metric,           ~value,
    "uia",        "(all)",      "n_rows",          2875,   # -6 rows
    "uia",        "(all)",      "median_chars",    3000,   # -25%
    "uis",        "(all)",      "n_with_sections", 1400,   # -12.5%
    "usn",        "(all)",      "median_chars",    2900,   # -3%
    "uit",        "(all)",      "n_with_text",     3000    # new
  )
  changes <- compare_metrics(old, new)
  expect_setequal(paste(changes$institution, changes$metric),
                  c("nla n_courses", "uia median_chars",
                    "uis n_with_sections", "uit n_with_text"))
})

test_that("the built data match the metrics snapshot", {
  files <- here::here(c("data/interim/course_offerings_full.RDS", "data/processed/plan_sections.RDS"))
  skip_if_not(all(file.exists(files)), "built data not available")
  changes <- suppressMessages(check_pipeline_metrics(
    snapshot = here::here(METRICS_SNAPSHOT),
    offerings = files[1], sections = files[2]
  ))
  expect(nrow(changes) == 0, paste0(
    "Built data differ from the metrics snapshot. If intended, run ",
    "check_pipeline_metrics(update = TRUE) and commit the snapshot:\n",
    paste(utils::capture.output(print(as.data.frame(changes))), collapse = "\n")
  ))
})

test_that("short plans without sections are page shells: no plan (#222)", {
  plans <- list(list(
    plans = tibble::tibble(plan_content_id = c("p1", "p2", "p3"), institution = "a",
                           Emnekode = c("X", "Y", "Z"),
                           course_plan = c("Emnebeskrivelse", strrep("a", 2000), "Innhold")),
    courses = tibble::tibble(course_id = c("1", "2", "3", "4"), institution = "a",
                             Emnekode = c("X", "Y", "Z", "X"), extracted_text = "t",
                             plan_content_id = c("p1", "p2", "p3", "p1"))))
  sections <- tibble::tibble(plan_content_id = "p3", institution = "a", Emnekode = "Z",
                             section = "course_content", text = "Innhold")
  # p1: short, no sections -> shell; p2: long, no sections -> kept; p3: has sections
  expect_equal(combine_plans(plans, sections)$plan_content_id, c("p2", "p3"))
  off <- combine_offerings(plans, sections)
  expect_equal(off$plan_content_id, c(NA, "p2", "p3", NA))
  expect_true(all(off$has_extracted_text))
})

test_that("plan_gaps() gives each course-year without a plan its reason (#296 follow-up)", {
  offerings <- tibble::tibble(
    course_id = c("a24", "a25s", "a25h", "a26", "b25"), institution = "x",
    Emnekode = c("A", "A", "A", "A", "B"), Årstall = c(2024, 2025, 2025, 2026, 2025),
    plan_content_id = c("p1", NA, NA, "p2", NA))
  status <- tibble::tibble(
    course_id = offerings$course_id, url = c("u24", "u25", "u25", "u26", NA),
    has_page = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    fetch_error = c(NA, "HTTP 404 Not Found.", "HTTP 500", NA, NA))
  g <- plan_gaps(offerings, status)
  a25 <- g[g$Emnekode == "A" & g$Årstall == 2025, ]
  expect_false(a25$has_plan)
  expect_equal(a25$reason, "404 + fetch error")
  expect_true(a25$between_plans)
  expect_equal(a25$url_next_door, "u26")
  expect_equal(g$reason[g$Emnekode == "B"], "no URL")
  expect_false(g$between_plans[g$Emnekode == "B"])
})
