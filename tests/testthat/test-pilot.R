# Tests for the pilot protocol: the sample (pilot_sample(), R/harvest.R) and
# the gate (gate_numbers(), R/pipeline_metrics.R)

# DBH offerings of one institution: 3 departments, course-years 2015-2025,
# each in both semesters
pilot_courses <- function(inst = "hivolda", n_codes = 60) {
  codes <- tibble::tibble(
    course_code_raw = sprintf("C%03d", seq_len(n_codes)),
    department_name = rep(c("Dept A", "Dept B", "Dept C"), length.out = n_codes),
    level_code = rep(c("LN", "LN", "HN", "LN", "FU", "LN"), length.out = n_codes),
    programme_name = ifelse(seq_len(n_codes) %% 10 == 0, "Videreutdanning i matematikk", "Bachelor"),
    language_code = ifelse(seq_len(n_codes) %% 4 == 0, "ENG", "NOB"))
  x <- tidyr::expand_grid(codes, year = 2015:2025, semester_name = c("Vår", "Høst"))
  x$institution <- inst
  x$status <- 1L
  x$course_code <- x$course_code_raw
  x <- x[c("institution", "year", "semester_name", "department_name", "programme_name",
           "course_code_raw", "course_code", "level_code", "status", "language_code")]
  attr(x, "dbh_snapshot") <- "2026-10-16"
  x
}

test_that("the level groups of the pilot sample", {
  expect_equal(pilot_level(c("LN", "HN", "FU", "ME", "LN", "VS", NA),
                           c("Bachelor", "Master", "Ph.d.", "Master", "Etter- og videreutdanning", "Forkurs", NA)),
               c("lower", "higher", "phd", "continuing", "continuing", "other", "other"))
})

test_that("the pilot sample: course-years of the pilot years, every stratum covered, reproducible", {
  courses <- pilot_courses()
  file <- withr::local_tempfile(fileext = ".csv")
  s <- pilot_sample("hivolda", oldest_year = 2016, n = 40, courses = courses, file = file)
  expect_named(s, names(courses))
  expect_equal(attr(s, "dbh_snapshot"), "2026-10-16")
  expect_setequal(unique(s$year), c(2016L, 2019L, 2022L, 2025L))
  # every semester of a sampled course-year
  expect_true(all(table(paste(s$course_code_raw, s$year)) == 2))
  units <- s[!duplicated(paste(s$course_code_raw, s$year)), ]
  expect_equal(nrow(units), 40)
  expect_true(all(table(units$year) %in% 10))   # equal numbers per year
  lv <- table(pilot_level(units$level_code, units$programme_name))
  expect_true(all(lv[c("lower", "higher", "phd", "continuing")] >= 5))
  expect_gte(sum(units$language_code == "ENG"), 10)
  expect_setequal(units$department_name, c("Dept A", "Dept B", "Dept C"))

  f <- read.csv(file, colClasses = "character")
  expect_named(f, c("institution", "course_id", "course_code_raw", "year", "semester_name",
                    "department_name", "level", "language", "dbh_snapshot", "seed"))
  expect_equal(f$course_id, add_course_id(s)$course_id)
  expect_equal(unique(f$dbh_snapshot), "2026-10-16")

  expect_identical(pilot_sample("hivolda", oldest_year = 2016, n = 40, courses = courses, file = NULL), s)
  expect_false(identical(pilot_sample("hivolda", oldest_year = 2016, n = 40, seed = 1,
                                      courses = courses, file = NULL), s))
})

test_that("pilot years before the oldest year, and offerings outside `within`, are left out", {
  courses <- pilot_courses()
  s <- pilot_sample("hivolda", oldest_year = 2020, n = 30, courses = courses, file = NULL)
  expect_setequal(unique(s$year), c(2020L, 2022L, 2025L))
  within <- add_course_id(courses[courses$course_code_raw %in% c("C001", "C002"), ])$course_id
  within <- sub("_1$", "_2", within)   # a later snapshot's status: matched on the offering
  s <- pilot_sample("hivolda", courses = courses, within = within, file = NULL)
  expect_setequal(s$course_code_raw, c("C001", "C002"))
  expect_equal(nrow(s), 2 * 3 * 2)   # 2 codes x 3 years x 2 semesters: fewer than n
})

test_that("a site that shows only the current plan is sampled in the latest DBH year", {
  s <- pilot_sample("uio", oldest_year = 2010, n = 20, courses = pilot_courses("uio"), file = NULL)
  expect_equal(unique(s$year), 2025L)
})

test_that("gate numbers count course-years of the sample in the built data", {
  # A: a plan with assessment and coursework; B: a plan without sections, one
  # semester not found; C: not found; D: a plan with an e-mail address left
  sample <- tibble::tibble(
    institution = "x", course_id = c("x_A_2025_autumn_1", "x_B_2025_spring_1", "x_B_2025_autumn_1",
                                     "x_C_2025_autumn_1", "x_D_2025_autumn_1"),
    course_code_raw = c("A", "B", "B", "C", "D"), year = 2025L)
  # the build has B's spring offering under another status
  ids <- c("x_A_2025_autumn_1", "x_B_2025_spring_2", "x_B_2025_autumn_1", "x_C_2025_autumn_1",
           "x_D_2025_autumn_1")
  coverage <- tibble::tibble(course_id = ids, outcome = c("ok", "not_found", "ok", "not_found", "ok"),
                             has_plan = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  offerings <- tibble::tibble(course_id = ids, course_code = c("A", "B", "B", "C", "D"),
                              plan_content_id = c("p1", NA, "p2", NA, "p3"),
                              course_plan = c("Plan A", NA, "Plan B", NA, "Kontakt: ola@uni.no"))
  sections <- tibble::tibble(institution = "x", plan_content_id = c("p1", "p1", "p3"),
                             course_code = c("A", "A", "D"),
                             section = c("assessment", "coursework_requirements", "assessment"),
                             text = c("Eksamen", "Arbeidskrav", "Eksamen"))
  page_map <- tibble::tibble(course_id = ids, page_id = c("x/1", NA, "x/2", NA, "x/3"))
  headings <- tibble::tibble(page_id = c("x/1", "x/1", "x/2", "x/3", "x/9"),
                             what = c("pattern", "unmapped", "pattern", "pattern", "unmapped"),
                             value = c("vurdering", "Kontakt", "innhold", "vurdering", "Other page"))
  g <- gate_numbers(sample, coverage, offerings, sections, headings, page_map,
                    thresholds = c(fetched = 0.8, plan = 0.8, assessment = 0.6, coursework = 0.3,
                                   unmapped_headings = 0.5, privacy_hits = 0))
  value <- setNames(g$value, g$metric)
  expect_equal(value[["fetched"]], 3 / 4)
  expect_equal(value[["plan"]], 3 / 4)
  expect_equal(value[["assessment"]], 2 / 3)
  expect_equal(value[["coursework"]], 1 / 3)
  expect_equal(value[["no_sections"]], 1)
  expect_equal(value[["unmapped_headings"]], 1 / 4)
  expect_equal(value[["privacy_hits"]], 1)
  expect_equal(g$n, c(4, 4, 3, 3, 3, 4, 3))
  expect_equal(g$review, c(TRUE, TRUE, FALSE, FALSE, NA, FALSE, TRUE))
  expect_equal(g$examples[g$metric == "plan"][[1]], "x_C_2025_autumn_1")
  expect_equal(g$examples[g$metric == "no_sections"][[1]], "x_B_2025_spring_1")
  expect_equal(g$examples[g$metric == "privacy_hits"][[1]], "x_D_2025_autumn_1")
  expect_equal(g$examples[g$metric == "unmapped_headings"][[1]], "Kontakt")
})
