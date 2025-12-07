library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

# 1. Les inn alle kurs
courses_all <- readRDS("data/courses.RDS")

# 2. Velg institusjonene vi vil høste nå
inst_keep <- c(
  "hivolda",
  "hiof",
  "uio",
  "ntnu",
  "uia",
  "uit",
  "uib",
  "nord",
  "hvl"
)

courses <- courses_all |>
  filter(
    institution_short %in% inst_keep
    # evt. år:
    # Årstall %in% c(2017, 2018, 2019, 2020, 2021)
  ) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

# 3. Hent HTML med checkpoint (felles fil for alle ni)
courses <- fetch_html_with_checkpoint(
  courses,
  checkpoint_path = "data/checkpoint/html_9inst.RDS"
)

# 4. Ekstraher fulltekst
courses$fulltext <- extract_fulltext(courses$institution_short, courses$html)

# 5. Hvis du vil ha dem separat etterpå:
courses_hivolda <- dplyr::filter(courses, institution_short == "hivolda")
courses_hiof    <- dplyr::filter(courses, institution_short == "hiof")
courses_uio     <- dplyr::filter(courses, institution_short == "uio")
courses_ntnu    <- dplyr::filter(courses, institution_short == "ntnu")
courses_uia     <- dplyr::filter(courses, institution_short == "uia")
courses_uit     <- dplyr::filter(courses, institution_short == "uit")
courses_uib     <- dplyr::filter(courses, institution_short == "uib")
courses_nord    <- dplyr::filter(courses, institution_short == "nord")
courses_hvl     <- dplyr::filter(courses, institution_short == "hvl")

