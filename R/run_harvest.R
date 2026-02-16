  library(dplyr)
  source("R/utils.R")
  source("R/fetch_html_cols.R")
  source("R/extract_fulltext.R")
  source("R/add_course_url.R")
  source("R/checkpoint.R")

  courses <- readRDS("data/courses.RDS")

  # Institutions ready to harvest (validated pipelines)
  harvest_list <- c("nord", "nih", "uib", "uit", "inn",
  "hvl", "mf", "hiof")

  # Institutions whose URLs have no year component — current content only
  no_year_insts <- c("uit", "uib", "nord", "hvl", "mf")

  for (inst in harvest_list) {
    message("=== Starting: ", inst, " ===")

    df <- courses |>
      filter(institution_short == inst) |>
      { if (inst %in% no_year_insts) filter(., Årstall == max(Årstall)) else . }() |>
      add_course_id() |>
      validate_courses("initial") |>
      add_course_url() |>
      validate_courses("with_url")

    message(inst, ": ", sum(!is.na(df$url)), "/",
  nrow(df), " URLs generated")

    df <- fetch_html_with_checkpoint(
      df,
      checkpoint_path = paste0("data/checkpoint/html_",
  inst, ".RDS")
    )

    df$fulltext <-
  extract_fulltext(df$institution_short, df$html)
    saveRDS(df, file = paste0("data/html_", inst,
  ".RDS"))

    message("=== Done: ", inst, " - ",
  sum(!is.na(df$fulltext)), "/", nrow(df), " with
  fulltext ===\n")
  }

  # Institutions with non-standard pipelines (PDFs, URL discovery, etc.)
  source("R/run_harvest_steiner.R")