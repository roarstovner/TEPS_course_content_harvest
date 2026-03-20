  library(dplyr)
  source("R/utils.R")
  source("R/fetch_html_cols.R")
  source("R/extract_fulltext.R")
  source("R/add_course_url.R")
  source("R/checkpoint.R")
  source("R/resolve_course_urls.R")

  courses <- readRDS("data/courses.RDS")

  # Institutions ready to harvest (validated pipelines)
  harvest_list <- c("ntnu", "nord") #c("nord", "nih", "uib", "uit", "inn", "hvl", "mf", "hiof", "hivolda")

  # Institutions whose URLs have no year component — current content only
  no_year_insts <- c("mf")

  for (inst in harvest_list) {
    message("=== Starting: ", inst, " ===")

    df <- courses |>
      filter(institution_short == inst) |>
      (\(df) if (inst %in% no_year_insts) filter(df, Årstall == max(Årstall)) else df)() |>
      add_course_id() |>
      validate_courses("initial") |>
      add_course_url() |>
      resolve_course_urls(
        checkpoint_path = paste0("data/checkpoint/urls_", inst, ".RDS")
      ) |>
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
