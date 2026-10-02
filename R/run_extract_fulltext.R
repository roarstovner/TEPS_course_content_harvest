# run_extract_fulltext.R
# Rebuilds extracted_text from the raw harvest (data/html_{inst}.RDS) with the
# current selectors and pre/post functions in R/institution_config.R, without
# writing to the raw files (#256). Writes data/extracted_text.RDS
# (course_id, institution, extracted_text), which run_dedup.R and
# run_extract_sections.R read via read_harvest(). Prints, per institution, how
# many rows differ from the text stored at harvest time.
#
#   Rscript R/run_extract_fulltext.R           # all institutions
#   Rscript R/run_extract_fulltext.R uit uis   # only these; their rows are
#                                              # replaced in extracted_text.RDS

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")       # institution_config depends on fetch_fn refs
source("R/extract_fulltext.R")
source("R/institution_config.R")

only <- commandArgs(trailingOnly = TRUE)
out_file <- "data/extracted_text.RDS"

files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
institutions <- sub("^html_(.*)\\.RDS$", "\\1", basename(files))
if (length(only)) institutions <- intersect(institutions, only)

texts <- purrr::map(institutions, function(inst) {
  df <- readRDS(file.path("data", paste0("html_", inst, ".RDS")))
  cat(sprintf("  %-8s %5d rows\n", inst, nrow(df)))
  text <- extract_fulltext_from_raw(df, get_institution_config(inst))
  changed <- sum(!mapply(identical, text, df$extracted_text, USE.NAMES = FALSE))
  if (changed > 0) {
    cat(sprintf("           %d rows differ from the text stored at harvest time\n",
                changed))
  }
  tibble::tibble(course_id = df$course_id, institution = inst,
                 extracted_text = text)
})
extracted <- bind_rows(texts)

if (length(only) && file.exists(out_file)) {
  extracted <- readRDS(out_file) |>
    filter(!institution %in% institutions) |>
    bind_rows(extracted)
}
saveRDS(extracted, out_file)
cat(sprintf("\nSaved %d rows (%d with text) to %s\n", nrow(extracted),
            sum(!is.na(extracted$extracted_text)), out_file))
