# run_harvest_samas.R
# Harvest pipeline for Sámi allaskuvla (samas)
#
# Website: samas.no/se/oahput (Northern Sámi)
#
# Samas publishes course plans as PDFs ("Oahppoplána") linked from course
# pages. However, the website uses its own course codes (SÁM-1005, DUO-1014,
# SER 103, etc.) that do NOT correspond to DBH codes (V1SAM-1100-1,
# V1DUO-1140-1, etc.). The numbering systems are completely different, so
# reliable matching is not possible. Additionally, the V1/V5 teacher education
# courses (which make up ~98% of DBH rows) only appear in program-level PDFs
# ("PROGRÁMMAPLÁNA") that list course names and credits but contain no
# per-course descriptions.
#
# Result: all samas courses get fulltext = NA. The data file is saved with
# url and fulltext columns (both NA) for pipeline compatibility.

library(dplyr)
source("R/utils.R")

message("=== Starting: Sámi allaskuvla harvest ===")

# --- Step 1: Load courses from courses.RDS (current year only) ---
courses <- readRDS("data/courses.RDS") |>
  filter(institution_short == "samas", Årstall == max(Årstall)) |>
  add_course_id()

message("Loaded ", nrow(courses), " samas courses (",
        n_distinct(courses$Emnekode_raw), " unique codes)")

# --- Step 2: Set fulltext to NA ---
# No reliable matching between DBH course codes and website course codes.
# See chainlink issues #85, #89, #90 for investigation details.
courses$url <- NA_character_
courses$fulltext <- NA_character_

message("0/", nrow(courses), " rows with fulltext (no reliable matching available)")

# --- Step 3: Save ---
saveRDS(courses, "data/html_samas.RDS")

message("=== Done: Sámi allaskuvla - saved to data/html_samas.RDS ===")
message("All ", nrow(courses), " rows have fulltext = NA")
