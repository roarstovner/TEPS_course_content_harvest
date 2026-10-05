# build_data.R — Build Parquet files for the OJS course-plan browser POC.
#
# Reads the published RDS datasets and emits two zstd-compressed Parquet files
# into ./data/ for the Quarto OJS page (index.qmd) to load via FileAttachment.
#
# The RDS files are the source of truth; the Parquet here is a regenerable build
# artifact (gitignored), rebuilt by targets::tar_make() (target ojs_data_files).
#
# Run from this directory:  Rscript build_data.R

suppressPackageStartupMessages(library(dplyr))

# --- Parquet writer (arrow preferred, nanoparquet fallback) ------------------
# NOTE: nanoparquet 0.5.1's "zstd" codec is a silent no-op (output == uncompressed),
# so the nanoparquet path uses "gzip" (~3.5x on this text). arrow, if installed,
# gets real zstd (a bit smaller). The browser's parquet reader handles both.
write_parquet_compressed <- function(df, path) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(df, path, compression = "zstd")
  } else if (requireNamespace("nanoparquet", quietly = TRUE)) {
    nanoparquet::write_parquet(df, path, compression = "gzip")
  } else {
    stop("Need the 'arrow' or 'nanoparquet' package installed to write Parquet.")
  }
}

# --- Paths -------------------------------------------------------------------
data_in  <- "../../data/processed"
data_out <- "data"
dir.create(data_out, showWarnings = FALSE)

offerings <- readRDS(file.path(data_in, "course_offerings.RDS"))
plans     <- readRDS(file.path(data_in, "course_plans.RDS"))

# --- Per-plan enrichment from offerings --------------------------------------
# A plan_content_id maps to many offerings (same plan reused across years and
# semesters). Pull a representative course name and an offering count; the plans
# table itself has no human-readable Emnenavn.
plan_meta <- offerings |>
  filter(!is.na(plan_content_id)) |>
  group_by(plan_content_id) |>
  summarise(
    Emnenavn = {
      nm <- Emnenavn[!is.na(Emnenavn)]
      if (length(nm)) nm[[1]] else NA_character_
    },
    n_offerings = n(),
    .groups = "drop"
  )

# --- plans.parquet: the searchable corpus ------------------------------------
# Integer year/count columns keep Parquet as INT32 -> JS number (avoids BigInt).
# course_plan_normalized is dropped; only the anonymized course_plan is shipped.
plans_out <- plans |>
  left_join(plan_meta, by = "plan_content_id") |>
  transmute(
    plan_content_id   = as.character(plan_content_id),
    institution       = as.character(institution),
    Emnekode          = as.character(Emnekode),
    Emnenavn          = as.character(Emnenavn),
    year_from         = as.integer(year_from),
    year_to           = as.integer(year_to),
    n_offerings       = as.integer(coalesce(n_offerings, 0L)),
    course_plan       = as.character(course_plan)
  )

# --- offerings.parquet: slim metadata for the "used by" panel ----------------
offerings_out <- offerings |>
  filter(!is.na(plan_content_id)) |>
  transmute(
    plan_content_id   = as.character(plan_content_id),
    institution       = as.character(institution),
    Emnekode_raw      = as.character(Emnekode_raw),
    Emnenavn          = as.character(Emnenavn),
    year              = as.integer(Årstall),
    semester          = as.character(Semesternavn),
    status            = as.character(Statusnavn)
  )

# --- Write -------------------------------------------------------------------
write_parquet_compressed(plans_out,     file.path(data_out, "plans.parquet"))
write_parquet_compressed(offerings_out, file.path(data_out, "offerings.parquet"))

# --- Report ------------------------------------------------------------------
report <- function(path, df) {
  size_mb <- file.info(path)$size / 1e6
  cat(sprintf("  %-20s %7d rows  %6.1f MB\n", basename(path), nrow(df), size_mb))
}
cat("Wrote Parquet to ", normalizePath(data_out), ":\n", sep = "")
report(file.path(data_out, "plans.parquet"), plans_out)
report(file.path(data_out, "offerings.parquet"), offerings_out)
