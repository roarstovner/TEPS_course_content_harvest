#!/usr/bin/env Rscript
# One-time migration: rename fulltext → extracted_text and
# fulltext_normalized → course_plan_normalized in all RDS data files.
#
# Run from project root: Rscript tmp/rename_rds_columns.R

rename_cols <- function(df) {
  nms <- names(df)
  nms[nms == "fulltext"] <- "extracted_text"
  nms[nms == "fulltext_normalized"] <- "course_plan_normalized"
  names(df) <- nms
  df
}

# --- html_*.RDS files (per-institution harvest output) ---
html_files <- list.files("data", pattern = "^html_.*\\.RDS$", full.names = TRUE)
for (f in html_files) {
  df <- readRDS(f)
  if ("fulltext" %in% names(df) || "fulltext_normalized" %in% names(df)) {
    df <- rename_cols(df)
    saveRDS(df, f)
    cat("Renamed columns in", basename(f), "\n")
  } else {
    cat("No rename needed:", basename(f), "\n")
  }
}

# --- courses_with_plan_id.RDS ---
cpid_path <- "data/courses_with_plan_id.RDS"
if (file.exists(cpid_path)) {
  df <- readRDS(cpid_path)
  if ("fulltext" %in% names(df) || "fulltext_normalized" %in% names(df)) {
    df <- rename_cols(df)
    saveRDS(df, cpid_path)
    cat("Renamed columns in courses_with_plan_id.RDS\n")
  } else {
    cat("No rename needed: courses_with_plan_id.RDS\n")
  }
}

# --- plan_lookup.RDS ---
plan_path <- "data/plan_lookup.RDS"
if (file.exists(plan_path)) {
  df <- readRDS(plan_path)
  if ("fulltext_normalized" %in% names(df)) {
    df <- rename_cols(df)
    saveRDS(df, plan_path)
    cat("Renamed columns in plan_lookup.RDS\n")
  } else {
    cat("No rename needed: plan_lookup.RDS\n")
  }
}

# --- Checkpoint files (html checkpoints may have fulltext from PDF strategy) ---
cp_files <- list.files("data/checkpoint", pattern = "\\.RDS$", full.names = TRUE)
for (f in cp_files) {
  df <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df)) next
  if ("fulltext" %in% names(df) || "fulltext_normalized" %in% names(df)) {
    df <- rename_cols(df)
    saveRDS(df, f)
    cat("Renamed columns in", basename(f), "\n")
  }
}

cat("\nDone.\n")
