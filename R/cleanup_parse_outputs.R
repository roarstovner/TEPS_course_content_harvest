### cleanup_parse_outputs.R ###

out_root <- "data/output"

# behold alt i html_raw/ og txt_raw/, men fjern clean- og csv-filer
dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
dirs <- dirs[basename(dirs) != "_sample_raw"]

for (d in dirs) {
  # slett txt_clean
  clean_dir <- file.path(d, "txt_clean")
  if (dir.exists(clean_dir)) {
    unlink(clean_dir, recursive = TRUE, force = TRUE)
    cat("Removed:", clean_dir, "\n")
  }
  
  # slett per-institusjon CSV
  csv_file <- file.path(d, "courses_clean.csv")
  if (file.exists(csv_file)) {
    file.remove(csv_file)
    cat("Removed:", csv_file, "\n")
  }
}

# slett aggregert mappe
agg_dir <- file.path(out_root, "_aggregated")
if (dir.exists(agg_dir)) {
  unlink(agg_dir, recursive = TRUE, force = TRUE)
  cat("Removed:", agg_dir, "\n")
}

