# ============================================================
# archive_parse_outputs.R
# Flytter gamle parse-outputs til arkivmappe i stedet for å slette dem.
# Beholder html_raw/ og txt_raw/, men flytter txt_clean/ og courses_clean.csv
# til data/archive/YYYYMMDD_HHMM/
# ============================================================

out_root <- "data/output"
archive_root <- "data/archive"

if (!dir.exists(archive_root)) dir.create(archive_root, recursive = TRUE)

# lag tidsstempel for arkiv
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
archive_dir <- file.path(archive_root, paste0("parse_backup_", timestamp))
dir.create(archive_dir, recursive = TRUE)

dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
dirs <- dirs[!basename(dirs) %in% c("_sample_raw", "_sample_clean")]

cat("=== 🗂️  Flytter eksisterende parse-output til arkiv ===\n")
cat("Arkivmappe:", archive_dir, "\n\n")

for (d in dirs) {
  inst <- basename(d)
  inst_archive <- file.path(archive_dir, inst)
  dir.create(inst_archive, recursive = TRUE)
  
  # flytt txt_clean
  clean_dir <- file.path(d, "txt_clean")
  if (dir.exists(clean_dir)) {
    dest_clean <- file.path(inst_archive, "txt_clean")
    file.rename(clean_dir, dest_clean)
    cat(sprintf("📦 Flyttet: %s → %s\n", clean_dir, dest_clean))
  }
  
  # flytt per-institusjon CSV
  csv_file <- file.path(d, "courses_clean.csv")
  if (file.exists(csv_file)) {
    dest_csv <- file.path(inst_archive, basename(csv_file))
    file.rename(csv_file, dest_csv)
    cat(sprintf("📦 Flyttet: %s → %s\n", csv_file, dest_csv))
  }
}

# flytt aggregert mappe
agg_dir <- file.path(out_root, "_aggregated")
if (dir.exists(agg_dir)) {
  dest_agg <- file.path(archive_dir, "_aggregated")
  file.rename(agg_dir, dest_agg)
  cat(sprintf("\n📦 Flyttet aggregert mappe → %s\n", dest_agg))
}

cat("\n✅ Ferdig! Alle tidligere parse-resultater er trygt arkivert.\n")

