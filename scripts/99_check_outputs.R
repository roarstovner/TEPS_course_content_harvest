# ============================================================
# 99_check_outputs.R
# TEPS output verification tool
# Sjekker alle institusjoners html_raw/txt_raw mapper
# mot course_urls_latest.csv
# ------------------------------------------------------------
# Skriptet gir:
#  - Antall URL-er, HTML-filer og TXT-filer per institusjon
#  - Antall manglende filer
#  - Kort terminalrapport og CSV-rapport
# ============================================================

options(stringsAsFactors = FALSE)

out_root <- "data/output"
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
inst_dirs <- inst_dirs[!inst_dirs %in% c("_aggregated", "_sample_raw", "_sample_clean")]

# Helper: teller filer i en mappe
count_files <- function(dir, pattern) {
  if (dir.exists(dir)) length(list.files(dir, pattern = pattern, full.names = TRUE)) else 0
}

results <- data.frame(
  institution = character(),
  urls = integer(),
  html_files = integer(),
  txt_files = integer(),
  html_missing = integer(),
  txt_missing = integer(),
  ok_html = logical(),
  ok_txt = logical(),
  stringsAsFactors = FALSE
)

cat("=== 🧩 TEPS Output Verification ===\n")

for (inst in inst_dirs) {
  csv_path <- file.path(out_root, inst, "course_urls_latest.csv")
  html_dir <- file.path(out_root, inst, "html_raw")
  txt_dir  <- file.path(out_root, inst, "txt_raw")
  
  if (!file.exists(csv_path)) {
    cat(sprintf("⚠️  %s → ingen course_urls_latest.csv funnet\n", inst))
    next
  }
  
  urls <- tryCatch(nrow(read.csv(csv_path, header = TRUE)), error = function(e) 0)
  html_files <- count_files(html_dir, "\\.html$")
  txt_files  <- count_files(txt_dir,  "\\.txt$")
  
  html_missing <- max(0, urls - html_files)
  txt_missing  <- max(0, urls - txt_files)
  
  ok_html <- html_missing == 0
  ok_txt  <- txt_missing == 0
  
  results <- rbind(results, data.frame(
    institution = inst,
    urls = urls,
    html_files = html_files,
    txt_files = txt_files,
    html_missing = html_missing,
    txt_missing = txt_missing,
    ok_html = ok_html,
    ok_txt = ok_txt,
    stringsAsFactors = FALSE
  ))
}

# --- Lagre resultat ---
agg_dir <- file.path(out_root, "_aggregated")
if (!dir.exists(agg_dir)) dir.create(agg_dir, recursive = TRUE)

out_csv <- file.path(agg_dir, "qa_scrape_check.csv")
write.csv(results, out_csv, row.names = FALSE)

# --- Terminalrapport ---
cat("\n=== 📊 Sammendrag ===\n")

total_insts <- nrow(results)
ok_insts <- sum(results$ok_html & results$ok_txt)
warn_insts <- total_insts - ok_insts

cat(sprintf("Totalt institusjoner: %d\n", total_insts))
cat(sprintf("✅ Fullt komplette:   %d\n", ok_insts))
cat(sprintf("⚠️  Mangler filer:    %d\n", warn_insts))

if (warn_insts > 0) {
  cat("\nInstitusjoner med mangler:\n")
  print(subset(results, !ok_html | !ok_txt)[, c("institution", "urls", "html_files", "txt_files", "html_missing", "txt_missing")], row.names = FALSE)
}

cat("\n✅ Rapport lagret til:", out_csv, "\n")
