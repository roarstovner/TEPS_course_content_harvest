# ============================================================
# 99_check_outputs.R
# TEPS output verification tool
# Checks all institutions' html_raw/txt_raw folders vs. course_urls_latest.csv
# ============================================================

# --- Sjekk output-mapper for alle institusjoner ---
out_root <- "data/output"
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)

check_dirs <- function(path) {
  html_dir <- file.path(path, "html_raw")
  txt_dir  <- file.path(path, "txt_raw")
  
  html_files <- if (dir.exists(html_dir)) length(list.files(html_dir, pattern = "\\.html$", full.names = TRUE)) else 0
  txt_files  <- if (dir.exists(txt_dir))  length(list.files(txt_dir,  pattern = "\\.txt$",  full.names = TRUE)) else 0
  
  data.frame(
    institution = basename(path),
    html_dir_exists = dir.exists(html_dir),
    html_files = html_files,
    txt_dir_exists = dir.exists(txt_dir),
    txt_files = txt_files,
    html_empty = html_files == 0,
    txt_empty  = txt_files == 0,
    stringsAsFactors = FALSE
  )
}

res <- do.call(rbind, lapply(inst_dirs, check_dirs))
print(res, row.names = FALSE)


## --- Samlet sjekk: HTML/TXT vs. antall URLer per institusjon ---
out_root <- "data/output"
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)

results <- data.frame(
  institution = character(),
  urls = integer(),
  html_files = integer(),
  txt_files = integer(),
  html_missing = integer(),
  txt_missing = integer(),
  stringsAsFactors = FALSE
)

for (inst in inst_dirs) {
  csv_path <- file.path(out_root, inst, "course_urls_latest.csv")
  html_dir <- file.path(out_root, inst, "html_raw")
  txt_dir  <- file.path(out_root, inst, "txt_raw")
  
  if (!file.exists(csv_path)) next
  
  urls <- nrow(read.csv(csv_path, stringsAsFactors = FALSE))
  html_files <- if (dir.exists(html_dir)) length(list.files(html_dir, "\\.html$", full.names = TRUE)) else 0
  txt_files  <- if (dir.exists(txt_dir))  length(list.files(txt_dir,  "\\.txt$",  full.names = TRUE)) else 0
  
  results <- rbind(results, data.frame(
    institution = inst,
    urls = urls,
    html_files = html_files,
    txt_files = txt_files,
    html_missing = max(0, urls - html_files),
    txt_missing = max(0, urls - txt_files),
    stringsAsFactors = FALSE
  ))
}

# --- Legg til TRUE/FALSE om alt er OK ---
results$ok_html <- results$html_missing == 0
results$ok_txt  <- results$txt_missing == 0

# --- Print tabell ---
print(results, row.names = FALSE)

# --- Kort sammendrag ---
cat("\nInstitusjoner med manglende filer:\n")
print(subset(results, !ok_html | !ok_txt))


