# ============================================================
# (QA Utility) Lag et lite utvalg av HTML/TXT fra alle institusjoner
# ============================================================

sample_n <- 2   # hvor mange filer du vil ta per institusjon
out_root <- "data/output"
sample_dir <- file.path(out_root, "_sample_clean")
dir.create(sample_dir, showWarnings = FALSE, recursive = TRUE)

inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[basename(inst_dirs) != "_sample_clean"]

for (inst_path in inst_dirs) {
  inst <- basename(inst_path)
  html_dir <- file.path(inst_path, "html_raw")
  txt_dir  <- file.path(inst_path, "txt_clean")   # <-- endret fra txt_raw
  
  if (dir.exists(html_dir) && dir.exists(txt_dir)) {
    html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
    txt_files  <- list.files(txt_dir,  pattern = "\\.txt$",  full.names = TRUE)
    
    if (length(html_files) > 0 && length(txt_files) > 0) {
      # velg sample_n tilfeldige eller forste sample_n
      sel_html <- head(sample(html_files, min(sample_n, length(html_files))), sample_n)
      sel_txt  <- head(sample(txt_files,  min(sample_n, length(txt_files))),  sample_n)
      
      out_sub <- file.path(sample_dir, inst)
      dir.create(out_sub, showWarnings = FALSE)
      
      file.copy(sel_html, out_sub, overwrite = TRUE)
      file.copy(sel_txt,  out_sub, overwrite = TRUE)
      
      cat(sprintf("✓ Kopiert %d HTML og %d TXT fra %s\n",
                  length(sel_html), length(sel_txt), inst))
    } else {
      cat(sprintf("⚠️  Ingen filer funnet for %s\n", inst))
    }
  } else {
    cat(sprintf("⚠️  Mangler mappe for %s\n", inst))
  }
}

cat("\n📦 Sampling done!\n")
cat("Se mappen: ", normalizePath(sample_dir), "\n", sep = "")

