# === Lage et lite utvalg av HTML/TXT fra alle institusjoner ===

sample_n <- 2   # hvor mange filer du vil ta per institusjon
out_root <- "data/output"
sample_dir <- file.path(out_root, "_sample_raw")
dir.create(sample_dir, showWarnings = FALSE)

inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[basename(inst_dirs) != "_sample_raw"]

for (inst_path in inst_dirs) {
  inst <- basename(inst_path)
  html_dir <- file.path(inst_path, "html_raw")
  txt_dir  <- file.path(inst_path, "txt_raw")
  
  if (dir.exists(html_dir) && dir.exists(txt_dir)) {
    html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
    txt_files  <- list.files(txt_dir,  pattern = "\\.txt$",  full.names = TRUE)
    
    if (length(html_files) > 0 && length(txt_files) > 0) {
      # velg sample_n tilfeldige eller forste sample_n
      sel_html <- head(html_files, sample_n)
      sel_txt  <- head(txt_files, sample_n)
      
      out_sub <- file.path(sample_dir, inst)
      dir.create(out_sub, showWarnings = FALSE)
      
      file.copy(sel_html, out_sub, overwrite = TRUE)
      file.copy(sel_txt,  out_sub, overwrite = TRUE)
      
      cat(sprintf("✓ Kopiert %d HTML og %d TXT fra %s\n",
                  length(sel_html), length(sel_txt), inst))
    }
  }
}

cat("\ Sampling done!!\n")
cat("Se mappen: ", normalizePath(sample_dir), "\n", sep = "")

