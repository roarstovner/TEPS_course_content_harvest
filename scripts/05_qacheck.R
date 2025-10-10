# ============================================================
# scripts/05_qacheck.R
# Kvalitetskontroll: sjekker om txt_clean inneholder
# sentrale seksjoner (Læringsutbytte, Arbeidskrav, Vurdering, ...)
# samt at tekstlengden er fornuftig (> min_chars)
# ============================================================

library(stringr)

out_root <- "data/output"
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[!basename(inst_dirs) %in% c("_aggregated", "_sample_raw", "_sample_clean")]

keywords <- c("Læringsutbytte", "Arbeidskrav", "Vurdering", "Undervisning", "Pensum")
min_chars <- 400L
report <- data.frame()

cat("=== QA-seksjonssjekk ===\n")

for (inst_path in inst_dirs) {
  inst <- basename(inst_path)
  txt_dir <- file.path(inst_path, "txt_clean")
  files <- list.files(txt_dir, pattern = "\\.txt$", full.names = TRUE)
  if (!length(files)) next
  
  cat(sprintf("\n[%s] Checking %d files...\n", inst, length(files)))
  
  for (f in files) {
    txt <- try(readLines(f, encoding = "UTF-8"), silent = TRUE)
    if (inherits(txt, "try-error")) next
    txt <- paste(txt, collapse = " ")
    n_chars <- nchar(txt)
    
    found <- sum(str_detect(txt, regex(keywords, ignore_case = TRUE)))
    status <- dplyr::case_when(
      n_chars < min_chars ~ "For kort / mulig feil selector",
      found == 0 ~ "Mangler hovedseksjoner",
      TRUE ~ "OK"
    )
    
    if (status != "OK") {
      cat(sprintf("⚠️  %s → %s\n", basename(f), status))
    }
    
    report <- rbind(report, data.frame(
      institution = inst,
      file = basename(f),
      n_chars = n_chars,
      found_sections = found,
      status = status,
      stringsAsFactors = FALSE
    ))
  }
}

# Sammendrag per institusjon
summary_tbl <- aggregate(file ~ institution + status, report, length)
print(summary_tbl)

# Lagre detaljer og sammendrag
agg_dir <- file.path(out_root, "_aggregated")
if (!dir.exists(agg_dir)) dir.create(agg_dir, recursive = TRUE)
write.csv(report, file.path(agg_dir, "qa_section_check_detailed.csv"), row.names = FALSE)
write.csv(summary_tbl, file.path(agg_dir, "qa_section_summary.csv"), row.names = FALSE)

cat("\n✅ Rapporter lagret i data/output/_aggregated/:\n")
cat("   - qa_section_check_detailed.csv\n")
cat("   - qa_section_summary.csv\n")
