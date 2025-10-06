# ============================================================
# scripts/05_qacheck.R
# Kvalitetskontroll: sjekker om .txt_clean inneholder
# sentrale seksjoner (Laeringsutbytte, Arbeidskrav, Vurdering, ...)
# ============================================================

library(stringr)

out_root <- "data/output"
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[!basename(inst_dirs) %in% c("_aggregated", "_sample_raw", "_sample_clean")]

keywords <- c("Læringsutbytte", "Arbeidskrav", "Vurdering", "Undervisning", "Pensum")
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
    
    found <- sum(str_detect(txt, regex(keywords, ignore_case = TRUE)))
    if (found == 0) {
      cat(sprintf("⚠️  %s → muligens for smal selector\n", basename(f)))
      report <- rbind(report, data.frame(institution = inst, file = basename(f),
                                         status = "Mangler hovedseksjoner",
                                         stringsAsFactors = FALSE))
    } else {
      report <- rbind(report, data.frame(institution = inst, file = basename(f),
                                         status = "OK", stringsAsFactors = FALSE))
    }
  }
}

# summary
summary_tbl <- aggregate(file ~ institution + status, report, length)
print(summary_tbl)

write.csv(report, file.path(out_root, "_aggregated", "qa_section_check.csv"), row.names = FALSE)
cat("\n✅ Rapport lagret i data/output/_aggregated/qa_section_check.csv\n")
