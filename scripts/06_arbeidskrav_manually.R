# ==========================================================
# TEPS: Arbeidskrav detection (full institutional scan)
# ----------------------------------------------------------
# Scans all txt_clean files under data/output/* using
# regex patterns from config/arbeidskrav.yaml
# ==========================================================

library(yaml)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# ----------------------------------------------------------
# 1. Setup paths
# ----------------------------------------------------------
config_file <- "config/arbeidskrav.yaml"
out_root    <- "data/output"
out_summary <- file.path(out_root, "_arbeidskrav_results")
dir.create(out_summary, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------
# 2. Load YAML patterns
# ----------------------------------------------------------
patterns <- yaml::read_yaml(config_file)

# Safe fallback function for NULL values
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper to merge global + institution-specific patterns
get_patterns <- function(inst, patterns) {
  include_global <- patterns$global$include
  exclude_global <- patterns$global$exclude
  include_inst   <- patterns[[inst]]$include %||% character(0)
  exclude_inst   <- patterns[[inst]]$exclude %||% character(0)
  include_all <- paste(c(include_global, include_inst), collapse = "|")
  exclude_all <- paste(c(exclude_global, exclude_inst), collapse = "|")
  list(include = include_all, exclude = exclude_all)
}

# ----------------------------------------------------------
# 3. Define arbeidskrav extraction function
# ----------------------------------------------------------
extract_arbeidskrav_yaml <- function(txt, inst, patterns) {
  pats <- get_patterns(inst, patterns)
  sentences <- unlist(str_split(txt, "(?<=[.!?])\\s+"))
  
  hits <- sentences[
    str_detect(sentences, pats$include) &
      !str_detect(sentences, pats$exclude)
  ]
  
  if (!length(hits)) return(NULL)
  
  tibble(
    arbeidskrav_text = hits,
    has_innlevering  = any(str_detect(hits, "(?i)(innlever|oppgave|mappe|tekst)")),
    has_prosjekt     = any(str_detect(hits, "(?i)(prosjekt|produksjon|forestilling|konsert)")),
    has_refleksjon   = any(str_detect(hits, "(?i)(refleksjon|analyse|notat|essay)")),
    has_praksis      = any(str_detect(hits, "(?i)(praksis|undervisningsopplegg|skolepraksis)")),
    has_presentasjon = any(str_detect(hits, "(?i)(presentasjon|framlegg|muntlig|oral)"))
  )
}

# ----------------------------------------------------------
# 4. Read all txt_clean files across institutions
# ----------------------------------------------------------
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[basename(inst_dirs) != "_arbeidskrav_results"]

read_file_safely <- function(f) {
  tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
           error = function(e) "")
}

texts <- map_df(inst_dirs, function(inst_path) {
  inst <- basename(inst_path)
  txt_dir <- file.path(inst_path, "txt_clean")
  if (!dir.exists(txt_dir)) return(NULL)
  txt_files <- list.files(txt_dir, pattern = "\\.txt$", full.names = TRUE)
  if (!length(txt_files)) return(NULL)
  tibble(
    institution = inst,
    file = txt_files,
    text = map_chr(txt_files, read_file_safely)
  )
})

cat("✅ Imported", nrow(texts), "files from", length(unique(texts$institution)), "institutions\n")

# ----------------------------------------------------------
# 5. Run arbeidskrav detection
# ----------------------------------------------------------
arbeidskrav_data <- texts %>%
  mutate(parsed = map2(text, institution, ~extract_arbeidskrav_yaml(.x, .y, patterns))) %>%
  filter(!map_lgl(parsed, is.null)) %>%
  unnest(parsed)

# ----------------------------------------------------------
# 6. Summarize results per institution + total file count
# ----------------------------------------------------------

# Count all txt_clean files (even those with 0 hits)
total_files_tbl <- texts %>%
  group_by(institution) %>%
  summarise(total_txt_files = n_distinct(file), .groups = "drop")

if (nrow(arbeidskrav_data) > 0) {
  summary_tbl <- arbeidskrav_data %>%
    group_by(institution) %>%
    summarise(
      n_files         = n_distinct(file),
      n_hits          = n(),
      n_innlevering   = sum(has_innlevering),
      n_prosjekt      = sum(has_prosjekt),
      n_refleksjon    = sum(has_refleksjon),
      n_praksis       = sum(has_praksis),
      n_presentasjon  = sum(has_presentasjon),
      .groups = "drop"
    ) %>%
    arrange(desc(n_hits)) %>%
    left_join(total_files_tbl, by = "institution") %>%
    relocate(total_txt_files, .after = n_files) %>%
    mutate(
      coverage_ratio = round(n_files / total_txt_files, 3)
    )
  
  print(summary_tbl, n = Inf)
  
  # Save results
  write.csv(summary_tbl, file.path(out_summary, "arbeidskrav_summary.csv"), row.names = FALSE)
  write.csv(arbeidskrav_data, file.path(out_summary, "arbeidskrav_detailed.csv"), row.names = FALSE)
  
  cat("✅ Results written to:", out_summary, "\n")
  
} else {
  cat("⚠️  No arbeidskrav found in dataset. Check regex patterns or file encoding.\n")
}



# Export detailed results to CSV
out_file <- file.path(out_summary, "arbeidskrav_detailed.csv")
write.csv(arbeidskrav_data, out_file, row.names = FALSE)

# Compress it (to make upload easier)
zip_file <- file.path(out_summary, "arbeidskrav_detailed.zip")
zip(zipfile = zip_file, files = out_file)

cat("✅ Exported and zipped:", zip_file, "\n")
