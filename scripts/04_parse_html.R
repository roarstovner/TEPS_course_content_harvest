# ============================================================
# scripts/04_parse_html.R
# Driver: parse all institutions quietly, with compact progress.
# - Reads selectors from config/parse_sections.yaml (your selector map)
# - Reads global parsing rules from config/default.yaml
# - Uses parse_html_generic(file, cfg)
# - Writes full text to CSV (no truncation) + per-file .txt
# ============================================================

library(yaml)
library(rvest)
library(xml2)
library(stringr)

source("R/parse_html_generic.R")

`%||%` <- function(a, b) if (is.null(a)) b else a
ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
clean_basename <- function(path) {
  base <- basename(tools::file_path_sans_ext(path))
  sub("_[0-9]{8}-[0-9]{4}$", "", base)
}

# ---- load YAML ----------------------------------------------------
selectors_cfg <- yaml::read_yaml("config/parse_sections.yaml")  # your per-institution selectors
global_cfg    <- yaml::read_yaml("config/default.yaml")         # keep_after, stop_before, sections, min_chars

# fallback selectors if an institution is missing in YAML
fallback_sel <- list(
  selector_main    = "main, article, #content, .region-content, .page-content",
  selector_exclude = c("header","footer","nav","aside",".breadcrumb",".site-header",".menu",".sidebar")
)

build_cfg <- function(inst) {
  sel <- selectors_cfg[[inst]] %||% fallback_sel
  list(
    selector_main    = sel$selector_main    %||% fallback_sel$selector_main,
    selector_exclude = sel$selector_exclude %||% fallback_sel$selector_exclude,
    keep_after  = global_cfg$keep_after  %||% list(),
    stop_before = global_cfg$stop_before %||% list(),
    sections    = global_cfg$sections    %||% list(),
    min_chars   = global_cfg$min_chars   %||% 400
  )
}

# ---- institutions -------------------------------------------------
out_root  <- file.path("data", "output")
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
inst_dirs <- inst_dirs[!inst_dirs %in% c("_aggregated", "_sample_raw")]

# optional: limit via TEPS_INST="hiof,usn"
inst_env <- Sys.getenv("TEPS_INST", "")
if (nzchar(inst_env)) {
  sel <- unique(trimws(tolower(strsplit(inst_env, ",", fixed = TRUE)[[1]])))
  inst_dirs <- intersect(inst_dirs, sel)
}
if (!length(inst_dirs)) stop("No institutions found to parse.")

# tame warning printing to avoid 'last.warning' noise
owarn <- getOption("warn")
on.exit(options(warn = owarn), add = TRUE)
options(warn = 0)

# ---- run ----------------------------------------------------------
agg_rows <- list()
cat("\n=== 04_parse_html: starting ===\n")

for (inst in inst_dirs) {
  cat("\n--- Institution:", inst, "---\n")
  
  inst_root <- file.path(out_root, inst)
  html_dir  <- file.path(inst_root, "html_raw")
  out_dir   <- file.path(inst_root, "txt_clean")
  ensure_dir(out_dir)
  
  files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
  if (!length(files)) {
    cat("No HTML files found for", inst, "\n")
    next
  }
  
  cfg <- build_cfg(inst)
  total <- length(files)
  t0 <- Sys.time()
  
  # counters
  success <- 0L; skipped_main <- 0L; skipped_short <- 0L; failed <- 0L
  
  inst_rows <- vector("list", total)
  write_idx <- 0L
  
  cat(sprintf("Found %d files. Parsing… ", total)); flush.console()
  
  for (i in seq_along(files)) {
    f <- files[[i]]
    base <- clean_basename(f)
    out_path <- file.path(out_dir, paste0(base, ".txt"))
    
    # parse (fully quiet)
    txt <- try(suppressWarnings(parse_html_generic(f, cfg)), silent = TRUE)
    
    if (inherits(txt, "try-error")) { failed <- failed + 1L; next }
    if (!nzchar(txt)) {
      # differentiate: empty main vs too short (best-effort guess)
      # look for any plausible main first, to attribute reason
      doc  <- suppressWarnings(xml2::read_html(f))
      main <- try(.pick_main(doc, cfg$selector_main), silent = TRUE)
      if (inherits(main, "try-error") || is.na(main) || !length(main)) {
        skipped_main <- skipped_main + 1L
      } else {
        skipped_short <- skipped_short + 1L
      }
      next
    }
    
    # write .txt
    writeLines(txt, out_path, useBytes = TRUE)
    success <- success + 1L
    
    # store row with FULL text
    write_idx <- write_idx + 1L
    # Hent kurskode fra filnavn 
    course_code <- sub("_.*$", "", basename(out_path))
    
    # fra course_urls_latest.csv hvis det finnes
    url_file <- file.path(inst_root, "course_urls_latest.csv")
    if (file.exists(url_file)) {
      urls <- try(read.csv(url_file, header = TRUE), silent = TRUE)
      if (!inherits(urls, "try-error") && "course_code" %in% names(urls) && "url" %in% names(urls)) {
        url_match <- urls$url[match(course_code, urls$course_code)]
      } else {
        url_match <- NA
      }
    } else {
      url_match <- NA
    }
    
    # Legg til rad i riktig format
    inst_rows[[write_idx]] <- data.frame(
      institution      = inst,
      course_code      = course_code,
      url              = url_match,
      fulltekst_renset = txt,
      arbeidskrav      = NA,          # placeholder, kan fylles i senere
      status_code      = NA,          # fra scraping 
      stringsAsFactors = FALSE
    )
    
    
    # compact progress: dot per 10 files
    if (i %% 10L == 0L) { cat("."); flush.console() }
  }
  
  # trim list
  inst_rows <- inst_rows[seq_len(write_idx)]
  
  if (write_idx > 0L) {
    inst_df <- do.call(rbind, inst_rows)
    utils::write.csv(inst_df, file.path(inst_root, "courses_clean.csv"), row.names = FALSE)
    agg_rows[[length(agg_rows) + 1L]] <- inst_df
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat("\n")
    cat(sprintf(
      "✅ %s: %d/%d ok (fail %d, no-main %d, too-short %d) — %.2f min\n",
      inst, success, total, failed, skipped_main, skipped_short, elapsed
    ))
  } else {
    cat("\n")
    cat(sprintf("⚠️  %s: No successful parses (fail %d, no-main %d, too-short %d)\n",
                inst, failed, skipped_main, skipped_short))
  }
}

# aggregate
if (length(agg_rows)) {
  all_df <- do.call(rbind, agg_rows)
  agg_dir <- file.path(out_root, "_aggregated")
  ensure_dir(agg_dir)
  out_csv <- file.path(agg_dir, "courses_clean_all.csv")
  utils::write.csv(all_df, out_csv, row.names = FALSE)
  cat(sprintf("\n=== ✅ Done. Aggregated %d rows → %s ===\n", nrow(all_df), out_csv))
} else {
  cat("\n=== ⚠️ Done. Nothing aggregated. ===\n")
}

