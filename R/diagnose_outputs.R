# R/diagnose_outputs.R
# --------------------
# Build triage tables from data/output/<inst>/{txt,html} and scrape_latest.csv

suppressPackageStartupMessages({
  library(stringr)
})

out_root <- file.path("data", "output")
stopifnot(dir.exists(out_root))

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

ensure_dir <- function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# TXT header reader (SAFE)
read_txt_header <- function(path_txt, max_lines = 200) {
  lines <- tryCatch(
    readLines(path_txt, n = max_lines, encoding = "UTF-8", warn = FALSE),
    error = function(e) character(0)
  )
  val <- function(key) {
    m <- regexpr(paste0("^", key, "\\s*:\\s*(.*)$"), lines, perl = TRUE)
    if (any(m > 0)) {
      i <- which(m > 0)[1]
      sub(paste0("^", key, "\\s*:\\s*"), "", lines[i], perl = TRUE)
    } else ""
  }
  block <- if (any(grepl("^----- MARKDOWN -----$", lines))) "MARKDOWN" else
    if (any(grepl("^----- FULLTEXT \\(HTML\\) -----$", lines))) "FULLTEXT_HTML" else
      if (any(grepl("^----- FETCH FAILED -----$", lines))) "FETCH_FAILED" else
        if (any(grepl("^----- NO CONTENT -----$", lines))) "NO_CONTENT" else "UNKNOWN"
  
  list(
    institution = val("institution"),
    code        = val("code"),
    year        = val("year"),
    hv          = val("hv"),
    url         = val("url"),
    status_line = val("status"),
    course_name = val("course_name_no"),
    block       = block
  )
}

# infer stub "<inst>_<code>_<year>_<hv>"
stub_from_txt <- function(path_txt) sub("\\.txt$", "", basename(path_txt), perl = TRUE)

# latest HTML matching "<stub>_YYYYMMDD-HHMM.html"
find_latest_html_for_stub <- function(dir_html, stub) {
  if (!dir.exists(dir_html)) return(NA_character_)
  patt <- paste0("^", str_replace_all(stub, "([.\\^$|()*+?{}\\[\\]\\\\])", "\\\\\\1"), "_\\d{8}-\\d{4}\\.html$")
  files <- list.files(dir_html, pattern = patt, full.names = TRUE, ignore.case = FALSE)
  if (!length(files)) return(NA_character_)
  files[which.max(file.mtime(files))]
}

# quick JSON sniff (client-rendered capture)
looks_like_json <- function(path_html, nbytes = 512) {
  if (!file.exists(path_html)) return(NA)
  raw <- readBin(path_html, "raw", n = nbytes)
  s <- rawToChar(raw); Encoding(s) <- "UTF-8"
  s <- gsub("^\\ufeff", "", s)   # strip BOM
  s <- trimws(s)
  startsWith(s, "{") || startsWith(s, "[")
}

# ------------------------------------------------------------
# Walk all institutions
# ------------------------------------------------------------
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
inst_dirs <- inst_dirs[dir.exists(file.path(out_root, inst_dirs))]
triage <- list()

for (inst in inst_dirs) {
  dir_inst <- file.path(out_root, inst)
  path_csv <- file.path(dir_inst, "scrape_latest.csv")
  dir_txt  <- file.path(dir_inst, "txt")
  dir_html <- file.path(dir_inst, "html")
  
  if (!file.exists(path_csv) || !dir.exists(dir_txt)) next
  
  df_status <- try(read.csv(path_csv, stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(df_status, "try-error")) next
  
  txts <- list.files(dir_txt, pattern = "\\.txt$", full.names = TRUE)
  if (!length(txts)) next
  
  cat("Scanning:", inst, " (", length(txts), " txt)\n")
  
  for (p in txts) {
    tryCatch({
      info   <- read_txt_header(p)
      size_b <- file.info(p)$size %||% NA_integer_
      stub   <- stub_from_txt(p)
      html_latest <- find_latest_html_for_stub(dir_html, stub)
      
      # status from scrape_latest.csv (match code+year+hv; fallback url)
      status_code <- NA_integer_
      if (nrow(df_status)) {
        cand <- subset(
          df_status,
          (!is.na(course_code_norm) & course_code_norm == info$code) &
            (!is.na(year) & as.character(year) == as.character(info$year)) &
            (!is.na(hv) & hv == info$hv)
        )
        if (!nrow(cand) && nzchar(info$url)) {
          cand <- subset(df_status, !is.na(url) & url == info$url)
        }
        if (nrow(cand)) status_code <- suppressWarnings(as.integer(cand$status[1]))
      }
      
      # content flags
      has_content_block <- info$block %in% c("MARKDOWN", "FULLTEXT_HTML")
      likely_textful    <- is.finite(size_b) && size_b >= 3000 && has_content_block
      
      # safe tail read to detect "NA" end in FULLTEXT_HTML
      safe_lines_400 <- tryCatch(
        readLines(p, n = 400L, encoding = "UTF-8", warn = FALSE),
        error = function(e) character(0)
      )
      na_tail <- any(grepl("^NA\\s*$", safe_lines_400, ignore.case = TRUE))
      
      ok200_but_empty <- isTRUE(status_code == 200) && (
        !has_content_block ||
          (identical(info$block, "FULLTEXT_HTML") && (is.na(size_b) || size_b < 3000 || na_tail)) ||
          (identical(info$block, "MARKDOWN")      && (is.na(size_b) || size_b < 3000))
      )
      
      fetch_failed <- identical(info$block, "FETCH_FAILED") || isTRUE(status_code >= 400)
      
      json_like <- if (!is.na(html_latest)) isTRUE(looks_like_json(html_latest)) else NA
      
      triage[[length(triage) + 1L]] <- data.frame(
        institution      = if (nzchar(info$institution)) info$institution else inst,
        code             = info$code,
        year             = info$year,
        hv               = info$hv,
        url              = info$url,
        status           = status_code,
        txt_path         = p,
        txt_bytes        = size_b,
        block            = info$block,
        html_path        = html_latest,
        html_json_like   = json_like,
        has_text         = likely_textful,
        ok200_but_empty  = ok200_but_empty,
        fetch_failed     = fetch_failed,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message(" [WARN] Skipping problematic TXT: ", p, " | ", conditionMessage(e))
    })
  }
}

triage_df <- if (length(triage)) do.call(rbind, triage) else data.frame()

# ------------------------------------------------------------
# Output triage CSVs
# ------------------------------------------------------------
triage_dir <- file.path("data", "triage")
ensure_dir(triage_dir)

write.csv(triage_df, file.path(triage_dir, "triage_all.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

tri_ok200_empty <- if (nrow(triage_df)) subset(triage_df, ok200_but_empty) else triage_df
write.csv(tri_ok200_empty, file.path(triage_dir, "triage_ok200_but_empty.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

tri_failed <- if (nrow(triage_df)) subset(triage_df, fetch_failed) else triage_df
write.csv(tri_failed, file.path(triage_dir, "triage_failed_or_non200.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

tri_has <- if (nrow(triage_df)) subset(triage_df, has_text) else triage_df
write.csv(tri_has, file.path(triage_dir, "triage_has_text.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Triage written to:", normalizePath(triage_dir, winslash = "/"), "\n")

# ------------------------------------------------------------
# OPTIONAL: copy files for quick manual inspection
# ------------------------------------------------------------
copy_set <- function(df, subdir) {
  if (!nrow(df)) return(invisible())
  tgt <- file.path(triage_dir, subdir)
  ensure_dir(tgt)
  for (i in seq_len(nrow(df))) {
    if (file.exists(df$txt_path[i])) {
      file.copy(df$txt_path[i], file.path(tgt, basename(df$txt_path[i])), overwrite = TRUE)
    }
    if (nzchar(df$html_path[i]) && !is.na(df$html_path[i]) && file.exists(df$html_path[i])) {
      file.copy(df$html_path[i], file.path(tgt, basename(df$html_path[i])), overwrite = TRUE)
    }
  }
  cat("Copied", nrow(df), "cases to", subdir, "\n")
}

copy_set(tri_ok200_empty, "ok200_but_empty")   # likely selector issues
copy_set(tri_failed,      "failed_or_non200")  # fetch issues
copy_set(tri_has,         "has_text")          # good references

