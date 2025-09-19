# scripts/03_scrape.R
# ====================

suppressPackageStartupMessages({
  library(yaml)
  library(httr)
  library(xml2)
  library(rvest)
  library(digest)
  library(commonmark)
})

# Helpers that live in R/scrape_helpers.R:
# - ensure_dir(path)
# - load_selectors(inst, "config/selectors.yaml")
# - fetch_html(url, max_tries = 4L, dryrun = FALSE)
# - parse_fields(raw_html_bytes, selectors_for_inst)
# - ts_now()
# - safe_stub(inst, code, year, hv)
source(file.path("R", "scrape_helpers.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a

# -----------------------------------------------------------------------------
# Config via environment variables (robust defaults)
# -----------------------------------------------------------------------------
run_scrape     <- isTRUE(as.logical(Sys.getenv("TEPS_RUN_SCRAPE", "1")))
save_html      <- isTRUE(as.logical(Sys.getenv("TEPS_SAVE_HTML",  "TRUE")))
save_txt       <- isTRUE(as.logical(Sys.getenv("TEPS_SAVE_TXT",   "TRUE")))
dryrun         <- isTRUE(as.logical(Sys.getenv("TEPS_DRYRUN",     "FALSE")))
inst_env       <- Sys.getenv("TEPS_INST", "")
req_delay_ms   <- as.integer(Sys.getenv("TEPS_REQ_DELAY_MS", "0"))           # e.g., 300
max_pages_inst <- as.integer(Sys.getenv("TEPS_MAX_PAGES_PER_INST", "0"))     # 0 = unlimited

message(sprintf(
  "Starting 03_scrape.R  |  RUN=%s HTML=%s TXT=%s DRYRUN=%s INST=%s DELAY_MS=%s MAX_PAGES=%s",
  as.character(run_scrape), as.character(save_html), as.character(save_txt),
  as.character(dryrun), ifelse(nzchar(inst_env), inst_env, "<all>"),
  as.character(req_delay_ms), as.character(max_pages_inst)
))

if (!run_scrape) {
  message("TEPS_RUN_SCRAPE=0 -> skipping scraping.")
  message("Done: scripts/03_scrape.R")
  invisible(return())
}

# -----------------------------------------------------------------------------
# Safe IO helpers (UTF-8)
# -----------------------------------------------------------------------------
safe_read_csv <- function(path) {
  tr <- try(read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE), silent = TRUE)
  if (!inherits(tr, "try-error")) return(tr)
  read.csv(path, stringsAsFactors = FALSE)
}

safe_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

safe_writeLines <- function(txt, path) {
  ensure_dir(dirname(path))
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeLines(txt, con, useBytes = TRUE)
  on.exit(NULL, add = FALSE)
}

# Small write-probe to detect permission/OneDrive issues early
ensure_dir(file.path("data","output"))
probe_path <- file.path("data","output","_write_probe.txt")
probe_ok <- try({
  con <- file(probe_path, open = "w", encoding = "UTF-8")
  writeLines(paste("probe:", Sys.time()), con, useBytes = TRUE)
  close(con)
  TRUE
}, silent = TRUE)
message("Write-probe to data/output: ", if (identical(probe_ok, TRUE)) "OK" else "FAILED")

# -----------------------------------------------------------------------------
# Find institutions to scrape
# -----------------------------------------------------------------------------
out_root  <- file.path("data", "output")
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
has_urls  <- function(inst) file.exists(file.path(out_root, inst, "course_urls_latest.csv"))
inst_all  <- inst_dirs[sapply(inst_dirs, has_urls)]

if (nzchar(inst_env)) {
  sel <- unique(trimws(tolower(strsplit(inst_env, ",", fixed = TRUE)[[1]])))
  inst_all <- intersect(inst_all, sel)
}
if (!length(inst_all)) stop("No institutions to scrape (missing course_urls_latest.csv).")

# -----------------------------------------------------------------------------
# Load selectors once
# -----------------------------------------------------------------------------
selectors_all <- lapply(inst_all, function(inst) try(load_selectors(inst, "config/selectors.yaml"), silent = TRUE))
names(selectors_all) <- inst_all

# -----------------------------------------------------------------------------
# Main loop
# -----------------------------------------------------------------------------
grand_ok <- 0L
grand_fail <- 0L

for (inst in inst_all) {
  start_time_inst <- Sys.time()
  cat("\n==========================\n>>> Scraper:", inst, "\n==========================\n")
  
  path_csv <- file.path(out_root, inst, "course_urls_latest.csv")
  urls_df  <- try(safe_read_csv(path_csv), silent = TRUE)
  if (inherits(urls_df, "try-error")) { message("  Could not read ", path_csv); next }
  
  # Minimal required columns (allow hv to be missing)
  required_min <- c("course_code_norm", "year", "url")
  miss <- setdiff(required_min, names(urls_df))
  if (length(miss)) {
    message("  Skipping: missing columns in ", path_csv, " -> ", paste(miss, collapse = ", "))
    next
  }
  if (!("hv" %in% names(urls_df))) urls_df$hv <- ""
  
  sel <- selectors_all[[inst]]
  if (inherits(sel, "try-error") || is.null(sel)) {
    message("  Error/no selectors in config/selectors.yaml for ", inst)
    next
  }
  
  # Directories
  dir_inst <- file.path(out_root, inst)
  dir_html <- file.path(dir_inst, "html"); ensure_dir(dir_html)
  dir_txt  <- file.path(dir_inst, "txt");  ensure_dir(dir_txt)
  
  # Result buffer
  res <- data.frame(
    institution_short = inst,
    course_code_norm  = urls_df$course_code_norm,
    year              = urls_df$year,
    hv                = urls_df$hv,
    url               = urls_df$url,
    status            = NA_integer_,
    ok                = FALSE,
    bytes             = NA_integer_,
    sha1              = NA_character_,
    course_name_no    = NA_character_,
    fulltext          = NA_character_,   # Markdown primarily; HTML fallback if MD is empty
    stringsAsFactors  = FALSE
  )
  
  count <- 0L
  for (i in seq_len(nrow(urls_df))) {
    count <- count + 1L
    if (max_pages_inst > 0L && count > max_pages_inst) break
    
    u   <- urls_df$url[i]
    cod <- urls_df$course_code_norm[i]
    yr  <- urls_df$year[i]
    hvv <- urls_df$hv[i]
    
    # Fetch HTML
    f <- fetch_html(u, max_tries = 4L, dryrun = dryrun)
    res$status[i] <- f$status
    res$ok[i]     <- isTRUE(f$ok)
    
    # Prepare defaults for parse results
    md <- ""
    parsed <- list(course_name_no = NA_character_, fulltext = "")
    
    if (isTRUE(f$ok) && length(f$content)) {
      res$bytes[i] <- length(f$content)
      res$sha1[i]  <- digest::digest(f$content, algo = "sha1", serialize = FALSE)
      
      # Save raw HTML (optional)
      if (save_html) {
        stub <- safe_stub(inst, cod, yr, hvv)
        fn_html <- file.path(dir_html, paste0(stub, "_", ts_now(), ".html"))
        con_html <- file(fn_html, open = "wb")
        on.exit(try(close(con_html), silent = TRUE), add = TRUE)
        writeBin(f$content, con_html)
        on.exit(NULL, add = FALSE)
      }
      
      # Parse with guard; if error -> keep defaults
      parsed_try <- try(parse_fields(f$content, sel), silent = TRUE)
      if (!inherits(parsed_try, "try-error") && !is.null(parsed_try)) {
        parsed <- parsed_try
      } else {
        warning(sprintf("[parse_fields] failed for %s", u))
      }
      
      # Try HTML -> Markdown conversion
      if (is.character(parsed$fulltext) && nzchar(parsed$fulltext)) {
        md_try <- try(commonmark::html_to_md(parsed$fulltext), silent = TRUE)
        if (!inherits(md_try, "try-error") && is.character(md_try) && nzchar(md_try)) {
          md <- md_try
        }
      }
    }
    
    res$course_name_no[i] <- parsed$course_name_no
    res$fulltext[i] <- if (nzchar(md)) md else (parsed$fulltext %||% "")
    
    # ALWAYS write one .txt per URL when TEPS_SAVE_TXT=1
    if (save_txt) {
      stub   <- safe_stub(inst, cod, yr, hvv)
      fn_txt <- file.path(dir_txt, paste0(stub, ".txt"))
      header <- paste0(
        "institution: ", inst, "\n",
        "code: ", cod, "\n",
        "year: ", yr, "\n",
        "hv: ", hvv, "\n",
        "url: ", u, "\n",
        "status: ", ifelse(is.na(f$status), "", f$status), "\n",
        "course_name_no: ", ifelse(is.na(parsed$course_name_no), "", parsed$course_name_no), "\n"
      )
      
      if (nzchar(md)) {
        safe_writeLines(c(header, "----- MARKDOWN -----", md), fn_txt)
      } else if (is.character(parsed$fulltext) && nzchar(parsed$fulltext)) {
        safe_writeLines(c(header, "----- FULLTEXT (HTML) -----", parsed$fulltext), fn_txt)
      } else if (!isTRUE(f$ok) || !length(f$content)) {
        safe_writeLines(c(header, "----- FETCH FAILED -----"), fn_txt)
      } else {
        safe_writeLines(c(header, "----- NO CONTENT -----"), fn_txt)
      }
    }
    
    # Delay between requests
    if (req_delay_ms > 0L) Sys.sleep(req_delay_ms / 1000)
    
    # Progress + ETA every 50 courses
    if (i %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time_inst, units = "secs"))
      avg_per_page <- elapsed / i
      remaining <- (nrow(urls_df) - i) * avg_per_page
      eta_min <- round(remaining / 60, 1)
      cat(sprintf("  %s: %d/%d pages fetched | ETA approx. %s min remaining\n",
                  inst, i, nrow(urls_df), eta_min))
    }
    
    # Periodic gzip to keep dirs small
    if (save_html && i %% 200 == 0) {
      invisible(gzip_html_dir(dir_html, keep_last = 1L))
    }
  }
  
  # Write CSVs
  ts <- ts_now()
  csv_ts     <- file.path(dir_inst, sprintf("scrape_%s.csv", ts))
  csv_latest <- file.path(dir_inst, "scrape_latest.csv")
  safe_write_csv(res, csv_ts)
  safe_write_csv(res, csv_latest)
  
  # Compress old HTML to save space (keep last per course)
  if (save_html) invisible(gzip_html_dir(dir_html, keep_last = 1L))
  
  ok_n   <- sum(res$ok, na.rm = TRUE)
  fail_n <- nrow(res) - ok_n
  
  # HTTP status summary
  stat_tbl <- sort(table(res$status), decreasing = TRUE)
  if (length(stat_tbl)) cat("  HTTP status: ", paste(names(stat_tbl), stat_tbl, sep = "x", collapse = ", "), "\n")
  
  # Failure log (useful operationally)
  fail_df <- subset(res, is.na(ok) | !ok | is.na(status) | status != 200)
  if (nrow(fail_df)) {
    safe_write_csv(fail_df, file.path(dir_inst, sprintf("scrape_fail_%s.csv", ts_now())))
  }
  
  grand_ok   <- grand_ok + ok_n
  grand_fail <- grand_fail + fail_n
  
  cat("  Done:", inst, "| OK:", ok_n, "| FAIL:", fail_n, "\n")
  cat("  Output:\n   ", gsub("^.+data/", "data/", csv_latest), "\n")
}

cat("\nSummary: OK =", grand_ok, "| FAIL =", grand_fail, "\n")
message("Done: scripts/03_scrape.R")
