# scripts/03_scrape.R
# ====================
# Scraper for TEPS pipeline
# - Reads course_urls latest.csv per institution
# - Fetches HTML, parses fields with selectors.yaml
# - Saves HTML (optional), TXT (optional), CSV results

suppressPackageStartupMessages({
  library(yaml)
  library(httr)
  library(xml2)
  library(rvest)
  library(digest)
  library(commonmark)
})

source(file.path("R", "scrape_helpers.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a))) b else a

# -----------------------------------------------------------------------------
# Config via environment variables (use TRUE/FALSE only)
# -----------------------------------------------------------------------------
RUN_SCRAPE     <- tolower(Sys.getenv("TEPS_RUN_SCRAPE", "TRUE"))  %in% c("true","1","yes","y")
SAVE_HTML      <- tolower(Sys.getenv("TEPS_SAVE_HTML",  "TRUE"))  %in% c("true","1","yes","y")
SAVE_TXT       <- tolower(Sys.getenv("TEPS_SAVE_TXT",   "TRUE"))  %in% c("true","1","yes","y")
DRYRUN         <- tolower(Sys.getenv("TEPS_DRYRUN",     "FALSE")) %in% c("true","1","yes","y")
INST_ENV       <- Sys.getenv("TEPS_INST", "")
REQ_DELAY_MS   <- as.integer(Sys.getenv("TEPS_REQ_DELAY_MS", "0"))
MAX_PAGES_INST <- as.integer(Sys.getenv("TEPS_MAX_PAGES_PER_INST", "0"))

message(sprintf(
  "Starting 03_scrape.R  |  RUN=%s HTML=%s TXT=%s DRYRUN=%s INST=%s DELAY_MS=%s MAX_PAGES=%s",
  RUN_SCRAPE, SAVE_HTML, SAVE_TXT, DRYRUN,
  ifelse(nzchar(INST_ENV), INST_ENV, "<all>"),
  REQ_DELAY_MS, MAX_PAGES_INST
))

if (!RUN_SCRAPE) {
  message("TEPS_RUN_SCRAPE=FALSE -> skipping scraping.")
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
  invisible(path)
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

if (nzchar(INST_ENV)) {
  sel <- unique(trimws(tolower(strsplit(INST_ENV, ",", fixed = TRUE)[[1]])))
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
    fulltext          = NA_character_,
    stringsAsFactors  = FALSE
  )
  
  count <- 0L
  for (i in seq_len(nrow(urls_df))) {
    count <- count + 1L
    if (MAX_PAGES_INST > 0L && count > MAX_PAGES_INST) break
    
    u   <- urls_df$url[i]
    cod <- urls_df$course_code_norm[i]
    yr  <- urls_df$year[i]
    hvv <- urls_df$hv[i]
    
    f <- fetch_html(u, max_tries = 4L, dryrun = DRYRUN)
    res$status[i] <- f$status
    res$ok[i]     <- isTRUE(f$ok)
    
    md <- ""
    parsed <- list(course_name_no = NA_character_, fulltext = "")
    
    if (isTRUE(f$ok) && length(f$content)) {
      res$bytes[i] <- length(f$content)
      res$sha1[i]  <- digest::digest(f$content, algo = "sha1", serialize = FALSE)
      
      # Save raw HTML
      if (SAVE_HTML) {
        stub <- safe_stub(inst, cod, yr, hvv)
        fn_html <- file.path(dir_html, paste0(stub, "_", ts_now(), ".html"))
        writeBin(f$content, fn_html)
      }
      
      parsed_try <- try(parse_fields(f$content, sel), silent = TRUE)
      if (!inherits(parsed_try, "try-error") && !is.null(parsed_try)) {
        parsed <- parsed_try
      }
      
      if (is.character(parsed$fulltext) && nzchar(parsed$fulltext)) {
        md_try <- try(commonmark::html_to_md(parsed$fulltext), silent = TRUE)
        if (!inherits(md_try, "try-error") && is.character(md_try) && nzchar(md_try)) {
          md <- md_try
        }
      }
    }
    
    res$course_name_no[i] <- parsed$course_name_no
    res$fulltext[i] <- if (nzchar(md)) md else (parsed$fulltext %||% "")
    
    # Write TXT
    if (SAVE_TXT) {
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
        safe_writeLines(normalize_text(c(header, "----- MARKDOWN -----", md)), fn_txt)
      } else if (is.character(parsed$fulltext) && nzchar(parsed$fulltext)) {
        safe_writeLines(normalize_text(c(header, "----- FULLTEXT (HTML) -----", parsed$fulltext)), fn_txt)
      } else if (!isTRUE(f$ok) || !length(f$content)) {
        safe_writeLines(normalize_text(c(header, "----- FETCH FAILED -----")), fn_txt)
      } else {
        safe_writeLines(normalize_text(c(header, "----- NO CONTENT -----")), fn_txt)
      }
    }
    
    if (REQ_DELAY_MS > 0L) Sys.sleep(REQ_DELAY_MS / 1000)
    
    if (i %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time_inst, units = "secs"))
      avg_per_page <- elapsed / i
      remaining <- (nrow(urls_df) - i) * avg_per_page
      eta_min <- round(remaining / 60, 1)
      cat(sprintf("  %s: %d/%d pages fetched | ETA approx. %s min remaining\n",
                  inst, i, nrow(urls_df), eta_min))
    }
    
    if (SAVE_HTML && i %% 200 == 0) {
      invisible(gzip_html_dir(dir_html, keep_last = 1L))
    }
  }
  
  ts <- ts_now()
  csv_ts     <- file.path(dir_inst, sprintf("scrape_%s.csv", ts))
  csv_latest <- file.path(dir_inst, "scrape_latest.csv")
  for (nm in names(res)) {
    if (is.character(res[[nm]])) {
      res[[nm]] <- normalize_text(res[[nm]])
    }
  }
  safe_write_csv(res, csv_ts)
  safe_write_csv(res, csv_latest)
  
  if (SAVE_HTML) invisible(gzip_html_dir(dir_html, keep_last = 1L))
  
  ok_n   <- sum(res$ok, na.rm = TRUE)
  fail_n <- nrow(res) - ok_n
  
  stat_tbl <- sort(table(res$status), decreasing = TRUE)
  if (length(stat_tbl)) {
    cat("  HTTP status: ", paste(names(stat_tbl), stat_tbl, sep = "x", collapse = ", "), "\n")
  }
  
  fail_df <- subset(res, is.na(ok) | !ok | is.na(status) | status != 200)
  if (nrow(fail_df)) {
    safe_write_csv(fail_df, file.path(dir_inst, sprintf("scrape_fail_%s.csv", ts_now())))
  }
  
  grand_ok   <- grand_ok + ok_n
  grand_fail <- grand_fail + fail_n
  
  cat("  Done:", inst, "| OK:", ok_n, "| FAIL:", fail_n, "\n")
  
  cat("  Output:\n")
  cat("   CSV : ", gsub("^.+data/", "data/", csv_latest), "\n", sep = "")
  if (SAVE_HTML) cat("   HTML: ", gsub("^.+data/", "data/", dir_html), "\n", sep = "")
  if (SAVE_TXT)  cat("   TXT : ", gsub("^.+data/", "data/", dir_txt),  "\n", sep = "")
}

cat("\nSummary: OK =", grand_ok, "| FAIL =", grand_fail, "\n")
message("Done: scripts/03_scrape.R")
