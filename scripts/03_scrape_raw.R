# scripts/03_scrape_raw.R
# =======================
# Phase 1 – Raw data scraping
# Reads course_urls_latest.csv
# Fetches HTML and saves both HTML and raw TXT body
# No parsing or filtering

# --- Encoding & locale ---
options(encoding = "UTF-8")
try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)

suppressPackageStartupMessages({
  library(httr)
  library(xml2)
  library(digest)
  library(chromote)
})

# ============================================================
# Helper: render full JS page with Chromote
# ============================================================
render_page_chromote <- function(url, wait_selector = "body", wait_time = 10) {
  if (!requireNamespace("chromote", quietly = TRUE))
    stop("Package 'chromote' is required for JS-rendered pages.")
  
  b <- chromote::ChromoteSession$new()
  b$Page$navigate(url)
  
  # Wait until the selector exists (polls every second)
  b$Runtime$evaluate(sprintf("
    new Promise(resolve => {
      const check = () => {
        if (document.querySelector('%s')) resolve(true);
        else setTimeout(check, 1000);
      };
      check();
    });
  ", wait_selector))
  
  Sys.sleep(wait_time)
  html <- b$DOM$getDocument()
  src  <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)[["outerHTML"]]
  Encoding(src) <- "UTF-8"
  b$close()
  src
}


source(file.path("R", "scrape_helpers.R"))

# -------------------------------------------------------------------
# Config from environment
# -------------------------------------------------------------------
INST_ENV       <- Sys.getenv("TEPS_INST", "")
REQ_DELAY_MS   <- as.integer(Sys.getenv("TEPS_REQ_DELAY_MS", "200"))
MAX_PAGES_INST <- as.integer(Sys.getenv("TEPS_MAX_PAGES_PER_INST", "0"))
DRYRUN         <- tolower(Sys.getenv("TEPS_DRYRUN", "FALSE")) %in% c("true","1","yes","y")

cat("Starting 03_scrape_raw.R | INST=",
    ifelse(nzchar(INST_ENV), INST_ENV, "<all>"),
    " delay=", REQ_DELAY_MS, "ms\n")

# -------------------------------------------------------------------
# Robust fetch_html with Chromote fallback
# -------------------------------------------------------------------
fetch_html <- function(url, wait_ms = 6000, retries = 3) {
  if (DRYRUN) return(list(ok = TRUE, status = 999, content = raw(0)))
  
  # --- Try httr first ---
  tr <- try({
    resp <- httr::GET(url, timeout(10))
    txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
    txt  <- enc2utf8(txt)
    list(status = resp$status_code, txt = txt)
  }, silent = TRUE)
  
  need_chromote <- FALSE
  if (inherits(tr, "try-error") || is.null(tr) || is.null(tr$txt)) {
    need_chromote <- TRUE
    cat("  [httr failed → Chromote]\n")
  } else if (tr$status != 200) {
    need_chromote <- TRUE
    cat(sprintf("  [HTTP %s → Chromote]\n", tr$status))
  } else if (nchar(tr$txt) < 2000) {
    need_chromote <- TRUE
    cat(sprintf("  [httr short (%d chars) → Chromote]\n", nchar(tr$txt)))
  }
  
  # --- Chromote fallback ---
  if (need_chromote) {
    for (attempt in seq_len(retries)) {
      cat(sprintf("  [chromote attempt %d/%d] %s\n", attempt, retries, url))
      out <- try({
        b <- chromote::ChromoteSession$new()
        b$Page$navigate(url)
        # Wait for document fully loaded
        b$Runtime$evaluate("new Promise(r => { 
          if (document.readyState === 'complete') r(); 
          else window.addEventListener('load', () => r()); 
        })")
        Sys.sleep(wait_ms / 1000)
        html <- b$DOM$getDocument()
        html_content <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)$outerHTML
        Encoding(html_content) <- "UTF-8"
        b$close()
        html_content
      }, silent = TRUE)
      
      if (!inherits(out, "try-error") && !is.null(out) && nzchar(out)) {
        cat("  [chromote OK]\n")
        return(list(ok = TRUE, status = 200, content = charToRaw(out)))
      } else {
        cat("  [chromote timeout/retry in 2s]\n")
        Sys.sleep(2)
      }
    }
    
    cat("  [chromote FAILED after retries]\n")
    return(list(ok = FALSE, status = 408, content = raw(0)))
  }
  
  # --- httr success ---
  return(list(ok = TRUE, status = tr$status, content = charToRaw(tr$txt)))
}

# -------------------------------------------------------------------
# Detect institutions
# -------------------------------------------------------------------
out_root  <- file.path("data", "output")
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
has_urls  <- function(inst) file.exists(file.path(out_root, inst, "course_urls_latest.csv"))
inst_all  <- inst_dirs[sapply(inst_dirs, has_urls)]

if (nzchar(INST_ENV)) {
  sel <- unique(trimws(tolower(strsplit(INST_ENV, ",", fixed = TRUE)[[1]])))
  inst_all <- intersect(inst_all, sel)
}
if (!length(inst_all)) stop("No institutions to scrape.")

# -------------------------------------------------------------------
# Main scraping loop
# -------------------------------------------------------------------
for (inst in inst_all) {
  cat("\n==========================\n>>> RAW scraper:", inst, "\n==========================\n")
  path_csv <- file.path(out_root, inst, "course_urls_latest.csv")
  urls_df  <- read.csv(path_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  dir_inst <- file.path(out_root, inst)
  dir_html <- file.path(dir_inst, "html_raw"); ensure_dir(dir_html)
  dir_txt  <- file.path(dir_inst, "txt_raw");  ensure_dir(dir_txt)
  
  res <- data.frame(
    institution_short = inst,
    course_code_norm  = urls_df$course_code_norm,
    year              = urls_df$year,
    hv                = if ("hv" %in% names(urls_df)) urls_df$hv else "",
    url               = urls_df$url,
    status            = NA_integer_,
    ok                = FALSE,
    bytes             = NA_integer_,
    sha1              = NA_character_,
    stringsAsFactors  = FALSE
  )
  
  for (i in seq_len(nrow(urls_df))) {
    if (MAX_PAGES_INST > 0L && i > MAX_PAGES_INST) break
    u   <- urls_df$url[i]
    cod <- urls_df$course_code_norm[i]
    yr  <- urls_df$year[i]
    hvv <- res$hv[i]
    
    cat(sprintf("  [%03d/%03d] Fetching %s ... ", i, nrow(urls_df), cod)); flush.console()
    f <- fetch_html(u)
    res$status[i] <- f$status
    res$ok[i]     <- isTRUE(f$ok)
    
    stub <- safe_stub(inst, cod, yr, hvv)
    fn_html <- file.path(dir_html, paste0(stub, "_", ts_now(), ".html"))
    fn_txt  <- file.path(dir_txt, paste0(stub, ".txt"))
    
    if (isTRUE(f$ok)) {
      res$bytes[i] <- if (length(f$content)) length(f$content) else NA
      res$sha1[i]  <- if (length(f$content)) digest::digest(f$content, algo = "sha1", serialize = FALSE) else NA
      
      # Save HTML
      if (length(f$content) > 0) {
        writeBin(f$content, fn_html)
        cat("[HTML OK] ")
      } else {
        writeLines("<!-- empty or null content -->", fn_html, useBytes = TRUE)
        cat("[EMPTY HTML] ")
      }
      
      # Extract body
      body_txt <- ""
      if (length(f$content) > 0) {
        tr <- try({
          doc <- read_html(f$content)
          xml_text(xml_find_first(doc, "//body"))
        }, silent = TRUE)
        if (!inherits(tr, "try-error") && is.character(tr)) body_txt <- tr
      }
      
      # Save TXT
      header <- paste0(
        "institution: ", inst, "\n",
        "code: ", cod, "\n",
        "year: ", yr, "\n",
        "hv: ", hvv, "\n",
        "url: ", u, "\n",
        "status: ", ifelse(is.na(f$status), "", f$status), "\n"
      )
      safe_writeLines(c(header, "----- RAW BODY -----", body_txt %||% "<no body extracted>"), fn_txt)
      cat("[TXT OK]\n")
    } else {
      writeLines("<!-- fetch failed -->", fn_html, useBytes = TRUE)
      cat("[FAILED]\n")
    }
    
    if (REQ_DELAY_MS > 0L) Sys.sleep(REQ_DELAY_MS / 1000)
  }
  
  csv_latest <- file.path(dir_inst, "scrape_raw_latest.csv")
  utils::write.csv(res, csv_latest, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("  Done:", inst, "| OK:", sum(res$ok, na.rm = TRUE),
      "| FAIL:", nrow(res) - sum(res$ok, na.rm = TRUE), "\n")
}

cat("\nSummary complete (RAW scrape)\n")

# -------------------------------------------------------------------
# Directory and result summary
# -------------------------------------------------------------------
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

# Compare URLs vs. scraped files
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
results <- data.frame()

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

results$ok_html <- results$html_missing == 0
results$ok_txt  <- results$txt_missing == 0

print(results, row.names = FALSE)

cat("\nInstitutions with missing files:\n")
print(subset(results, !ok_html | !ok_txt))
