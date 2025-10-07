# ============================================================
# 03_scrape_chromote_only.R
# TEPS Chromote-only scraper with progress, ETA & summaries
# Windows-safe parallel with graceful fallback
# ============================================================

options(encoding = "UTF-8")
try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)

suppressPackageStartupMessages({
  library(parallel)
  library(chromote)
  library(xml2)
  library(digest)
})

source(file.path("R", "scrape_helpers.R"))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "")) a else b

# -------------------------------------------------------------------
# Config (env)
# -------------------------------------------------------------------
INST_ENV        <- Sys.getenv("TEPS_INST", "")
REQ_DELAY_MS    <- as.integer(Sys.getenv("TEPS_REQ_DELAY_MS", "400"))
MAX_PAGES_INST  <- as.integer(Sys.getenv("TEPS_MAX_PAGES_PER_INST", "0"))
DRYRUN          <- tolower(Sys.getenv("TEPS_DRYRUN", "FALSE")) %in% c("true","1","yes","y")

out_root  <- file.path("data", "output")
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
has_urls  <- function(inst) file.exists(file.path(out_root, inst, "course_urls_latest.csv"))
inst_all  <- inst_dirs[sapply(inst_dirs, has_urls)]

if (nzchar(INST_ENV)) {
  sel <- unique(trimws(tolower(strsplit(INST_ENV, ",", fixed = TRUE)[[1]])))
  inst_all <- intersect(inst_all, sel)
}
if (!length(inst_all)) stop("No institutions to scrape.")

cat("Starting Chromote-only scraper\n",
    "  INST      : ", ifelse(nzchar(INST_ENV), INST_ENV, "<all>"), "\n",
    "  DELAY     : ", REQ_DELAY_MS, " ms\n",
    "  MAX PAGES : ", MAX_PAGES_INST, ifelse(MAX_PAGES_INST > 0, " (cap per inst)", ""), "\n",
    "  DRYRUN    : ", DRYRUN, "\n",
    sep="")

cat("Institutions to scrape: ", paste(inst_all, collapse = ", "), "\n", sep="")

# -------------------------------------------------------------------
# Helper: render full JS page using Chromote
# -------------------------------------------------------------------
render_page_chromote <- function(url, wait_selector = "usn-study", wait_time = 12) {
  message("[Chromote] Navigating: ", url)
  b <- chromote::ChromoteSession$new()
  b$Page$navigate(url)
  
  # Wait for <usn-study> to exist (max wait_time sec)
  for (i in seq_len(wait_time)) {
    has_study <- b$Runtime$evaluate(
      "!!document.querySelector('usn-study')"
    )$result$value
    if (isTRUE(has_study)) break
    Sys.sleep(1)
  }
  
  # Try to read shadowRoot if available
  html_usn <- b$Runtime$evaluate(
    "document.querySelector('usn-study')?.shadowRoot?.innerHTML || ''"
  )$result$value
  
  # Fallback to normal body if empty
  if (is.null(html_usn) || nchar(html_usn) < 500) {
    message("[Fallback] Using <body> instead of shadowRoot")
    html_usn <- b$Runtime$evaluate("document.body.outerHTML")$result$value
  }
  
  b$close()
  enc2utf8(html_usn)
}


# -------------------------------------------------------------------
# Scrape one institution (with progress, ETA & CSV log)
# -------------------------------------------------------------------
scrape_institution <- function(inst) {
  cat("\n===========================\n>>> Chromote scraper: ", inst, "\n===========================\n", sep="")
  
  path_csv <- file.path(out_root, inst, "course_urls_latest.csv")
  urls_df  <- read.csv(path_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  dir_inst <- file.path(out_root, inst)
  dir_html <- file.path(dir_inst, "html_raw"); ensure_dir(dir_html)
  dir_txt  <- file.path(dir_inst, "txt_raw");  ensure_dir(dir_txt)
  
  n <- nrow(urls_df)
  t0 <- Sys.time()
  
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
  
  for (i in seq_len(n)) {
    if (MAX_PAGES_INST > 0L && i > MAX_PAGES_INST) break
    
    u   <- urls_df$url[i]
    cod <- urls_df$course_code_norm[i]
    yr  <- urls_df$year[i]
    hvv <- res$hv[i]
    
    # Progress + ETA
    done <- i - 1L
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rate <- if (done > 0) elapsed / done else NA_real_
    remaining <- if (is.finite(rate)) (n - done) * rate else NA_real_
    eta_txt <- if (is.finite(remaining)) {
      paste0("ETA ", format(.POSIXct(Sys.time() + remaining, tz = ""), "%H:%M:%S"))
    } else {
      "ETA --:--:--"
    }
    
    cat(sprintf("  [%03d/%03d] %s  (%s) ... ", i, n, cod, eta_txt)); flush.console()
    
    # Fetch (Chromote-only)
    html_content <- try(render_page_chromote(u, wait_selector = "body", wait_time = 8, inst = inst), silent = TRUE)
    ok <- !(inherits(html_content, "try-error") || !nzchar(html_content))
    
    # Prepare filenames
    stub    <- safe_stub(inst, cod, yr, hvv)
    fn_html <- file.path(dir_html, paste0(stub, "_chromote_", ts_now(), ".html"))
    fn_txt  <- file.path(dir_txt,  paste0(stub, "_chromote.txt"))
    
    if (ok) {
      writeLines(html_content, fn_html, useBytes = TRUE)
      raw_bytes <- nchar(html_content, type = "bytes")
      res$ok[i]     <- TRUE
      res$status[i] <- 200L
      res$bytes[i]  <- raw_bytes
      res$sha1[i]   <- digest::digest(charToRaw(html_content), algo = "sha1", serialize = FALSE)
      
      # Extract body text (best-effort)
      body_txt <- ""
      tr <- try({
        doc <- read_html(html_content)
        xml_text(xml_find_first(doc, "//body"))
      }, silent = TRUE)
      if (!inherits(tr, "try-error") && is.character(tr)) body_txt <- tr
      
      header <- paste0(
        "institution: ", inst, "\n",
        "code: ", cod, "\n",
        "year: ", yr, "\n",
        "hv: ", hvv %||% "", "\n",
        "url: ", u, "\n",
        "status: ", res$status[i], "\n"
      )
      safe_writeLines(c(header, "----- BODY -----", body_txt %||% "<no body extracted>"), fn_txt)
      cat("[OK]\n")
    } else {
      res$ok[i]     <- FALSE
      res$status[i] <- 408L
      writeLines("<!-- chromote fetch failed -->", fn_html, useBytes = TRUE)
      cat("[FAILED]\n")
    }
    
    if (REQ_DELAY_MS > 0L) Sys.sleep(REQ_DELAY_MS / 1000)
  }
  
  # Write per-institution log CSV
  csv_latest <- file.path(dir_inst, "scrape_chromote_latest.csv")
  utils::write.csv(res, csv_latest, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("  Done: ", inst, " | OK: ", sum(res$ok, na.rm = TRUE),
      " | FAIL: ", nrow(res) - sum(res$ok, na.rm = TRUE), "\n", sep = "")
  
  invisible(res)
}

# -------------------------------------------------------------------
# Parallel orchestration (with warnings & fallback)
# -------------------------------------------------------------------
start_time_all <- Sys.time()
num_cores <- max(1L, min(3L, detectCores() - 1L))

if (length(inst_all) == 1L) {
  cat("\n⚙️  Parallel mode: OFF (1 institution)\n")
  agg <- list(scrape_institution(inst_all))
} else {
  cat("\n⚙️  Parallel mode: ON (", num_cores, " cores)\n", sep = "")
  cl <- try(makeCluster(num_cores), silent = TRUE)
  
  if (inherits(cl, "try-error")) {
    cat("⚠️  Failed to start cluster — running sequentially instead.\n")
    agg <- lapply(inst_all, scrape_institution)
  } else {
    cat("📤  Exporting environment to workers ...\n")
    clusterExport(cl, c("scrape_institution", "render_page_chromote", "out_root",
                        "REQ_DELAY_MS", "MAX_PAGES_INST", "DRYRUN",
                        "safe_stub", "ts_now", "ensure_dir", "%||%"), envir = environment())
    clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(chromote); library(xml2); library(digest)
      })
      source(file.path("R", "scrape_helpers.R"))
      TRUE
    })
    cat("🚀  Starting parallel scraping for ", length(inst_all), " institutions...\n", sep = "")
    agg <- parLapply(cl, inst_all, scrape_institution)
    stopCluster(cl)
    cat("🧮  Cluster stopped.\n")
  }
}

elapsed_all <- round(as.numeric(difftime(Sys.time(), start_time_all, units = "mins")), 2)
cat("\n✅ Chromote-only scrape complete for all institutions (", elapsed_all, " min)\n", sep = "")

# -------------------------------------------------------------------
# Directory and result summary (like 03_scrape_raw.R)
# -------------------------------------------------------------------
out_root <- "data/output"
inst_dirs_full <- list.dirs(out_root, recursive = FALSE, full.names = TRUE)

check_dirs <- function(path) {
  html_dir <- file.path(path, "html_raw")
  txt_dir  <- file.path(path, "txt_raw")
  
  html_files <- if (dir.exists(html_dir)) length(list.files(html_dir, pattern = "\\.html$", full.names = TRUE)) else 0
  txt_files  <- if (dir.exists(txt_dir))  length(list.files(txt_dir,  pattern = "\\.txt$",  full.names = TRUE)) else 0
  
  data.frame(
    institution     = basename(path),
    html_dir_exists = dir.exists(html_dir),
    html_files      = html_files,
    txt_dir_exists  = dir.exists(txt_dir),
    txt_files       = txt_files,
    html_empty      = html_files == 0,
    txt_empty       = txt_files == 0,
    stringsAsFactors = FALSE
  )
}

cat("\n--- Directory summary ---\n")
res_dirs <- do.call(rbind, lapply(inst_dirs_full, check_dirs))
print(res_dirs, row.names = FALSE)

# Compare URLs vs. scraped files
inst_short <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
results <- data.frame()

for (inst in inst_short) {
  csv_path <- file.path(out_root, inst, "course_urls_latest.csv")
  html_dir <- file.path(out_root, inst, "html_raw")
  txt_dir  <- file.path(out_root, inst, "txt_raw")
  
  if (!file.exists(csv_path)) next
  
  urls <- nrow(read.csv(csv_path, stringsAsFactors = FALSE))
  html_files <- if (dir.exists(html_dir)) length(list.files(html_dir, "\\.html$", full.names = TRUE)) else 0
  txt_files  <- if (dir.exists(txt_dir))  length(list.files(txt_dir,  "\\.txt$",  full.names = TRUE)) else 0
  
  results <- rbind(results, data.frame(
    institution = inst,
    urls        = urls,
    html_files  = html_files,
    txt_files   = txt_files,
    html_missing = pmax(0, urls - html_files),
    txt_missing  = pmax(0, urls - txt_files),
    stringsAsFactors = FALSE
  ))
}

results$ok_html <- results$html_missing == 0
results$ok_txt  <- results$txt_missing == 0

cat("\n--- Coverage summary ---\n")
print(results, row.names = FALSE)

cat("\nInstitutions with missing files:\n")
print(subset(results, !ok_html | !ok_txt))

