# scripts/03_scrape.R
suppressPackageStartupMessages({
  library(yaml)
  library(httr)
  library(xml2)
  library(rvest)
  library(digest)
  library(commonmark)
})

source(file.path("R", "scrape_helpers.R"))

message("Starter 03_scrape.R")

# -------- Konfig via miljøvariabler --------
save_html       <- isTRUE(as.logical(Sys.getenv("TEPS_SAVE_HTML", "TRUE")))
save_txt        <- isTRUE(as.logical(Sys.getenv("TEPS_SAVE_TXT",  "TRUE")))
dryrun          <- isTRUE(as.logical(Sys.getenv("TEPS_DRYRUN",    "FALSE")))
inst_env        <- Sys.getenv("TEPS_INST", "")
req_delay_ms    <- as.integer(Sys.getenv("TEPS_REQ_DELAY_MS", "0"))       # f.eks. 300
max_pages_inst  <- as.integer(Sys.getenv("TEPS_MAX_PAGES_PER_INST", "0")) # 0 = ubegrenset

# -------- Finn institusjoner med course_urls_latest.csv --------
out_root  <- file.path("data", "output")
inst_dirs <- list.dirs(out_root, recursive = FALSE, full.names = FALSE)
has_urls  <- function(inst) file.exists(file.path(out_root, inst, "course_urls_latest.csv"))
inst_all  <- inst_dirs[sapply(inst_dirs, has_urls)]

if (nzchar(inst_env)) {
  sel <- unique(trimws(tolower(strsplit(inst_env, ",", fixed = TRUE)[[1]])))
  inst_all <- intersect(inst_all, sel)
}
if (!length(inst_all)) stop("Ingen institusjoner å skrape (mangler course_urls_latest.csv).")

# -------- Last selectors én gang --------
selectors_all <- lapply(inst_all, function(inst) try(load_selectors(inst, "config/selectors.yaml"), silent = TRUE))
names(selectors_all) <- inst_all

# -------- Hovedløp --------
grand_ok <- 0L; grand_fail <- 0L

for (inst in inst_all) {
  start_time_inst <- Sys.time()
  cat("\n==========================\n>>> Scraper:", inst, "\n==========================\n")
  
  path_csv <- file.path(out_root, inst, "course_urls_latest.csv")
  urls_df  <- try(read.csv(path_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(urls_df, "try-error")) { message("  Kunne ikke lese ", path_csv); next }
  
  # Minimumskrav (tillater at hv mangler)
  required_min <- c("course_code_norm", "year", "url")
  miss <- setdiff(required_min, names(urls_df))
  if (length(miss)) { message("  Hopper over: mangler kolonner i ", path_csv, " -> ", paste(miss, collapse = ", ")); next }
  if (!("hv" %in% names(urls_df))) urls_df$hv <- ""
  
  sel <- selectors_all[[inst]]
  if (inherits(sel, "try-error")) { message("  Feil/ingen selectors i config/selectors.yaml for ", inst); next }
  
  # Kataloger
  dir_inst <- file.path(out_root, inst)
  dir_html <- file.path(dir_inst, "html"); ensure_dir(dir_html)
  dir_txt  <- file.path(dir_inst, "txt");  ensure_dir(dir_txt)
  
  # Resultatbuffer
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
    fulltext          = NA_character_,   # <- Markdown lagres her
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
    
    f <- fetch_html(u, max_tries = 4L, dryrun = dryrun)
    res$status[i] <- f$status
    res$ok[i]     <- isTRUE(f$ok)
    
    if (isTRUE(f$ok) && length(f$content)) {
      res$bytes[i] <- length(f$content)
      res$sha1[i]  <- digest::digest(f$content, algo = "sha1", serialize = FALSE)
      
      # Rå HTML (valgfritt)
      if (save_html) {
        stub <- safe_stub(inst, cod, yr, hvv)
        fn_html <- file.path(dir_html, paste0(stub, "_", ts_now(), ".html"))
        writeBin(f$content, fn_html)
      }
      
      # Parse + konverter til Markdown (fallback til original HTML-tekst ved feil)
      parsed <- parse_fields(f$content, sel)
      res$course_name_no[i] <- parsed$course_name_no
      
      md <- tryCatch(
        {
          if (!is.null(parsed$fulltext) && nzchar(parsed$fulltext)) {
            commonmark::html_to_md(parsed$fulltext)
          } else {
            ""
          }
        },
        error = function(e) parsed$fulltext %||% ""
      )
      res$fulltext[i] <- md
      
      # TXT per emne (skriv MARKDOWN)
      if (save_txt && is.character(res$fulltext[i]) && nzchar(res$fulltext[i])) {
        stub <- safe_stub(inst, cod, yr, hvv)
        fn_txt <- file.path(dir_txt, paste0(stub, ".txt"))
        header <- paste0(
          "institution: ", inst, "\n",
          "code: ", cod, "\n",
          "year: ", yr, "\n",
          "hv: ", hvv, "\n",
          "url: ", u, "\n",
          "status: ", ifelse(is.na(f$status), "", f$status), "\n",
          "course_name_no: ", ifelse(is.na(parsed$course_name_no), "", parsed$course_name_no), "\n",
          "----- MARKDOWN -----\n"
        )
        con <- file(fn_txt, open = "w", encoding = "UTF-8")
        on.exit(try(close(con), silent = TRUE), add = TRUE)
        writeLines(c(header, res$fulltext[i]), con, useBytes = TRUE)
        on.exit(NULL, add = FALSE)
      }
    }
    
    if (req_delay_ms > 0L) Sys.sleep(req_delay_ms / 1000)
    
    # progress + ETA hvert 50. kurs
    if (i %% 50 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time_inst, units = "secs"))
      avg_per_page <- elapsed / i
      remaining <- (nrow(urls_df) - i) * avg_per_page
      eta_min <- round(remaining / 60, 1)
      cat(sprintf("  %s: %d/%d sider hentet | ETA ca. %s min igjen\n",
                  inst, i, nrow(urls_df), eta_min))
    }
  }
  
  # Skriv CSV-er
  ts   <- ts_now()
  csv_ts     <- file.path(dir_inst, sprintf("scrape_%s.csv", ts))
  csv_latest <- file.path(dir_inst, "scrape_latest.csv")
  utils::write.csv(res, csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(res, csv_latest, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Komprimer gamle HTML for å spare plass (behold siste pr. kurs)
  if (save_html) invisible(gzip_html_dir(dir_html, keep_last = 1L))
  
  ok_n   <- sum(res$ok, na.rm = TRUE)
  fail_n <- nrow(res) - ok_n
  
  # HTTP-status sammendrag
  stat_tbl <- sort(table(res$status), decreasing = TRUE)
  if (length(stat_tbl)) cat("  HTTP-status: ", paste(names(stat_tbl), stat_tbl, sep="x", collapse=", "), "\n")
  
  grand_ok   <- grand_ok + ok_n
  grand_fail <- grand_fail + fail_n
  
  cat("  Ferdig:", inst, "| OK:", ok_n, "| FAIL:", fail_n, "\n")
  cat("  Output:\n   ", gsub("^.+data/", "data/", csv_latest), "\n")
}

cat("\nSummary: OK =", grand_ok, "| FAIL =", grand_fail, "\n")
message("Done: scripts/03_scrape.R")
