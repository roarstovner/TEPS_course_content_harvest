# scripts/00_run_all.R
# =====================
# Master-runner for TEPS pipeline.
#
# Steg:
#   (01) scripts/01_prepare_input.R   -> lager renset cache (courses_std.RDS)
#   (02) scripts/02_generate_urls.R   -> kjører alle generate_urls_*.R
#   (03) R/cleanup_outputs.R          -> (valgfritt) sletter gamle filer, beholder kun *latest*
#   (04) scripts/03_scrape.R          -> (valgfritt) skraper alle institusjoner
#
options(stringsAsFactors = FALSE, warn = 1)

# ---- Hjelpere ----
get_env_or_default <- function(var, default) {
  val <- Sys.getenv(var, "")
  if (nzchar(val)) return(val)
  default
}
as_bool <- function(x) tolower(as.character(x)) %in% c("1","true","yes","y")

# ---- Konfig (default, kan overstyres via miljøvariabler) ----
CLEAN      <- TRUE
DRYRUN     <- FALSE
TEPS_INST  <- ""
RUN_SCRAPE <- TRUE   # default verdi

# Scrape defaults
SAVE_HTML        <- TRUE
SAVE_TXT         <- TRUE
REQ_DELAY_MS     <- 300
MAX_PAGES_INST   <- 0

# ---- Overstyr med env-variabler ----
CLEAN        <- as_bool(get_env_or_default("TEPS_CLEAN",  if (CLEAN) "1" else "0"))
DRYRUN       <- as_bool(get_env_or_default("TEPS_DRYRUN", if (DRYRUN) "1" else "0"))
RUN_SCRAPE   <- as_bool(get_env_or_default("TEPS_RUN_SCRAPE", if (RUN_SCRAPE) "1" else "0"))
TEPS_INST    <- get_env_or_default("TEPS_INST", TEPS_INST)
SAVE_HTML    <- as_bool(get_env_or_default("TEPS_SAVE_HTML", if (SAVE_HTML) "1" else "0"))
SAVE_TXT     <- as_bool(get_env_or_default("TEPS_SAVE_TXT",  if (SAVE_TXT)  "1" else "0"))
REQ_DELAY_MS <- as.integer(get_env_or_default("TEPS_REQ_DELAY_MS", as.character(REQ_DELAY_MS)))
MAX_PAGES_INST <- as.integer(get_env_or_default("TEPS_MAX_PAGES_PER_INST", as.character(MAX_PAGES_INST)))


find_root <- function(marker = "config/institutions.yaml", max_up = 6) {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:max_up) {
    if (file.exists(file.path(d, marker))) return(d)
    d <- dirname(d)
  }
  stop("Fant ikke prosjektrot (mangler ", marker, ").")
}
banner <- function(x){
  cat("\n", paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
  cat("=== ", x, " ===\n", sep="")
  cat(paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
}
source_utf8 <- function(path, env = parent.frame()) {
  if (!file.exists(path)) stop("Mangler fil: ", path)
  # 1) prøv rett fram med UTF-8
  ok <- try(suppressWarnings(source(path, local = env, encoding = "UTF-8")), silent = TRUE)
  if (!inherits(ok, "try-error")) return(invisible(TRUE))
  # 2) prøv å re-encode innholdet til UTF-8 og source på nytt
  encs <- c("UTF-8","UTF-16LE","UTF-16BE","latin1","windows-1252")
  for (e in encs) {
    con <- file(path, open = "r", encoding = e)
    txt <- try(readLines(con, warn = FALSE), silent = TRUE)
    close(con)
    if (!inherits(txt, "try-error") && length(txt)) {
      tmp <- tempfile(fileext = ".R")
      writeLines(enc2utf8(txt), tmp, useBytes = TRUE)
      ok2 <- try(suppressWarnings(source(tmp, local = env, encoding = "UTF-8")), silent = TRUE)
      unlink(tmp)
      if (!inherits(ok2, "try-error")) return(invisible(TRUE))
    }
  }
  stop("Klarte ikke å source som UTF-8: ", path)
}

run_script <- function(path, env_list = list()) {
  if (!file.exists(path)) { cat("Skip (mangler): ", path, "\n", sep=""); return(invisible(FALSE)) }
  banner(paste("Running", path))
  t0 <- Sys.time()
  e <- list2env(env_list, parent = globalenv())
  
  if (!exists("%||%", envir = e, inherits = FALSE)) {
    e[["%||%"]] <- function(a,b) if (is.null(a) || length(a)==0) b else a
  }
  if (!exists("safe_read_yaml", envir = e, inherits = FALSE)) {
    e[["safe_read_yaml"]] <- function(path) {
      tr <- try({
        con <- file(path, "r", encoding = "UTF-8")
        on.exit(try(close(con), silent=TRUE), add=TRUE)
        yaml::read_yaml(con)
      }, silent = TRUE)
      if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
      raw <- readBin(path, "raw", file.info(path)$size)
      txt <- rawToChar(raw); Encoding(txt) <- "unknown"
      txt <- sub("^\ufeff", "", txt)      # fjern BOM
      txt <- gsub("\r\n?", "\n", txt)     # normaliser CRLF/CR
      txt <- gsub("\t", "  ", txt, fixed=TRUE)
      yaml::yaml.load(txt)
    }
  }
  
  ok <- TRUE
  warning_handler <- function(w) {
    message("[WARN] ", conditionMessage(w))
    try(invokeRestart("muffleWarning"), silent = TRUE)
  }
  
  tryCatch(
    withCallingHandlers(
      { source_utf8(path, env = e) },
      warning = warning_handler
    ),
    error = function(err) {
      ok <<- FALSE
      message("[ERROR] ", conditionMessage(err))
    }
  )
  cat(sprintf("Done: %s (%.1f s)\n", path, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(ok)
}

# ---- Prosjektrot ----
proj <- find_root(); setwd(proj)
# Les env → overstyr defaults
CLEAN          <- as_bool(get_env_or_default("TEPS_CLEAN",  if (CLEAN) "1" else "0"))
DRYRUN         <- as_bool(get_env_or_default("TEPS_DRYRUN", if (DRYRUN) "1" else "0"))
TEPS_INST      <- get_env_or_default("TEPS_INST", TEPS_INST)
RUN_SCRAPE     <- as_bool(get_env_or_default("TEPS_RUN_SCRAPE", if (RUN_SCRAPE) "1" else "0"))
SAVE_HTML      <- as_bool(get_env_or_default("TEPS_SAVE_HTML", if (SAVE_HTML) "1" else "0"))
SAVE_TXT       <- as_bool(get_env_or_default("TEPS_SAVE_TXT",  if (SAVE_TXT) "1" else "0"))
REQ_DELAY_MS   <- as.integer(get_env_or_default("TEPS_REQ_DELAY_MS", as.character(REQ_DELAY_MS)))
MAX_PAGES_INST <- as.integer(get_env_or_default("TEPS_MAX_PAGES_PER_INST", as.character(MAX_PAGES_INST)))

cat("Prosjektrot: ", proj, "\n", sep="")
cat("TEPS_CLEAN=", CLEAN, "  TEPS_DRYRUN=", DRYRUN,
    if (nzchar(TEPS_INST)) paste0("  TEPS_INST=", TEPS_INST) else "", "\n", sep="")
cat("SCRAPE: run=", RUN_SCRAPE,
    " save_html=", SAVE_HTML, " save_txt=", SAVE_TXT,
    " delay_ms=", REQ_DELAY_MS, " max_pages_inst=", MAX_PAGES_INST, "\n", sep="")

# ---- (01) Forbered cache ----
ok1 <- run_script("scripts/01_prepare_input.R")

# ---- (02) Generér URL-er ----
Sys.setenv(TEPS_INST = TEPS_INST)
ok2 <- run_script("scripts/02_generate_urls.R", env_list = list(TEPS_INST = TEPS_INST))

# ---- (02b) Hurtig-fiks for UIA: candidates -> course_urls hvis mangler ----
uia_dir <- file.path("data","output","uia")
if (dir.exists(uia_dir)) {
  cu_csv <- file.path(uia_dir, "course_urls_latest.csv")
  cu_txt <- file.path(uia_dir, "course_urls_latest.txt")
  ca_csv <- file.path(uia_dir, "candidates_latest.csv")
  ca_txt <- file.path(uia_dir, "candidates_latest.txt")
  if (!file.exists(cu_csv) && file.exists(ca_csv)) {
    file.copy(ca_csv, cu_csv, overwrite = TRUE)
    cat("[UIA] Kopierte candidates_latest.csv -> course_urls_latest.csv\n")
  }
  if (!file.exists(cu_txt) && file.exists(ca_txt)) {
    file.copy(ca_txt, cu_txt, overwrite = TRUE)
    cat("[UIA] Kopierte candidates_latest.txt -> course_urls_latest.txt\n")
  }
}

# ---- (03) Opprydding (optional) ----
if (CLEAN) {
  ok3 <- run_script("R/cleanup_outputs.R", env_list = list(dry_run = DRYRUN))
} else {
  cat("\nOpprydding hoppet over (sett TEPS_CLEAN=1 for å aktivere).\n")
  ok3 <- TRUE
}

# ---- (04) Scrape (optional) ----
if (RUN_SCRAPE) {
  # sett scrape-env
  Sys.setenv(
    TEPS_SAVE_HTML        = if (SAVE_HTML) "1" else "0",
    TEPS_SAVE_TXT         = if (SAVE_TXT)  "1" else "0",
    TEPS_REQ_DELAY_MS     = as.character(REQ_DELAY_MS),
    TEPS_MAX_PAGES_PER_INST = as.character(MAX_PAGES_INST),
    TEPS_DRYRUN           = if (DRYRUN) "1" else "0",
    TEPS_INST             = TEPS_INST
  )
  ok4 <- run_script("scripts/03_scrape.R")
} else {
  ok4 <- TRUE
  cat("\nScrape hoppet over (sett TEPS_RUN_SCRAPE=1 for å aktivere).\n")
}

cat("\nAlt ferdig. Status: ",
    if (ok1 && ok2 && ok3 && ok4) "OK" else "med feil (se over).", "\n", sep="")
