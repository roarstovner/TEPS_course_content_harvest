# =====================
# TEPS pipeline master runner
# =====================
# rm(list = ls(all.names = TRUE))

options(stringsAsFactors = FALSE, warn = 1)

# Suppress "closing unused connection" warnings
suppressWarnings({
  options(showConnections = FALSE)
})

# =====================
# Helpers
# =====================
get_env_or_default <- function(var, default) {
  val <- Sys.getenv(var, "")
  if (nzchar(val)) return(val)
  default
}
as_bool <- function(x) tolower(as.character(x)) %in% c("1","true","yes","y")

find_root <- function(marker = "config/institutions.yaml", max_up = 6) {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:max_up) {
    if (file.exists(file.path(d, marker))) return(d)
    d <- dirname(d)
  }
  stop("Project root not found (missing ", marker, ").")
}
banner <- function(x){
  cat("\n", paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
  cat("=== ", x, " ===\n", sep="")
  cat(paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
}
source_utf8 <- function(path, env = parent.frame()) {
  if (!file.exists(path)) stop("Missing file: ", path)
  ok <- try(suppressWarnings(source(path, local = env, encoding = "UTF-8")), silent = TRUE)
  if (!inherits(ok, "try-error")) return(invisible(TRUE))
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
  stop("Could not source as UTF-8: ", path)
}
run_script <- function(path, env_list = list()) {
  if (!file.exists(path)) { cat("Skip (missing): ", path, "\n", sep=""); return(invisible(FALSE)) }
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
      txt <- sub("^\ufeff", "", txt)
      txt <- gsub("\r\n?", "\n", txt)
      txt <- gsub("\t", "  ", txt, fixed=TRUE)
      yaml::yaml.load(txt)
    }
  }
  
  ok <- TRUE
  warning_handler <- function(w) {
    if (grepl("closing unused connection", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    } else {
      message("[WARN] ", conditionMessage(w))
      try(invokeRestart("muffleWarning"), silent = TRUE)
    }
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

# =====================
# Defaults (constants)
# =====================
DEFAULT_CLEAN          <- TRUE
DEFAULT_DRYRUN         <- TRUE
DEFAULT_TEPS_INST      <- ""
DEFAULT_RUN_SCRAPE     <- TRUE

DEFAULT_SAVE_URL_CSV   <- TRUE
DEFAULT_SAVE_URL_TXT   <- TRUE

DEFAULT_SAVE_HTML      <- TRUE
DEFAULT_SAVE_TXT       <- TRUE
DEFAULT_REQ_DELAY_MS   <- 100L
DEFAULT_MAX_PAGES_INST <- 0L

# ---- Project root ----
proj <- find_root(); setwd(proj)

# =====================
# ---- Read env overrides ----
CLEAN          <- as_bool(get_env_or_default("TEPS_CLEAN",  if (DEFAULT_CLEAN)  "TRUE" else "FALSE"))
DRYRUN         <- as_bool(get_env_or_default("TEPS_DRYRUN", if (DEFAULT_DRYRUN) "TRUE" else "FALSE"))
TEPS_INST      <- get_env_or_default("TEPS_INST", DEFAULT_TEPS_INST)
RUN_SCRAPE     <- as_bool(get_env_or_default("TEPS_RUN_SCRAPE", if (DEFAULT_RUN_SCRAPE) "TRUE" else "FALSE"))

SAVE_URL_CSV   <- as_bool(get_env_or_default("TEPS_SAVE_URL_CSV", if (DEFAULT_SAVE_URL_CSV) "TRUE" else "FALSE"))
SAVE_URL_TXT   <- as_bool(get_env_or_default("TEPS_SAVE_URL_TXT", if (DEFAULT_SAVE_URL_TXT) "TRUE" else "FALSE"))

SAVE_HTML      <- as_bool(get_env_or_default("TEPS_SAVE_HTML", if (DEFAULT_SAVE_HTML) "TRUE" else "FALSE"))
SAVE_TXT       <- as_bool(get_env_or_default("TEPS_SAVE_TXT",  if (DEFAULT_SAVE_TXT)  "TRUE" else "FALSE"))
REQ_DELAY_MS   <- as.integer(get_env_or_default("TEPS_REQ_DELAY_MS", as.character(DEFAULT_REQ_DELAY_MS)))
MAX_PAGES_INST <- as.integer(get_env_or_default("TEPS_MAX_PAGES_PER_INST", as.character(DEFAULT_MAX_PAGES_INST)))

# ---- Echo effective config ----
cat("Project root: ", proj, "\n", sep="")
cat("CLEAN=", CLEAN, "  DRYRUN=", DRYRUN,
    if (nzchar(TEPS_INST)) paste0("  TEPS_INST=", TEPS_INST) else "", "\n", sep="")
cat("URL: csv=", SAVE_URL_CSV, "  txt=", SAVE_URL_TXT, "\n", sep="")
cat("SCRAPE: run=", RUN_SCRAPE,
    "  save_html=", SAVE_HTML, "  save_txt=", SAVE_TXT,
    "  delay_ms=", REQ_DELAY_MS, "  max_pages_inst=", MAX_PAGES_INST, "\n", sep="")

# =====================
# (01) Prepare cache
# =====================
Sys.setenv(
  TEPS_TARGET_YEAR = "2024",
  TEPS_MODE        = "hv",
  TEPS_YEAR_H      = "2025",
  TEPS_YEAR_V      = "2026",
  TEPS_INST        = ""
)
ok1 <- run_script("scripts/01_prepare_input.R")

# =====================
# (02) Generate URLs
# =====================
Sys.setenv(
  TEPS_INST          = TEPS_INST,
  TEPS_SAVE_URL_CSV  = if (SAVE_URL_CSV) "1" else "0",
  TEPS_SAVE_URL_TXT  = if (SAVE_URL_TXT) "1" else "0"
)
ok2 <- run_script("scripts/02_generate_urls.R",
                  env_list = list(TEPS_INST = TEPS_INST,
                                  SAVE_URL_CSV = SAVE_URL_CSV,
                                  SAVE_URL_TXT = SAVE_URL_TXT))

# Remove TXT if disabled
if (!SAVE_URL_TXT) {
  inst_dirs <- list.dirs("data/output", recursive = FALSE, full.names = TRUE)
  inst_dirs <- inst_dirs[file.info(inst_dirs)$isdir]
  for (d in inst_dirs) {
    f <- file.path(d, "course_urls_latest.txt")
    if (file.exists(f)) { unlink(f, force = TRUE); cat("Removed: ", f, "\n", sep="") }
  }
}

# =====================
# (03) Cleanup (optional)
# =====================
if (CLEAN) {
  ok3 <- run_script("R/cleanup_outputs.R", env_list = list(dry_run = DRYRUN))
} else {
  cat("\nCleanup skipped (set TEPS_CLEAN=1 to enable).\n")
  ok3 <- TRUE
}

# =====================
# (04) Scrape (optional)
# =====================
if (RUN_SCRAPE) {
  Sys.setenv(
    TEPS_RUN_SCRAPE     = "TRUE",
    TEPS_SAVE_HTML      = if (SAVE_HTML) "TRUE" else "FALSE",
    TEPS_SAVE_TXT       = if (SAVE_TXT)  "TRUE" else "FALSE",
    TEPS_REQ_DELAY_MS   = as.character(REQ_DELAY_MS),
    TEPS_MAX_PAGES_PER_INST = as.character(MAX_PAGES_INST),
    TEPS_DRYRUN         = if (DRYRUN) "TRUE" else "FALSE",
    TEPS_INST           = TEPS_INST
  )
  ok4 <- run_script("scripts/03_scrape.R")
} else {
  ok4 <- TRUE
  cat("\nScrape skipped (set TEPS_RUN_SCRAPE=1 to enable).\n")
}

# =====================
# Finish
# =====================
cat("\nAll done. Status: ",
    if (ok1 && ok2 && ok3 && ok4) "OK" else "with errors (see above).", "\n", sep="")
