# scripts/00_run_all.R
# =====================
# Master-runner for TEPS pipeline.
#
# ========== 0. Clean ==========
# rm(list = ls(all = TRUE))   ## Rense
#
# Bruk:
#   source("scripts/00_run_all.R")
#
# Steg:
#   (01) scripts/01_prepare_input.R   -> lager renset cache (courses_std.RDS)
#   (02) scripts/02_generate_urls.R   -> kjører alle generate_urls_*.R
#   (03) R/cleanup_outputs.R          -> (valgfritt) sletter gamle filer, beholder kun *latest*

# ---- Konfig (kan overstyres med miljøvariabler) ----
CLEAN     <- TRUE     # TRUE = kjør cleanup etterpå
DRYRUN    <- FALSE    # TRUE = bare vis hva som ville slettes i cleanup
TEPS_INST <- ""       # "" = alle inst.; eller f.eks. "usn,uio"

# Hvis du vil bruke miljøvariabler:
# Sys.setenv(TEPS_CLEAN="1", TEPS_DRYRUN="0", TEPS_INST="usn,uio")

get_env_or_default <- function(var, default) {
  val <- Sys.getenv(var, "")
  if (nzchar(val)) return(val)
  default
}
CLEAN     <- tolower(get_env_or_default("TEPS_CLEAN",  if (CLEAN) "1" else "0")) %in% c("1","true","yes","y")
DRYRUN    <- tolower(get_env_or_default("TEPS_DRYRUN", if (DRYRUN) "1" else "0")) %in% c("1","true","yes","y")
TEPS_INST <- get_env_or_default("TEPS_INST", TEPS_INST)

options(stringsAsFactors = FALSE, warn = 1)

# ---- Finn prosjektrot og gå dit ----
find_root <- function(marker = "config/institutions.yaml", max_up = 6) {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 0:max_up) {
    if (file.exists(file.path(d, marker))) return(d)
    d <- dirname(d)
  }
  stop("Fant ikke prosjektrot (mangler ", marker, ").")
}
proj <- find_root(); setwd(proj)

# ---- Utskriftsnytte ----
banner <- function(x){
  cat("\n", paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
  cat("=== ", x, " ===\n", sep="")
  cat(paste(rep("=", nchar(x)+8), collapse=""), "\n", sep="")
}

# ---- Kjør et skript (med helpers tilgjengelig) ----
run_script <- function(path, env = list()) {
  if (!file.exists(path)) {
    cat("Skip (missing): ", path, "\n", sep = "")
    return(invisible(FALSE))
  }
  banner(paste("Running", path))
  t0 <- Sys.time()
  e <- list2env(env, parent = globalenv())
  
  # --- helpers for scripts that expect them ---
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
      txt <- sub("^\ufeff", "", txt)        # fjern BOM
      txt <- gsub("\r\n?", "\n", txt)       # normaliser CRLF/CR
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
      { source(path, local = e, echo = FALSE, chdir = FALSE) },
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

# ---- Run all steps ----
cat("Prosjektrot: ", proj, "\n", sep="")
cat("TEPS_CLEAN=", CLEAN, "  TEPS_DRYRUN=", DRYRUN,
    if (nzchar(TEPS_INST)) paste0("  TEPS_INST=", TEPS_INST) else "", "\n", sep="")

# (01) Forbered cache
ok1 <- run_script("scripts/01_prepare_input.R")

# (02) Generér URLer
Sys.setenv(TEPS_INST = TEPS_INST)
ok2 <- run_script("scripts/02_generate_urls.R", env = list(TEPS_INST = TEPS_INST))

# (03) Opprydding
if (CLEAN) {
  ok3 <- run_script("R/cleanup_outputs.R", env = list(dry_run = DRYRUN))
} else {
  cat("\nOpprydding hoppet over (sett TEPS_CLEAN=1 for å aktivere).\n")
  ok3 <- TRUE
}

cat("\nAlt ferdig. Status: ",
    if (ok1 && ok2 && ok3) "OK" else "med feil (se over).", "\n", sep="")
