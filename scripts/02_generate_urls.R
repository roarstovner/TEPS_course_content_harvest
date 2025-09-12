# scripts/02_generate_urls.R
# Run ALL generate_urls_*.R scripts from project root, one by one.

stopifnot(dir.exists("R"), dir.exists("config"), file.exists("config/institutions.yaml"))

options(warn = 1)                 # show warnings immediately
op <- options(encoding = "UTF-8") # avoid console issues
on.exit(options(op), add = TRUE)

# 1) Find all scripts
scripts <- unique(c(
  Sys.glob(file.path("R", "generate_urls_*.R")),
  Sys.glob(file.path("R", "*", "generate_urls_*.R"))
))
if (!length(scripts)) stop("No generate_urls_*.R files found under R/")

# Sort: top-level R/ first, then subfolders
scripts <- c(
  sort(scripts[dirname(scripts) == "R"]),
  sort(scripts[dirname(scripts) != "R"])
)

cat("Found", length(scripts), "scripts to run.\n")

run_one <- function(script_path) {
  cat("\n==========================\n")
  cat(">>> Running:", script_path, "\n")
  cat("==========================\n")
  
  t0 <- Sys.time()
  res <- try(
    source(script_path, local = new.env(parent = globalenv()), echo = FALSE, chdir = FALSE),
    silent = FALSE
  )
  t1 <- Sys.time()
  
  if (inherits(res, "try-error")) {
    cat("✗ ERROR in", script_path, ":\n", as.character(res), "\n")
    return(invisible(FALSE))
  }
  
  # Guess institution code from filename: generate_urls_<inst>.R
  inst <- sub("^generate_urls_([^.]+)\\.R$", "\\1", basename(script_path))
  outd <- file.path("data", "output", inst)
  cat("Output directory:", normalizePath(outd, winslash = "/", mustWork = FALSE), "\n")
  
  if (dir.exists(outd)) {
    latest <- list.files(outd, pattern = "latest\\.(csv|txt)$", full.names = TRUE)
    if (length(latest)) {
      cat("Latest files:\n")
      for (p in latest) cat("  ", p, "\n", sep = "")
    } else {
      cat("No latest files found (script may have written timestamped files).\n")
    }
  } else {
    cat("No output directory found for", inst, "(script may have failed).\n")
  }
  
  cat(sprintf("Finished in %.1f seconds\n", as.numeric(difftime(t1, t0, units = "secs"))))
  invisible(TRUE)
}

ok <- vapply(scripts, run_one, logical(1))
cat("\nSummary: SUCCESS =", sum(ok), " | FAIL =", sum(!ok), "\n")
