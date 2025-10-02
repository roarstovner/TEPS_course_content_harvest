# R/uia/generate_urls_uia.R
# =========================
# UiA URL-generator med:
# - MODE-styring (hv/both/H/V/next)
# - Trygg YAML-lesing (UTF-8, fallback)
# - Semester-labels iht. "semester_style"
# - UTF-8-sikker skriving (write.csv med fileEncoding, writeLines(useBytes=TRUE))
# - "latest"-pekere for kandidater OG speil til course_urls_latest.* for 03_scrape.R
# - Dedup av URL-er + advarsel for uerstattede tokens

local({
  inst_short <- "uia"
  
  # ---------------------------
  # Styring / modus
  # ---------------------------
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  # ---------------------------
  # Hjelpere
  # ---------------------------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  # YAML leser mer robust for encoding problemer
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      yaml::read_yaml(con)
    }, silent = TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    # Fallback
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)       # dropp BOM
    txt <- gsub("\r\n?", "\n", txt)      # CRLF -> LF
    txt <- gsub("\t", "  ", txt, TRUE)   # tabs -> spaces
    yaml::yaml.load(txt)
  }
  
  # Sikker writeLines: UTF-8 + useBytes=TRUE
  safe_writeLines <- function(x, file) {
    con <- base::file(file, open = "w", encoding = "UTF-8")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    writeLines(x, con, useBytes = TRUE)
    on.exit(NULL, add = FALSE)
  }
  
  # Enkel templating
  tpl_sub <- function(pat, env) {
    out <- pat
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # ---------------------------
  # Data
  # ---------------------------
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # ---------------------------
  # Konfig fra YAML
  # ---------------------------
  cfg <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "uia")
  
  # ---------------------------
  # Semester-label i URL
  # ---------------------------
  sem_url <- function(style, hv) {
    hv <- toupper(ifelse(is.na(hv), "", hv))
    if (style %in% c("uia", "hiof", "nih")) return(ifelse(hv == "H", "host", ifelse(hv == "V", "var", "")))
    if (style == "ntnu")                   return(ifelse(hv == "H", "1",    ifelse(hv == "V", "2",   "")))
    if (style %in% c("oslomet", "plain_url")) {
      lbl <- ifelse(hv == "H", "H\u00D8ST", ifelse(hv == "V", "V\u00C5R", ""))
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                  return(ifelse(hv == "H", "H\u00F8st", ifelse(hv == "V", "V\u00E5r", "")))
    ""  # default tom
  }
  
  # ---------------------------
  # Utvid rader iht. MODE
  # ---------------------------
  expand_one <- function(i) {
    have  <- toupper(df$semester_hv[i])
    wants <- switch(
      MODE,
      both  = c("H", "V"),
      H     = "H",
      V     = "V",
      hv    = have,
      "next" = if (have == "V") "V" else "H",
      have
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv                   = wants,
      year                 = ifelse(wants == "H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_upper           = df$code_upper[i],
      code_base            = df$code_base[i],
      code_lower_nodashnum = tolower(df$code_base[i]),
      stringsAsFactors = FALSE
    )
  }
  
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0, ]
  stopifnot(nrow(out) > 0)
  
  # ---------------------------
  # Bygg URL-er
  # ---------------------------
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))) {
    s <- sem_url(semester_style, out$hv[i])
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(out$year[i]),
      semester             = s,
      semester_url         = s,
      course_code          = out$course_code_norm[i],
      code_upper           = out$code_upper[i],
      code_lower           = tolower(out$course_code_norm[i]),
      code_upper_base      = out$code_base[i],
      code_lower_base      = tolower(out$code_base[i]),
      code_lower_nodashnum = out$code_lower_nodashnum[i]
    ))
  }
  out$url <- urls
  
  # Dedup + advarsel for tokens som ikke ble erstattet
  out <- out[!duplicated(out$url), , drop = FALSE]
  if (any(grepl("\\{[^}]+\\}", out$url))) {
    warning("Uerstattede tokens ??? sjekk YAML. Eksempel: ",
            grep("\\{[^}]+\\}", out$url, value = TRUE)[1])
  }
  
  # ---------------------------
  # Eksport (UTF-8)
  # ---------------------------
  ts    <- format(Sys.time(), "%Y%m%d-%H%M")
  outd  <- file.path("data", "output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  keep_cols <- c("course_code_norm", "year", "hv", "url")
  
  # Samle-filer (alle kandidater)
  csv_all <- file.path(outd, sprintf("candidates_%s_%s.csv", inst_short, ts))
  txt_all <- file.path(outd, sprintf("candidates_%s_%s.txt", inst_short, ts))
  utils::write.csv(out[, keep_cols], csv_all, row.names = FALSE, fileEncoding = "UTF-8")
  safe_writeLines(out$url[nchar(out$url) > 0], txt_all)
  
  # Per-kombinasjon (nyttig ved troubleshooting)
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)) {
    o <- split_out[[k]]
    utils::write.csv(
      o[, keep_cols],
      file.path(outd, sprintf("candidates_%s_%s_%s.csv", inst_short, k, ts)),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    safe_writeLines(
      o$url[nchar(o$url) > 0],
      file.path(outd, sprintf("candidates_%s_%s_%s.txt", inst_short, k, ts))
    )
  }
  
  # ---------------------------
  # latest pekere (alltid oppdatert)
  # ---------------------------
  # Pekere for kandidater
  file.copy(csv_all, file.path(outd, "candidates_latest.csv"), overwrite = TRUE)
  file.copy(txt_all, file.path(outd, "candidates_latest.txt"), overwrite = TRUE)
  
  # Speiling til course_urls_* slik at 03_scrape.R plukker dem opp
  file.copy(file.path(outd, "candidates_latest.csv"),
            file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(file.path(outd, "candidates_latest.txt"),
            file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  
  cat("UIA candidates: ", nrow(out), " rader (MODE=", MODE, ")\n", sep = "")
})

