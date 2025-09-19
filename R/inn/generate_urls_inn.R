# R/inn/generate_urls_inn.R
# =========================
# INN URL-generator:
# - MODE-styring (hv/both/H/V/next) med YEAR_H/YEAR_V
# - Trygg YAML-lesing (UTF-8 + fallback)
# - Rensing av kode-suffiks: fjerner -1/-A fra baade course_code (til URL) og code_upper
# - UTF-8-sikker skriving (write.csv med fileEncoding, writeLines(useBytes=TRUE))
# - Dedup + varsling om uerstattede tokens
# - Oppdaterer course_urls_latest.* slik at 03_scrape.R plukker dem opp

local({
  inst_short <- "inn"
  
  # ---------------------------
  # Styring
  # ---------------------------
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  # ---------------------------
  # Hjelpere (encoding/IO)
  # ---------------------------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      yaml::read_yaml(con)
    }, silent = TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)        # dropp BOM
    txt <- gsub("\r\n?", "\n", txt)       # CRLF -> LF
    txt <- gsub("\t", "  ", txt, TRUE)    # tabs -> spaces
    yaml::yaml.load(txt)
  }
  
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
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  cfg  <- get_cfg("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "uia")  # 'h'/'v' i URL
  
  # ---------------------------
  # Semester-label i URL
  # ---------------------------
  sem_url <- function(style, hv) {
    hv <- toupper(ifelse(is.na(hv), "", hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                  return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))  # H/V
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                 return(ifelse(hv=="H","H\u00F8st", ifelse(hv=="V","V\u00E5r",""))) # H/V
    ""
  }
  
  # ---------------------------
  # Utvid rader iht MODE
  # ---------------------------
  expand_one <- function(i) {
    have  <- toupper(df$semester_hv[i])  # "H"/"V"/NA
    wants <- switch(
      MODE,
      both  = c("H","V"),
      H     = "H",
      V     = "V",
      hv    = have,
      "next" = if (have == "V") "V" else "H",  # NB: "next" maa siteres
      have
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv                   = wants,
      year                 = ifelse(wants=="H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_upper           = df$code_upper[i],  # renses senere
      code_base            = df$code_base[i],
      code_lower_nodashnum = tolower(df$code_base[i]),
      stringsAsFactors = FALSE
    )
  }
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0, ]
  stopifnot(nrow(out) > 0)
  
  # ---------------------------
  # Rens kode-suffiks til base 
  # ---------------------------
  # Fjerner trailing "-<alfa/num>" (f.eks. -1, -A, -2B); robustere enn strip1A
  strip_suffix <- function(x) sub("-[0-9A-Za-z]+$", "", x)
  
  # Spesifikk patch: fjern -1/-A fra code_upper slik at token matcher INNs URL-struktur
  strip1A <- function(x) sub("-(1|A)$", "", x)
  
  base_codes        <- strip_suffix(out$course_code_norm)  # til bruk i 'course_code' token
  code_upper_clean  <- strip1A(out$code_upper)             # til bruk i 'code_upper' token
  
  trimmed_code_n    <- sum(base_codes       != out$course_code_norm, na.rm = TRUE)
  trimmed_upper_n   <- sum(code_upper_clean != out$code_upper,       na.rm = TRUE)
  
  # ---------------------------
  # Bygg URL-er
  # ---------------------------
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))) {
    s <- sem_url(semester_style, out$hv[i])
    base_code <- base_codes[i]           # course_code uten -1/-A
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(out$year[i]),
      semester             = s,
      semester_url         = s,
      course_code          = base_code,                         # <- bruk base_code uten suffiks
      code_upper           = code_upper_clean[i],               # <- renset code_upper
      code_lower           = tolower(base_code),
      code_upper_base      = out$code_base[i],
      code_lower_base      = tolower(out$code_base[i]),
      code_lower_nodashnum = out$code_lower_nodashnum[i]
    ))
  }
  out$url <- urls
  
  # ---------------------------
  # Dedup + varsel om uerstattede tokens
  # ---------------------------
  out <- out[!duplicated(out$url), , drop = FALSE]
  if (any(grepl("\\{[^}]+\\}", out$url))) {
    warning("Uerstattede tokens, sjekk YAML. Eksempel: ",
            grep("\\{[^}]+\\}", out$url, value = TRUE)[1])
  }
  
  # ---------------------------
  # Eksport (UTF-8)
  # ---------------------------
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output",inst_short); dir.create(outd, TRUE, TRUE)
  
  keep_cols <- c("course_code_norm","year","hv","url")
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  utils::write.csv(out[, keep_cols], csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  safe_writeLines(out$url[nchar(out$url) > 0], txt_ts)
  
  # Per-sesong (praktisk ved MODE="both")
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)) {
    o <- split_out[[k]]
    utils::write.csv(o[, keep_cols],
                     file.path(outd, sprintf("course_urls_%s_%s_%s.csv", inst_short, k, ts)),
                     row.names = FALSE, fileEncoding = "UTF-8")
    safe_writeLines(o$url[nchar(o$url) > 0],
                    file.path(outd, sprintf("course_urls_%s_%s_%s.txt", inst_short, k, ts)))
  }
  
  # Oppdater latest pekere (det er disse 03_scrape.R leter etter)
  file.copy(csv_ts, file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(txt_ts, file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  
  cat("Skrev filer for ", inst_short,
      " (", nrow(out), " rader; MODE=", MODE,
      "; strip_course_code=", trimmed_code_n,
      "; strip_code_upper=", trimmed_upper_n, ")\n", sep = "")
})
