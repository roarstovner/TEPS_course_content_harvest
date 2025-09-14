# generate_urls_hiof.R
local({
  inst_short <- "hiof"
  
  # --- styring ---
  MODE   <- "hv"   # "both" | "single" | "next" | "hv"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # --- templating ---
  tpl_sub <- function(pat, env) {
    out <- pat
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # --- yaml + stil ---
  cfg <- get_cfg("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "hiof")
  
  # --- semesteretikett ---
  sem_url <- function(style, hv) {
    hv <- toupper(ifelse(is.na(hv), "", hv))
    
    if (style %in% c("uia", "hiof", "nih")) {
      return(ifelse(hv == "H", "host", ifelse(hv == "V", "var", "")))
    }
    if (style == "ntnu") {
      return(ifelse(hv == "H", "1", ifelse(hv == "V", "2", "")))
    }
    if (style %in% c("oslomet", "plain_url")) {
      lbl <- ifelse(hv == "H", "H??ST", ifelse(hv == "V", "V??R", ""))
      return(URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain") {
      return(ifelse(hv == "H", "H??st", ifelse(hv == "V", "V??r", "")))
    }
    ""
  }
  
  # --- tokens ---
  df$code_lower_nodashnum <- tolower(df$code_base)
  df$code_upper_nodashnum <- df$code_base
  
  # --- utvid rader ---
  expand_one <- function(i) {
    have <- toupper(df$semester_hv[i])
    wants <- switch(
      MODE,
      both  = c("H", "V"),
      H     = "H",
      V     = "V",
      hv    = have,
      "next" = if (have == "V") "V" else "H",  # <
# generate_urls_hivolda.R
local({
  inst_short <- "hivolda"
  
  # --- hva skal ut? (??r/hv brukes kun til metadata i CSV) ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) out <- gsub(paste0("\\{",k,"\\}"),
                                      ifelse(is.na(env[[k]]),"",env[[k]]),
                                      out, perl=TRUE)
    out
  }
  
  cfg <- get_cfg("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "plain")
  
  # HVO: URLen har ikke ??r/semester ??? men vi tar dem med som metadata
  expand_one <- function(i){
    have  <- toupper(df$semester_hv[i])
    wants <- switch(
      MODE,
      both = c("H","V"),
      H    = "H",
      V    = "V",
      hv   = have,
      "next" = if (have == "V") "V" else "H",
      have
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv   = wants,
      year = ifelse(wants=="H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_upper           = df$code_upper[i],
      code_base            = df$code_base[i],
      code_upper_nodashnum = df$code_base[i],         # {code_upper_nodashnum}
      stringsAsFactors = FALSE
    )
  }
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0,]
  stopifnot(nrow(out) > 0)
  
  # bygg URL (uten ??r/semester)
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))){
    urls[i] <- tpl_sub(pattern, list(
      course_code          = out$course_code_norm[i],
      code_upper           = out$code_upper[i],
      code_lower           = tolower(out$course_code_norm[i]),
      code_upper_base      = out$code_base[i],
      code_lower_base      = tolower(out$code_base[i]),
      code_upper_nodashnum = out$code_upper_nodashnum[i]
    ))
  }
  out$url <- urls
  out <- out[!duplicated(out$url), , drop=FALSE]
  
  if (any(grepl("\\{[^}]+\\}", out$url))) {
    warning("Uerstattede tokens ??? sjekk YAML. Eksempel: ",
            grep("\\{[^}]+\\}", out$url, value=TRUE)[1])
  }
  
  # eksport
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output",inst_short); dir.create(outd, TRUE, TRUE)
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  write.csv(out[,c("course_code_norm","year","hv","url")], csv_ts, row.names=FALSE, fileEncoding="UTF-8")
  writeLines(out$url[nchar(out$url)>0], txt_ts, useBytes=TRUE)
  
  # per-sesong (kun for oversikt)
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)){
    o <- split_out[[k]]
    write.csv(o[,c("course_code_norm","year","hv","url")],
              file.path(outd, sprintf("course_urls_%s_%s_%s.csv", inst_short, k, ts)),
              row.names=FALSE, fileEncoding="UTF-8")
    writeLines(o$url[nchar(o$url)>0],
               file.path(outd, sprintf("course_urls_%s_%s_%s.txt", inst_short, k, ts)),
               useBytes=TRUE)
  }
  
  file.copy(csv_ts, file.path(outd,"course_urls_latest.csv"), overwrite=TRUE)
  file.copy(txt_ts, file.path(outd,"course_urls_latest.txt"), overwrite=TRUE)
  cat("Skrev filer for ", inst_short, " (", nrow(out), " rader; MODE=", MODE, ")\n", sep="")
})
# ============================================
# generate_urls_hvl.R
# ============================================
local({
  inst_short <- "hvl"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(pattern, env) {
    out <- pattern
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, fixed = TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# R/inn/generate_urls_inn.R
local({
  inst_short <- "inn"
  
  # --- hva skal ut? ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # enkel templating
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) out <- gsub(paste0("\\{",k,"\\}"),
                                      ifelse(is.na(env[[k]]),"",env[[k]]),
                                      out, perl=TRUE)
    out
  }
  
  # YAML
  cfg <- get_cfg("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "uia")  # host/var i URL
  
  # semester-label
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv),"",hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                 return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))  # H
# ============================================
# generate_urls_mf.R  (lowercase + nodashnum)
# ============================================
local({
  inst_short <- "mf"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# generate_urls_nih.R
local({
  inst_short <- "nih"
  
  # --- hva skal ut? ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) out <- gsub(paste0("\\{",k,"\\}"),
                                      ifelse(is.na(env[[k]]),"",env[[k]]),
                                      out, perl=TRUE)
    out
  }
  
  cfg <- get_cfg("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "nih")  # host/var i URL
  
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv),"",hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                 return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H??ST", ifelse(hv=="V","V??R",""))
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                return(ifelse(hv=="H","H??st", ifelse(hv=="V","V??r","")))
    ""
  }
  
  df$code_lower_nodashnum <- tolower(df$code_base)
  
  expand_one <- function(i){
    have  <- toupper(df$semester_hv[i])
    wants <- switch(
      MODE,
      both = c("H","V"),
      H    = "H",
      V    = "V",
      hv   = have,
      "next" = if (have == "V") "V" else "H",
      have
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv   = wants,
      year = ifelse(wants=="H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_upper           = df$code_upper[i],
      code_base            = df$code_base[i],
      code_lower_nodashnum = tolower(df$code_base[i]),
      stringsAsFactors = FALSE
    )
  }
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0,]
  stopifnot(nrow(out) > 0)
  
  # Bygg URL (m??nster: https://www.nih.no/studier/emner/{year}/{semester_url}/{code_lower_nodashnum}.html)
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))){
    s <- sem_url(semester_style, out$hv[i])  # "host"/"var"
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(out$year[i]),
      semester             = s,
      semester_url         = s,
      course_code          = out$course_code_norm[i],
      code_lower_nodashnum = out$code_lower_nodashnum[i],
      code_upper           = out$code_upper[i],
      code_lower           = tolower(out$course_code_norm[i]),
      code_upper_base      = out$code_base[i],
      code_lower_base      = tolower(out$code_base[i])
    ))
  }
  out$url <- urls
  out <- out[!duplicated(out$url), , drop=FALSE]
  
  if (any(grepl("\\{[^}]+\\}", out$url))) {
    warning("Uerstattede tokens ??? sjekk YAML. Eksempel: ",
            grep("\\{[^}]+\\}", out$url, value=TRUE)[1])
  }
  
  # eksport
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output",inst_short); dir.create(outd, TRUE, TRUE)
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  write.csv(out[,c("course_code_norm","year","hv","url")], csv_ts, row.names=FALSE, fileEncoding="UTF-8")
  writeLines(out$url[nchar(out$url)>0], txt_ts, useBytes=TRUE)
  
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)){
    o <- split_out[[k]]
    write.csv(o[,c("course_code_norm","year","hv","url")],
              file.path(outd, sprintf("course_urls_%s_%s_%s.csv", inst_short, k, ts)),
              row.names=FALSE, fileEncoding="UTF-8")
    writeLines(o$url[nchar(o$url)>0],
               file.path(outd, sprintf("course_urls_%s_%s_%s.txt", inst_short, k, ts)),
               useBytes=TRUE)
  }
  
  file.copy(csv_ts, file.path(outd,"course_urls_latest.csv"), overwrite=TRUE)
  file.copy(txt_ts, file.path(outd,"course_urls_latest.txt"), overwrite=TRUE)
  cat("Skrev filer for ", inst_short, " (", nrow(out), " rader; MODE=", MODE, ")\n", sep="")
})
# generate_urls_nla.R
local({
  inst_short <- "nla"
  
  # --- hva skal ut? ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # --- enkel templating ---
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) {
      out <- gsub(paste0("\\{",k,"\\}"),
                  ifelse(is.na(env[[k]]),"",env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # --- YAML ---
  cfg   <- get_cfg("config/institutions.yaml")
  inst  <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "plain")
  
  # --- semester-etikett (ASCII-sikker) ---
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv),"",hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                  return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))   # H
# ============================================
# generate_urls_nmbu.R  ({code_upper_nodash1} == basekode)
# ============================================
local({
  inst_short <- "nmbu"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# ============================================
# generate_urls_nord.R  (lowercase + nodashnum)
# ============================================
local({
  inst_short <- "nord"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# ============================================
# generate_urls_ntnu.R  (fjern -1 eller -A: {code_upper_nosfx1A})
# ============================================
local({
  inst_short <- "ntnu"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# R/run/generate_urls_oslomet.R ??? mini (base-R) med MODE-bryter

local({
  inst_short <- "oslomet"
  
  # ====== KONFIG FOR HVILKE SESONGER DU VIL HA ======
  MODE <- "single"          # "both" | "single" | "next"
  SINGLE_YEAR <- 2025L     # brukes n??r MODE == "single"
  SINGLE_HV   <- "H"       # "H" eller "V" for MODE == "single"
  
  # ====== DATA ======
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short","course_code_norm","code_upper","code_base",
              "semester_hv","year","semester_name_raw")
    x <- if (exists("courses_std")) {
      courses_std
    } else if (exists("courses")) {
      courses
    } else {
      readRDS(path)
    }
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler kolonner: ", paste(miss, collapse = ", "))
    x
  }
  courses0 <- get_courses_std()
  df <- subset(courses0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short, " i cache.")
  
  # ====== liten templater ======
  tpl_sub <- function(pattern, env) {
    out <- pattern
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # ====== robust YAML-leser ======
  safe_read_yaml <- function(path) {
    try_utf8 <- try({
      con <- file(path, open = "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      yaml::read_yaml(con)
    }, silent = TRUE)
    if (!inherits(try_utf8, "try-error") && !is.null(try_utf8)) return(try_utf8)
    
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)              # fjern BOM
    txt <- gsub("\r\n?", "\n", txt)             # CRLF/CR -> LF
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    if (length(lines) == 0 || nzchar(tail(lines, 1))) lines <- c(lines, "")
    lines <- gsub("\t", "  ", lines, fixed = TRUE)  # tabs -> spaces
    yaml::yaml.load(paste(lines, collapse = "\n"))
  }
  
  # ====== hent m??nster + stil fra YAML ======
  cfg  <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst) || is.null(inst$url_pattern) || !nzchar(as.character(inst$url_pattern)[1])) {
    stop("Mangler url_pattern i YAML for ", inst_short)
  }
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- if (is.null(inst$semester_style)) "plain" else as.character(inst$semester_style)[1]
  
  # ====== semester-label for {semester}/{semester_url} ======
  sem_url <- function(style, hv, raw) {
    hv <- toupper(ifelse(is.na(hv), "", hv))
    if (style == "ntnu")                     return(ifelse(hv == "H", "1",        ifelse(hv == "V", "2",        "")))
    if (style %in% c("uia","hiof","nih"))    return(ifelse(hv == "H", "host",     ifelse(hv == "V", "var",      "")))
    if (style %in% c("oslomet","plain_url")) {
      # Oslomet: H??ST/V??R (uppercase) URL-enkodet
      lbl <- ifelse(hv == "H", "H\u00D8ST", ifelse(hv == "V", "V\u00C5R", ""))
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                    return(ifelse(hv == "H", "H\u00F8st", ifelse(hv == "V", "V\u00E5r",  raw)))
    raw
  }
  
  # ====== targets (avhenger av MODE) ======
  build_rows <- switch(
    MODE,
    "both" = {
      function(i) {
        data.frame(
          year = c(2025L, 2026L),
          hv   = c("H",   "V"),
          stringsAsFactors = FALSE
        )
      }
    },
    "single" = {
      function(i) {
        data.frame(year = SINGLE_YEAR, hv = toupper(SINGLE_HV), stringsAsFactors = FALSE)
      }
    },
    "next" = {
      function(i) {
        # H i historikk -> 2025/H, V -> 2026/V, annet -> 2025/H
        hv_i <- toupper(df$semester_hv[i])
        if (hv_i == "V") {
          data.frame(year = 2026L, hv = "V", stringsAsFactors = FALSE)
        } else {
          data.frame(year = 2025L, hv = "H", stringsAsFactors = FALSE)
        }
      }
    },
    stop("Ukjent MODE: ", MODE)
  )
  
  # token {code_upper_nodashnum} = basekode (UPPER, uten avsluttende -/_/. + siffer)
  df$code_upper_nodashnum <- df$code_base
  
  # ====== utvid til m??l-rader ======
  expand_one <- function(i) {
    tgt <- build_rows(i)
    if (!nrow(tgt)) return(NULL)
    data.frame(
      year                 = tgt$year,
      hv                   = tgt$hv,
      course_code_norm     = df$course_code_norm[i],
      code_upper           = df$code_upper[i],
      code_base            = df$code_base[i],
      code_upper_nodashnum = df$code_upper_nodashnum[i],
      stringsAsFactors = FALSE
    )
  }
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0, ]
  if (!nrow(out)) stop("Ingen rader ?? generere for ", inst_short, ".")
  
  # ====== bygg URL ======
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))) {
    s_lbl <- sem_url(semester_style, out$hv[i], "")
    urls[i] <- tpl_sub(pattern, list(
      year                  = as.character(out$year[i]),
      semester              = s_lbl,
      semester_url          = s_lbl,
      course_code           = out$course_code_norm[i],
      code_upper            = out$code_upper[i],
      code_lower            = tolower(out$course_code_norm[i]),
      code_upper_base       = out$code_base[i],
      code_lower_base       = tolower(out$code_base[i]),
      code_upper_nodashnum  = out$code_upper_nodashnum[i]
    ))
  }
  out$url <- urls
  
  # sikkerhets-deduplisering hvis noe skulle bli identisk:
  out <- out[!duplicated(out$url), , drop = FALSE]
  
  # ====== advarsel om evt. uerstattede tokens ======
  if (any(grepl("\\{[^}]+\\}", out$url))) {
    bad <- grep("\\{[^}]+\\}", out$url, value = TRUE)[1]
    warning("Uerstattede tokens i noen URLer ??? sjekk YAML. Eksempel: ", bad)
  }
  
  # ====== eksport ======
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data", "output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  # samlet
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  write.csv(out[, c("course_code_norm","year","hv","url")],
            csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(out$url[nchar(out$url) > 0], txt_ts, useBytes = TRUE)
  # sesong-spesifikke filer (praktisk hvis MODE="both")
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)) {
    o <- split_out[[k]]
    p_csv <- file.path(outd, sprintf("course_urls_%s_%s_%s.csv", inst_short, k, ts))
    p_txt <- file.path(outd, sprintf("course_urls_%s_%s_%s.txt", inst_short, k, ts))
    write.csv(o[, c("course_code_norm","year","hv","url")], p_csv, row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(o$url[nchar(o$url) > 0], p_txt, useBytes = TRUE)
  }
  
  # "latest"-pekere
  file.copy(csv_ts, file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(txt_ts, file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  
  cat("Skrev filer for ", inst_short, " (", nrow(out), " rader; MODE=", MODE, ")\n", sep = "")
})
# generate_urls_uia.R  
# ============================================
# spesiallogikk sem_uia trengtes
# problemer med 
# ============================================
# generate_urls_uib.R  (lowercase + nodashnum)
# ============================================
local({
  inst_short <- "uib"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# R/run/generate_urls_uio.R ??? mini (base-R), kun URL-bygging for UiO

local({
  inst_short <- "uio"
  
  # ---------- hent standardisert datasett ----------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short","course_code_norm","code_upper","code_base",
              "year","semester_name_raw","semester_hv","faculty_name","field_name")
    x <- if (exists("courses_std")) {
      courses_std
    } else if (exists("courses")) {
      courses
    } else {
      readRDS(path)
    }
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler kolonner: ", paste(miss, collapse = ", "))
    x
  }
  courses0 <- get_courses_std()
  df <- subset(courses0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short, " i cache.")
  
  # ---------- helpers ----------
  norm_key <- function(x) {
    x <- ifelse(is.na(x), "", x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")   # ?????? ??? ae/oe/aa, etc
    tolower(trimws(gsub("\\s+", " ", x)))
  }
  tpl_sub <- function(pattern, env) {
    out <- pattern
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # ---------- UiO slug-maps ----------
  uio_fac_map <- c(
    "det humanistiske fakultet"="hf","det juridiske fakultet"="jus",
    "det matematisk-naturvitenskapelige fakultet"="matnat","det medisinske fakultet"="med",
    "det odontologiske fakultet"="odont","det samfunnsvitenskapelige fakultet"="sv",
    "det teologiske fakultet"="teologi","det utdanningsvitenskapelige fakultet"="uv"
  )
  uio_inst_map <- list(
    # HF
    "institutt for arkeologi, konservering og historiske studier"=c("hf","iakh"),
    "institutt for kulturstudier og orientalske sprak"           =c("hf","ikos"),
    "institutt for filosofi, ide- og kunsthistorie og klassiske sprak"=c("hf","ifikk"),
    "institutt for litteratur, omradestudier og europeiske sprak"=c("hf","ilos"),
    "institutt for lingvistiske og nordiske studier"             =c("hf","iln"),
    "institutt for medier og kommunikasjon"                      =c("hf","imk"),
    # SV
    "institutt for sosiologi og samfunnsgeografi"                =c("sv","iss"),
    "institutt for statsvitenskap"                               =c("sv","statsvitenskap"),
    "psykologisk institutt"                                      =c("sv","psi"),
    "sosialantropologisk institutt"                              =c("sv","sai"),
    "okonomisk institutt"                                        =c("sv","oekonomi"),
    # MATNAT
    "institutt for informatikk"                                  =c("matnat","ifi"),
    "institutt for geofag"                                       =c("matnat","geofag"),
    "kjemisk institutt"                                          =c("matnat","kjemi"),
    "fysisk institutt"                                           =c("matnat","fys"),
    "matematisk institutt"                                       =c("matnat","math"),
    "institutt for biovitenskap"                                 =c("matnat","ibv"),
    "farmasoytisk institutt"                                     =c("matnat","farmasi"),
    "institutt for teoretisk astrofysikk"                        =c("matnat","astro"),
    "institutt for teknologisystemer"                            =c("matnat","its"),
    # MED
    "institutt for helse og samfunn"                             =c("med","helsam"),
    "institutt for medisinske basalfag"                          =c("med","imb"),
    "institutt for klinisk medisin"                              =c("med","klinmed"),
    # ODONT
    "institutt for oral biologi"                                 =c("odont","iob"),
    "institutt for klinisk odontologi"                           =c("odont","iko"),
    # UV
    "institutt for larerutdanning og skoleforskning"             =c("uv","ils"),
    "institutt for spesialpedagogikk"                            =c("uv","isp"),
    "institutt for pedagogikk"                                   =c("uv","iped"),
    "cemo - centre for educational measurement"                  =c("uv","cemo"),
    # Generiske (noen emner ligger p?? fakultetet)
    "det samfunnsvitenskapelige fakultet"                        =c("sv","sv"),
    "det teologiske fakultet"                                    =c("teologi","tf")
  )
  legacy_inst_map <- function(inst_name, field_name) {
    n <- norm_key(inst_name); f <- norm_key(field_name)
    if (grepl("nordistikk|nordisk", n)) {
      if (grepl("fransk|spansk|engelsk|italiensk|portugisisk|europeiske", f)) return(c("hf","ilos"))
      return(c("hf","iln"))
    }
    if (grepl("klassisk", n) || grepl("romansk", n)) {
      if (grepl("fransk|spansk|italiensk|portugisisk|romansk", f)) return(c("hf","ilos"))
      if (grepl("latin|gresk|klassisk", f)) return(c("hf","ifikk"))
      return(c("hf","ifikk"))
    }
    if (grepl("^biologisk institutt$", n) || grepl("molekylaer biovitenskap", n)) return(c("matnat","ibv"))
    if (grepl("naturfagsenteret", n)) return(c("matnat","naturfagsenteret"))
    c(NA_character_, NA_character_)
  }
  
  # ---------- lag slugs ----------
  df$uio_faculty_slug <- NA_character_
  df$uio_inst_slug    <- NA_character_
  fac_nm <- norm_key(df$faculty_name)
  
  hit <- match(fac_nm, names(uio_inst_map))
  has <- !is.na(hit)
  if (any(has)) {
    pair <- do.call(rbind, uio_inst_map[hit[has]])
    df$uio_faculty_slug[has] <- pair[,1]
    df$uio_inst_slug[has]    <- pair[,2]
  }
  
  need <- is.na(df$uio_faculty_slug) | is.na(df$uio_inst_slug)
  if (any(need)) {
    fac_left <- df$faculty_name[need]
    fld_left <- if ("field_name" %in% names(df)) df$field_name[need] else rep(NA_character_, sum(need))
    pair2 <- t(mapply(legacy_inst_map, inst_name = fac_left, field_name = fld_left))
    if (is.matrix(pair2) && nrow(pair2)) {
      idx <- which(need)
      fill_fac  <- is.na(df$uio_faculty_slug[need])
      fill_inst <- is.na(df$uio_inst_slug[need])
      df$uio_faculty_slug[idx[fill_fac]] <- pair2[fill_fac, 1]
      df$uio_inst_slug[idx[fill_inst]]   <- pair2[fill_inst, 2]
    }
  }
  
  fac_only <- is.na(df$uio_faculty_slug) & !is.na(df$faculty_name)
  if (any(fac_only)) {
    guess <- uio_fac_map[norm_key(df$faculty_name[fac_only])]
    ok <- !is.na(guess)
    df$uio_faculty_slug[which(fac_only)[ok]] <- guess[ok]
  }
  
  # ---------- robust YAML-leser ----------
  safe_read_yaml <- function(path) {
    try_utf8 <- try({
      con <- file(path, open = "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      yaml::read_yaml(con)
    }, silent = TRUE)
    if (!inherits(try_utf8, "try-error") && !is.null(try_utf8)) return(try_utf8)
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    if (length(lines) == 0 || nzchar(tail(lines, 1))) lines <- c(lines, "")
    lines <- gsub("\t", "  ", lines, fixed = TRUE)
    yaml::yaml.load(paste(lines, collapse = "\n"))
  }
  
  # ---------- hent m??nster (fra YAML) ----------
  cfg  <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst) || is.null(inst$url_pattern) || !nzchar(as.character(inst$url_pattern)[1])) {
    stop("Mangler url_pattern i YAML for ", inst_short)
  }
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- if (is.null(inst$semester_style)) "plain" else as.character(inst$semester_style)[1]
  # (UiO bruker normalt ikke semester i path, men vi st??tter {semester}/{semester_url} hvis m??nsteret trenger det)
  
  # ---------- tokens (UiO) ----------
  # Bruk basekode (UPPER, uten trailing -/_/.+digits) for {code_upper_nodash1}
  df$code_upper_nodash1 <- df$code_base
  
  # ---------- bygg URL ----------
  urls <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(df$year[i]),
      semester             = df$semester_name_raw[i],
      semester_url         = "",  # UiO: tomt (ingen semester i path)
      course_code          = df$course_code_norm[i],
      code_upper           = df$code_upper[i],
      code_lower           = tolower(df$course_code_norm[i]),
      code_upper_base      = df$code_base[i],
      code_lower_base      = tolower(df$code_base[i]),
      code_upper_nodash1   = df$code_upper_nodash1[i],
      uio_faculty_slug     = df$uio_faculty_slug[i],
      uio_inst_slug        = df$uio_inst_slug[i]
    ))
  }
  df$url <- urls
  
  df$hv <- ""  # UiO bruker ikke H/V i path; tom streng er fint
  
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  
  keep <- c("course_code_norm","year","hv","url")
  write.csv(df[, keep, drop = FALSE], csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(df$url[nchar(df$url) > 0], txt_ts, useBytes = TRUE)
  
  file.copy(csv_ts, file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(txt_ts, file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  cat("Skrev filer for uio (", nrow(df), " rader)\n", sep = "")
  
  
  # ---------- advarsel om uerstattede tokens ----------
  leftover <- grep("\\{[^}]+\\}", df$url, value = TRUE)
  if (length(leftover)) warning("Uerstattede tokens i noen URLer ??? sjekk YAML/slug-maps. Eksempel: ", leftover[1])
  
  # ---------- eksport ----------
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  
  keep <- intersect(c("course_code_norm","year","uio_faculty_slug","uio_inst_slug","url"), names(df))
  write.csv(df[, keep, drop = FALSE], csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(df$url[nchar(df$url) > 0], txt_ts, useBytes = TRUE)
  
  # latest-peker
  file.copy(csv_ts, file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(txt_ts, file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  
  cat("Skrev filer for ", inst_short, " (", nrow(df), " rader)\n", sep = "")
})

# ============================================
# generate_urls_uis.R
# ============================================
local({
  inst_short <- "uis"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# ============================================
# generate_urls_uit.R  ({code_upper_nodash1} == basekode)
# ============================================
local({
  inst_short <- "uit"
  
  # ---------------------------
  # 1) Data
  # ---------------------------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short", "course_code_norm", "code_upper", "code_base", "year", "semester_hv")
    x <- if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler: ", paste(miss, collapse = ", "))
    x
  }
  
  df0 <- get_courses_std()
  df  <- subset(df0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # ---------------------------
  # 2) Hjelpere
  # ---------------------------
  tpl_sub <- function(p, e) {
    out <- p
    for (k in names(e)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(e[[k]]), "", e[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      yaml::read_yaml(con)
    }, TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, TRUE)
    yaml::yaml.load(txt)
  }
  
  # ---------------------------
  # 3) M
# generate_urls_usn.R
local({
  inst_short <- "usn"
  
  # --- hva skal ut? ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # --- enkel templating ---
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) {
      out <- gsub(paste0("\\{",k,"\\}"),
                  ifelse(is.na(env[[k]]),"",env[[k]]),
                  out, perl=TRUE)
    }
    out
  }
  
  # --- YAML ---
  cfg   <- get_cfg("config/institutions.yaml")
  inst  <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(inst$url_pattern))
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "plain")
  
  # --- semester-etikett (ASCII-sikker) ---
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv),"",hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                  return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))      # H
