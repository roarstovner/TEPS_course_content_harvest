# generate_urls_uia.R  
# ============================================
# spesiallogikk sem_uia trengtes
# problemer med å tilpasse h og v til eksisterende sider
# ============================================

local({
  inst_short <- "uia"
  
  # --- styringsvalg ---
  MODE   <- "hv"     # "hv" | "H" | "V" | "both"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  # --- liten fallback for YAML-lesing (bruker safe_read_yaml hvis den finnes) ---
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  stopifnot(nrow(df) > 0)
  
  # --- enkel templating ---
  tpl_sub <- function(pat, env){
    out <- pat
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # --- YAML ---
  cfg   <- get_cfg("config/institutions.yaml")
  inst  <- cfg$institutions[[inst_short]]
  stopifnot(!is.null(inst$url_pattern) && nzchar(as.character(inst$url_pattern)[1]))
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- if (is.null(inst$semester_style)) "uia" else as.character(inst$semester_style)[1]
  
  # --- semesteretikett (fellesfunksjon) ---
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv), "", hv))
    if (style %in% c("uia","hiof","nih"))    return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                     return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))  # HØST/VÅR
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                    return(ifelse(hv=="H","H\u00F8st", ifelse(hv=="V","V\u00E5r",""))) # Høst/Vår
    ""
  }
  
  # --- tokens (UIA trenger spesielt code_lower_nodashnum) ---
  df$code_lower_nodashnum <- tolower(df$code_base)
  
  # --- utvid rader iht MODE ---
  expand_one <- function(i){
    have  <- toupper(df$semester_hv[i])  # "H"/"V"/NA
    wants <- switch(
      MODE,
      both = c("H","V"),
      H    = "H",
      V    = "V",
      hv   = if (have %in% c("H","V")) have else c("H","V"),
      have
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv                   = wants,
      year                 = ifelse(wants=="H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_lower_nodashnum = df$code_lower_nodashnum[i],
      stringsAsFactors = FALSE
    )
  }
  parts <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(parts)) do.call(rbind, parts) else df[0,]
  stopifnot(nrow(out) > 0)
  
  # --- bygg URL ---
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))) {
    s <- sem_url(semester_style, out$hv[i])  # "host"/"var" for UIA
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(out$year[i]),
      semester             = s,   # ikke nødvendig, men ufarlig
      semester_url         = s,   # UIA bruker dette i YAML
      course_code          = out$course_code_norm[i],
      code_lower_nodashnum = out$code_lower_nodashnum[i]
    ))
  }
  out$url <- urls
  
  # dedupe + advarsel om tokens
  out <- out[!duplicated(out$url), , drop = FALSE]
  if (any(grepl("\\{[^}]+\\}", out$url)))
    warning("Uerstattede tokens – sjekk YAML. Eksempel: ",
            grep("\\{[^}]+\\}", out$url, value = TRUE)[1])
  
  # --- eksport: kandidater (samlet + per sesong) + latest ---
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output",inst_short); dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  keep_cols <- c("course_code_norm","year","hv","url")
  
  csv_all <- file.path(outd, sprintf("candidates_%s_%s.csv", inst_short, ts))
  txt_all <- file.path(outd, sprintf("candidates_%s_%s.txt", inst_short, ts))
  utils::write.csv(out[, keep_cols], csv_all, row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(out$url[nchar(out$url) > 0], txt_all, useBytes = TRUE)
  
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)) {
    o <- split_out[[k]]
    p_csv <- file.path(outd, sprintf("candidates_%s_%s_%s.csv", inst_short, k, ts))
    p_txt <- file.path(outd, sprintf("candidates_%s_%s_%s.txt", inst_short, k, ts))
    utils::write.csv(o[, keep_cols], p_csv, row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(o$url[nchar(o$url) > 0], p_txt, useBytes = TRUE)
  }
  
  # latest-pekerne
  file.copy(csv_all, file.path(outd, "candidates_latest.csv"), overwrite = TRUE)
  file.copy(txt_all, file.path(outd, "candidates_latest.txt"), overwrite = TRUE)
  
  cat("UIA candidates: ", nrow(out), " rader (MODE=", MODE, ")\n", sep = "")
})
