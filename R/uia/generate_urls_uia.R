# ============================================
# UIA generator (uten precheck)
# ============================================
local({
  inst_short <- "uia"
  
  # --- styringsvalg ---
  MODE   <- "hv"      # "hv" (bruk H/V fra data), "H", "V" eller "both"
  YEAR_H <- 2025L     # H??st-??r
  YEAR_V <- 2026L     # V??r-??r
  
  # --- data ---
  if (!exists("courses")) courses <- readRDS("data/cache/courses_std.RDS")
  df <- subset(courses, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short)
  
  # --- enkel templating ---
  tpl_sub <- function(pattern, env){
    out <- pattern
    for (k in names(env)) {
      out <- gsub(paste0("\\{", k, "\\}"),
                  ifelse(is.na(env[[k]]), "", env[[k]]),
                  out, perl = TRUE)
    }
    out
  }
  
  # --- YAML (m??nster for UIA) ---
  cfg  <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst) || is.null(inst$url_pattern) || !nzchar(as.character(inst$url_pattern)[1])) {
    stop("Mangler url_pattern i YAML for ", inst_short)
  }
  pattern <- as.character(inst$url_pattern)[1]
  
  # --- semesteretikett for UIA (host/var) ---
  sem_uia <- function(hv) {
    hv <- toupper(ifelse(is.na(hv), "", hv))
    ifelse(hv == "H", "host", ifelse(hv == "V", "var", ""))
  }
  
  # --- token til m??nsteret ---
  # UIA bruker {code_lower_nodashnum} => bruk basekode i lower
  df$code_lower_nodashnum <- tolower(df$code_base)
  
  # --- utvid rader i henhold til MODE ---
  expand_one <- function(i){
    have <- toupper(df$semester_hv[i])   # "H"/"V"/NA fra 01
    wants <- switch(MODE,
                    hv   = if (have %in% c("H","V")) have else c("H","V"),
                    H    = "H",
                    V    = "V",
                    both = c("H","V"),
                    have # fallback
    )
    if (!length(wants) || all(is.na(wants))) return(NULL)
    data.frame(
      hv                   = wants,
      year                 = ifelse(wants == "H", YEAR_H, YEAR_V),
      course_code_norm     = df$course_code_norm[i],
      code_lower_nodashnum = df$code_lower_nodashnum[i],
      stringsAsFactors = FALSE
    )
  }
  pieces <- lapply(seq_len(nrow(df)), expand_one)
  out <- if (length(pieces)) do.call(rbind, pieces) else df[0, ]
  if (!nrow(out)) stop("Ingenting ?? generere for ", inst_short, " (MODE=", MODE, ")")
  
  # --- bygg URL ---
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))) {
    s_lbl <- sem_uia(out$hv[i])  # "host"/"var"
    urls[i] <- tpl_sub(pattern, list(
      year                 = as.character(out$year[i]),
      semester             = s_lbl,     # (ikke brukt av UIA-m??nsteret, men ok)
      semester_url         = s_lbl,     # <- brukt av UIA
      course_code          = out$course_code_norm[i],
      code_lower_nodashnum = out$code_lower_nodashnum[i]
    ))
  }
  out$url <- urls
  
  # --- advarsel hvis tokens st??r igjen ---
  leftover <- grep("\\{[^}]+\\}", out$url, value = TRUE)
  if (length(leftover)) warning("Uerstattede tokens i noen URLer ??? sjekk YAML. Eksempel: ", leftover[1])
  
  # --- dedupe identiske URLer ---
  out <- out[!duplicated(out$url), , drop = FALSE]
  
  # --- eksport (kandidater) + sesongfiler + latest ---
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data","output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  # samlet kandidater
  csv_all <- file.path(outd, sprintf("candidates_%s_%s.csv", inst_short, ts))
  txt_all <- file.path(outd, sprintf("candidates_%s_%s.txt", inst_short, ts))
  utils::write.csv(out[, c("course_code_norm","year","hv","url")], csv_all,
                   row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(out$url[nchar(out$url) > 0], txt_all, useBytes = TRUE)
  
  # sesong-spesifikke kandidater
  split_out <- split(out, paste0(out$year, out$hv))
  for (k in names(split_out)) {
    o <- split_out[[k]]
    p_csv <- file.path(outd, sprintf("candidates_%s_%s_%s.csv", inst_short, k, ts))
    p_txt <- file.path(outd, sprintf("candidates_%s_%s_%s.txt", inst_short, k, ts))
    utils::write.csv(o[, c("course_code_norm","year","hv","url")], p_csv,
                     row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(o$url[nchar(o$url) > 0], p_txt, useBytes = TRUE)
  }
  
  # "latest"-pekere (for kandidater)
  file.copy(csv_all, file.path(outd, "candidates_latest.csv"), overwrite = TRUE)
  file.copy(txt_all, file.path(outd, "candidates_latest.txt"), overwrite = TRUE)
  
  cat("UIA candidates: ", nrow(out), " rader (MODE=", MODE, ")\n", sep = "")
})

