# generate_urls_inn.R
local({
  inst_short <- "inn"
  
  # --- hva skal ut? ---
  MODE   <- "hv"        # "hv" | "both" | "H" | "V" | "next"
  YEAR_H <- 2025L
  YEAR_V <- 2026L
  
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  get_cfg <- function(p) if (exists("safe_read_yaml")) safe_read_yaml(p) else yaml::read_yaml(p)
  
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
  
  # semester ??? label
  sem_url <- function(style, hv){
    hv <- toupper(ifelse(is.na(hv),"",hv))
    if (style %in% c("uia","hiof","nih")) return(ifelse(hv=="H","host", ifelse(hv=="V","var","")))
    if (style == "ntnu")                 return(ifelse(hv=="H","1",    ifelse(hv=="V","2","")))
    if (style %in% c("oslomet","plain_url")) {
      lbl <- ifelse(hv=="H","H\u00D8ST", ifelse(hv=="V","V\u00C5R",""))
      return(utils::URLencode(lbl, reserved = TRUE))
    }
    if (style == "plain")                return(ifelse(hv=="H","H\u00F8st", ifelse(hv=="V","V\u00E5r","")))
    ""
  }
  
  # utvid rader iht MODE
  expand_one <- function(i){
    have  <- toupper(df$semester_hv[i])  # "H"/"V"/NA
    wants <- switch(
      MODE,
      both = c("H","V"),
      H    = "H",
      V    = "V",
      hv   = have,
      "next" = if (have == "V") "V" else "H",  # NB: "next" m?? siteres
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
  
  # bygg URL
  urls <- character(nrow(out))
  for (i in seq_len(nrow(out))){
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
  
  # per-sesong (praktisk ved MODE="both")
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
