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
