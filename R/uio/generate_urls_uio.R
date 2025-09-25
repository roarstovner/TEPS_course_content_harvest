# R/uio/generate_urls_uio.R
# =========================
# UiO URL-generator (base R)
# - Leser standardisert cache (courses_std.RDS)
# - Lager fakultets-/institutt-slugs
# - Bygger URL-er ut fra YAML-pattern
# - UTF-8 trygg I/O, dedupe, token-sjekk
# - Eksporterer CSV/TXT + oppdaterer course_urls_latest.*

local({
  inst_short <- "uio"
  
  # ---------- trygge hjelpere ----------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  norm_key <- function(x) {
    x <- ifelse(is.na(x), "", x)
    # translitterer norske tegn der mulig; lar heller YAML pattern definere korrekt slug 
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    tolower(trimws(gsub("\\s+", " ", x)))
  }
  
  safe_read_yaml <- function(path) {
    tr <- try({
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      yaml::read_yaml(con)
    }, silent = TRUE)
    if (!inherits(tr, "try-error") && !is.null(tr)) return(tr)
    
    raw <- readBin(path, "raw", file.info(path)$size)
    txt <- rawToChar(raw); Encoding(txt) <- "unknown"
    txt <- sub("^\ufeff", "", txt)
    txt <- gsub("\r\n?", "\n", txt)
    txt <- gsub("\t", "  ", txt, fixed = TRUE)
    yaml::yaml.load(txt)
  }
  
  safe_writeLines <- function(x, file) {
    con <- base::file(file, open = "w", encoding = "UTF-8")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    writeLines(x, con, useBytes = TRUE)
    on.exit(NULL, add = FALSE)
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
  
  # ---------- hent cache ----------
  get_courses_std <- function(path = "data/cache/courses_std.RDS") {
    need <- c("institution_short","course_code_norm","code_upper","code_base",
              "year","semester_name_raw","semester_hv","faculty_name","field_name")
    x <- if (exists("courses_std")) courses_std else if (exists("courses")) courses else readRDS(path)
    miss <- setdiff(need, names(x))
    if (length(miss)) stop("Cache mangler kolonner: ", paste(miss, collapse = ", "))
    x
  }
  
  courses0 <- get_courses_std()
  df <- subset(courses0, institution_short == inst_short & nzchar(course_code_norm))
  if (!nrow(df)) stop("Ingen rader for ", inst_short, " i cache.")
  
  # ---------- UiO slug-maps ----------
  # Fakultets-slugs
  uio_fac_map <- c(
    "det humanistiske fakultet"="hf",
    "det juridiske fakultet"="jus",
    "det matematisk-naturvitenskapelige fakultet"="matnat",
    "det medisinske fakultet"="med",
    "det odontologiske fakultet"="odont",
    "det samfunnsvitenskapelige fakultet"="sv",
    "det teologiske fakultet"="teologi",
    "det utdanningsvitenskapelige fakultet"="uv"
  )
  
  # Institutt-/enhets-slugs (key = institutt/enhetsnavn, ikke fakultet!)
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
    # Fakultetsnivaa fallback (noen emner ligger her)
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
  
  # Fakultet
  fac_guess <- uio_fac_map[norm_key(df$faculty_name)]
  df$uio_faculty_slug[!is.na(fac_guess)] <- fac_guess[!is.na(fac_guess)]
  
  # Institutt/enhet: direkte match paa field_name (vanligvis institutt)
  inst_guess <- lapply(norm_key(df$field_name), function(k) uio_inst_map[[k]] %||% c(NA, NA))
  inst_mat <- do.call(rbind, inst_guess)
  if (is.matrix(inst_mat)) {
    fill <- is.na(df$uio_faculty_slug) & !is.na(inst_mat[,1])
    df$uio_faculty_slug[fill] <- inst_mat[fill, 1]
    df$uio_inst_slug[!is.na(inst_mat[,2])] <- inst_mat[!is.na(inst_mat[,2]), 2]
  }
  
  # Legacy heuristikk der vi fortsatt mangler
  need <- is.na(df$uio_faculty_slug) | is.na(df$uio_inst_slug)
  if (any(need)) {
    pair2 <- t(mapply(legacy_inst_map,
                      inst_name = df$faculty_name[need],
                      field_name = df$field_name[need]))
    if (is.matrix(pair2) && nrow(pair2)) {
      idx <- which(need)
      fill_fac  <- is.na(df$uio_faculty_slug[need]) & !is.na(pair2[,1])
      fill_inst <- is.na(df$uio_inst_slug[need])    & !is.na(pair2[,2])
      df$uio_faculty_slug[idx[fill_fac]] <- pair2[fill_fac, 1]
      df$uio_inst_slug[idx[fill_inst]]   <- pair2[fill_inst, 2]
    }
  }
  
  # ---------- YAML konfig ----------
  cfg  <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst) || is.null(inst$url_pattern) || !nzchar(as.character(inst$url_pattern)[1])) {
    stop("Mangler url_pattern i YAML for ", inst_short)
  }
  pattern        <- as.character(inst$url_pattern)[1]
  semester_style <- as.character(inst$semester_style %||% "plain")
  
  cfg  <- safe_read_yaml("config/institutions.yaml")
  inst <- cfg$institutions[[inst_short]]
  
  message("UiO YAML keys at institutions$uio: ", paste(names(inst), collapse = ", "))
  message("UiO YAML url_pattern (as read): ", as.character(inst$url_pattern)[1])
  
  pattern <- as.character(inst$url_pattern)[1]
  if (is.null(pattern) || !nzchar(pattern)) {
    stop("UiO: url_pattern is missing/empty in YAML. This will produce 0 URLs.")
  }
  
  
  # ---------- tokens og URL-bygging ----------
  # UiO bruker normalt ikke semester i path; vi fyller likevel tokens hvis pattern krever det
  df$code_upper_nodash1 <- df$code_base  # basekode uten suffiks
  
  urls <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    urls[i] <- tpl_sub(pattern, list(
      year               = as.character(df$year[i]),
      semester           = df$semester_name_raw[i],
      semester_url       = "",  # typisk tomt for UiO
      course_code        = df$course_code_norm[i],
      code_upper         = df$code_upper[i],
      code_lower         = tolower(df$course_code_norm[i]),
      code_upper_base    = df$code_base[i],
      code_lower_base    = tolower(df$code_base[i]),
      code_upper_nodash1 = df$code_upper_nodash1[i],
      uio_faculty_slug   = df$uio_faculty_slug[i],
      uio_inst_slug      = df$uio_inst_slug[i]
    ))
  }
  df$url <- urls
  
  # Dedup URL-er
  df <- df[!duplicated(df$url), , drop = FALSE]
  
  # UiO bruker ikke H/V i URL; sett hv=""
  df$hv <- ""
  
  # Advarsel om uerstattede tokens
  leftover <- grep("\\{[^}]+\\}", df$url, value = TRUE)
  if (length(leftover)) {
    warning("Uerstattede tokens i noen URLer, sjekk YAML/slug-maps. Eksempel: ", leftover[1])
  }
  
  # ---------- eksport ----------
  ts   <- format(Sys.time(), "%Y%m%d-%H%M")
  outd <- file.path("data", "output", inst_short)
  dir.create(outd, recursive = TRUE, showWarnings = FALSE)
  
  csv_ts <- file.path(outd, sprintf("course_urls_%s_%s.csv", inst_short, ts))
  txt_ts <- file.path(outd, sprintf("course_urls_%s_%s.txt", inst_short, ts))
  
  keep <- c("course_code_norm","year","hv","url")
  utils::write.csv(df[, keep, drop = FALSE], csv_ts, row.names = FALSE, fileEncoding = "UTF-8")
  safe_writeLines(df$url[nchar(df$url) > 0], txt_ts)
  
  # Oppdater latest pekere
  file.copy(csv_ts, file.path(outd, "course_urls_latest.csv"), overwrite = TRUE)
  file.copy(txt_ts, file.path(outd, "course_urls_latest.txt"), overwrite = TRUE)
  
  cat("Skrev filer for ", inst_short, " (", nrow(df), " rader)\n", sep = "")
})

