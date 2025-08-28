# ========== 0. Clean ==========
rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)

# ========== 1. Pakker ==========
# install.packages(c("yaml","glue","stringi"))  # kj??r ??n gang ved behov
library(yaml)
library(glue)
library(stringi)

# ========== 1b. (valgfritt) Verifisering ==========
DO_VERIFY <- FALSE  # Sett TRUE for ?? test-hente et utvalg URLer
if (DO_VERIFY) {
  # install.packages(c("httr2","rvest","xml2"))
  library(httr2)
  library(rvest)
  library(xml2)
}

# ========== 2. Paths ==========
root <- getwd()
path_in_rds       <- file.path(root, "data", "input",  "courses.RDS")
path_cfg_urls     <- file.path(root, "config",        "institutions.yaml")
path_cache_rds    <- file.path(root, "data", "cache", "courses_std.RDS")
path_out_csv_ok   <- file.path(root, "data", "output","course_urls.csv")
path_out_csv_bad  <- file.path(root, "data", "output","course_urls_missing.csv")
path_inst_counts  <- file.path(root, "data", "output","institution_counts.csv")
path_check_csv    <- file.path(root, "data", "output","url_check.csv")

dir.create(dirname(path_cache_rds), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_out_csv_ok), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(path_in_rds))  stop("Finner ikke data/input/courses.RDS")
if (!file.exists(path_cfg_urls)) stop("Finner ikke config/institutions.yaml")

# ========== 3. Les data ==========
courses_data <- readRDS(path_in_rds)

# ========== 4. Standardiser kolonnenavn ==========
orig <- names(courses_data)
names_map <- c(
  "Institusjonskode"       = "institution_code",
  "Institusjonsnavn"       = "institution_name",
  "Avdelingskode"          = "faculty_code",
  "Avdelingsnavn"          = "faculty_name",
  "Avdelingskode_SSB"      = "faculty_code_ssb",
  "Semester"               = "semester_int",
  "Semesternavn"           = "semester_name",
  "Studieprogramkode"      = "program_code",
  "Studieprogramnavn"      = "program_name",
  "Emnekode"               = "course_code",
  "Emnenavn"               = "course_name",
  "Studiepoeng"            = "ects",
  "NUS-kode"               = "nus_code",
  "Status"                 = "status_code",
  "Statusnavn"             = "status_name",
  "Navn"                   = "instruction_language_name",
  "Fagkode"                = "field_code",
  "Fagnavn"                = "field_name",
  "Oppgave (ny fra h2012)" = "thesis_flag"
)
idx_year <- which(grepl("??rstall|Arstall|\\u00C5rstall|.rstall", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_year) == 1) names_map[ orig[idx_year] ] <- "year"
idx_lang <- which(grepl("Underv.*spr", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_lang) == 1) names_map[ orig[idx_lang] ] <- "instruction_language_code"
idx_level_code <- which(grepl("Niv.*kod",  orig, ignore.case = TRUE, useBytes = TRUE))
idx_level_name <- which(grepl("Niv.*navn", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_level_code) == 1) names_map[ orig[idx_level_code] ] <- "level_code"
if (length(idx_level_name) == 1) names_map[ orig[idx_level_name] ] <- "level_name"

new_names <- unname(names_map[orig])
new_names[is.na(new_names)] <- orig[is.na(new_names)]
names(courses_data) <- new_names

# ========== 5. Plukk felt ==========
needed <- c("institution_name","year","semester_int","semester_name","course_code","course_name")
missing_needed <- setdiff(needed, names(courses_data))
if (length(missing_needed)) stop("Mangler kolonner: ", paste(missing_needed, collapse=", "))

year_raw     <- courses_data[["year"]]
semester_raw <- courses_data[["semester_int"]]
year_num     <- ifelse(grepl("^[0-9]+$", year_raw),     as.integer(year_raw),     NA_integer_)
semester_num <- ifelse(grepl("^[0-9]+$", semester_raw), as.integer(semester_raw), NA_integer_)

courses <- data.frame(
  institution_name = courses_data[["institution_name"]],
  year             = year_num,
  semester_int     = semester_num,
  semester_name    = courses_data[["semester_name"]],
  course_code      = trimws(courses_data[["course_code"]]),
  course_name      = courses_data[["course_name"]],
  stringsAsFactors = FALSE
)

# Kun 2023 og 2024 (kilde for generering)
courses <- subset(courses, year %in% c(2023, 2024))

# ========== 5b. Generer m??l-rader for 2025/2026 basert p?? historiske emner ==========
MATCH_SEASONALITY <- TRUE   # behold H/V kun der emnet fantes historisk

# Rens & basekode
src <- subset(courses, !is.na(course_code) & nzchar(trimws(course_code)))
canon_base_local <- function(x) sub("([-_.])[0-9]+$", "", trimws(x))
src$course_code_base <- canon_base_local(src$course_code)

to_season_hv <- function(s) {
  s2 <- stringi::stri_trans_general(tolower(ifelse(is.na(s),"",s)), "Latin-ASCII")
  ifelse(s2 %in% c("host","haust","h","autumn"), "H",
         ifelse(s2 %in% c("var","v","vaar","spring"),  "V", NA_character_))
}
src$season_hv <- to_season_hv(src$semester_name)

season_by_key <- aggregate(
  list(season = src$season_hv),
  by = list(inst = src$institution_name, code_base = src$course_code_base),
  FUN = function(x) sort(unique(na.omit(x)))
)

pick_idx <- !duplicated(src[, c("institution_name","course_code_base")])
key_table <- src[pick_idx, c("institution_name","course_code_base","course_code","course_name")]
key_table <- merge(key_table, season_by_key,
                   by.x = c("institution_name","course_code_base"),
                   by.y = c("inst","code_base"),
                   all.x = TRUE, all.y = FALSE)

targets <- data.frame(
  year   = c(2025L, 2026L),
  season = c("H","V"),
  stringsAsFactors = FALSE
)

expand_for_targets <- function(row) {
  have <- row$season[[1]]
  wanted <- if (!MATCH_SEASONALITY || length(have) == 0L) targets else subset(targets, season %in% have)
  if (!nrow(wanted)) return(NULL)
  data.frame(
    institution_name = row$institution_name,
    year             = wanted$year,
    semester_int     = NA_integer_,
    semester_name    = wanted$season,
    course_code      = row$course_code,  # behold en faktisk forekomst (for ?? danne tokens)
    course_name      = row$course_name,
    stringsAsFactors = FALSE
  )
}

lst <- lapply(split(key_table, seq_len(nrow(key_table))), expand_for_targets)
future_rows <- if (length(lst)) do.call(rbind, lst) else key_table[0, c("institution_name","course_code","course_name")]
if (!nrow(future_rows)) stop("Ingen rader ?? generere for 2025/2026 gitt MATCH_SEASONALITY.")

courses <- future_rows
courses$future_generated <- TRUE
row.names(courses) <- NULL

# ========== 6. Les URL-konfig + valider YAML ==========
cfg <- yaml::read_yaml(path_cfg_urls)

bad <- vapply(
  cfg$institutions,
  function(x) {
    pat <- if (is.null(x$url_pattern)) "" else x$url_pattern
    grepl("\\(", pat, perl = TRUE)  # ikke tillat funksjonskall i YAML
  },
  logical(1)
)
if (any(bad)) {
  stop("Oppdater institutions.yaml: url_pattern m?? bruke tokens (f.eks. {code_upper_nodash1}, {semester_url}) ??? ikke funksjonskall. Feil hos: ",
       paste(names(which(bad)), collapse = ", "))
}

# ========== 6b. Alias + robust normalisering ==========
aliases <- cfg$aliases
if (is.null(aliases)) aliases <- list()
aliases <- unlist(aliases, use.names = TRUE)

norm_utf8  <- function(x) enc2utf8(ifelse(is.na(x), "", x))
norm_ascii <- function(x) stringi::stri_trans_general(norm_utf8(x), "Latin-ASCII")
norm_lower <- function(x) tolower(norm_ascii(x))

courses$institution_name <- norm_utf8(courses$institution_name)
if (length(aliases)) names(aliases) <- norm_utf8(names(aliases))

fallback_map <- c(
  "oslomet"   = "oslomet",
  "agder"     = "uia",
  "ntnu"      = "ntnu",
  "innlandet" = "inn",
  "ostfold"   = "hiof",
  "vestlandet"= "hvl",
  "mf"        = "mf",
  "nla"       = "nla",
  "nord"      = "nord",
  "idrett"    = "nih",
  "bergen"    = "uib",
  "oslo"      = "uio",
  "stavanger" = "uis",
  "sorost"    = "usn",
  "arktiske"  = "uit",
  "volda"     = "hivolda",
  "nmbu"      = "nmbu",
  "miljo"     = "nmbu",
  "samisk"    = "samas",
  "sami "     = "samas"
)

map_to_short <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  nx <- norm_utf8(x)
  if (nx %in% names(aliases)) return(aliases[[nx]])
  xl <- norm_lower(x)
  for (k in names(fallback_map)) {
    if (grepl(k, xl, fixed = TRUE)) return(fallback_map[[k]])
  }
  NA_character_
}
courses$institution_short <- vapply(courses$institution_name, map_to_short, character(1))

# ========== 7. Semester-normalisering ==========
norm_no <- function(x) stringi::stri_trans_general(tolower(ifelse(is.na(x),"",x)), "Latin-ASCII")

standardize_semester <- function(semester, style = c("uia","ntnu","inn","url","hiof","nih","plain","oslomet")) {
  style <- match.arg(style)
  s <- norm_no(semester)
  if (style == "uia")     return(ifelse(s %in% c("host","haust","h"), "host",
                                        ifelse(s %in% c("var","v","vaar"), "var", s)))
  if (style == "ntnu")    return(ifelse(s %in% c("host","haust","h"), "1",
                                        ifelse(s %in% c("var","v","vaar"), "2", s)))
  if (style == "hiof")    return(ifelse(s %in% c("var","v","vaar","spring"), "var",
                                        ifelse(s %in% c("host","haust","h","autumn"), "host", s)))
  if (style == "nih")     return(ifelse(s %in% c("host","haust","h","autumn"), "host",
                                        ifelse(s %in% c("var","v","vaar","spring"), "var", s)))
  if (style == "inn")     return(s)
  if (style == "url")     return(utils::URLencode(s, reserved = TRUE))
  if (style == "oslomet") {
    # Oslomet ??nsker "H??ST"/"V??R" (URL-enkodes senere)
    hv <- ifelse(s %in% c("host","haust","h","autumn"), "H??ST",
                 ifelse(s %in% c("var","v","vaar","spring"), "V??R", toupper(s)))
    return(utils::URLencode(hv, reserved = TRUE))
  }
  s
}

# ========== 7b. Emnekode-tokenisering ==========
# Vi trenger flere varianter:
#  - nodash1: fjerner kun endesuffiks "-1"/"_1"/".1" (beholder f.eks. "-7")
#  - nosfxA:  fjerner endesuffiks "-A"/"_B"/".C" (??n bokstav)
#  - nosfx1A: fjerner enten "-1" ELLER "-A" (nyttig for NTNU)
#  - nodashnum: fjerner et hvilket som helst numerisk endesuffiks "-\d+" (bruk med omhu)
canon_remove_trailing_instance1 <- function(x) sub("([\\-_.])1$", "", x, perl = TRUE)
canon_remove_trailing_letter1   <- function(x) sub("([\\-_.])[A-Za-z]$", "", x, perl = TRUE)
canon_remove_trailing_num       <- function(x) sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)

make_code_tokens <- function(code) {
  raw <- trimws(code)
  nodash1   <- canon_remove_trailing_instance1(raw)
  nosfxA    <- canon_remove_trailing_letter1(raw)
  nosfx1A   <- canon_remove_trailing_letter1(canon_remove_trailing_instance1(raw))
  nodashnum <- canon_remove_trailing_num(raw)
  base      <- nodashnum
  list(
    course_code         = raw,
    code_lower          = tolower(raw),
    code_upper          = toupper(raw),
    code_lower_nodash1  = tolower(nodash1),
    code_upper_nodash1  = toupper(nodash1),
    code_lower_nosfxA   = tolower(nosfxA),
    code_upper_nosfxA   = toupper(nosfxA),
    code_lower_nosfx1A  = tolower(nosfx1A),
    code_upper_nosfx1A  = toupper(nosfx1A),
    code_lower_nodashnum= tolower(nodashnum),
    code_upper_nodashnum= toupper(nodashnum),
    code_lower_base     = tolower(base),
    code_upper_base     = toupper(base)
  )
}

# ========== 8. Bygg URL ==========
build_url <- function(inst_short, course_code, year, semester) {
  if (is.na(inst_short) || !nzchar(inst_short)) return(NA_character_)
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst)) return(NA_character_)
  pattern <- inst$url_pattern
  style   <- inst$semester_style
  if (is.null(pattern) || !nzchar(pattern)) return(NA_character_)  # tomt m??nster => NA
  if (is.null(style))   style <- "plain"
  code_tokens <- make_code_tokens(course_code)
  data_env <- c(code_tokens, list(
    year         = as.character(year),
    semester     = semester,
    semester_url = standardize_semester(semester, style)
  ))
  tryCatch(
    glue::glue_data(.x = data_env, pattern) |> as.character(),
    error = function(e) NA_character_
  )
}

# ========== 8b. Generer URL KUN for institusjoner som er definert i YAML ==========
supported <- names(cfg$institutions)
courses$supported_inst <- !is.na(courses$institution_short) & courses$institution_short %in% supported

courses$url <- NA_character_
idx <- which(courses$supported_inst)
if (length(idx)) {
  courses$url[idx] <- mapply(
    FUN = build_url,
    inst_short = courses$institution_short[idx],
    course_code = courses$course_code[idx],
    year        = courses$year[idx],
    semester    = courses$semester_name[idx],
    SIMPLIFY    = TRUE, USE.NAMES = FALSE
  )
}

# Del opp: ferdige vs. manglende
is_missing <- is.na(courses$url) | !nzchar(courses$url)
courses_ok      <- subset(courses, !is_missing)
courses_missing <- subset(courses,  is_missing)

# ========== 8c. Deduplisering ==========
# Mange m??nstre (NTNU, HVL, MF, ???) bruker ikke ??r/semester i URL, s?? 2025/2026 kan gi samme lenke.
# Vi beholder den tidligste (2025) ved ?? sortere og s?? ta unique(url).
if (nrow(courses_ok)) {
  courses_ok <- courses_ok[order(courses_ok$year, courses_ok$institution_short, courses_ok$course_code), ]
  dup <- duplicated(courses_ok$url)
  courses_ok <- courses_ok[!dup, , drop = FALSE]
}

# ========== 8d. Mer presis feildiagnose ==========
get_pat <- function(short) {
  if (is.na(short) || !nzchar(short)) return(NA_character_)
  inst <- cfg$institutions[[short]]
  if (is.null(inst)) return(NA_character_)
  pat <- inst$url_pattern
  if (is.null(pat) || length(pat) == 0L) return(NA_character_)
  as.character(pat)[1L]
}
pat_vec <- if (nrow(courses_missing)) vapply(courses_missing$institution_short, get_pat, character(1), USE.NAMES = FALSE) else character(0)

courses_missing$missing_reason <- if (nrow(courses_missing)) ifelse(
  is.na(courses_missing$institution_short),
  "mangler alias/institution_short",
  ifelse(!(courses_missing$institution_short %in% supported),
         "institusjon ikke definert i institutions",
         ifelse(is.na(pat_vec),
                "mangler url_pattern i YAML",
                ifelse(!nzchar(pat_vec),
                       "tomt url_pattern i YAML",
                       "url_pattern ga tom streng")))) else character(0)

# ========== 9. Oppsummering per institusjon ==========
inst_counts <- sort(table(courses$institution_name), decreasing = TRUE)
inst_counts_df <- data.frame(
  institution_name = names(inst_counts),
  n_rows = as.integer(inst_counts),
  row.names = NULL
)

# ========== 9b. Skriv med tidsstempel + "latest" ==========
stamp  <- format(Sys.time(), "%Y%m%d-%H%M")
out_dir <- dirname(path_out_csv_ok)

path_out_csv_ok_ts   <- file.path(out_dir, sprintf("course_urls_%s.csv", stamp))
path_out_csv_bad_ts  <- file.path(out_dir, sprintf("course_urls_missing_%s.csv", stamp))
path_inst_counts_ts  <- file.path(out_dir, sprintf("institution_counts_%s.csv", stamp))
path_cache_rds_ts    <- file.path(dirname(path_cache_rds), sprintf("courses_std_%s.RDS", stamp))

utils::write.csv(courses_ok,      path_out_csv_ok_ts,  row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(courses_missing, path_out_csv_bad_ts, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(inst_counts_df,  path_inst_counts_ts, row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(courses, path_cache_rds_ts)

file.copy(path_out_csv_ok_ts,   path_out_csv_ok,   overwrite = TRUE)
file.copy(path_out_csv_bad_ts,  path_out_csv_bad,  overwrite = TRUE)
file.copy(path_inst_counts_ts,  path_inst_counts,  overwrite = TRUE)
saveRDS(courses, path_cache_rds)

cat("??? Skrev (timestamp):\n ",
    path_out_csv_ok_ts,  "\n ",
    path_out_csv_bad_ts, "\n ",
    path_inst_counts_ts, "\n ",
    path_cache_rds_ts,   "\n", sep = "")
cat("??????  Oppdatert 'latest':\n ",
    path_out_csv_ok,  "\n ",
    path_out_csv_bad, "\n ",
    path_inst_counts, "\n ",
    path_cache_rds,   "\n", sep = "")

# ========== 10. Diagnostikk ==========
na_n   <- nrow(courses_missing)
na_pct <- round(na_n / (nrow(courses_ok) + nrow(courses_missing)) * 100, 2)
cat("\nUten URL:", na_n, "(", na_pct, "% )\n", sep = "")

cat("\nAntall med URL:", nrow(courses_ok), " | Uten URL:", nrow(courses_missing), "\n")
if (nrow(courses_missing)) {
  cat("\nInstitusjoner (short) med manglende url:\n")
  print(sort(unique(courses_missing$institution_short)))
  cat("\nEksempler p?? rader uten URL:\n")
  print(utils::head(courses_missing[
    , c("institution_name","institution_short","course_code","year","semester_name","missing_reason")
  ], 12))
}

cat("\n???? Skrev institusjonsoppsummering:", path_inst_counts, "\n")

# ========== 11. (Valgfritt) Verifiser et utvalg URLer ==========
if (DO_VERIFY) {
  sample_for_verification <- function(df,
                                      frac = 0.05,
                                      min_per_inst = 10L,
                                      max_per_inst = 60L,
                                      max_total = 1200L,
                                      seed = 42L) {
    df <- subset(df, !is.na(url) & nzchar(url) & !is.na(institution_short))
    set.seed(seed)
    out_list <- list(); total <- 0L
    for (inst in unique(df$institution_short)) {
      if (total >= max_total) break
      dfi <- df[df$institution_short == inst, , drop = FALSE]
      n <- nrow(dfi)
      k <- max(min_per_inst, ceiling(n * frac))
      k <- min(k, max_per_inst, n, max_total - total)
      if (k <= 0) next
      strata_key <- paste(dfi$year, dfi$semester_name, sep = "|")
      strata <- split(dfi, strata_key)
      m <- length(strata); base_q <- if (m > 0) floor(k / m) else k
      remainder <- k - base_q * m
      pick <- lapply(strata, function(s) {
        if (!nrow(s)) return(s[0, , drop = FALSE])
        s[sample(seq_len(nrow(s)), size = min(base_q, nrow(s))), , drop = FALSE]
      })
      if (remainder > 0 && m > 0) {
        for (ek in sample(names(strata), size = remainder)) {
          s <- strata[[ek]]; if (!nrow(s)) next
          already <- pick[[ek]]
          rem_idx <- setdiff(seq_len(nrow(s)), match(rownames(already), rownames(s)))
          if (length(rem_idx)) pick[[ek]] <- rbind(already, s[sample(rem_idx, 1L), , drop = FALSE])
        }
      }
      out_list[[length(out_list) + 1L]] <- do.call(rbind, pick)
      total <- total + nrow(out_list[[length(out_list)]])
    }
    out <- if (length(out_list)) do.call(rbind, out_list) else df[0, ]
    if (nrow(out) > max_total) out <- out[seq_len(max_total), , drop = FALSE]
    rownames(out) <- NULL; out
  }
  
  verify_status <- function(df, sleep = 0.25, timeout = 12, max_tries = 2) {
    stopifnot("url" %in% names(df))
    df <- subset(df, !is.na(url) & nzchar(url))
    ua   <- "TEPS-url-verifier/1.0 (+github.com/teps)"
    cols <- c("url","final_url","status","ok","content_type","bytes","error")
    res  <- vector("list", nrow(df))
    for (i in seq_len(nrow(df))) {
      url <- df$url[i]
      req <- request(url) |>
        req_user_agent(ua) |>
        req_timeout(timeout) |>
        req_error(is_error = ~ FALSE) |>
        req_retry(max_tries = max_tries, backoff = ~ min(6, 1.5 ^ attempt)) |>
        req_headers(`Accept-Language` = "nb,no;q=0.9,en;q=0.8")
      resp <- try(req_perform(req), silent = TRUE)
      if (inherits(resp, "response")) {
        row <- data.frame(
          url          = url,
          final_url    = resp_url(resp),
          status       = resp_status(resp),
          ok           = resp_status(resp) >= 200 & resp_status(resp) < 400,
          content_type = resp_content_type(resp),
          bytes        = length(resp_body_raw(resp)),
          error        = NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        emsg <- tryCatch(as.character(attr(resp, "condition")$message), error = function(e) "request failed")
        row <- data.frame(
          url          = url,
          final_url    = NA_character_,
          status       = NA_integer_,
          ok           = FALSE,
          content_type = NA_character_,
          bytes        = NA_integer_,
          error        = emsg,
          stringsAsFactors = FALSE
        )
      }
      row <- row[, cols]
      res[[i]] <- row
      if (sleep > 0) Sys.sleep(sleep)
      if (i %% 50 == 0) cat("...sjekket", i, "av", nrow(df), "URLer\n")
    }
    out <- do.call(rbind, res); rownames(out) <- NULL; out
  }
  
  target <- sample_for_verification(courses_ok, frac = 0.05, min_per_inst = 10, max_per_inst = 60,
                                    max_total = 1200, seed = 42)
  status_check <- verify_status(target, sleep = 0.25, timeout = 12, max_tries = 2)
  utils::write.csv(status_check, path_check_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("???? Skrev URL-sjekk til:", path_check_csv, "\n")
  cat("\nAndel OK (2xx/3xx):", round(mean(status_check$ok, na.rm = TRUE) * 100, 2), "%\n")
  print(sort(table(status_check$status), decreasing = TRUE))
  cat("\nEksempler p?? feil:\n")
  print(utils::head(subset(status_check, !ok | is.na(ok)), 20))
}



