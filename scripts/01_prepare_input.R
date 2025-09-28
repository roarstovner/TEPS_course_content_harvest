# 01_prepare_input.R
# Purpose:
# - Read data/input/courses.RDS
# - Standardize column names and types
# - Normalize to UTF-8 + trim + normalize exotic dashes in course codes
# - Produce semester_hv (H/V/NA) in an institution-agnostic way
# - Map institution_name to institution_short using config/institutions.yaml (robust with ASCII keys)
# - Create light code tokens (code_upper, code_base)
# - Detect and REPORT duplicates (do not delete in the report stage)
# - Write data/cache/courses_std.RDS (RDS only)
#
# Important: Module 01 does *not* build URLs, scrape, or create future years.
# It produces a cleaned, stable cache for downstream steps.

# ========== 0. Clean ==========
rm(list = ls(all = TRUE))   ## Clean workspace (optional)

# (Optional) make UTF-8 the default for messages/console; ignore if it fails on OS
try(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")), silent = TRUE)
# (Optional) help R print UTF-8 in this session
options(encoding = "UTF-8", stringsAsFactors = FALSE, warn = 1)

# ===== 1) Packages =====
suppressPackageStartupMessages({
  library(yaml)
  library(stringi)
  library(glue)
  library(rvest)
  library(readr)
})

# ===== 2) Paths =====
root <- getwd()
path_in_rds    <- file.path(root, "data", "input",  "courses.RDS")
path_cfg_yaml  <- file.path(root, "config",        "institutions.yaml")
path_cache_rds <- file.path(root, "data", "cache", "courses_std.RDS")

## Ensure cache dir exists even in a fresh clone
dir.create(dirname(path_cache_rds), recursive = TRUE, showWarnings = FALSE)

## Early hard-stop if input or config is missing (better than failing halfway)
if (!file.exists(path_in_rds))   stop("Missing ", path_in_rds)
if (!file.exists(path_cfg_yaml)) stop("Missing ", path_cfg_yaml)

# ===== 3) Helpers (local to module 01) =====
## Normalize to UTF-8 and convert NA to empty string
norm_utf8  <- function(x) enc2utf8(ifelse(is.na(x), "", x))

## Trim ends and collapse multiple spaces to one
trim_multi <- function(x) gsub("\\s+", " ", trimws(x))

## Replace all “exotic” dashes (– — ‒ − etc.) with a plain hyphen '-'
normalize_dashes <- function(x) {
  x <- norm_utf8(x)
  gsub("[\u2010-\u2015\u2212]", "-", x, perl = TRUE)
}

## Map local semester names to H/V (autumn/host/haust -> H; vår/spring -> V); vectorized
to_sem_hv <- function(s) {
  x <- enc2utf8(ifelse(is.na(s), "", s))
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("[^a-z]+", " ", x)          # non-letters -> space
  x <- trimws(gsub("\\s+", " ", x))     # collapse spaces
  
  h_tokens <- c("host","hosten","haust","hausten","autumn","fall","h")
  v_tokens <- c("var","varen","vaar","vaaren","spring","v")
  
  if (!length(x)) return(character(0))
  
  vapply(strsplit(x, " ", fixed = TRUE), function(words) {
    if (length(words) == 0 || (length(words) == 1 && words == "")) return(NA_character_)
    if (any(words %in% h_tokens)) return("H")
    if (any(words %in% v_tokens)) return("V")
    NA_character_
  }, FUN.VALUE = character(1))
}

## Remove trailing -/_/. + digits (e.g., -1, _2, .3); used for code_base
canon_remove_trailing_num <- function(x) sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)

## Look up institution_short via YAML aliases; fallback via ASCII-lower substring
alias_institution_short <- function(inst_name_utf8, cfg_aliases) {
  aliases <- cfg_aliases
  if (is.null(aliases)) aliases <- list()
  aliases <- unlist(aliases, use.names = TRUE)
  if (length(aliases)) names(aliases) <- norm_utf8(names(aliases))
  
  ## Substring fallback on ASCII-lowered name
  fallback_map <- c(
    "oslomet"="oslomet","agder"="uia","ntnu"="ntnu","innlandet"="inn","ostfold"="hiof",
    "vestlandet"="hvl","mf"="mf","nla"="nla","nord"="nord","idrett"="nih","bergen"="uib",
    "oslo"="uio","stavanger"="uis","sorost"="usn","arktiske"="uit","volda"="hivolda",
    "nmbu"="nmbu","miljo"="nmbu","samisk"="samas","sami " = "samas"
  )
  
  map_one <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    nx <- norm_utf8(x)
    ## Exact alias hit on UTF-8 name
    if (nx %in% names(aliases)) return(aliases[[nx]])
    ## ASCII-lower substring fallback
    xl <- tolower(stringi::stri_trans_general(nx, "Latin-ASCII"))
    for (k in names(fallback_map)) if (grepl(k, xl, fixed = TRUE)) return(fallback_map[[k]])
    NA_character_
  }
  vapply(inst_name_utf8, map_one, character(1))
}

# ===== 4) Read data + YAML =====
raw <- readRDS(path_in_rds)          # RDS preserves what was saved; we normalize strings below
cfg  <- yaml::read_yaml(path_cfg_yaml)

message(sprintf("Read input: %s | rows: %d, cols: %d",
                normalizePath(path_in_rds, winslash = "/"),
                nrow(raw), ncol(raw)))
## YAML here is only for alias lookup; URL patterns are handled later

# ===== 5) Standardize column names =====
## Harmonize source column variants to stable English names
orig <- names(raw)
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
## Tolerate variants for "year"
idx_year <- which(grepl("årstall|Arstall|\\u00C5rstall|.rstall", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_year) == 1) names_map[ orig[idx_year] ] <- "year"
## Tolerate variants for language/level
idx_lang <- which(grepl("Underv.*spr", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_lang) == 1) names_map[ orig[idx_lang] ] <- "instruction_language_code"
idx_level_code <- which(grepl("Niv.*kod",  orig, ignore.case = TRUE, useBytes = TRUE))
idx_level_name <- which(grepl("Niv.*navn", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_level_code) == 1) names_map[ orig[idx_level_code] ] <- "level_code"
if (length(idx_level_name) == 1) names_map[ orig[idx_level_name] ] <- "level_name"

new_names <- unname(names_map[orig]); new_names[is.na(new_names)] <- orig[is.na(new_names)]
names(raw) <- new_names

# ===== 6) Filter to 2024 =====
## ---- Year filter ----
TARGET_YEAR <- as.integer(Sys.getenv("TEPS_TARGET_YEAR", "2024"))

if ("year" %in% names(raw)) {
  ynum <- suppressWarnings(as.integer(gsub("\\D", "", raw$year)))
  raw  <- subset(raw, ynum == TARGET_YEAR)
}

message("After filter to year ", TARGET_YEAR, ": rows = ", nrow(raw))

# ===== 7) Select and normalize fields =====
needed   <- c("institution_name","year","semester_int","semester_name","course_code","course_name")
opt_keep <- c("faculty_name","field_name","faculty_code","field_code")  # passthrough
missing_needed <- setdiff(needed, names(raw))
if (length(missing_needed)) stop("Missing columns: ", paste(missing_needed, collapse = ", "))

## UTF-8 + trim on core text fields
institution_name <- trim_multi(norm_utf8(raw[["institution_name"]]))
semester_name    <- trim_multi(norm_utf8(raw[["semester_name"]]))
course_code_raw  <- trim_multi(norm_utf8(raw[["course_code"]]))
course_name      <- trim_multi(norm_utf8(raw[["course_name"]]))

## course_code_raw  : as in source (UTF-8 + trim)
## course_code_norm : raw with exotic dashes normalized to "-" (otherwise identical)
## code_upper       : normalized uppercase — safe for matching regardless of case
## code_base        : normalized without trailing -/_/. + digits (ABC123-1 -> ABC123)

## Normalize dashes in course code
course_code_norm <- normalize_dashes(course_code_raw)

## Numeric fields
year_num     <- ifelse(grepl("^[0-9]+$", raw[["year"]]), as.integer(raw[["year"]]), NA_integer_)
semester_num <- ifelse(grepl("^[0-9]+$", raw[["semester_int"]]), as.integer(raw[["semester_int"]]), NA_integer_)

## Semester H/V
semester_hv <- to_sem_hv(semester_name)

## Lightweight code tokens for downstream steps
code_upper <- toupper(course_code_norm)
code_base  <- toupper(canon_remove_trailing_num(course_code_norm))

## Passthrough metadata (UTF-8/trimmed) used by some adapters
pt <- intersect(opt_keep, names(raw))
pt_df <- if (length(pt)) {
  as.data.frame(lapply(raw[pt], function(col) trim_multi(norm_utf8(col))), stringsAsFactors = FALSE)
} else NULL

courses_std <- data.frame(
  institution_name = institution_name,
  institution_short = NA_character_,   # filled below (alias)
  year = year_num,
  semester_int = semester_num,
  semester_name_raw = semester_name,
  semester_hv = semester_hv,
  course_code_raw = course_code_raw,
  course_code_norm = course_code_norm,
  course_name = course_name,
  code_upper = code_upper,
  code_base  = code_base,
  stringsAsFactors = FALSE
)
if (!is.null(pt_df)) courses_std <- cbind(courses_std, pt_df)

# ===== 8) Alias to institution_short =====
## Stable short-ID per institution (uia/ntnu/uio/oslomet etc.)
courses_std$institution_short <- alias_institution_short(courses_std$institution_name, cfg$aliases)

# ===== 9) Diagnostics (missing/alias) =====
n_total <- nrow(courses_std)
n_missing_core  <- sum(!nzchar(courses_std$course_code_norm) | is.na(courses_std$year) | is.na(courses_std$semester_hv))
n_missing_alias <- sum(is.na(courses_std$institution_short))

message(sprintf("Diagnostics: total=%d | missing_core=%d | missing_alias=%d",
                n_total, n_missing_core, n_missing_alias))

if (n_missing_alias > 0) {
  top_missing <- sort(table(courses_std$institution_name[is.na(courses_std$institution_short)]), decreasing = TRUE)
  message("Top institution names without alias (up to 10):")
  print(utils::head(top_missing, 10))
}

# ===== 10) Duplicate check (report) =====
## “Strict” key: inst + code (upper) + year + H/V — catches real collisions within same semester
dup_key  <- with(courses_std, paste(institution_short, code_upper, year, semester_hv, sep = "|"))
dup_flag <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
n_dup    <- sum(dup_flag, na.rm = TRUE)

message("Duplicates (on key inst_short|code_upper|year|semester_hv): ", n_dup)
if (n_dup > 0) {
  message("Examples of duplicate keys (up to 10):")
  print(utils::head(unique(dup_key[dup_flag]), 10))
}

# ===== 10b) De-duplication (operational) =====
## For URL generation we want one row per institution+course_code — keep first occurrence
dup_key2 <- with(courses_std, paste(institution_short, course_code_norm, sep = "|"))
dup_rows <- duplicated(dup_key2)

n_before <- nrow(courses_std)
courses_std <- courses_std[!dup_rows, ]
n_after  <- nrow(courses_std)

message(sprintf("De-duplication: kept %d rows (%d removed as duplicates on inst+code).",
                n_after, n_before - n_after))

# ===== 11) Write cache (RDS) =====
## Single source of truth for the rest of the pipeline
saveRDS(courses_std, path_cache_rds)
message("Saved: ", normalizePath(path_cache_rds, winslash = "/"))

## Quick structure check — remove if you want quieter logs
str(courses_std)

