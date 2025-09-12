# 01_prepare_input.R
# Form??l:
# - Lese data/input/courses.RDS
# - Standardisere kolonnenavn og typer
# - UTF-8 + trimming + normalisering av bindestreker i emnekoder
# - Lage semester_hv (H/V/NA) ??? institusjonsn??ytralt
# - Alias til institution_short fra config/institutions.yaml (robust med ASCII-n??kler)
# - Lage lette emnekode-tokens (code_upper, code_base)
# - Finne og RAPPORTERE duplikater (ikke slette)
# - Skrive data/cache/courses_std.RDS (kun RDS)
#
# Viktig: Modul 01 gj??r *ikke* URL-bygging, scraping eller generering av framtids??r.
# Den lager kun en renset og stabil basisfil (cache) for videre steg.

# ========== 0. Clean ==========
# rm(list = ls(all = TRUE))   ## Rense

# ===== 1) Pakker =====
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

## S??rger for at cache-mappen finnes selv om prosjektet er nylig klonet
dir.create(dirname(path_cache_rds), recursive = TRUE, showWarnings = FALSE)

## Hard-stopp tidlig hvis input eller konfig mangler ??? bedre enn ?? feile halvveis
if (!file.exists(path_in_rds))   stop("Finner ikke ", path_in_rds)
if (!file.exists(path_cfg_yaml)) stop("Finner ikke ", path_cfg_yaml)

# ===== 3) Hjelpefunksjoner (kun internt i 01) =====
## Normaliser til UTF-8 og gj??r NA til tom streng
norm_utf8  <- function(x) enc2utf8(ifelse(is.na(x), "", x))

## Trim ytterkanter og samle flere mellomrom til ett
trim_multi <- function(x) gsub("\\s+", " ", trimws(x))

## Bytt alle ???rare??? bindestreker (??? ??? ??? osv.) til vanlig '-'
normalize_dashes <- function(x) {
  x <- norm_utf8(x)
  gsub("[\u2010-\u2015\u2212]", "-", x, perl = TRUE)
}

## Mapp lokale semester-navn til H/V (autumn/host/haust ??? H, vaar/spring ??? V) ??? VEKTO-RISERT
to_sem_hv <- function(s) {
  x <- enc2utf8(ifelse(is.na(s), "", s))
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("[^a-z]+", " ", x)                  # ikke-bokstaver ??? space
  x <- trimws(gsub("\\s+", " ", x))             # komprimer spaces
  
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



## Fjern avsluttende -/_/. + tall (f.eks. -1, _2, .3); brukes til code_base
canon_remove_trailing_num <- function(x) sub("([\\-_.])[0-9]+$", "", x, perl = TRUE)

## Sl?? opp kortnavn fra YAML-aliasser; fallback med ASCII-lower substring
alias_institution_short <- function(inst_name_utf8, cfg_aliases) {
  aliases <- cfg_aliases
  if (is.null(aliases)) aliases <- list()
  aliases <- unlist(aliases, use.names = TRUE)
  if (length(aliases)) names(aliases) <- norm_utf8(names(aliases))
  
  ## fallback-regler (substring p?? ASCII-lower)
  fallback_map <- c(
    "oslomet"="oslomet","agder"="uia","ntnu"="ntnu","innlandet"="inn","ostfold"="hiof",
    "vestlandet"="hvl","mf"="mf","nla"="nla","nord"="nord","idrett"="nih","bergen"="uib",
    "oslo"="uio","stavanger"="uis","sorost"="usn","arktiske"="uit","volda"="hivolda",
    "nmbu"="nmbu","miljo"="nmbu","samisk"="samas","sami " = "samas"
  )
  
  map_one <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    nx <- norm_utf8(x)
    ## eksakt alias-treff p?? UTF-8-navn
    if (nx %in% names(aliases)) return(aliases[[nx]])
    ## ASCII-lower substring fallback
    xl <- tolower(stri_trans_general(nx, "Latin-ASCII"))
    for (k in names(fallback_map)) if (grepl(k, xl, fixed = TRUE)) return(fallback_map[[k]])
    NA_character_
  }
  vapply(inst_name_utf8, map_one, character(1))
}

# ===== 4) Les data + YAML =====
raw <- readRDS(path_in_rds)
cfg  <- yaml::read_yaml(path_cfg_yaml)

message(sprintf("Lest input: %s | rader: %d, kolonner: %d",
                normalizePath(path_in_rds, winslash = "/"),
                nrow(raw), ncol(raw)))
## YAML brukes her kun til alias-oppslag; URL-m??nstre h??ndteres i senere steg

# ===== 5) Standardiser kolonnenavn =====
## Harmoniser kolonnenavn fra DB/Excel-varianter til stabile engelske navn
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
## Toler??r variasjoner for "??rstall"
idx_year <- which(grepl("??rstall|Arstall|\\u00C5rstall|.rstall", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_year) == 1) names_map[ orig[idx_year] ] <- "year"
## Toler??r varianter for undervisningsspr??k/niv??
idx_lang <- which(grepl("Underv.*spr", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_lang) == 1) names_map[ orig[idx_lang] ] <- "instruction_language_code"
idx_level_code <- which(grepl("Niv.*kod",  orig, ignore.case = TRUE, useBytes = TRUE))
idx_level_name <- which(grepl("Niv.*navn", orig, ignore.case = TRUE, useBytes = TRUE))
if (length(idx_level_code) == 1) names_map[ orig[idx_level_code] ] <- "level_code"
if (length(idx_level_name) == 1) names_map[ orig[idx_level_name] ] <- "level_name"

new_names <- unname(names_map[orig]); new_names[is.na(new_names)] <- orig[is.na(new_names)]
names(raw) <- new_names

# ===== 6) Filtrer til 2024 =====
## Modul 01 skal kun produsere basis for ett ??r (2024 n??).
TARGET_YEAR <- 2024L
if ("year" %in% names(raw)) {
  ynum <- suppressWarnings(as.integer(gsub("\\D", "", raw$year)))
  raw <- subset(raw, ynum == TARGET_YEAR)
}
message("Etter filter til 2024: rader = ", nrow(raw))

# ===== 7) Plukk og normaliser felt =====
needed  <- c("institution_name","year","semester_int","semester_name","course_code","course_name")
opt_keep <- c("faculty_name","field_name","faculty_code","field_code")  # pass-through
missing_needed <- setdiff(needed, names(raw))
if (length(missing_needed)) stop("Mangler kolonner: ", paste(missing_needed, collapse = ", "))

## UTF-8 + trim p?? sentrale tekstfelter
institution_name <- trim_multi(norm_utf8(raw[["institution_name"]]))
semester_name    <- trim_multi(norm_utf8(raw[["semester_name"]]))
course_code_raw  <- trim_multi(norm_utf8(raw[["course_code"]]))
course_name      <- trim_multi(norm_utf8(raw[["course_name"]]))

## course_code_raw  : slik koden kom fra kilden (kun UTF-8 + trim)
## course_code_norm : raw med ???rare dash??? ??? "-" (ellers identisk)
## code_upper       : norm i store bokstaver ??? trygg matching uansett case
## code_base        : norm uten avsluttende -/_/. + tall (ABC123-1 ??? ABC123)

## Normaliser emnekode-bindestreker til '-'
course_code_norm <- normalize_dashes(course_code_raw)

## Tallfelt
year_num     <- ifelse(grepl("^[0-9]+$", raw[["year"]]), as.integer(raw[["year"]]), NA_integer_)
semester_num <- ifelse(grepl("^[0-9]+$", raw[["semester_int"]]), as.integer(raw[["semester_int"]]), NA_integer_)

## Semester H/V
semester_hv <- to_sem_hv(semester_name)

## Enkle tokens for videre steg
code_upper <- toupper(course_code_norm)
code_base  <- toupper(canon_remove_trailing_num(course_code_norm))

## Pass-through av metadata (UTF-8/trimmet) ??? brukes av enkelte adaptere
pt <- intersect(opt_keep, names(raw))
pt_df <- if (length(pt)) {
  as.data.frame(lapply(raw[pt], function(col) trim_multi(norm_utf8(col))), stringsAsFactors = FALSE)
} else NULL

courses_std <- data.frame(
  institution_name = institution_name,
  institution_short = NA_character_,       # fylles under (alias)
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

# ===== 8) Alias til institution_short =====
## Gir stabil kort-ID per institusjon (uia/ntnu/uio/oslomet osv.)
courses_std$institution_short <- alias_institution_short(courses_std$institution_name, cfg$aliases)

# ===== 9) Diagnostikk (mangler/alias) =====
## Rask helsecheck: mangler kjernefelt? mangler alias?
n_total <- nrow(courses_std)
n_missing_core <- sum(!nzchar(courses_std$course_code_norm) | is.na(courses_std$year) | is.na(courses_std$semester_hv))
n_missing_alias <- sum(is.na(courses_std$institution_short))

message(sprintf("Diagnostikk: total=%d | mangler_kjernefelt=%d | mangler_alias=%d",
                n_total, n_missing_core, n_missing_alias))

if (n_missing_alias > 0) {
  top_missing <- sort(table(courses_std$institution_name[is.na(courses_std$institution_short)]), decreasing = TRUE)
  message("Topp institusjonsnavn uten alias (inntil 10):")
  print(utils::head(top_missing, 10))
}

# ===== 10) Duplikat-sjekk (rapport) =====
## ???Streng??? n??kkel: inst + kode (upper) + ??r + H/V ??? fanger reelle kollisjoner innen samme semester
dup_key <- with(courses_std, paste(institution_short, code_upper, year, semester_hv, sep = "|"))
dup_flag <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)
n_dup <- sum(dup_flag, na.rm = TRUE)

message("Duplikater (p?? n??kkel inst_short|code_upper|year|semester_hv): ", n_dup)
if (n_dup > 0) {
  message("Eksempler p?? duplikatn??kler (inntil 10):")
  print(utils::head(unique(dup_key[dup_flag]), 10))
}

# ===== 10b. Deduplisering (operativ) =====
## For URL-generering ??nsker vi kun ??n rad per institusjon+emnekode ??? behold f??rste forekomst
dup_key2 <- with(courses_std, paste(institution_short, course_code_norm, sep = "|"))
dup_rows <- duplicated(dup_key2)

n_before <- nrow(courses_std)
courses_std <- courses_std[!dup_rows, ]
n_after <- nrow(courses_std)

message(sprintf("Deduplisering: beholdt %d rader (%d fjernet som duplikater p?? inst+kode).",
                n_after, n_before - n_after))

# ===== 11) Skriv cache (RDS) =====
## ??n ???sannhet??? for resten av pipeline: renset, normalisert og deduplisert per inst+kode
saveRDS(courses_std, path_cache_rds)
message("???? Lagret: ", normalizePath(path_cache_rds, winslash = "/"))

## Hurtigsjekk av felter og typer ??? fjern i batch-kj??ring om du vil ha helt stille logging
str(courses_std)

