# R/inspect_modes.R
# Scan all generate_urls_*.R and extract MODE / YEAR_* settings.

stopifnot(dir.exists("R"))

# -- find scripts
scripts <- unique(c(
  Sys.glob(file.path("R", "generate_urls_*.R")),
  Sys.glob(file.path("R", "*", "generate_urls_*.R"))
))
if (!length(scripts)) stop("No generate_urls_*.R found under R/")

# -- tiny safe reader (tolerates enc issues)
read_text <- function(path) {
  txt <- try(readLines(path, warn = FALSE, encoding = "UTF-8"), silent = TRUE)
  if (!inherits(txt, "try-error")) return(txt)
  raw <- readBin(path, "raw", file.info(path)$size)
  s <- rawToChar(raw); Encoding(s) <- "unknown"
  s <- sub("^\ufeff", "", s)
  s <- gsub("\r\n?", "\n", s)
  strsplit(s, "\n", fixed = TRUE)[[1]]
}

# -- strip line comments (simple: cut at # if not inside quotes)
strip_comments <- function(lines) {
  out <- character(length(lines))
  for (i in seq_along(lines)) {
    x <- lines[i]
    # walk & remove trailing # if not in quotes
    in_s <- FALSE; in_d <- FALSE; cut <- nchar(x) + 1L
    for (j in seq_along(strsplit(x, "")[[1]])) {
      ch <- substr(x, j, j)
      if (ch == "'" && !in_d) in_s <- !in_s
      if (ch == '"' && !in_s) in_d <- !in_d
      if (ch == "#" && !in_s && !in_d) { cut <- j; break }
    }
    out[i] <- substr(x, 1L, cut - 1L)
  }
  out
}

# -- generic extractor: NAME <- "VALUE" / NAME = "VALUE"
grab_scalar <- function(lines, name, pat = '"?([A-Za-z0-9._-]+)"?') {
  rx <- sprintf("\\b%s\\s*(?:<-|=)\\s*%s", name, pat)
  m  <- regexpr(rx, lines, perl = TRUE, ignore.case = FALSE)
  hit <- which(m > 0L)
  if (!length(hit)) return(NA_character_)
  # take first match; extract group 1
  line <- lines[hit[1]]
  mm <- regexec(rx, line, perl = TRUE)
  regmatches(line, mm)[[1]][2]
}

grab_int <- function(lines, name) {
  v <- grab_scalar(lines, name, pat = "([0-9]+)\\s*[Ll]?")
  if (is.na(v)) return(NA_integer_)
  as.integer(v)
}

inspect_one <- function(path) {
  lines <- read_text(path)
  lines <- strip_comments(lines)
  # collapse multiple spaces
  lines <- gsub("[ \t]+", " ", lines)
  
  inst <- sub("^generate_urls_([^.]+)\\.R$", "\\1", basename(path))
  
  mode        <- grab_scalar(lines, "MODE", '"?([A-Za-z]+)"?')
  year_h      <- grab_int(lines, "YEAR_H")
  year_v      <- grab_int(lines, "YEAR_V")
  single_year <- grab_int(lines, "SINGLE_YEAR")
  single_hv   <- grab_scalar(lines, "SINGLE_HV", '"?([HVhv])"?')
  
  data.frame(
    inst = inst,
    MODE = ifelse(is.na(mode), "(not set; default hv?)", mode),
    YEAR_H = year_h,
    YEAR_V = year_v,
    SINGLE_YEAR = single_year,
    SINGLE_HV = ifelse(is.na(single_hv), NA, toupper(single_hv)),
    path = path,
    stringsAsFactors = FALSE
  )
}

df <- do.call(rbind, lapply(sort(scripts), inspect_one))

# pretty print
df <- df[order(df$inst), ]
row.names(df) <- NULL
print(df, row.names = FALSE)
