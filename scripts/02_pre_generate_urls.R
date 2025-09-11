

## Last opp datasett for neste steg - generere urls ##
courses <- readRDS("data/cache/courses_std.RDS")

# -*- coding: UTF-8 -*-

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a


safe_read_yaml <- function(path) {
  # pr??v vanlig UTF-8-lesing f??rst
  try_utf8 <- try({
    con <- file(path, open = "r", encoding = "UTF-8")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    yaml::read_yaml(con)
  }, silent = TRUE)
  if (!inherits(try_utf8, "try-error") && !is.null(try_utf8)) return(try_utf8)
  
  # fallback: les r?? bytes, fjern BOM, normaliser linjeslutt, konverter til UTF-8 og parse
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  txt <- rawToChar(raw); Encoding(txt) <- "unknown"
  txt <- sub("^\ufeff", "", txt)        # fjern BOM
  txt <- gsub("\r\n?", "\n", txt)       # CRLF/CR -> LF
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0 || nzchar(tail(lines, 1))) lines <- c(lines, "")  # s??rg for siste linjeskift
  # erstatt tabs med spaces (YAML liker ikke tabs)
  lines <- gsub("\t", "  ", lines, fixed = TRUE)
  yaml::yaml.load(paste(lines, collapse = "\n"))
}





