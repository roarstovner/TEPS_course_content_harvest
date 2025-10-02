options(encoding = "UTF-8")
Sys.setlocale(category = "LC_CTYPE", locale = "C")   # eller en UTF-8-locale hvis tilgjengelig

# skal returnere en character-vektor uten warnings:
x <- readLines("scripts/00_run_all.R", encoding = "UTF-8", warn = TRUE)

# viser evt. ikke-ASCII posisjoner hvis noe fortsatt er rart:
tools::showNonASCIIfile("scripts/00_run_all.R")


# Pakker (installer ved behov)
suppressPackageStartupMessages({
  if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")
  if (!requireNamespace("tools", quietly = TRUE))   install.packages("tools")
})

# A) Rappporter ikke-ASCII-tegn i 02_generate_urls.R
tools::showNonASCIIfile("scripts/02_generate_urls.R")

# B) Oppdag sannsynlig encoding
raw <- readBin("scripts/02_generate_urls.R", "raw", file.info("scripts/02_generate_urls.R")$size)
cat("BOM? ", startsWith(tolower(paste(format(raw[1:3]), collapse=" ")), "ef bb bf"), "\n")
enc_guess <- stringi::stri_enc_detect(rawToChar(raw))
enc_guess[1]  # se forslag (latin1/windows-1252/utf-8 etc.)

scan_bad_bytes <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  has_nul <- any(raw == as.raw(0x00))
  # Aksepter ASCII kontroll: TAB(09), LF(0A), CR(0D), ellers 0x20–0x7E
  ok <- raw %in% as.raw(c(0x09,0x0A,0x0D, 0x20:0x7E))
  bad_idx <- which(!ok)
  list(
    has_nul = has_nul,
    n_bad = length(bad_idx),
    bad_preview = utils::head(bad_idx, 50),
    hex_preview = sapply(utils::head(bad_idx, 50), function(i) sprintf("pos %d: 0x%02X", i, as.integer(raw[i])))
  )
}

scan_bad_bytes("scripts/02_generate_urls.R")
