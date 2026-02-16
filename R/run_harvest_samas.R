# run_harvest_samas.R
# Harvest pipeline for Sámi allaskuvla (samas) — PDF-based course plans
#
# Website: samas.no/se/oahput (Northern Sámi)
# Course pages link to Oahppoplána PDFs. Some pages are programs (teacher ed,
# master) containing multiple emner; others are individual courses.
# DBH codes use V1 (1-7 teacher ed) and V5 (5-10 teacher ed) prefixes.
# Website codes (SÁM-1005, DUO-1014) don't match DBH numbering, so we match
# by subject abbreviation + course name similarity.

library(dplyr)
library(purrr)
library(stringr)
library(rvest)
library(pdftools)
source("R/utils.R")

dir.create("temp/samas", showWarnings = FALSE, recursive = TRUE)

message("=== Starting: Sámi allaskuvla PDF harvest ===")

# --- Step 1: Load courses from courses.RDS (current year only) ---
courses <- readRDS("data/courses.RDS") |>
  filter(institution_short == "samas", Årstall == max(Årstall)) |>
  add_course_id()

message("Loaded ", nrow(courses), " samas courses (",
        n_distinct(courses$Emnekode_raw), " unique codes)")

# --- Step 2: Scrape course listing pages ---
message("Scraping course listing from samas.no/se/oahput...")

web_courses <- list()
for (page in 0:1) {
  url <- paste0("https://samas.no/se/oahput?page=", page)
  html <- read_html(url)
  links <- html |> html_nodes("a[href]")
  web_courses[[page + 1]] <- tibble(
    web_name = html_text2(links),
    href = html_attr(links, "href")
  ) |>
    filter(str_detect(href, "^/se/(studie|studier|node)/"), nchar(web_name) > 3) |>
    distinct()
}
web_courses <- bind_rows(web_courses) |> distinct()
message("Found ", nrow(web_courses), " course pages")

# --- Step 3: Fetch PDF URLs from each course page ---
message("Fetching PDF links from course pages...")

get_pdf_link <- function(path) {
  url <- paste0("https://samas.no", path)
  Sys.sleep(1)
  html <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(html)) return(NA_character_)
  pdf_links <- html |> html_nodes("a[href*='.pdf']") |> html_attr("href")
  if (length(pdf_links) == 0) return(NA_character_)
  pdf_links[1]
}

web_courses$pdf_url <- map_chr(web_courses$href, get_pdf_link, .progress = TRUE)
web_courses <- web_courses |> filter(!is.na(pdf_url))
message("Found ", nrow(web_courses), " courses with PDFs")

# --- Step 4: Download PDFs ---
message("Downloading PDFs...")

download_pdf <- function(pdf_url, i) {
  local_path <- file.path("temp/samas", paste0("course_", i, ".pdf"))
  if (!file.exists(local_path)) {
    tryCatch(
      download.file(pdf_url, local_path, mode = "wb", quiet = TRUE),
      error = function(e) message("  Failed: ", pdf_url)
    )
  }
  local_path
}

web_courses$local_path <- map2_chr(
  web_courses$pdf_url, seq_len(nrow(web_courses)), download_pdf
)

# --- Step 5: Extract text from PDFs ---
message("Extracting text from PDFs...")

extract_pdf_text <- function(path) {
  pages <- tryCatch(pdf_text(path), error = function(e) NULL)
  if (is.null(pages)) return(NA_character_)
  paste(pages, collapse = "\n\n")
}

web_courses$fulltext <- map_chr(web_courses$local_path, extract_pdf_text)
message("Extracted text from ", sum(!is.na(web_courses$fulltext)), " PDFs")

# --- Step 6: Extract website course codes and build subject key ---
# Website codes: SÁM-1005, DUO-1014, SER 103, OKT 305, JUR 103, SÁÁL 1, GMOA, PED-6501
# Normalize: remove diacritics from subject abbreviation for matching
web_courses <- web_courses |>
  mutate(
    web_code = str_extract(web_name, "^[A-ZÁÉÍÓÚÀÈÌÒÙÄËÏÖÜÂÊÎÔÛČĐŊŠŦŽ]+([-\\s]\\d+)?"),
    web_subject = str_extract(web_code, "^[A-ZÁÉÍÓÚÀÈÌÒÙÄËÏÖÜÂÊÎÔÛČĐŊŠŦŽ]+") |>
      str_replace_all("Á", "A") |>
      str_replace_all("É", "E")
  )

# DBH codes: V1SAM-1100-1, V5DUO-1140-1, etc.
# Extract subject abbreviation from DBH codes
courses <- courses |>
  mutate(
    dbh_subject = str_extract(Emnekode_raw, "(?<=V[15])[A-Z]+")
  )

# --- Step 7: Match by subject abbreviation ---
# Map website subjects to DBH subjects
subject_map <- tribble(
  ~web_subject, ~dbh_subject,
  "SAM",   "SAM",
  "DUO",   "DUO",
  "SER",   "SER",
  "OKT",   NA_character_,   # Master-level, may not be in teacher ed DBH codes
  "JUR",   NA_character_,   # Standalone course
  "SAAL",  NA_character_,   # Language intro course
  "PED",   "PAC",           # Pedagogikk
  "GMOA",  NA_character_,   # Kindergarten program
  "MAT",   "MAT",
  "LUO",   "LUO"
)

# For each DBH course, try to find a matching web course
# Priority: subject match, then if multiple, pick by name similarity
match_web_course <- function(dbh_subject, emnenavn, web_df) {
  if (is.na(dbh_subject)) return(NA_integer_)

  candidates <- which(web_df$web_subject == dbh_subject)
  if (length(candidates) == 0) return(NA_integer_)
  if (length(candidates) == 1) return(candidates[1])

  # Multiple candidates: pick best name match
  # Use simple word overlap
  emne_words <- str_to_lower(emnenavn) |> str_split("\\s+") |> pluck(1)
  scores <- map_dbl(candidates, function(i) {
    web_words <- str_to_lower(web_df$web_name[i]) |> str_split("\\s+") |> pluck(1)
    length(intersect(emne_words, web_words))
  })
  candidates[which.max(scores)]
}

courses$web_idx <- map2_int(
  courses$dbh_subject, courses$Emnenavn,
  ~match_web_course(.x, .y, web_courses)
)

# Also try matching program pages to courses without a subject match
# Teacher ed programs (1-7 ceahki → V1, 5-10 ceahki → V5)
program_1_7 <- which(str_detect(web_courses$web_name, "1\\.-7\\. ceahki"))
program_5_10 <- which(str_detect(web_courses$web_name, "5\\.-10\\. ceahki"))

# Assign program PDF to unmatched V1/V5 courses
courses <- courses |>
  mutate(
    prefix = str_extract(Emnekode_raw, "^V[15]"),
    web_idx = case_when(
      !is.na(web_idx) ~ web_idx,
      prefix == "V1" & length(program_1_7) > 0 ~ program_1_7[1],
      prefix == "V5" & length(program_5_10) > 0 ~ program_5_10[1],
      TRUE ~ web_idx
    )
  )

# Join fulltext
courses$url <- ifelse(
  !is.na(courses$web_idx),
  paste0("https://samas.no", web_courses$href[courses$web_idx]),
  NA_character_
)
courses$fulltext <- ifelse(
  !is.na(courses$web_idx),
  web_courses$fulltext[courses$web_idx],
  NA_character_
)

# Clean up temp columns
courses <- courses |> select(-web_idx, -dbh_subject, -prefix)

message(sum(!is.na(courses$fulltext)), "/", nrow(courses), " rows with fulltext")

# --- Step 8: Save ---
saveRDS(courses, "data/html_samas.RDS")

message("=== Done: Sámi allaskuvla - saved to data/html_samas.RDS ===")
message("Courses with fulltext: ", sum(!is.na(courses$fulltext)), "/", nrow(courses))
message("Courses without PDF: ",
        sum(is.na(courses$fulltext) & !duplicated(courses$Emnekode_raw)), " unique codes")
