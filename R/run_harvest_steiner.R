# run_harvest_steiner.R
# Harvest pipeline for Steinerhøyskolen - PDFs with multiple emner per file
#
# Steiner course plans are PDFs hosted on steinerhoyskolen.no. Each PDF covers
# one subject (e.g., Norsk 1) and contains multiple emne sections. We download
# the PDFs, split them by emne heading, then join fulltext onto courses.RDS rows.
#
# Special case: Pedagogikk PDF has emne 1-5 continuously, but courses.RDS splits
# them into M-PEL1 (emne 1-3) and M-PEL2 (emne 1-2, corresponding to PDF emne 4-5).

library(dplyr)
library(purrr)
library(stringr)
library(rvest)
library(pdftools)
source("R/utils.R")

dir.create("temp/steiner", showWarnings = FALSE, recursive = TRUE)

message("=== Starting: Steinerhøyskolen PDF harvest ===")

# --- Step 1: Load courses from courses.RDS ---
# PDFs are current course plans — only apply to current year rows
courses <- readRDS("data/courses.RDS") |>
  filter(institution_short == "steiner", Årstall == max(Årstall)) |>
  add_course_id()

message("Loaded ", nrow(courses), " steiner courses (",
        n_distinct(courses$Emnenavn), " unique emner)")

# --- Step 2: Define wrapper pages for each subject PDF ---
wrapper_pages <- tibble::tibble(
  wrapper_url = c(
    "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-norsk-1",
    "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-matematikk-1",
    "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-naturfag-1",
    "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-pedagogikk-og-elevkunnskap-1",
    "https://www.steinerhoyskolen.no/dokumenter/laerer-bachelor-samfunnsfag-1"
  ),
  subject = c("Norsk", "Matematikk", "Naturfag",
              "Pedagogikk og elevkunnskap", "Samfunnsfag")
)

# --- Step 3: Extract actual PDF URLs from wrapper pages ---
get_pdf_url <- function(wrapper_url) {
  message("  Fetching wrapper: ", basename(wrapper_url))
  html <- tryCatch(read_html(wrapper_url), error = function(e) NULL)
  if (is.null(html)) return(NA_character_)

  page_text <- html |> html_text()
  pdf_urls <- str_extract_all(
    page_text, "https?://[^\"'\\s]+\\.pdf[^\"'\\s]*"
  )[[1]]

  # Also check embedded elements

  pdf_from_attrs <- html |>
    html_nodes("iframe, embed, object") |>
    html_attr("src") |>
    str_subset("\\.pdf")

  all_pdfs <- unique(c(pdf_urls, pdf_from_attrs))
  if (length(all_pdfs) == 0) return(NA_character_)
  all_pdfs[1]
}

wrapper_pages <- wrapper_pages |>
  mutate(pdf_url = map_chr(wrapper_url, get_pdf_url)) |>
  filter(!is.na(pdf_url))

message("Found ", nrow(wrapper_pages), " PDF URLs")

# --- Step 4: Download PDFs ---
download_pdf <- function(pdf_url, subject) {
  filename <- subject |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_-]", "") |>
    tolower()
  local_path <- file.path("temp/steiner", paste0(filename, ".pdf"))

  if (!file.exists(local_path)) {
    message("  Downloading: ", subject)
    download.file(pdf_url, local_path, mode = "wb", quiet = TRUE)
  }
  local_path
}

wrapper_pages <- wrapper_pages |>
  mutate(local_path = map2_chr(pdf_url, subject, download_pdf))

# --- Step 5: Extract emne sections from each PDF ---
extract_emner_from_pdf <- function(pdf_path, subject, wrapper_url) {
  pages <- tryCatch(pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages)) return(tibble())

  # Detect emne boundaries: page starts with "{Subject} N, emne M"
  emne_pattern <- paste0(
    "^\\s*",
    str_replace_all(subject, "\\s+", "\\\\s+"),
    "\\s+\\d+\\s*,\\s*emne\\s+\\d+"
  )

  page_is_emne_start <- map_lgl(pages, function(p) {
    first_line <- str_split(p, "\n")[[1]][1]
    str_detect(first_line, regex(emne_pattern, ignore_case = TRUE))
  })

  # Drop pages that match "emne N-M" (overview sections, not individual emner)
  page_is_overview <- map_lgl(pages, function(p) {
    first_line <- str_split(p, "\n")[[1]][1]
    str_detect(first_line, regex("emne\\s+\\d+\\s*-\\s*\\d+", ignore_case = TRUE))
  })

  emne_start_pages <- which(page_is_emne_start & !page_is_overview)
  if (length(emne_start_pages) == 0) return(tibble())

  message("  ", subject, ": ", length(emne_start_pages), " emner at pages ",
          paste(emne_start_pages, collapse = ", "))

  # Build one row per emne section

  map_dfr(seq_along(emne_start_pages), function(i) {
    start <- emne_start_pages[i]
    # End at next emne start (including overviews) or end of PDF
    all_starts <- which(page_is_emne_start | page_is_overview)
    next_starts <- all_starts[all_starts > start]
    end <- if (length(next_starts) > 0) next_starts[1] - 1 else length(pages)

    emne_text <- paste(pages[start:end], collapse = "\n\n")
    first_line <- str_trim(str_split(pages[start], "\n")[[1]][1])

    tibble(
      pdf_emne_name = first_line,
      url = wrapper_url,
      fulltext = emne_text
    )
  })
}

pdf_emner <- pmap_dfr(
  list(wrapper_pages$local_path, wrapper_pages$subject, wrapper_pages$wrapper_url),
  extract_emner_from_pdf
)

message("Extracted ", nrow(pdf_emner), " emne sections from PDFs")

# --- Step 6: Build join key to match courses.RDS Emnenavn ---
# Normalize: lowercase, collapse whitespace, remove space between subject and number
# "Norsk 1, emne 1" → "norsk1, emne 1" (but keep "emne 1" intact)
normalize_emne_name <- function(x) {
  x |>
    str_squish() |>
    str_to_lower() |>
    str_replace("^(.+?)\\s+(\\d+)\\s*,", "\\1\\2,")  # "norsk 1," → "norsk1,"
}

# The PEL special case: PDF has "pedagogikk og elevkunnskap1, emne 4" and "emne 5"
# but courses.RDS has M-PEL2 "pedagogikk og elevkunnskap2, emne 1" and "emne 2"
# Map: PEL emne 4 → PEL2 emne 1, PEL emne 5 → PEL2 emne 2
remap_pel_emne <- function(name) {
  if (str_detect(name, "^pedagogikk og elevkunnskap1, emne 4$")) {
    return("pedagogikk og elevkunnskap2, emne 1")
  }
  if (str_detect(name, "^pedagogikk og elevkunnskap1, emne 5$")) {
    return("pedagogikk og elevkunnskap2, emne 2")
  }
  name
}

pdf_emner <- pdf_emner |>
  mutate(
    join_key = normalize_emne_name(pdf_emne_name) |>
      map_chr(remap_pel_emne)
  )

# Normalize courses.RDS side
courses <- courses |>
  mutate(join_key = normalize_emne_name(Emnenavn))

# Verify the join will work
pdf_keys <- unique(pdf_emner$join_key)
course_keys <- unique(courses$join_key)
matched <- intersect(pdf_keys, course_keys)
unmatched_pdf <- setdiff(pdf_keys, course_keys)
unmatched_course <- setdiff(course_keys, pdf_keys)

message("Join keys: ", length(matched), " matched, ",
        length(unmatched_pdf), " PDF-only, ",
        length(unmatched_course), " courses-only")
if (length(unmatched_pdf) > 0) {
  message("  Unmatched PDF emner: ", paste(unmatched_pdf, collapse = "; "))
}
if (length(unmatched_course) > 0) {
  message("  Unmatched courses (no PDF): ", paste(unmatched_course, collapse = "; "))
}

# --- Step 7: Join fulltext onto courses ---
pdf_lookup <- pdf_emner |> select(join_key, url, fulltext)

df <- courses |>
  left_join(pdf_lookup, by = "join_key") |>
  select(-join_key)

message(sum(!is.na(df$fulltext)), "/", nrow(df), " rows with fulltext")

# --- Step 8: Save ---
saveRDS(df, "data/html_steiner.RDS")

message("=== Done: Steinerhøyskolen - saved to data/html_steiner.RDS ===")
message("Courses with fulltext: ", sum(!is.na(df$fulltext)), "/", nrow(df))
message("Courses without PDF (Praksis etc): ",
        sum(is.na(df$fulltext) & !duplicated(df$Emnenavn)), " unique emner")
