# run_harvest_uis.R
# Harvest pipeline for UiS (University of Stavanger)
# Supports multi-year harvesting via dropdown discovery + PDF extraction

library(dplyr)

source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

# --- 1) Prepare all UiS courses (all years, not just latest) ---
test_course_codes <- courses |>
  filter(institution_short == "uis") |>
  slice_sample(n = 10) |>
  purrr::pluck("Emnekode")

df <- courses |>
  filter(
         institution_short == "uis",
         #Emnekode %in% test_course_codes
         ) |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

message("UiS: ", nrow(df), " total course rows across all years")

# --- 2) Discovery phase: fetch base pages and parse dropdowns ---
# One base page per unique Emnekode_raw (the base URL is year-agnostic)
discovery_checkpoint_path <- "data/checkpoint/uis_discovery.RDS"

unique_codes <- df |>
  distinct(Emnekode_raw, url) |>
  filter(!is.na(url))

existing_discovery <- read_checkpoint(discovery_checkpoint_path)

if (is.null(existing_discovery)) {
  codes_to_discover <- unique_codes
} else {
  codes_to_discover <- anti_join(unique_codes, existing_discovery,
                                 by = "Emnekode_raw")
}

if (nrow(codes_to_discover) > 0) {
  message("Discovering dropdowns for ", nrow(codes_to_discover), " course codes...")

  discovery_results <- purrr::map2(
    codes_to_discover$Emnekode_raw,
    codes_to_discover$url,
    \(code, base_url) {
      tryCatch({
        resp <- base_url |>
          httr2::request() |>
          httr2::req_user_agent(
            "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
          ) |>
          httr2::req_perform()
        html <- httr2::resp_body_string(resp)
        parsed <- .parse_uis_semester_dropdown(html)
        parsed$Emnekode_raw <- code
        parsed
      }, error = \(e) {
        tibble::tibble(label = character(), url = character(),
                       type = character(), year = integer(),
                       Emnekode_raw = character())
      })
    },
    .progress = TRUE
  ) |>
    bind_rows()

  # Append to discovery checkpoint
  updated_discovery <- bind_rows(existing_discovery, discovery_results)
  write_checkpoint(updated_discovery, discovery_checkpoint_path)
  message("Discovery complete: ", nrow(discovery_results), " semester entries found")
} else {
  message("All course codes already discovered (checkpoint)")
  updated_discovery <- existing_discovery
}

# --- 3) Match discovered URLs with course rows ---
# Label "2025 - 2026" → year=2025 maps to Årstall=2025 (autumn start year)
discovered <- updated_discovery |>
  filter(!is.na(year))

df_matched <- df |>
  left_join(
    discovered |> select(Emnekode_raw, year, discovered_url = url, url_type = type),
    by = c("Emnekode_raw", "Årstall" = "year")
  )

# Use discovered URL when available, falling back to base URL for latest year
df_matched <- df_matched |>
  mutate(
    url = coalesce(discovered_url, url),
    url_type = coalesce(url_type, "html")  # default to html for base URL fallback
  )

message("Matched: ",
        sum(df_matched$url_type == "html", na.rm = TRUE), " HTML, ",
        sum(df_matched$url_type == "pdf", na.rm = TRUE), " PDF, ",
        sum(is.na(df_matched$url_type) | is.na(df_matched$url)), " unmatched")

# --- 4) Process HTML subset ---
df_html <- df_matched |> filter(url_type == "html", !is.na(url))

if (nrow(df_html) > 0) {
  message("Fetching HTML for ", nrow(df_html), " courses...")
  df_html <- fetch_html_with_checkpoint(
    df_html,
    checkpoint_path = "data/checkpoint/html_uis.RDS"
  )
  df_html$fulltext <- extract_fulltext(df_html$institution_short, df_html$html)
} else {
  df_html <- df_html |>
    mutate(html = NA_character_, html_error = list(NULL),
           html_success = NA, fulltext = NA_character_)
}

message("HTML subset: ", sum(!is.na(df_html$fulltext)), "/", nrow(df_html), " with fulltext")

# --- 5) Process PDF subset ---
df_pdf <- df_matched |> filter(url_type == "pdf", !is.na(url))

pdf_checkpoint_path <- "data/checkpoint/pdf_uis.RDS"
existing_pdf <- read_checkpoint(pdf_checkpoint_path)

if (nrow(df_pdf) > 0) {
  if (!is.null(existing_pdf)) {
    to_fetch_pdf <- anti_join(df_pdf, existing_pdf, by = "course_id")
  } else {
    to_fetch_pdf <- df_pdf
  }

  if (nrow(to_fetch_pdf) > 0) {
    message("Downloading ", nrow(to_fetch_pdf), " PDFs...")

    pdf_results <- purrr::map(
      to_fetch_pdf$url,
      \(pdf_url) {
        tryCatch({
          resp <- pdf_url |>
            httr2::request() |>
            httr2::req_user_agent(
              "TEPS research project - https://uni.oslomet.no/teps/ - robast@oslomet.no"
            ) |>
            httr2::req_perform()
          list(raw = httr2::resp_body_raw(resp), error = NULL, success = TRUE)
        }, error = \(e) {
          list(raw = NULL, error = conditionMessage(e), success = FALSE)
        })
      },
      .progress = TRUE
    )

    # Extract text from successfully downloaded PDFs
    fulltext <- extract_fulltext_pdf(purrr::map(pdf_results, "raw"))

    fetched_pdf <- tibble::tibble(
      course_id = to_fetch_pdf$course_id,
      html = NA_character_,  # PDFs don't have HTML
      html_error = purrr::map(pdf_results, \(r) r$error),
      html_success = purrr::map_lgl(pdf_results, \(r) r$success),
      fulltext = fulltext
    )

    # Update PDF checkpoint
    updated_pdf <- bind_rows(existing_pdf, fetched_pdf)
    write_checkpoint(updated_pdf, pdf_checkpoint_path)
  } else {
    updated_pdf <- existing_pdf
  }

  # Join PDF results back to df_pdf
  df_pdf <- df_pdf |>
    left_join(
      updated_pdf |> select(course_id, html, html_error, html_success, fulltext),
      by = "course_id"
    )
} else {
  df_pdf <- df_pdf |>
    mutate(html = NA_character_, html_error = list(NULL),
           html_success = NA, fulltext = NA_character_)
}

message("PDF subset: ", sum(!is.na(df_pdf$fulltext)), "/", nrow(df_pdf), " with fulltext")

# --- 6) Handle unmatched rows (no discovered URL) ---
df_none <- df_matched |> filter(is.na(url) | (!url_type %in% c("html", "pdf")))
if (nrow(df_none) > 0) {
  df_none <- df_none |>
    mutate(html = NA_character_, html_error = list(NULL),
           html_success = NA, fulltext = NA_character_)
}

# --- 7) Combine and save ---
# bind_rows handles different column sets automatically
result <- bind_rows(df_html, df_pdf, df_none) |>
  select(-any_of(c("discovered_url", "url_type")))

saveRDS(result, "data/html_uis.RDS")

message("UiS done: ", sum(!is.na(result$fulltext)), "/", nrow(result),
        " with fulltext across ", n_distinct(result$Årstall), " years")
