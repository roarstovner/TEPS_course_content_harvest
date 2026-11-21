# R/harvest_strategies.R
# Strategy implementations for the config-driven harvest pipeline.
# Each function receives (df, config) and returns df with url, html,
# html_error (text), html_success, extracted_text columns. Checkpoints are in
# R/checkpoint.R; harvest_institution() removes them for a refetch.

# --- Helpers ---

#' Ensure uniform output columns exist on df
#' Adds missing columns with appropriate NA types.
ensure_output_columns <- function(df) {
  if (!"url" %in% names(df)) df$url <- NA_character_
  if (!"html" %in% names(df)) df$html <- NA_character_
  if (!"html_error" %in% names(df)) df$html_error <- NA_character_
  if (!"html_success" %in% names(df)) df$html_success <- NA
  if (!"extracted_text" %in% names(df)) df$extracted_text <- NA_character_
  df
}

# --- Strategy: standard ---
# URL -> HTTP fetch -> CSS extract
# Used by: oslomet, uia, ntnu, inn, hvl, mf, nih, nmbu, uib, nord

harvest_standard <- function(df, config) {
  df <- fetch_html_with_checkpoint(df, checkpoint_dir(config$name, "html", df$year), config = config)

  df$extracted_text <- extract_fulltext_from_raw(df, config)
  df
}

# --- Strategy: url_discovery ---
# URL -> discover real URL -> HTTP fetch -> CSS extract
# Used by: hivolda, uit, uio, hiof

harvest_url_discovery <- function(df, config) {
  # Resolve URLs (hivolda, uit, uio have institution-specific batch resolvers)
  df <- resolve_course_urls(df, checkpoint_path = checkpoint_dir(config$name, "urls"))

  # Fetch HTML
  df <- fetch_html_with_checkpoint(df, checkpoint_dir(config$name, "html", df$year), config = config)

  # Extract fulltext
  df$extracted_text <- extract_fulltext_from_raw(df, config)
  df
}

# --- Strategy: shadow_dom ---
# URL -> Chrome + Shadow DOM extraction (URL discovery + fetch combined)
# Used by: usn

harvest_shadow_dom <- function(df, config) {
  # resolve_course_urls for USN also fetches HTML (combined step)
  df <- resolve_course_urls(df, checkpoint_path = checkpoint_dir(config$name, "urls"))

  # Add html_success/html_error for output consistency
  df <- df |>
    dplyr::mutate(
      html_success = !is.na(html) & html != "",
      html_error = NA_character_
    )

  # USN stores pre-rendered text, not raw HTML (cleanup, not CSS extraction)
  df$extracted_text <- extract_fulltext_from_raw(df, config)
  df
}

# --- Strategy: html_pdf_discovery ---
# URL -> discover dropdown -> HTML or PDF fetch -> extract
# Used by: uis

harvest_html_pdf_discovery <- function(df, config) {
  discovery_cp <- checkpoint_dir(config$name, "discovery")
  pdf_cp <- checkpoint_dir(config$name, "pdf")

  # --- Discovery phase: fetch base pages and parse dropdowns ---
  unique_codes <- df |>
    dplyr::distinct(course_code_raw, url) |>
    dplyr::filter(!is.na(url))

  existing_discovery <- read_checkpoint(discovery_cp, "course_code_raw", unique_codes$course_code_raw)

  if (is.null(existing_discovery)) {
    codes_to_discover <- unique_codes
  } else {
    codes_to_discover <- dplyr::anti_join(unique_codes, existing_discovery,
                                           by = "course_code_raw")
  }

  if (nrow(codes_to_discover) > 0) {
    message("Discovering dropdowns for ", nrow(codes_to_discover), " course codes...")

    refused <- refusal_watch(config)
    discovery_results <- purrr::map2(
      codes_to_discover$course_code_raw,
      codes_to_discover$url,
      \(code, base_url) {
        if (robots_allows(base_url)) polite_pause(config$request_delay)
        tryCatch({
          resp <- site_request(base_url) |> site_perform()
          refused(httr2::resp_status(resp), base_url)
          html <- httr2::resp_body_string(resp)
          parsed <- .parse_uis_semester_dropdown(html)
          parsed$course_code_raw <- code
          parsed
        }, error = \(e) {
          refused(http_status_of(e), base_url)
          tibble::tibble(label = character(), url = character(),
                         type = character(), year = integer(),
                         course_code_raw = character())
        })
      },
      .progress = TRUE
    ) |>
      dplyr::bind_rows()

    checkpoint_append(discovery_results, discovery_cp)
    updated_discovery <- dplyr::bind_rows(existing_discovery, discovery_results)
    message("Discovery complete: ", nrow(discovery_results), " semester entries found")
  } else {
    message("All course codes already discovered (checkpoint)")
    updated_discovery <- existing_discovery
  }

  # --- Match discovered URLs with course rows ---
  # A menu entry is an academic year ("2024 - 2025"): a spring or summer
  # offering takes the year before (url_year = "academic").
  discovered <- updated_discovery |> dplyr::filter(!is.na(year))

  df_matched <- df |>
    dplyr::mutate(.year = academic_start_year(year, semester_name)) |>
    dplyr::left_join(
      discovered |> dplyr::select(course_code_raw, .year = year, discovered_url = url, url_type = type),
      by = c("course_code_raw", ".year")
    ) |>
    dplyr::select(-.year) |>
    dplyr::mutate(
      url = dplyr::coalesce(discovered_url, url),
      url_type = dplyr::coalesce(url_type, "html")
    )

  message("Matched: ",
          sum(df_matched$url_type == "html", na.rm = TRUE), " HTML, ",
          sum(df_matched$url_type == "pdf", na.rm = TRUE), " PDF, ",
          sum(is.na(df_matched$url_type) | is.na(df_matched$url)), " unmatched")

  # --- Process HTML subset ---
  df_html <- df_matched |> dplyr::filter(url_type == "html", !is.na(url))

  if (nrow(df_html) > 0) {
    df_html <- fetch_html_with_checkpoint(df_html, checkpoint_dir(config$name, "html", df$year),
                                          config = config)
    df_html$extracted_text <- extract_fulltext_from_raw(df_html, config)
  } else {
    df_html <- df_html |>
      dplyr::mutate(html = NA_character_, html_error = NA_character_,
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Process PDF subset ---
  df_pdf <- df_matched |> dplyr::filter(url_type == "pdf", !is.na(url))
  existing_pdf <- read_checkpoint(pdf_cp, "course_id", df_pdf$course_id)

  if (nrow(df_pdf) > 0) {
    if (!is.null(existing_pdf)) {
      to_fetch_pdf <- dplyr::anti_join(df_pdf, existing_pdf, by = "course_id")
    } else {
      to_fetch_pdf <- df_pdf
    }

    if (nrow(to_fetch_pdf) > 0) {
      message("Downloading ", nrow(to_fetch_pdf), " PDFs...")
      # robots.txt disallows the PDF archive, www6.uis.no (data/robots_txt.csv)
      skip <- sum(!robots_allows(to_fetch_pdf$url))
      if (skip) message("robots.txt disallows ", skip, " of them: not requested")

      refused <- refusal_watch(config)
      pdf_results <- purrr::map(
        to_fetch_pdf$url,
        \(pdf_url) {
          if (robots_allows(pdf_url)) polite_pause(config$request_delay)
          requested_at <- Sys.time()
          tryCatch({
            resp <- site_request(pdf_url) |> site_perform()
            refused(httr2::resp_status(resp), pdf_url)
            list(raw = httr2::resp_body_raw(resp), error = NULL, success = TRUE,
                 status = httr2::resp_status(resp), requested_at = requested_at)
          }, error = \(e) {
            refused(http_status_of(e), pdf_url)
            list(raw = NULL, error = conditionMessage(e), success = FALSE,
                 status = http_status_of(e), requested_at = requested_at)
          })
        },
        .progress = TRUE
      )

      extracted_text <- extract_fulltext_pdf(purrr::map(pdf_results, "raw"))

      # the PDF is kept beside its text (`pdf`), so it can be read again
      fetched_pdf <- tibble::tibble(
        course_id = to_fetch_pdf$course_id,
        html = NA_character_,
        html_error = purrr::map_chr(pdf_results, \(r) r$error %||% NA_character_),
        html_success = purrr::map_lgl(pdf_results, \(r) r$success),
        extracted_text = extracted_text,
        pdf = purrr::map(pdf_results, "raw"),
        http_status = purrr::map_int(pdf_results, "status"),
        requested_at = .POSIXct(purrr::map_dbl(pdf_results, "requested_at"))
      )

      checkpoint_append(fetched_pdf, pdf_cp)
      updated_pdf <- dplyr::bind_rows(existing_pdf, fetched_pdf)
    } else {
      updated_pdf <- existing_pdf
    }

    df_pdf <- df_pdf |>
      dplyr::left_join(
        updated_pdf |> dplyr::select(course_id, html, html_error, html_success, extracted_text,
                                     dplyr::any_of(c("pdf", REQUEST_COLUMNS))),
        by = "course_id"
      )
  } else {
    df_pdf <- df_pdf |>
      dplyr::mutate(html = NA_character_, html_error = NA_character_,
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Handle unmatched rows ---
  df_none <- df_matched |> dplyr::filter(is.na(url) | (!url_type %in% c("html", "pdf")))
  if (nrow(df_none) > 0) {
    df_none <- df_none |>
      dplyr::mutate(html = NA_character_, html_error = NA_character_,
                    html_success = NA, extracted_text = NA_character_)
  }

  # --- Combine and clean up ---
  dplyr::bind_rows(df_html, df_pdf, df_none) |>
    dplyr::select(-dplyr::any_of(c("discovered_url", "url_type")))
}

# --- Strategy: pdf_split ---
# Fetch wrapper page -> download PDF -> split by emne section
# Used by: steiner

harvest_pdf_split <- function(df, config) {
  # --- Define wrapper pages for each subject PDF ---
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

  # robots.txt disallows the pages' folder, /dokumenter/ (data/robots_txt.csv):
  # without a permission nothing is requested, and each offering says why
  if (!all(robots_allows(wrapper_pages$wrapper_url))) {
    why <- robots_refusal(sub("[^/]*$", "", wrapper_pages$wrapper_url[1]))
    message(why)
    df$html_error <- rep(why, nrow(df))
    return(df)
  }

  # --- Extract PDF URLs from wrapper pages ---
  wrapper_pages <- wrapper_pages |>
    dplyr::mutate(pdf_url = purrr::map_chr(wrapper_url, .steiner_get_pdf_url)) |>
    dplyr::filter(!is.na(pdf_url))

  message("Found ", nrow(wrapper_pages), " PDF URLs")

  # --- Download PDFs (each harvest fetches them anew, into memory) ---
  wrapper_pages$pdf <- purrr::map(wrapper_pages$pdf_url, .steiner_download_pdf)

  # --- Extract emne sections ---
  pdf_emner <- purrr::pmap_dfr(
    list(wrapper_pages$pdf, wrapper_pages$subject, wrapper_pages$wrapper_url),
    .steiner_extract_emner_from_pdf
  )

  message("Extracted ", nrow(pdf_emner), " emne sections from PDFs")
  if (nrow(pdf_emner) == 0) {   # no PDF read or split: each offering says so
    df$html_error <- rep("no emne read from Steiner's subject PDFs", nrow(df))
    return(df)
  }

  # --- Build join key and match to courses ---
  pdf_emner <- pdf_emner |>
    dplyr::mutate(join_key = purrr::map(.steiner_normalize_emne_name(pdf_emne_name),
                                        .steiner_remap_pel_emne)) |>
    tidyr::unnest_longer(join_key)

  df <- df |>
    dplyr::mutate(join_key = .steiner_normalize_emne_name(course_name))

  # Diagnostics
  pdf_keys <- unique(pdf_emner$join_key)
  course_keys <- unique(df$join_key)
  matched <- intersect(pdf_keys, course_keys)
  unmatched_pdf <- setdiff(pdf_keys, course_keys)
  unmatched_course <- setdiff(course_keys, pdf_keys)

  message("Join keys: ", length(matched), " matched, ",
          length(unmatched_pdf), " PDF-only, ",
          length(unmatched_course), " courses-only")
  if (length(unmatched_pdf) > 0) {
    message("  Unmatched PDF emner: ", paste(unmatched_pdf, collapse = "; "))
  }

  # --- Join fulltext ---
  pdf_lookup <- pdf_emner |> dplyr::select(join_key, url, extracted_text)

  # the URL builder's `url` is NA here: the join's is the wrapper's (runs until
  # 2026-11 split it into url.x, NA, and url.y, and `url` was NA)
  df <- df |>
    dplyr::select(-dplyr::any_of(c("url", "extracted_text"))) |>
    dplyr::left_join(pdf_lookup, by = "join_key") |>
    dplyr::select(-join_key)

  message(sum(!is.na(df$extracted_text)), "/", nrow(df), " rows with extracted text")
  .keep_pdf_once(df, wrapper_pages)
}

# A PDF's bytes on the first row its text joined to (`pdf`, the raw store's
# binary column; `wrapper_url` the row's `url`), NULL on the others: a PDF
# holds the plans of several offerings (Steiner: some 7), so a copy per row
# would add ~20 MB a run (steiner, ldh).
.keep_pdf_once <- function(df, wrapper_pages) {
  first <- match(wrapper_pages$wrapper_url, ifelse(is.na(df$extracted_text), NA, df$url))
  df$pdf <- vector("list", nrow(df))
  df$pdf[first[!is.na(first)]] <- wrapper_pages$pdf[!is.na(first)]
  df
}

# Steiner helper: extract PDF URL from wrapper page
.steiner_get_pdf_url <- function(wrapper_url) {
  message("  Fetching wrapper: ", basename(wrapper_url))
  polite_pause()
  html <- tryCatch(site_html(wrapper_url), error = function(e) NULL)
  if (is.null(html)) return(NA_character_)

  page_text <- html |> rvest::html_text()
  pdf_urls <- stringr::str_extract_all(
    page_text, "https?://[^\"'\\s]+\\.pdf[^\"'\\s]*"
  )[[1]]

  pdf_from_attrs <- html |>
    rvest::html_nodes("iframe, embed, object") |>
    rvest::html_attr("src") |>
    stringr::str_subset("\\.pdf")

  all_pdfs <- unique(c(pdf_urls, pdf_from_attrs))
  if (length(all_pdfs) == 0) return(NA_character_)
  all_pdfs[1]
}

# Steiner helper: a PDF's bytes, NULL if the download fails. Not cached on
# disk: a file kept from an earlier harvest would give last year's plans.
.steiner_download_pdf <- function(pdf_url) {
  message("  Downloading: ", basename(pdf_url))
  polite_pause()
  tryCatch(httr2::resp_body_raw(site_perform(site_request(pdf_url))),
           error = function(e) { message("  ", conditionMessage(e)); NULL })
}

# Steiner helper: extract emne sections from a PDF (its bytes or a path)
.steiner_extract_emner_from_pdf <- function(pdf, subject, wrapper_url) {
  pages <- if (is.null(pdf)) NULL else tryCatch(pdftools::pdf_text(pdf), error = function(e) NULL)
  if (is.null(pages)) return(tibble::tibble())

  emne_pattern <- paste0(
    "^\\s*",
    stringr::str_replace_all(subject, "\\s+", "\\\\s+"),
    "\\s+\\d+\\s*,\\s*emne\\s+\\d+"
  )

  page_is_emne_start <- purrr::map_lgl(pages, function(p) {
    first_line <- stringr::str_split(p, "\n")[[1]][1]
    stringr::str_detect(first_line, stringr::regex(emne_pattern, ignore_case = TRUE))
  })

  page_is_overview <- purrr::map_lgl(pages, function(p) {
    first_line <- stringr::str_split(p, "\n")[[1]][1]
    stringr::str_detect(first_line, stringr::regex("emne\\s+\\d+\\s*-\\s*\\d+", ignore_case = TRUE))
  })

  emne_start_pages <- which(page_is_emne_start & !page_is_overview)
  if (length(emne_start_pages) == 0) return(tibble::tibble())

  message("  ", subject, ": ", length(emne_start_pages), " emner at pages ",
          paste(emne_start_pages, collapse = ", "))

  purrr::map_dfr(seq_along(emne_start_pages), function(i) {
    start <- emne_start_pages[i]
    all_starts <- which(page_is_emne_start | page_is_overview)
    next_starts <- all_starts[all_starts > start]
    end <- if (length(next_starts) > 0) next_starts[1] - 1 else length(pages)

    emne_text <- paste(pages[start:end], collapse = "\n\n") |>
      stringr::str_remove_all("Side\\s+\\d+\\s+av\\s+\\d+") |>
      stringr::str_replace_all("(?m)^[ \\t]+|[ \\t]+$", "") |>
      stringr::str_replace_all("\\n{3,}", "\n\n") |>
      stringr::str_trim()
    first_line <- stringr::str_trim(stringr::str_split(pages[start], "\n")[[1]][1])

    tibble::tibble(
      pdf_emne_name = first_line,
      url = wrapper_url,
      extracted_text = emne_text
    )
  })
}

# Steiner helper: normalize emne name for joining
.steiner_normalize_emne_name <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace("^(.+?)\\s+(\\d+)\\s*,", "\\1\\2,") |>
    # DBH may add the title after the number ("emne 5. FoU-oppgave, ...")
    stringr::str_replace("(, emne \\d+)\\b.*$", "\\1")
}

# Steiner helper: the join keys of a PDF emne. The PDFs are those of the
# bachelor (Steinerpedagogisk lærerutdanning), whose DBH names match theirs;
# the 1-7 programme (M- codes) has "pedagogikk og elevkunnskap1, emne 4" and
# "emne 5" as M-PEL2 "pedagogikk og elevkunnskap2, emne 1" and "emne 2", so
# these two get both keys.
.steiner_remap_pel_emne <- function(name) {
  m <- stringr::str_match(name, "^pedagogikk og elevkunnskap1, emne ([45])$")[, 2]
  if (is.na(m)) return(name)
  c(name, paste0("pedagogikk og elevkunnskap2, emne ", as.integer(m) - 3L))
}

# --- Strategy: json_extract ---
# URL -> HTTP fetch -> JSON extraction from <script> tag
# Used by: nla

harvest_json_extract <- function(df, config) {
  # NLA has a page per course and academic year (/emneplan/{CODE}/2024-2025),
  # whose JSON holds the plan of that year only: each URL is fetched once
  # (fetch_html_with_checkpoint()) and its page joined to the offerings whose
  # URL it is (autumn 2024 and spring 2025). Harvests until 2026-10-17
  # fetched one page per course code and gave it to every year.
  df <- df |>
    dplyr::mutate(academic_year = nla_academic_year(year, semester_name))
  df <- fetch_html_with_checkpoint(df, checkpoint_dir(config$name, "html", df$year), config = config)

  # the year's plan from the JSON (the offering's academic year, from its course_id)
  df$extracted_text <- extract_fulltext_from_raw(df, config)
  df
}

# --- Strategy: noop ---
# Set extracted_text = NA (no harvestable course plan content)
# Used by: samas

harvest_noop <- function(df, config) {
  df$url <- NA_character_
  df$extracted_text <- NA_character_
  message("0/", nrow(df), " rows with extracted text (noop strategy)")
  df
}

# --- Strategy: cohort_plans ---
# A plan per programme cohort: HTML course pages of some cohorts, and the
# programme study plans as PDFs, each split by course heading; an offering
# takes the plan of the cohort that has the course in its semester.
# Used by: ldh

harvest_cohort_plans <- function(df, config) {
  index <- ldh_plan_index()
  m <- cohort_plan_match(df$course_code, semester_term(df$year, df$semester_name), index)
  df$url <- index$url[m]
  pdf <- !is.na(m) & !is.na(index$text[m])
  df <- fetch_html_with_checkpoint(dplyr::mutate(df, url = ifelse(pdf, NA_character_, url)),
                                   checkpoint_dir(config$name, "html", df$year), config = config)
  df$url[pdf] <- index$url[m[pdf]]
  df$extracted_text <- extract_fulltext_from_raw(df, config)
  df$extracted_text[pdf] <- clean_chars(index$text[m[pdf]])
  message(sum(!is.na(df$extracted_text)), "/", nrow(df), " rows with extracted text (", sum(pdf), " from PDFs)")
  pdfs <- attr(index, "pdfs")
  .keep_pdf_once(df, tibble::tibble(wrapper_url = names(pdfs), pdf = unname(pdfs)))
}

#' The plan of each offering among a cohort-plan index
#'
#' A cohort that starts in term `start` (semester_term()) has a course in its
#' semesters `k1` to `k2` of the plan, so in the terms start + k1 - 1 to
#' start + k2 - 1: an offering in term `term` takes the entry of its code
#' whose terms hold it, an HTML page before a PDF, then the latest cohort
#' (an entry without its semester, `k1` NA, holds every term from its start).
#' Else, as for a re-sit or a delayed student of an earlier cohort (DBH
#' registers BSY-301 of the plan of 2019-2023 up to 2025), the cohort that had
#' the course last before the offering's term. Codes are compared in upper
#' case without spaces.
#'
#' @param code,term The offerings' course codes and terms
#' @param index Tibble of code, start, k1, k2, url and text (NA for an HTML
#'   page), from ldh_plan_index()
#' @return Row of `index` for each offering, NA for none
cohort_plan_match <- function(code, term, index) {
  key <- function(x) gsub(" ", "", toupper(x), fixed = TRUE)
  first <- index$start + dplyr::coalesce(index$k1, 1L) - 1L
  last <- ifelse(is.na(index$k1), Inf, index$start + index$k2 - 1L)
  o <- order(!is.na(index$text), -index$start)
  vapply(seq_along(code), function(i) {
    same <- o[key(index$code[o]) == key(code[i]) & first[o] <= term[i]]
    hit <- same[term[i] <= last[same]]
    if (length(hit)) hit[1] else if (length(same)) same[order(-first[same])][1] else NA_integer_
  }, integer(1))
}

# LDH: the cohort-plan index (cohort_plan_match()). The bachelor in nursing's
# course pages of the cohorts on the site (/emnerBIS/emnerBIS25/bsy-321: BIS25
# starts in autumn 2025, and the code's first digit is the course's
# semester, as the page's "3. semester: høst 2026" says), from the sitemap;
# and the courses of every programme study plan (PDF) linked from the
# programme pages and the page of earlier plans (ldh_pdf_courses()). The PDFs'
# bytes are attribute `pdfs`, by URL. Checked 2026-11-20.
ldh_plan_index <- function(root = "https://ldh.no") {
  locs <- sitemap_locs(paste0(root, "/sitemap.xml"))
  m <- stringr::str_match(locs, "/emnerBIS/emnerBIS(\\d{2})/([a-z]+-\\d+[a-z]?)$")
  k <- as.integer(substr(stringr::str_extract(m[, 3], "\\d+"), 1, 1))
  html <- tibble::tibble(code = toupper(m[, 3]), start = semester_term(2000L + as.integer(m[, 2]), "Høst"),
                         k1 = k, k2 = k, url = locs, text = NA_character_)[!is.na(m[, 1]), ]
  pages <- c(grep(paste0("^", root, "/studietilbud/(masterstudier|videreutdanninger)/[^/]+$"), locs, value = TRUE),
             paste0(root, c("/studietilbud/bachelor-i-sykepleie",
                            "/student/semester-og-eksamensoversikt/tidligere-studieplaner")))
  links <- unique(unlist(lapply(pages, function(p) {
    polite_pause()
    a <- tryCatch(rvest::html_elements(site_html(p), "a[href]"), error = function(e) NULL)
    href <- xml2::url_absolute(rvest::html_attr(a, "href"), p)
    href[grepl("(?i)(studieplan|fagplan)", utils::URLdecode(href), perl = TRUE) &
           startsWith(href, root) & !href %in% pages]
  })))
  pdfs <- list()
  pdf <- dplyr::bind_rows(lapply(links, function(url) {
    polite_pause()
    bytes <- tryCatch(httr2::resp_body_raw(site_perform(site_request(url))), error = function(e) NULL)
    courses <- if (!is.null(bytes)) ldh_pdf_courses(bytes, url)
    if (!NROW(courses)) return(NULL)
    pdfs[[url]] <<- bytes
    courses
  }))
  message("LDH: ", nrow(html), " course pages, ", nrow(pdf), " courses in ", length(pdfs), " study plans")
  out <- dplyr::bind_rows(html, pdf)
  if (!nrow(out)) stop("no course plan found at ", root)
  structure(out, pdfs = pdfs)
}

# LDH: the courses of a programme study plan (a PDF's bytes, or its pages'
# text), each from its heading, a line that starts with the code ("MAPSY-100:
# Innføring i ...", "BSY-100 Sykepleie, ...") followed within six lines by
# "Engelsk emnenavn" (or "Engelsk navn"), to the next heading or a heading of
# the plan: in capitals ("STUDIETS OVERORDNEDE LÆRINGSUTBYTTER"), "Studiets
# overordnede læringsutbytter" (BIS23; not a course's own "Læringsutbytte"),
# "Referanser" or "Vedlegg" ("Vedlegg til studieplan – selvvalgte emner").
# Without the page chrome: the header ("Lovisenberg diakonale høgskole"), the
# footers ("Bachelor i sykepleie BIS23     38", "Master i klinisk
# intensivsykepleie H2025. Sist oppdatert 07.09.26") and a page number on a
# line of its own among a page's last three lines (not a number in a table;
# 2026-11-21, the pilot audit found BIS23's last course running into the
# national competence areas and page numbers left in). The cohort's first
# term is the title page's or the file name's "Kull H2024", "Kull V2025" or
# "kull 2019" (autumn), else "BIS21" (autumn 2021), else the file name's
# "H2023"; a plan without it is left out. The course's semesters are its
# "Semester: 5 og 6" or "Semester Første semester, høst". A code that a plan
# has twice keeps the first.
ldh_pdf_courses <- function(pdf, url) {
  pages <- if (is.character(pdf)) pdf else tryCatch(pdftools::pdf_text(pdf), error = function(e) character())
  none <- tibble::tibble(code = character(), start = integer(), k1 = integer(), k2 = integer(),
                         url = character(), text = character())
  if (!length(pages)) return(none)
  title <- paste(pages[1], utils::URLdecode(basename(url)))
  kull <- stringr::str_match(title, "(?i)\\bkull\\s+(H|V)?\\s?(\\d{4})")
  bis <- stringr::str_match(title, "\\bBIS(\\d{2})\\b")[, 2]
  name <- stringr::str_match(utils::URLdecode(basename(url)), "\\b(H|V)(\\d{4})\\b")
  start <- if (!is.na(kull[1, 1])) semester_term(kull[1, 3], if (toupper(kull[1, 2]) %in% "V") "Vår" else "Høst") else
    if (!is.na(bis)) semester_term(2000L + as.integer(bis), "Høst") else
    if (!is.na(name[1, 1])) semester_term(name[1, 3], c(H = "Høst", V = "Vår")[[name[1, 2]]]) else NA
  if (is.na(start)) return(none)

  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n"))
  page <- rep(seq_along(pages), stringr::str_count(pages, "\n") + 1L)[seq_along(lines)]
  full <- which(nzchar(trimws(lines)))
  last <- full[stats::ave(full, page[full], FUN = function(i) rev(seq_along(i))) <= 3]
  lines <- lines[!grepl("^\\s*Lovisenberg diakonale høgskole( AS)?\\s*$", lines) &
                   !grepl("^\\s{10,}\\S.*\\s{3,}\\d{1,3}\\s*$", lines) &
                   !grepl("^\\s*\\S.*\\bSist oppdatert \\d{2}\\.\\d{2}\\.\\d{2,4}(\\s+\\d{1,3})?\\s*$", lines) &
                   !(seq_along(lines) %in% last & grepl("^\\s*\\d{1,3}\\s*$", lines))]
  code_rx <- "^([A-ZÆØÅ]{2,8}-\\s?\\d{2,4}[A-Z]?)\\b:?\\s+\\S"
  head <- which(grepl(code_rx, lines) & vapply(seq_along(lines), function(i)
    any(grepl("Engelsk (emne)?navn", lines[i + seq_len(6)])), logical(1)))
  if (!length(head)) return(none)
  stop <- c(which(grepl("^(?:[A-ZÆØÅ][A-ZÆØÅ .,–-]{8,}|Studiets overordnede læringsutbytter|Referanser|Vedlegg( \\d+| til studieplan.*)?)\\s*$", lines, perl = TRUE)),
            length(lines) + 1L)
  end <- pmin(c(head[-1], length(lines) + 1L), stop[findInterval(head, stop) + 1L]) - 1L
  ord <- c(første = 1, andre = 2, tredje = 3, fjerde = 4, femte = 5, sjette = 6)
  out <- dplyr::bind_rows(lapply(seq_along(head), function(j) {
    sec <- lines[head[j]:end[j]]
    sem <- tolower(stringr::str_match(sec[seq_len(min(15, length(sec)))], "^\\s*Semester:?\\s+(.+)$")[, 2])
    sem <- sem[!is.na(sem)][1]
    k <- c(as.integer(stringr::str_extract_all(sem, "\\d")[[1]]), unname(ord[stringr::str_extract_all(sem, paste(names(ord), collapse = "|"))[[1]]]))
    text <- stringr::str_trim(stringr::str_replace_all(paste(sub("\\s+$", "", sec), collapse = "\n"), "\\n{3,}", "\n\n"))
    tibble::tibble(code = gsub(" ", "", stringr::str_match(sec[1], code_rx)[, 2]),
                   start = as.integer(start), k1 = if (length(k)) as.integer(min(k)) else NA_integer_,
                   k2 = if (length(k)) as.integer(max(k)) else NA_integer_, url = url, text = text)
  }))
  out[!duplicated(out$code), ]
}
