# R/extract_sections.R
# Structured section extraction from course HTML.
#
# Entry point: extract_sections(institution_short, html, extracted_text,
# course_id) — returns a long tibble (course_id, institution_short,
# section, raw_text).
#
# Strategies:
#   - html_headings  (#185) — DOM-walk by heading level
#   - text_split     (#186) — line-based splitter on extracted_text
# Remaining strategies (accordion_nord, details_uib, json_nla) are stubs
# until #187–#189.

# Section-extraction config lives on each institution in
# R/institution_config.R (fields: section_strategy, section_heading_level).
# Selector and pre_fn are reused from the institution's existing fields.
.section_cfg <- function(institution_short) {
  ic <- get_institution_config(institution_short)
  if (is.null(ic$section_strategy)) {
    stop("No section_strategy for institution: ", institution_short)
  }
  list(
    strategy      = ic$section_strategy,
    heading_level = ic$section_heading_level,
    selector      = ic$selector,
    pre_fn        = ic$pre_fn,
    institution   = institution_short
  )
}

#' Extract sections from course HTML for one institution
#'
#' Vectorised over the row inputs. All rows are assumed to share
#' `institution_short`. Returns a long tibble:
#'   course_id <chr>, institution_short <chr>, section <chr>, raw_text <chr>
#'
#' @param institution_short Character scalar.
#' @param html Character vector of raw HTML.
#' @param extracted_text Character vector of pre-extracted plain text
#'   (used by text_split and by html_headings' text-split fallback).
#'   Same length as `html`.
#' @param course_id Character vector of course ids (same length as `html`).
extract_sections <- function(institution_short, html, extracted_text, course_id) {
  stopifnot(length(institution_short) == 1)
  stopifnot(length(html) == length(course_id))
  stopifnot(length(extracted_text) == length(html))

  cfg <- .section_cfg(institution_short)

  fn <- .section_strategy_fn(cfg$strategy)
  safe_fn <- purrr::possibly(fn, otherwise = .empty_sections())

  # html_headings falls back to text_split when it finds fewer than 3
  # sections (per issue #183 plan).
  use_fallback <- identical(cfg$strategy, "html_headings")
  fallback_fn <- if (use_fallback) {
    purrr::possibly(.section_strategy_fn("text_split"),
                    otherwise = .empty_sections())
  } else NULL

  rows <- purrr::pmap(list(html, extracted_text, course_id), function(h, txt, cid) {
    input <- list(html = h, extracted_text = txt, course_id = cid,
                  institution = institution_short)
    out <- safe_fn(input, cfg)
    if (!is.null(fallback_fn) && nrow(out) < 3) {
      fb <- fallback_fn(input, cfg)
      if (nrow(fb) > nrow(out)) out <- fb
    }
    tibble::tibble(
      course_id         = rep(cid, nrow(out)),
      institution_short = rep(institution_short, nrow(out)),
      section           = out$section,
      raw_text          = out$raw_text
    )
  })

  dplyr::bind_rows(rows)
}

.section_strategy_fn <- function(strategy) {
  switch(
    strategy,
    html_headings  = extract_sections_html,
    text_split     = extract_sections_text,
    accordion_nord = extract_sections_nord,
    details_uib    = extract_sections_uib,
    json_nla       = extract_sections_nla,
    noop           = function(input, cfg) .empty_sections(),
    .extract_sections_stub(strategy, "unknown")
  )
}

#' html_headings strategy — walk the DOM under the container in document
#' order, bucketing content by heading matches against the section map.
#'
#' Returns a tibble with columns: section, raw_text. One row per matched
#' canonical section; content from multiple headings mapping to the same
#' section is concatenated in document order.
#'
#' Algorithm:
#'   1. Do a DFS over the container subtree.
#'   2. When a heading node at the target level is visited, close out
#'      the current section (flush accumulated text) and start a new one
#'      keyed by the canonical match of the heading text.
#'   3. When visiting a non-heading element that contains no heading
#'      descendants at the target level, emit its `html_text2()` into
#'      the current section and do not recurse (the rendered text
#'      already captures all descendants).
#'   4. Otherwise recurse into children.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (must include `heading_level`
#'   and `selector`).
extract_sections_html <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  heading_level <- cfg$heading_level %||% "h2"
  selector <- cfg$selector

  if (!is.null(cfg$pre_fn)) html <- cfg$pre_fn(html)

  doc <- rvest::read_html(html)

  container <- if (!is.null(selector)) {
    node <- rvest::html_element(doc, selector)
    if (is.na(node)) return(.empty_sections())
    node
  } else {
    doc
  }

  sections <- list()
  state <- new.env()
  state$current_section <- NA_character_
  state$chunks <- character()

  flush <- function() {
    if (!is.na(state$current_section) && length(state$chunks) > 0) {
      txt <- paste(state$chunks[nzchar(state$chunks)], collapse = "\n")
      if (nzchar(txt)) {
        prior <- sections[[state$current_section]]
        sections[[state$current_section]] <<- c(prior, txt)
      }
    }
    state$chunks <- character()
  }

  visit <- function(node) {
    tag <- tolower(rvest::html_name(node))

    if (tag == heading_level) {
      flush()
      state$current_section <- match_heading_to_section(rvest::html_text2(node))
      return(invisible())
    }

    # Does this subtree contain any headings at the target level?
    nested <- rvest::html_elements(node, heading_level)
    if (length(nested) == 0) {
      if (!is.na(state$current_section)) {
        state$chunks <- c(state$chunks, rvest::html_text2(node))
      }
      return(invisible())
    }

    # Recurse into children to find the headings in order.
    for (k in rvest::html_children(node)) visit(k)
  }

  for (child in rvest::html_children(container)) visit(child)
  flush()

  if (length(sections) == 0) return(.empty_sections())

  tibble::tibble(
    section  = names(sections),
    raw_text = vapply(sections, function(v) paste(v, collapse = "\n\n"),
                      character(1))
  )
}

#' text_split strategy — line-based heading splitter on extracted text
#'
#' Splits `extracted_text` by lines that look like section headings:
#'   - short (<= 80 chars after trim)
#'   - trims to a match in the heading pattern table
#'   - either the first non-blank line, or preceded by a blank line
#'
#' Consecutive headings mapping to the same canonical section have
#' their content concatenated in document order (with a blank line
#' between chunks), matching html_headings' behaviour.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (unused for now).
extract_sections_text <- function(input, cfg) {
  extracted_text <- input$extracted_text
  if (is.na(extracted_text) || !nzchar(extracted_text)) return(.empty_sections())

  lines <- stringr::str_split_1(extracted_text, "\\r?\\n")
  n <- length(lines)
  if (n == 0) return(.empty_sections())

  is_blank <- !nzchar(trimws(lines))

  sections <- list()
  current_section <- NA_character_
  chunks <- character()

  flush <- function() {
    if (!is.na(current_section) && length(chunks) > 0) {
      txt <- paste(chunks[nzchar(chunks)], collapse = "\n")
      txt <- trimws(txt)
      if (nzchar(txt)) {
        prior <- sections[[current_section]]
        sections[[current_section]] <<- c(prior, txt)
      }
    }
  }

  prev_blank <- TRUE  # treat start-of-text like a preceding blank line
  for (i in seq_len(n)) {
    line <- lines[i]
    if (is_blank[i]) {
      if (!is.na(current_section)) chunks <- c(chunks, "")
      prev_blank <- TRUE
      next
    }

    trimmed <- trimws(line)
    heading_match <- NA_character_
    if (prev_blank && nchar(trimmed) <= 80) {
      # Strip trailing punctuation like ":" that frequently follows
      # plain-text section labels.
      candidate <- stringr::str_remove(trimmed, "[:：]\\s*$")
      heading_match <- match_heading_to_section(candidate)
    }

    if (!is.na(heading_match)) {
      flush()
      chunks <- character()
      current_section <- heading_match
    } else if (!is.na(current_section)) {
      chunks <- c(chunks, line)
    }
    prev_blank <- FALSE
  }
  flush()

  if (length(sections) == 0) return(.empty_sections())

  tibble::tibble(
    section  = names(sections),
    raw_text = vapply(sections, function(v) paste(v, collapse = "\n\n"),
                      character(1))
  )
}

#' accordion_nord strategy — Nord's accordion-based course pages
#'
#' Each section lives in a `div.ac` block containing `button.ac-trigger`
#' (the section heading) and `div.ac-panel > .ac-panel--inner` (the body).
#' The trigger button also contains a nested "Kopier lenke / Kopiert"
#' copy-link label which is stripped before matching.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (unused for now).
extract_sections_nord <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  doc <- rvest::read_html(html)
  items <- rvest::html_elements(doc, "div.ac")
  if (length(items) == 0) return(.empty_sections())

  sections <- list()

  for (item in items) {
    trigger <- rvest::html_element(item, "button.ac-trigger")
    panel <- rvest::html_element(item, ".ac-panel--inner")
    if (is.na(trigger) || is.na(panel)) next

    # Strip nested copy-link label from the trigger text.
    copy_span <- rvest::html_element(trigger, ".copy-accordion-anchor")
    if (!is.na(copy_span)) xml2::xml_remove(copy_span)
    heading_text <- trimws(rvest::html_text2(trigger))

    section <- match_heading_to_section(heading_text)
    if (is.na(section)) next

    body <- trimws(rvest::html_text2(panel))
    if (!nzchar(body)) next

    sections[[section]] <- c(sections[[section]], body)
  }

  if (length(sections) == 0) return(.empty_sections())

  tibble::tibble(
    section  = names(sections),
    raw_text = vapply(sections, function(v) paste(v, collapse = "\n\n"),
                      character(1))
  )
}

#' details_uib strategy — UiB hybrid details/summary + h2 sections
#'
#' UiB course pages use two parallel structures:
#'   - `<details><summary>Heading</summary>...body...</details>` for
#'     accordion-style sections (e.g. Krav til forkunnskaper, Vurderingsformer,
#'     Litteraturliste).
#'   - Top-level `<h2>` headings for the main content blocks (Mål og
#'     innhold, Læringsutbytte).
#'
#' We run both passes and concatenate matches into the canonical section
#' buckets. The h2 pass skips any `<details>` subtrees so their content
#' isn't double-counted.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (unused for now).
extract_sections_uib <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  doc <- rvest::read_html(html)
  sections <- list()
  add <- function(section, text) {
    text <- trimws(text)
    if (is.na(section) || !nzchar(text)) return(invisible())
    sections[[section]] <<- c(sections[[section]], text)
  }

  # Pass 1: details/summary accordion sections.
  for (d in rvest::html_elements(doc, "details")) {
    summary <- rvest::html_element(d, "summary")
    if (is.na(summary)) next
    section <- match_heading_to_section(rvest::html_text2(summary))
    if (is.na(section)) next
    # Body = details text minus the summary text.
    summary_text <- rvest::html_text2(summary)
    body <- rvest::html_text2(d)
    body <- stringr::str_remove(body, stringr::fixed(summary_text))
    add(section, body)
  }

  # Pass 2: top-level h2 sections. DFS that skips <details> subtrees.
  current_section <- NA_character_
  chunks <- character()
  flush <- function() {
    add(current_section, paste(chunks[nzchar(chunks)], collapse = "\n"))
    chunks <<- character()
  }

  visit <- function(node) {
    tag <- tolower(rvest::html_name(node))
    if (tag == "details") return(invisible())
    if (tag == "h2") {
      flush()
      current_section <<- match_heading_to_section(rvest::html_text2(node))
      return(invisible())
    }
    # If this subtree contains no h2 and no <details>, emit as leaf.
    if (length(rvest::html_elements(node, "h2, details")) == 0) {
      if (!is.na(current_section)) {
        chunks <<- c(chunks, rvest::html_text2(node))
      }
      return(invisible())
    }
    for (k in rvest::html_children(node)) visit(k)
  }

  body_root <- rvest::html_element(doc, "body")
  if (is.na(body_root)) body_root <- doc
  for (k in rvest::html_children(body_root)) visit(k)
  flush()

  if (length(sections) == 0) return(.empty_sections())

  tibble::tibble(
    section  = names(sections),
    raw_text = vapply(sections, function(v) paste(v, collapse = "\n\n"),
                      character(1))
  )
}

#' json_nla strategy — NLA embeds course data as JSON in a script tag
#'
#' Reuses the JSON discovery logic from `.extract_nla_json_one()`
#' (R/extract_fulltext.R): locate the EmneplanPage script block, parse
#' the JSON, index into `props$items[[academic_year]]`, then map the
#' per-item `title` fields (from `table` and `accordions`) to canonical
#' sections via the heading map.
#'
#' The academic year is derived from `course_id` (format
#' `nla_CODE_YEAR_SEMESTER_STATUS`): autumn → "YEAR-(YEAR+1)",
#' spring → "(YEAR-1)-YEAR".
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (unused for now).
extract_sections_nla <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  academic_year <- .nla_academic_year_from_course_id(input$course_id)
  if (is.na(academic_year)) return(.empty_sections())

  doc <- rvest::read_html(html)
  scripts <- rvest::html_elements(doc, "script")
  script_texts <- rvest::html_text(scripts)
  idx <- grep("EmneplanPage", script_texts, fixed = TRUE)
  if (length(idx) == 0) return(.empty_sections())

  json_match <- regmatches(script_texts[idx[1]],
                           regexpr("\\{.*\\}", script_texts[idx[1]]))
  if (length(json_match) == 0) return(.empty_sections())

  parsed <- jsonlite::fromJSON(json_match, simplifyVector = FALSE)
  year_data <- parsed$props$items[[academic_year]]
  if (is.null(year_data)) return(.empty_sections())

  sections <- list()
  add <- function(section, text) {
    text <- trimws(text %||% "")
    if (is.na(section) || !nzchar(text)) return(invisible())
    sections[[section]] <<- c(sections[[section]], text)
  }

  # table items: plain title + content
  for (item in year_data$table %||% list()) {
    section <- match_heading_to_section(item$title %||% NA_character_)
    add(section, item$content)
  }

  # accordion items: title + html-rendered content
  for (item in year_data$accordions %||% list()) {
    section <- match_heading_to_section(item$title %||% NA_character_)
    if (is.na(section)) next
    body <- item$content %||% ""
    if (nzchar(body)) {
      body_doc <- rvest::read_html(paste0("<div>", body, "</div>"))
      body <- rvest::html_text2(rvest::html_element(body_doc, "div"))
    }
    add(section, body)
  }

  if (length(sections) == 0) return(.empty_sections())

  tibble::tibble(
    section  = names(sections),
    raw_text = vapply(sections, function(v) paste(v, collapse = "\n\n"),
                      character(1))
  )
}

# Parse course_id like "nla_CODE_2024_spring_1" into the academic-year
# key used in NLA's JSON ("2023-2024" for spring 2024, "2024-2025" for
# autumn 2024).
.nla_academic_year_from_course_id <- function(course_id) {
  if (is.na(course_id)) return(NA_character_)
  # course_id format: {inst}_{code}_{year}_{semester}_{status}
  m <- stringr::str_match(course_id, "_(\\d{4})_(spring|autumn|summer)_\\d+$")
  if (is.na(m[1, 1])) return(NA_character_)
  year <- as.integer(m[1, 2])
  semester <- m[1, 3]
  if (semester == "autumn") paste0(year, "-", year + 1)
  else if (semester == "spring") paste0(year - 1, "-", year)
  else NA_character_
}

.empty_sections <- function() {
  tibble::tibble(section = character(), raw_text = character())
}

# Strategy stub — returns empty for unimplemented strategies so the
# dispatcher is safe to call across all institutions now.
.extract_sections_stub <- function(strategy_name, issue_ref) {
  warned <- FALSE
  function(input, cfg) {
    if (!warned) {
      message(sprintf("[extract_sections] strategy '%s' not yet implemented (%s)",
                      strategy_name, issue_ref))
      warned <<- TRUE
    }
    .empty_sections()
  }
}

