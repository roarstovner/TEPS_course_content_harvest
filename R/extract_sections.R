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

# Strategy assignment (explicit mapping; will migrate to institution_config
# in issue #190). Each entry: strategy name + optional heading level +
# optional selector override. Selectors default to the institution's
# config selector.
.section_strategies <- list(
  oslomet = list(strategy = "html_headings", heading_level = "h2"),
  ntnu    = list(strategy = "html_headings", heading_level = "h3"),
  uio     = list(strategy = "html_headings", heading_level = "h2"),
  hiof    = list(strategy = "html_headings", heading_level = "h2"),
  hvl     = list(strategy = "html_headings", heading_level = "h3"),
  uia     = list(strategy = "html_headings", heading_level = "h2"),
  nih     = list(strategy = "html_headings", heading_level = "h2"),
  mf      = list(strategy = "html_headings", heading_level = "h2"),
  nmbu    = list(strategy = "html_headings", heading_level = "h3"),
  uit     = list(strategy = "html_headings", heading_level = "h2"),
  inn     = list(strategy = "html_headings", heading_level = "h2",
                 pre_fn = .add_table_cell_breaks, text_fallback = TRUE),
  uis     = list(strategy = "html_headings", heading_level = "h2"),

  hivolda = list(strategy = "text_split"),
  usn     = list(strategy = "text_split"),
  steiner = list(strategy = "text_split"),

  nord    = list(strategy = "accordion_nord"),
  uib     = list(strategy = "details_uib", heading_level = "h2"),
  nla     = list(strategy = "json_nla"),

  samas   = list(strategy = "noop")
)

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

  cfg <- .section_strategies[[institution_short]]
  if (is.null(cfg)) stop("No section extraction strategy for: ", institution_short)
  cfg$institution <- institution_short

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
    out <- safe_fn(h, txt, cfg)
    if (!is.null(fallback_fn) && nrow(out) < 3) {
      fb <- fallback_fn(h, txt, cfg)
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
    accordion_nord = .extract_sections_stub("accordion_nord", "#187"),
    details_uib    = .extract_sections_stub("details_uib", "#188"),
    json_nla       = .extract_sections_stub("json_nla", "#189"),
    noop           = function(html, txt, cfg) .empty_sections(),
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
#' @param html Character scalar of raw HTML.
#' @param cfg Section-extraction config list (must include `heading_level`
#'   and `selector`).
extract_sections_html <- function(html, extracted_text, cfg) {
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  heading_level <- cfg$heading_level %||% "h2"
  selector <- cfg$selector %||% get_institution_config(cfg$institution)$selector

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
#' @param html Ignored — included for strategy-function signature parity.
#' @param extracted_text Character scalar of pre-extracted plain text.
#' @param cfg Section-extraction config list (unused for now).
extract_sections_text <- function(html, extracted_text, cfg) {
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

.empty_sections <- function() {
  tibble::tibble(section = character(), raw_text = character())
}

# Strategy stub — returns empty for unimplemented strategies so the
# dispatcher is safe to call across all institutions now.
.extract_sections_stub <- function(strategy_name, issue_ref) {
  warned <- FALSE
  function(html, extracted_text, cfg) {
    if (!warned) {
      message(sprintf("[extract_sections] strategy '%s' not yet implemented (%s)",
                      strategy_name, issue_ref))
      warned <<- TRUE
    }
    .empty_sections()
  }
}

