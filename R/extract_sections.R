# R/extract_sections.R
# Structured section extraction from course HTML.
#
# Entry point: extract_sections(institution_short, html, course_id) — returns
# a long tibble (course_id, institution_short, section, raw_text).
#
# Issue #185 scope: dispatcher + html_headings strategy. Other strategies
# (text_split, accordion_nord, details_uib, json_nla) are stubs here; they
# will be implemented in issues #186-#189.

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
#' Vectorised over `html` / `course_id`. All rows are assumed to share
#' `institution_short`. Returns a long tibble:
#'   course_id <chr>, institution_short <chr>, section <chr>, raw_text <chr>
#'
#' @param institution_short Character scalar.
#' @param html Character vector of raw HTML.
#' @param course_id Character vector of course ids (same length as `html`).
extract_sections <- function(institution_short, html, course_id) {
  stopifnot(length(institution_short) == 1)
  stopifnot(length(html) == length(course_id))

  cfg <- .section_strategies[[institution_short]]
  if (is.null(cfg)) stop("No section extraction strategy for: ", institution_short)
  cfg$institution <- institution_short

  fn <- .section_strategy_fn(cfg$strategy)
  safe_fn <- purrr::possibly(fn, otherwise = .empty_sections())

  # html_headings falls back to text_split when it finds fewer than 3
  # sections (per issue #183 plan). text_split itself is a stub until
  # issue #186; the fallback plumbing is in place for when it lands.
  use_fallback <- identical(cfg$strategy, "html_headings")
  fallback_fn <- if (use_fallback) {
    purrr::possibly(.section_strategy_fn("text_split"),
                    otherwise = .empty_sections())
  } else NULL

  rows <- purrr::map2(html, course_id, function(h, cid) {
    out <- if (is.na(h) || !nzchar(h)) .empty_sections() else safe_fn(h, cfg)
    if (!is.null(fallback_fn) && nrow(out) < 3) {
      fb <- fallback_fn(h, cfg)
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
    text_split     = .extract_sections_stub("text_split", "#186"),
    accordion_nord = .extract_sections_stub("accordion_nord", "#187"),
    details_uib    = .extract_sections_stub("details_uib", "#188"),
    json_nla       = .extract_sections_stub("json_nla", "#189"),
    noop           = function(html, cfg) .empty_sections(),
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
extract_sections_html <- function(html, cfg) {
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

.empty_sections <- function() {
  tibble::tibble(section = character(), raw_text = character())
}

# Strategy stub — returns empty for unimplemented strategies so the
# dispatcher is safe to call across all institutions now.
.extract_sections_stub <- function(strategy_name, issue_ref) {
  warned <- FALSE
  function(html, cfg) {
    if (!warned) {
      message(sprintf("[extract_sections] strategy '%s' not yet implemented (%s)",
                      strategy_name, issue_ref))
      warned <<- TRUE
    }
    .empty_sections()
  }
}

