# R/extract_sections.R
# Structured section extraction from course HTML.
#
# Entry point: extract_sections(institution, html, extracted_text,
# course_id) — returns a long tibble (course_id, institution,
# section, raw_text).
#
# Strategies (config field `section_strategy`, see .section_strategy_fn()):
#   - html_headings  — DOM walk by heading (+ optional sub-headings)
#   - html_fields    — semantic div.field-<name> containers (hivolda)
#   - details_mf     — details/summary accordions + intro block (mf)
#   - details_uib    — details/summary accordions + h2 (uib)
#   - accordion_nord — div.ac trigger/panel pairs (nord)
#   - json_nla       — titles in the embedded EmneplanPage JSON (nla)
#   - text_split     — heading-shaped lines in extracted_text

# Section-extraction config lives on each institution in
# R/institution_config.R (fields: section_strategy, section_heading_level).
# Selector and pre_fn are reused from the institution's existing fields.
.section_cfg <- function(institution) {
  ic <- get_institution_config(institution)
  if (is.null(ic$section_strategy)) {
    stop("No section_strategy for institution: ", institution)
  }
  list(
    strategy            = ic$section_strategy,
    heading_level       = ic$section_heading_level,
    heading_selector    = ic$section_heading_selector,
    subheading_selector = ic$section_subheading_selector,
    intro_selector      = ic$section_intro_selector,
    fields              = ic$section_fields,
    inline_coursework   = isTRUE(ic$section_inline_coursework),
    # Container for section extraction; defaults to the fulltext selector.
    # A multi-element fulltext selector (selector_mode = "multi") cannot be
    # used here: html_element() would take only its first match.
    selector            = ic$section_selector %||% ic$selector,
    pre_fn           = ic$pre_fn,
    institution      = institution
  )
}

#' Extract sections from course HTML for one institution
#'
#' Vectorised over the row inputs. All rows are assumed to share
#' `institution`. Returns a long tibble:
#'   course_id <chr>, institution <chr>, section <chr>, raw_text <chr>
#'
#' @param institution Character scalar.
#' @param html Character vector of raw HTML.
#' @param extracted_text Character vector of pre-extracted plain text
#'   (used by text_split and by html_headings' text-split fallback).
#'   Same length as `html`.
#' @param course_id Character vector of course ids (same length as `html`).
extract_sections <- function(institution, html, extracted_text, course_id) {
  stopifnot(length(institution) == 1)
  stopifnot(length(html) == length(course_id))
  stopifnot(length(extracted_text) == length(html))

  cfg <- .section_cfg(institution)

  fn <- .section_strategy_fn(cfg$strategy)
  safe_fn <- purrr::possibly(fn, otherwise = .empty_sections())

  # html_headings falls back to text_split when it finds fewer than 3
  # sections (per issue #183 plan).
  use_fallback <- identical(cfg$strategy, "html_headings")
  fallback_fn <- if (use_fallback) {
    purrr::possibly(.section_strategy_fn("text_split"),
                    otherwise = .empty_sections())
  } else NULL

  rows <- purrr::pmap(list(html, extracted_text, course_id), .progress = institution, function(h, txt, cid) {
    input <- list(html = h, extracted_text = txt, course_id = cid,
                  institution = institution)
    out <- safe_fn(input, cfg)
    if (!is.null(fallback_fn) && nrow(out) < 3) {
      fb <- fallback_fn(input, cfg)
      if (nrow(fb) > nrow(out)) out <- fb
    }
    if (cfg$inline_coursework) out <- .split_inline_coursework(out)
    out <- .clean_sections(out, institution)
    tibble::tibble(
      course_id         = rep(cid, nrow(out)),
      institution       = rep(institution, nrow(out)),
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
    details_mf     = extract_sections_mf,
    html_fields    = extract_sections_fields,
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

  # Heading nodes are normally identified by tag (e.g. "h2"). Some
  # institutions mark section headings with a CSS class instead (e.g. inn's
  # `div.label`); `section_heading_selector` lets the same DOM-walk treat any
  # matching element as a section boundary. We precompute the heading node set
  # and identify membership by xml_path (stable per node).
  heading_sel <- cfg$heading_selector
  if (!is.null(heading_sel)) {
    heading_paths <- vapply(rvest::html_elements(container, heading_sel),
                            xml2::xml_path, character(1))
    is_heading <- function(node) xml2::xml_path(node) %in% heading_paths
    has_nested <- function(node) {
      length(rvest::html_elements(node, heading_sel)) > 0
    }
  } else {
    is_heading <- function(node) tolower(rvest::html_name(node)) == heading_level
    has_nested <- function(node) {
      length(rvest::html_elements(node, heading_level)) > 0
    }
  }

  # Sub-headings inside a section (uia's <p><em>Faget i praksis</em></p>,
  # uio's <h3>Obligatoriske forkunnskaper</h3> or <p>Obligatorisk aktivitet:</p>)
  # switch to their own section until the next heading of either kind.
  subs <- .subheading_sections(container, cfg$subheading_selector, is_heading)
  is_sub <- function(node) xml2::xml_path(node) %in% names(subs)
  has_nested_any <- function(node) {
    has_nested(node) ||
      any(startsWith(names(subs), paste0(xml2::xml_path(node), "/")))
  }

  sections <- list()
  state <- new.env()
  # Text before the first heading is dropped unless the caller names its
  # section (details_mf reads an untitled intro block as course_content).
  state$parent_section <- cfg$initial_section %||% NA_character_
  state$current_section <- state$parent_section
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
    if (is_heading(node)) {
      flush()
      state$parent_section <- match_heading_to_section(rvest::html_text2(node))
      state$current_section <- state$parent_section
      return(invisible())
    }

    if (is_sub(node)) {
      flush()
      sub <- subs[[xml2::xml_path(node)]]
      # An unmapped heading tag (e.g. <h3>Karakterskala</h3>) returns the
      # text that follows it to the enclosing section.
      state$current_section <- if (is.na(sub)) state$parent_section else sub
      return(invisible())
    }

    # Does this subtree contain any headings? If not, emit it as a leaf.
    if (!has_nested_any(node)) {
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
#'   - the first non-blank line, or preceded by a blank line
#'   - heading-shaped (see .heading_shaped_line(): capitalised, short, no
#'     digits, no "Label: value", no closing full stop) (#214)
#'   - a pattern match that starts at a word boundary
#'   - mapping to a section other than the one already open; a line that
#'     maps to the open section ("Kunnskap" inside learning outcomes) is
#'     kept as content
#'
#' Headings mapping to the same canonical section have their content
#' concatenated in document order (with a blank line between chunks),
#' matching html_headings' behaviour.
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
    if (prev_blank && .heading_shaped_line(trimmed)) {
      # Strip trailing punctuation like ":" that frequently follows
      # plain-text section labels.
      candidate <- stringr::str_remove(trimmed, "[:：]\\s*$")
      heading_match <- match_heading_to_section(candidate, word_start = TRUE)
    }

    if (!is.na(heading_match) && !identical(heading_match, current_section)) {
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

# A plain-text line that could be a section heading: starts with a capital
# letter, at most 8 words and 80 characters, no digits (course titles, "emne 2",
# reading-list entries), no "Label: value" colon, and no closing full stop
# (sentences). Learning-outcome bullets ("har kunnskap om ...") start in lower
# case and are rejected.
.heading_shaped_line <- function(line) {
  nchar(line) <= 80 &&
    grepl("^\\p{Lu}", line, perl = TRUE) &&
    !grepl("\\d", line) &&
    !grepl(":\\s*\\S", line) &&
    !grepl("\\.$", line) &&
    lengths(strsplit(line, "\\s+")) <= 8
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
  details <- .details_sections(doc)
  for (i in seq_len(nrow(details))) add(details$section[i], details$raw_text[i])

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

# details/summary accordion sections under `root`: one row per <details>
# whose <summary> maps to a section; body is the details text minus the
# summary. Shared by the details_uib and details_mf strategies.
.details_sections <- function(root) {
  out <- .empty_sections()
  for (d in rvest::html_elements(root, "details")) {
    summary <- rvest::html_element(d, "summary")
    if (is.na(summary)) next
    summary_text <- rvest::html_text2(summary)
    section <- match_heading_to_section(summary_text)
    if (is.na(section)) next
    body <- stringr::str_remove(rvest::html_text2(d), stringr::fixed(summary_text))
    out <- dplyr::bind_rows(out, tibble::tibble(section = section,
                                                raw_text = trimws(body)))
  }
  out
}

# Concatenate rows of the same section in order of first appearance.
.merge_sections <- function(out) {
  out <- out[!is.na(out$raw_text) & nzchar(out$raw_text), , drop = FALSE]
  if (nrow(out) == 0) return(.empty_sections())
  secs <- unique(out$section)
  tibble::tibble(
    section  = secs,
    raw_text = vapply(secs, function(x) paste(out$raw_text[out$section == x],
                                              collapse = "\n\n"),
                      character(1), USE.NAMES = FALSE)
  )
}

#' details_mf strategy — MF's WordPress course pages (#213)
#'
#' The plan is a set of `<details><summary>` accordions (Læringsutbytte,
#' Obligatoriske aktiviteter, Avsluttende vurdering/eksamen, Litteraturliste)
#' plus an untitled intro block above them that describes the course. The
#' intro is read as course_content; paragraph sub-headings inside it
#' ("Arbeidsform og organisering:", "Forkunnskaper:") switch section. The
#' contact card and the facts box sit outside both and are never read.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (`intro_selector`,
#'   `subheading_selector`).
extract_sections_mf <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())

  details <- .details_sections(rvest::read_html(html))
  intro <- extract_sections_html(
    input,
    utils::modifyList(cfg, list(selector = cfg$intro_selector,
                                initial_section = "course_content"))
  )
  .merge_sections(dplyr::bind_rows(intro, details))
}

#' html_fields strategy — pages built from semantic field containers (#214)
#'
#' hivolda (Drupal) renders each part of the plan as `div.field-<name>` with a
#' `div.label` heading, and the exam table sits in an unlabelled
#' `div.field-assessments-row`. `cfg$fields` maps field class names to
#' sections; fields not in the map (contact person, approval, evaluation,
#' student numbers) are never read. The label is dropped from the first field
#' of a section and kept on later ones, so learning-outcome groups keep their
#' "Kunnskapar"/"Ferdigheiter" prefix.
#'
#' @param input List with `html`, `extracted_text`, `course_id`, `institution`.
#' @param cfg Section-extraction config list (`selector`, `fields`, `pre_fn`).
extract_sections_fields <- function(input, cfg) {
  html <- input$html
  if (is.na(html) || !nzchar(html)) return(.empty_sections())
  if (!is.null(cfg$pre_fn)) html <- cfg$pre_fn(html)

  doc <- rvest::read_html(html)
  root <- if (is.null(cfg$selector)) doc else rvest::html_element(doc, cfg$selector)
  if (is.na(root)) return(.empty_sections())

  nodes <- rvest::html_elements(root, "div[class*='field-']")
  field <- stringr::str_extract(xml2::xml_attr(nodes, "class"),
                                "(?<![\\w-])field-[a-z-]+")
  section <- unname(cfg$fields[field])
  keep <- !is.na(section)
  nodes <- nodes[keep]
  section <- section[keep]
  if (length(nodes) == 0) return(.empty_sections())

  text <- character(length(nodes))
  for (i in seq_along(nodes)) {
    first_of_section <- !(section[i] %in% section[seq_len(i - 1)])
    if (first_of_section) {
      xml2::xml_remove(rvest::html_elements(nodes[[i]], "div.label"))
    }
    text[i] <- trimws(rvest::html_text2(nodes[[i]]))
  }
  .merge_sections(tibble::tibble(section = section, raw_text = text))
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

#' Collect raw heading-candidate texts for diagnostic purposes
#'
#' Returns the set of heading strings that a given strategy *would*
#' try to match against the pattern table — before any matching
#' occurs. Used by run_extract_sections.R to surface unmapped
#' heading texts that suggest pattern-table additions (#192).
#'
#' Returns character() for strategies that don't have a heading
#' concept (text_split, json_nla, noop).
.collect_heading_candidates <- function(html, strategy, institution_config) {
  if (is.na(html) || !nzchar(html)) return(character())
  doc <- rvest::read_html(html)

  if (strategy == "html_headings") {
    # Mirror extract_sections_html: prefer the class-based heading selector
    # when configured (e.g. inn's div.label), else the heading tag.
    heading_sel <- institution_config$section_heading_selector %||%
      (institution_config$section_heading_level %||% "h2")
    container <- rvest::html_element(
      doc, institution_config$section_selector %||% institution_config$selector)
    if (is.na(container)) return(character())
    nodes <- rvest::html_elements(container, heading_sel)
    return(vapply(nodes, rvest::html_text2, character(1)))
  }

  if (strategy == "accordion_nord") {
    triggers <- rvest::html_elements(doc, "button.ac-trigger")
    out <- vapply(triggers, function(t) {
      copy_span <- rvest::html_element(t, ".copy-accordion-anchor")
      if (!is.na(copy_span)) xml2::xml_remove(copy_span)
      trimws(rvest::html_text2(t))
    }, character(1))
    return(out)
  }

  if (strategy == "details_uib") {
    summaries <- rvest::html_elements(doc, "details > summary")
    h2s <- rvest::html_elements(doc, "h2")
    return(c(vapply(summaries, rvest::html_text2, character(1)),
             vapply(h2s, rvest::html_text2, character(1))))
  }

  character()
}

#' Find sub-heading nodes for html_headings
#'
#' Returns a named character vector: names are xml_paths of sub-heading nodes,
#' values their section (NA = an unmapped heading tag, which hands the text
#' that follows back to the enclosing section). Heading tags (h3-h6) match like
#' main headings. Other elements (<p>) count only when their whole text, minus
#' a trailing colon, equals a heading pattern, so ordinary sentences and
#' list-like paragraphs never split a section.
.subheading_sections <- function(container, selector, is_heading) {
  none <- stats::setNames(character(), character())
  if (is.null(selector)) return(none)
  nodes <- rvest::html_elements(container, selector)
  nodes <- nodes[!vapply(nodes, is_heading, logical(1))]
  if (length(nodes) == 0) return(none)

  text <- stringr::str_remove(stringr::str_squish(rvest::html_text2(nodes)),
                              "[:：]$")
  is_htag <- grepl("^h[1-6]$", tolower(xml2::xml_name(nodes)))
  in_list <- !vapply(xml2::xml_find_first(nodes, "ancestor::li"),
                     inherits, logical(1), what = "xml_missing")

  section <- section_heading_patterns$section[match(tolower(text),
                                                    section_heading_patterns$pattern)]
  section[is_htag] <- vapply(text[is_htag], match_heading_to_section, character(1))
  keep <- nzchar(text) & !in_list & (is_htag | (!is.na(section) & nchar(text) <= 80))
  stats::setNames(section[keep], vapply(nodes[keep], xml2::xml_path, character(1)))
}

.empty_sections <- function() {
  tibble::tibble(section = character(), raw_text = character())
}

#' Post-process a strategy's (section, raw_text) output for one course.
#'
#' Applies cheap, high-precision cleanup shared across all strategies (#198):
#'   - removes ".drop" rows (admission and exam-logistics headings),
#'   - strips trailing FS page-generation timestamps,
#'   - drops a leading line that merely repeats the section's own heading,
#'   - removes rows that are empty or only a placeholder ("Ingen",
#'     "Se fagplanen.", "-", a Leganto pointer; see .placeholder_phrases).
#'   - strips known notices and page widgets from assessment and coursework
#'     (.section_noise; #215).
#' Fuzzier cleanup (exam tables, arbeidskrav/eksamen boundaries inside one
#' paragraph) is left to the later Phase-2 LLM lifting.
.clean_sections <- function(out, institution = NULL) {
  # ".drop" collects text under headings that end a section but are not
  # course content (admission, exam logistics; see section_heading_map.R).
  out <- out[out$section != ".drop", , drop = FALSE]
  if (nrow(out) == 0) return(out)
  exam <- out$section %in% c("assessment", "coursework_requirements")
  out$raw_text[exam] <- .strip_section_noise(out$raw_text[exam], institution)
  out$raw_text <- mapply(.clean_section_text, out$raw_text, out$section,
                         USE.NAMES = FALSE)
  keep <- mapply(.keep_section_row, out$raw_text, out$section,
                 USE.NAMES = FALSE)
  out[keep, , drop = FALSE]
}

# Lines in an assessment block that state a coursework gate (#212). nord
# writes them as "Arbeidskrav (AK): ..." or "Obligatorisk deltakelse (OD): ..."
# inside one undifferentiated vurdering block, with no heading to split on.
.inline_coursework_regex <- paste0(
  "^\\s*(?:Arbeidskrav(?:ene|et)?|",
  "Obligatoriske? (?:deltakelse|deltagelse|deltaking|arbeid|arbeidskrav|",
  "aktivitet(?:er)?|oppmøte|frammøte|fremmøte)|",
  "Deltakelse i undervisning(?:en)? er obligatorisk)\\b"
)

# Move coursework-gate lines from assessment into coursework_requirements.
.split_inline_coursework <- function(out) {
  i <- which(out$section == "assessment")
  if (length(i) != 1) return(out)
  lines <- stringr::str_split_1(out$raw_text[i], "\n")
  gate <- grepl(.inline_coursework_regex, lines, perl = TRUE)
  if (!any(gate)) return(out)
  out$raw_text[i] <- paste(lines[!gate], collapse = "\n")
  moved <- paste(lines[gate], collapse = "\n")
  j <- which(out$section == "coursework_requirements")
  if (length(j) == 1) {
    out$raw_text[j] <- paste(out$raw_text[j], moved, sep = "\n\n")
  } else {
    out <- dplyr::bind_rows(out, tibble::tibble(section = "coursework_requirements",
                                                raw_text = moved))
  }
  out
}

# Notices and page widgets with no heading of their own, so .drop cannot catch
# them (#215). Applied to assessment and coursework_requirements only, so a
# course that teaches about AI keeps "kunstig intelligens" in its content.
# "all" applies to every institution; other entries per institution. Each
# pattern removes whole lines (or, with (?s), a trailing block).
.section_noise <- list(
  all = c(
    "(?m)^.*(?:plagiatkontroll|for plagiat).*$",                 # nih
    "(?m)^.*(?:ChatGPT|kunstig intelligens|artificial intelligence).*$", # nord
    "(?m)^.*(?:[Cc]ovid-19|[Kk]orona).*$",                       # nord
    "(?m)^.*[Mm]idlertidig forskrift.*$",
    "(?m)^.*endres vurderingsform.*$",
    "(?m)^MERK: (?:Våren|Høsten) \\d{4}.*$",
    "(?m)^Se under [^.\n]{0,40}\\.?$"                            # oslomet pointer
  ),
  uib = c(
    "(?s)\n?Dette bør du vite om eksamen.*$",                 # footer
    "(?m)^Vi opplever problemer med å hente inn eksamensinformasjon.*\n(?:Lukk\n.*)?",
    "(?m)^Til toppen$"
  ),
  hvl  = "(?m)^Me(?:i)?r om hjelpemidd?el(?:er)?[ \t]*$",          # link text
  # flattened exam table ("Muntlig eksamen Karakterregel: ... Hjelpemiddelkode:
  # ..."); a line with a sentence before the table keeps its text
  nmbu = "(?m)^[^.\n]{0,60}Karakterregel:.*$"
)

.strip_section_noise <- function(text, institution = NULL) {
  rx <- c(.section_noise$all, if (!is.null(institution)) .section_noise[[institution]])
  for (r in rx) text <- stringr::str_remove_all(text, r)
  stringr::str_replace_all(text, "\n{3,}", "\n\n")
}

.clean_section_text <- function(text, section) {
  if (is.na(text)) return(text)
  # Trailing FS (Felles studentsystem) page-generation timestamp is admin
  # boilerplate appended at harvest time, never section content.
  text <- stringr::str_remove(
    text, "(?s)\\s*Sist hentet fra FS \\(Felles studentsystem\\).*$")
  # A leading line that merely repeats this section's own heading is noise
  # (e.g. uia "Læringsutbytte" echoed as the first body line). Use an EXACT
  # match against the heading patterns (not the substring matcher) so we never
  # delete real one-line content that merely contains a section keyword
  # (e.g. "Ingen pensumliste tilgjengelig" or "Vurdering skjer ved eksamen").
  lines <- stringr::str_split_1(text, "\\r?\\n")
  if (length(lines) >= 1) {
    norm_first <- tolower(trimws(stringr::str_remove(lines[1], "[:：]\\s*$")))
    eq <- section_heading_patterns$pattern == norm_first
    if (any(eq) && identical(section_heading_patterns$section[which(eq)[1]], section)) {
      text <- paste(lines[-1], collapse = "\n")
    }
  }
  trimws(text)
}

.keep_section_row <- function(text, section) {
  if (is.na(text) || !nzchar(trimws(text))) return(FALSE)
  !.is_placeholder_text(text)
}

# Phrases that say a section is absent or lives elsewhere (#210). A row is a
# placeholder only when its WHOLE text is made of these phrases, so real
# content that merely starts with "Ingen" or mentions Leganto is kept.
.placeholder_phrases <- c(
  # bare negation / dummy values: "Ingen", "Ingen krav", "None", "x", "-", "..."
  "ingen(?: spesielle)?(?: krav| forkunnskapskrav)?", "none", "n/a", "x",
  "[-–.…]+",
  # pointers to another document
  "(?:se|sjå) (?:fag|program|studie)?plan(?:en)?",
  "se emnearkivet",
  "ingen emner i programmet",
  "emnebeskrivelsen finnes kun på engelsk[^.]*",
  # reading-list pointers and absence notes
  "ingen pensumliste(?: tilgjengelig)?(?: for dette emnet)?",
  "no reading list(?: available)?(?: for this course)?",
  "(?:gjeldende )?litteraturliste for [^.]{0,20} finner du i leganto",
  "litteratur og faglige ressurser finner du her",
  "pensumlist[ea] for emnet finn(?:er)? du her"
)
.placeholder_regex <- paste0(
  "^(?:(?:", paste(.placeholder_phrases, collapse = "|"), ")[\\s.:;,]*)+$"
)

.is_placeholder_text <- function(text) {
  grepl(.placeholder_regex, stringr::str_squish(tolower(text)), perl = TRUE)
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

