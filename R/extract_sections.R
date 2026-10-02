# R/extract_sections.R
# Structured section extraction from course HTML.
#
# Entry point: extract_sections(config, html, extracted_text, course_id)
# — returns a long tibble (course_id, institution, section, raw_text).
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
# `ic` is passed in rather than looked up, so that in the {targets} pipeline
# only the changed institution's sections are rebuilt (#264).
.section_cfg <- function(ic) {
  institution <- ic$name
  if (is.null(ic$section_strategy)) {
    stop("No section_strategy for institution: ", institution)
  }
  list(
    strategy            = ic$section_strategy,
    heading_level       = ic$section_heading_level,
    heading_selector    = ic$section_heading_selector,
    subheading_selector = ic$section_subheading_selector,
    intro_selector      = ic$section_intro_selector,
    text_header         = ic$section_text_header,
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
#' Vectorised over the row inputs, which all belong to the institution of
#' `config`. Returns a long tibble:
#'   course_id <chr>, institution <chr>, section <chr>, raw_text <chr>
#'
#' @param config Institution config from get_institution_config().
#' @param html Character vector of raw HTML.
#' @param extracted_text Character vector of pre-extracted plain text
#'   (used by text_split and by html_headings' text-split fallback).
#'   Same length as `html`.
#' @param course_id Character vector of course ids (same length as `html`).
extract_sections <- function(config, html, extracted_text, course_id) {
  stopifnot(length(html) == length(course_id))
  stopifnot(length(extracted_text) == length(html))

  cfg <- .section_cfg(config)
  institution <- cfg$institution

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

  doc <- .read_doc(html)

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
  subs <- .subheading_sections(container, cfg$subheading_selector, is_heading,
                               has_nested)
  sub_rest <- attr(subs, "rest") %||% character()
  sub_lead <- attr(subs, "lead") %||% character()
  sub_head <- attr(subs, "head") %||% character()
  sub_keep <- attr(subs, "keep") %||% character()
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
      # Sub-headings act only inside a mapped section: under an unmapped
      # heading (oslomet's programme "Fagplan" block) their text stays out.
      if (is.na(state$parent_section)) return(invisible())
      path <- xml2::xml_path(node)
      sub <- subs[[path]]
      inline <- path %in% names(sub_rest)
      # A sub-heading naming the section already open ("Kunnskap" or
      # "Kunnskap: Studenten kan ..." inside learning outcomes) is a group
      # label: keep it as content, on its own line (#240, #249).
      if (!is.na(sub) && identical(sub, state$current_section)) {
        state$chunks <- c(state$chunks, if (inline) {
          paste(sub_lead[[path]], sub_rest[[path]], sep = "\n")
        } else {
          rvest::html_text2(node)
        })
        return(invisible())
      }
      # A heading on the last line of a paragraph: the lines before it
      # belong to the section that is open.
      if (path %in% names(sub_head)) {
        state$chunks <- c(state$chunks, sub_head[[path]])
      }
      flush()
      # An unmapped sub-heading (<h3>Karakterskala</h3>, a bold group label)
      # returns to the enclosing section and stays as the first line there.
      state$current_section <- if (is.na(sub)) state$parent_section else sub
      if (is.na(sub) || path %in% sub_keep) state$chunks <- rvest::html_text2(node)
      # An inline lead (<p><em>Faget i praksis</em>I løpet ...) starts its
      # section with the rest of the paragraph as its first text.
      if (inline && nzchar(sub_rest[[path]])) state$chunks <- sub_rest[[path]]
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

  .list_to_sections(sections)
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
  is_blank <- !nzchar(trimws(lines))
  current_section <- NA_character_

  # Skip a title + metadata block (uis PDFs: "Emnekode: ...", "Tilbys av:
  # ..."), through the paragraph holding its last line; the untitled text
  # after it is the course introduction.
  if (!is.null(cfg$text_header)) {
    hdr <- which(grepl(cfg$text_header, utils::head(lines, 40), perl = TRUE))
    if (length(hdr)) {
      end <- which(is_blank & seq_along(lines) > max(hdr))[1]
      if (is.na(end)) end <- length(lines)
      lines <- lines[-seq_len(end)]
      is_blank <- is_blank[-seq_len(end)]
      current_section <- "course_content"
    }
  }
  # Skip a table of contents (usn): from "Innholdsfortegnelse" to where its
  # first entry repeats as the real heading (#244).
  toc <- which(trimws(lines) == "Innholdsfortegnelse")[1]
  if (!is.na(toc)) {
    entries <- which(!is_blank & seq_along(lines) > toc)
    body <- entries[-1][trimws(lines[entries[-1]]) == trimws(lines[entries[1]])][1]
    if (!is.na(body)) {
      lines <- lines[-(toc:(body - 1))]
      is_blank <- is_blank[-(toc:(body - 1))]
    }
  }
  n <- length(lines)
  if (n == 0) return(.empty_sections())

  sections <- list()
  chunks <- character()
  strict <- FALSE  # reading list opened by a whole heading ("Litteratur")

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
    exact <- FALSE
    if (prev_blank && .heading_shaped_line(trimmed)) {
      # Strip trailing punctuation like ":" that frequently follows
      # plain-text section labels.
      candidate <- stringr::str_remove(trimmed, "[:：]\\s*$")
      heading_match <- match_heading_to_section(candidate, word_start = TRUE)
      # In a reading list under a whole heading only another whole heading
      # switches section, so a book title such as "Kompetansemål og
      # vurdering" does not (#243).
      exact <- tolower(candidate) %in% section_heading_patterns$pattern
      if (strict && !exact) heading_match <- NA_character_
    }

    if (!is.na(heading_match) && !identical(heading_match, current_section)) {
      flush()
      chunks <- character()
      current_section <- heading_match
      strict <- heading_match == "reading_list" && exact
    } else if (!is.na(current_section)) {
      chunks <- c(chunks, line)
    }
    prev_blank <- FALSE
  }
  flush()

  .list_to_sections(sections)
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

  doc <- .read_doc(html)
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

  .list_to_sections(sections)
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

  doc <- .read_doc(html)
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

  .list_to_sections(sections)
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

# A strategy's list of section -> text chunks as a (section, raw_text) tibble.
.list_to_sections <- function(sections) {
  if (length(sections) == 0) return(.empty_sections())
  .merge_sections(tibble::tibble(section = rep(names(sections), lengths(sections)),
                                 raw_text = unlist(sections, use.names = FALSE)))
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

  details <- .details_sections(.read_doc(html))
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

  doc <- .read_doc(html)
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

  doc <- rvest::read_html(html)  # not .read_doc(): the data is in a <script>
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

  .list_to_sections(sections)
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
#' occurs. Used by unmapped_headings() (R/pipeline.R) to surface unmapped
#' heading texts that suggest pattern-table additions (#192).
#'
#' Returns character() for strategies that don't have a heading
#' concept (text_split, json_nla, noop).
.collect_heading_candidates <- function(html, strategy, institution_config) {
  if (is.na(html) || !nzchar(html)) return(character())
  doc <- .read_doc(html)

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
#' values their section (NA = an unmapped sub-heading, which hands the text
#' that follows back to the enclosing section). Heading tags (h3-h6) match like
#' main headings. A <p> counts when, minus a trailing colon,
#'   - its whole text equals a heading pattern ("whole"), so ordinary
#'     sentences and list-like paragraphs never split a section;
#'   - it opens with an <em>/<strong> run, or a first line before <br>, that
#'     equals one (uia's <p><em>Faget i praksis</em>I løpet ...</p>):
#'     attributes "lead" and "rest" hold the label and the remaining text;
#'   - its last line after <br> equals one (<p>... utvikling.<br>Faget i
#'     praksis</p>): attribute "head" holds the lines before it;
#'   - it is all bold and names no section (<p><strong>Vurdering for studentar
#'     som tar faget 3. studieår</strong></p>): a group label (value NA),
#'     which ends a sub-section and keeps its text;
#'   - it is a colon-ended lead-in naming a coursework gate: coursework, its
#'     text kept (attribute "keep").
#' A <p> inside a list item is skipped unless that <li> holds a section heading
#' (oslomet wraps each whole section in an accordion <li>; #240).
.subheading_sections <- function(container, selector, is_heading, has_heading) {
  none <- stats::setNames(character(), character())
  if (is.null(selector)) return(none)
  nodes <- rvest::html_elements(container, selector)
  nodes <- nodes[!vapply(nodes, is_heading, logical(1))]
  if (length(nodes) == 0) return(none)

  exact <- function(x) {
    x <- stringr::str_remove(stringr::str_squish(x), "[:：]$")
    sec <- section_heading_patterns$section[match(tolower(x),
                                                  section_heading_patterns$pattern)]
    sec[is.na(x) | !nzchar(x) | nchar(x) > 80] <- NA_character_
    sec
  }

  raw <- rvest::html_text2(nodes)
  is_htag <- grepl("^h[1-6]$", tolower(xml2::xml_name(nodes)))
  li <- xml2::xml_find_all(nodes, "ancestor::li[1]", flatten = FALSE)
  in_list <- vapply(li, function(l) length(l) > 0 && !has_heading(l[[1]]),
                    logical(1))

  section <- exact(raw)
  htext <- stringr::str_squish(raw[is_htag])
  section[is_htag] <- vapply(htext, match_heading_to_section, character(1))
  whole <- !is.na(section)
  whole[is_htag] <- nzchar(htext)
  open <- !is_htag & !whole

  # Leading <em>/<strong> run
  lead <- xml2::xml_find_first(
    nodes, "./node()[normalize-space()][1][self::em or self::strong or self::b]")
  has_lead <- !vapply(lead, inherits, logical(1), what = "xml_missing")
  lead_raw <- rep(NA_character_, length(nodes))
  lead_raw[has_lead] <- stringr::str_trim(rvest::html_text2(lead[has_lead]))
  lead_section <- exact(lead_raw)
  inline <- open & !is.na(lead_section)

  # First or last line of a <p> split by <br>
  lines <- lapply(stringr::str_split(raw, "\n"),
                  function(l) stringr::str_trim(l[nzchar(stringr::str_trim(l))]))
  multi <- lengths(lines) > 1
  first <- vapply(lines, function(l) if (length(l)) l[1] else NA_character_, character(1))
  last <- vapply(lines, function(l) if (length(l)) l[length(l)] else NA_character_, character(1))
  br_lead <- open & !inline & multi & !is.na(exact(first))
  lead_raw[br_lead] <- first[br_lead]
  lead_section[br_lead] <- exact(first)[br_lead]
  inline <- inline | br_lead
  tail <- open & !inline & multi & !is.na(exact(last))

  bold_name <- rep(NA_character_, length(nodes))
  bold_name[has_lead] <- tolower(xml2::xml_name(lead[has_lead]))
  label <- open & !inline & !tail & bold_name %in% c("strong", "b") &
    stringr::str_squish(lead_raw) == stringr::str_squish(raw) &
    nchar(stringr::str_squish(raw)) <= 80

  # A colon-ended lead-in that names a coursework gate ("Emnet inkluderer
  # følgende obligatoriske aktiviteter, som må være godkjent før eksamen:")
  # starts coursework_requirements and stays as its first line (uio; #246).
  sq <- stringr::str_squish(raw)
  leadin <- open & !inline & !tail & !label & nchar(sq) <= 150 &
    grepl("[:：]$", sq) & grepl(.coursework_leadin, sq, perl = TRUE)

  section[inline] <- lead_section[inline]
  section[tail] <- exact(last)[tail]
  section[leadin] <- "coursework_requirements"
  keep <- !in_list & (whole | inline | tail | label | leadin)
  paths <- vapply(nodes, xml2::xml_path, character(1))
  out <- stats::setNames(section[keep], paths[keep])

  sel <- keep & inline
  rest <- mapply(function(r, l) {
    after <- stringr::str_sub(r, stringr::str_locate(r, stringr::fixed(l))[, "end"] + 1)
    stringr::str_remove(after, "^\\s*[:：]?\\s*")
  }, raw[sel], lead_raw[sel], USE.NAMES = FALSE)
  attr(out, "rest") <- stats::setNames(as.character(rest), paths[sel])
  attr(out, "lead") <- stats::setNames(lead_raw[sel], paths[sel])
  head <- vapply(lines[keep & tail], function(l) paste(l[-length(l)], collapse = "\n"),
                 character(1))
  attr(out, "head") <- stats::setNames(head, paths[keep & tail])
  attr(out, "keep") <- paths[keep & leadin]
  out
}

.coursework_leadin <- paste0("(?i)arbeidskrav|obligatorisk\\w* (?:læringsaktivitet|aktivitet|",
                             "oppmøte|frammøte|fremmøte|deltak|deltag)")

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
  rl <- out$section == "reading_list"
  out$raw_text[rl] <- stringr::str_remove(   # hiof date stamp line (#248)
    out$raw_text[rl], "^\\s*Litteratur(?:listen|lista) er sist oppdatert[^\\n]*\\s*")
  pre <- out$section == "prerequisites"
  out$raw_text[pre] <- vapply(out$raw_text[pre], .strip_admission_lines,
                              character(1), USE.NAMES = FALSE)
  out$raw_text <- mapply(.clean_section_text, out$raw_text, out$section,
                         USE.NAMES = FALSE)
  keep <- mapply(.keep_section_row, out$raw_text, out$section,
                 USE.NAMES = FALSE)
  out[keep, , drop = FALSE]
}

# Admission/study-right sentences inside a prerequisites block with no heading
# of their own (nord, uib, oslomet; #211). A line is removed only if it has an
# admission phrase and nothing that looks like a real or recommended
# prerequisite (a course code, "bestått", "forkunnskap", credits, "bør",
# "i tillegg til", ...).
.admission_line_regex <- paste0(
  "(?i)opptak skjer|frittstående (?:fag|emne)|studierett|",
  "tilgjengelig som valgfag|søke opptak|enkeltemnestudent|krever opptak til|",
  "^\\s*opptak til (?:studie|lektor|lærer|master|bachelor|grunnskole)|",
  "^\\s*generell studiekompetanse\\.?\\s*$"
)
.real_prerequisite_regex <- paste0(
  "\\b[A-ZÆØÅ]{2,}[A-ZÆØÅ0-9-]*\\d{2,}|",
  "(?i:bestått|forkunnskap|studiepoeng|\\bstp\\b|\\bsp\\b|",
  "\\bbør\\b|anbefal|fordel|rå til|i tillegg|inklusive|bygger på)"
)

.strip_admission_lines <- function(text) {
  lines <- stringr::str_split_1(text, "\n")
  admin <- grepl(.admission_line_regex, lines, perl = TRUE) &
    !grepl(.real_prerequisite_regex, lines, perl = TRUE)
  paste(lines[!admin], collapse = "\n")
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
# "all" applies to every institution; other entries per institution. Most
# patterns remove whole lines (or, with (?s), a trailing block).
.exam_table_header <- paste0(
  "Vurderingsform|Vurderingsordning|Gruppering|Gruppe/individuell|Varighet|",
  "Lengd|Karakterskala|Karakter|Vekting|Vekt|Andel|Kommentarer|Kommentar|",
  "Hjelpemidler|Hjelpemiddel|Omfang")
.section_noise <- list(
  all = c(
    "(?m)^.*(?:plagiatkontroll|for plagiat).*$",                 # nih
    "(?m)^.*(?:ChatGPT|kunstig intelligens|artificial intelligence).*$", # nord
    # COVID notices: the sentence only, since the line may also hold the
    # assessment itself (nord PO111LS; #241)
    paste0("(?m)(?:\\b(?i:pga|jf|ca|evt)\\.[ \t]*)?",
           "(?=[^.!?\n ])[^.!?\n]*(?i:covid|korona)[^.!?\n]*(?:[.!?]+[ \t]*|$)"),
    # flattened exam-table header glued to its data row ("Vurderingsform
    # Gruppering Varighet Karakterskala Andel ... Mappe Individuell A-F";
    # uis, hivolda, inn): drop the header words, keep the values (#248)
    paste0("(?m)^(?:(?:", .exam_table_header, ")[ \t]+){3,}(?:",
           .exam_table_header, ")(?=[ \t]|$)[ \t]*"),
    # stock resit sentence in running text (oslomet; #241)
    paste0("(?i)(?:Deleksamen \\d:[ \t]*)?Ny(?:/| og | eller )ut(?:satt|sett) ",
           "eksamen (?:arrangeres|gjennomføres|foregår|blir arrangert|",
           "blir gjennomført|vert arrangert|vert gjennomført) ",
           "(?:som (?:ved )?)?ordinær(?:e)? eksamen\\.?[ \t]*"),
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
  # A learning-outcome group label ("Kunnskap") is content, not an echo
  # (#249), and a line that is the whole section ("Praksis") stays.
  lines <- stringr::str_split_1(text, "\\r?\\n")
  norm_first <- tolower(trimws(stringr::str_remove(lines[1], "[:：]\\s*$")))
  eq <- section_heading_patterns$pattern == norm_first
  if (length(lines) > 1 && any(eq) &&
      identical(section_heading_patterns$section[which(eq)[1]], section) &&
      !grepl(.lo_group_label, norm_first, perl = TRUE)) {
    text <- paste(lines[-1], collapse = "\n")
  }
  trimws(text)
}

.lo_group_label <- paste0("^(?:kunnskap(?:er|ar)?|ferdighe(?:i)?t(?:er)?|",
                          "generell kompetanse|knowledge|skills|general competence)$")

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
  "pensumlist[ea] for emnet finn(?:er)? du her",
  "oppgis senere",                                                    # ntnu
  "(?:pensum-/)?litteraturliste(?:n)? er ikke publisert ennå",        # usn
  "litteratur(?:lista|listen)? vil være klart? [^.]{0,80}",           # hiof
  "(?:pensumlist[ea]|anbefalt litteratur) for (?:høsten|våren) ?\\d{4}(?:(?: og |-)(?:høsten|våren) ?\\d{4})?",  # nih
  "ettersom vi er i en overgangsfase mellom to systemer .{0,300}",    # nih
  # mf: the library-access notice alone (~600 chars), no list (#248)
  "litteraturlisten for .{0,30}tilgang til litteratur .{0,700}"
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

