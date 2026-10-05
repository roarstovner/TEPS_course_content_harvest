# R/blocks.R
# The block model (#272): a page is read once into a block table, one row per
# heading or piece of text in document order. The fulltext (.blocks_text())
# and the sections (sectionize()) are views of the blocks.
#
# A block table has the columns
#   role     "heading" opens a section (an unmapped heading closes it);
#            "sub"     a sub-heading inside a section (see .sub_blocks());
#            "text"    content;
#            "start", "end" enter and leave an element that holds one section
#            of its own (`section_scope`: a <details>, an accordion item), so
#            the section open around it continues after it
#   text     the block's text
#   section  where a heading or sub-heading maps (R/section_heading_map.R);
#            NA when it maps nowhere, and on text
#   keep     a sub-heading whose text stays as the first line of its section
#   tag      the element the text came from; "p" is a paragraph (a blank line
#            around it in the fulltext), "p+" the rest of a paragraph split at
#            a sub-heading
#
# Readers, chosen by `section_strategy` in R/institution_config.R:
#   html   — a DOM walk under `selector` without `exclude`; headings by
#            `section_heading_selector` (default h2), sub-headings by
#            `section_subheading_selector`, sections of their own by
#            `section_scope`; text before the first heading goes to
#            `section_initial` (default: dropped)
#            When `section_fields` is set (hivolda), each top-level
#            `div.field-<name>` is a heading of its own (see .field_block())
#   json   — nla's EmneplanPage JSON
#   text   — lines of extracted_text (usn, PDF plans; the html fallback),
#            after a header matching `section_text_header`
#   noop   — no plan text (samas)

.empty_blocks <- function() {
  tibble::tibble(role = character(), text = character(), section = character(),
                 keep = logical(), tag = character())
}

# Block reader settings from an institution config. `ic` is passed in rather
# than looked up, so that in the {targets} pipeline only the changed
# institution is rebuilt (#264).
.block_cfg <- function(ic) {
  reader <- ic$section_strategy %||% stop("No section_strategy for institution: ", ic$name)
  if (!reader %in% c("html", "json", "text", "noop")) {
    stop("Unknown section_strategy for ", ic$name, ": ", reader)
  }
  list(
    reader            = reader,
    container         = ic$selector,
    exclude           = ic$exclude,
    heading           = ic$section_heading_selector %||% "h2",
    sub               = ic$section_subheading_selector,
    scope             = ic$section_scope,
    initial           = ic$section_initial,
    text_header       = ic$section_text_header,
    fields            = ic$section_fields,
    inline_coursework = isTRUE(ic$section_inline_coursework),
    pre_fn            = ic$pre_fn,
    institution       = ic$name
  )
}

#' Read one page into a block table
#'
#' @param html Raw HTML of the page (NA for pages without HTML).
#' @param text The page's extracted_text, read by the text reader.
#' @param cfg Reader settings from .block_cfg().
#' @param course_id The offering, for nla's academic year.
#' @return A block table (see the top of R/blocks.R).
page_blocks <- function(html, text, cfg, course_id = NA_character_) {
  switch(cfg$reader,
    html   = .html_blocks(html, cfg),
    json   = .json_blocks(html, course_id),
    text   = .text_blocks(text, cfg),
    .empty_blocks()
  )
}

# Collects blocks in order; $add(role, text, section, keep, tag), $table().
.block_list <- function() {
  role <- text <- section <- tag <- character()
  keep <- logical()
  list(
    add = function(r, t, s = NA_character_, k = FALSE, g = "") {
      role <<- c(role, r); text <<- c(text, t); section <<- c(section, s)
      keep <<- c(keep, k); tag <<- c(tag, g)
    },
    table = function() tibble::tibble(role = role, text = text, section = section,
                                      keep = keep, tag = tag)
  )
}

#' The fulltext of a page from its blocks
#'
#' Block texts in order, one per line, with a blank line around paragraphs
#' (as rvest::html_text2() renders them).
#'
#' @param blocks Block table from page_blocks().
#' @return Character(1), NA when the blocks hold no text.
.blocks_text <- function(blocks) {
  b <- blocks[!blocks$role %in% c("start", "end") & nzchar(trimws(blocks$text)), ]
  if (nrow(b) == 0) return(NA_character_)
  prev <- c("", b$tag[-nrow(b)])
  gap <- b$tag == "p" | (prev %in% c("p", "p+") & b$tag != "p+")
  paste0(c("", ifelse(gap, "\n\n", "\n")[-1]), trimws(b$text), collapse = "")
}

# --- html ---------------------------------------------------------------------

# Elements that flow within a line: a run of them and of text nodes between
# blocks is one text block.
.inline_tags <- c("a", "abbr", "b", "bdi", "bdo", "br", "cite", "code", "data",
                  "dfn", "em", "font", "i", "img", "kbd", "mark", "q", "s",
                  "samp", "small", "span", "strong", "sub", "sup", "time", "u",
                  "var", "wbr")

.html_blocks <- function(html, cfg) {
  if (is.na(html) || !nzchar(html)) return(.empty_blocks())
  if (!is.null(cfg$pre_fn)) html <- cfg$pre_fn(html)
  doc <- .read_doc(html)
  if (!is.null(cfg$exclude)) xml2::xml_remove(rvest::html_elements(doc, cfg$exclude))
  root <- if (is.null(cfg$container)) doc else rvest::html_element(doc, cfg$container)
  if (inherits(root, "xml_missing")) return(.empty_blocks())

  # Mark headings, sub-headings and scopes on the nodes, and their ancestors
  # as nodes to walk into; any other subtree is read whole.
  # With section_fields the fields are the headings
  if (is.null(cfg$fields)) {
    xml2::xml_set_attr(rvest::html_elements(root, cfg$heading), "data-block", "heading")
  }
  if (!is.null(cfg$sub)) {
    subs <- rvest::html_elements(root, cfg$sub)
    subs <- subs[is.na(xml2::xml_attr(subs, "data-block"))]
    # A sub-heading in a list item is content, unless the item holds a
    # heading (oslomet wraps each section in an accordion <li>; #240).
    li <- xml2::xml_find_first(subs, "ancestor::li[1]")
    in_list <- vapply(li, function(l) {
      !inherits(l, "xml_missing") &&
        length(xml2::xml_find_all(l, ".//*[@data-block='heading']")) == 0
    }, logical(1))
    xml2::xml_set_attr(subs[!in_list], "data-block", "sub")
  }
  if (!is.null(cfg$scope)) {
    xml2::xml_set_attr(rvest::html_elements(root, cfg$scope), "data-scope", "1")
  }
  if (!is.null(cfg$fields)) {
    fields <- xml2::xml_find_all(root, paste0(
      ".//div[contains(@class, 'field-') and ",
      "not(ancestor::div[contains(@class, 'field-')])]"))
    xml2::xml_set_attr(fields, "data-block", "field")
    xml2::xml_set_attr(fields, "data-scope", "1")
  }
  marked <- xml2::xml_find_all(root, ".//*[@data-block or @data-scope]")
  xml2::xml_set_attr(xml2::xml_find_all(marked, "ancestor::*"), "data-walk", "1")

  out <- .block_list()
  if (!is.null(cfg$initial)) out$add("heading", "", cfg$initial)
  read_sections <- character()
  walk_contents <- function(node) {
    run <- list()
    flush_run <- function() {
      if (length(run) == 0) return()
      txt <- vapply(run, function(n) if (xml2::xml_name(n) == "br") " " else xml2::xml_text(n),
                    character(1))
      txt <- stringr::str_squish(paste(txt, collapse = ""))
      if (nzchar(txt)) out$add("text", txt)
      run <<- list()
    }
    for (k in xml2::xml_contents(node)) {
      type <- xml2::xml_type(k)
      if (type == "text" || (type == "element" && xml2::xml_name(k) %in% .inline_tags &&
                             is.na(xml2::xml_attr(k, "data-walk")) &&
                             is.na(xml2::xml_attr(k, "data-block")))) {
        run[[length(run) + 1]] <- k
      } else if (type == "element") {
        flush_run()
        visit(k)
      }
    }
    flush_run()
  }
  visit <- function(node) {
    mark <- xml2::xml_attr(node, "data-block")
    scope <- !is.na(xml2::xml_attr(node, "data-scope"))
    if (scope) out$add("start", "")
    tag <- tolower(xml2::xml_name(node))
    if (identical(mark, "heading")) {
      txt <- rvest::html_text2(node)
      out$add("heading", txt, match_heading_to_section(txt), g = tag)
    } else if (identical(mark, "sub")) {
      .sub_blocks(node, out$add)
    } else if (identical(mark, "field")) {
      read_sections <<- .field_block(node, cfg$fields, read_sections, out$add)
    } else if (is.na(xml2::xml_attr(node, "data-walk"))) {
      txt <- rvest::html_text2(node)
      if (nzchar(trimws(txt))) out$add("text", txt, g = tag)
    } else {
      walk_contents(node)
    }
    if (scope) out$add("end", "")
  }
  walk_contents(root)
  out$table()
}

# The section a sub-heading's text names exactly (minus a trailing colon), or
# NA. Exact, so ordinary sentences and list-like paragraphs never split a
# section.
.exact_section <- function(x) {
  x <- stringr::str_remove(stringr::str_squish(x), "[:：]$")
  if (is.na(x) || !nzchar(x) || nchar(x) > 80) return(NA_character_)
  section_heading_patterns$section[match(tolower(x), section_heading_patterns$pattern)]
}

.coursework_leadin <- paste0("(?i)arbeidskrav|obligatorisk\\w* (?:forhold|komponent|læringsaktivitet|aktivitet|",
                             "oppmøte|frammøte|fremmøte|deltak|deltag)")

# A sub-heading candidate (`section_subheading_selector`) as blocks. Heading
# tags (h3-h6) are sub-headings, mapped like headings; an unmapped one
# (<h3>Karakterskala</h3>) stays as a line of the enclosing section. Any other
# element (a <p>) is a sub-heading when
#   - its whole text names a section exactly;
#   - an <em>/<strong> run, or a first line before <br>, opening it names
#     one (uia's <p><em>Faget i praksis</em>I løpet ...</p>); the rest of
#     the paragraph is text;
#   - its last line after <br> names one (<p>... utvikling.<br>Faget i
#     praksis</p>); the lines before it are text;
#   - it is all bold and names no section (<p><strong>Vurdering for studentar
#     som tar faget 3. studieår</strong></p>): a group label, which ends a
#     sub-section and stays as text;
#   - it is a colon-ended lead-in naming a coursework gate ("Emnet inkluderer
#     følgende obligatoriske aktiviteter, som må være godkjent før eksamen:"):
#     coursework_requirements, its text kept (uio; #246).
# Otherwise it is text.
.sub_blocks <- function(node, add_block) {
  raw <- rvest::html_text2(node)
  sq <- stringr::str_squish(raw)
  if (!nzchar(sq)) return(invisible())
  tag <- tolower(xml2::xml_name(node))
  if (grepl("^h[1-6]$", tag)) return(add_block("sub", raw, match_heading_to_section(sq), g = tag))
  # pieces of one paragraph: the first is tagged "p", the rest "p+"
  first <- TRUE
  add <- function(r, t, s = NA_character_, k = FALSE) {
    add_block(r, t, s, k, g = if (first) "p" else "p+")
    first <<- FALSE
  }
  sec <- .exact_section(raw)
  if (!is.na(sec)) return(add("sub", raw, sec))

  lines <- stringr::str_trim(stringr::str_split_1(raw, "\n"))
  lines <- lines[nzchar(lines)]
  lead_node <- xml2::xml_find_first(
    node, "./node()[normalize-space()][1][self::em or self::strong or self::b]")
  lead <- if (inherits(lead_node, "xml_missing")) NA_character_ else
    stringr::str_trim(rvest::html_text2(lead_node))
  first_line <- if (length(lines) > 1) lines[1] else NA_character_
  for (head in c(lead, first_line)) {
    sec <- .exact_section(head)
    if (is.na(sec)) next
    # the sub-heading keeps a colon that follows it ("Ferdigheter: Etter ...")
    after <- stringr::str_sub(raw, stringr::str_locate(raw, stringr::fixed(head))[, "end"] + 1)
    m <- stringr::str_match(after, "(?s)^\\s*([:：]?)\\s*(.*)$")
    add("sub", paste0(head, m[, 2]), sec)
    if (nzchar(m[, 3])) add("text", m[, 3])
    return(invisible())
  }
  if (length(lines) > 1) {
    sec <- .exact_section(lines[length(lines)])
    if (!is.na(sec)) {
      add("text", paste(lines[-length(lines)], collapse = "\n"))
      return(add("sub", lines[length(lines)], sec))
    }
  }
  bold <- !is.na(lead) && tolower(xml2::xml_name(lead_node)) %in% c("strong", "b")
  if (bold && stringr::str_squish(lead) == sq && nchar(sq) <= 80) {
    return(add("sub", raw, NA_character_))
  }
  if (nchar(sq) <= 150 && grepl("[:：]$", sq) && grepl(.coursework_leadin, sq, perl = TRUE)) {
    return(add("sub", raw, "coursework_requirements", TRUE))
  }
  add("text", raw)
}

# --- fields (hivolda) -----------------------------------------------------------

# hivolda (Drupal) renders each part of the plan as `div.field-<name>` with a
# `div.label` heading; the exam table is the unlabelled field-assessments-row
# (#214). Each top-level field is a heading (`section_fields` maps field class
# names to sections; a field not in it maps nowhere) and its text, in a scope
# of its own, so text outside the fields (title, programmes) is in the
# fulltext but in no section. A field opens its section without its label; a
# later field of a section already read keeps its label as text, so
# learning-outcome groups keep their "Kunnskapar" / "Ferdigheiter" prefix.
# Returns `read` plus the field's section.
.field_block <- function(node, fields, read, add) {
  field <- stringr::str_extract(xml2::xml_attr(node, "class"), "(?<![\\w-])field-[a-z-]+")
  section <- unname(fields[field])
  label <- rvest::html_element(node, "div.label")
  seen <- !is.na(section) && section %in% read
  add("heading", if (seen || inherits(label, "xml_missing")) "" else
    rvest::html_text2(label), section, g = "div")
  if (!seen) xml2::xml_remove(rvest::html_elements(node, "div.label"))
  txt <- trimws(rvest::html_text2(node))
  if (nzchar(txt)) add("text", txt, g = "div")
  c(read, section)
}

# --- json (nla) -------------------------------------------------------------------

# nla embeds the plan as JSON (EmneplanPage) in a script tag; the year's entry
# is `props$items[[academic year]]`. Table items and accordions are a title
# and a content (HTML in accordions).
.json_blocks <- function(html, course_id) {
  if (is.na(html) || !nzchar(html)) return(.empty_blocks())
  academic_year <- .nla_academic_year_from_course_id(course_id)
  if (is.na(academic_year)) return(.empty_blocks())

  doc <- rvest::read_html(html)  # not .read_doc(): the data is in a <script>
  scripts <- rvest::html_text(rvest::html_elements(doc, "script"))
  idx <- grep("EmneplanPage", scripts, fixed = TRUE)
  if (length(idx) == 0) return(.empty_blocks())
  json <- regmatches(scripts[idx[1]], regexpr("\\{.*\\}", scripts[idx[1]]))
  if (length(json) == 0) return(.empty_blocks())
  year_data <- jsonlite::fromJSON(json, simplifyVector = FALSE)$props$items[[academic_year]]
  if (is.null(year_data)) return(.empty_blocks())

  out <- .block_list()
  if (nzchar(year_data$title %||% "")) out$add("text", year_data$title, g = "p")
  # contents are HTML (accordions; links in the table)
  item <- function(title, content) {
    title <- title %||% ""
    out$add("heading", title, match_heading_to_section(title), g = "p")
    content <- content %||% ""
    if (grepl("<", content, fixed = TRUE)) {
      content <- rvest::html_text2(rvest::html_element(
        rvest::read_html(paste0("<div>", content, "</div>")), "div"))
    }
    if (nzchar(trimws(content))) out$add("text", trimws(content), g = "p+")
  }
  # Newer pages wrap the table items: table = {list: [...], cta, ctaText}.
  table <- year_data$table
  if (!is.null(table$list)) table <- table$list
  for (it in table %||% list()) item(it$title, it$content)
  for (it in year_data$accordions %||% list()) item(it$title, it$content)
  out$table()
}

# Parse course_id like "nla_CODE_2024_spring_1" into the academic-year key
# used in NLA's JSON ("2023-2024" for spring 2024, "2024-2025" for autumn
# 2024).
.nla_academic_year_from_course_id <- function(course_id) {
  if (is.na(course_id)) return(NA_character_)
  m <- stringr::str_match(course_id, "_(\\d{4})_(spring|autumn|summer)_\\d+$")
  if (is.na(m[1, 1])) return(NA_character_)
  year <- as.integer(m[1, 2])
  switch(m[1, 3], autumn = paste0(year, "-", year + 1),
         spring = paste0(year - 1, "-", year), NA_character_)
}

# --- text -----------------------------------------------------------------------

# Lines of extracted_text. A line is a heading when it
#   - is the first line or follows a blank line,
#   - is heading-shaped (.heading_shaped_line()),
#   - matches a heading pattern at a word start ("elevkunnskap" does not
#     match "kunnskap"),
#   - and names a section other than the open one; a line naming the open
#     section ("Kunnskap" inside learning outcomes) is text.
# In a reading list opened by a whole heading ("Litteratur") only another
# whole heading switches, so a book title such as "Kompetansemål og
# vurdering" does not (#243).
.text_blocks <- function(text, cfg) {
  if (is.na(text) || !nzchar(text)) return(.empty_blocks())
  lines <- stringr::str_split_1(text, "\\r?\\n")
  is_blank <- !nzchar(trimws(lines))
  out <- .block_list()
  current <- NA_character_

  # Skip a title + metadata block (uis PDFs: "Emnekode: ...", "Tilbys av:
  # ..."), through the paragraph holding its last line; the untitled text
  # after it is the course introduction (#243).
  if (!is.null(cfg$text_header)) {
    hdr <- which(grepl(cfg$text_header, utils::head(lines, 40), perl = TRUE))
    if (length(hdr)) {
      end <- which(is_blank & seq_along(lines) > max(hdr))[1]
      if (is.na(end)) end <- length(lines)
      lines <- lines[-seq_len(end)]
      is_blank <- is_blank[-seq_len(end)]
      current <- "course_content"
      out$add("heading", "", current)
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

  strict <- FALSE
  prev_blank <- TRUE
  for (i in seq_along(lines)) {
    if (is_blank[i]) {
      prev_blank <- TRUE
      next
    }
    trimmed <- trimws(lines[i])
    section <- NA_character_
    if (prev_blank && .heading_shaped_line(trimmed)) {
      candidate <- stringr::str_remove(trimmed, "[:：]\\s*$")
      section <- match_heading_to_section(candidate, word_start = TRUE)
      exact <- tolower(candidate) %in% section_heading_patterns$pattern
      if (strict && !exact) section <- NA_character_
    }
    if (!is.na(section) && !identical(section, current)) {
      out$add("heading", lines[i], section)
      current <- section
      strict <- section == "reading_list" && exact
    } else {
      out$add("text", lines[i])
    }
    prev_blank <- FALSE
  }
  out$table()
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
