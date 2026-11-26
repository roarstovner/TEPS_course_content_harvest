# R/extract_sections.R
# Sections of a course plan, from the page's blocks (R/blocks.R).
#
# Entry points:
#   page_sections(blocks, text, cfg) — sections of one page: sectionize(),
#     the text fallback, inline coursework, cleanup
#   extract_sections(config, html, extracted_text, course_id) — the same for
#     many pages of one institution, as a long tibble (course_id,
#     institution, section, raw_text)
#
# The heading map is in R/section_heading_map.R; how an institution's pages
# are read into blocks is set by its section_* fields in
# R/institution_config.R.

#' Sections of one page from its blocks
#'
#' Walks the blocks in order. A heading opens the section it maps to; an
#' unmapped one closes the open section, so the text under it is dropped. A
#' sub-heading acts only inside a section a heading opened: one naming the
#' open section is a group label and stays as content ("Kunnskap" inside
#' learning outcomes; so is a heading "Kunnskap", inn), as does "Litteratur:" inside learning outcomes (a
#' group of outcomes, not a reading list); one naming another section
#' switches to it;
#' an unmapped one returns to the section its heading opened and stays as a
#' line there, except under a `keep` coursework sub-heading (a numbered
#' requirement "Arbeidskrav II: ...", a lead-in "... følgende obligatoriske
#' aktiviteter:"), whose own labels ("Vurderingskriterier:") stay in it; a
#' `keep` sub-heading stays as the first line of its section.
#' A `label` heading or sub-heading ("Hjelpemidler til eksamen") stays as the
#' first line of its text; with no text, or only a dash or pointer ("-", "Se
#' fagplanen."), it goes. "Ingen" stays: no aids is a value.
#' Blocks between "start" and "end" (a <details>, an accordion item) hold a
#' section of their own; the section open around them continues after them.
#' Text goes to the open section. Text under the same section is merged in
#' document order. The title and facts box (`facts` blocks) are in no
#' section.
#' The text under a `practice` heading ("Praksis", "Faget i praksis") also
#' goes to the overlay section `practice`, up to the next heading, start or
#' end, or the next sub-heading (under a practice heading only one that
#' names a section); the practice heading may also be a line of its own
#' inside teaching methods (uit's "Praksis" paragraph), or anywhere as a
#' lettered component ("D. Praksis", nmh), when the text after
#' it opens with a sentence or a line with a count or three words (uis's
#' "15 dager veiledet praksis"), not a label or list item (not in ntnu's
#' learning-forms list "Praksis", "Veiledning", "Obligatoriske arbeidskrav:").
#'
#' @param blocks Block table from page_blocks().
#' @return Tibble `section`, `raw_text`.
sectionize <- function(blocks) {
  blocks <- blocks[!blocks$facts %in% TRUE, ]   # the title and facts box
  sections <- list()
  chunks <- character()
  parent <- current <- NA_character_
  outer <- list()
  labelled <- FALSE   # the chunks open with a label heading
  requirement <- FALSE   # under a `keep` coursework sub-heading
  over <- NA_character_   # role of the practice heading copied to `practice`
  copied <- character()
  practices <- .is_practice_heading(blocks$text)
  flush <- function() {
    if (!is.na(current) && length(chunks) > 0 && !(labelled && .empty_label_text(chunks[-1], current))) {
      txt <- trimws(paste(chunks[nzchar(chunks)], collapse = "\n"))
      if (nzchar(txt)) sections[[current]] <<- c(sections[[current]], txt)
    }
    chunks <<- character()
    labelled <<- FALSE
  }
  flush_practice <- function(to = NA_character_) {
    txt <- trimws(paste(copied[nzchar(copied)], collapse = "\n"))
    if (nzchar(txt)) sections[["practice"]] <<- c(sections[["practice"]], txt)
    copied <<- character()
    over <<- to
  }
  for (i in seq_len(nrow(blocks))) {
    role <- blocks$role[i]
    text <- blocks$text[i]
    section <- blocks$section[i]
    practice <- practices[i]
    if (role %in% c("heading", "start", "end") ||
        (role == "sub" && (practice || identical(over, "sub") || !is.na(section)))) {
      flush_practice(if (practice && (role == "heading" || (role == "sub" && !is.na(parent)))) role else NA_character_)
    } else if (role == "text" && is.na(over) && practice &&
               (identical(current, "teaching_methods") ||
                  grepl(.component_prefix, stringr::str_squish(text), perl = TRUE)) &&
               .opens_sentence(blocks$text[i + 1])) {
      flush_practice("sub")
    } else if (!is.na(over)) {
      copied <- c(copied, text)
    }
    if (role == "heading") {
      flush()
      requirement <- FALSE
      parent <- current <- section
      # a group label as a heading of its own (inn's div.label "Kunnskap")
      # stays as content, as it does as a sub-heading
      labelled <- .is_label_heading(text) || (identical(section, "learning_outcomes") &&
        grepl(.lo_group_label, tolower(sub("[:：]\\s*$", "", trimws(text))), perl = TRUE))
      if (labelled) chunks <- text
    } else if (role == "start") {
      flush()
      outer <- c(list(c(parent, current)), outer)
    } else if (role == "end") {
      flush()
      parent <- outer[[1]][1]
      current <- outer[[1]][2]
      outer <- outer[-1]
    } else if (role == "sub") {
      if (is.na(parent)) next
      # a group label of the open section, or "Litteratur:" inside learning
      # outcomes (hiof: literature, culture, language), which is not a
      # reading list
      # a numbered requirement starts a paragraph group of its own, also
      # under a coursework heading (oslomet); a group label under a `keep`
      # coursework sub-heading stays in it
      numbered <- blocks$keep[i] && identical(section, "coursework_requirements") &&
        .numbered_coursework(sub("[:：].*", "", text), TRUE)
      if ((identical(section, current) && !numbered) ||
          (identical(current, "learning_outcomes") && identical(section, "reading_list")) ||
          (is.na(section) && requirement && !identical(current, parent))) {
        chunks <- c(chunks, text)
        next
      }
      flush()
      requirement <- identical(section, "coursework_requirements") && blocks$keep[i]
      current <- if (is.na(section)) parent else section
      labelled <- !is.na(section) && .is_label_heading(text)
      if (is.na(section) || blocks$keep[i] || labelled) chunks <- text
    } else if (!is.na(current)) {
      chunks <- c(chunks, text)
      # a numbered requirement as a list item under a coursework sub-heading
      # (himolde's "Arbeidskrav I" over a bold title): the group labels under
      # it stay in coursework, as under a `keep` sub-heading
      if (identical(current, "coursework_requirements") && .numbered_coursework(text)) requirement <- TRUE
    }
  }
  flush()
  flush_practice()
  .list_to_sections(sections)
}

# Whether a text opens with a sentence or a content line, with a count or
# three words (uis's "15 dager veiledet praksis"), not a label or list item
# ("Veiledning", "Obligatoriske arbeidskrav:", "Observasjonspraksis 1
# (høst)", ntnu's run-together "ForelesningGruppearbeidSimulering...");
# NA (no text) is not.
.opens_sentence <- function(x) {
  l <- sub("\n.*", "", trimws(x))
  !is.na(l) && !grepl("[:：]$", l) &&
    (grepl("[.!?]", l) && !.is_label(l) ||
       grepl("\\d\\s+\\p{L}|\\p{L}+\\s+\\p{L}+\\s+\\p{L}", l, perl = TRUE) &&
         !grepl("\\p{Ll}\\p{Lu}", l, perl = TRUE))
}

# The text under a label heading says nothing: none, a dash or a pointer.
# "Ingen" answers an aid or a language label, but under a prerequisite label
# ("Krav til forkunnskaper", "Anbefalte forkunnskaper") it is the placeholder
# it is in a prerequisites section without one, and goes with its label.
.empty_label_text <- function(x, section = NA_character_) {
  v <- stringr::str_squish(paste(x, collapse = " "))
  !nzchar(v) || (.is_placeholder_text(v) &&
                   (identical(section, "prerequisites") || !grepl("^(?i:ingen|none|no)\\b", v, perl = TRUE)))
}

# A line that is only a label heading, opening with its pattern
# ("Anbefalte forkunnskaper", "Hjelpemidler:", "Forkunnskapskrav utover
# opptakskrav"), not a sentence, a label with its value or a phrase with the
# word ("Studiets forkunnskapskrav.").
.label_line <- function(x) {
  s <- tolower(sub("[.:：]\\s*$", "", stringr::str_squish(x)))
  out <- nchar(s) > 0 & nchar(s) <= 60 & !grepl("[.!?:：]", s)
  out[out] <- vapply(s[out], function(l) .is_label_heading(l) &&
                       startsWith(l, section_heading_patterns$pattern[heading_pattern(l)]),
                     logical(1), USE.NAMES = FALSE)
  out
}

# Label lines whose text went in the cleanup (an admission sentence, a
# progression rule) or that have none go: up to the next label line, the
# text under a label must say something (.empty_label_text()).
.drop_empty_labels <- function(text, section) {
  lines <- stringr::str_split_1(text, "\n")
  lab <- which(.label_line(lines))
  if (!length(lab)) return(text)
  gone <- logical(length(lines))
  for (k in seq_along(lab)) {
    under <- seq_len(length(lines))[seq_along(lines) > lab[k] &
                                      seq_along(lines) < c(lab, length(lines) + 1)[k + 1]]
    if (.empty_label_text(lines[under], section)) gone[c(lab[k], under)] <- TRUE
  }
  paste(lines[!gone], collapse = "\n")
}

#' Sections of one page
#'
#' sectionize() on the page's blocks (for the text reader, the lines of
#' extracted_text). When the html reader finds fewer than 3
#' sections, the lines of extracted_text are tried instead, and kept if they
#' give more and one that no heading on the page names: the fallback is for
#' headings the html reader missed, and where it saw them its sections stand
#' (ntnu's terse plans, whose admission and unit lines the text would read as
#' course content). A section that only points to another block on the page
#' is filled from it (`section_pointer`), coursework-gate sentences move out of
#' assessment, and so do nord-style coursework lines
#' (`section_inline_coursework`), a prerequisites label in course
#' content takes what it names to prerequisites, and .clean_sections() tidies
#' the rows.
#'
#' @param blocks Block table from page_blocks().
#' @param text The page's extracted_text, for the fallback.
#' @param cfg Reader settings from .block_cfg().
#' @return Tibble `section`, `raw_text`.
page_sections <- function(blocks, text, cfg) {
  title <- blocks$text[blocks$facts %in% TRUE][1]   # for .is_progression_sentence()
  if (identical(cfg$reader, "text")) blocks <- .text_blocks(text, cfg)
  out <- sectionize(.text_subheadings(blocks, cfg$institution))
  if (identical(cfg$reader, "html") && nrow(out) < 3) {
    fallback <- sectionize(.text_blocks(text, cfg))
    named <- blocks$section[blocks$role %in% c("heading", "sub")]
    if (nrow(fallback) > nrow(out) && !all(fallback$section %in% c(named, ".drop"))) out <- fallback
  }
  if (isTRUE(cfg$exam_sessions)) {
    a <- out$section == "assessment"
    out$raw_text[a] <- vapply(out$raw_text[a], .exam_tab_design, character(1), USE.NAMES = FALSE)
  }
  out <- .fill_pointer_sections(out, blocks, cfg$pointer)
  out <- .split_inline_coursework(out, labels = isTRUE(cfg$inline_coursework))
  for (s in c("teaching_methods", "course_content")) out <- .split_inline_coursework(out, "heading", s)
  out <- .split_lo_intro(.move_sentences(out, cfg$institution), cfg$institution)
  out <- .split_inline_prerequisites(out)
  .clean_sections(out, cfg$institution, title)
}

# Paragraphs that open another section with no sub-heading of their own, per
# institution: as a sub-heading would, a text block matching the pattern
# switches to its section and keeps its text, and an unmapped sub-heading
# matching it maps there and stays a line of it. fjellhaug writes the final
# assessment after the arbeidskrav, in the same paragraph group:
# "Sluttvurdering" in bold alone, before a line break ("Sluttvurdering\n
# Skriftlig skoleeksamen ...") or in a sentence ("Sluttvurdering i emnet er
# mappevurdering ..."), and the exam table with its header row
# ("Vurderingsform\tVarighet\t...", "Form of assessment\tDuration\t..."),
# with the grading-scale sentence after it (2026-11-26). "I tillegg til
# sluttvurdering er det tre arbeidskrav" stays.
.text_subheading_rules <- list(
  fjellhaug = c(assessment = "^\\s*(?:Sluttvurdering\\b|(?:Vurderingsform|Form of assessment)\t)")
)
.text_subheadings <- function(blocks, institution) {
  rules <- .text_subheading_rules[[institution %||% ""]]
  if (is.null(rules) || !nrow(blocks)) return(blocks)
  for (s in names(rules)) {
    hit <- grepl(rules[[s]], blocks$text, perl = TRUE) & !blocks$facts %in% TRUE
    sub <- hit & blocks$role == "sub" & is.na(blocks$section)
    blocks$section[sub] <- s
    blocks$keep[sub] <- TRUE   # stays a line, as before it said where the walk went back to
    txt <- which(hit & blocks$role == "text")
    if (!length(txt)) next
    opener <- blocks[txt, ]
    opener$role <- "sub"
    opener$text <- ""
    opener$section <- s
    opener$keep <- FALSE
    blocks <- dplyr::bind_rows(blocks, opener)[order(c(seq_len(nrow(blocks)), txt - 0.5)), ]
  }
  blocks
}

# A prerequisites label inside course_content moves what it names to
# prerequisites: a label line ("Forkunnskapskrav", "Forkunnskapskrav og
# anbefalte forkunnskaper:"; uit "Om emnet") the next paragraph, a label with
# text ("Forkunnskapskrav: 140 stp ...", mf) that text. The label goes, since
# it only names the section. A label followed by another heading-like line
# names nothing. A label after a sentence starts a line ("... enkeltemne.
# ForkunnskapskravJf. opptakskrav ...", uit).
.split_inline_prerequisites <- function(out) {
  i <- which(out$section == "course_content")
  if (length(i) != 1) return(out)
  txt <- stringr::str_replace_all(
    out$raw_text[i], "(?<=[.!?])[ \t]*(?=Forkunnskapskrav[ \t]*(?:[:：]|\\p{Lu}))", "\n")
  txt <- stringr::str_replace_all(txt, "(?m)^([ \t]*Forkunnskapskrav)[ \t]*(?=\\p{Lu})", "\\1: ")
  lines <- stringr::str_split_1(txt, "\n")
  m <- stringr::str_match(lines, "^\\s*([^:：]{1,80}?)\\s*(?:[:：]\\s*(.*))?$")
  # every part of the label names prerequisites
  is_pre <- vapply(m[, 2], function(x) !is.na(x) && all(vapply(
    stringr::str_split_1(x, "\\s*(?:,|\\bog\\b)\\s*"), .exact_section, character(1))
    %in% "prerequisites"), logical(1), USE.NAMES = FALSE)
  if (!any(is_pre)) return(out)
  blank <- !nzchar(trimws(lines))
  n <- length(lines)
  move <- is_pre
  for (k in which(is_pre)) {
    if (!is.na(m[k, 3]) && nzchar(m[k, 3])) {
      lines[k] <- m[k, 3]
      next
    }
    # not when a heading follows (uib pages that list only headings)
    from <- which(!blank & seq_len(n) > k)[1]
    if (is.na(from) || .heading_shaped_line(trimws(lines[from]))) {
      move[k] <- FALSE
      next
    }
    lines[k] <- ""
    to <- c(which(blank & seq_len(n) > from), n + 1)[1] - 1
    move[from:to] <- TRUE
  }
  if (!any(move)) return(out)
  out$raw_text[i] <- stringr::str_replace_all(paste(lines[!move], collapse = "\n"),
                                              "\n[ \t]*(?:\n[ \t]*)+\n", "\n\n")
  moved <- paste(lines[move & nzchar(lines)], collapse = "\n")
  j <- which(out$section == "prerequisites")
  if (length(j) == 1) {
    out$raw_text[j] <- paste(out$raw_text[j], moved, sep = "\n\n")
  } else {
    out <- dplyr::bind_rows(out, tibble::tibble(section = "prerequisites", raw_text = moved))
  }
  out
}

# A section whose whole text is a placeholder naming another block of the page
# ("Se fagplanen.") gets that block's text under the matching sub-heading,
# after a lead that names the source: "Se fagplanen. Fagplanen sier:\n...".
# `pointer` (config `section_pointer`): `heading` of the block, `lead`, and
# `sections`, a sub-heading per section (oslomet). A part of the block runs
# to its next sub-heading, or to a line that starts with a `stop` label (a
# label the page did not mark as a sub-heading). The parts under the `append`
# sub-headings join their section, after the lead, when the page has it with
# text of its own (oslomet: the programme's coursework and attendance rules;
# practicum courses, whose sections only say "Se fagplanen.", get none).
# The parts under the `add` sub-headings that say more than a pointer join
# their section, or make it, on such a page too (oslomet: the subject's
# "Praksistilknytning" and "Praksisopplæring" make practice).
.fill_pointer_sections <- function(out, blocks, pointer) {
  if (is.null(pointer) || nrow(out) == 0) return(out)
  heads <- which(blocks$role == "heading")
  start <- heads[stringr::str_squish(blocks$text[heads]) == pointer$heading][1]
  if (is.na(start)) return(out)
  end <- c(heads[heads > start], nrow(blocks) + 1)[1]
  block <- blocks[seq.int(start + 1, length.out = end - start - 1), ]
  subs <- which(block$role == "sub")
  part <- function(names) {
    txt <- vapply(subs[stringr::str_squish(block$text[subs]) %in% names], function(from) {
      to <- c(subs[subs > from], nrow(block) + 1)[1]
      p <- block[seq.int(from + 1, length.out = to - from - 1), ]
      x <- p$text[p$role == "text"]
      if (!is.null(pointer$stop)) x <- x[cumsum(grepl(pointer$stop, trimws(x))) == 0]
      trimws(paste(x, collapse = "\n"))
    }, character(1))
    paste(txt[nzchar(txt)], collapse = "\n")
  }
  for (sec in names(pointer$sections)) {
    i <- match(sec, out$section)
    if (is.na(i) || !.is_placeholder_text(out$raw_text[i]) ||
        !grepl(tolower(pointer$heading), tolower(out$raw_text[i]), fixed = TRUE)) next
    txt <- part(pointer$sections[[sec]])
    if (nzchar(txt)) out$raw_text[i] <- paste0(trimws(out$raw_text[i]), " ", pointer$lead, "\n", txt)
  }
  for (sec in names(pointer$append)) {
    i <- match(sec, out$section)
    if (is.na(i) || .is_placeholder_text(out$raw_text[i])) next
    txt <- part(pointer$append[[sec]])
    if (nzchar(txt)) out$raw_text[i] <- paste0(trimws(out$raw_text[i]), "\n\n", pointer$lead, "\n", txt)
  }
  if (all(.is_placeholder_text(out$raw_text))) return(out)
  for (sec in names(pointer$add)) {
    txt <- vapply(pointer$add[[sec]], part, character(1))
    txt <- txt[nzchar(txt) & !.is_placeholder_text(txt)]
    if (length(txt)) out <- .append_section(out, sec, paste0(pointer$lead, "\n", paste(txt, collapse = "\n")))
  }
  out
}

#' Sections of many pages of one institution
#'
#' @param config Institution config from get_institution_config().
#' @param html Character vector of raw HTML.
#' @param extracted_text Character vector of extracted text, same length.
#' @param course_id Character vector of course ids, same length.
#' @return Long tibble `course_id`, `institution`, `section`, `raw_text`.
extract_sections <- function(config, html, extracted_text, course_id) {
  stopifnot(length(html) == length(course_id), length(extracted_text) == length(html))
  cfg <- .block_cfg(config)
  rows <- purrr::pmap(list(html, extracted_text, course_id), function(h, txt, cid) {
    out <- page_sections(page_blocks(h, txt, cfg, cid), txt, cfg)
    tibble::tibble(course_id = rep(cid, nrow(out)), institution = rep(config$name, nrow(out)),
                   section = out$section, raw_text = out$raw_text)
  })
  # typed columns also when there are no rows (samas has no plans)
  dplyr::bind_rows(tibble::tibble(course_id = character(), institution = character(),
                                  section = character(), raw_text = character()), rows)
}

# A strategy's list of section -> text chunks as a (section, raw_text) tibble.
# A chunk of 40 or more characters that another chunk of its section repeats
# is dropped (nord's short description is often the first lines of
# "Beskrivelse av emnet", uis repeats a PDF plan's lead sentence).
# Shorter chunks are labels ("Geometri"), which may recur as words.
.list_to_sections <- function(sections) {
  if (length(sections) == 0) return(.empty_sections())
  sections <- lapply(sections, function(x) {
    sq <- stringr::str_squish(x)
    x[!vapply(seq_along(x), function(i) nchar(sq[i]) >= 40 && any(
      stringr::str_detect(sq[-i], stringr::fixed(sq[i])) & (sq[-i] != sq[i] | seq_along(x)[-i] < i)),
      logical(1))]
  })
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

.empty_sections <- function() {
  tibble::tibble(section = character(), raw_text = character())
}

#' Post-process a strategy's (section, raw_text) output for one course.
#'
#' Applies cheap, high-precision cleanup shared across all strategies:
#'   - removes ".drop" rows (admission and exam-logistics headings),
#'   - appends the exam language of an ".exam_language" row to assessment
#'     (.exam_language_text()) and the prerequisites of an ".admission" row
#'     to prerequisites (.admission_prerequisites()),
#'   - strips trailing FS page-generation timestamps,
#'   - drops a leading line that merely repeats the section's own heading,
#'   - removes label lines with nothing under them (.drop_empty_labels()),
#'   - removes rows that are empty or only a placeholder ("Ingen",
#'     "Se fagplanen.", "-", a Leganto pointer; see .placeholder_phrases).
#'   - removes outgoing progression rules from every section
#'     (.is_progression_sentence()),
#'   - strips page widgets, pointers and stock logistics sentences from
#'     assessment and coursework (.section_noise), and approval stamps,
#'     logistics and cost sentences from every section
#'     (.approval_sentence_regex, .logistics_sentences), and uis's workload
#'     budget (.logistics_blocks),
#'   - removes a section whose whole text (100+ characters) repeats an
#'     earlier section's.
#' Fuzzier cleanup (exam tables, arbeidskrav/eksamen boundaries inside one
#' paragraph) is left to the later Phase-2 LLM lifting.
.clean_sections <- function(out, institution = NULL, title = NULL) {
  # ".drop" collects text under headings that end a section but are not
  # course content (admission, exam logistics; see section_heading_map.R).
  out <- out[out$section != ".drop", , drop = FALSE]
  adm <- out$section == ".admission"
  if (any(adm)) {
    out$raw_text[adm] <- .admission_prerequisites(out$raw_text[adm],
                                                  paste(out$raw_text[out$section == "prerequisites"], collapse = "\n"))
    out$section[adm] <- "prerequisites"
    out <- .merge_sections(out[order(adm), , drop = FALSE])   # after the page's own
  }
  el <- out$section == ".exam_language"
  if (any(el)) {
    out$raw_text[el] <- vapply(out$raw_text[el], .exam_language_text, character(1), USE.NAMES = FALSE)
    out$section[el] <- "assessment"
    out <- .merge_sections(out[order(el), , drop = FALSE])   # after the page's own
  }
  if (nrow(out) == 0) return(out)
  pr <- grepl("(?i)(?:for å|skal) (?:kunne )?(?:fortsette|starte|begynne|gå videre)|gå videre til", out$raw_text, perl = TRUE)
  out$raw_text[pr] <- vapply(out$raw_text[pr], .strip_progression, character(1),
                             title = title, USE.NAMES = FALSE)
  exam <- out$section %in% c("assessment", "coursework_requirements")
  out$raw_text[exam] <- .strip_section_noise(out$raw_text[exam], institution)
  rx <- if (!is.null(institution)) .logistics_sentences[[institution]]
  if (!is.null(rx)) out$raw_text <- vapply(out$raw_text, .strip_sentences, character(1),
                                           rx = paste0("(?:", rx, ")", collapse = "|"), USE.NAMES = FALSE)
  for (r in if (!is.null(institution)) .logistics_blocks[[institution]])
    out$raw_text <- stringr::str_remove_all(out$raw_text, r)
  ap <- grepl("(?i)vedtatt i|godkjent|revi(?:dert|sjon)|redaksjonell|etablert av|gjeldende fra",
              out$raw_text, perl = TRUE)
  out$raw_text[ap] <- vapply(out$raw_text[ap], .strip_sentences, character(1),
                             rx = .approval_sentence_regex, USE.NAMES = FALSE)
  rl <- out$section == "reading_list"
  out$raw_text[rl] <- stringr::str_remove(   # hiof date stamp line
    out$raw_text[rl], "^\\s*Litteratur\\w*(?: er)? sist oppdater[^\\n]*\\s*")
  out$raw_text[rl] <- stringr::str_remove(   # usn's stock line after a list or a note
    out$raw_text[rl], paste0("(?i)\\n\\s*(?:(?:pensum-/)?litteraturliste(?:n)? er ikke publisert ennå|",
                             "the reading list is not published yet)\\.?\\s*$"))
  out$raw_text[rl] <- stringr::str_remove(   # uit's Leganto notice and the list's title
    out$raw_text[rl], paste0("^\\s*Du kan se og få tilgang til deler av pensum via Leganto\\.\\s*",
                             "(?:Pensumliste for [^\\n]*(?:\\n|$))?\\s*"))
  out$raw_text[rl] <- vapply(out$raw_text[rl], .strip_list_tabs, character(1),
                             USE.NAMES = FALSE)
  pre <- out$section == "prerequisites"
  out$raw_text[pre] <- vapply(out$raw_text[pre], .strip_admission_lines,
                              character(1), USE.NAMES = FALSE)
  tm <- out$section %in% c("teaching_methods", "practice")
  out$raw_text[tm] <- stringr::str_remove_all(out$raw_text[tm], .praksis_pointer_regex)
  pr <- out$section == "practice"
  out$raw_text[pr] <- stringr::str_remove_all(out$raw_text[pr], .no_practice_regex)
  tm <- out$section == "teaching_methods"
  out$raw_text[tm] <- stringr::str_remove_all(out$raw_text[tm], .evaluation_part_regex)
  ev <- tm & identical(institution, "uit") & grepl("(?i)evalu|undergo|kvalitetssikring|quality", out$raw_text, perl = TRUE)
  out$raw_text[ev] <- vapply(stringr::str_remove_all(out$raw_text[ev], .evaluation_label_regex), .strip_sentences,
                             character(1), rx = .evaluation_sentence_regex, USE.NAMES = FALSE)
  out$raw_text <- mapply(.drop_empty_labels, out$raw_text, out$section, USE.NAMES = FALSE)
  out$raw_text <- mapply(.strip_placeholder_lines, out$raw_text,
                         trailing = out$section != "assessment", USE.NAMES = FALSE)
  out$raw_text <- mapply(.clean_section_text, out$raw_text, out$section,
                         USE.NAMES = FALSE)
  keep <- mapply(.keep_section_row, out$raw_text, out$section,
                 USE.NAMES = FALSE)
  out <- out[keep, , drop = FALSE]
  # A section whose whole text of 100 or more characters repeats an earlier
  # one's says nothing about its own part: the page copied it (khio's teaching
  # methods under "Vurdering", its outcomes under "Undervisning og
  # læringsformer"; inn's course content under the teaching methods). A
  # short value each section states itself ("Individuelt.", "Må bestås")
  # stays, and so does practice, which copies its heading's section by design.
  sq <- stringr::str_squish(out$raw_text)
  sq[out$section == "practice" | nchar(sq) < 100] <- NA
  out[!duplicated(sq, incomparables = NA), , drop = FALSE]
}

# The tab bar of a Leganto widget with several lists (usn): the reading list
# opens with the lists' labels ("Norsk, emne 2 (MG1NO2) Drammen"), and each
# list repeats its label. Leading labels that recur later go.
.strip_list_tabs <- function(text) {
  lines <- stringr::str_split_1(text, "\n")
  sq <- stringr::str_squish(lines)
  k <- 0
  while (k < length(lines) - 1 &&
         grepl("\\(\\s*\\p{Lu}{2,}[\\p{Lu}0-9-]*\\d", sq[k + 1], perl = TRUE) &&
         sq[k + 1] %in% sq[-seq_len(k + 1)]) k <- k + 1
  if (k == 0) text else paste(lines[-seq_len(k)], collapse = "\n")
}

# Admission and study right in a prerequisites block, under the block's own
# heading (spec: prerequisites hold prior knowledge, not admission). A
# sentence goes if it has an admission phrase and nothing that looks like a
# real or recommended prerequisite (a course code, "bestått", "forkunnskap",
# credits, "bør", "i tillegg til", ...), or if it says there is nothing
# beyond admission ("Ingen utover opptakskravet."). Mixed sentences
# stay ("Opptak til femårig lektorutdanning og beståtte eksamener i henhold
# til forskrift."). A line that opens with the admission rule or with the
# applicants ("Opptakskrav er ...", "Søkere må ...") states admission and
# goes whole (.admission_rule_regex).
.admission_line_regex <- paste0(
  "(?i)opptak skjer|frittstående|studierett|enkeltemne|tilgjengelig som valgfag|",
  "søke[rs]? (?:særskilt )?opptak|krev(?:er|es) generell studiekompetanse|",
  "(?:forutsetter|forusetter|krever|krav om|godkjent|betinget) opptak|",
  "prioritert ved opptak|rangeres (?:av|etter)|fordeler studieplass|",
  "(?:\\bfore?beholdt?|begrenset til) (?:for )?student|",   # "Emnet er forbeholdt studenter ..."
  "obligatorisk for studenter (?:som er )?tatt opp|",       # nmbu: "... tatt opp til studieprogrammet LUR."
  "må være tatt opp (?:på|til|som) (?!.*\\b(?:og|eller) (?:ha|har)\\b)|",   # study right, not "... og ha ..."
  "åpent for studenter som er tatt opp|for å (?:kunne )?bli tatt opp|",
  "^(?:(?:jf\\.?|jamfør|som|tilsvarende) )?opptakskrav|",   # pointers to the programme
  "må oppfylle (?:\\S+ ){0,3}opptakskrav|^studiets opptakskrav\\.?$|",
  "^generelle opptakskrav(?: (?:for|til) [^.]*)?\\.?$|",
  "^(?:se|sjå)\\s.*opptak|",
  "^(?:ved )?opptak (?:til|på) (?:\\S+ ){0,3}?\\S*(?:utdanning|program|studi|lektor|lærer|master)|",
  "^opptaksgrunnlag\\w*\\.?$|^spesielle vilkår$|",       # ntnu's admission heading
  "^generell studiekompetanse(?: eller (?:godkjent )?realkompetanse)?(?: etter [^.]*)?\\.?$|",
  "admission requirement|^admission to\\b|^(?:the students? (?:must|should) (?:have been|be) )?admitted to\\b"   # English pages
)
.admission_rule_regex <- paste0(
  "(?i)^(?:forkunnskapskrav\\s*:\\s*)?(?:opptakskrav(?:et)?(?: til \\S+)? er\\b|opptakskrav\\s*:|",
  "(?:slike |andre (?:kvalifiserte )?)?søkere\\b|applicants\\b)"
)
.no_prerequisite_regex <- paste0(
  "(?i)^(?!.*(?:\\bmen\\b|\\bbør\\b|anbefal|\\bbut\\b|recommend|advantage))(?:",
  "(?:(?:det (?:kreves|er)|kurset har|emnet har) )?ingen\\b.*\\b(?:ut ?over|utenom)\\b.*opptak|",
  "(?:no|this course does not require any)\\b.*\\b(?:over and above|beyond|apart from|other than)\\b.*admission)"
)
.real_prerequisite_regex <- paste0(
  "\\b[A-ZÆØÅ]{2,}[A-ZÆØÅ0-9-]*\\d{2,}|",
  # aho's course codes on a line of their own or before the title ("61
  # 110", "70 111 GK1 Introduksjon til design") and the lead-in over them
  # ("Gjennomført:"); completed study years ("Gjennomført 1.studieår
  # bachelor i utøvende musikk.", "Normalt gjennomført 1. og 2. studieår",
  # barrattdue); 2026-11-22
  "^\\s*\\d{2} \\d{3}(?:\\s*$|\\s+\\p{Lu})|(?i:^\\s*(?:gjennomført|fullført)\\s*:\\s*$)|",
  "(?i:(?:gjennomført|fullført)(?:\\s+(?:\\d\\.|og|til|[-–]|første|andre|tredje|fjerde))+\\s?studieår)|",
  "(?i:bestått|forkunnskap|studiepoeng|\\bstp\\b|\\bsp\\b|",
  "\\bbør\\b|anbefal|fordel(?!er )|rå til|i tillegg|inklusive|bygger på|",
  "\\bpassed\\b|completed|prior knowledge|recommend|advantage|\\bECTS\\b|credits|in addition)"
)

# Outgoing progression rules: what passing this course permits later is no
# prior knowledge for it, and belongs to no section (spec: prerequisites).
# Its subject is this course and its condition is to go on to something
# later ("Emnet må være bestått for å kunne fortsette på Kunst og Håndverk 2
# (GLU 1-7).", "Emnet må være bestått for å kunne starte på avsluttende
# praktisk-teologisk utdanning (APTU).", "Må tas for å fortsette med
# masterfag ...", "Studenten må stå i emnet for å kunne fortsette på ...",
# "Studentene må bestå dette for å kunne starte på masteroppgaven 5.
# studieår.", "Studenten må ha fulgt undervisningen i emnet for å kunne
# fortsette videre i studiet", "Studenter kan, ved å gjennomføre
# dette emnet og emnet ... gå videre til ...", barrattdue's "Eksamen må være
# bestått for at studenten skal kunne fortsette PPU-studiet 2. år."), or the
# course it names first
# is this one, by its title ("En må ha fulgt Kunst og håndverk 1 for å kunne
# fortsette på Kunst og håndverk 2" in the Kunst og håndverk 1 plans,
# "Studenten må ha fulgt undervisning og fått godkjent arbeidskravene i
# MGLSPES101 Spesialpedagogiske grunnlagsspørsmål for å kunne fortsette på
# MGLSPES102 ..."; in the plan of the later course the same sentence is a
# prerequisite and stays).
.progression_regex <- paste0(
  "(?i)^\\W*(?:(?:(?:dette )?(?:emnet|kurset) )?må (?:være )?(?:bestått|gjennomført|fullført|tas)|",
  "(?:studenten\\w*|en|du) må (?:stå i|bestå|ha fulgt undervisning(?:en)? i) (?:dette emnet|emnet|dette))\\b",
  "[^.]*?\\bfor å (?:kunne )?(?:fortsette|starte|begynne|gå videre)\\b|",
  "\\bved å (?:gjennomf(?:ø)?re|fullføre|bestå) dette emnet\\b[^.]*?\\bgå videre til\\b|",
  "^\\W*bestått (?:semester|emne|studieår) er en forutsetning for å (?:kunne )?(?:fortsette|starte|begynne|gå videre)\\b|",
  "^\\W*eksamen må være bestått for at (?:studenten|du) skal kunne (?:fortsette|starte|begynne|gå videre)\\b")
.progression_named_regex <- paste0(
  "(?i)^\\W*(?:en|du|studenten\\w*) må ha (?:fulgt|bestått|gjennomført|fullført) ",
  "(?:undervisning(?:en)?,? og fått godkjent arbeidskravene (?:i|på) )?(?-i:(?:\\d?\\p{Lu}{2,}[\\p{Lu}0-9-]*\\d\\w*\\s+)?)(.+?)",
  " for å (?:kunne )?(?:fortsette|starte|begynne|gå videre)\\b")
.is_progression_sentence <- function(s, title = NULL) {
  out <- grepl(.progression_regex, s, perl = TRUE)
  t <- tolower(stringr::str_squish(title %||% NA_character_))
  if (is.na(t) || !nzchar(t)) return(out)
  named <- tolower(stringr::str_squish(stringr::str_match(s, .progression_named_regex)[, 2]))
  this <- !is.na(named) & startsWith(t, named) &   # the title, or its opening before "(GLU 5-10)"
    !grepl("^[\\p{L}\\p{N}]", substring(t, nchar(named) + 1), perl = TRUE)
  out | this
}

# A colon-ended rule takes the list it introduces, the rest of its paragraph
# or the next one ("Emnet må være bestått for å kunne fortsette på:" over
# "P261 Innføring i spesialpedagogisk arbeid ...").
.strip_progression <- function(text, title = NULL) {
  lines <- stringr::str_split_1(text, "\n")
  blank <- function(k) k <= length(lines) && !nzchar(trimws(lines[k]))
  for (k in rev(which(grepl("[:：]\\s*$", lines) & .is_progression_sentence(trimws(lines), title)))) {
    to <- k + 1
    while (blank(to)) to <- to + 1
    while (to <= length(lines) && !blank(to)) to <- to + 1
    while (blank(to)) to <- to + 1
    lines <- lines[-(k:(to - 1))]
  }
  .strip_sentences(paste(lines, collapse = "\n"), function(s) .is_progression_sentence(s, title))
}

.is_admission_sentence <- function(s) {
  s <- sub("(?i)^forkunnskapskrav\\s*:\\s*", "", trimws(s), perl = TRUE)
  code <- gsub("\\(\\p{Lu}{3,}[\\p{Lu}0-9-]*\\)", "", s, perl = TRUE)   # programme: "(MGLU5-10)"
  grepl(.admission_line_regex, s, perl = TRUE) &
    !grepl(.real_prerequisite_regex, code, perl = TRUE) |
    grepl(.no_prerequisite_regex, s, perl = TRUE)
}

# The prerequisites in a block that mixes them with admission (nord's
# "Adgangsregulering", `section_headings` ".admission"; uit's "Opptakskrav";
# nmh's summary field "Forhåndskrav:", barrattdue's and ldh's "Krav til
# forkunnskaper:", whose label goes): its sentences that
# name a prior course, credits or a passed part ("Bestått 90 stp. i
# masterfaget og bestått FoU-oppgave.", "Gjennomført SAM1001 Samfunnsfag 1A
# eller tilsvarende.") and are not admission ("Emnet er kun åpent for
# studenter på ...", "Søker må ha fullført en bachelorgrad ...", "Det tas
# forbehold om ledig kapasitet", a quota, a pointer "Eventuelle
# forkunnskapskrav til emnet må være oppfylt."), minus those the prerequisites already say. A
# sentence that names both stays, without an "Opptakskrav:" label ("Opptak
# til studieprogrammet og bestått Pedagogikk og elevkunnskap 1A",
# "Opptakskrav: Generell studiekompetanse samt ... bestått eksamen i emnet
# NO106L ..."). They follow the page's own prerequisites.
# What makes a sentence of such a block admission although it names credits
# or a passed part: the applicants, prior learning, places, quotas; and
# (uit's "Opptakskrav", 2026-11-16) the engineering admission routes
# ("bestått 1-årig forkurs for ingeniørutdanning, eller", "2-årig teknisk
# fagskole ...", "Matematikk R1 ... programfag:"), the grade average and how
# it is computed, the applicant's motivation, work experience, education
# abroad, what an admission gives access to, a bare prerequisites label
# whose text is elsewhere ("Anbefalte forkunnskaper:"), and a degree with no
# course code ("Avlagt bachelorgrad hvor det inngår en fordypning på minimum
# 80 studiepoeng sosiologi ...", "Graden må ha en faglig fordypning ...",
# .degree_regex): admission to the programme (course_plan_spec.md, known
# deviations), and (nmh) a passed admission audition ("Opptak til et av
# studieprogrammene ... samt bestått opptaksprøve til ordningen."). "For
# studenter som tar emnet som videreutdanning er forkunnskapskravet fullført
# lærerutdanning med minimum 60 studiepoeng i norsk." names the subject and
# stays. Also admission (uia's and nih's admission fields, 2026-11-25): the
# pointer "I tillegg må eventuelle forkunnskapskrav vere oppfylte.", "I
# tillegg må det avklares at studentene har tilstrekkelig tid ...", who is
# pre-approved ("Det er også personer som har ..."); and in .degree_regex a
# degree named as the admission rule ("Opptakskrav er bachelorgrad ...",
# "Krav til opptak er bachelorgrad (180 studiepoeng) i helse- og sosialfag.",
# not "... og grunnleggende forkunnskaper innen e-helse") and a completed
# teacher education with no credits ("For enkeltemnestudenter: fullført
# lærerutdanning ..., samt ordinære forkunnskapskrav."; "..., og minst 60 stp
# i engelsk." stays).
.admission_only_regex <- paste0(
  "(*UCP)(?i)^\\W*(?:søkere?|applicants?)\\b|må søkeren oppfylle|realkompetanse|studieplass|prioriter|kvote|",
  "upper secondary|^\\W*(?:i tillegg må )?(?:eventuelle )?forkunnskapskrav\\w* (?:til emnet )?(?:må )?(?:være|vera|vere) oppfylt|",
  "må det avklares|^\\W*(?:det er også )?personer (?:som|med)\\b|",
  "forkurs|teknisk fagskole|realfagkurs|\\b(?:R1|R2|S1|S2)\\b|programfag|",
  "gjennomsnittskarakter|karaktergjennomsnitt|snittkarakter|karakteren beregnes|vurderingsuttrykk|minstekrav|",
  "motivasjon|yrkespraksis|års (?:relevant )?praksis|arbeidserfaring|fra utlandet|gir adgang til|kvalifiserer (?:også )?(?:til|for) opptak|",
  "opptaksgrunnlag|søke opptak|vurderes for opptak|bestått opptaksprøve|rangeres|av fordypningen|kreves karakteren|\\(jfr?\\.? opptakskrav|^\\W*fordypning(?:en)? (?:skal|må)|fordypning på minimum|fordypningsenhet|karakterkrav|^\\W*av de \\d+ (?:stp|studiepoeng)|^\\W*(?:anbefal\\w* )?(?:forkunnskap\\w*|forkunnskapskrav)(?:,? anbefal\\w* forkunnskap\\w*)?\\s*:?\\s*$")
.degree_regex <- paste0(
  "(?i)^\\W*(?:(?:avlagt|fullført|relevant|treårig) )?(?:bachelor\\w*|master(?:grad\\w*)?\\b|cand\\.)|^\\W*graden må|",
  "^\\W*fordypning:|^\\W*det kreves (?:godkjent )?utdanning|krav om (?:\\S+ )?bachelorgrad|",
  "^\\s*[-–•]\\s*(?:minst \\d+ studiepoeng|\\d-årig studium)|",
  "^\\W*(?:generelt )?(?:opptakskrav|krav til opptak) er (?:en )?(?:\\p{L}+ )?bachelorgrad(?![^.]*forkunnskap)|",
  "^\\W*(?:for enkeltemnestudent\\w*:\\s*|opptakskrav for enkeltemnestudent\\w* er\\s+)?(?:fullført|godkjent) ",
  "lær[ae]r\\w*utdanning\\b(?![^.]*\\b(?:stp|studiepoeng|sp)\\b)")
# A degree in a named subject joined to the admission in one sentence names
# both and stays whole (barrattdue's PPU field "Krav til forkunnskaper:
# Bachelorgrad i musikk og Opptak til Praktisk-pedagogisk utdanning";
# 2026-11-23, in no section before); a bare degree or a bare admission
# ("Opptak til ...") does not.
.degree_admission_regex <- "(?i)^\\W*(?:(?:avlagt|fullført) )?(?:bachelor|master)\\w* i \\p{L}[^.]*?\\bog opptak (?:til|på)\\b"
# A label in the block that opens recommended prior knowledge (aho's
# "Recommended previous knowledge." over a list of topics, "Recommended
# prerequisite knowledge" over "The course builds on the design skills ...";
# 2026-11-22) keeps the lines after it whole, but a link ("Upload portfolio
# here: https://..."). The label stays, as does a label line ("Krav til
# forkunnskaper", "Anbefalte forkunnskaper") or the label of a field ("Krav
# til forkunnskaper: BSY-100", ldh) before what is kept (2026-11-26: they say
# whether it is required or recommended); .drop_empty_labels() takes a label
# with nothing after it.
.recommended_knowledge_regex <- "(?i)^\\W*recommended (?:previous|prerequisite|prior) knowledge\\W*$"
.admission_prerequisites <- function(text, prerequisites = "") {
  had <- stringr::str_squish(prerequisites)
  ul <- .unwrap_lines(stringr::str_split_1(text, "\n"))
  rec <- cumsum(grepl(.recommended_knowledge_regex, ul, perl = TRUE)) > 0 & !grepl("https?://", ul)
  lines <- vapply(seq_along(ul), function(i) {
    l <- ul[i]
    if (rec[i] || .label_line(l)) return(trimws(l))
    lab <- stringr::str_match(l, "(?i)^\\s*(krav til forkunnskaper\\s*:)")[, 2]
    s <- sub("(?i)^(\\s*)(?:opptakskrav(?: for enkeltemnestudent\\w*)?|forhåndskrav|krav til forkunnskaper)\\s*:\\s*", "\\1", stringr::str_split_1(l, .sentence_end), perl = TRUE)
    k <- (grepl(.real_prerequisite_regex, s, perl = TRUE) & !.is_admission_sentence(s) &
      !grepl(.admission_only_regex, s, perl = TRUE) &
      !(grepl(.degree_regex, s, perl = TRUE) & !grepl("\\b[A-ZÆØÅ]{2,}[A-ZÆØÅ0-9-]*\\d{3,}", s, perl = TRUE)) |
      grepl(.degree_admission_regex, s, perl = TRUE)) &
      !vapply(stringr::str_squish(s), function(x) !nzchar(x) || grepl(x, had, fixed = TRUE), logical(1))
    trimws(paste(c(if (!is.na(lab) && k[1]) lab, s[k]), collapse = if (!is.na(lab) && k[1]) " " else ""))
  }, character(1), USE.NAMES = FALSE)
  lines <- lines[nzchar(lines)]
  # a bare lead-in of up to six words whose list all went or the page
  # already says ("I tillegg:", "Anbefalte krav:", uia; "Recommended
  # prerequisites:", uit) goes with it
  while (length(lines) && grepl("^\\W*(?:\\S+\\s+){0,5}\\S+[:：]\\s*$", lines[length(lines)], perl = TRUE))
    lines <- lines[-length(lines)]
  paste(lines, collapse = "\n")
}

# The exam language in a field of both languages (uit's "Undervisnings- og
# eksamensspråk", "Language of instruction and examination", `section_headings`
# ".exam_language"; 2026-11-16): the label line and the sentences that say
# what language the exam or the work handed in is in ("Eksamen gis på norsk.",
# "The thesis must be written in English.", "Undervisnings- og eksamensspråket
# er norsk.") or that are a bare value ("Norsk", "Norwegian or English by
# request"), which the label makes the exam language too; they follow the
# page's own assessment. The teaching, supervision and syllabus language alone
# ("Undervisningsspråket er norsk.", "Samisk kan benyttes ...", "Pensum er
# skrevet på nordisk og engelsk.", "Emnet er norskspråklig.") goes, and the
# label with it when nothing is left.
.exam_language_regex <- "(?i)\\b(?:eksam|eskam|exam|besvar|answer|avlegg|test)"
.written_work_regex <- paste0("(?i)\\b(?:skriv|writ|lever|innlever|submit|thesis|essay|paper|reports?\\b|rapport|",
                              "avhandling|artikkel|article)|oppg[aå]v")
.teaching_language_regex <- paste0("(?i)undervis|veiled|instruction|teaching|taught|supervision|lecture|",
                                   "forelesning|pensum|syllabus|curriculum|litteratur|seminar|reading")
.language_value_regex <- paste0("(?i)^[\\s\\p{P}]*(?:(?:\\p{L}+sk[e]?|\\p{L}+ish|norwegian|russian|german|french|",
                                "spanish|sami|scandinavian|bokmål|nynorsk|og|eller|and|or|ved behov|by request|",
                                "språk|languages?)[\\s\\p{P}]*)+$")
.exam_language_text <- function(text) {
  lines <- stringr::str_split_1(text, "\n")
  rest <- .strip_sentences(paste(lines[-1], collapse = "\n"), function(s) !(
    grepl(.exam_language_regex, s, perl = TRUE) | grepl(.language_value_regex, s, perl = TRUE) |
      grepl(.written_work_regex, s, perl = TRUE) & !grepl(.teaching_language_regex, s, perl = TRUE)))
  if (nzchar(trimws(rest))) paste(lines[1], rest, sep = "\n") else ""
}

# Where a sentence ends: after . ! ? before a capital, not after an
# abbreviation ("Jf. Opptakskrav ...").
.sentence_end <- paste0("(?<=[.!?])(?<!\\b(?i:jf|jfr|jamfør|ca|nr|kap|bl\\.a|f\\.eks|evt|osv|ph\\.d)\\.)",
                        "(?=\\s+\\p{Lu}|\\p{Lu}\\p{Ll})")

# Admission sentences go from their lines. A colon-ended admission line (or
# "Spesielle vilkår") takes what it introduces (a programme list: the rest of
# its paragraph, or the next one), and a label whose text all went goes too
# ("Forkunnskapskrav:" over "Som opptakskrav til studieprogrammet, se ...").
# A sentence across lines (.unwrap_lines) is read whole, so "Bestått ...,
# eller" over "Opptak til ..." names both and stays.
.strip_admission_lines <- function(text) {
  lines <- .unwrap_lines(stringr::str_split_1(text, "\n"))
  n <- length(lines)
  gone <- logical(n)
  for (k in seq_len(n)) {
    s <- stringr::str_split_1(lines[k], .sentence_end)
    a <- .is_admission_sentence(s) | grepl(.admission_rule_regex, trimws(s[1]), perl = TRUE)
    gone[k] <- all(a) && nzchar(trimws(lines[k]))
    if (any(a) && !gone[k]) lines[k] <- trimws(paste(s[!a], collapse = ""))
  }
  if (!any(gone)) return(paste(lines, collapse = "\n"))
  blank <- !nzchar(trimws(lines))
  lead <- grepl("[:：]\\s*$|(?i)^\\s*spesielle vilkår\\s*$", lines, perl = TRUE)
  after <- function(k) {
    from <- which(!blank & seq_len(n) > k)[1]
    if (is.na(from)) return(integer())
    to <- which(blank & seq_len(n) > from)[1]
    from:(if (is.na(to)) n else to - 1)
  }
  for (k in which(gone & lead)) gone[after(k)] <- TRUE
  for (k in rev(which(!gone & lead))) {
    i <- after(k)
    if (length(i) > 0 && all(gone[i])) gone[k] <- TRUE
  }
  # a conjunction left dangling by a dropped paragraph goes: "..., eller" over
  # it, or a line "eller" next to it
  conj <- "(*UCP)(?i)[,;]?\\s*\\b(?:eller|or|og|and)\\s*$"
  nb <- which(!blank)
  for (k in which(!gone & grepl(conj, lines, perl = TRUE))) {
    bare <- !grepl("\\S", sub(conj, "", lines[k], perl = TRUE))
    if (isTRUE(gone[nb[nb > k][1]]) || (bare && isTRUE(gone[rev(nb[nb < k])[1]]))) {
      lines[k] <- sub(conj, "", lines[k], perl = TRUE)
      gone[k] <- bare
    }
  }
  paste(lines[!gone], collapse = "\n")
}

# Lines in an assessment block that state a coursework gate. nord
# writes them as "Arbeidskrav (AK): ..." or "Obligatorisk deltakelse (OD): ..."
# inside one undifferentiated vurdering block, with no heading to split on,
# or by the abbreviation alone ("AK: Inntil tre arbeidskrav", "• OD
# Obligatorisk deltakelse", "To arbeidskrav.", English "Coursework
# requirements:"); uit as "Følgende arbeidskrav må være godkjent før ...:"
# and a list.
.inline_coursework_regex <- paste0(
  "(?i)^\\s*(?:[-•*]\\s*)?(?:(?:(?:inntil|minst|opptil) )?",
  "(?:\\d+|en|ett?|ei|to|tre|fire|fem|seks|sju|syv|åtte|ni|ti) (?:\\w+ )?arbeidskrav(?:ene|et)?|",
  "følg(?:ende|jande) (?:obligatoriske )?arbeidskrav|(?:beskriv\\w+ av )?arbeidskrav(?:ene|et)?|",
  "\\d\\) [^.\\n]{0,200}\\bgodkjente oppgaver|",   # uit 2008: "1) ... krav til antall godkjente oppgaver"
  "\\(\\p{Lu}\\) betyr at arbeidskrav\\w*|",       # dmmh: "(P) betyr at arbeidskravet bygger på oppgaver ..."
  "(?-i:AK|OD|OA)\\d*|",                         # nord: AK, OD2 as their label
  "obligatoriske? (?:deltakelse|deltagelse|deltaking|arbeid|arbeidskrav|",
  "aktivitet(?:er)?|oppmøte|frammøte|fremmøte)|",
  "deltakelse i undervisning(?:en)? er obligatorisk|coursework(?: requirements?)?|",
  "(?:compul\\w?s[ou]ry|mandatory) (?:participation|attendance|work|assignments?|",   # aho: "Compulasory", "Compulsury"
  "(?:learning )?activit(?:y|ies))|",
  "(?:(?:course )?requirements|assignments)(?=\\s*[:：]?\\s*$))\\b"   # uit: "Requirements", "Assignments:"
)
# A gate line that names an exam component too ("Arbeidskrav og
# hjemmeeksamen (3 dager)", nord), or counts towards the grade
# (.graded_component_regex): it stays in assessment and is copied.
.coursework_and_exam_regex <- "(?i)arbeidskrav\\w* (?:og|\\+|samt) (?:\\w+ ){0,2}\\w*eksamen\\b"
# A component that counts towards the grade ("teller 100/100 av
# karakteren", "represents 100/100 of the grade", "Vektes 40 %",
# "Obligatorisk arbeid (40%)", usn); a gate counts 0/100, and a share of
# attendance is no weight ("Obligatorisk frammøte (80%)", "Obligatorisk
# deltakelse i laboratoriearbeid (100%)").
.graded_component_regex <- paste0(
  "(?i)(?<![\\d/])[1-9]\\d* ?/ ?100\\b|",
  "\\b(?:vekt(?:es|as)|tel(?:ler)?|counts)\\b[^.]{0,20}\\b[1-9]\\d* ?%|",
  "^(?!.*(?:deltak|deltag|møte|tilstedevær|nærvær|attendance|participation)).* \\([1-9]\\d* ?%\\)")

# A sentence that makes approval, attendance or a passed test a condition
# for the exam or the course, written in assessment without a label: "X må
# være godkjente før studenten kan framstille seg til eksamen", "Tilgang til
# slutteksamen forutsetter godkjente øvelser.", "Godkjend obligatorisk arbeid
# er ein føresetnad for å gå opp til eksamen.", "For at studenten skal kunne
# gå opp til eksamen, må obligatoriske ... arbeidskrav være godkjent.",
# "Arbeidskrav inngår ikke i karakteren, men må ... være godkjent ... for at
# emnet skal være bestått.", "The assignments have to be approved ... before
# students are allowed to sit the final exam." It names a gate (approval,
# arbeidskrav, attendance, obligatory work) and a condition (must, or
# "kreves"/"krav", ... before or in order to sit the exam, to hand in or
# present the work that is assessed, to be assessed or graded, access to the
# exam, passing the course): "For å kunne fremstille prosjektoppgaven til vurdering må
# deltakerne ha fått godkjent ...", "Det kreves 75 % oppmøte for å kunne
# fremstille seg til eksamen.", "Arbeidskrav må være godkjent for å få
# endelig karakter i emnet.", "... før ein kan få oppgåva vurdert.", "For å
# kunne fremstille seg til eksamen forutsettes det at obligatoriske aktiviteter
# er godkjent ...", "...
# before the student can submit the semester paper.", "... must be approved in
# order for a student to receive a final course assessment." (khio). Not a
# row of an exam table (tabs), nor a sentence about the exam's own parts ("Begge
# deleksamener må være bestått ...", "Mappen må være godkjent ...", "alle
# delene må være godkjent ... for å få endelig karakter", "... er deleksamen
# som må være godkjent ...", "få godkjent vurderingsmappen", a part passed
# with a grade: "The semester assignment must be considered as passed (A - E)
# in order for the students to ... the oral exam.", nla),
# nor one without a subject ("Må være godkjent for å ...", which names its
# gate in the sentence before). Losing the right to the exam is a condition
# too (nih, 2026-11-25: "... and forfeit the right to take the final
# examination (see below) if the group assignments are unsatisfactory.",
# "... mister retten til å gå opp til avsluttende eksamen dersom
# arbeidskravene ikke er tilfredsstilt."; uit, usn, ntnu: "Student som ikke
# oppfyller arbeidskrav mister retten til å framstille seg til eksamen.");
# not the right to feedback or supervision.
.gate_sentence_regex <- paste0(
  "(*UCP)(?is)^(?=[^\\t]*$)(?!.*\\b(?:(?:alle|begge) (?:del(?:er|ene|ar|ane)|\\w*eksam\\w*)|er (?:en )?deleksamen)\\b)(?!.*\\(A ?[-–] ?E\\))",
  "(?=.*\\b(?:(?:godkje?n|approv)\\w*+(?!\\W+(?:\\w+\\W+)?(?:e(?:ks|x)am|\\w*vurdering|\\w*mappe))|arbeidskrav|obligatori|compulsory|mandatory|coursework|øving|",
  "øvelse|oppmøte|frammøte|fremmøte|tilstedeværelse|nærvær|attendance|participation|assignments?\\b|",
  "homework|exercises?\\b|passing grade))",
  "(?!\\W*(?:må|skal|must)\\b)(?!\\W*(?!(?:for (?:å|at)|in order|before|to)\\b)(?:\\w+\\W+){0,4}?(?:\\w*eksam|\\w*exam|delvurdering|",
  "del(?:er|ene|ar|ane)\\b|parts?\\b|mappe|portfolio)\\w*\\W+(?:\\w+\\W+){0,6}?(?:må|skal|must|ha(?:ve|s) to)\\b)",
  "(?=.*(?:\\b(?:må|skal)\\b.{0,150}\\b(?:før|innan|innen)\\b.{0,60}(?:eksam|vurder|sluttprøv)|",
  "\\b(?:må|skal|(?:kreves|krevs|krav)(?![^\\t]*praksis))\\b.{0,150}\\bfor (?:å|at|(?:fram|frem)stilling til)\\b.{0,80}(?:eksam|vurder|\\b(?:inn)?lever|framstill|fremstill|karakter)|",
  "\\bfor (?:å|at)\\b.{0,60}\\b(?:gå opp|framstill|fremstill|avlegg|lever|vurder|få (?:\\w+ ){0,2}vurder)\\w*.{0,60},? (?:må|skal|(?:forutsett|føresett)\\w* det at)\\b|",
  "\\b(?:må|skal)\\b.{0,150}\\bfor (?:å|at) (?:bestå\\b|(?:emnet|faget) skal (?:være|vere|kunne) bestått|",
  "få (?:emnet|faget) bestått)|",
  "\\b(?:tilgang|adgang|rett|vilkår|forutsetning|føresetnad)\\b (?:til|for) (?:å )?(?:\\w+ ){0,4}?\\w*(?:eksam|prøve)|",
  "\\b(?:must|ha(?:ve|s) to|required?|need|prerequisite|condition)\\b.{0,150}\\b(?:before|prior to|",
  "in order (?:to|for (?:a|the) students? to)|allowed to|admitted to|for permission to|eligible|qualif\\w*)\\b.{0,60}\\b(?:\\w*exam|assessment|submit)|",
  "\\b(?:before|in order to)\\b.{0,60}\\bexam\\w*,? .{0,60}\\b(?:must|need|have to|has to|required?)\\b|",
  "\\b(?:forfeits?|loses?) the right to (?:take|sit)\\b.{0,40}\\bexam|",
  "\\b(?:mister|taper) (?:retten|rett) til å (?:avlegge|ta|gå opp til|fr[ae]mstille seg til)\\b.{0,30}eksam))")

# Compulsory attendance stated alone, without a condition: "Deltakelse på 80
# % av undervisning.", "70 % obligatorisk deltakelse på seminarundervisning",
# "Oppmøte undervisning: minimum 80%." (nord, uit, ntnu). The spec puts it in
# coursework requirements. Not attendance in praksis or clinical studies,
# which is how a practice course is assessed ("For å bestå praksis må
# studenten ... ha 100 % oppmøte"), unless the attendance is at teaching
# ("Som ein del av praksisen ... er det obligatorisk å delta i minst 8 av 10
# undervisningssamlingar."). With "Det er" before it, an obligation
# says it is required ("Det er obligatorisk deltakelse på samlingene.", "Det
# er krav om 90 % deltakelse på samlingene.", phs); so does "All
# undervisning er obligatorisk" ("Alle læringsaktiviteter er obligatoriske.",
# nla). Also, in a sentence of up to 250 characters
# with no negation ("Det vil ikke være noe obligatorisk oppmøte", "frivillig"),
# no exam ("obligatorisk frammøte under hele eksamen", "Eksamen blir
# gjennomført i grupper ... der deltaking ... er obligatorisk"; but
# "Eksamenskrav:" and "Forskrift om ... studium og eksamen") and
# not about the learning outcomes: an obligation right before the attendance
# ("Det er derfor obligatorisk oppmøte i undervisningen.", "Det vil derfor
# være krav om obligatorisk deltakelse ...", "Det kreves 80 % tilstedeværelse.";
# not "krever aktiv deltakelse", which describes the teaching); attendance or
# teaching that is compulsory ("Attendance to lectures and seminars is
# mandatory.", "Undervisningen er obligatorisk, og krever ...", "Deler av
# undervisningen er obligatorisk."; not "dersom emnet er obligatorisk i
# studieplanen"); a rule for absence beyond a limit ("Fravær over 20 % ...
# medfører at ...", "Dersom fravær overstiger 20 prosent, ...", "Should a
# student exceed the maximum limit for absence, ..."). Not a list
# of teaching forms with attendance in it ("Forelesninger, teoriøvinger og
# laboratoriearbeid med obligatorisk tilstedeværelse.", "Undervisning og
# læring skjer gjennom selvstudier, forelesninger, obligatorisk deltakelse i
# seminargrupper, ...", "Emnet består av totalt 40 undervisningstimer i form av
# forelesninger og seminarer med obligatorisk oppmøte, ...", fjellhaug
# 2026-11-26; not "... i form av seminarer, og det er obligatorisk oppmøte
# ..."), which describes the teaching, unless it opens with
# the requirement ("Det kreves minimum 80 % deltakelse på ekskursjoner,
# laboratoriearbeid og ...").
.teaching_form <- paste0("(?:forelesning|førelesing|lectures?|seminar|øving|øvelse|laboratori|lab\\b|gruppearbeid|",
                         "selvstudi|sjølvstudi|veiledning|rettleiing|workshops?|verksted|feltarbeid|feltundervisning|",
                         "ekskursjon|presentasjon|diskusjon|demonstrasjon|prosjekt|foredrag|undervisning|samling|",
                         "kollokvi|basisgrupp|ferdighetstrening|practical work|group work)")
.attendance_word <- "(?:\\w*(?:deltak|deltag|oppmøt|frammøt|fremmøt|tilstedevær|nærvær)\\w*|attendance|participation)"
.attendance_sentence_regex <- paste0(
  "(*UCP)(?i)^\\s*(?!(?!.*\\b(?:å delta|", .attendance_word, ")\\s+(?:i|på|ved|at|in)\\s+(?:(?!\\w*(?:praksis|placement))\\w+\\s+){0,4}?",
  "(?:undervisning|samling|seminar|forelesning|lecture|teaching)).*(?:praksis|praktiske studier|placement|internship|",
  "professional training|practical training))(?!.*(?:klini[sk]|clinical|pasient|læringsutbytt|learning outcome))",
  "(?:(?=[^\\t]{0,120}$)(?:",
  "alle? (?:undervisning|samling|seminar|forelesning|læringsaktivitet)\\w* er obligatorisk\\w*\\b[^.]{0,80}\\.?\\s*$|",
  "(?:det er (?=[^.]*(?:obligatorisk|krav))|(?=.*(?:\\d ?(?:%|prosent)|\\balle?\\b)))",
  "(?:(?:obligatorisk|mandatory|compulsory|krav (?:om|til)|minimum|minst|min\\.)\\s+)*",
  "(?:(?:\\d+|hundre) ?(?:%|prosent)\\s+(?:\\w+\\s+){0,2}?)?(?:å delta|deltakelse|deltaking|deltagelse|",
  "oppmøte|frammøte|fremmøte|tilstedeværelse|nærvær|attendance|participation)\\b[^.]{0,80}\\b(?:undervisning|",
  "seminar|forelesning|samling|teaching|lecture|class|kurs|workshop|øving|lab)\\w*[^.]{0,40}\\.?\\s*$)|",
  "(?=[^\\t]{0,250}$)(?:",
  # a rule for absence beyond a limit, also with the exam as its sanction
  "\\W*(?:(?:ved|dersom|hvis|if)\\s+)?(?:\\w+\\s+){0,2}?(?:fravær|absence)\\w*\\b[^.]{0,30}\\b",
  "(?:over|utover|overstig\\w*|overskrid\\w*|mer enn|more than|exceed\\w*|på mellom|beyond)\\b|",
  "\\W*(?:dersom|hvis|if|should)\\s+(?:\\w+\\s+){0,3}?(?:overstig|overskrid|exceed)\\w*\\b[^.]{0,40}\\b(?:fravær|absence)|",
  "(?!.*(?:\\b(?:ikke|ikkje|not|no|ingen|uten|utan|valgfri\\w*|frivillig\\w*|optional|",
  "uavhengig|anbefal\\w*|recommended)\\b|(?<!studium og )\\b\\w*(?:eksam|exam)(?!enskrav|ple)|",
  "(?<!\\bi )(?<!\\bpå )\\b(?:emnet|faget|kurset|the course|the subject) (?:er|is) (?:\\w+ )?(?:obligatorisk|compulsory|mandatory)))",
  "(?!(?!\\W*(?:det|obligatorisk|krav|mandatory|compulsory|alle?|\\d+ ?%|eksamenskrav|arbeidskrav|for å|i tillegg er det|",
  .attendance_word, ")\\b).*(?:", .teaching_form, "\\w*(?:\\s*\\([^)]*\\))?,\\s*(?:\\w+\\s+){0,3}?", .teaching_form,
  "|\\bi form av\\s+(?:\\w+\\s+){0,2}?", .teaching_form, "[^,.;]{0,60}?\\bmed (?:obligatorisk|pliktig)))(?:",
  ".*\\b(?:(?:obligatorisk\\w*|pliktig\\w*|mandatory|compulsory|krav|kreves|krevs|required)",
  "(?:\\s+(?:om|til|derfor|minimum|minst|min\\.|full|obligatorisk\\w*|mandatory|compulsory|\\d+ ?(?:%|prosent)|at least))*",
  "\\s+(?:å\\s+delta\\b|", .attendance_word, ")|",
  # "krever" with a share or "obligatorisk", or before presence: not "krev
  # deltaking og medansvar"
  "(?:krever|krev|requires?)(?:\\s+derfor)?(?:(?:\\s+(?:minimum|minst|min\\.|obligatorisk\\w*|\\d+ ?(?:%|prosent)|at least))+",
  "\\s+", .attendance_word, "|\\s+(?:oppmøt|frammøt|fremmøt|tilstedevær|nærvær|attendance)\\w*))\\b|",
  ".*(?:", .attendance_word, "|undervisning\\w*|samling\\w*|seminar\\w*|forelesning\\w*|lectures?|seminars?|classes|teaching)",
  "\\b[^.]{0,100}?\\b(?:er|is|are|blir|vil være)\\s+(?:\\w+\\s+)?(?:obligatorisk\\w*|mandatory|compulsory|påkrevd|required)\\b)))")
# A sentence that a gate label opens inside a paragraph (uit 2008: "...
# supervisor. Mandatory course work: Attendance is mandatory in all seminars
# ..."); not "Arbeidskrav: 20 timer", a workload line (uis).
.gate_label_sentence_regex <- paste0(
  "(*UCP)(?i)^\\s*(?:(?:mandatory|compulsory|obligatory) (?:course ?work|work requirements?|assignments?|",
  "activit(?:y|ies)|requirements?)|obligatoriske? (?:arbeidskrav|aktivitet(?:er)?))\\s*:\\s*\\S")
# A gate sentence whose subject points back ("Begge disse kreves godkjent
# ...", "Den må være godkjent ...", "These must be approved ..."); not a
# determiner ("Dei obligatoriske arbeidskrava må ...", which names its gate);
# or one about attendance "på disse" ("Det er krav om 80 % oppmøte på disse
# seminarene.", which names the seminars in the sentence before).
.gate_backref_regex <- paste0(
  "(*UCP)(?i)^\\W*(?:begge disse|disse|desse|dette|den|these|this|it|they)\\W+",
  "(?:må|skal|kreves|krevst|must|ha(?:ve|s) to|is required)\\b|\\b(?:på|i|til|to|on|in|at) (?:disse|desse|these)\\b")

# A hard-wrapped sentence (usn's text) is read as one line: a line that
# starts in lower case continues one that ends in neither a full stop nor a
# colon, and any line continues one that ends in "eller" or "or" ("... i
# bioingeniørfag, eller" over "Opptak til kompletterende utdanning ...",
# oslomet). Not any line after a comma: nord's "Obligatorisk deltakelse (OD):
# ... Teller 0/100 av karakteren," is followed by the graded "Praktisk eksamen
# (PE): ...". The line break stays.
.unwrap_lines <- function(lines) {
  n <- length(lines)
  cont <- c(FALSE, (grepl("^\\s*\\p{Ll}", lines[-1], perl = TRUE) &
                      grepl("[\\p{L}\\d,)]\\s*$", lines[-n], perl = TRUE)) |
              (grepl("(*UCP)(?i)\\b(?:eller|or)\\s*$", lines[-n], perl = TRUE) &
                 nzchar(trimws(lines[-1]))))
  # a point "1)" continued after one blank line (uit 2008: "1) ... om
  # obligatoriske" / blank / "skal løses ... for å gå opp til skriftlig eksamen.")
  gap <- if (n > 2) which(c(FALSE, !nzchar(trimws(lines[-c(1, n)])), FALSE) &
                 grepl("^\\s*\\d+\\)\\s.*\\p{L}\\s*$", c("", lines[-n]), perl = TRUE) &
                 grepl("^\\s*\\p{Ll}", c(lines[-1], ""), perl = TRUE))
  cont[c(gap, gap + 1)] <- TRUE
  if (!any(cont)) return(lines)
  vapply(split(lines, cumsum(!cont)), paste, character(1), collapse = "\n", USE.NAMES = FALSE)
}

# A gate line that heads a block of its own: "OBLIGATORISKE ARBEIDSKRAV",
# "Obligatoriske aktiviteter/arbeidskrav:", "Arbeidskrav" (ntnu's "Mer om
# vurdering"), "Compulsory assignments", "Coursework requirements:"; a gate
# line by itself, so not nord's "Arbeidskrav (AK):".
.coursework_heading_regex <- paste0(
  "(?i)^\\s*(?:(?:obligatoriske? \\w+(?:/\\w+)?|arbeidskrav(?:ene)?|coursework(?: requirements?)?|",
  "(?:course )?requirements|assignments|",
  "(?:compul\\w?s[ou]ry|mandatory) \\w+(?: \\w+)?)\\s*[:：]?\\s*$|",
  # khio: "The course requirements are" over lower-case items, which
  # .unwrap_lines reads as one line with it
  "the course requirements are\\s*[:：]?\\s*(?:\\n|$))")
# A line that starts the assessment proper: "Eksamen:", "Vurderingsordning
# for emnet:", "Karakter: Bokstavkarakterer", "EKSAMEN", "Exam:", "Further
# on evaluation"; not "Vurdering: Godkjent/ikke godkjent", which is the gate's
# own.
.assessment_label_regex <- paste0(
  "(?i)^\\s*(?:eksamen|vurdering|vurderingsordning|vurderingsform|eksamensform|karakter(?:er)?|",
  "exam(?:ination)?|assessment|(?:further on )?evaluation|grad(?:e|ing))\\b",
  "(?:[^:：.\\n]{0,40}[:：](?!\\s*(?:godkjent|bestått|approved|pass)\\b)|\\s*$)")
# A line under teaching methods that states the assessment form:
# "Vurderingsform: Masteroppgaven, samt 1 times muntlig eksamen ..." (ntnu
# masters), "Vurderingsinnhold: Kriterier for praksis" (oslomet); not "Vurdering: ..." or "Eksamen: ...", which also give topics,
# exam dates or workload (uis "Eksamen: 6 timer").
.design_line_regex <- "(?i)^\\s*(?:vurderings|eksamens)(?:form|innhold)\\s*[:：]\\s*\\S"

# Move coursework-gate lines from assessment into coursework_requirements. A
# gate heading (ntnu) takes the lines under it, and the paragraphs after it
# that are list items or still speak of a gate, up to a line about the exam;
# over a colon-ended lead-in ("Arbeidskrav:", "Følgende arbeidskrav ...:",
# uit) it is a label like any other.
# Any other gate line that is a label takes what it names along. A colon-ended lead-in
# ("Følgende arbeidskrav må være godkjent ...:", uit) takes the rest of its
# paragraph, or the next paragraph when it ends its own. A label without one
# ("Obligatorisk deltakelse (OD)", nord) takes the next paragraph unless that
# is a label too ("Eksamen"), and the paragraphs after it that still speak of
# a gate ("Det er derfor obligatorisk oppmøte ..."). Both stop at the next
# assessment component ("Hjemmeeksamen (HJ): ...", "teller 100/100"). A gate
# line that also names an exam component stays in assessment as well.
# A gate sentence (.gate_sentence_regex, or compulsory attendance stated
# alone, .attendance_sentence_regex) is a gate line when it is the whole
# line, and moves by itself from a line with other sentences. Without
# `labels` (institutions without `section_inline_coursework`) only gate
# sentences move. `section`: teaching_methods and course_content are read
# too (labels = "heading"): their gate sentences, gate headings with their
# block, and nord's "(OD)"/"(OA)"/"(AK)" lines move, since a requirement
# belongs in coursework requirements whatever heading the page puts it under;
# a line under teaching methods that states the assessment form
# (.design_line_regex) moves to assessment, with the exam labels right under it.
.split_inline_coursework <- function(out, labels = TRUE, section = "assessment") {
  i <- which(out$section == section)
  if (length(i) != 1) return(out)
  lines <- .unwrap_lines(stringr::str_split_1(out$raw_text[i], "\n"))
  is_gate <- function(x) {
    if (isTRUE(labels)) return(grepl(.inline_coursework_regex, x, perl = TRUE))
    # under teaching or content only a gate heading, not "Assignments" or
    # "Requirements" (ntnu lists "Lectures / Assignments / Project work"),
    # and a line naming nord's abbreviation ("Obligatorisk deltakelse (OD) på
    # laboratorieøvelser")
    identical(labels, "heading") &
      ((grepl(.coursework_heading_regex, x, perl = TRUE) &
          !grepl("(?i)^\\s*(?:(?:course )?requirements|assignments)\\b", x, perl = TRUE)) |
         grepl("\\((?:OD|OA|AK)\\d?\\)", x, perl = TRUE))
  }
  gate <- is_gate(lines)
  # "Arbeidskrav: ... Vurdering: ..." on one line (uit master theses)
  lines <- unlist(lapply(seq_along(lines), function(k) if (gate[k] && section == "assessment")
    stringr::str_split_1(lines[k], .assessment_midline_regex) else lines[k]))
  sents <- lapply(lines, function(l) if (grepl("\t", l)) l else stringr::str_split_1(l, .sentence_end))
  sg <- lapply(sents, function(s) {
    g <- grepl(.gate_sentence_regex, s, perl = TRUE) | grepl(.attendance_sentence_regex, s, perl = TRUE) |
      grepl(.gate_label_sentence_regex, s, perl = TRUE)
    # "Det blir gitt 2 øvinger. Begge disse kreves godkjent for adgang til
    # eksamen." (nord): a gate that points back takes the sentence it names
    b <- which(g & grepl(.gate_backref_regex, s, perl = TRUE))
    g[b[b > 1] - 1] <- TRUE
    g
  })
  whole <- vapply(sg, all, logical(1))
  gate <- is_gate(lines) | whole
  part <- !gate & vapply(sg, any, logical(1))
  design <- section == "teaching_methods" & grepl(.design_line_regex, lines, perl = TRUE)
  for (k in which(design)) {   # "Karakter: Bokstavkarakter (A-F)" under it
    m <- k + seq_len(length(lines) - k)
    m <- m[cumprod(grepl(.assessment_label_regex, lines[m], perl = TRUE) &
                     grepl("[:：]\\s*\\S", lines[m])) == 1]
    design[m] <- TRUE
  }
  if (!any(gate | part | design)) return(out)
  orig <- lines
  aside <- character(length(lines))   # gate sentences of a line that stays
  for (k in which(part)) {
    s <- sents[[k]]
    keep <- !sg[[k]] | grepl(.graded_component_regex, s, perl = TRUE)
    aside[k] <- trimws(paste(s[sg[[k]]], collapse = ""))
    lines[k] <- paste(s[keep], collapse = "")
    if (!keep[1]) lines[k] <- trimws(lines[k], "left")
  }
  blank <- !nzchar(trimws(lines))
  label <- .is_label(lines)
  colon <- grepl("[:：]\\s*$", lines)
  graded <- grepl(.graded_component_regex, lines, perl = TRUE)
  stop <- !gate & (grepl(.component_label_regex, lines, perl = TRUE) |
                     grepl(.assessment_label_regex, lines, perl = TRUE) | graded)
  both <- gate & (graded | grepl(.coursework_and_exam_regex, lines, perl = TRUE))
  n <- length(lines)
  # a gate sentence that ends in a colon heads a block too ("The following
  # coursework requirements must be approved in order to take the exam:" over
  # list items separated by blank lines, uit)
  heading <- gate & (grepl(.coursework_heading_regex, lines, perl = TRUE) |
                       (whole & colon))
  # where a gate heading's block ends: a paragraph (not a list item) that
  # does not speak of the gate, a heading naming other information, or a line
  # about the exam (in its first words) or the thesis ("Masteroppgaven skrives
  # normalt i monografiform.", which ntnu writes after the arbeidskrav rules
  # without a blank line) that does not speak of the gate; no gate label
  # takes a line about the exam ("Eksamen og vurdering")
  item <- grepl("^\\s*(?:[-•*o]\\s|\\d+[.)])", lines)
  talk <- grepl(paste0("(?i)arbeidskrav|obligatori|godkjen|kompensatorisk|øving(?!s)|",
                       "fravær|oppmøte|tilstedeværelse|coursework|compulsory|mandatory|",
                       "approved|attendance|absence"), orig, perl = TRUE)
  exam <- !gate & !talk & !blank & !grepl("^\\s*[-•*o]\\s", lines) & grepl(paste0(
    "(*UCP)(?i)^\\W*(?:(?:master|bachelor)oppg[aå]v|(?:\\w+\\W+){0,5}(?:\\w*e(?:ks|x)am|",
    "(?<!\\w)(?:(?:del)?vurdering|evaluering|karakter|assessment|evaluation|grad(?:e|ing))(?:en|er|ene)?\\b))"),
    lines, perl = TRUE)
  ends <- exam | (!gate & !talk & !blank & !item &
                    (c(TRUE, blank[-n]) | (label & grepl("(?i)informasjon", lines))))
  block <- logical(n)
  for (k in which(gate & (label | colon) & !both & (heading | isTRUE(labels)))) {
    from <- which(!blank & seq_len(n) > k)[1]
    if (block[k] || is.na(from) || stop[from] ||
        (exam[from] && label[from] && grepl("^\\s*\\p{Lu}", lines[from], perl = TRUE))) next
    if (heading[k] && !(gate[from] && colon[from])) {
      to <- which((stop | ends | heading) & seq_len(n) > from)[1]   # or the next gate heading, which takes its own
      block[from:(if (is.na(to)) n else to - 1)] <- TRUE
      gate <- gate | block
      next
    }
    if (!colon[k] && label[from]) next
    repeat {
      # a label under a colon-ended lead ends it ("Midtveisvurdering:" after
      # "Arbeidskrav som må godkjennes for å starte i praksis:" over "- PL1",
      # dmmh)
      to <- which((blank | stop | ends | (colon[k] & colon & label & !gate & !item)) & seq_len(n) > from)[1]
      gate[from:(if (is.na(to)) n else to - 1)] <- TRUE
      if (colon[k] || is.na(to)) break
      from <- which(!blank & seq_len(n) > to)[1]
      if (is.na(from) || stop[from] || label[from] ||
          !grepl(.coursework_leadin, lines[from], perl = TRUE)) break
    }
  }
  # rows of codes under a gate, across blank lines (dmmh: "PEK1: K1-DR1 og
  # K1-DR2", "OPG1: F-TF1 og F-TF4" under "Arbeidskrav som må godkjennes for å
  # gå opp til eksamen:", "F-TF1 er ekvivalent med AK 1" under "Arbeidskrav
  # som er ekvivalent med tidligere arbeidskrav:"), join it
  if (isTRUE(labels)) for (k in which(grepl(.coursework_code_row_regex, lines, perl = TRUE))) {
    p <- utils::tail(which(!blank & seq_len(n) < k), 1)
    if (length(p) && gate[p]) gate[k] <- TRUE
  }
  gate[blank] <- FALSE
  design <- design & !gate
  lines[gate] <- orig[gate]
  out$raw_text[i] <- paste(lines[(!gate | both) & !design], collapse = "\n")
  out <- .append_section(out, "coursework_requirements",
                         paste(ifelse(gate, lines, aside)[gate | nzchar(aside)], collapse = "\n"))
  .append_section(out, "assessment", paste(lines[design], collapse = "\n"))
}

# A line of coursework codes (dmmh): codes, or an exam part's code and the
# codes of the coursework it requires ("PEK3: K1-MU, K1-TF2 og OBLTS",
# "- PL1"), or a code's equivalents in an earlier version ("K1-MU1 og K1-MU2
# ekvivalent til K1-MU", "F-TF1 er ekvivalent med AK 1", "Ingen ekvivalente
# til ..."). A code has a digit or three capitals: not a grade scale "A-F".
.coursework_code <- "(?=[A-ZÆØÅ0-9-]*(?:\\d|[A-ZÆØÅ]{3}))[A-ZÆØÅ][A-ZÆØÅ0-9]*(?:-[A-ZÆØÅ0-9]+)*"
.coursework_code_row_regex <- local({
  codes <- paste0(.coursework_code, "(?:\\s*(?:,|og|and|&)\\s*", .coursework_code, ")*")
  paste0("^\\s*(?:[-•]\\s*)?(?:", codes, "(?:\\s*[:：]\\s*", codes, ")?\\s*$|",
         .coursework_code, "\\b.*\\bekvivalent|(?i:ingen ekvivalent))")
})

# Add text to a section, after a blank line, or as a new row. A line of 20
# or more characters that the section already has, also without its label,
# is not added again: a requirement the page states under the teaching and
# again in the exam description (aho: "Det er krav om tilstedeværelse på
# delgjennomganger og presentasjoner.", "Oppmøte: Kurset er praksisbasert
# ..."; 2026-11-22).
.append_section <- function(out, section, text) {
  j <- which(out$section == section)
  if (length(j) == 1 && nzchar(text)) {
    had <- stringr::str_squish(stringr::str_split_1(out$raw_text[j], "\n"))
    lines <- stringr::str_split_1(text, "\n")
    sq <- stringr::str_squish(lines)
    dup <- nchar(sq) >= 20 & (sq %in% had | sub("^[^:]{1,30}:\\s*", "", sq) %in% had)
    if (any(dup)) text <- trimws(gsub("\n{3,}", "\n\n", paste(lines[!dup], collapse = "\n")))
  }
  if (!nzchar(text)) return(out)
  if (length(j) == 1) {
    out$raw_text[j] <- paste(out$raw_text[j], text, sep = "\n\n")
  } else {
    out <- dplyr::bind_rows(out, tibble::tibble(section = section, raw_text = text))
  }
  out
}

# Sentences that the page writes under one section but that belong to
# another (course_plan_spec.md), per institution: each `from` section's
# sentences matching `rx` join the `to` section. phs: the feedback on the
# arbeidskrav, written under "Vurdering" before the exam ("Studentene blir
# vurdert underveis gjennom arbeidskrav, og får tilbakemeldinger på disse
# ...", "Noen av tilbakemeldingene vil gjennomføres som en
# utviklingssamtale"), is about the coursework requirements, not the
# assessment; "Deler av mappen vil være gjenstand for en avsluttende
# vurdering." under teaching says what is assessed. The continuous assessment
# of the dog-handler courses ("Studenten/ekvipasjen blir vurdert gjennom hele
# utdanningen ...") is their assessment and stays. uit: the law programme's
# progression rules, under the assessment or the course content ("Før
# studentene kan begynne på 4. avdeling, må de ha bestått 3. avdeling.",
# "Ex. fac. (JUR-0500) må være bestått før studenten får avlegge avsluttende
# eksamen på avdelingen."), are prerequisites; admission and study right in
# the course content ("Emnet kan ikke tas som enkeltemne.", "Emnet er
# forbeholdt studenter med studierett ved ...", "This course may be taken as
# a single course by students who meet the admission requirements ...") go
# (".drop", .uit_admission_sentence()). hivolda:
# the Didaktikk 2 courses open their coursework requirements with the passed
# course before them ("Bestått didaktikkemne 1."), a prerequisite. fhs and nmh: a
# sentence in the course content that says only which courses it builds on
# ("Emnet bygger på ING1502 Matematiske metoder 1.", "The course is based on
# ING1501 ...", "Emnet bygger på Hovedinstrument I (UTHO13).", nmh's codes
# with two digits) names prerequisites; one that goes on about the course ("...
# og skal gi studentene ...") or names no course code stays. hvl: the
# engineering theses and projects say in their content how the work is done
# ("Arbeidet utføres normalt i grupper og kan være et samarbeid med en
# bedrift ..."), teaching. usn: the PPU courses grade the practice in their
# teaching ("Praksis vurderes til bestått/ikke bestått."), assessment; a
# grade with a condition ("... til godkjent dersom ... fravær ikke
# overstiger 10 %") stays. nla: the COVID notice in the course content
# ("Grunnet Covid-19 situasjoen kan deler av (evt. hele) undervisningen bli
# gjennomført digitalt. Dette vil bli spesifisert i undervisningsplanen for
# emnet."), teaching. hiof: the plans of 2006-2015 give the teaching hours
# under "Undervisningssemester" (`section_headings` ".drop"), on lines of
# their own: a line that opens with hours or a teaching form and names one
# ("4 timer forelesning + øvinger pr. uke.", "Timer pr. uke: 4 t plenum",
# "Veiledning og forelesninger etter behov.", "Veiledningsomfanget utgjør 20
# timer."), teaching; the semester, the workload ("Minimum 12 timer pr.
# uke.", "Timer pr. uke: 4") and a line that opens with a semester label
# ("Vår 2006/ ett semester. ... 4 timer forelesning per uke", 2005-2007) stay
# out. A wording found in one or two courses stays (MF, UiS
# MGL2120, INN SOSP3001/3002; course_plan_spec.md, known deviations).
# Whether each sentence says only who may take the course (uit's course
# content): every clause of it (cut at ", og", " men er", ". ", " (E" ...)
# opens with the course or the verb and says who may take it ("Emnet er
# forbeholdt studenter med studierett ved ... og kan ikke tas som
# enkeltemne.", "Kan ikke tas som enkeltemne."), and it names no
# prerequisite (.is_admission_sentence()). A sentence that also says
# something about the course stays whole: "Emnet utgjør praksisdelen i
# utdanningen og kan ikke tas som enkeltemne.", "Theoretical. (Åpen for UiT
# sine ph.d.-studenter ...", "Masteroppgaven er et selvstendig arbeid
# forbeholdt studenter ...", "Oppgaven er det største enkeltemnet ...".
.uit_admission_sentence <- function(s) {
  who <- paste0(
    "(?i)^\\W*(?:(?:(?:eksamens)?emnet|emne|det|kurset|this course|the course|it)\\s+)?",
    "(?:(?:er|is|kan|may|can|ikke|ikkje|også|derfor|dermed|kun|bare|only|not|also|be|etter søknad|",
    "under visse vilkår)\\s+)*",
    "(?:(?:tas|takast|be taken) (?:som|as) (?:et |eit |a )?(?:master)?(?:enkelt ?emne|single|singular)|",
    "forbehold\\w* (?:for )?\\w*student|tilgjengelig som enkeltemne|tas av enkeltemnestudent)")
  vapply(s, function(x) {
    cl <- stringr::str_split_1(trimws(x), paste0(
      "(?i)(?:,\\s*(?:og|men|but|and)\\s+|\\s+(?:og|men|but|and)\\s+(?=(?:emnet\\s+)?(?:kan|er|is|can|may|skal|vil|",
      "utgjør|gir|passer|inngår|tilbys|undervises|velges|gis|du|man|studenten\\w*)\\b)|\\.\\s+|",
      "\\s+\\((?=\\p{Lu}))"))
    all(grepl(who, cl, perl = TRUE)) && .is_admission_sentence(x)
  }, logical(1), USE.NAMES = FALSE)
}
# uit, more sentences that the course content or the 2008-2011 "Mål"
# (learning_outcomes) hold but that belong elsewhere (2026-11-16): the prior
# knowledge the course requires ("Det forutsettes grunnleggende kunnskaper i
# kjemi, biokjemi og mikrobiologi.", "Undervisningen forutsetter
# forkunnskaper tilsvarende KVE-1006 Praktisk kvensk 1.", "Emnet kan tas som
# enkeltemne, men det forutsettes ...") goes to prerequisites, not a bare
# "none" ("Det forutsettes ingen forkunnskaper utover generell
# studiekompetanse ...") unless it recommends something; the intake capacity
# ("Det forutsettes opptak av minimum 10 studenter ...", "Det tas opp
# maksimum 22 studenter.", "Det er 30-40 studieplasser på kurset.") and the
# programme placement that is all a "Mål" sentence says ("Valgfritt
# masteremne.", "Kan blant annet brukes som støtteemne i historie og
# religionsvitenskap", "Emnet er obligatorisk i bachelorstudiet i
# historie.") go: admission and programme lists, plan text in no section.
.uit_prior_knowledge <- function(s) {
  grepl(paste0("(?i)^\\W*(?:emnet kan tas som enkeltemne, men )?(?:det|emnet|undervisningen|kurset) ",
               "forutsett(?:es|er)\\b(?=[^.]{0,40}(?:kunnskap|ferdighet))"), s, perl = TRUE) &
    (!grepl("(?i)\\b(?:ikke|ingen)\\b", s, perl = TRUE) | grepl("(?i)fordel|anbefal", s, perl = TRUE))
}
.uit_capacity_regex <- paste0(
  "(?i)^\\W*(?:det forutsettes opptak av minimum|det tas opp (?:inntil|maksimum|maks\\.?|minimum) \\S+ (?:studenter|deltakere)|",
  "(?:det er|(?:studiet|emnet|kurset|dagkurset|kveldskurset) (?:er lukket og )?har)(?: totalt)? \\d+(?:\\s*[-–]\\s*\\d+)? (?:studie)?plasser|",
  "(?:det er (?:et )?)?begrenset antall (?:studie)?plasser|maks(?:imum|\\.)? \\d+ (?:studenter|deltakere)|",
  "på grunn av kapasitet|studentantall på emne|om \\w+ er det totalt \\d+ plasser|",
  "emnet (?:arrangeres|tilbys|starter|gjennomføres)\\b[^.]{0,30}\\b(?:dersom|om|hvis|forbehold om at) (?:det er )?minimum \\S+ studenter)")
.uit_placement_regex <- paste0(
  "(?i)^\\W*(?![^.]*\\b(?:og (?:er|skal|gir|vil)|videreføring|men)\\b)(?:",
  "emnet (?:er|inngår)(?: \\S+){0,3}? (?:obligatorisk|valgfri\\w*|valfri\\w*|valgfag|valgemne|valemne)\\b|",
  "obligatorisk (?:emne|del av|for|hvis|i)\\b|valgfritt(?: emne| masteremne)?\\b|",
  "(?:emnet )?kan (?:\\S+ ){0,2}brukes som støtteemne|emnet inngår (?:som obligatorisk|i mastergradsprogram))")
.builds_on_move <- list(from = "course_content", to = "prerequisites", rx = paste0(
  "^\\s*(?i:(?:dette )?emnet bygger (?:videre )?på|the course is based on) ",
  "(?=[^.]*\\b[A-ZÆØÅ]{2,}\\d{2,})(?![^.]*\\b(?:og|and) (?:skal|vil|tar|gir|legger|søker|ser|introduserer|aims?|will)\\b)",
  "(?!.*,\\s*$)"))
# aho's coursework gates, written in the teaching text or in the exam
# description (2026-11-22, 2026-11-23): a sentence about attendance or
# submission that states a requirement ("Students are required to attend
# weekly seminars.", "Final reviews are mandatory", "Obligatorisk
# innleveringer av ...", "Oppmøte påkrevd: Ja", "the seminar requires
# full-day attendance"), a consequence ("Alle oppgaver må være levert for å
# bli sensurert i kurset.", "The student must submit all assignments in the
# course to receive final assessment.", "All deliveries/presentations ...
# must be completed in order to be assessed.", "Kurset er praksisbasert der
# høyt oppmøte ... er en forutsetning for gjennomføring av kurset.") or an
# attendance threshold ("det forventes 80 % oppmøte på forelesninger"). A
# bare expectation stays ("Fysisk oppmøte til undervisning ... er forventet
# og nødvendig for å oppfylle læringsutbytte.", "Attendance is expected.",
# "it requires a high degree of studio work and presence"), and so does the
# assessment design: what is assessed and how ("assessed based on ... two
# mandatory coursework assignments"), the final work, the parts of the
# portfolio and its deliverables ("Et avsluttende obligatorisk essay ... må
# leveres.", "Alle prosjektoppgavene må leveres.", "Deliveries from all
# three phases are required."), deadlines and the exam system ("Du må gjøre
# deg kjent med ... frist for innlevering av besvarelse."), and a lead-in
# ending in a colon or naming "the following" (the stock "In order to be
# assessed in this course, the student must have completed the following
# activities and assignments:", which most pages fill with the exam).
.aho_gate_sentence <- function(s) {
  attend <- "(*UCP)(?i)oppmøte|tilstedeværelse|deltakelse|deltagelse|\\bdelta på|attend|presence|participat|\\breviews?\\b"
  submit <- paste0("(*UCP)(?i)\\bsubmi(?:t|ssion)|\\binnlevering|\\b(?:levert|leveres|levere|lever)\\b|",
                   "\\b(?:assignments?|oppgaver)\\b.{0,40}\\b(?:approved|completed|godkjent)")
  must <- paste0("(*UCP)(?i)obligatori|mandatory|compul|compl\\w*sory|\\brequir|påkrevd|\\bkrav\\b|\\bmå\\b|\\bmust\\b|",
                 "prerequisite|forutsetning|for å bli (?:sensurert|vurdert)|to receive (?:a )?final assessment|",
                 "in order to be assessed|to pass the course|for å bestå")
  design <- paste0("(*UCP)(?i)consists of|består av|\\b(?:assessed|evaluated) (?:as|on|based)|vektlegg|bedøm|karakter|",
                   "avsluttende|\\bfinal (?:portfolio|paper|project|submission|delivery|exhibition)|deliverables|",
                   "prosjektoppgavene|alle moduler|the following|a high degree|frist|kursslutt|deadline|besvarelse|[:：]\\s*$")
  att <- grepl(attend, s, perl = TRUE)
  (att | grepl(submit, s, perl = TRUE) | grepl("\\bin order to be assessed\\b", s, perl = TRUE)) &
    !grepl(design, s, perl = TRUE) &
    (grepl(must, s, perl = TRUE) | (att & grepl("\\d ?%", s, perl = TRUE)))
}
.sentence_moves <- list(
  phs = list(
    list(from = "assessment", to = "coursework_requirements", rx = paste0(
      "(?i)\\bvurdert underveis gjennom arbeidskrav|^\\s*arbeidskravene blir vurdert underveis|",
      "^\\s*det gis tilbakemeldinger? på disse\\b|tilbakemeldingene (?:vil )?gjennomføres som en utviklingssamtale")),
    list(from = "teaching_methods", to = "assessment",
         rx = "(?i)^\\s*deler av mappen (?:vil )?(?:være|er) gjenstand for (?:en )?avsluttende vurdering")
  ),
  uit = c(lapply(c("assessment", "course_content"), function(from) list(
    from = from, to = "prerequisites", rx = paste0(
      "(?i)^\\s*(?:før studentene kan begynne på \\d\\. avdeling, må de ha (?:fullført og )?bestått|",
      "ex\\. ?fac\\.? \\(jur-0500\\) må være bestått)"))),
    list(list(from = "course_content", to = "prerequisites", rx = function(s) .uit_prior_knowledge(s)),
         list(from = "course_content", to = ".drop", rx = function(s) .uit_admission_sentence(s)),
         list(from = "learning_outcomes", to = ".drop", rx = .uit_placement_regex)),
    lapply(c("course_content", "teaching_methods"), function(from)
      list(from = from, to = ".drop", rx = .uit_capacity_regex))),
  fhs = list(.builds_on_move),
  nmh = list(.builds_on_move),
  # nhh: how long approved coursework counts, a stock sentence under
  # "Assessment" too ("Compulsory activities (work requirements) is valid for
  # one semester after the semester it was obtained."; once in coursework
  # when the page says it under both; 2026-11-26)
  nhh = list(list(from = "assessment", to = "coursework_requirements",
                  rx = "^\\W*Compulsory activities \\(work requirements\\) (?:is|are) valid for one semester after\\b")),
  hivolda = list(list(from = "coursework_requirements", to = "prerequisites",
                      rx = "(?i)^\\s*bestått didaktikkemne \\d\\.?\\s*$")),
  hvl = list(list(from = "course_content", to = "teaching_methods",
                  rx = "(?i)^\\s*arbeidet utføres normalt i grupper\\b")),
  # dmmh: the practice courses' "Forkunnskapskrav" (2026-11-21): attendance
  # as the condition for a placement ("All undervisning i kunnskapsområdene
  # bidrar til studentens forberedelse til praksis, så tilstedeværelse i
  # undervisningen er en forutsetning ...", "Mangelfull tilstedeværelse i
  # undervisningen kan gjøre det nødvendig å foreta en skjønnsmessig vurdering
  # av om studenten kan tilbys praksisplass.") is a coursework requirement;
  # the transfer rule ("Ved overflytting fra annet studieprogram vil det bli
  # foretatt en særskilt faglig vurdering ...") is admission, in no section
  dmmh = list(list(from = "prerequisites", to = "coursework_requirements", rx = paste0(
                "^\\W*(?:All undervisning i kunnskapsområdene bidrar til studentens forberedelse til praksis|",
                "Mangelfull tilstedeværelse i undervisningen kan gjøre det nødvendig)")),
              list(from = "prerequisites", to = ".drop",
                   rx = "^\\W*Ved overflytting fra annet studieprogram vil det bli foretatt en særskilt faglig vurdering")),
  # barrattdue: how long completed teaching and coursework count, written
  # under "Organisering og arbeidsformer" ("Gjennomført undervisning og
  # arbeidskrav er gyldig inntil to semestre etter ordinær eksamen.")
  barrattdue = list(list(from = "teaching_methods", to = "coursework_requirements",
                         rx = "^\\W*Gjennomført undervisning og arbeidskrav er gyldige?\\b"),
                    # DID's obligatory course in a sub-course's topics ("Obligatorisk
                    # kurs: instrumentalteknikk og kroppsbevissthet", "Obligatorisk kurs
                    # i ..."), as its list under "Obligatorisk kurs:" (2026-11-22)
                    list(from = "course_content", to = "coursework_requirements",
                         rx = "^\\W*Obligatorisk kurs(?:\\s*:|\\s+i)\\s+\\S"),
                    # the elective chamber-music courses' admission rule ("Opptak i emnet
                    # forutsetter at kriterier som besetning og repertoar oppfylles på
                    # bakgrunn av søkermasse og tilgjengelig undervisningsressurs.",
                    # KAMFOR5100 and KAMP5100) is admission, in no section (2026-11-23)
                    list(from = "course_content", to = ".drop",
                         rx = "^\\W*Opptak i emnet forutsetter at kriterier som besetning og repertoar")),
  # aho: a gate written in the teaching text or the exam description
  # (.aho_gate_sentence()); an "Oppmøte"/"Attendance:" label goes with the
  # paragraph under it when all of that moves
  aho = lapply(c("assessment", "teaching_methods"), function(from) list(
    from = from, to = "coursework_requirements", rx = function(s) .aho_gate_sentence(s),
    label = "(?i)^\\s*(?:oppmøte|attend[ae]nce)\\s*:?\\s*$")),
  usn = list(list(from = "teaching_methods", to = "assessment",
                  rx = "(?i)^\\s*praksis vurderes (?:til|med|som) (?:bestått|godkjent) ?/ ?ikke (?:bestått|godkjent)\\.?\\s*$")),
  nla = list(list(from = "course_content", to = "teaching_methods", rx = paste0(
    "(?i)^\\s*(?:grunnet covid-19 situasjo|dette vil bli spesifisert i undervisningsplanen for emnet\\b)"))),
  hiof = list(list(from = ".drop", to = "teaching_methods", rx = paste0(
    "(?i)^(?=.*(?:forelesn|øving|øvelse|undervisning|veiledning|lab|seminar|workshop|plenum|oppgaveløs))",
    "\\s*(?:timer (?:pr\\.?|per) uke\\s*:?\\s*)?(?:(?:inntil|opptil|ca\\.?|i gjennomsnitt|minimum|ingen faste)\\s+)*",
    "(?:(?:\\d+|to|tre|fire|seks)(?:\\s*[-–x]\\s*\\d+)?\\s*(?:t|timer|timers|time|h)\\b|forelesn|veiledning)")))
)
.move_sentences <- function(out, institution) {
  for (m in .sentence_moves[[institution %||% ""]]) {
    i <- which(out$section == m$from)
    if (length(i) != 1) next
    x <- .strip_sentences(out$raw_text[i], m$rx, taken = TRUE, label = m$label)
    if (!length(x$taken)) next
    out$raw_text[i] <- x$rest
    # the placeholder that ends the target ("Ingen." from nla's Praksis part)
    # goes before the sentences follow it, as it would at the end
    j <- which(out$section == m$to & m$to != "assessment")
    if (length(j) == 1) out$raw_text[j] <- .strip_placeholder_lines(out$raw_text[j])
    out <- .append_section(out, m$to, paste(x$taken, collapse = "\n"))
  }
  out
}

# Lines under the learning-outcomes heading before the first group label
# ("Kunnskap") that describe the subject, not the outcomes, go to
# course_content (phs's instructor courses: "Pedagogikk i
# instruktørutdanningen vektlegger ...", "Fagdidaktikken binder faget og
# pedagogikken sammen."); a lead-in ("Etter gjennomført emne har studenten
# følgende læringsutbytte:") or a paragraph about the outcomes stays.
.lo_intro_content <- "phs"
.split_lo_intro <- function(out, institution) {
  i <- which(out$section == "learning_outcomes")
  if (!isTRUE(institution %in% .lo_intro_content) || length(i) != 1) return(out)
  lines <- stringr::str_split_1(out$raw_text[i], "\n")
  group <- which(grepl(.lo_group_label, tolower(sub("[:：]\\s*$", "", trimws(lines))), perl = TRUE))[1]
  if (is.na(group) || group == 1) return(out)
  intro <- seq_len(group - 1)
  move <- intro[!grepl("(?i)læringsutbytte|learning outcome|[:：]\\s*$", lines[intro], perl = TRUE) &
                  nzchar(trimws(lines[intro]))]
  if (!length(move)) return(out)
  out$raw_text[i] <- trimws(stringr::str_replace_all(paste(lines[-move], collapse = "\n"), "\n[ \t]*(?:\n[ \t]*)+\n", "\n\n"))
  .append_section(out, "course_content", paste(lines[move], collapse = "\n\n"))
}

# A line that names what follows rather than saying it: ends with a colon, or
# is short and does not end a sentence. "Arbeidskrav 3: credo" says it.
.is_label <- function(lines) {
  x <- trimws(lines)
  nzchar(x) & !grepl("[:：].*[^:：\\s]", x) &
    (grepl("[:：]$", x) | (nchar(x) <= 60 & !grepl("[.!?]$", x)))
}

# An assessment component named by its abbreviation (nord): "Hjemmeeksamen
# (HJ): 6 timer", "Mappe (MA) bestående av ...".
.component_label_regex <- "^\\s*[^:\\n]{1,60}\\([A-ZÆØÅ]{1,4}\\d?\\)"
# Where the assessment starts inside a gate line; not "Vurdering:
# Godkjent/ikke godkjent", which is the gate's own.
.assessment_midline_regex <- paste0(
  "(?<=[.!?]) ?(?=(?:Vurdering(?:sform)?|Eksamen(?:/vurderingsform)?|",
  "Eksamensform):[ \t]*(?!(?i:godkjent|bestått)\\b)\\S|EKSAMEN\\b|Beskriv\\w+ av eksamen:|",
  "\\d\\) (?:Skriftlig |Muntlig )?eksamen\\b|",
  "Emnet (?:evalueres|vurderes)\\b|Assessment(?: method)?:(?![ \t]*(?i:approved|pass)\\b))|",
  "(?<=\\S) (?=Eksamen består av)")

# The exam registration in ntnu's exam tab (`exam_sessions` in the config):
# "Vurderingsordning: ..." and "Karakter: ...", then a block per exam session
# ("Ordinær eksamen - Høst 2022", "Utsatt eksamen - Vår 2023"; without the
# semester once anonymized) that lists the exam parts with their facts. The
# sessions repeat the parts, so the assessment design is the parts of one
# session: in each assessment form the first ordinary session, else the
# first. Session headings and the exam system are logistics and go. Used for
# the assessment section and for the plan id (deduplicate_plans()).
.exam_session_regex <- paste0("(?i)^\\s*(?:(?:Ordinær|Utsatt|Ny|Kontinuasjons) ?eksamen|",
                              "(?:Ordinary|Re-sit) examination)(?: - .*)?$")

.exam_tab_design <- function(text) {
  if (is.na(text)) return(text)
  lines <- stringr::str_split_1(text, "\n")
  sess <- grepl(.exam_session_regex, lines, perl = TRUE)
  if (!any(sess)) return(text)
  form <- cumsum(grepl("(?i)^\\s*(?:Vurderingsordning|Examination arrangement):", lines, perl = TRUE))
  # the session heading a line is under; a new assessment form ends it
  head <- cummax(ifelse(sess, seq_along(lines), 0L))
  head[head > 0L & form[pmax(head, 1L)] != form] <- 0L
  ordinary <- sess & grepl("(?i)^\\s*Ordin(?:ær|ary)", lines, perl = TRUE)
  kept <- vapply(split(which(sess), form[sess]), function(h) {
    o <- h[ordinary[h]]
    if (length(o)) o[1] else h[1]
  }, integer(1))
  part <- head %in% kept & !sess
  # an exam part's "(1)" points to a session comment, which is not read
  lines[part] <- sub("[ \t]*\\(\\d{1,2}\\)[ \t]*$", "",
                     sub("(?i)[ \t]*(?:Eksamenssystem|Exam system)\\b.*$", "", lines[part], perl = TRUE))
  paste(lines[head == 0L | part], collapse = "\n")
}

# Page widgets, pointers and exam logistics with no heading of their own, so
# .drop cannot catch them. Applied to assessment and coursework_requirements
# only. Notices that say something about the assessment (AI use, plagiarism,
# COVID changes of the exam form) and exam tables with their header row are
# assessment text and stay (course_plan_spec.md). "all" applies to every
# institution; other entries per institution. Most patterns remove whole
# lines (or, with (?s), a trailing block).
.section_noise <- list(
  all = c(
    # stock resit sentence in running text (oslomet)
    paste0("(?i)(?:Deleksamen \\d:[ \t]*)?Ny(?:/| og | eller )ut(?:satt|sett) ",
           "eksamen (?:arrangeres|gjennomføres|foregår|blir arrangert|",
           "blir gjennomført|vert arrangert|vert gjennomført) ",
           "(?:som (?:ved )?)?ordinær(?:e)? eksamen\\.?[ \t]*"),
    "(?m)^Se under [^.\n]{0,40}\\.?$",                           # oslomet pointer
    # where the resit rights are and that the student registers (oslomet, hiof)
    paste0("[ \t]*(?:Studentens (?:retter|rettigheter) og plikter ved ny/utsatt eksamen ",
           "fr[ae]mgår av [^\n]*?\\.(?=\\s|$))?[ \t]*Studente[rn] er selv ansvarlig\\w* for å ",
           "melde seg opp til eventuell ny/utsatt(?: eksamen)?(?: på Student[Ww]eb)?\\.?[ \t]*"),
    # a dated change-log entry: "13.03.2023: Eksamensform våren 2023 er
    # endret etter vurdering av ...", "26.09.2018: Endret arbeidskrav" (hiof)
    "(?m)^[ \t]*\\d{1,2}\\.\\d{1,2}\\.(?:\\d{4}|\\d{2})[ \t]*(?::|[-–][ \t])[^\n]*(?:\n|$)"
  ),
  uib = c(
    "(?s)\n?Dette bør du vite om eksamen.*$",                 # footer
    "(?m)^Vi opplever problemer med å hente inn eksamensinformasjon.*\n(?:Lukk\n.*)?",
    "(?m)^Til toppen$"
  ),
  # the pointer to the exam regulations after the permitted aids ("All in
  # accordance with Supplementary provisions to the Regulations for Full-time
  # Study Programmes at the Norwegian School of Economics Ch.4 Permitted
  # support material https://...", "Alt i henhold til Utfyllende bestemmelser
  # til Forskrift om fulltidsstudiene ved Norges Handelshøyskole Kap 4
  # https://... og informasjonen som er gitt her: https://...", "For details
  # see ...", "See also: Forskrift om ..."; 2026-11-26), with the aids
  # heading when it is all there is under it
  nhh  = paste0("(?im)(?:^(?:Permitted Support Material|Hjelpemidler til eksamen)[ \t]*\n)?",
                "^[ \t]*(?:(?:All |Alt )?(?:in accordance with|i henhold til)[ \t]*|",
                "For det(?:ails see|aljer se)[ \t]+|See also:[ \t]*)",
                "(?:Supplementary provisions to the Regulations|Utfyllende bestemmelser til|Forskrift om)[^\n]*(?:\n|$)"),
  # the aids link text; a pointer to the AI page, after the aids rule or alone
  hvl  = c("(?m)^Me(?:i)?r om hjelpemid(?:del|ler)[ \t]*$",
           paste0("(?m)[ \t]*For (?:mer |meir )?informasjon om bruk av kunstig intelligens ",
                  "\\(KI\\),? s(?:e|jå) [^\n]*$")),
  nord = c(
    # link labels: to the AI page, after the AI rule or alone, to the aids
    # page (with the aids heading, when it is all there is under it) and to
    # the exam guidelines
    "[ \t]*Informasjon om ChatGPT/kunstig intelligens[^\n]{0,20}? finner du her\\.?",
    "(?m)(?:^Hjelpemidler til eksamen[ \t]*\n)?[ \t]*Generell informasjon om hjelpemid(?:del|ler) til eksamen finn(?:er)? du her\\.?",
    paste0("(?m)^[ \t]*(?:Praktisk informasjon og retningslinjer for eksamen|",
           "Les mei?r om retningslinjer og praktisk informasjon om eksamen)\\.?[ \t]*$"),
    # the form may change; StudentWeb shows the right one at registration
    paste0("[ \t]*Det tas forbehold om endringer i vurderingsform\\w*\\.?(?:[ \t]*Korrekt\\w*(?:/\\w+)? ",
           "vurderingsform\\w* vises i StudentWeb[^.\n]*\\.?)?")),
  # the fitness-to-practise label over a pointer to the programme plan
  oslomet = paste0("(?im)^[ \t]*(?:Skikkethetsvurdering[ \t]*(?:[,:][ \t]*|\n[ \t]*)?)?",
                   "se programplanen for [^\n]*?(?:skikkethetsvurdering|for nærmere informasjon)[^\n]*(?:\n|$)"),
  # the national fitness-to-practise notice (criteria a-h), the same in the
  # plans of the practice programmes
  usn  = paste0("(?s)Skikkethetsvurdering\\nKunnskapsdepartementet fastsatte .{0,1500}?",
                "\\nh\\) [^\\n]*(?:\\n|$)"),
  # the exam system after the form ("Digital skoleeksamen - Inspera",
  # "Digital school assessment-Insperia"; the form stays), a deadline
  # weekday as the duration ("Duration: Due date on Thursday at 23:59") and
  # a start time after it ("30 minutes (per group) – Friday morning,
  # starting at 9:15."; the duration stays). Which part is due on which day
  # of an intensive week (LOG904) is the assessment's design and stays.
  himolde = c("(?m)[ \t]*[-–][ \t]*Insperi?a[ \t]*$",
              paste0("(?m)^Duration:[ \t]*Due date on (?:Mon|Tues|Wednes|Thurs|Fri|Satur|Sun)day",
                     "(?: at \\d{1,2}[:.]\\d{2})?\\.?[ \t]*(?:\n|$)"),
              paste0("[ \t]*[-–][ \t]*(?:Mon|Tues|Wednes|Thurs|Fri|Satur|Sun)day(?: morning| afternoon)?,? ",
                     "starting at \\d{1,2}[:.]\\d{2}\\.?")),
  # PDF lines wrapped mid-sentence: the examiner, the diploma, the pointer to
  # the rules for appeal and resit, and the resubmission after F; the grading
  # scale in the examiner sentence before them stays
  steiner = c("[ \t]*Veileder er vanligvis\\s+intern sensor\\.",
              "[ \t]*Tittel og karakter på [^.]{0,40}? noteres\\s+på vitnemålet\\.",
              "(?m)^[ \t]*Kriterier for karakterfastsettelse og bestemmelser om sensur[^.]*\\.[ \t]*(?:\n|$)",
              "(?m)^[ \t]*Hvis oppgaven vurderes til karakteren F[^.]*?kan kandidaten levere ny\\s+oppgave[^.]*\\.[ \t]*(?:\n|$)"),
  # the university's rule for the number of attempts at a thesis, quoted
  # from its regulations with the link
  uis  = paste0("(?s)[ \t]*Forskrift om studier og eksamen ved Universitetet i Stavanger §\\s?2-10 Antall forsøk.*?",
                "(?:blir vurdert som ikke bestått\\.(?:\\s*Les mer:\\s*\\S+)?|Les mer:\\s*\\S+)[ \t]*(?:\n|$)"),
  uit  = c("(?m)^Eksamensdato er foreløpig.*$",
           # the grading cell of each row of the 2020s gate table, the same
           # for every gate; the gate's name stays
           "\tKarakterregel:[ \t]*\nGodkjent – ikke godkjent[ \t]*")
)

# Sentences of exam logistics, costs and workload, in any section
# (course_plan_spec.md: plan text, but in no section), per institution. hvl: what Studentweb or the
# learning platform announces ("Tid og sted for eksamen blir opplyst på
# Studentweb.", "Innleveringsfrist blir oppgitt på Studentweb og digitalt
# eksamenssystem.", "Tid og stad for eksamen blir gitt på Canvas."), when the
# exam is held ("Eksamen blir avholdt i starten av januar.") and what the
# student pays ("Kr 400 i materialpenger betales inn ved studiestart",
# "Materialkostnader (kr 500) må dekkes av studentene.", an excursion's
# "egenandel" or price); a sentence that also names a coursework requirement
# stays. phs: the workload ("Omfanget på videreutdanningen er anslått til ca.
# 420 timer.", "Utdanningen er estimert til ca. 210 timer."); the hours of the
# gatherings ("Samlingene utgjør inntil 105 timer ...") say how the course is
# taught and stay. uia, ntnu, uit: the expected workload stated alone
# (.workload_sentence_regex).
# The student's expected workload, a sentence that opens with it and gives
# one figure of hours: "Emnet har et forventet arbeidsomfang på rundt 200
# timer.", "Forventet arbeidsinnsats i emnet er 135 timer.", "Estimated
# workload is about 200 hours.", "For å nå læringsmålene i emnet forventes det
# at studentene arbeider 200 timer inkludert forelesninger, ...". It stays
# when it also says how the course is taught or assessed: the hours of the
# teaching forms ("..., der organisert undervisning utgjør: Forelesninger
# ...", "Forventet arbeidsbelastning per uke er 4 timer forelesninger, ..."),
# a split ("fordelt slik:", "hvorav", "derav"), supervision ("Maximum workload
# for the supervisor ..."), the exam, praksis, a compulsory part or a grade.
.workload_sentence_regex <- paste0(
  "(?i)^\\W*",
  "(?!.*\\b(?:veiled|supervis|gruppeoppg|eksamen|exam|praksis|obligator|mandatory|compulsory|",
  "teller|karakter|grade|derav|hvorav|av dis+e|av dette|rest)\\w*)",
  "(?!.*\\bfordel\\w* (?!(?:på|over) (?:\\S+ )?semest))",
  "(?!(?:(?!inkl|includ).)*\\b(?:forelesn|forelesing|øving|øvning|laborator|seminar|kollokv|lecture|",
  "timeplan|organiser|teoritim|undervisning|teaching|krav)\\w*)",
  "(?!(?:.*\\b(?:timer|timar|hours)\\b){2})(?!.*:\\s*$)(?=.*\\b(?:timer|timar|hours)\\b)",
  "(?:(?:\\S+\\s+){0,6}?(?:arbeidsomfang|arbeidsbelastning|arbeidsinnsats|arbeidsmengd|workload|arbeidstimer)|",
  "(?:\\S+\\s+){0,10}?(?:student\\w*|deltaker\\w*) (?:(?:is|are) expected to work|arbeider)\\b)"
)

# The examiner arrangement stated alone: "Intern og ekstern sensor
# (benyttes).", "Interne sensorar.", "Intern sensur med minst to sensorar.",
# "Sensureres av faglærer og én ekstern sensor.", "Sensur gjennomføres av to
# sensorer, hvorav en ekstern.", "Vurdering av bacheloroppgåva blir gjort av
# ein intern og ein ekstern sensor.", "Eksamenskommisjonen består av ...",
# "Veileder er ikke sensor.". A sentence that goes on after the examiners
# ("... av intern og ekstern sensor med karakterskala A – F.", "... på
# grunnlag av læringsutbyttebeskrivelsene"), names a grade ("Intern sensur
# med nytt vurderingsuttrykk: Bestått / ikke bestått.") or names an exam form
# as its subject ("Muntlig eksamen vurderes av to sensorer.") stays.
.examiner_sentence_regex <- paste0(
  "(?i)^(?!.*(?:karakter|vurderingsuttrykk|bestått|\\bA ?[-–] ?F\\b|\\bgrade))\\W*(?:",
  "(?:intern\\w*|ekstern\\w*)(?: og (?:intern|ekstern))? sensor\\w*(?: og (?:intern|ekstern) sensor\\w*)?",
  "(?: (?:benyttes|brukes|nyttast))?|",
  "det (?:benyttes|brukes|nyttast) (?:både )?(?:ekstern og intern|intern og ekstern|to) sensor\\w*|",
  "(?:intern|ekstern) sensur med [^.]*|",
  "(?:sensur(?:en)? (?:gjennomføres|gjennomførast|foretas|skjer) av|sensureres av) [^.]*|",
  "(?!(?:\\S+ ){0,4}?(?:muntl|munnl|skriftl|praktisk)\\w*)(?:[\\p{L}-]+ ){0,4}?",
  "(?:vurderes|vurderast|blir vurdert|vert vurdert|blir gjort|vert gjort|gjøres|evalueres) av ",
  "(?:[^.]*? )?(?:sensor\\w*|eksamenskommisjon[^.]*)",
  "(?:,? (?:hvorav|der|ein av desse|en av disse|som høgskolen oppnevner)[^.]*)?|",
  "(?:eksamens|sensor|klage)kommisjon\\w* (?:består|skal bestå|skal vera sett saman|er sett saman) [^.]*|",
  "veileder(?:en)? er (?:ikke|ikkje) (?:sensor|med i (?:eksamens)?kommisjonen)|",
  "(?:den (?:muntlige|munnlege) (?:prøven|eksamenen)) (?:gjennomføres|vert gjennomført) med \\w+ sensor\\w*",
  ")\\W*$"
)

# Per institution, a regex or a vector of them (a sentence goes when one
# matches). The resit and appeal rules (course_plan_spec.md: out of the
# sections) go when they say only who may resit or resubmit, when or how
# often, or how to appeal; a sentence that says what the resit consists of
# stays ("Ved kontinuasjonseksamen bearbeides samme oppgave.", "Utsatt
# eksamen er en individuell skriftlig hjemmeeksamen."). mf: withdrawal before
# the deadline, the grading deadline and the pointers to the exam pages; uis:
# no new supervision at a new attempt; inn: appeal, course evaluation, the
# admission requirement in course content and the project money; nmbu: the
# repeat of a failed practice period, the suitability notice and who
# assesses it; oslomet: who may resit or resubmit, the publication
# agreement, submission months and exam-plan pointers; uib: the deadlines
# ("Fristar ...") and who attends the presentation; uio: the exam system
# (Inspera), the examiner guide, "Adgang til undervisning:", contact
# pointers and insurance; vid: how many resits; hiof: when the resit is held
# and the appeal rules; hvl also the programme line ("Studieprogram: ...")
# and "Utdanningen er gratis"; nmh: the automatic registration ("Studenten
# meldes automatisk opp til undervisning/veiledning og vurdering i emnet
# ..."), the pointer to the programme's "Organisering" and the stock resit
# rule ("For ny vurdering gjelder samme ordning som for ordinær vurdering.");
# khio: who assesses, stated alone ("Vurderingen foretas av emneansvarlig og
# kan eventuelt gjøres i samarbeid med interne faglærere og/eller eksterne
# gjestelærere.", "Emnet vurderes av lærerne i samråd med
# programansvarlig."; "Studenten vurderes av pedagogen kontinuerlig ..."
# says how and stays); aho (2026-11-22): the exam system ("Besvarelsen
# leveres i Wiseflow.", "Du må gjøre deg kjent med innlogging og
# innleveringsmåte ...", "Se informasjon om Wiseflow på nettsiden."), the
# source-use notice ("Ved oppgaveskriving plikter du ... kildebruk ...",
# "Ved brudd på reglene ... fusk"), the pointers to Moodle's deadline and
# delivery rules, and the diploma's withdrawal deadline ("A diploma project
# may be withdrawn from examination by December 1st ..."); "Fem timers
# skriftlig hjemmeeksamen i Wiseflow." gives the exam form and stays; dmmh
# and barrattdue (2026-11-21): the examiners and
# who they normally are ("Intern sensor er normalt samme person som var
# opponent ...", "Sensur med to interne sensorer, normalt seksjonsleder og
# hovedinstrumentlærer.", "To interne sensorer.", "Studentens
# hovedinstrumentlærer inngår normalt som en av sensorene."); dmmh: the
# resubmission rules ("Ved ikke godkjent praksisrapport gis det mulighet til
# å levere en gang til.", "Dersom rapporten vurderes til ikke å kunne
# godkjennes, får studenten 5 virkedager til å forbedre den.", "Om
# praksisrapport ikke er levert innen fastsatt innleveringsfrist, regnes
# dette som et forsøk.", "Studenter som har fått
# restpraksis grunnet gyldig fravær, vil få ny innleveringsfrist ...");
# barrattdue: who decides how the resit is held ("Dette avgjøres individuelt
# av faglærer.", "Individuell vurdering av faglærer/seksjonsleder"), the
# dated deadlines ("Frist for innlevering av årsrapport 15.mai") and
# (2026-11-23) the stock resit rules under "Ny vurdering" ("Den delen som
# ikke er bestått må gjennomføres på nytt. Deretter gis det ny samlet
# vurdering.", the PPU plans; "Studenten må gjennomføre hele eller deler av
# emnet på nytt."); "Ny eksamen", "Ny innlevering av refleksjonsnotat,
# repertoarrapport og ny praktisk eksamen." say what the resit is and stay.
.examiner_role_regex <- paste0(
  "(?i)^\\W*(?:(?:intern |ekstern )?sensur med (?:minst |minimum )?(?:\\w+ )?(?:interne? |eksterne? )?sensor[^.]*|",
  "(?:en|to|tre|\\d) (?:interne? |eksterne? )?sensor(?:er)?\\b[^.]*|",
  "(?:(?:intern |ekstern |en )?sensor(?:en)?|en av sensorene|studentens \\w*lærer(?: i emnet)?) ",
  "(?:er|inngår) normalt\\b[^.]*)\\W*$")
# UiA's pointers to Canvas alone, on both trees (2026-11-25): "Nærmere
# informasjon gis i Canvas ved semesterstart.", "Se Canvas for nærmere
# informasjon om eksamen.", "Nærmere informasjon om krav til mappen blir gitt
# i Canvas.", "More information will be given in Canvas at the start of the
# semester.", "Please refer to Canvas for more information."; not a sentence
# that says more ("Approved mandatory assignment, further information will be
# given in Canvas ...", "Alle obligatoriske øvelser ... må være godkjent, se
# Canvas for mer informasjon.").
.canvas_pointer_regex <- paste0(
  "(?i)^\\W*(?:(?:for (?:datoer og andre detaljer|nærmere spesifisering),? )?(?:se|sjå) (?:nærmere |mer |utfyllende )?",
  "(?:informasjon )?(?:i |på )?canvas(?: for (?:mer|nærmere|detaljert|utfyllende|ytterligere) informasjon(?: om eksamen)?| for detaljer)?|",
  "(?:please )?refer to canvas for (?:more|further) information|det vises til (?:ytterligere|nærmere) informasjon i canvas|",
  "(?:(?:nærmere|mer(?: detaljert)?|utfyllende|ytterligere|further|more(?: detailed)?|detailed)\\s+)?(?:informasjon|information)",
  "(?: (?:om|about|on) [^.]{0,80}?)?(?: (?:gis|finnes|legges ut|kommer|fremlegges|publiseres|er publisert|blir gitt|",
  "vil (?:bli )?(?:gitt|gis|lagt ut|publisert|gjort tilgjengelig)|will be (?:given|provided|available|posted|published)|",
  "is (?:given|provided|available|posted)|can be found))? (?:i|på|in|on|through) (?:the )?",
  "(?:studentenes læringsstøttesystem |emnerommet i )?canvas)",
  "(?: (?:ved|før|at|by|when|i starten|in the beginning) [^.]{0,60})?\\W*$")
.logistics_sentences <- list(
  hvl = c(paste0(
    "(?i)studentweb|",
    "^\\s*(?:tid|dato|frist|innleveringstidspunkt|innleveringsfrist)\\b[^\\n]*?",
    "(?:opplys|oppgi|oppgje|annonser|kunngj|publiser|blir gitt|vert gitt)|",
    "^\\s*eksamen\\w* (?:blir|vert) (?:avholdt|avhalde|halde|haldt)\\b[^\\n]*?",
    "\\b(?:januar|februar|mars|april|mai|juni|juli|august|september|oktober|november|desember)\\b|",
    "^(?![^\\n]*arbeidskrav)[^\\n]*(?:materialpeng|materialkostnad|kostpeng|e(?:i)?genandel|",
    "\\bkr\\.?[ \\t]?\\d|\\d[ \\t]?kr\\b)"),
    "(?i)^\\W*Ved ikke bestått karakter på bacheloroppgaven,? gis [^.]*anledning til å levere en forbedret versjon",
    "^\\W*Studieprogram:|^\\W*Utdanningen er gratis\\b",
    .examiner_sentence_regex),
  phs = "(?i)^\\s*(?=.*\\btimer\\b)(?:[^\\s.]+\\s+){1,4}?er\\s+(?:totalt\\s+)?(?:anslått|estimert)\\s+til\\b",
  uia = c(.workload_sentence_regex, .canvas_pointer_regex,
          # the exam held both semesters for the auditors' grade requirement
          "(?i)^\\W*(?:due to|av hensyn til|på grunn av) (?:the )?(?:grade requirements?|karakterkrav\\w*) for (?:state authori[sz]ed|statsautoriserte)"),
  nih = paste0("(?i)^\\W*(?:the )?(?:project )?(?:assignment|prosjektoppgaven|oppgaven)\\b[^.]{0,30}?\\b(?:submitted in|leveres i) ",
               "wiseflow (?:during|i) (?:a designated|en oppgitt) (?:submission period|innleveringsperiode)"),
  # nhh: when the assignment is handed out or due, stated alone ("The
  # assignment will be distributed in January.", "The submission deadline will
  # be in April.", "Submission deadline is 7 weeks after the last lecture."),
  # and the stock resit rule "Re-take is offered the semester after the
  # course was offered for students with valid compulsory activities (work
  # requirements)." (2026-11-26); not a sentence with the task, the group
  # size, a weight or the time to do it ("3-day group home exam (30%). The
  # exam will be distributed on day 1, with submission deadline on day 3."
  # keeps its first sentence only; "The assignment will be distributed after
  # the final lecture and must be submitted within three weeks." stays)
  nhh = paste0("(?i)^\\W*(?:(?:The )?(?:final )?submission deadline (?:is |will be )?(?:approximately |approx\\. )?",
               "(?:in|at|by|during|\\d+ weeks?)\\b[^.%]*|",
               "The (?:assignment|exam|term paper)s? will be (?:distributed|released|handed out)\\b",
               "(?![^.]*\\b(?:within|to complete|ha(?:ve|s) \\d))[^.%]*|",
               "Re-take is offered (?:for [^.]{0,40}?)?the semester after the course was (?:offered|provided) ",
               "for students with valid compulsory activities\\b[^.]*)\\W*$"),
  ntnu = .workload_sentence_regex, uit = .workload_sentence_regex,
  mf = c(paste0("(?i)^\\W*(?:For ikke å bruke (?:opp )?et eksamensforsøk|",
                "[^.]*\\btrekk(?:e|er) sin (?:oppmelding|vurderingsmelding))"),
         "(?i)^\\W*(?:Sensurfristen\\b|Informasjon om innleveringsfrister\\b|Se eksamenssidene\\b)",
         "(?i)^\\W*Sensorveiledning\\w*(?: for [^.]*?)? (?:er tilgjengelig|finner du|finnes)"),
  uis = c(paste0("(?i)^\\W*(?:Studente[rn]|Studentar|Kandidat\\w*) har ikkj?e krav på nye ",
                 "(?:veiledningstimer|rettleiingstimar) ved nytt eksamensforsøk"),
          "(?i)^\\W*Kontinuasjonseksamen (?:vil )?følg(?:j)?e?r? (?:same|samme) oppsett som (?:ved )?ordinær eksamen\\W*$"),
  inn = c(.examiner_sentence_regex,
          "(?i)^\\W*(?:Eksamensresultatet|Karakteren|Resultatet) kan (?:ikke )?påklages\\W*$",
          "(?i)^\\W*Det er kun karakteren på [^.]* som kan påklages",
          paste0("(?i)^\\W*(?:Emneansvarlig\\w*(?: lærer)? har ansvar\\w* for at (?:studentene involveres i )?evaluering|",
                 "(?:Det|Til vanlig skal det) gjennomføres evaluering\\w* av alle emner|",
                 "Tidspunkt og metode avgjøres i samråd med tillitsvalgte)"),
          "^\\W*Opptakskravet er\\b",
          "^\\W*(?:Skolen bidrar med en fast sum prosjektmidler|Alle studenter mottar prosjektmidler|Fordeling og størrelse besluttes fra år til år)"),
  nmbu = c("^\\W*Dersom studenten ikke består en praksisperiode, kan samme periode gjennomføres bare en gang til",
           "^\\W*Gjennom praksisopplæringen skjer også en kontinuerlig skikkethetsvurdering",
           "^\\W*Praksislærer\\w*/praksisskole\\w* har hovedansvaret for vurdering\\w* av praksisopplæringen"),
  oslomet = c(paste0("(?i)^\\W*(?:Studente[rn]|Kandidate[rn])\\b[^.]{0,200}?\\b(?:kan|har rett til å) ",
                     "(?:fremstille|framstille|melde) seg(?: opp)? til (?:ny/utsatt|utsatt/ny|ny og utsatt|ny eller utsatt) ",
                     "eksamen(?: i emnet)?",
                     "(?: i tråd med forskrift[^.]*)?\\W*$"),
              paste0("(?i)^\\W*(?:Studente[rn]|Kandidate[rn])\\b[^.]{0,120}?har rett til ny/utsatt eksamen",
                     "(?:,? som har lik utforming som ordinær(?: eksamen)?)?\\W*$"),
              paste0("(?i)^\\W*(?:En student kan levere ny eller revidert masteroppgave|",
                     "Dersom masteroppgaven vurderes til ikke bestått, kan studenten levere ny eller revidert oppgave|",
                     "Der flere skriver sammen må hele gruppa levere ny eller revidert oppgave|",
                     "Det er ikke adgang til å få vurdert en ny masteroppgave|",
                     "For at masteroppgaven skal kunne publiseres|Forutsatt studentens godkjenning vil beståtte|",
                     "For studenter som (?:har rett til ny/utsatt eksamen|strøk ved ordinær eksamen)|",
                     "Studenten får da tilbud om \\w+ timer? ytterligere veiledning|",
                     "Ny/utsatt eksamen har (?:samme|lik) form som ordinær eksamen\\W*$|",
                     "Siste frist (?:for innlevering )?vil fremgå av eksamensplanen|",
                     "(?:Masteroppgaven skal leveres|Innlevering vil bli) i månedsskiftet)")),
  uib = c(.examiner_sentence_regex,
          "(?i)^\\W*(?:Fristar\\b|Mal for oppgåva\\b|Oppgåver på \\d+ (?:stp|sp) (?:innleverast|leverast))",
          "(?i)^\\W*Sensor og (?:veileder|rettleiar) skal (?:være|vere) til stede"),
  uio = c(.examiner_sentence_regex,
          paste0("(?i)^\\W*(?:(?:Oppgaven|Masteroppgaven)\\s+(?:skal\\s+)?leveres\\s+(?:elektronisk\\s+)?i\\s+Inspera|",
                 "Innlevering av [^.]*i Inspera|Les mer om innlevering|The thesis must be uploaded electronically)"),
          "(?i)^\\W*Sensorveiledning\\w*(?: for [^.]*?)? (?:er tilgjengelig|finner du|finnes)|samleside over tidligere eksamensoppgaver",
          "^\\W*Adgang til undervisning:|har du (?:ikkj?e )?(?:krav på|rett til) ny undervisning\\b",
          "(?i)^\\W*Ta kontakt med\\b[^.]*\\bfor (?:å få )?(?:mer|flere)\\b",
          "(?i)forsikret som student|tegne en egen reise- og personskadeforsikring"),
  nmh = c("^\\W*(?:Studenten|Kandidaten) meldes automatisk opp til undervisning",
          "^\\W*For oversikt over [^.]*?\\bveiledende undervisningsmengde\\b[^.]*?,? se\\b",
          "^\\W*(?:For ny vurdering|Ved ikke bestått vurdering) gjelder samme ordning som (?:for |ved )?ordinær vurdering\\W*$"),
  khio = c(.examiner_sentence_regex,
           local({
             who <- paste0("(?:emneansvarlig|programansvarlig|(?:fag)?lærer(?:e|ne)?|veileder(?:en)?|",
                           "koreograf(?:er|ene)?|ekstern sensor|to eksterne sensorer)")
             paste0("^\\W*(?:Vurderingen (?:foretas|gjøres)|Emnet vurderes) av (?:to sensorer\\b[^.]*|",
                    who, "(?:(?:/| og | eller | og/eller )", who, ")*",
                    "(?: (?:og kan eventuelt gjøres|i (?:samråd|samarbeid) med|sammen med)\\b[^.]*)?)\\W*$")
           })),
  dmmh = c(.examiner_sentence_regex, .examiner_role_regex,
           "(?i)^\\W*Ved ikke godkjent \\w+ (?:gis det|får studenten)\\b[^.]*\\blevere en gang til\\b",
           "(?i)^\\W*Om \\w+ ikke er levert innen fastsatt innleveringsfrist, regnes dette som ett? forsøk",
           "(?i)^\\W*Dersom \\w+ vurderes til ikke å kunne godkjennes, får studenten \\w+ virkedager til å forbedre",
           "(?i)^\\W*Studenter (?:som har fått|med) restpraksis grunnet gyldig fravær,? vil få ny (?:innleverings)?frist"),
  barrattdue = c(.examiner_sentence_regex, .examiner_role_regex,
                 "(?i)^\\W*(?:Det(?:te)? avgjøres individuelt av faglærer|Individuell vurdering av faglærer/seksjonsleder\\W*$)",
                 paste0("(?i)^\\W*(?:Den delen som ikke er bestått må gjennomføres på nytt|Deretter gis det ny samlet vurdering|",
                        "Studenten må gjennomføre hele eller deler av emnet på nytt)\\W*$"),
                 paste0("(?i)^\\W*Frist (?:for )?innlevering av [^.]*?\\d{1,2}\\.\\s?",
                        "(?:januar|februar|mars|april|mai|juni|juli|august|september|oktober|november|desember)\\W*$")),
  aho = c(paste0("(?i)^\\W*(?:(?:Besvarelsen|Essayet|Oppgaven|Masteroppgaven|Rapporten) (?:skal )?leveres ",
                 "(?:som (?:en )?pdf )?i wiseflow\\W*$|Du må gjøre deg kjent med innlogging og innleveringsmåte|",
                 "Se informasjon om wiseflow|Se nettsiden for mer informasjon\\W*$)"),
          paste0("(?i)^\\W*(?:Ved oppgaveskriving plikter du å gjøre deg kjent med reglene for kildebruk|",
                 "Ved brudd på reglene kan du bli mistenkt for fusk)"),
          "(?i)^\\W*(?:Eksakt frist for innlevering er opplyst|Endelige innleveringskrav er beskrevet i detalj) på Moodle\\W*$",
          "^\\W*A diploma project may be withdrawn from examination by\\b"),
  vid = c(.examiner_sentence_regex,
          "(?i)^\\W*Utsatt eksamen er tilsvarende (?:som )?ordinær(?: eksamen)?\\W*$",
          "(?i)^\\W*VID arrangerer inntil \\w+ utsatte prøver"),
  hiof = c(paste0("(?i)^\\W*(?:If (?:a student gets F, the re-sit exam might be arranged|less than three apply for re-sit)|",
                  "If (?:the )?(?:students|candidates) wish to improve their grade|",
                  "In the case of a fail(?:, the group may improve| in the oral exam the student will be reexamined)|",
                  "If the portfolio is given a fail, it may be improved and resubmitted|",
                  "(?:The (?:mark|grade|result)|Each partial mark)[^.]*can be appealed|",
                  "(?:If|In the case of)\\b[^.]*\\bappeal|The result of the appeal)"),
           paste0("(?i)^\\W*(?:Klageadgang:|Skriftlig eksamen kan påklages|Resultatet \"?ikke bestått\"? kan påklages|",
                  "Klage på karakter\\w*[^.]*må undertegnes|Ved klage på (?:sensur|karakterfastsetting)\\b|",
                  "Dersom studenten ønsker å klage|Resultatet av klagesensuren|Ved nytt resultat slås [^.]*sammen på nytt|",
                  "Ved eventuell endring av karakter,? skal det arrangeres ny muntlig eksamen)"),
           paste0("(?i)^\\W*(?:Eventuell )?(?:ny/utsatt|ny og utsatt) eksamen (?:foregår ved neste ordinære|",
                  "må tas neste gang|kan først gjennomføres ved neste ordinære|",
                  "gjennomføres på samme måte som (?:ved )?ordinær eksamen\\W*$)"))
)

# Blocks of workload in any section, per institution: uis's budget, a header
# ("Forventet timeinnsats (413 timer):", "Anslag over tidsforbruk:",
# "Arbeidsinnsatsen er beregnet slik:") and the lines of hours under it
# ("Forberedelse + etterarbeid: 214 timer"); a header with no hours in or
# under it stays; and the stock paragraph on the hours of a year of study
# ("Universitetet i Stavanger definerer eit studieår til 40 veker, ...")
# with its label. nmh: the programme footer ("Inngår i studieprogram" over
# "BACHELOR - Bachelorstudiet i utøving – klassisk", the end of the page and
# so of its last section), the responsible unit ("Emneansvarlig: Fagseksjon
# for ...") and the resit heading "Ny vurdering" left alone at the end by
# its stock rule (.logistics_sentences); khio: the notice that the
# description may change ("OBS: det kan komme mindre endringer i
# emnebeskrivelsen ..."); dmmh: the examiner row of the exam table
# ("Sensorordning:\tIntern og ekstern", "Censor type:\tInternal"; 2026-11-21).
.logistics_blocks <- list(
  dmmh = "(?m)^[ \t]*(?:Sensorordning|Censor type):[^\n]*(?:\n|$)",
  nmh = c("(?m)^[ \t]*Inngår i studieprogram[ \t]*(?:\n[ \t]*\\p{Lu}{2,}[\\p{Lu} ]* - [^\n]*)*(?:\n|$)",
          "(?m)^[ \t]*Emneansvarlig:[ \t]*(?:[Ff]agseksjon|[Ii]nstitutt|[Aa]vdeling)\\b[^\n]*(?:\n|$)",
          "\\s*(?:^|\n)[ \t]*(?:Ny vurdering|New assessment)[ \t]*\\s*$"),
  khio = paste0("[ \t]*OBS: det kan komme mindre endringer i emnebeskrivelsen[^.]*\\.",
                "(?:[ \t]*Endringene publiseres[^.]*\\.)?[ \t]*(?:\n|$)"),
  uis = c(paste0(
    "(?m)(?:^[ \t]*Arbeidsmengde?:?[ \t]*\n)?^[ \t]*(?:Arbeidsmengde?:[ \t]*)?Universitetet i Stavanger definerer ",
    "e(?:it|t) studieår til 40 (?:veker|uker),(?s:.{0,900}?)\\b1200-1500 sider\\b[^.]*\\.[ \t]*(?:\n|$)"),
    paste0(
    "(?m)^[ \t]*(?:(?:[Ff]orventet|[Ee]stimert|[Bb]eregnet|[Tt]otal forventet)[ \t]+)?",
    "(?:[Tt]imeinnsats|[Aa]rbeidsinnsats(?:en er beregnet slik)?|[Aa]rbeidsbelastning|[Aa]rbeidsmengde?|",
    "Anslag over tidsforbruk)\\b",
    "(?:[^\n]*\\d[ \t]*(?:timer|t)(?:\\b|(?=\\p{Lu}))[^\n]*|[^\n]*(?=\n[^\n]*\\d[ \t]*(?:timer|t)(?:\\b|(?=\\p{Lu}))))",
    "(?:\n[^\n]*\\d[ \t]*(?:timer|t)(?:\\b|(?=\\p{Lu}))[^\n]*)*(?:\n|$)"))
)

# Approval stamps, in any section (course_plan_spec.md: out of the sections):
# oslomet's "Vedtatt i avdelingsstyret. Godkjent av rektor. Siste revisjon
# godkjent av studieutvalget ved fakultet LUI. Redaksjonell endring foretatt",
# "Emneplanen er godkjent av utdanningsutvalget ved ...", "Revisjon godkjent
# på fullmakt ...", "Etablert av dekanen <date>", "Pensum oppdatert.",
# "Gjeldende fra <date>". A sentence goes when it opens with the stamp.
.approval_sentence_regex <- paste0(
  "(?i)^\\s*(?:vedtatt i \\w*styret|emneplan(?:en)? (?:er |ble |blei )?(?:godkjent|revidert)|",
  "(?:siste )?revisjon(?:en)? (?:er |ble )?godkjent|revidert (?:versjon|emneplan)\\b[^.]*godkjent|",
  "revidert av \\w*utvalg|godkjent (?:(?:av|i|på) )?",
  "(?:rektor|dekan|fakultet|studieutval|utdanningsutval|doktorgradsutval|styret|fullmakt)|",
  "redaksjonell\\w* endring|(?:studiet ble )?etablert av (?:dekan|fakultet)|",
  "pensum oppdatert\\.?\\s*$|g(?:jeldende|yldig) fra\\b.{0,40}$)"
)

# The sentences of a text that match `rx` (a regex, or a function of the
# sentences that says which) go (.sentence_end), and a line left empty with
# them; so does a line matching `label` when the next line with text all
# went. With `taken`, a list of the text left (`rest`) and the sentences that
# went, a line each (`taken`).
.strip_sentences <- function(text, rx, taken = FALSE, label = NULL) {
  if (is.na(text)) return(if (taken) list(rest = text, taken = character()) else text)
  lines <- stringr::str_split_1(text, "\n")
  gone <- logical(length(lines))
  out <- character(length(lines))
  for (k in seq_along(lines)) {
    s <- stringr::str_split_1(lines[k], .sentence_end)
    drop <- if (is.function(rx)) rx(s) else stringr::str_detect(s, rx)
    if (!any(drop)) next
    out[k] <- trimws(paste(s[drop], collapse = ""))
    lines[k] <- trimws(paste(s[!drop], collapse = ""))
    gone[k] <- !nzchar(lines[k])
  }
  if (!is.null(label)) for (k in which(grepl(label, lines, perl = TRUE) & !gone)) {
    nxt <- which(seq_along(lines) > k & (gone | grepl("\\S", lines)))[1]
    if (isTRUE(gone[nxt])) {
      out[k] <- trimws(lines[k])
      gone[k] <- TRUE
    }
  }
  rest <- paste(lines[!gone], collapse = "\n")
  if (taken) list(rest = rest, taken = out[nzchar(out)]) else rest
}

.strip_section_noise <- function(text, institution = NULL) {
  rx <- c(.section_noise$all, if (!is.null(institution)) .section_noise[[institution]])
  for (r in rx) text <- stringr::str_remove_all(text, r)
  stringr::str_replace_all(text, "\n{3,}", "\n\n")
}

.clean_section_text <- function(text, section) {
  if (is.na(text)) return(text)
  # The FS (Felles studentsystem) page-generation timestamp is admin
  # boilerplate appended at harvest time, never section content. It ends the
  # page (uia, hiof), but a gate sentence moved from assessment may follow it
  # in coursework, so only its line goes.
  text <- stringr::str_remove(
    text, "\\s*Sist hentet fra FS \\(Felles studentsystem\\)[^\n]*")
  # A leading line that merely repeats this section's own heading is noise
  # (e.g. uia "Læringsutbytte" echoed as the first body line). Use an EXACT
  # match against the heading patterns (not the substring matcher) so we never
  # delete real one-line content that merely contains a section keyword
  # (e.g. "Ingen pensumliste tilgjengelig" or "Vurdering skjer ved eksamen").
  # A learning-outcome group label ("Kunnskap") and a label heading
  # ("Hjelpemidler") are content, not an echo, and a line that is the whole
  # section ("Praksis") stays.
  lines <- stringr::str_split_1(text, "\\r?\\n")
  norm_first <- tolower(trimws(stringr::str_remove(lines[1], "[:：]\\s*$")))
  eq <- section_heading_patterns$pattern == norm_first
  if (length(lines) > 1 && any(eq) &&
      identical(section_heading_patterns$section[which(eq)[1]], section) &&
      !section_heading_patterns$label[which(eq)[1]] &&
      !grepl(.lo_group_label, norm_first, perl = TRUE)) {
    text <- paste(lines[-1], collapse = "\n")
  }
  trimws(text)
}

.lo_group_label <- paste0("^(?:kunnskap(?:er|ar)?|ferdighe(?:i)?t(?:er)?|",
                          "generell kompetanse|knowledge|skills|general competence)$")

.keep_section_row <- function(text, section) {
  if (is.na(text) || !nzchar(trimws(text))) return(FALSE)
  !.is_placeholder_text(text) && !.is_heading_list(text)
}

# A row that only lists headings, half of them section names (uib 2018 shell
# pages: "Læringsutbytte\nFulltid/deltid\nKrav til forkunnskaper\n...").
# Label lines ("Anbefalte forkunnskaper") do not count: under them short
# values look like headings ("Etter avtale med instituttet", ntnu).
.is_heading_list <- function(text) {
  l <- trimws(stringr::str_split_1(text, "\n"))
  l <- l[nzchar(l)]
  l <- l[!.label_line(l)]
  length(l) >= 3 && all(nchar(l) <= 80) &&
    all(vapply(l, .heading_shaped_line, logical(1))) &&
    mean(!is.na(vapply(l, .exact_section, character(1)))) >= 0.5
}

# Phrases that say a section is absent or lives elsewhere. A row is a
# placeholder only when its WHOLE text is made of these phrases, so real
# content that merely starts with "Ingen" or mentions Leganto is kept.
.placeholder_phrases <- c(
  # bare negation / dummy values: "Ingen", "Ingen krav", "None", "x", "-", "..."
  "ingen(?: spesielle)?(?: krav| forkunnskapskrav| arbeidskrav)?", "nei", "none", "no", "n/a", "x",
  "not relevant",                                                    # uib
  "det er ingen (?:vurdering|eksamen) i emnet",                      # hvl
  "[-–.…_]++", "ingen info",                                         # hvl "_", oslomet
  # no coursework: "Ingen arbeidskrav i emnet.", "No coursework requirements."
  paste0("(?:det er )?ingen (?:obligatoriske )?(?:arbeidskrav|obligatoriske aktivitet\\w*)",
         "(?: ?(?:eller|og|/) ?(?:andre vilkår|obligatorisk\\w* aktivitet\\w*))?",
         "(?: (?:i|for|knytt\\w* til) (?:dette )?(?:emnet|studiet))?"),
  "no coursework requirements(?: or compulsory activities)?",
  "i emnet inngår ingen obligatoriske? aktivitet\\w*",            # ldh (2026-11-21)
  # a bare yes: coursework "Ja", "Ja, vert spesifisert i semesterplan ved semesterstart" (hvl)
  "ja(?:,? \\(?(?:vert |blir )?spesifiser\\w* i (?:semester|undervisnings)plan\\w* ved semesterstart\\)?)?",
  "me(?:i)?r om hjelpemid(?:del|ler)",                               # hvl link
  # pointers to another document
  "(?:se|sjå) (?:fag|program|studie)?plan(?:en)?(?: for [^.]{0,100})?",
  "se emnearkivet",
  # dmmh (2026-11-21): "Se avsnittet «Arbeids-, lærings- og vurderingsformer»
  # i studieplanens generelle del.", "Se studieplanens generelle del."
  "se (?:avsnitt(?:et)? [«\"“][^»\"”]{1,80}[»\"”] i )?studieplanens generelle del",
  # ntnu: the recommended field points to the required; oslomet: "Se
  # forkunnskapskrav beskrevet under Innhold og oppbygging i programplanen."
  "(?:se|sjå) forkunnskapskrav(?:et|a|ene)?(?: beskrevet under [^.]{0,80})?",
  # oslomet practicum courses (2026-11-11): "Se omtale av praksis i
  # programplanen", "Se nærmere omtale av praksis i programplanen", "Se mer
  # utfyllende informasjon om praksis i programplanen og i praksisguiden for
  # PPU-Y", "Se praksisguiden for yrkesfaglærerutdanningen"
  paste0("s?se (?:mer )?(?:nærmere |(?:mer )?utfyllende )?(?:omtale|informasjon)(?: (?:av|om) [^.]{0,60}?)? ?",
         "i (?:program-?(?: og emne)?plan(?:en|delen)?|emneplan(?:en)?)(?: og i;? praksisguide[^.]{0,80})?"),
  "se praksisguiden for [^.]{0,80}",
  # oslomet's Fagplan (2026-11-11): "Praksisopplæring er nærmere beskrevet i
  # programplanen.", "... i fagplan for praksisopplæring for trinn 1-7.",
  # "Om praksisopplæringa, sjå programplanen."
  "praksisopplæring(?:en)? er nærmere beskrevet i [^.]{0,100}", "om praksisopplæringa,? sjå programplanen",
  "se (?:kursinnhold|emnebeskrivelse)(?:et|n)?",                     # nord intro
  "ingen emner(?: i programmet)?",
  # the plan is on the English page (hvl, nord, ntnu; `english_page` in the config)
  # nhh (2026-11-26): "Se de engelske sidene for emnebeskrivelse"
  # ntnu, some 60 wordings (2026-11-05): "Se engelsk versjon / See English
  # version", "Emnet undervises på engelsk. Se engelsk versjon for
  # emnebeskrivelse.", "Se emnets engelske nettside - International MSc"
  paste0("(?:/ ?)?(?:(?:dette )?(?:emnet|faget) undervises (?:kun )?på engelsk[.,]? ?|",
         "the subject is taught in english\\. ?|vennligst |for fullstendig emnebeskrivelse, )?",
         "(?:se|see|cf\\.)(?: derfor)? (?:the |den |de |emnets |the course's )?(?:engels\\w*|english)",
         "(?: (?:vers\\w*|vesjon|tekst|text|utgave|page|informasjon|website|nettside|emneside|kursside|sidene|",
         "(?:emne|kurs)?beskrivelse\\w*|description))?(?: (?:for|om) [^.]{0,40})?",
         "(?: [-–\u0096] international msc)?"),
  "se (?:emne|kurs)?beskrivelsen? på engelsk", "see (?:the description )?in english",
  "faglig innhold er beskrevet på engelsk", "undervises kun på engelsk",
  "you will find the course description in the english version",
  # hvl, some 30 wordings (2026-11-08): "Emneomtalen finnest berre på engelsk,
  # gå til ...", "Dette emnet vert undervist på engelsk. For å sjå
  # emneskildringa, klikk på "English" ...", "Kursinformasjon finnes kun på
  # engelsk. Se informasjon på https://...", "You can read about this course
  # on our English website."
  paste0("(?:(?:emne|studieprogram)?(?:beskrivelsen?|planen|omtalen?|skildringa)|kursinformasjon) ",
         "(?:finnes|finnast|finnest|finst|er) (?:kun|kunn|bare|berre) (?:tilgjengelig )?på engelsk[^.]*"),
  "gå til våre enge?lske nettsider[^.]*",                             # hvl, after a full stop
  "(?:dette )?emne[tr]? er (?:kun |bare |berre )?(?:beskrevet|skildra) på engelsk",
  "(?:dette )?emne[tr]? (?:er|undervises|vert undervist) på engelsk[.,!]? (?:for å sjå emneskildringa, )?klikk på [^.]*",
  "klikk på (?:knappen )?[\"“”]?english[\"“”]?[^.]*",
  paste0("(?:dette emnet kan du lese om|du finn(?:er)? (?:mer )?(?:informasjon om dette emnet|emneplanen)|",
         "informasjon om emnet finn du) på dei? engelske nettsidene(?: våre)?"),
  # uib (2026-11-11): "Informasjonen fins ikkje på norsk. Sjå dei engelske
  # nettsidene.", "For informasjon om mål og innhald i emnet - sjå dei
  # engelske nettsidene."; a page of them has no plan (course_plan_spec.md)
  "t?informasjonen fins ikkje på norsk", "(?:for informasjon om [^.]{0,60}? [-–] )?sjå dei engelske nettsidene",
  "sjå den engelske sida vår[^.]*", "se informasjon på https?://\\S+", "denne finn du ved å velja språk[^.]*",
  "you can read about this (?:course|topic) on our english website",
  "this course will be taught in english, please see the english version of the course description",
  "to find information about the course you have to change language to english[^.]*",
  "for english course descript\\w*, choose[^.]*",
  "the course description is available in english only", "go to our english website by clicking[^.]*",
  "(?:emnet foregår på engelsk\\. )?se engelske nettsider[^.]*",
  # reading-list pointers and absence notes
  "ingen pensumliste(?: tilgjengelig)?(?: for dette emnet)?",
  "no reading list(?: available)?(?: for this course)?",
  "(?:gjeldende )?litteraturliste for [^.]{0,20} finner du i leganto",
  "litteratur og faglige ressurser finn(?:er du|es) her",
  "follow this link for reading list and academic resources",         # nla
  "(?:(?:se|sjå) )?pensumlist[ea] for emnet (?:finn(?:er)? du )?her",  # hivolda
  # himolde, hiof: the Leganto link's label or notice, the reading-list
  # widget's absence note, and "TBA" (2026-11-06)
  "klikk her for pensum(?:oversikt|litteratur)(?: \\d\\. studieår)?", "click here for (?:the )?reading list",
  "(?:the )?current reading list(?: for)?(?: [^.]{0,30})? can be found in leganto",
  "(?:pensumoversikt )?fant ingen pensumliste for dette emnet", "(?:pensum:? )?se leganto", "tba",
  "ta kontakt med (?:instituttet|fakultetet)(?:/(?:instituttet|fakultetet))?",
  "se (?:vår(?:e)? |fakultetets )?hjemmesid(?:e|en|er|ene)",
  # uit: "Contact the department/faculty.", the law courses' "Se fagbeskrivelse
  # med pensum", "Se vår hjemmeside: Fagplan med pensum."
  "(?:please )?contact the (?:departe?ment|faculty)(?: ?/ ?(?:departe?ment|faculty))?",
  "(?:se )?fag(?:beskrivelse|plan)(?:er)? med pensum(?:liste)?(?: ?for (?:\\d\\. )?avdeling)?",
  # uit: the list is on the learning platform or got from the bookshop
  "syllabus and reading list (?:are|will be) available in (?:fronter|canvas),? or by contacting the (?:responsible )?teacher or (?:the|a) student adviser",
  "(?:(?:pensum)?litteratur )?(?:oppgis|opplyses) (?:i (?:fronter|canvas|uits digitale læringsplattform) )?ved (?:semester|studie)start(?: eller (?:kan oppgis og kjøpes|fås) ved henvendelse til [^.]{0,80})?",
  "bøkene kan kjøpes på www\\.akademi[ak]a\\.no",
  "pensumliste(?:n)? (?:vil )?legges (?:vanligvis )?ut i (?:fronter|canvas),? eller kan fås ved henvendelse til emneansvarlig eller studiekonsulent",
  "literature will be announced in fronter at the beginning of term or may be obtained by contacting the (?:campus )?bookstore[^.]{0,60}",
  "se egen pensumliste for studieenheten i (?:fronter|canvas)",
  "available on request",                                             # uit 2008-2010
  # uit project and thesis courses: "Kontakt studiekonsulenten for informasjon
  # om tilgjengelige prosjektoppgaver.", "Please contact the study advisor for
  # information about available master thesis' or if you have question about ..."
  paste0("(?:please |ta )?(?:kontakt|contact) (?:the )?(?:studiekonsulent(?:en)?|stud(?:ent|y) advis[eo]r) ",
         "for (?:informasjon om|information about) (?:tilgjengelige|available) [^.]{0,80}?",
         "(?:,? (?:og prosedyre for|or if you have questions? about) [^.]{0,80})?"),
  "ny pensumbok vurderes",                                            # hiof
  # hiof PPU didactics: the common list is in the PPU pedagogy plan, the
  # course's own given at the start; the list is in Leganto at the start
  "felleslitteratur \\(pedagogikk og fagdidaktikk\\) fremgår av emnebeskrivelsen for pedagogikk i praktisk-pedagogisk utdanning",
  "(?:litteratur spesifikt for emnet: ?)?opplyses ved studiestart og gis til dels i kompendium",
  "oppdatert litteraturliste vil v(?:æ|e)r[ae] tilgjengel[ie]g i leganto ved semesterstart",
  # aho (2026-11-22): "Pensumlisten publiseres i Leganto.", "Litteraturliste vil være tilgjengelig i Leganto."
  "(?:pensumlist|litteraturlist)(?:en|a|e)? (?:publiseres|vil (?:bli publisert|være tilgjengelig)) i leganto",
  # dmmh (2026-11-23): a link's label alone, "Her finner du pensumliste for
  # studieåret" (its academic year gone with the anonymizer, or not yet)
  "her finner du pensumlist(?:en|a|e)? for studieåret(?: \\d{4} ?[-–/] ?\\d{2,4})?",
  # ldh (2026-11-23): the Leganto notice and its links' labels, "Pensumlister
  # finnes i Leganto og er lenket til under. Obs! For å få tilgang til
  # kompendiefiler, ... Kjernelitteratur finner du her. ..."
  "pensumlist(?:er|ene|a|en|e)? finnes i leganto(?: og er lenket til (?:under|nedenfor))?",
  "(?:obs! )?for å få tilgang til kompendiefiler,? må du gå inn i pensumlistene via emnet i canvas",
  "(?:kjerne|emnespesifikk )litteratur finner du her",
  # the list is given at the start or put on the learning platform (hiof, ntnu)
  "(?:(?:aktuell )?pensum(?:liste|litteratur)?|litteratur(?:en)?|bøker og artikler) ?oppgis (?:ved|før) [^.]{0,40}",
  "oppgis (?:ved|før)(?: [^.]{0,40})?", "se pensumliste(?:n)?",
  paste0("(?:den )?(?:endelig\\w* )?(?:pensumlist[ea]n?|semesterplan|studieplan)",
         "(?: og (?:eventuel\\w* )?(?:pensumlist[ea]|semesterplan|studieplan|tilleggsstoff))? ?",
         "(?:vil )?legges ut (?:på|i) (?:blackboard|(?:den digitale |studiets )?læringsplattform\\w*)[^.]{0,60}"),
  "den kan også søkes opp på innsida",
  "oppgis senere",                                                    # ntnu
  "(?:pensum-/)?litteraturliste(?:n)? er ikke publisert ennå",        # usn
  "ikke bestemt", "ingen så langt",                                   # usn
  "the reading list is not published yet",                            # usn
  "litteratur(?:lista|listen)? vil være klart? [^.]{0,80}",           # hiof
  "(?:pensumlist[ea]|anbefalt litteratur) for (?:høsten|våren) ?\\d{4}(?:(?: og |-)(?:høsten|våren) ?\\d{4})?",  # nih
  "ettersom vi er i en overgangsfase mellom to systemer .{0,300}",    # nih
  # nih's English pages (2026-11-25): the same notice, "No subjects (in the
  # program / at program level)." in prerequisites, a list label without
  # its year ("Core reading for.", "Reading list for")
  "as we are in a transition phase between two systems,? .{0,300}",
  "no subjects?(?: in the program(?:me)?| at program(?:me)? level)?", "(?:core reading|reading list) for",
  # mf: the library-access notice alone (~600 chars), no list
  "litteraturlisten for .{0,30}tilgang til litteratur .{0,700}",
  # the reading list's label alone, its list a link or gone (2026-11-26:
  # "Pensum.", "Litteraturliste", nih; "Pensumliste", "Pensumlitteratur",
  # ntnu; "Pensum", uit; "Litteraturliste", uib; "Reading list", hivolda)
  "pensum(?:liste|litteratur)?", "litteraturliste", "reading list",
  # nhh's page template left unfilled (the test course TEST "Testemne",
  # 2026-11-26): its page is then a shell, with no plan
  "læringsutbytte på bokmål", "tekst om hva du lærer"
)
# Praksis pointers and the bare "Praksis" label, left in teaching_methods
# since Praksis maps there, and in practice, which a pointer alone does not
# make. Lines that say something ("Det er tre
# uker praksis i 6. semester. Se ...") stay.
.praksis_pointer_regex <- paste0(
  "(?im)^[ \\t]*(?:praksis:?|(?:praksis: )?for nærm(?:ere|are) informasjon om ",
  "praksis, (?:se|sjå) ei?gen praksisplan\\.?|se (?:egen|eiga|egne) (?:praksisplan|",
  "plan for praksis|emneplaner om praksis)[^.\\n]{0,80}\\.?|",
  "det er ingen praksis knyt(?:tet|t) til emnet\\.?)[ \\t]*(?:\\n|$)")

# A line that only says the course has no practice ("Det er ingen veiledet
# praksis i emnet.", "Det er ikkje knytt praksis til emnet.", hiof) or points
# to the practice plan ("Se praksisplan for MGLU5.", "Her henvises til egen
# praksisplan for grunnskolelærerutdanningen.", "Sjå relevante
# læringsutbytteformuleringar og elles eigen praksisplan.", "For studenter i
# årsstudiet: Se studieplan.", nla; "Se studieprogrambeskrivelse.", "Se
# Kompetanseguiden og emnebeskrivelse for praksis 1. studieår", nord;
# "Fremgår av studieprogramplan.", "Se egen emneplan for praksis for en
# nærmere beskrivelse.", usn) makes
# no practice section; it stays in
# teaching methods.
.no_practice_regex <- local({
  none <- paste0("(?:det er )?(?:ingen|ikke|ikkje)(?: knytt(?:et)?)? (?:veiled(?:et|a) )?praksis(?:studier|opplæring)?",
                 "(?: (?:er )?(?:knytt(?:et)? )?(?:til|i) (?:dette )?(?:emnet|studiet))?|ingen krav til praksis")
  pointer <- paste0("(?:for (?:nærm\\w+ )?info(?:rmasjon)? om praksis\\w*,? )?(?:for )?(?:studen\\w+ (?:i|på|som) [^:.\\n]{0,80}?(?:[:,]| -) ?)?(?:",
                    "(?:se|sjå|seg) (?:også )?(?:relevante læringsutbytteformuler\\w+ og ell?ers )?|",
                    "(?:her |det )?henvises (?:ellers )?til |(?:studen\\w+ som [^.\\n]{0,80}?)?følg(?:er|jer) )",
                    "(?:egen |eigen |eiga )?(?:plan )?(?:praksisplan(?:en)?|(?:emne)?plan for praksis\\w*|studieplan(?:en)?|",
                    "studieprogrambeskrivelse\\w*|kompetanseguiden(?: (?:og|for) [^\\n]{0,80}?)?)",
                    "(?: (?:for|foe|i) [^.\\n]{0,80})?(?: for en nærmere beskrivelse)?|",
                    "(?:fremkommer|fremgår|framgår) av (?:studie)?programplan(?:en)?|",
                    "emneplanen skal gjennomføres i samsvar med emneplan i praksis")
  paste0("(?im)^[ \\t]*(?:(?:", none, "|", pointer, ")\\.?[ \\t]*)+(?:\\n|$)")
})

# The course evaluation (course_plan_spec.md: plan text in no section) as a
# paragraph under its label at the end of the teaching methods: inn's
# "Emneevaluering – kvalitetssystem", uis's "Emneevaluering". The label and
# the paragraph after it go; uit's coursework item "Emneevaluering" stays.
.evaluation_part_regex <- paste0(
  "(?im)\\n?^[ \\t]*emneevaluering(?:[ \\t]*[-–][ \\t]*kvalitetssystem)?[ \\t]*:?[ \\t]*\\n",
  "(?:[ \\t]*\\n)*(?:[^\\n]*\\S[^\\n]*(?:\\n|$))*")
# The course evaluation in sentences of its own in the teaching methods
# (uit, 2026-11-16, some 10,000 plans): the label "Kvalitetssikring (av
# emnet)" / "Quality assurance" alone or before the notice, and sentences
# whose subject is the course, the courses or the evaluation itself ("Emnet
# evalueres i henhold til instituttets retningslinjer.", "Alle emner
# evalueres en gang i løpet av programperioden.", "Programstyret avgjør
# hvilke emner som skal evalueres ...", "Evalueringen gjennomgås av faglærer
# og drøftes i programstyret.", "The course undergoes a midterm evaluation
# ..."). Not a grade ("Emnet evalueres med bokstavkarakterer."), an exam, or
# students evaluating their own or each other's work. Only uit: elsewhere
# the same words also describe the assessment ("Midtveisevalueringen
# gjennomføres som en minidisputas ...", nih; "Sluttevalueringen gjennomføres
# sammen med praksisveileder ...", usn).
.evaluation_label <- "(?:kvalitetssikring(?: av emnet)?|quality (?:assurance|control)(?: of (?:the )?course)?)"
.evaluation_label_regex <- paste0("(?im)^[ \\t]*", .evaluation_label, "[ \\t]*[:;]?[ \\t]*(?:\\n|$)")
.evaluation_sentence_regex <- paste0(
  "(?i)^\\W*(?:", .evaluation_label, "\\s*[:;]?\\s*)?",
  "(?![^.]*(?:karakter|grade|bestått|godkjent|eksam|exam|mappe|portfolio|innlevering))(?:",
  "(?:alle emne[rn]?|emnet|kurset|studiet|programmet|all courses|(?:the|this) (?:course|subject|program(?:me)?))\\s+",
  "(?:\\S+\\s+){0,4}?(?:\\S*evaluer(?:es|ast|t|te)\\b|(?:will |is to |is )?(?:be )?(?:\\S+ )?evaluated\\b|",
  "undergo(?:es)? (?:a|an) [^.]*evaluation|har (?:\\S+ ){0,2}evaluering|gjenstand for (?:\\S+ )?evaluering)|",
  "i henhold til kvalitets\\w* evalueres|every \\w+ year the (?:course|subject) is evaluated|",
  "(?:programstyret|studieprogramle\\w+|emnestyret|the (?:programme |program )?board|the study programme leader)\\b[^.]*evalu|",
  "denne gjennomgås av faglærer og drøftes|subjectid=\\d+&from(?:subject)?=0(?:#eval_emne)?\\s*$|",   # a link split at "?S"
  "(?:underveis|slutt|midtveis)?evaluering(?:en|ene)? (?:foretas|gjennomgås|gjennomføres)|",
  "(?:the )?(?:final |teaching )?evaluation (?:will take place|will be conducted|is normally undertaken|consists of)|",
  "meir informasjon om emneevaluering|oversikt over kva emne som skal evaluerast)")

# Placeholder lines before real text go: "Ingen" (required) and then the
# recommended prerequisites (hvl, uib), "." (nord), "Ingen" before usn's
# attendance block. So does a placeholder paragraph after a sentence, the
# empty part that follows the section's own (nla: "Ingen." from the Praksis
# part after the teaching methods); not in assessment, where "Ingen" may
# answer a label ("Hjelpemidler").
.strip_placeholder_lines <- function(text, trailing = TRUE) {
  lines <- stringr::str_split_1(text, "\n")
  real <- which(nzchar(trimws(lines)) & !.is_placeholder_text(lines))
  if (length(real) == 0) return(text)
  last <- max(real)
  if (!trailing || last == length(lines) || nzchar(trimws(lines[last + 1])) ||
      !grepl("[.!?]\\s*$", lines[last])) last <- length(lines)
  if (real[1] == 1 && last == length(lines)) return(text)
  paste(lines[real[1]:last], collapse = "\n")
}

# Possessive dashes and separators: a run of dashes could be split among
# the repeats in every way, so a long one with other text after it ("_____
# Generelt", 75 underscores) ran into PCRE's match limit, a warning and no
# match (uit 2017/2018, build of 2026-11-01). No other phrase begins with a
# separator or a dash, so the matches are the same.
.placeholder_regex <- paste0(
  "^(?:(?:", paste(.placeholder_phrases, collapse = "|"), ")[\\s.:;,]*+)+$"
)

.is_placeholder_text <- function(text) {
  grepl(.placeholder_regex, stringr::str_squish(tolower(text)), perl = TRUE)
}
