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
#' line there; a `keep` sub-heading stays as the first line of its section.
#' A `label` heading or sub-heading ("Hjelpemidler til eksamen") stays as the
#' first line of its text; with no text, or only a dash or pointer ("-", "Se
#' fagplanen."), it goes. "Ingen" stays: no aids is a value.
#' Blocks between "start" and "end" (a <details>, an accordion item) hold a
#' section of their own; the section open around them continues after them.
#' Text goes to the open section. Text under the same section is merged in
#' document order. The title and facts box (`facts` blocks) are in no
#' section.
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
  flush <- function() {
    if (!is.na(current) && length(chunks) > 0 && !(labelled && .empty_label_text(chunks[-1]))) {
      txt <- trimws(paste(chunks[nzchar(chunks)], collapse = "\n"))
      if (nzchar(txt)) sections[[current]] <<- c(sections[[current]], txt)
    }
    chunks <<- character()
    labelled <<- FALSE
  }
  for (i in seq_len(nrow(blocks))) {
    role <- blocks$role[i]
    text <- blocks$text[i]
    section <- blocks$section[i]
    if (role == "heading") {
      flush()
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
      if (identical(section, current) ||
          (identical(current, "learning_outcomes") && identical(section, "reading_list"))) {
        chunks <- c(chunks, text)
        next
      }
      flush()
      current <- if (is.na(section)) parent else section
      labelled <- !is.na(section) && .is_label_heading(text)
      if (is.na(section) || blocks$keep[i] || labelled) chunks <- text
    } else if (!is.na(current)) {
      chunks <- c(chunks, text)
    }
  }
  flush()
  .list_to_sections(sections)
}

# The text under a label heading says nothing: none, a dash or a pointer.
.empty_label_text <- function(x) {
  v <- stringr::str_squish(paste(x, collapse = " "))
  !nzchar(v) || (.is_placeholder_text(v) && !grepl("^(?i:ingen|none)\\b", v, perl = TRUE))
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
  if (identical(cfg$reader, "text")) blocks <- .text_blocks(text, cfg)
  out <- sectionize(blocks)
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
  .clean_sections(out, cfg$institution)
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
#'   - strips trailing FS page-generation timestamps,
#'   - drops a leading line that merely repeats the section's own heading,
#'   - removes rows that are empty or only a placeholder ("Ingen",
#'     "Se fagplanen.", "-", a Leganto pointer; see .placeholder_phrases).
#'   - strips page widgets, pointers and stock logistics sentences from
#'     assessment and coursework (.section_noise), and approval stamps,
#'     logistics and cost sentences from every section
#'     (.approval_sentence_regex, .logistics_sentences).
#' Fuzzier cleanup (exam tables, arbeidskrav/eksamen boundaries inside one
#' paragraph) is left to the later Phase-2 LLM lifting.
.clean_sections <- function(out, institution = NULL) {
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
  if (nrow(out) == 0) return(out)
  exam <- out$section %in% c("assessment", "coursework_requirements")
  out$raw_text[exam] <- .strip_section_noise(out$raw_text[exam], institution)
  rx <- if (!is.null(institution)) .logistics_sentences[[institution]]
  if (!is.null(rx)) out$raw_text <- vapply(out$raw_text, .strip_sentences, character(1),
                                           rx = rx, USE.NAMES = FALSE)
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
  tm <- out$section == "teaching_methods"
  out$raw_text[tm] <- stringr::str_remove_all(out$raw_text[tm], .praksis_pointer_regex)
  out$raw_text[tm] <- stringr::str_remove_all(out$raw_text[tm], .evaluation_part_regex)
  out$raw_text <- mapply(.strip_placeholder_lines, out$raw_text,
                         trailing = out$section != "assessment", USE.NAMES = FALSE)
  out$raw_text <- mapply(.clean_section_text, out$raw_text, out$section,
                         USE.NAMES = FALSE)
  keep <- mapply(.keep_section_row, out$raw_text, out$section,
                 USE.NAMES = FALSE)
  out[keep, , drop = FALSE]
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
  "(?i:bestått|forkunnskap|studiepoeng|\\bstp\\b|\\bsp\\b|",
  "\\bbør\\b|anbefal|fordel(?!er )|rå til|i tillegg|inklusive|bygger på|",
  "\\bpassed\\b|completed|prior knowledge|recommend|advantage|\\bECTS\\b|credits|in addition)"
)

.is_admission_sentence <- function(s) {
  s <- sub("(?i)^forkunnskapskrav\\s*:\\s*", "", trimws(s), perl = TRUE)
  code <- gsub("\\(\\p{Lu}{3,}[\\p{Lu}0-9-]*\\)", "", s, perl = TRUE)   # programme: "(MGLU5-10)"
  grepl(.admission_line_regex, s, perl = TRUE) &
    !grepl(.real_prerequisite_regex, code, perl = TRUE) |
    grepl(.no_prerequisite_regex, s, perl = TRUE)
}

# The prerequisites in a block that mixes them with admission (nord's
# "Adgangsregulering", `section_headings` ".admission"): its sentences that
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
.admission_prerequisites <- function(text, prerequisites = "") {
  had <- stringr::str_squish(prerequisites)
  lines <- vapply(.unwrap_lines(stringr::str_split_1(text, "\n")), function(l) {
    s <- sub("(?i)^(\\s*)opptakskrav\\s*:\\s*", "\\1", stringr::str_split_1(l, .sentence_end), perl = TRUE)
    k <- grepl(.real_prerequisite_regex, s, perl = TRUE) & !.is_admission_sentence(s) &
      !grepl(paste0("(*UCP)(?i)^\\W*(?:søkere?|applicants?)\\b|realkompetanse|studieplass|prioriter|kvote|",
                    "upper secondary|^\\W*(?:i tillegg må |eventuelle )?forkunnskapskrav\\w* (?:til emnet )?(?:må )?(?:være|vera) oppfylt"),
             s, perl = TRUE) &
      !vapply(stringr::str_squish(s), function(x) !nzchar(x) || grepl(x, had, fixed = TRUE), logical(1))
    trimws(paste(s[k], collapse = ""))
  }, character(1), USE.NAMES = FALSE)
  paste(lines[nzchar(lines)], collapse = "\n")
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
  "(?-i:AK|OD|OA)\\d*|",                         # nord: AK, OD2 as their label
  "obligatoriske? (?:deltakelse|deltagelse|deltaking|arbeid|arbeidskrav|",
  "aktivitet(?:er)?|oppmøte|frammøte|fremmøte)|",
  "deltakelse i undervisning(?:en)? er obligatorisk|coursework(?: requirements?)?|",
  "(?:compulsory|mandatory) (?:participation|attendance|work|assignments?|",
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
# endelig karakter i emnet.", "... før ein kan få oppgåva vurdert.", "...
# before the student can submit the semester paper." Not a row of an exam
# table (tabs), nor a sentence about the exam's own parts ("Begge
# deleksamener må være bestått ...", "Mappen må være godkjent ...", "alle
# delene må være godkjent ... for å få endelig karakter", "... er deleksamen
# som må være godkjent ...", "få godkjent vurderingsmappen"),
# nor one without a subject ("Må være godkjent for å ...", which names its
# gate in the sentence before).
.gate_sentence_regex <- paste0(
  "(*UCP)(?is)^(?=[^\\t]*$)(?!.*\\b(?:(?:alle|begge) (?:del(?:er|ene|ar|ane)|\\w*eksam\\w*)|er (?:en )?deleksamen)\\b)",
  "(?=.*\\b(?:(?:godkje?n|approv)\\w*+(?!\\W+(?:\\w+\\W+)?(?:e(?:ks|x)am|\\w*vurdering|\\w*mappe))|arbeidskrav|obligatori|compulsory|mandatory|coursework|øving|",
  "øvelse|oppmøte|frammøte|fremmøte|tilstedeværelse|nærvær|attendance|participation|assignments?\\b|",
  "homework|exercises?\\b|passing grade))",
  "(?!\\W*(?:må|skal|must)\\b)(?!\\W*(?!(?:for (?:å|at)|in order|before|to)\\b)(?:\\w+\\W+){0,4}?(?:\\w*eksam|\\w*exam|delvurdering|",
  "del(?:er|ene|ar|ane)\\b|parts?\\b|mappe|portfolio)\\w*\\W+(?:\\w+\\W+){0,6}?(?:må|skal|must|ha(?:ve|s) to)\\b)",
  "(?=.*(?:\\b(?:må|skal)\\b.{0,150}\\b(?:før|innan|innen)\\b.{0,60}(?:eksam|vurder|sluttprøv)|",
  "\\b(?:må|skal|(?:kreves|krevs|krav)(?![^\\t]*praksis))\\b.{0,150}\\bfor (?:å|at|(?:fram|frem)stilling til)\\b.{0,80}(?:eksam|vurder|\\b(?:inn)?lever|framstill|fremstill|karakter)|",
  "\\bfor (?:å|at)\\b.{0,60}\\b(?:gå opp|framstill|fremstill|avlegg|lever|vurder|få (?:\\w+ ){0,2}vurder)\\w*.{0,60},? (?:må|skal)\\b|",
  "\\b(?:må|skal)\\b.{0,150}\\bfor (?:å|at) (?:bestå\\b|(?:emnet|faget) skal (?:være|vere|kunne) bestått|",
  "få (?:emnet|faget) bestått)|",
  "\\b(?:tilgang|adgang|rett|vilkår|forutsetning|føresetnad)\\b (?:til|for) (?:å )?(?:\\w+ ){0,4}?\\w*(?:eksam|prøve)|",
  "\\b(?:must|ha(?:ve|s) to|required?|need|prerequisite|condition)\\b.{0,150}\\b(?:before|prior to|",
  "in order to|allowed to|admitted to|for permission to|eligible|qualif\\w*)\\b.{0,60}\\b(?:\\w*exam|assessment|submit)|",
  "\\b(?:before|in order to)\\b.{0,60}\\bexam\\w*,? .{0,60}\\b(?:must|need|have to|has to|required?)\\b))")

# Compulsory attendance stated alone, without a condition: "Deltakelse på 80
# % av undervisning.", "70 % obligatorisk deltakelse på seminarundervisning",
# "Oppmøte undervisning: minimum 80%." (nord, uit, ntnu). The spec puts it in
# coursework requirements. Not attendance in praksis or clinical studies,
# which is how a practice course is assessed ("For å bestå praksis må
# studenten ... ha 100 % oppmøte"). With "Det er" before it, an obligation
# says it is required ("Det er obligatorisk deltakelse på samlingene.", "Det
# er krav om 90 % deltakelse på samlingene.", phs); so does "All
# undervisning er obligatorisk". Also, in a sentence of up to 250 characters
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
# medfører at ...", "Dersom fravær overstiger 20 prosent, ..."). Not a list
# of teaching forms with attendance in it ("Forelesninger, teoriøvinger og
# laboratoriearbeid med obligatorisk tilstedeværelse.", "Undervisning og
# læring skjer gjennom selvstudier, forelesninger, obligatorisk deltakelse i
# seminargrupper, ..."), which describes the teaching, unless it opens with
# the requirement ("Det kreves minimum 80 % deltakelse på ekskursjoner,
# laboratoriearbeid og ...").
.teaching_form <- paste0("(?:forelesning|førelesing|lectures?|seminar|øving|øvelse|laboratori|lab\\b|gruppearbeid|",
                         "selvstudi|sjølvstudi|veiledning|rettleiing|workshops?|verksted|feltarbeid|feltundervisning|",
                         "ekskursjon|presentasjon|diskusjon|demonstrasjon|prosjekt|foredrag|undervisning|samling|",
                         "kollokvi|basisgrupp|ferdighetstrening|practical work|group work)")
.attendance_word <- "(?:\\w*(?:deltak|deltag|oppmøt|frammøt|fremmøt|tilstedevær|nærvær)\\w*|attendance|participation)"
.attendance_sentence_regex <- paste0(
  "(*UCP)(?i)^\\s*(?!.*(?:praksis|praktiske studier|placement|internship|professional training|klini[sk]|clinical|",
  "pasient|læringsutbytt|learning outcome))(?:(?=[^\\t]{0,120}$)(?:",
  "alle? (?:undervisning|samling|seminar|forelesning)\\w* er obligatorisk\\w*\\b[^.]{0,80}\\.?\\s*$|",
  "(?:det er (?=[^.]*(?:obligatorisk|krav))|(?=.*(?:\\d ?(?:%|prosent)|\\balle?\\b)))",
  "(?:(?:obligatorisk|mandatory|compulsory|krav (?:om|til)|minimum|minst|min\\.)\\s+)*",
  "(?:(?:\\d+|hundre) ?(?:%|prosent)\\s+(?:\\w+\\s+){0,2}?)?(?:å delta|deltakelse|deltaking|deltagelse|",
  "oppmøte|frammøte|fremmøte|tilstedeværelse|nærvær|attendance|participation)\\b[^.]{0,80}\\b(?:undervisning|",
  "seminar|forelesning|samling|teaching|lecture|class|kurs|workshop|øving|lab)\\w*[^.]{0,40}\\.?\\s*$)|",
  "(?=[^\\t]{0,250}$)(?:",
  # a rule for absence beyond a limit, also with the exam as its sanction
  "\\W*(?:(?:ved|dersom|hvis|if)\\s+)?(?:\\w+\\s+){0,2}?(?:fravær|absence)\\w*\\b[^.]{0,30}\\b",
  "(?:over|utover|overstig\\w*|overskrid\\w*|mer enn|more than|exceed\\w*|på mellom|beyond)\\b|",
  "(?!.*(?:\\b(?:ikke|ikkje|not|no|ingen|uten|utan|valgfri\\w*|frivillig\\w*|optional|",
  "uavhengig|anbefal\\w*|recommended)\\b|(?<!studium og )\\b\\w*(?:eksam|exam)(?!enskrav|ple)|",
  "(?<!\\bi )(?<!\\bpå )\\b(?:emnet|faget|kurset|the course|the subject) (?:er|is) (?:\\w+ )?(?:obligatorisk|compulsory|mandatory)))",
  "(?!(?!\\W*(?:det|obligatorisk|krav|mandatory|compulsory|alle?|\\d+ ?%|eksamenskrav|arbeidskrav|for å|i tillegg er det|",
  .attendance_word, ")\\b).*", .teaching_form, "\\w*(?:\\s*\\([^)]*\\))?,\\s*(?:\\w+\\s+){0,3}?", .teaching_form, ")(?:",
  ".*\\b(?:(?:obligatorisk\\w*|pliktig\\w*|mandatory|compulsory|krav|kreves|krevs|required)",
  "(?:\\s+(?:om|til|derfor|minimum|minst|min\\.|full|obligatorisk\\w*|mandatory|compulsory|\\d+ ?(?:%|prosent)|at least))*",
  "\\s+", .attendance_word, "|",
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
  if (!any(cont)) return(lines)
  vapply(split(lines, cumsum(!cont)), paste, character(1), collapse = "\n", USE.NAMES = FALSE)
}

# A gate line that heads a block of its own: "OBLIGATORISKE ARBEIDSKRAV",
# "Obligatoriske aktiviteter/arbeidskrav:", "Arbeidskrav" (ntnu's "Mer om
# vurdering"), "Compulsory assignments", "Coursework requirements:"; a gate
# line by itself, so not nord's "Arbeidskrav (AK):".
.coursework_heading_regex <- paste0(
  "(?i)^\\s*(?:obligatoriske? \\w+(?:/\\w+)?|arbeidskrav(?:ene)?|coursework(?: requirements?)?|",
  "(?:course )?requirements|assignments|",
  "(?:compulsory|mandatory) \\w+(?: \\w+)?)\\s*[:：]?\\s*$")
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
      to <- which((stop | ends) & seq_len(n) > from)[1]
      block[from:(if (is.na(to)) n else to - 1)] <- TRUE
      gate <- gate | block
      next
    }
    if (!colon[k] && label[from]) next
    repeat {
      to <- which((blank | stop | ends) & seq_len(n) > from)[1]
      gate[from:(if (is.na(to)) n else to - 1)] <- TRUE
      if (colon[k] || is.na(to)) break
      from <- which(!blank & seq_len(n) > to)[1]
      if (is.na(from) || stop[from] || label[from] ||
          !grepl(.coursework_leadin, lines[from], perl = TRUE)) break
    }
  }
  gate[blank] <- FALSE
  design <- design & !gate
  lines[gate] <- orig[gate]
  out$raw_text[i] <- paste(lines[(!gate | both) & !design], collapse = "\n")
  out <- .append_section(out, "coursework_requirements",
                         paste(ifelse(gate, lines, aside)[gate | nzchar(aside)], collapse = "\n"))
  .append_section(out, "assessment", paste(lines[design], collapse = "\n"))
}

# Add text to a section, after a blank line, or as a new row.
.append_section <- function(out, section, text) {
  if (!nzchar(text)) return(out)
  j <- which(out$section == section)
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
# course before them ("Bestått didaktikkemne 1."), a prerequisite. fhs: a
# sentence in the course content that says only which courses it builds on
# ("Emnet bygger på ING1502 Matematiske metoder 1.", "The course is based on
# ING1501 ...") names prerequisites; one that goes on about the course ("...
# og skal gi studentene ...") or names no course code stays.
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
    list(list(from = "course_content", to = ".drop", rx = function(s) .uit_admission_sentence(s)))),
  fhs = list(list(from = "course_content", to = "prerequisites", rx = paste0(
    "^\\s*(?i:(?:dette )?emnet bygger (?:videre )?på|the course is based on) ",
    "(?=[^.]*\\b[A-ZÆØÅ]{2,}\\d{3,})(?![^.]*\\b(?:og|and) (?:skal|vil|tar|gir|legger|søker|ser|introduserer|aims?|will)\\b)",
    "(?!.*,\\s*$)"))),
  hivolda = list(list(from = "coursework_requirements", to = "prerequisites",
                      rx = "(?i)^\\s*bestått didaktikkemne \\d\\.?\\s*$"))
)
.move_sentences <- function(out, institution) {
  for (m in .sentence_moves[[institution %||% ""]]) {
    i <- which(out$section == m$from)
    if (length(i) != 1) next
    x <- .strip_sentences(out$raw_text[i], m$rx, taken = TRUE)
    if (!length(x$taken)) next
    out$raw_text[i] <- x$rest
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

.logistics_sentences <- list(
  hvl = paste0(
    "(?i)studentweb|",
    "^\\s*(?:tid|dato|frist|innleveringstidspunkt|innleveringsfrist)\\b[^\\n]*?",
    "(?:opplys|oppgi|oppgje|annonser|kunngj|publiser|blir gitt|vert gitt)|",
    "^\\s*eksamen\\w* (?:blir|vert) (?:avholdt|avhalde|halde|haldt)\\b[^\\n]*?",
    "\\b(?:januar|februar|mars|april|mai|juni|juli|august|september|oktober|november|desember)\\b|",
    "^(?![^\\n]*arbeidskrav)[^\\n]*(?:materialpeng|materialkostnad|kostpeng|e(?:i)?genandel|",
    "\\bkr\\.?[ \\t]?\\d|\\d[ \\t]?kr\\b)"
  ),
  phs = "(?i)^\\s*(?=.*\\btimer\\b)(?:[^\\s.]+\\s+){1,4}?er\\s+(?:totalt\\s+)?(?:anslått|estimert)\\s+til\\b"
,
  uia = .workload_sentence_regex, ntnu = .workload_sentence_regex, uit = .workload_sentence_regex
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
# them. With `taken`, a list of the text left (`rest`) and the sentences that
# went, a line each (`taken`).
.strip_sentences <- function(text, rx, taken = FALSE) {
  if (is.na(text)) return(if (taken) list(rest = text, taken = character()) else text)
  lines <- stringr::str_split_1(text, "\n")
  gone <- logical(length(lines))
  out <- character()
  for (k in seq_along(lines)) {
    s <- stringr::str_split_1(lines[k], .sentence_end)
    drop <- if (is.function(rx)) rx(s) else stringr::str_detect(s, rx)
    if (!any(drop)) next
    out <- c(out, trimws(paste(s[drop], collapse = "")))
    lines[k] <- trimws(paste(s[!drop], collapse = ""))
    gone[k] <- !nzchar(lines[k])
  }
  rest <- paste(lines[!gone], collapse = "\n")
  if (taken) list(rest = rest, taken = out) else rest
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
.is_heading_list <- function(text) {
  l <- trimws(stringr::str_split_1(text, "\n"))
  l <- l[nzchar(l)]
  length(l) >= 3 && all(nchar(l) <= 80) &&
    all(vapply(l, .heading_shaped_line, logical(1))) &&
    mean(!is.na(vapply(l, .exact_section, character(1)))) >= 0.5
}

# Phrases that say a section is absent or lives elsewhere. A row is a
# placeholder only when its WHOLE text is made of these phrases, so real
# content that merely starts with "Ingen" or mentions Leganto is kept.
.placeholder_phrases <- c(
  # bare negation / dummy values: "Ingen", "Ingen krav", "None", "x", "-", "..."
  "ingen(?: spesielle)?(?: krav| forkunnskapskrav| arbeidskrav)?", "nei", "none", "n/a", "x",
  "det er ingen (?:vurdering|eksamen) i emnet",                      # hvl
  "[-–.…_]++", "ingen info",                                         # hvl "_", oslomet
  # no coursework: "Ingen arbeidskrav i emnet.", "No coursework requirements."
  paste0("(?:det er )?ingen (?:obligatoriske )?(?:arbeidskrav|obligatoriske aktivitet\\w*)",
         "(?: ?(?:eller|og|/) ?(?:andre vilkår|obligatorisk\\w* aktivitet\\w*))?",
         "(?: (?:i|for|knytt\\w* til) (?:dette )?(?:emnet|studiet))?"),
  "no coursework requirements(?: or compulsory activities)?",
  # a bare yes: coursework "Ja", "Ja, vert spesifisert i semesterplan ved semesterstart" (hvl)
  "ja(?:,? \\(?(?:vert |blir )?spesifiser\\w* i (?:semester|undervisnings)plan\\w* ved semesterstart\\)?)?",
  "me(?:i)?r om hjelpemid(?:del|ler)",                               # hvl link
  # pointers to another document
  "(?:se|sjå) (?:fag|program|studie)?plan(?:en)?(?: for [^.]{0,100})?",
  "se emnearkivet",
  "(?:se|sjå) forkunnskapskrav(?:et|a|ene)?",       # ntnu: the recommended field points to the required
  "se (?:kursinnhold|emnebeskrivelse)(?:et|n)?",                     # nord intro
  "ingen emner(?: i programmet)?",
  # the plan is on the English page (hvl, nord, ntnu; `english_page` in the config)
  # ntnu, some 60 wordings (2026-11-05): "Se engelsk versjon / See English
  # version", "Emnet undervises på engelsk. Se engelsk versjon for
  # emnebeskrivelse.", "Se emnets engelske nettside - International MSc"
  paste0("(?:/ ?)?(?:(?:dette )?(?:emnet|faget) undervises (?:kun )?på engelsk[.,]? ?|",
         "the subject is taught in english\\. ?|vennligst |for fullstendig emnebeskrivelse, )?",
         "(?:se|see|cf\\.)(?: derfor)? (?:the |den |emnets |the course's )?(?:engels\\w*|english)",
         "(?: (?:vers\\w*|vesjon|tekst|text|utgave|page|informasjon|website|nettside|emneside|kursside|",
         "(?:emne|kurs)?beskrivelse\\w*|description))?(?: (?:for|om) [^.]{0,40})?",
         "(?: [-–\u0096] international msc)?"),
  "se (?:emne|kurs)?beskrivelsen? på engelsk", "see (?:the description )?in english",
  "faglig innhold er beskrevet på engelsk", "undervises kun på engelsk",
  "you will find the course description in the english version",
  "emnebeskrivelsen? (?:finnes|er) kun (?:tilgjengelig )?på engelsk[^.]*",
  "gå til våre engelske nettsider[^.]*",                              # hvl, after a full stop
  "(?:dette )?emne[tr]? er (?:kun beskrevet|berre skildra) på engelsk",
  "klikk på (?:knappen )?[\"“”]?english[\"“”]?[^.]*",
  "(?:emnet foregår på engelsk\\. )?se engelske nettsider[^.]*",
  # reading-list pointers and absence notes
  "ingen pensumliste(?: tilgjengelig)?(?: for dette emnet)?",
  "no reading list(?: available)?(?: for this course)?",
  "(?:gjeldende )?litteraturliste for [^.]{0,20} finner du i leganto",
  "litteratur og faglige ressurser finn(?:er du|es) her",
  "follow this link for reading list and academic resources",         # nla
  "(?:(?:se|sjå) )?pensumlist[ea] for emnet (?:finn(?:er)? du )?her",  # hivolda
  "ta kontakt med (?:instituttet|fakultetet)(?:/(?:instituttet|fakultetet))?",
  "se (?:vår(?:e)? |fakultetets )?hjemmesid(?:e|en|er|ene)",
  # uit: "Contact the department/faculty.", the law courses' "Se fagbeskrivelse
  # med pensum", "Se vår hjemmeside: Fagplan med pensum."
  "(?:please )?contact the (?:departe?ment|faculty)(?: ?/ ?(?:departe?ment|faculty))?",
  "(?:se )?fag(?:beskrivelse|plan)(?:er)? med pensum(?:liste)?(?: ?for (?:\\d\\. )?avdeling)?",
  # uit: the list is on the learning platform or got from the bookshop
  "syllabus and reading list (?:are|will be) available in (?:fronter|canvas),? or by contacting the (?:responsible )?teacher or (?:the|a) student adviser",
  "(?:pensumlitteratur )?(?:oppgis|opplyses) (?:i (?:fronter|canvas|uits digitale læringsplattform) )?ved (?:semester|studie)start(?: eller (?:kan oppgis og kjøpes|fås) ved henvendelse til [^.]{0,80})?",
  "bøkene kan kjøpes på www\\.akademi[ak]a\\.no",
  "pensumliste(?:n)? (?:vil )?legges (?:vanligvis )?ut i (?:fronter|canvas),? eller kan fås ved henvendelse til emneansvarlig eller studiekonsulent",
  "literature will be announced in fronter at the beginning of term or may be obtained by contacting the (?:campus )?bookstore[^.]{0,60}",
  "se egen pensumliste for studieenheten i (?:fronter|canvas)",
  "available on request",                                             # uit 2008-2010
  "ny pensumbok vurderes",                                            # hiof
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
  # mf: the library-access notice alone (~600 chars), no list
  "litteraturlisten for .{0,30}tilgang til litteratur .{0,700}"
)
# Praksis pointers and the bare "Praksis" label, left in teaching_methods
# since Praksis maps there. Lines that say something ("Det er tre
# uker praksis i 6. semester. Se ...") stay.
.praksis_pointer_regex <- paste0(
  "(?im)^[ \\t]*(?:praksis:?|(?:praksis: )?for nærm(?:ere|are) informasjon om ",
  "praksis, (?:se|sjå) ei?gen praksisplan\\.?|se (?:egen|eiga|egne) (?:praksisplan|",
  "plan for praksis|emneplaner om praksis)[^.\\n]{0,80}\\.?|",
  "det er ingen praksis knyt(?:tet|t) til emnet\\.?)[ \\t]*(?:\\n|$)")

# The course evaluation (course_plan_spec.md: plan text in no section) as a
# paragraph under its label at the end of the teaching methods: inn's
# "Emneevaluering – kvalitetssystem", uis's "Emneevaluering". The label and
# the paragraph after it go; uit's coursework item "Emneevaluering" stays.
.evaluation_part_regex <- paste0(
  "(?im)\\n?^[ \\t]*emneevaluering(?:[ \\t]*[-–][ \\t]*kvalitetssystem)?[ \\t]*:?[ \\t]*\\n",
  "(?:[ \\t]*\\n)*(?:[^\\n]*\\S[^\\n]*(?:\\n|$))*")

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
