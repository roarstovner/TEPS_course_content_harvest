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
#' learning outcomes); one naming another section switches to it;
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
      labelled <- .is_label_heading(text)
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
      if (identical(section, current)) {
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
#' is filled from it (`section_pointer`), nord-style coursework lines move out
#' of assessment (`section_inline_coursework`), a prerequisites label in course
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
  if (isTRUE(cfg$inline_coursework)) out <- .split_inline_coursework(out)
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
#'     assessment and coursework (.section_noise).
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
  out$raw_text[rl] <- stringr::str_remove(   # hiof date stamp line
    out$raw_text[rl], "^\\s*Litteratur\\w*(?: er)? sist oppdater[^\\n]*\\s*")
  out$raw_text[rl] <- vapply(out$raw_text[rl], .strip_list_tabs, character(1),
                             USE.NAMES = FALSE)
  pre <- out$section == "prerequisites"
  out$raw_text[pre] <- vapply(out$raw_text[pre], .strip_admission_lines,
                              character(1), USE.NAMES = FALSE)
  tm <- out$section == "teaching_methods"
  out$raw_text[tm] <- stringr::str_remove_all(out$raw_text[tm], .praksis_pointer_regex)
  out$raw_text <- vapply(out$raw_text, .strip_leading_placeholder, character(1),
                         USE.NAMES = FALSE)
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
# til forskrift.").
.admission_line_regex <- paste0(
  "(?i)opptak skjer|frittstående|studierett|enkeltemne|tilgjengelig som valgfag|",
  "søke[rs]? (?:særskilt )?opptak|krever generell studiekompetanse|",
  "(?:forutsetter|forusetter|krever|krav om|godkjent) opptak|",
  "(?:\\bfore?beholdt?|begrenset til) (?:for )?student|",   # "Emnet er forbeholdt studenter ..."
  "^(?:(?:jf\\.?|jamfør|som|tilsvarende) )?opptakskrav|",   # pointers to the programme
  "^(?:se|sjå)\\b.*opptak|",
  "^(?:ved )?opptak (?:til|på) (?:\\S+ ){0,3}?\\S*(?:utdanning|program|studi|lektor|lærer|master)|",
  "^opptaksgrunnlag\\w*\\.?$|^spesielle vilkår$|",       # ntnu's admission heading
  "^generell studiekompetanse(?: eller (?:godkjent )?realkompetanse)?(?: etter [^.]*)?\\.?$"
)
.no_prerequisite_regex <- paste0(
  "(?i)^(?!.*(?:\\bmen\\b|\\bbør\\b|anbefal))",
  "(?:(?:det (?:kreves|er)|kurset har|emnet har) )?ingen\\b.*\\b(?:ut ?over|utenom)\\b.*opptak"
)
.real_prerequisite_regex <- paste0(
  "\\b[A-ZÆØÅ]{2,}[A-ZÆØÅ0-9-]*\\d{2,}|",
  "(?i:bestått|forkunnskap|studiepoeng|\\bstp\\b|\\bsp\\b|",
  "\\bbør\\b|anbefal|fordel|rå til|i tillegg|inklusive|bygger på)"
)

.is_admission_sentence <- function(s) {
  s <- sub("(?i)^forkunnskapskrav\\s*:\\s*", "", trimws(s), perl = TRUE)
  code <- gsub("\\(\\p{Lu}{3,}[\\p{Lu}0-9-]*\\)", "", s, perl = TRUE)   # programme: "(MGLU5-10)"
  grepl(.admission_line_regex, s, perl = TRUE) &
    !grepl(.real_prerequisite_regex, code, perl = TRUE) |
    grepl(.no_prerequisite_regex, s, perl = TRUE)
}

# Where a sentence ends: after . ! ? before a capital, not after an
# abbreviation ("Jf. Opptakskrav ...").
.sentence_end <- paste0("(?<=[.!?])(?<!\\b(?i:jf|jfr|jamfør|ca|nr|kap|bl\\.a|f\\.eks|evt|osv)\\.)",
                        "(?=\\s+\\p{Lu}|\\p{Lu}\\p{Ll})")

# Admission sentences go from their lines. A colon-ended admission line (or
# "Spesielle vilkår") takes what it introduces (a programme list: the rest of
# its paragraph, or the next one), and a label whose text all went goes too
# ("Forkunnskapskrav:" over "Som opptakskrav til studieprogrammet, se ...").
.strip_admission_lines <- function(text) {
  lines <- stringr::str_split_1(text, "\n")
  n <- length(lines)
  gone <- logical(n)
  for (k in seq_len(n)) {
    s <- stringr::str_split_1(lines[k], .sentence_end)
    a <- .is_admission_sentence(s)
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
  paste(lines[!gone], collapse = "\n")
}

# Lines in an assessment block that state a coursework gate. nord
# writes them as "Arbeidskrav (AK): ..." or "Obligatorisk deltakelse (OD): ..."
# inside one undifferentiated vurdering block, with no heading to split on;
# uit as "Følgende arbeidskrav må være godkjent før ...:" and a list.
.inline_coursework_regex <- paste0(
  "(?i)^\\s*(?:(?:(?:inntil|minst|opptil) )?\\d+ (?:obligatoriske )?arbeidskrav|",
  "følg(?:ende|jande) (?:obligatoriske )?arbeidskrav|arbeidskrav(?:ene|et)?|",
  "(?:AK|OD|OA)\\d+\\b|",                       # nord: AK1, OD2 under their label
  "obligatoriske? (?:deltakelse|deltagelse|deltaking|arbeid|arbeidskrav|",
  "aktivitet(?:er)?|oppmøte|frammøte|fremmøte)|",
  "deltakelse i undervisning(?:en)? er obligatorisk)\\b"
)

# Move coursework-gate lines from assessment into coursework_requirements. A
# gate line that is a label takes what it names along. A colon-ended lead-in
# ("Følgende arbeidskrav må være godkjent ...:", uit) takes the rest of its
# paragraph, or the next paragraph when it ends its own. A label without one
# ("Obligatorisk deltakelse (OD)", nord) takes the next paragraph unless that
# is a label too ("Eksamen"), and the paragraphs after it that still speak of
# a gate ("Det er derfor obligatorisk oppmøte ..."). Both stop at the next
# assessment component ("Hjemmeeksamen (HJ): ...").
.split_inline_coursework <- function(out) {
  i <- which(out$section == "assessment")
  if (length(i) != 1) return(out)
  lines <- stringr::str_split_1(out$raw_text[i], "\n")
  gate <- grepl(.inline_coursework_regex, lines, perl = TRUE)
  if (!any(gate)) return(out)
  # "Arbeidskrav: ... Vurdering: ..." on one line (uit master theses)
  lines <- unlist(lapply(seq_along(lines), function(k) if (gate[k])
    stringr::str_split_1(lines[k], .assessment_midline_regex) else lines[k]))
  gate <- grepl(.inline_coursework_regex, lines, perl = TRUE)
  blank <- !nzchar(trimws(lines))
  label <- .is_label(lines)
  colon <- grepl("[:：]\\s*$", lines)
  stop <- !gate & grepl(.component_label_regex, lines, perl = TRUE)
  n <- length(lines)
  for (k in which(gate & (label | colon))) {
    from <- which(!blank & seq_len(n) > k)[1]
    if (is.na(from) || stop[from] || (!colon[k] && label[from])) next
    repeat {
      to <- which((blank | stop) & seq_len(n) > from)[1]
      gate[from:(if (is.na(to)) n else to - 1)] <- TRUE
      if (colon[k] || is.na(to)) break
      from <- which(!blank & seq_len(n) > to)[1]
      if (is.na(from) || stop[from] || label[from] ||
          !grepl(.coursework_leadin, lines[from], perl = TRUE)) break
    }
  }
  gate[blank] <- FALSE
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
  "Eksamensform):[ \t]*(?!(?:Godkjent|Bestått)\\b)\\S|EKSAMEN\\b|",
  "Emnet (?:evalueres|vurderes)\\b)|(?<=\\S) (?=Eksamen består av)")

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
  # a pointer to the AI page, after the AI rule or alone
  nord = "[ \t]*Informasjon om ChatGPT/kunstig intelligens[^\n]{0,20}? finner du her\\.?",
  uit  = c("(?m)^Eksamensdato er foreløpig.*$",
           # the grading cell of each row of the 2020s gate table, the same
           # for every gate; the gate's name stays
           "\tKarakterregel:[ \t]*\nGodkjent – ikke godkjent[ \t]*")
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
  "[-–.…]+",
  "me(?:i)?r om hjelpemid(?:del|ler)",                               # hvl link
  # pointers to another document
  "(?:se|sjå) (?:fag|program|studie)?plan(?:en)?(?: for [^.]{0,100})?",
  "se emnearkivet",
  "(?:se|sjå) forkunnskapskrav(?:et|a|ene)?",       # ntnu: the recommended field points to the required
  "se (?:kursinnhold|emnebeskrivelse)(?:et|n)?",                     # nord intro
  "ingen emner(?: i programmet)?",
  # the plan is on the English page (hvl, nord, ntnu; `english_page` in the config)
  "see english version", "se engelsk beskrivelse",                    # ntnu
  "emnebeskrivelsen finnes kun på engelsk[^.]*",
  "gå til våre engelske nettsider[^.]*",                              # hvl, after a full stop
  "(?:dette )?emne[tr]? er (?:kun beskrevet|berre skildra) på engelsk",
  "klikk på (?:knappen )?[\"“”]?english[\"“”]?[^.]*",
  "(?:emnet foregår på engelsk\\. )?se engelske nettsider[^.]*",
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

# Placeholder lines before real text go: "Ingen" (required) and then the
# recommended prerequisites (hvl, uib), "." (nord), "Ingen" before usn's
# attendance block.
.strip_leading_placeholder <- function(text) {
  lines <- stringr::str_split_1(text, "\n")
  real <- which(nzchar(trimws(lines)) & !.is_placeholder_text(lines))
  if (length(real) == 0 || real[1] == 1) return(text)
  paste(lines[real[1]:length(lines)], collapse = "\n")
}

.placeholder_regex <- paste0(
  "^(?:(?:", paste(.placeholder_phrases, collapse = "|"), ")[\\s.:;,]*)+$"
)

.is_placeholder_text <- function(text) {
  grepl(.placeholder_regex, stringr::str_squish(tolower(text)), perl = TRUE)
}
