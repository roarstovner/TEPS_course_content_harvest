# R/extract_sections.R
# Sections of a course plan, from the page's blocks (R/blocks.R; #275).
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
#' learning outcomes; #240, #249); one naming another section switches to it;
#' an unmapped one returns to the section its heading opened and stays as a
#' line there; a `keep` sub-heading stays as the first line of its section.
#' Blocks between "start" and "end" (a <details>, an accordion item) hold a
#' section of their own; the section open around them continues after them.
#' Text goes to the open section. Text under the same section is merged in
#' document order.
#'
#' @param blocks Block table from page_blocks().
#' @return Tibble `section`, `raw_text`.
sectionize <- function(blocks) {
  sections <- list()
  chunks <- character()
  parent <- current <- NA_character_
  outer <- list()
  flush <- function() {
    if (!is.na(current) && length(chunks) > 0) {
      txt <- trimws(paste(chunks[nzchar(chunks)], collapse = "\n"))
      if (nzchar(txt)) sections[[current]] <<- c(sections[[current]], txt)
    }
    chunks <<- character()
  }
  for (i in seq_len(nrow(blocks))) {
    role <- blocks$role[i]
    text <- blocks$text[i]
    section <- blocks$section[i]
    if (role == "heading") {
      flush()
      parent <- current <- section
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
      if (is.na(section) || blocks$keep[i]) chunks <- text
    } else if (!is.na(current)) {
      chunks <- c(chunks, text)
    }
  }
  flush()
  .list_to_sections(sections)
}

#' Sections of one page
#'
#' sectionize() on the page's blocks. When the html reader finds fewer than 3
#' sections, the lines of extracted_text are tried instead, and kept if they
#' give more (#183). Then nord-style coursework lines move out of assessment
#' (`section_inline_coursework`) and .clean_sections() tidies the rows.
#'
#' @param blocks Block table from page_blocks().
#' @param text The page's extracted_text, for the fallback.
#' @param cfg Reader settings from .block_cfg().
#' @return Tibble `section`, `raw_text`.
page_sections <- function(blocks, text, cfg) {
  out <- sectionize(blocks)
  if (identical(cfg$reader, "html") && nrow(out) < 3) {
    fallback <- sectionize(.text_blocks(text, cfg))
    if (nrow(fallback) > nrow(out)) out <- fallback
  }
  if (isTRUE(cfg$inline_coursework)) out <- .split_inline_coursework(out)
  .clean_sections(out, cfg$institution)
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
