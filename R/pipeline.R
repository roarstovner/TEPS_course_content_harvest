# R/pipeline.R
# Steps of the {targets} pipeline in _targets.R (README "Rebuilding Derived
# Data"): every stored page is read once, per raw partition (institution x
# DBH year: data/raw/{inst}/{year}/); per institution, read_harvest() gives
# each offering the page it takes, without reading the pages; the offerings
# of all institutions then get their plans, and each frame (FRAMES) its data
# files. The harvest itself stays outside the pipeline (network, checkpoints).

# A column, or NA for each row where the data frame has none.
.chr <- function(df, col) df[[col]] %||% rep(NA_character_, nrow(df))

# --- Per raw partition -------------------------------------------------------
# A partition's steps work on its raw rows (read_raw(page_id = TRUE)), keyed
# by page id (raw_page_id()), whichever offerings take their page.

# Every page with HTML read once into blocks (R/blocks.R): page_id plus the
# block columns. Institutions read from text (usn, steiner) have no blocks
# here; their sections read the extracted_text. nla's reader takes the
# offering's academic year from the course id: its pages are only ever their
# own offering's.
partition_blocks <- function(files, config) {
  rows <- read_raw(files, page_id = TRUE)
  cfg <- .block_cfg(config)
  html <- .chr(rows, "html")
  read <- cfg$reader %in% c("html", "json") & !is.na(html) & nzchar(html)
  blocks <- lapply(which(read), function(i) page_blocks(html[i], NA, cfg, rows$course_id[i]))
  dplyr::bind_rows(tibble::tibble(page_id = character()), .empty_blocks(),
                   tibble::tibble(page_id = rep(rows$page_id[read], vapply(blocks, nrow, 1L)),
                                  dplyr::bind_rows(.empty_blocks(), blocks)))
}

# The text and plan of every page: extracted_text, the page's blocks as text,
# or for pages without blocks (PDF plans, usn's rendered text) as
# extract_fulltext_from_raw() gives it; plan_text, the text without the title
# and facts box where the config names them (`facts`) or, without blocks, the
# text opens with them (text_plan_text()), else NA; the page's plan
# (page_plans()); page_term, the start semester of the version the page
# shows (`page_term`, semester_term());
# english_pointer, whether the page says that the plan is on the English page
# (`english_page`: a page fetched before its notice was known).
partition_text <- function(files, blocks, config) {
  rows <- read_raw(files, page_id = TRUE)
  rows <- rows[!is.na(.chr(rows, "html")) | !is.na(.chr(rows, "extracted_text")), ]
  pages <- split(blocks[names(.empty_blocks())], blocks$page_id)
  text <- vapply(pages, page_fulltext, character(1), config = config)[rows$page_id]
  rest <- !rows$page_id %in% names(pages)
  text[rest] <- extract_fulltext_from_raw(rows[rest, ], config)
  plan_text <- rep(NA_character_, nrow(rows))
  if (!is.null(config$facts)) {
    plan_text <- vapply(pages, page_fulltext, character(1), config = config,
                        facts = FALSE)[rows$page_id]
  }
  plan_text[rest] <- text_plan_text(text[rest], .block_cfg(config))
  page_term <- if (is.null(config$page_term)) rep(NA_integer_, nrow(rows)) else
    as.integer(config$page_term(.chr(rows, "html")))
  notice <- config$english_page$notice
  tibble::tibble(page_id = rows$page_id, extracted_text = unname(text), plan_text = unname(plan_text),
                 page_plans(rows$institution, unname(text), unname(plan_text),
                            isTRUE(config$exam_sessions)),
                 page_term = page_term,
                 english_pointer = !is.null(notice) & grepl(notice %||% "", .chr(rows, "html"), perl = TRUE))
}

# A page's plan: course_plan, its text anonymized, and the plan id
# (plan_ids()), made from the plan text without the title and facts box
# where it is known (plan_text).
page_plans <- function(institution, text, plan_text, exam_sessions = FALSE) {
  institution <- rep_len(institution, length(text))
  course_plan <- anonymize_text(institution, text, .progress = FALSE)
  id_text <- course_plan
  facts <- !is.na(plan_text) & !is.na(course_plan)
  id_text[facts] <- anonymize_text(institution[facts], plan_text[facts], .progress = FALSE)
  tibble::tibble(course_plan = course_plan, plan_ids(id_text, exam_sessions))
}

# The sections of the pages a plan is cut from (`ids`, plan_source_pages()),
# anonymized: sections are cut from raw text. A season after a semester label
# is timing, not section text, and goes (.drop_semester_values()). The text
# fallback reads the plan text, without the title and facts box; pages read
# as text (no blocks) cut them in the reader (.text_header()).
partition_sections <- function(blocks, page_text, config, ids) {
  cfg <- .block_cfg(config)
  read <- blocks$page_id %in% ids
  pages <- split(blocks[read, names(.empty_blocks())], blocks$page_id[read])
  pt <- page_text[match(ids, page_text$page_id), ]
  text <- dplyr::if_else(ids %in% names(pages), dplyr::coalesce(pt$plan_text, pt$extracted_text),
                         pt$extracted_text)
  rows <- lapply(seq_along(ids), function(i) {
    out <- page_sections(pages[[ids[i]]] %||% .empty_blocks(), text[i], cfg)
    tibble::tibble(page_id = rep(ids[i], nrow(out)), out)
  })
  dplyr::bind_rows(tibble::tibble(page_id = character()), .empty_sections(), rows) |>
    dplyr::mutate(text = .drop_semester_values(
      anonymize_text(rep(config$name, dplyr::n()), raw_text, .progress = FALSE))) |>
    dplyr::filter(!is.na(text)) |>
    dplyr::select(page_id, section, text)
}

# The headings of each page: `what` "unmapped", a heading the section
# extractor cannot map (`value`: the heading), or "pattern", a heading-map
# pattern that opens a section (`value`: the pattern). Headings and
# sub-headings are read from the page, or for the text reader (usn, steiner)
# from the heading lines of extracted_text; the html reader's text fallback
# (uis PDF plans) is not counted.
partition_headings <- function(blocks, page_text, config) {
  cfg <- .block_cfg(config)
  if (identical(cfg$reader, "text")) {
    pt <- page_text[!is.na(page_text$extracted_text), ]
    pages <- lapply(pt$extracted_text, .text_blocks, cfg = cfg)
    blocks <- dplyr::bind_rows(tibble::tibble(page_id = character()), .empty_blocks(), tibble::tibble(
      page_id = rep(pt$page_id, vapply(pages, nrow, 1L)), dplyr::bind_rows(.empty_blocks(), pages)))
  }
  unmapped <- dplyr::filter(blocks, role == "heading", is.na(section), nzchar(trimws(text)))
  b <- blocks[blocks$role %in% c("heading", "sub") & !is.na(blocks$section), ]
  row <- vapply(stringr::str_squish(b$text), heading_pattern, integer(1),
                word_start = identical(cfg$reader, "text"), USE.NAMES = FALSE)
  # field headings (hivolda) take their section from the field, not the text
  keep <- !is.na(row) & section_heading_patterns$section[row] == b$section
  dplyr::distinct(dplyr::bind_rows(
    tibble::tibble(page_id = unmapped$page_id, what = "unmapped", value = stringr::str_squish(unmapped$text)),
    tibble::tibble(page_id = b$page_id[keep], what = "pattern",
                   value = section_heading_patterns$pattern[row[keep]])))
}

# Page ids of a partition (a row of raw_partitions()).
partition_page_ids <- function(ids, partition) {
  ids[startsWith(ids, paste0(partition$institution, "/", partition$year, "/"))]
}

# --- Per institution ---------------------------------------------------------

# The institution's config as a target value. Functions in it (post_fn,
# fetch_fn) are rebuilt from their source: R byte-compiles a function
# after a few calls, which changes its serialized form and with it the hash
# {targets} uses to decide what is outdated.
institution_config_target <- function(institution) {
  lapply(get_institution_config(institution), function(x) {
    if (is.function(x)) eval(parse(text = deparse(x)), envir = globalenv()) else x
  })
}

# One institution's raw harvest: the rows of all its raw files
# (harvest_files(), read_raw()). Each run adds offerings the earlier ones lack, so
# an offering in two runs is an error, unless the later one is a retry of an
# offering that gave no plan (harvest_all(retry = TRUE)): it then has
# the latest row with a page, else the latest row.
#
# A "current" site (plan_years) shows only the plan in force, so a page is the
# plan of its course in the academic year it was fetched in, whichever DBH row
# it was fetched for. Its files are snapshots: each offering gets the page of
# its course code (course_code_raw) from the latest harvest in the offering's
# academic year (autumn Y: Y/Y+1, spring Y: Y-1/Y), or no page;
# harvested_at says which harvest that was.
#
# The bytes of a PDF (`pdf`) are left out: the pipeline reads the text the
# harvest read from it (extracted_text).
#
# With `content = FALSE` the pages are not read: `html` and `extracted_text`
# hold the page id of the raw row whose page the offering takes (raw_page_id(),
# offering_pages()), and the pipeline reads the pages per raw partition.
read_harvest <- function(files, config, content = TRUE) {
  current <- identical(config$plan_years, "current")
  rows <- read_raw(files, run = TRUE, content = content)
  if (current) rows$harvested_at <- harvest_date(rows$.run, rows[["harvested_at"]])
  rows$.run <- NULL
  rows$pdf <- NULL
  if (!current) {
    retried <- rows$course_id[rows[["retry"]] %in% TRUE]
    dup <- setdiff(rows$course_id[duplicated(rows$course_id)], retried)
    if (length(dup)) stop(config$name, ": offerings in more than one raw file: ",
                          paste(utils::head(dup, 5), collapse = ", "))
    page <- rep_len(!is.na(rows[["html"]] %||% NA) | !is.na(rows[["extracted_text"]] %||% NA), nrow(rows))
    pick <- order(rows$course_id, !page, -seq_len(nrow(rows)))
    pick <- pick[!duplicated(rows$course_id[pick])]
    rows <- dplyr::select(rows[sort(pick), ], -dplyr::any_of("retry"))
    if (identical(config$url_year, "academic")) {
      rows <- if (identical(config$strategy, "html_pdf_discovery")) .menu_year_pages(rows) else
        .academic_year_pages(rows)
    }
    return(rows)
  }
  page <- c("html", "html_error", "html_success", "extracted_text", "harvested_at")
  snapshots <- rows |>
    dplyr::filter(!is.na(html) | !is.na(extracted_text)) |>
    dplyr::mutate(academic_year = academic_year_of_date(harvested_at)) |>
    dplyr::arrange(dplyr::desc(harvested_at), -seq_along(harvested_at)) |>   # same day: the later file
    dplyr::distinct(course_code_raw, academic_year, .keep_all = TRUE) |>
    dplyr::select(course_code_raw, academic_year, dplyr::any_of(page))
  # an offering's DBH columns from the first file that has it: a later file
  # never changes an earlier row
  rows |>
    dplyr::arrange(harvested_at) |>
    dplyr::distinct(course_id, .keep_all = TRUE) |>
    dplyr::select(-dplyr::any_of(page)) |>
    dplyr::mutate(academic_year = nla_academic_year(year, semester_name)) |>
    dplyr::left_join(snapshots, by = c("course_code_raw", "academic_year")) |>
    dplyr::select(-academic_year) |>
    dplyr::arrange(course_id)
}

# A site whose URL holds the academic year (`url_year = "academic"`): an
# offering takes the page at the URL add_course_url() gives it (spring 2025
# and autumn 2024: "/2024"), fetched for whichever offering; a page fetched
# for its own offering comes first. Harvests until 2026-10-13 put the
# calendar year in the URL of spring offerings too. An offering whose URL
# nobody fetched keeps that URL and no page ("no page" in plan_gaps()) until
# harvest_all(retry = TRUE) fetches it.
.academic_year_pages <- function(rows) {
  cols <- intersect(c("url", "html", "html_error", "html_success", "extracted_text"), names(rows))
  fetched <- as.character(rows$url)
  needed <- as.character(add_course_url(rows)$url)
  right <- !is.na(fetched) & fetched == needed
  pages <- which(!is.na(rows$html))
  pages <- pages[order(!right[pages], pages)]
  src <- pages[match(needed, fetched[pages])]
  own <- right & (!is.na(rows$html) | is.na(src))   # its page, or its 404
  src[own] <- which(own)
  out <- rows
  out[, cols] <- rows[src, cols]
  out$url <- needed
  out
}

# A site whose URLs come from the course page's semester menu
# (html_pdf_discovery: uis): an entry is an academic year, and its URL holds
# it (".../Stine/EMNE/2024/VÅR/....pdf" and "?semester=2024H" are 2024/25).
# An offering takes the page of its course whose URL holds its academic
# year, whichever offering it was fetched for, its own first. Harvests until
# 2026-10-14 matched the menu on the calendar year, so a spring offering got
# the next academic year's page; one whose page nobody fetched has no page,
# at the course page's URL, until harvest_all(retry = TRUE) fetches it. The
# course page (no year in its URL: years the menu lacks) stays with its
# offering, unless a page of the offering's academic year was fetched.
.menu_year_pages <- function(rows) {
  cols <- intersect(c("url", "html", "html_error", "html_success", "extracted_text"), names(rows))
  m <- stringr::str_match(rows$url, "/Stine/EMNE/(\\d{4})/|[?&]semester=(\\d{4})")
  year <- as.integer(dplyr::coalesce(m[, 2], m[, 3]))
  key <- paste(rows$course_code_raw, year)
  want <- paste(rows$course_code_raw, academic_start_year(rows$year, rows$semester_name))
  page <- !is.na(rows$html) | !is.na(rows$extracted_text)
  pages <- which(!is.na(year) & page)
  src <- pages[match(want, key[pages])]
  own <- dplyr::if_else(is.na(year), is.na(src), key == want & (page | is.na(src)))   # its page, or its 404
  src[own] <- which(own)
  out <- rows
  out[, cols] <- rows[src, cols]
  none <- is.na(src)
  if (any(none)) out$url[none] <- add_course_url(rows[none, ])$url
  out
}

#' Harvested rows with the current extracted_text
#'
#' read_harvest() for each institution, with `extracted_text` from
#' data/interim/extracted_text.RDS, which the {targets} pipeline rebuilds with
#' the current config (the text stored at harvest time is ignored). For the
#' audit scripts.
#'
#' @param institutions Institutions to read; all when empty.
harvested_rows <- function(institutions = NULL,
                           text_file = "data/interim/extracted_text.RDS") {
  if (!file.exists(text_file)) stop(text_file, " is missing: run targets::tar_make() first")
  found <- harvested_institutions()
  if (length(institutions)) found <- intersect(found, institutions)
  text <- readRDS(text_file)[, c("course_id", "extracted_text")]
  lapply(found, function(i) read_harvest(harvest_files(i), get_institution_config(i))) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of(c("extracted_text", "fulltext"))) |>
    dplyr::left_join(text, by = "course_id")
}

# How each offering's fetch went: its URL, whether a page (or PDF text) was
# stored, and the fetch error (e.g. "HTTP 404 Not Found."), for plan_gaps().
# `df`: the institution's offerings (read_harvest()).
institution_fetch_status <- function(df) {
  tibble::tibble(
    course_id = df$course_id,
    url = as.character(df$url %||% NA_character_),
    has_page = !is.na(df$html %||% NA) | !is.na(df$extracted_text %||% NA),
    fetch_error = fetch_error_text(df$html_error %||% vector("list", nrow(df)))
  )
}

# Course-years (institution x course_code x year) and why those without a plan
# have none: "no URL" (no page for that year, or not found by URL discovery),
# "404", "fetch error", "page, no plan" (a shell, not the plan, or a year
# missing from nla's JSON), "no page" (a current site without a harvest in
# that year). DBH lists most courses in both semesters, so a 404 for one
# semester is normal; a course-year is a gap only when no semester has a
# plan. `between_plans`: the course has plans the year before and after, the
# gaps most likely to be fixable (wrong URL, a missed page). `url_tried` and
# `url_next_door` (the page of an adjacent year with a plan) are for checking
# by hand.
plan_gaps <- function(offerings, fetch_status) {
  x <- offerings |>
    dplyr::select(course_id, institution, course_code, year, plan_content_id) |>
    dplyr::left_join(fetch_status, by = "course_id") |>
    dplyr::mutate(reason = dplyr::case_when(
      !is.na(plan_content_id) ~ "plan",
      is.na(url) ~ "no URL",
      grepl("404", fetch_error) ~ "404",
      !is.na(fetch_error) ~ "fetch error",
      has_page ~ "page, no plan",
      TRUE ~ "no page"))
  cy <- x |>
    dplyr::group_by(institution, course_code, year) |>
    dplyr::summarise(has_plan = any(reason == "plan"),
                     reason = if (any(reason == "plan")) "plan" else
                       paste(sort(unique(reason)), collapse = " + "),
                     url_tried = dplyr::first(url), .groups = "drop")
  with_plan <- x |> dplyr::filter(reason == "plan") |>
    dplyr::distinct(institution, course_code, year, .keep_all = TRUE)
  key <- function(i, e, y) paste(i, e, y)
  planned <- key(with_plan$institution, with_plan$course_code, with_plan$year)
  cy |>
    dplyr::mutate(
      between_plans = !has_plan & key(institution, course_code, year - 1) %in% planned &
        key(institution, course_code, year + 1) %in% planned,
      url_next_door = with_plan$url[match(key(institution, course_code, year + 1), planned)],
      url_next_door = dplyr::coalesce(url_next_door,
        with_plan$url[match(key(institution, course_code, year - 1), planned)])
    )
}

# Every offering of a DBH frame and how far the harvest got with it:
# `outcome`, how its URL answered last (offering_outcome(): an outcome class,
# "not requested", or "not harvested": in no raw run); `has_plan` in the
# build; `retry`: no plan, and harvest_all(retry = TRUE) would fetch it again
# (never at a site that shows only the current plan, which takes a new
# snapshot in each academic year instead).
fetch_coverage <- function(frame, ledger, offerings) {
  frame <- add_course_id(frame[!is.na(frame$institution), ])
  frame$course_id <- first_course_id(frame$course_id, unique(ledger$course_id))
  i <- match(frame$course_id, offerings$course_id)
  has_plan <- !is.na(offerings$plan_content_id[i])
  outcome <- offering_outcome(frame$course_id, as.character(offerings$url[i]), ledger)
  current <- vapply(unique(frame$institution), function(x)
    identical(get_institution_config(x)$plan_years, "current"), logical(1))
  tibble::tibble(institution = frame$institution, course_id = frame$course_id,
                 course_code = frame$course_code, year = frame$year,
                 semester_name = frame$semester_name, outcome = outcome, has_plan = has_plan,
                 retry = !has_plan & !unname(current[frame$institution]) & retry_wanted(outcome))
}

# Pages whose plan text changed between two harvest runs that fetched the
# same URL: a site that reworded an old page, a current site's plan of a new
# year. The last fetch of the URL in a run is compared with its last fetch in
# the run before that fetched it. A page whose stored body is the same
# (`sha256` in the ledger) has not changed; otherwise the text the pipeline
# reads from it (extract_fulltext_from_raw()) is compared, since a body
# changes on every fetch at some sites (Liferay's authToken at NTNU and
# OsloMet). One row per change: institution, url, and the run and offering
# of the fetch before and of the changed one.
page_changes <- function(ledger, files, config) {
  ok <- ledger[ledger$outcome == "ok" & !is.na(ledger$url), ]
  ok <- ok[!duplicated(ok[c("url", "run")], fromLast = TRUE), ]
  ok <- ok[order(ok$url, ok$run, method = "radix"), ]
  n <- nrow(ok)
  before <- seq_len(n) - 1L   # the URL's fetch in the run before
  before[before == 0L | c(TRUE, ok$url[-1] != ok$url[-n])] <- NA
  pairs <- tibble::tibble(institution = ok$institution, url = ok$url,
                          previous_run = ok$run[before], previous_course_id = ok$course_id[before],
                          run = ok$run, course_id = ok$course_id)
  pairs <- pairs[!is.na(before) & ok$sha256 != ok$sha256[before], ]
  if (!nrow(pairs)) return(pairs)
  rows <- read_raw(files[raw_run(files) %in% c(pairs$run, pairs$previous_run)], run = TRUE)
  key <- paste(rows$.run, rows$course_id)
  rows <- rows[key %in% c(paste(pairs$run, pairs$course_id), paste(pairs$previous_run, pairs$previous_course_id)), ]
  text <- stats::setNames(extract_fulltext_from_raw(rows, config), paste(rows$.run, rows$course_id))
  pairs[!mapply(identical, text[paste(pairs$run, pairs$course_id)],
                text[paste(pairs$previous_run, pairs$previous_course_id)]), ]
}

# Headings by the offerings whose page has them (partition_headings()):
# `what = "unmapped"`, the headings the section extractor meets but cannot
# map, candidates for R/section_heading_map.R (columns institution, heading,
# n_pages; most pages first); "pattern", the pages each heading-map pattern
# opens a section on (institution, pattern, n_pages).
heading_counts <- function(headings, page_map, what, institutions) {
  name <- if (what == "unmapped") "heading" else "pattern"
  x <- dplyr::inner_join(page_map, headings[headings$what == what, ], by = "page_id",
                         relationship = "many-to-many") |>
    dplyr::distinct(institution, value, course_id) |>
    dplyr::count(institution, value, name = "n_pages")
  x <- x[order(match(x$institution, institutions), -x$n_pages * (what == "unmapped")), ]
  names(x)[names(x) == "value"] <- name
  x
}

# Every heading-map pattern with the pages it opens a section on, over all
# institutions: a pattern on no page is a candidate for removal.
heading_pattern_use <- function(hits) {
  used <- hits |>
    dplyr::group_by(pattern) |>
    dplyr::summarise(n_pages = sum(n_pages),
                     institutions = paste(sort(institution), collapse = ", "), .groups = "drop")
  section_heading_patterns[, c("pattern", "section")] |>
    dplyr::left_join(used, by = "pattern") |>
    dplyr::mutate(n_pages = dplyr::coalesce(n_pages, 0L))
}

# --- All offerings and the frames ---------------------------------------------

# The page of each offering of read_harvest(content = FALSE) (`pages`, one
# data frame per institution): `page_id`, NA when it has none.
offering_pages <- function(pages) {
  dplyr::bind_rows(lapply(pages, function(x) tibble::tibble(
    course_id = x$course_id, institution = x$institution,
    page_id = dplyr::coalesce(.chr(x, "html"), .chr(x, "extracted_text")))))
}

# extracted_text and plan_text of every offering (partition_text()), for
# data/interim/extracted_text.RDS. `pages` and `config`: one per institution.
# Institutions are combined as {targets} combines branches (vctrs), so the
# file keeps its bytes.
offering_fulltext <- function(pages, page_text, config) {
  do.call(vctrs::vec_rbind, lapply(seq_along(pages), function(i) {
    t <- match(offering_pages(pages[i])$page_id, page_text$page_id)
    tibble::tibble(course_id = pages[[i]]$course_id, institution = config[[i]]$name,
                   extracted_text = page_text$extracted_text[t], plan_text = page_text$plan_text[t])
  }))
}

# Every offering with its text, course_plan and plan id (the plan of its page,
# page_plans()), before the shell rule: one row per offering of the raw store.
# An offering whose page shows a version of the plan that starts after the
# offering's semester (`page_term`; a page that names only an academic year
# counts as its autumn), or only says that the plan is on the English page
# (`english_page`), gets no plan.
plan_offerings <- function(pages, page_text, config) {
  page_cols <- c("extracted_text", "fulltext", "html", "html_error", "html_success", "url.x", "url.y")
  dplyr::bind_rows(lapply(seq_along(pages), function(i) {
    cfg <- config[[i]]
    t <- page_text[match(offering_pages(pages[i])$page_id, page_text$page_id), ]
    df <- dplyr::select(pages[[i]], -dplyr::any_of(page_cols))
    df$extracted_text <- t$extracted_text
    plan <- t[c("course_plan", "course_plan_normalized", "plan_content_id", "plan_nchar")]
    later <- (t$page_term > semester_term(df$year, df$semester_name)) %in% TRUE
    none <- page_plans(cfg$name, NA_character_, NA_character_, isTRUE(cfg$exam_sessions))
    for (col in names(plan)) plan[[col]][is.na(t$page_id) | later | t$english_pointer %in% TRUE] <- none[[col]]
    df$course_plan <- plan$course_plan
    df$plan_year_basis <- cfg$plan_years   # how the plan's year is known
    df$course_plan_normalized <- plan$course_plan_normalized
    df$plan_content_id <- plan$plan_content_id
    df$plan_nchar <- plan$plan_nchar
    df
  })) |>
    # columns only some institutions' raw rows have: in the order of
    # data/variables.csv, whichever institution has them first
    dplyr::relocate(dplyr::any_of(c("harvested_at", "academic_year")), .after = plan_nchar) |>
    dplyr::mutate(has_extracted_text = !is.na(extracted_text) & nchar(extracted_text) > 0,
                  extracted_text_nchar = nchar(extracted_text))
}

#' The offerings of each frame
#'
#' "all": every offering in the raw store; "teacher_ed": those in the
#' teacher-ed frame of the DBH snapshot (dbh_courses("teacher_ed")), matched
#' on the offering (offering_key(): a later snapshot can change the status).
#'
#' @param course_id The offerings (plan_offerings()).
#' @param date,input_dir The DBH snapshot (dbh_courses()).
#' @return A list by frame (FRAMES): whether each offering is in it.
frame_members <- function(course_id, date = NULL, input_dir = INPUT_DIR) {
  te <- add_course_id(dbh_courses("teacher_ed", date = date, input_dir = input_dir))$course_id
  list(all = rep(TRUE, length(course_id)),
       teacher_ed = offering_key(course_id) %in% offering_key(te))
}

# The plans of each frame: plan_table() of its offerings, so a frame's plan
# has the text, years and source of its own earliest offering.
frame_plans <- function(offerings, frames) {
  lapply(frames, function(m) plan_table(offerings[m, ]))
}

# The pages the plans of any frame are cut from (their source offering's page).
plan_source_pages <- function(plans, page_map) {
  src <- unique(unlist(lapply(plans, `[[`, "source_course_id")))
  sort(unique(page_map$page_id[match(src, page_map$course_id)]), method = "radix")
}

# Sections per plan: the sections of the page of the offering whose text the
# plan keeps (source_course_id; partition_sections()), so a plan's sections
# and its course_plan come from the same page. Institution by institution,
# each in the order of its plans.
frame_sections <- function(plans, source_sections, page_map, institutions) {
  src <- plans[c("plan_content_id", "institution", "course_code", "source_course_id")]
  src$page_id <- page_map$page_id[match(src$source_course_id, page_map$course_id)]
  sections <- dplyr::inner_join(src, source_sections, by = "page_id", relationship = "many-to-many") |>
    dplyr::select(plan_content_id, institution, course_code, source_course_id, section, text)
  # combined as {targets} combines branches (vctrs), so the file keeps its bytes
  do.call(vctrs::vec_rbind, unname(split(sections, factor(sections$institution, institutions))))
}

# course_offerings_full of each frame: its offerings, without a plan where it
# is a page shell. A page with no section to read and under SHELL_MAX_NCHAR
# characters of plan text (without the title and facts box, `plan_nchar`) is
# a page shell, not a plan (no text about any of the seven sections,
# course_plan_spec.md): the facts box only (uia), a credit-overlap box
# (nord), "Emnebeskrivelse" (uib), a pointer to the English page (hvl), every
# section "Se fagplanen." (oslomet 2018 practicum). Longer pages without
# sections stay: oslomet practicum courses keep the programme's practicum
# plan. The offerings keep their text but get no plan.
SHELL_MAX_NCHAR <- 1500

.shell_plans <- function(plans, sections) {
  key <- c("plan_content_id", "institution", "course_code")
  plans |>
    dplyr::anti_join(sections, by = key) |>
    dplyr::filter(plan_nchar < SHELL_MAX_NCHAR) |>
    dplyr::select(dplyr::all_of(key))
}

frame_offerings <- function(offerings, frames, plans, sections) {
  stats::setNames(lapply(names(frames), function(f) {
    shell <- do.call(paste, .shell_plans(plans[[f]], sections[[f]]))
    x <- offerings[frames[[f]], ]
    x$plan_content_id <- dplyr::if_else(paste(x$plan_content_id, x$institution, x$course_code) %in% shell,
                                        NA_character_, x$plan_content_id)
    x
  }), names(frames))
}

# Published slim offerings: DBH metadata and the plan id, no url or text.
slim_offerings <- function(offerings) {
  dplyr::select(offerings, -url, -extracted_text, -course_plan, -course_plan_normalized,
                -dplyr::any_of("plan_nchar"))
}

# course_plans of a frame: its plans without the page shells.
combine_plans <- function(plans, sections) {
  plans |>
    dplyr::anti_join(.shell_plans(plans, sections),
                     by = c("plan_content_id", "institution", "course_code")) |>
    dplyr::select(-plan_nchar) |>
    dplyr::arrange(plan_content_id, institution, course_code)
}

# fetch_coverage() of each frame's DBH offerings (`frame`: the frame).
frame_coverage <- function(date, ledger, offerings) {
  dplyr::bind_rows(lapply(names(offerings), function(f) tibble::tibble(
    frame = f, fetch_coverage(dbh_courses(f, date = date), ledger, offerings[[f]]))))
}

# pipeline_metrics() of each frame, by frame.
frame_metrics <- function(offerings, sections) {
  stats::setNames(lapply(names(offerings), function(f) pipeline_metrics(offerings[[f]], sections[[f]])),
                  names(offerings))
}

# Write `x` and return the path, for format = "file" targets.
write_rds_file <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, ...)
  path
}

# Changes beyond tolerance against the metrics snapshot of each frame
# (`snapshot`: the files of METRICS_SNAPSHOT, `metrics`: frame_metrics()).
# The warning makes them show in tar_make(); the test in
# test-pipeline-metrics.R fails on them too.
metrics_vs_snapshot <- function(snapshot, metrics) {
  changes <- dplyr::bind_rows(lapply(snapshot, function(s) {
    f <- names(METRICS_SNAPSHOT)[match(basename(s), basename(METRICS_SNAPSHOT))]
    tibble::tibble(frame = f, compare_metrics(utils::read.csv(s), metrics[[f]]))
  }))
  if (nrow(changes) > 0) {
    warning(nrow(changes), " metric(s) differ from ", paste(unique(snapshot), collapse = ", "),
            " beyond tolerance: see tar_read(metrics_check)", call. = FALSE)
  }
  changes
}

# Personal data found in the shareable files (data/processed/). The warning
# makes it show in tar_make(); test-anonymize.R fails on it too.
check_personal_data <- function(files) {
  found <- personal_data_in(files)
  if (nrow(found) > 0) {
    warning("personal data in ", paste(unique(found$file), collapse = ", "),
            ": see tar_read(privacy_check)", call. = FALSE)
  }
  found
}

# Run a script for the files it writes. `inputs` is not used: naming the input
# targets in the call is what makes {targets} rerun the script when they change.
run_r_script <- function(script, outputs, inputs = NULL, chdir = FALSE) {
  source(script, local = new.env(), chdir = chdir)
  outputs
}

render_quarto <- function(qmd, output, inputs = NULL) {
  if (system2("quarto", c("render", qmd)) != 0) stop("quarto render failed: ", qmd)
  output
}
