# search.R — Concordance search over course plans and sections
#
# Search is live, not precomputed, so any ad-hoc query works. It stays instant
# by keeping a pre-lowercased copy of every searchable text: a literal
# case-insensitive search then reduces to stri_detect_fixed on already-folded
# text, which is ~40x faster than asking the regex engine to fold case.
#
# Regex queries cannot use that path — lowercasing a pattern would corrupt it
# (`\S` becomes `\s`) — so they run case-insensitively against the original
# text. That is the slower power-user path, and it is still well under a second.

library(stringi)

PLAN_KEYS <- c("plan_content_id", "institution", "Emnekode")

SECTION_LABELS <- c(
  learning_outcomes       = "Læringsutbytte",
  course_content          = "Innhold",
  teaching_methods        = "Undervisningsformer",
  assessment              = "Vurdering",
  coursework_requirements = "Arbeidskrav",
  prerequisites           = "Forkunnskaper",
  reading_list            = "Pensum"
)

#' Pre-lowercase every searchable text once
#'
#' Costs one pass at startup and roughly as much memory again as the texts
#' themselves, and buys a ~40x speedup on the common literal search.
#'
#' @param plans Plan table with a `course_plan` column
#' @param sections Plan-level section table with `text`, or NULL
#' @return List with `plan_lc` and `section_lc` character vectors
build_search_index <- function(plans, sections = NULL) {
  list(
    plan_lc = stri_trans_tolower(plans$course_plan),
    section_lc = if (is.null(sections)) NULL else stri_trans_tolower(sections$text)
  )
}

#' Resolve a search scope to the texts it covers
#'
#' @param scope "plan" for whole plans, otherwise a section name
#' @param plans Plan table
#' @param sections Plan-level section table, or NULL
#' @param index Output of build_search_index()
#' @return List with `keys` (data frame of plan keys), `text`, `text_lc`
resolve_scope <- function(scope, plans, sections, index) {
  if (identical(scope, "plan") || is.null(sections)) {
    return(list(
      keys = plans[, PLAN_KEYS, drop = FALSE],
      text = plans$course_plan,
      text_lc = index$plan_lc
    ))
  }
  rows <- which(sections$section == scope)
  list(
    keys = sections[rows, PLAN_KEYS, drop = FALSE],
    text = sections$text[rows],
    text_lc = index$section_lc[rows]
  )
}

#' Split a literal query into AND-ed terms
#'
#' Whitespace separates terms and every term must be present (AND); a
#' "quoted phrase" is one term, so literals containing spaces still work.
#' Regex has no AND operator, and the lookahead trick that emulates one
#' (`(?s)^(?=.*a)(?=.*b)`) matches zero-width, so it can filter but never
#' highlight — hence AND lives here rather than in the pattern.
#'
#' @param query Raw query string
#' @return Character vector of terms, in the order typed
parse_query <- function(query) {
  if (is.null(query) || length(query) != 1 || is.na(query)) return(character(0))
  query <- trimws(query)
  if (!nzchar(query)) return(character(0))

  m <- stri_match_all_regex(query, '"([^"]*)"|(\\S+)')[[1]]
  terms <- ifelse(!is.na(m[, 2]), m[, 2], m[, 3])
  terms <- trimws(terms)
  unique(terms[nzchar(terms)])
}

#' Check that a query compiles, so a half-typed regex never errors a render
#' @param query Raw query string
#' @param regex TRUE if the query is a regex
#' @return TRUE when the query is usable
query_is_valid <- function(query, regex = FALSE) {
  if (is.null(query) || length(query) != 1 || is.na(query) || !nzchar(trimws(query))) {
    return(FALSE)
  }
  if (!regex) return(length(parse_query(query)) > 0)
  tryCatch({
    stri_detect_regex("", query)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

#' Count each term separately in one text
#' @param txt Character(1)
#' @param query Raw query string
#' @param regex TRUE to treat `query` as a single regex
#' @param match_case TRUE for case-sensitive matching
#' @return Named integer vector of per-term counts
count_terms <- function(txt, query, regex = FALSE, match_case = FALSE) {
  if (length(txt) != 1 || is.na(txt) || !query_is_valid(query, regex)) {
    return(integer(0))
  }
  if (regex) {
    n <- stri_count_regex(txt, trimws(query),
                          opts_regex = stri_opts_regex(case_insensitive = !match_case))
    return(setNames(as.integer(n), trimws(query)))
  }
  terms <- parse_query(query)
  n <- vapply(terms, function(t) {
    as.integer(stri_count_fixed(
      txt, t, opts_fixed = stri_opts_fixed(case_insensitive = !match_case)))
  }, integer(1))
  setNames(n, terms)
}

#' Count matches of a query across a vector of texts
#'
#' Counts only — the result table never needs match positions, and locating
#' every match of a very common word costs far more than counting it. Positions
#' come from locate_matches() for the one plan the user selects.
#'
#' @param text Original texts
#' @param text_lc Pre-lowercased counterpart of `text`
#' @param query Raw query string
#' @param regex TRUE to treat `query` as a regex
#' @param match_case TRUE for case-sensitive matching
#' @return List with `hits` (integer indices into `text`), `n` (total match count
#'   per hit, summed over terms) and `terms` (the terms that were required)
search_text <- function(text, text_lc, query, regex = FALSE, match_case = FALSE) {
  empty <- list(hits = integer(0), n = integer(0), terms = character(0))
  if (!query_is_valid(query, regex)) return(empty)

  if (regex) {
    counts <- stri_count_regex(
      text, trimws(query),
      opts_regex = stri_opts_regex(case_insensitive = !match_case)
    )
    counts[is.na(counts)] <- 0L
    hits <- which(counts > 0)
    return(list(hits = hits, n = as.integer(counts[hits]), terms = trimws(query)))
  }

  terms <- parse_query(query)
  cnt <- matrix(0L, nrow = length(text), ncol = length(terms))
  for (j in seq_along(terms)) {
    cj <- if (match_case) {
      stri_count_fixed(text, terms[j])
    } else {
      # The fast path: both sides already folded, no case handling at match time
      stri_count_fixed(text_lc, stri_trans_tolower(terms[j]))
    }
    cj[is.na(cj)] <- 0L
    cnt[, j] <- as.integer(cj)
  }

  # AND: every term must appear at least once
  hits <- which(rowSums(cnt > 0L) == length(terms))
  list(hits = hits, n = as.integer(rowSums(cnt)[hits]), terms = terms)
}

#' Merge overlapping match ranges
#'
#' Two terms can overlap ("lærer" inside "lærerutdanning"). mark_matches() walks
#' ranges assuming they do not, so overlaps would emit nested or duplicated
#' markup. The surviving range keeps the earlier term's colour.
#'
#' @param loc Matrix with start, end, term columns
#' @return Matrix with the same columns, sorted and non-overlapping
merge_locs <- function(loc) {
  loc <- loc[order(loc[, 1], loc[, 2]), , drop = FALSE]
  out <- matrix(loc[1, ], nrow = 1)
  for (i in seq_len(nrow(loc))[-1]) {
    last <- nrow(out)
    if (loc[i, 1] <= out[last, 2]) {
      out[last, 2] <- max(out[last, 2], loc[i, 2])
    } else {
      out <- rbind(out, loc[i, ])
    }
  }
  colnames(out) <- c("start", "end", "term")
  out
}

#' Locate every match of a query within a single text
#'
#' @param txt Character(1)
#' @param query Raw query string
#' @param regex TRUE to treat `query` as a regex
#' @param match_case TRUE for case-sensitive matching
#' @return Matrix with start, end and term columns, or NULL when nothing matches
locate_matches <- function(txt, query, regex = FALSE, match_case = FALSE) {
  if (length(txt) != 1 || is.na(txt) || !nzchar(txt)) return(NULL)
  if (!query_is_valid(query, regex)) return(NULL)

  if (regex) {
    loc <- stri_locate_all_regex(
      txt, trimws(query),
      opts_regex = stri_opts_regex(case_insensitive = !match_case)
    )[[1]]
    # Zero-width regex matches and misses both come back unusable
    loc <- loc[!is.na(loc[, 1]) & loc[, 2] >= loc[, 1], , drop = FALSE]
    if (nrow(loc) == 0) return(NULL)
    return(merge_locs(cbind(loc, term = 1L)))
  }

  terms <- parse_query(query)
  pieces <- list()
  for (j in seq_along(terms)) {
    l <- stri_locate_all_fixed(
      txt, terms[j],
      opts_fixed = stri_opts_fixed(case_insensitive = !match_case)
    )[[1]]
    l <- l[!is.na(l[, 1]) & l[, 2] >= l[, 1], , drop = FALSE]
    if (nrow(l)) pieces[[length(pieces) + 1L]] <- cbind(l, term = j)
  }
  if (!length(pieces)) return(NULL)
  merge_locs(do.call(rbind, pieces))
}

#' Escape text for HTML and wrap located matches in `<mark>`
#'
#' Matches are located on the raw string before escaping, so each piece is
#' escaped separately and escaping never shifts an offset.
#'
#' @param txt Character(1)
#' @param loc Matrix from locate_matches(), or NULL for no highlighting
#' @return Character(1) of HTML
mark_matches <- function(txt, loc = NULL) {
  if (length(txt) != 1 || is.na(txt)) return("")
  if (is.null(loc) || nrow(loc) == 0) return(htmltools::htmlEscape(txt))

  pieces <- character(0)
  pos <- 1L
  for (i in seq_len(nrow(loc))) {
    st <- loc[i, 1]
    en <- loc[i, 2]
    if (st > pos) pieces <- c(pieces, htmltools::htmlEscape(substr(txt, pos, st - 1L)))
    pieces <- c(pieces, mark_open(loc, i),
                htmltools::htmlEscape(substr(txt, st, en)), "</mark>")
    pos <- en + 1L
  }
  total <- nchar(txt)
  if (pos <= total) pieces <- c(pieces, htmltools::htmlEscape(substr(txt, pos, total)))
  paste0(pieces, collapse = "")
}

#' Opening `<mark>` tag carrying the term's colour class
#' @param loc Matrix from locate_matches()
#' @param i Row to build the tag for
#' @return Character(1)
mark_open <- function(loc, i) {
  if (ncol(loc) < 3) return("<mark>")
  paste0("<mark class=\"t", ((loc[i, 3] - 1L) %% 4L) + 1L, "\">")
}

#' Keyword-in-context snippets around each located match
#'
#' @param txt Character(1) the matches were located in
#' @param loc Two-column start/end matrix
#' @param width Characters of context either side of the match
#' @param max_n Cap on snippets returned (NULL for all)
#' @return Character vector of HTML snippets
kwic_snippets <- function(txt, loc, width = 140L, max_n = NULL) {
  if (is.null(loc) || nrow(loc) == 0) return(character(0))
  if (!is.null(max_n) && nrow(loc) > max_n) loc <- loc[seq_len(max_n), , drop = FALSE]

  total <- nchar(txt)
  vapply(seq_len(nrow(loc)), function(i) {
    st <- loc[i, 1]
    en <- loc[i, 2]
    lo <- max(1L, st - width)
    hi <- min(total, en + width)
    # Keep the spaces that sit against the match; trim only the outer edges
    collapse <- function(x) gsub("[[:space:]]+", " ", x)
    pre  <- sub("^ ", "", collapse(substr(txt, lo, st - 1L)))
    mid  <- collapse(substr(txt, st, en))
    post <- sub(" $", "", collapse(substr(txt, en + 1L, hi)))
    paste0(
      if (lo > 1L) "… " else "",
      htmltools::htmlEscape(pre),
      mark_open(loc, i), htmltools::htmlEscape(mid), "</mark>",
      htmltools::htmlEscape(post),
      if (hi < total) " …" else ""
    )
  }, character(1))
}
