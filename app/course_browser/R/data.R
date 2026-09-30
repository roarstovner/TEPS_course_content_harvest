# data.R — Data loading and labels for the course browser
#
# The app loads one prebuilt payload (data/browser_data.RDS) rather than the
# raw pipeline outputs, so startup is fast and the payload stays small enough
# to ship to a browser later. Rebuild it with: Rscript R/build_browser_data.R

institution_labels <- c(
  hiof     = "HiØ",
  hivolda  = "HVO",
  hvl      = "HVL",
  inn      = "INN",
  mf       = "MF",
  nih      = "NIH",
  nla      = "NLA",
  nmbu     = "NMBU",
  nord     = "Nord",
  ntnu     = "NTNU",
  oslomet  = "OsloMet",
  samas    = "SÁMAS",
  steiner  = "Steiner",
  uia      = "UiA",
  uib      = "UiB",
  uio      = "UiO",
  uis      = "UiS",
  uit      = "UiT",
  usn      = "USN"
)

#' Load the prebuilt browser payload
#' @param data_dir Path to the data/ directory
#' @return List with `plans`, `sections`, `coverage`, `built_at`
load_browser_data <- function(data_dir = "../../data") {
  path <- file.path(data_dir, "browser_data.RDS")
  if (!file.exists(path)) {
    stop("Missing ", path, "\nBuild it first:  Rscript R/build_browser_data.R",
         call. = FALSE)
  }
  readRDS(path)
}

#' Load raw HTML for one offering on demand
#'
#' The html_*.RDS files are far too large to hold in the payload, so they are
#' read per institution and cached for as long as the user stays on it.
#'
#' @param course_id Offering to look up
#' @param inst institution value
#' @param cache reactiveValues with `inst` and `data` slots
#' @param data_dir Path to the data/ directory
#' @return Character(1) of raw HTML, or NULL
load_course_html <- function(course_id, inst, cache, data_dir = "../../data") {
  if (is.null(cache$inst) || !identical(cache$inst, inst)) {
    path <- file.path(data_dir, paste0("html_", inst, ".RDS"))
    if (!file.exists(path)) return(NULL)
    raw <- readRDS(path)
    cache$data <- raw[, c("course_id", "html"), drop = FALSE]
    cache$inst <- inst
  }
  row <- cache$data[cache$data$course_id == course_id, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row$html[[1]]
}

#' Load an optional term instrument
#'
#' A terms YAML (same shape as emneplan_LK20/LK20_terms.yaml: a `terms:` list of
#' `id`/`label`/`regex`, optionally `category` and `strength`) turns the search
#' box into an instrument inspector. Entirely optional — set the
#' TEPS_BROWSER_TERMS environment variable, or drop a browser_terms.yaml in the
#' repo root. Absent, the feature hides itself and ad-hoc search is unaffected.
#'
#' @param data_dir Path to the data/ directory, used to locate the repo root
#' @return Named character vector of regexes (names are labels), or NULL
load_term_set <- function(data_dir = "../../data") {
  path <- Sys.getenv("TEPS_BROWSER_TERMS", "")
  if (!nzchar(path)) path <- file.path(dirname(data_dir), "browser_terms.yaml")
  if (!file.exists(path) || !requireNamespace("yaml", quietly = TRUE)) return(NULL)

  parsed <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  terms <- parsed$terms
  if (is.null(terms) || !length(terms)) return(NULL)

  regexes <- vapply(terms, function(t) t$regex %||% NA_character_, character(1))
  labels <- vapply(terms, function(t) {
    t$label_short %||% t$label %||% t$id %||% NA_character_
  }, character(1))
  keep <- !is.na(regexes) & !is.na(labels)
  if (!any(keep)) return(NULL)
  setNames(regexes[keep], labels[keep])
}

#' Render an HTML diff between two plan texts
#'
#' @param text_a Character(1), the older version
#' @param text_b Character(1), the newer version
#' @param banner_a Label for the older version
#' @param banner_b Label for the newer version
#' @param mode "sidebyside" or "unified"
#' @return HTML string, or NULL when either text is missing
render_diff_html <- function(text_a, text_b, banner_a = "A", banner_b = "B",
                             mode = "sidebyside") {
  if (is.na(text_a) || is.na(text_b)) return(NULL)

  split_to_lines <- function(txt) {
    lines <- unlist(strsplit(trimws(txt), "(?<=\\.)\\s+|\\n+", perl = TRUE))
    lines <- trimws(lines)
    lines[nzchar(lines)]
  }

  diff_obj <- diffobj::diffChr(
    split_to_lines(text_a), split_to_lines(text_b),
    format = "html", mode = mode,
    tar.banner = banner_a, cur.banner = banner_b,
    pager = "off",
    style = list(html.output = "diff.w.style"),
    context = 3L, word.diff = TRUE
  )
  paste(as.character(diff_obj), collapse = "\n")
}
