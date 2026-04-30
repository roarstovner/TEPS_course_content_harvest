# helpers.R — Data loading and utilities for Course Browser

status_labels <- c(

  "0" = "Unknown",
  "1" = "Active",

  "2" = "New",
  "3" = "Discontinued",
  "4" = "Disc. w/ exam"
)

# Display names for institutions (title-cased short names)
institution_labels <- c(
  hiof     = "HiOF",
  hivolda  = "Hivolda",
  hvl      = "HVL",
  inn      = "INN",
  mf       = "MF",
  nih      = "NIH",
  nla      = "NLA",
  nmbu     = "NMBU",
  nord     = "Nord",
  ntnu     = "NTNU",
  oslomet  = "OsloMet",
  samas    = "Samas",
  steiner  = "Steiner",
  uia      = "UiA",
  uib      = "UiB",
  uio      = "UiO",
  uis      = "UiS",
  uit      = "UiT",
  usn      = "USN"
)

#' Load course offerings data
#' @param data_dir Path to data/ directory
#' @return tibble of course offerings (no heavy pipeline columns)
load_courses <- function(data_dir = "../../data") {
  path <- file.path(data_dir, "course_offerings.RDS")
  if (!file.exists(path)) stop("Data file not found: ", path)
  readRDS(path)
}

#' Load raw HTML for a single course on demand
#' @param course_id The course_id to look up
#' @param inst The institution_short value
#' @param cache A reactiveValues object used for caching (keys: inst, data)
#' @param data_dir Path to data/ directory
#' @return Character string of raw HTML, or NULL if not found
load_course_html <- function(course_id, inst, cache, data_dir = "../../data") {
  # Load institution file if not already cached

  if (is.null(cache$inst) || cache$inst != inst) {
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

#' Render an HTML diff between two text strings
#'
#' Splits text into lines/sentences, runs diffobj::diffChr, returns
#' self-contained HTML string suitable for embedding in a Shiny UI.
#'
#' @param text_a Character(1), the "old" version
#' @param text_b Character(1), the "new" version
#' @param banner_a Label for old version
#' @param banner_b Label for new version
#' @param mode "sidebyside" or "unified"
#' @return HTML string, or NULL if inputs are invalid
render_diff_html <- function(text_a, text_b, banner_a = "A", banner_b = "B",
                             mode = "sidebyside") {
  if (is.na(text_a) || is.na(text_b)) return(NULL)

  # Split into lines at sentence boundaries for readable diffs
  split_to_lines <- function(txt) {
    txt <- trimws(txt)
    # Split on double+ whitespace or period-space boundaries
    lines <- unlist(strsplit(txt, "(?<=\\.)\\s+|\\n+", perl = TRUE))
    lines <- trimws(lines)
    lines[nzchar(lines)]
  }

  lines_a <- split_to_lines(text_a)
  lines_b <- split_to_lines(text_b)

  diff_obj <- diffobj::diffChr(
    lines_a, lines_b,
    format = "html",
    mode = mode,
    tar.banner = banner_a,
    cur.banner = banner_b,
    pager = "off",
    style = list(html.output = "diff.w.style"),
    context = 3L,
    word.diff = TRUE
  )

  # diffobj returns a Diff object; as.character gives the HTML
  paste(as.character(diff_obj), collapse = "\n")
}
