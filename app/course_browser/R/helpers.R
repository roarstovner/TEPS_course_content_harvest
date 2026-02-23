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

#' Load courses data, dropping heavy columns
#' @param data_dir Path to data/ directory
#' @return tibble with html and html_error columns removed
load_courses <- function(data_dir = "../../data") {
  path <- file.path(data_dir, "courses_with_plan_id.RDS")
  if (!file.exists(path)) stop("Data file not found: ", path)
  df <- readRDS(path)
  df$html <- NULL
  df$html_error <- NULL
  df
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
