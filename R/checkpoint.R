# R/checkpoint.R
# Checkpoints resume an interrupted harvest (README "About Checkpointing").
# Each is a folder of append-only Parquet parts, part-000001.parquet,
# part-000002.parquet, ...: data/raw/checkpoint/{inst}/{year}/html/ holds the
# pages of a year's run, by URL; {inst}/urls/, {inst}/discovery/ and
# {inst}/pdf/ are institution-wide, kept across the years of a harvest
# (harvest_all() removes them when it has written every year). A part is
# written as part-NNNNNN.parquet.tmp and then renamed, so a crash leaves at
# most a .tmp, which the next read deletes. The RDS checkpoints of the
# harvests until 2026-10-28 (data/raw/checkpoint/{type}_{inst}.RDS) are not
# read.

#' The folder of a checkpoint
#'
#' @param institution The institution
#' @param type "html" (by year), or the institution-wide "urls", "discovery",
#'   "pdf"
#' @param year The year of an html checkpoint: the year of the offerings
#'   (several years: "all")
checkpoint_dir <- function(institution, type = "html", year = NULL) {
  if (type != "html") return(file.path(RAW_DIR, "checkpoint", institution, type))
  year <- unique(year)
  file.path(RAW_DIR, "checkpoint", institution, if (length(year) == 1) year else "all", "html")
}

checkpoint_parts <- function(dir) Sys.glob(file.path(dir, "part-*.parquet"))

#' Read a checkpoint
#'
#' @param dir Checkpoint folders (checkpoint_dir()), read in this order, each
#'   part by part. A part left as .tmp (a crash while it was written) is
#'   deleted.
#' @param key,values Only the rows whose column `key` is in `values`; NULL
#'   for all
#' @return A tibble (`html_error` as text, `pdf` a list of raw vectors), or
#'   NULL when no row is left
read_checkpoint <- function(dir, key = NULL, values = NULL) {
  unlink(Sys.glob(file.path(dir, "part-*.parquet.tmp")))
  rows <- lapply(unlist(lapply(dir, checkpoint_parts)), function(f) {
    if (!is.null(key) && !any(arrow::read_parquet(f, col_select = dplyr::all_of(key))[[key]] %in% values)) {
      return(NULL)
    }
    x <- arrow::read_parquet(f)
    if (!is.null(key)) x <- x[x[[key]] %in% values, ]
    if ("pdf" %in% names(x)) x$pdf <- lapply(x$pdf, function(p) p)
    x
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out)) out else NULL
}

#' Append rows to a checkpoint
#'
#' As a new part, numbered 1 + the highest part in `dir`, with the columns of
#' the raw store (`html_error` as text, `pdf` as binary: .parquet_columns()).
#'
#' @param rows The rows; none writes nothing
#' @param dir The checkpoint folder (checkpoint_dir())
#' @return The part written, invisibly
checkpoint_append <- function(rows, dir) {
  if (!NROW(rows)) return(invisible(NULL))
  n <- max(0L, as.integer(sub("^part-(\\d+)\\.parquet$", "\\1", basename(checkpoint_parts(dir)))))
  f <- file.path(dir, sprintf("part-%06d.parquet", n + 1L))
  .write_plain_parquet(.parquet_columns(rows), paste0(f, ".tmp"))
  if (!file.rename(paste0(f, ".tmp"), f)) stop("could not write ", f)
  invisible(f)
}

#' Remove checkpoints of an institution
#'
#' @param institution The institution
#' @param parts Folders in its checkpoint folder: years (the year's html
#'   checkpoint) or "urls", "discovery", "pdf"; NULL for all of them. The RDS
#'   checkpoints until 2026-10-28 stay.
remove_checkpoints <- function(institution, parts = NULL) {
  dirs <- file.path(RAW_DIR, "checkpoint", institution)
  if (!is.null(parts)) dirs <- file.path(dirs, parts)
  dirs <- dirs[dir.exists(dirs)]
  if (length(dirs)) message("Checkpoints removed: ", paste(dirs, collapse = ", "))
  unlink(dirs, recursive = TRUE)
  # and the institution's folder, when nothing is left in it
  inst <- file.path(RAW_DIR, "checkpoint", institution)
  if (!length(list.files(inst, recursive = TRUE, all.files = TRUE))) unlink(inst, recursive = TRUE)
  invisible(dirs)
}

#' Fetch HTML with checkpoint support
#'
#' Each URL is fetched once and its page joined to every offering with that
#' URL (an academic-year page serves an autumn and a spring offering; NTNU's
#' 255,055 offerings in DBH's snapshot of 2026-10-16 have 137,474 URLs). A
#' URL whose page is in the checkpoint, or in the html checkpoint of another
#' year of the institution (harvest_all() keeps the year before's until this
#' year's run is written), is not fetched again.
#'
#' @param courses A tibble with url and institution columns
#' @param checkpoint_dir The year's html checkpoint (checkpoint_dir()): the
#'   pages fetched are appended to it every `save_every` pages
#' @param .progress Show progress bar
#' @return courses with html, html_error (text), html_success, and the
#'   request's http_status and requested_at (fetch_html_cols()); NA for an
#'   offering without a URL. A site that refuses the harvest (HTTP 429, or
#'   403 to FORBIDDEN_RUN requests in a row and to a page known to be public;
#'   refusal_watch()) stops it, with the pages fetched before in the
#'   checkpoint. A batch is written without its trailing 403s (they wait for
#'   the next batch), so the refusal's 403s never reach the checkpoint and
#'   the next harvest asks for those pages again. A URL robots.txt disallows
#'   is not requested (robots_allows()); a host whose robots.txt has not been
#'   read stops the harvest before its first request.
fetch_html_with_checkpoint <- function(courses, checkpoint_dir, .progress = TRUE, config = NULL,
                                       save_every = 50) {
  if (!all(c("url", "institution") %in% names(courses))) stop("courses must contain: url, institution")
  urls <- dplyr::distinct(tibble::tibble(url = as.character(courses$url), institution = courses$institution),
                          url, .keep_all = TRUE)
  urls <- urls[!is.na(urls$url) & nzchar(urls$url), ]
  # the other years' checkpoints first: the year's own pages win
  dirs <- union(setdiff(Sys.glob(file.path(dirname(dirname(checkpoint_dir)), "*", "html")), checkpoint_dir),
                checkpoint_dir)
  have <- read_checkpoint(dirs, "url", urls$url)
  to_fetch <- urls[!urls$url %in% have$url, ]
  n <- nrow(to_fetch)
  message(nrow(urls), " URLs: ", nrow(urls) - n, " in the checkpoint, ", n, " to fetch")

  fetched <- vector("list", n)
  done <- saved <- 0L   # pages fetched; of these in the checkpoint
  # append the pages fetched since the last save, without the trailing 403s
  # unless `all` (they may be a refusal's: they wait for the next save)
  save <- function(all = FALSE) {
    if (done <= saved) return()
    new <- dplyr::bind_rows(fetched[(saved + 1L):done])
    keep <- if (all) nrow(new) else max(0L, which(!new$http_status %in% 403L))
    checkpoint_append(new[seq_len(keep), ], checkpoint_dir)
    saved <<- saved + keep
  }
  # an error, an interrupt or a refusal (refusal_watch() stops the loop):
  # the pages fetched so far
  on.exit(save())

  if (.progress && n) pb <- progress::progress_bar$new(
    format = "  Fetching [:bar] :current/:total (:percent) ETA: :eta",
    total = n, clear = FALSE
  )
  refused <- refusal_watch(config)
  for (i in seq_len(n)) {
    url <- to_fetch$url[i]
    # robots.txt: a host not read stops here; a URL it disallows is not
    # requested (site_request()), so nothing to wait for
    if (robots_allows(url)) polite_pause(config$request_delay)
    cols <- fetch_html_cols(urls = url, institution = to_fetch$institution[i], config = config,
                            .progress = FALSE)
    refused(cols$http_status, url)
    cols$html_error <- fetch_error_text(cols$html_error)
    fetched[[i]] <- dplyr::bind_cols(tibble::tibble(url = url), cols)
    done <- i
    # a part per save_every pages: a crash loses fewer pages than that
    if (i - saved >= save_every || i == n) save(all = i == n)
    if (.progress) pb$tick()
  }

  pages <- dplyr::bind_rows(c(list(
    tibble::tibble(url = character(), html = character(), html_error = character(), html_success = logical(),
                   http_status = integer(), requested_at = .POSIXct(numeric())),
    have), fetched))
  pages <- pages[!duplicated(pages$url, fromLast = TRUE), ]
  dplyr::bind_cols(courses, pages[match(as.character(courses$url), pages$url),
                                  c("html", "html_error", "html_success", REQUEST_COLUMNS)])
}
