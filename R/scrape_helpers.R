# R/scrape_helpers.R

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x

ua_string <- function() {
  paste0("TEPS-scraper/1.0 ",
         "(R/", getRversion(), "; httr/", as.character(utils::packageVersion("httr")), ")")
}

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

ts_now <- function() format(Sys.time(), "%Y%m%d-%H%M")

safe_stub <- function(inst, cod, yr, hv) {
  cod <- gsub("[^A-Za-z0-9]", "", cod)     # rydd kurskode
  yr  <- ifelse(is.na(yr), "", yr)
  hv  <- ifelse(is.na(hv), "", hv)
  paste(c(inst, cod, yr, hv), collapse = "_")
}

# ---- SELECTORS ----
load_selectors <- function(inst_short, cfg_path = "config/selectors.yaml") {
  stopifnot(file.exists(cfg_path))
  y <- yaml::read_yaml(cfg_path)
  sel <- y$selectors[[inst_short]]
  if (is.null(sel)) stop("Fant ingen selectors for inst='", inst_short, "' i ", cfg_path)
  list(
    fulltext       = sel$fulltext %||% NA_character_,
    course_name_no = sel$course_name_no %||% NA_character_
  )
}

# ---- FETCH ----
fetch_html <- function(url, max_tries = 4L, dryrun = FALSE) {
  if (dryrun) return(list(ok = TRUE, status = NA_integer_, content = charToRaw("<html></html>")))
  resp <- try(httr::RETRY(
    "GET", url,
    httr::add_headers(`User-Agent` = ua_string()),
    times = max_tries, pause_min = 1, pause_cap = 6,
    terminate_on = c(200L, 404L, 410L),
    quiet = TRUE
  ), silent = TRUE)
  if (inherits(resp, "try-error") || is.null(resp)) {
    return(list(ok = FALSE, status = NA_integer_, content = raw()))
  }
  status <- httr::status_code(resp)
  ok <- status >= 200L && status < 400L
  content <- if (ok) httr::content(resp, as = "raw") else raw()
  list(ok = ok, status = status, content = content)
}

# ---- PARSE ----
parse_fields <- function(raw_html, selectors) {
  out <- list(fulltext = NA_character_, course_name_no = NA_character_)
  if (!length(raw_html)) return(out)
  doc <- try(xml2::read_html(raw_html, encoding = "UTF-8"), silent = TRUE)
  if (inherits(doc, "try-error")) return(out)
  
  if (is.character(selectors$course_name_no) && nzchar(selectors$course_name_no)) {
    nodes <- rvest::html_elements(doc, css = selectors$course_name_no)
    val <- rvest::html_text2(nodes)
    out$course_name_no <- if (length(val)) paste(val, collapse = " ") else NA_character_
  }
  if (is.character(selectors$fulltext) && nzchar(selectors$fulltext)) {
    nodes <- rvest::html_elements(doc, css = selectors$fulltext)
    val <- rvest::html_text2(nodes)
    out$fulltext <- if (length(val)) paste(val, collapse = "\n\n") else NA_character_
  }
  out
}

# ---- RE-PARSE LOKAL HTML (troubleshoot) ----
reparse_html_file <- function(path_html, selectors) {
  raw <- readBin(path_html, what = "raw", n = file.info(path_html)$size)
  parse_fields(raw, selectors)
}

# ---- PLASS-SPARING: GZIP HTML-HISTORIKK ----
gzip_html_dir <- function(dir_html, keep_last = 1L) {
  files <- list.files(dir_html, pattern = "\\.html$", full.names = TRUE)
  if (!length(files)) return(invisible(0L))
  key <- sub("_[0-9]{8}-[0-9]{4}\\.html$", "", basename(files))
  ord <- order(key, file.info(files)$mtime, decreasing = TRUE)
  files <- files[ord]; key <- key[ord]
  keep_idx <- ave(seq_along(files), key, FUN = function(ix) ix[seq_len(min(keep_last, length(ix)))])
  to_zip <- files[!(seq_along(files) %in% keep_idx)]
  n <- 0L
  for (f in to_zip) {
    raw <- readBin(f, "raw", n = file.info(f)$size)
    gz  <- memCompress(raw, "gzip")
    fgz <- paste0(f, ".gz")
    writeBin(gz, fgz); unlink(f); n <- n + 1L
  }
  invisible(n)
}

# safe_writeLines: writes UTF-8 text to file, auto-closes connection
safe_writeLines <- function(txt, path, append = FALSE) {
  ensure_dir(dirname(path))
  mode <- if (append) "a" else "w"
  con <- file(path, open = mode, encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeLines(txt, con, useBytes = TRUE)
  invisible(path)
}
