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

fetch_html <- function(url, max_tries = 4L, dryrun = FALSE) {
  if (dryrun) return(list(ok = TRUE, status = NA_integer_, content = raw(0)))
  
  # Ensure UiO print view (server-rendered)
  u <- url
  if (grepl("^https?://www\\.uio\\.no/", u, ignore.case = TRUE) &&
      !grepl("[?&]vrtx=print($|&)", u, ignore.case = TRUE)) {
    u <- paste0(u, if (grepl("\\?", u)) "&" else "?", "vrtx=print")
  }
  
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
  # Build the common args once (each is a separate object, not a list of lists)
  common_args <- list(
    httr::user_agent(ua),
    httr::add_headers(
      "Accept"                    = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language"           = "nb-NO,nb;q=0.9,no;q=0.8,en;q=0.7",
      "Cache-Control"             = "no-cache",
      "Pragma"                    = "no-cache",
      "Upgrade-Insecure-Requests" = "1",
      "Referer"                   = sub("(^https?://[^/]+).*$", "\\1/", u)
    ),
    httr::timeout(30)
  )
  
  # helper to attempt a specific HTTP version, using do.call to spread args
  do_try <- function(http_ver) {
    args <- c(
      list("GET", u),
      common_args,
      list(
        httr::config(http_version = http_ver),
        terminate_on = c(200, 404, 410, 501),
        pause_base = 1, pause_cap = 5, times = max_tries
      )
    )
    resp <- do.call(httr::RETRY, args)
    status  <- httr::status_code(resp)
    content <- try(httr::content(resp, as = "raw"), silent = TRUE)
    if (inherits(content, "try-error") || is.null(content)) content <- raw(0)
    list(status = status, content = content)
  }
  
  # Try HTTP/2 first; if 501, fall back to HTTP/1.1
  r2 <- do_try(2)
  if (r2$status == 501) {
    r1 <- do_try(1.1)
    status  <- r1$status
    content <- r1$content
  } else {
    status  <- r2$status
    content <- r2$content
  }
  
  looks_like_json <- function(raw, nbytes = 512) {
    if (!length(raw)) return(FALSE)
    s <- rawToChar(raw[seq_len(min(nbytes, length(raw)))])
    Encoding(s) <- "UTF-8"
    s <- gsub("^\ufeff", "", s)
    s <- trimws(s)
    startsWith(s, "{") || startsWith(s, "[")
  }
  
  ok <- status >= 200 && status < 300 && length(content) > 0 && !looks_like_json(content)
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


# ============================
# Text normalization helper
# ============================
normalize_text <- function(x) {
  if (!is.character(x)) return(x)
  # normalize line endings
  x <- gsub("\r\n?", "\n", x)
  # collapse runs of 2+ newlines into exactly 2
  x <- gsub("\n{2,}", "\n\n", x)
  # trim leading/trailing whitespace
  x <- trimws(x)
  x
}
