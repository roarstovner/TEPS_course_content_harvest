# ============================================================
# R/parse_html_generic.R
# Generic HTML → clean text (Markdown-ish) parser.
# Used by 04_parse_html.R with YAML configs.
# ============================================================

safelength <- function(x) if (is.null(x)) 0L else length(x)
`%||%` <- function(a, b) if (is.null(a)) b else a

# choose largest candidate by visible text length
.pick_main <- function(doc, css) {
  nodes <- rvest::html_elements(doc, css)
  if (!length(nodes)) return(NA)
  txtlen <- vapply(nodes, function(nd) nchar(rvest::html_text2(nd)), integer(1))
  nodes[[which.max(txtlen)]]
}

# clip between keep_after / stop_before regexes
.clip_window <- function(txt, keep_after, stop_before) {
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  
  idx_start <- 1L
  if (safelength(keep_after)) {
    pat <- paste(keep_after, collapse = "|")
    hits <- grep(pat, lines, perl = TRUE)
    if (length(hits)) idx_start <- min(hits)
  }
  
  idx_end <- length(lines)
  if (safelength(stop_before)) {
    pat2 <- paste(stop_before, collapse = "|")
    hits2 <- grep(pat2, lines, perl = TRUE)
    if (length(hits2)) idx_end <- min(hits2) - 1L
  }
  
  if (idx_end < idx_start) return(character(0))
  lines[idx_start:idx_end]
}

# promote section titles → markdown headers
.promote_headers <- function(lines, sections) {
  if (!length(lines) || !safelength(sections)) return(lines)
  for (sec_name in names(sections)) {
    pat <- paste(sections[[sec_name]], collapse = "|")
    hits <- grepl(pat, lines, perl = TRUE)
    if (any(hits)) lines[hits] <- paste0("## ", sec_name)
  }
  lines
}

# flatten block nodes to simple lines
.flatten_blocks <- function(main) {
  nodes <- rvest::html_elements(
    main,
    xpath = ".//h1|.//h2|.//h3|.//h4|.//h5|.//h6|.//p|.//li"
  )
  if (!length(nodes)) return(character(0))
  
  out <- character(0)
  for (nd in nodes) {
    tag <- xml2::xml_name(nd)
    tx  <- rvest::html_text2(nd)
    if (!nzchar(tx)) next
    if (tag %in% c("h1","h2","h3","h4","h5","h6")) {
      out <- c(out, paste0("## ", tx))
    } else if (tag == "li") {
      out <- c(out, paste0("- ", tx))
    } else {
      out <- c(out, tx)
    }
  }
  
  keep <- c(TRUE, out[-1] != out[-length(out)])
  out[keep]
}

# ---- HELPER: normalize + de-crumb text ----------------------
.normalize_text <- function(txt) {
  lines <- unlist(strsplit(txt, "\n"))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  # squash consecutive duplicates or blanks
  lines <- lines[c(TRUE, lines[-1] != lines[-length(lines)])]
  # remove breadcrumb/menu lines
  crumb_pat <- "(?i)\\b(hjem|home|studier|menu|breadcrumb|personvern|cookies|logg inn)\\b"
  lines <- lines[!grepl(crumb_pat, lines) | nchar(lines) > 60]
  txt <- paste(lines, collapse = "\n")
  txt <- gsub("\n{3,}", "\n\n", txt)
  trimws(txt)
}

# ---- MAIN FUNCTION -----------------------------------------
parse_html_generic <- function(file_path, cfg) {
  min_chars <- cfg$min_chars %||% 400
  
  # --- Encoding fallback ---
  html <- try(read_html(file_path, encoding = "UTF-8"), silent = TRUE)
  if (inherits(html, "try-error")) {
    html <- try(read_html(file_path, encoding = "ISO-8859-1"), silent = TRUE)
  }
  if (inherits(html, "try-error")) return("")
  
  # --- Pick largest main container ---
  main <- .pick_main(html, cfg$selector_main %||% "main, article, #content")
  if (is.na(main) || !length(main)) return("")
  
  # --- Remove excluded sections ---
  ex <- cfg$selector_exclude %||% c("header","footer","nav","aside")
  for (sel in ex) {
    suppressWarnings({
      junk <- rvest::html_elements(main, sel)
      if (length(junk)) xml2::xml_remove(junk)
    })
  }
  
  # --- Flatten to lines ---
  lines <- .flatten_blocks(main)
  if (!length(lines)) {
    txt <- rvest::html_text(main, trim = TRUE)
    lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
  }
  
  # --- Clip + promote headers ---
  lines <- .clip_window(paste(lines, collapse = "\n"), cfg$keep_after, cfg$stop_before)
  lines <- .promote_headers(lines, cfg$sections)
  
  # --- Normalize + clean ---
  out <- paste(lines, collapse = "\n\n")
  out <- .normalize_text(out)
  
  # --- Encoding normalize ---
  out <- iconv(out, from = "", to = "UTF-8", sub = "byte")
  
  # --- Return ---
  if (nchar(out) < min_chars) return("")
  out
}
