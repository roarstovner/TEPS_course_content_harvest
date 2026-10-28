#' Resolve course URLs that can't be determined from metadata alone
#'
#' Some institutions (like USN) require trial-and-error to discover valid URLs.
#' This function takes courses with NA urls and attempts to discover them.
#'
#' @param df A tibble with course_id and url columns
#' @param checkpoint_path The institution's URL checkpoint folder
#'   (checkpoint_dir(inst, "urls")); its sibling "discovery" holds the course
#'   codes UiT and hivolda have looked up (discover_codes()). Set to NULL to
#'   disable checkpointing (useful for testing).
#' @param .progress Show progress bar
#' @return df with url column filled in where possible
resolve_course_urls <- function(df,
                                checkpoint_path = checkpoint_dir(df$institution[1], "urls"),
                                .progress = TRUE) {
  # Guardrails
  if (!all(c("course_id", "url", "institution") %in% names(df))) {
    stop("df must contain: course_id, url, institution")
  }

  # Only process rows where url is NA
  needs_resolution <- df |> dplyr::filter(is.na(url))

  if (nrow(needs_resolution) == 0) {
    return(df)
  }

  # Load checkpoint if exists (unless checkpointing disabled)
  checkpoint <- if (!is.null(checkpoint_path)) {
    read_checkpoint(checkpoint_path, "course_id", needs_resolution$course_id)
  }
  checkpoint <- checkpoint[!duplicated(checkpoint$course_id, fromLast = TRUE), ]

  # Skip courses already resolved in checkpoint
  n_needs_resolution <- nrow(needs_resolution)
  if (!is.null(checkpoint)) {
    needs_resolution <- dplyr::anti_join(needs_resolution, checkpoint, by = "course_id")
  }

  # Return early if nothing left to resolve (all in checkpoint)
  if (nrow(needs_resolution) == 0) {
    cli::cli_alert_success("All {n_needs_resolution} courses found in checkpoint")
    # Merge checkpoint data back into df
    update_cols <- intersect(c("course_id", "url", "html"), names(checkpoint))
    if ("html" %in% names(checkpoint) && !"html" %in% names(df)) {
      df <- df |> dplyr::mutate(html = NA_character_)
    }
    return(df |>
      dplyr::rows_update(
        checkpoint |> dplyr::select(dplyr::all_of(update_cols)),
        by = "course_id",
        unmatched = "ignore"
      ))
  }

  # Dispatch to institution-specific batch resolvers (enables reuse of chromium sessions)
  n_from_checkpoint <- n_needs_resolution - nrow(needs_resolution)
  if (n_from_checkpoint > 0) {
    cli::cli_alert_info("Found {n_from_checkpoint} courses in checkpoint, resolving {nrow(needs_resolution)} remaining")
  }

  discovery <- if (!is.null(checkpoint_path)) file.path(dirname(checkpoint_path), "discovery")
  resolve_batch <- function(df, inst, checkpoint_path, .progress) {
    if (inst == "usn") {
      resolve_urls_usn_batch(df, checkpoint_path = checkpoint_path, .progress = .progress)
    } else if (inst == "hivolda") {
      resolve_urls_hivolda_batch(df, .progress = .progress, checkpoint = discovery)
    } else if (inst == "uit") {
      resolve_urls_uit_batch(df, .progress = .progress, checkpoint = discovery)
    } else if (inst == "uio") {
      resolve_urls_uio_batch(df)
    } else if (inst == "hiof") {
      resolve_urls_hiof_batch(df)
    } else {
      df |> dplyr::mutate(url = NA_character_)
    }
  }

  resolved <- needs_resolution |>
    dplyr::group_by(institution) |>
    dplyr::group_modify(~ resolve_batch(.x, .y$institution, checkpoint_path, .progress)) |>
    dplyr::ungroup()

  # Append the offerings resolved now to the checkpoint (only if checkpointing
  # enabled), but those USN's resolver appended as it went
  if (!is.null(checkpoint_path)) {
    saved <- read_checkpoint(checkpoint_path, "course_id", resolved$course_id)$course_id
    new <- resolved[!resolved$course_id %in% saved, ]
    checkpoint_append(dplyr::select(new, dplyr::any_of(c("course_id", "url", "html"))), checkpoint_path)
    cli::cli_alert_success("Checkpoint: {nrow(new)} courses added")
  }

  # Merge back into original df: the rows resolved now and the checkpoint's
  # rows of the offerings resolved before (USN includes html, others may not)
  cols <- c("course_id", "url", "html")
  merged <- dplyr::bind_rows(
    if (!is.null(checkpoint)) {
      checkpoint |>
        dplyr::filter(course_id %in% df$course_id) |>
        dplyr::select(dplyr::any_of(cols))
    },
    resolved |> dplyr::select(dplyr::any_of(cols))
  )
  merged <- merged[!duplicated(merged$course_id, fromLast = TRUE), ]

  # Ensure df has html column if the merged rows have it
  if ("html" %in% names(merged) && !"html" %in% names(df)) {
    df <- df |> dplyr::mutate(html = NA_character_)
  }

  df |>
    dplyr::rows_update(merged, by = "course_id", unmatched = "ignore")
}

#' The semester pages of course codes, each code's page read once
#'
#' UiT and hivolda list a course's semester pages on one page per course
#' code. A code looked up before is read from the discovery checkpoint `dir`
#' instead, so a harvest that takes its years one at a time (harvest_all())
#' asks for each code's page once (UiT: about 13,000 codes, 16 hours). A
#' code without pages is kept as a row without url; a code whose page fails is
#' not, and is asked for again next time.
#'
#' @param codes Course codes
#' @param discover Function of a code: its pages, a tibble of course_code,
#'   year, semester_name and url (an error for a page that fails)
#' @param dir Discovery checkpoint folder (checkpoint_dir(inst,
#'   "discovery")), or NULL for none; appended every `save_every` codes
#' @param .progress Show progress bar
#' @return Tibble of course_code, year, semester_name, url: the pages found
discover_codes <- function(codes, discover, dir = NULL, save_every = 50, .progress = TRUE) {
  codes <- unique(codes)
  have <- if (!is.null(dir)) read_checkpoint(dir, "course_code", codes)
  todo <- setdiff(codes, have$course_code)
  cli::cli_alert_info("Discovering URLs for {length(todo)} course codes ({length(codes) - length(todo)} in the checkpoint)")

  found <- pending <- list()
  save <- function() {
    if (!is.null(dir)) checkpoint_append(dplyr::bind_rows(unname(pending)), dir)
    pending <<- list()
  }
  on.exit(save())   # also after an error or an interrupt
  if (.progress && length(todo)) pb <- cli::cli_progress_bar(total = length(todo))
  for (code in todo) {
    pending[[code]] <- found[[code]] <- tryCatch({
      polite_pause()
      x <- discover(code)
      if (nrow(x)) x else tibble::tibble(course_code = code, year = NA_integer_,
                                         semester_name = NA_character_, url = NA_character_)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch listing for {code}: {e$message}")
      NULL
    })
    if (length(pending) >= save_every) save()
    if (.progress) cli::cli_progress_update(id = pb)
  }
  none <- tibble::tibble(course_code = character(), year = integer(), semester_name = character(),
                         url = character())
  out <- dplyr::bind_rows(unname(c(list(none, have), found)))
  out[!is.na(out$url), names(none)]
}

#' Resolve URLs for a batch of Hivolda courses
#'
#' For each unique course_code, fetches the base page /emne/{CODE} and parses
#' the "Semestre" section to find semester-specific version URLs.
#' Link text uses Nynorsk ("Haust") which is mapped to Bokmål ("Høst").
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param .progress Show progress bar
#' @param checkpoint Discovery checkpoint folder (discover_codes()), or NULL
#' @return df with url column filled in where possible
resolve_urls_hivolda_batch <- function(df, .progress = TRUE, checkpoint = NULL) {
  if (nrow(df) == 0) return(df |> dplyr::mutate(url = NA_character_))

  lookup <- discover_codes(df$course_code, function(code) {
    hivolda_semester_pages(site_html(paste0("https://www.hivolda.no/emne/", code)), code)
  }, checkpoint, .progress = .progress)

  if (nrow(lookup) == 0) {
    cli::cli_alert_warning("No semester URLs discovered")
    return(df |> dplyr::mutate(url = NA_character_))
  }

  lookup <- lookup |>
    dplyr::distinct(course_code, year, semester_name, .keep_all = TRUE)
  cli::cli_alert_success("Discovered {nrow(lookup)} semester URLs across {length(unique(df$course_code))} course codes")

  # Join lookup to df, replacing the NA url column
  df |>
    dplyr::select(-url) |>
    dplyr::left_join(lookup, by = c("course_code", "year", "semester_name"))
}

# The semester pages a Hivolda course page links to, /emne/{CODE}/{ID}, by
# the link text ("2024 Haust", "2023 Vår"; Nynorsk "Haust" is "Høst").
hivolda_semester_pages <- function(page, code) {
  links <- rvest::html_elements(page, "a")
  links <- links[grepl(paste0("/emne/", code, "/\\d+"), rvest::html_attr(links, "href"))]
  m <- stringr::str_match(trimws(rvest::html_text2(links)), "(\\d{4})\\s+(\\S+)")
  tibble::tibble(
    course_code = code, year = as.integer(m[, 2]),
    semester_name = dplyr::case_match(tolower(m[, 3]), "haust" ~ "Høst", "vår" ~ "Vår", .default = m[, 3]),
    url = paste0("https://www.hivolda.no", rvest::html_attr(links, "href"))
  )[!is.na(m[, 1]), ]
}

#' Resolve URLs for a batch of HiOF courses from the site's folder listings
#'
#' A HiOF page is /studier/emner/{folder}/{year}/{var|host}/{code}.html, in a
#' faculty or department folder that DBH does not give and that changed with
#' the reorganisations (lusp/slik/, iio/ing/, nta/, videre/); a course no
#' longer taught moves to /studier/emner/historiske-emner/{hv|ir|it|lu|oss}/
#' with its years (up to 2024 there), and some sit in another faculty's folder
#' (hsb1209 in ir/). hiof_course_index() lists the pages of the offerings'
#' years; hiof_match_urls() picks the offering's.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index Pages and year folders read, from hiof_course_index()
#' @return df with url filled in where possible
resolve_urls_hiof_batch <- function(df, index = hiof_course_index(unique(df$year))) {
  cli::cli_alert_info("HiOF: {nrow(index$pages)} pages in {length(index$read)} year folders")
  hiof_match_urls(df, index)
}

#' The HiOF page of each offering
#'
#' The page of the offering's code (the file name, lower case), year and
#' semester; of several, the one at the URL add_course_url_hiof() gives (the
#' teacher-education folders), then one outside historiske-emner. An offering
#' without a page whose year folder at add_course_url_hiof()'s URL was not
#' read (its listing failed) keeps that URL; else its URL is NA.
#'
#' @param df A tibble with course_code, year, semester_name
#' @param index List of `pages` (tibble of url, code, year, semester) and
#'   `read` (the year folders read in full)
#' @return df with url
hiof_match_urls <- function(df, index) {
  built <- as.character(add_course_url_hiof(df$course_code, df$year, df$semester_name))
  p <- index$pages
  p <- p[order(!p$url %in% built, grepl("/historiske-emner/", p$url)), ]
  url <- p$url[match(paste(tolower(df$course_code), df$year, semester_to_url(df$semester_name)),
                     paste(p$code, p$year, p$semester))]
  unread <- is.na(url) & !sub("[^/]+/[^/]+$", "", built) %in% index$read
  url[unread] <- built[unread]
  df$url <- url
  df
}

#' HiOF's course pages of some years, from the site's folder listings
#'
#' Walks /studier/emner/ three levels down (faculty, department, the
#' historiske-emner folders) to the year folders, and reads each year folder
#' of `years` page by page (?page=2, ...) until a page lists no new course
#' page. About 20 pages a listing page: some 1000 requests for all years. A
#' year folder whose listing fails is not in `read`.
#'
#' @param years Calendar years of the offerings
#' @param root The course folder
#' @param delay Seconds between requests (polite_pause())
#' @return List of `pages`, a tibble of url, code (lower case), year and
#'   semester ("var", "host"), and `read`, the year folders read in full
hiof_course_index <- function(years, root = "https://www.hiof.no/studier/emner/",
                              delay = get_institution_config("hiof")$request_delay) {
  links <- function(url) {
    polite_pause(delay)
    page <- tryCatch(rvest::read_html(httr2::resp_body_string(httr2::req_perform(
      site_request(url, BROWSER_USER_AGENT)))), error = function(e) NULL)
    if (is.null(page)) return(NULL)
    href <- rvest::html_attr(rvest::html_elements(page, "a[href]"), "href")
    unique(sub("[?#].*$", "", xml2::url_absolute(href, url)))
  }
  below <- function(l, folder, rx) l[startsWith(l, folder) & grepl(rx, substring(l, nchar(folder) + 1))]

  folders <- root
  year_folders <- character()
  for (depth in 1:3) {
    kids <- unlist(lapply(folders, function(f) below(links(f), f, "^[^/]+/$")))
    year <- grepl("/\\d{4}/$", kids)
    year_folders <- c(year_folders, kids[year & as.integer(stringr::str_match(kids, "/(\\d{4})/$")[, 2]) %in% years])
    folders <- unique(kids[!year])
  }

  read <- character()
  found <- list()
  for (yf in unique(year_folders)) {
    pages <- character()
    for (p in 1:100) {
      l <- links(if (p == 1) yf else paste0(yf, "?page=", p))
      if (is.null(l)) break
      new <- setdiff(below(l, yf, "^(var|host)/[^/]+\\.html$"), pages)
      if (!length(new)) {
        read <- c(read, yf)
        break
      }
      pages <- c(pages, new)
    }
    found[[yf]] <- pages
  }
  url <- unlist(found, use.names = FALSE) %||% character()
  m <- stringr::str_match(url, "/(\\d{4})/(var|host)/([^/]+)\\.html$")
  list(pages = tibble::tibble(url = url, code = tolower(vapply(m[, 4], utils::URLdecode, "", USE.NAMES = FALSE)),
                              year = as.integer(m[, 2]), semester = m[, 3]),
       read = read)
}

#' Resolve URLs for a batch of UiO courses from the sitemap
#'
#' A UiO course page sits in a faculty and department folder
#' (/studier/emner/hf/ilos/ENG2165/) that DBH does not give. The sitemap lists
#' the folder of every course taught now (the same courses as the listing
#' /studier/emner/alle/). A course not taught now keeps its page but is not
#' listed: it gets the folder most listed courses with its code prefix
#' ("ENG") have, else the one most listed courses of its DBH department
#' (department_code) have, and a wrong guess is a 404. The URL is always the
#' base URL, never a semester page (/h24/: logistics, not the plan).
#'
#' @param df A tibble with course_code and department_code columns
#' @param folders Tibble of code, folder and name, from uio_course_folders()
#' @return df with url filled in
resolve_urls_uio_batch <- function(df, folders = uio_course_folders()) {
  code <- chartr("Ø", "O", toupper(df$course_code))   # MØNA2504's folder is MONA2504
  prefix <- function(code) sub("[0-9].*", "", code)
  common <- function(key, folder) {   # the folder most courses with the key have
    dplyr::count(tibble::tibble(key, folder), key, folder) |>
      dplyr::slice_max(n, n = 1, with_ties = FALSE, by = key)
  }
  listed <- dplyr::distinct(tibble::tibble(code, dept = df$department_code)) |>
    dplyr::inner_join(folders, by = "code")
  by_prefix <- common(prefix(folders$code), folders$folder)
  by_dept <- common(listed$dept, listed$folder)
  i <- match(code, folders$code)
  guess <- dplyr::coalesce(by_prefix$folder[match(prefix(code), by_prefix$key)],
                           by_dept$folder[match(df$department_code, by_dept$key)])
  path <- dplyr::coalesce(paste(folders$folder, folders$name, sep = "/")[i],
                          dplyr::if_else(is.na(guess), NA, paste(guess, code, sep = "/")))
  cli::cli_alert_info(paste("UiO: {sum(!is.na(i))} of {nrow(df)} offerings in the sitemap,",
                            "{sum(!is.na(path))} with a URL"))
  df$url <- dplyr::if_else(is.na(path), NA, paste0("https://www.uio.no/studier/emner/", path, "/"))
  df
}

#' UiO's course folders, from its sitemap
#'
#' A few folders are named in lower case (sv/iss/sos9009), and the URL must
#' be too.
#'
#' @param index URL of the sitemap index
#' @return Tibble of code (upper case), folder ("hf/ilos") and name (the
#'   course folder's own name), one row per code
uio_course_folders <- function(index = "https://www.uio.no/sitemapindex.xml") {
  get <- function(url) httr2::resp_body_string(fetch_html_cols_single(url))
  maps <- stringr::str_match_all(get(index), "<loc>(/sitemap-www\\.uio\\.no-\\d+\\.xml)</loc>")[[1]][, 2]
  if (!length(maps)) stop("no www.uio.no sitemap in ", index)
  m <- do.call(rbind, lapply(paste0("https://www.uio.no", maps), function(url) {
    polite_pause()
    stringr::str_match_all(get(url), "<loc>https://www\\.uio\\.no/studier/emner/([a-z]+/[^/<]+)/([A-Za-z0-9-]+)/")[[1]]
  }))
  tibble::tibble(code = toupper(m[, 3]), folder = m[, 2], name = m[, 3]) |>
    dplyr::distinct(code, .keep_all = TRUE)
}

#' Resolve URLs for a batch of UiT courses via document ID discovery
#'
#' For each unique course_code, fetches the active page /utdanning/aktivt/emne/{CODE}
#' and reads its documents with uit_semester_documents().
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param .progress Show progress bar
#' @param checkpoint Discovery checkpoint folder (discover_codes()), or NULL
#' @return df with url column filled in where possible
resolve_urls_uit_batch <- function(df, .progress = TRUE, checkpoint = NULL) {
  if (nrow(df) == 0) return(df |> dplyr::mutate(url = NA_character_))

  docs <- discover_codes(df$course_code, function(code) {
    uit_semester_documents(site_html(paste0("https://uit.no/utdanning/aktivt/emne/", toupper(code))), code)
  }, checkpoint, .progress = .progress)

  if (nrow(docs) == 0) {
    cli::cli_alert_warning("No semester URLs discovered")
    return(df |> dplyr::mutate(url = NA_character_))
  }

  lookup <- docs |>
    dplyr::semi_join(df, by = c("course_code", "year", "semester_name")) |>
    uit_one_per_semester()
  cli::cli_alert_success("Discovered {nrow(lookup)} semester URLs across {length(unique(df$course_code))} course codes")

  # Join lookup to df, replacing the NA url column
  df |>
    dplyr::select(-url) |>
    dplyr::left_join(lookup, by = c("course_code", "year", "semester_name"))
}

#' The plan documents of a UiT course page, by semester
#'
#' The active page /utdanning/aktivt/emne/{CODE} shows the course's latest
#' document (its semester under "Semester / år", e.g. "Høst 2025", "Autumn
#' 2025"; its id only in the login link, `securerefresh?p_url=...?p_document_id=`)
#' and lists the others in a <select> ("Andre år og semester"; option text
#' "{CODE}: H 2020" / "{CODE}: V 2021"), not the one shown. The shown one is
#' added for a semester the list does not have. A page "Error rendering
#' component" (no course with this code) has neither.
#'
#' @param page The active page (an xml_document)
#' @param code The course code
#' @return Tibble of course_code, year, semester_name, url, in picker order,
#'   the shown document last
uit_semester_documents <- function(page, code) {
  opts <- rvest::html_elements(page, "option[value*='p_document_id']")
  m <- stringr::str_match(rvest::html_text2(opts), ":\\s+([HV])\\s+(\\d{4})")
  docs <- tibble::tibble(course_code = code, year = as.integer(m[, 3]),
                         semester_name = unname(c(H = "Høst", V = "Vår")[m[, 2]]),
                         url = paste0("https://uit.no", rvest::html_attr(opts, "value")))
  docs <- docs[!is.na(m[, 1]), ]

  shown <- stringr::str_match(rvest::html_text2(rvest::html_element(page, "#semester .sub_hero_text_innhold")),
                              "(Høst|Vår|Autumn|Spring)\\s+(\\d{4})")
  id <- stringr::str_match(rvest::html_attr(rvest::html_element(page, "a[href*='securerefresh']"), "href"),
                           "p_document_id=([1-9]\\d*)")[, 2]
  if (!is.na(shown[1]) && !is.na(id)) {
    sem <- c(Høst = "Høst", Autumn = "Høst", Vår = "Vår", Spring = "Vår")[[shown[2]]]
    year <- as.integer(shown[3])
    if (!any(docs$year == year & docs$semester_name == sem)) {
      docs <- tibble::add_row(docs, course_code = code, year = year, semester_name = sem,
                              url = paste0("https://uit.no/utdanning/emner/emne?p_document_id=", id))
    }
  }
  docs
}

#' One UiT document per semester
#'
#' The semester picker can list a semester twice: an English document (heading
#' "Course code:", for ENG-2980 V 2017 an empty page) beside the Norwegian one,
#' or two Norwegian revisions. Only for such a semester are the documents
#' fetched; the first Norwegian one is kept.
#'
#' @param lookup Tibble with course_code, year, semester_name, url, in picker order
#' @param is_english Function of a URL: TRUE for an English document
#' @return lookup with one row per semester
uit_one_per_semester <- function(lookup, is_english = uit_page_is_english) {
  lookup |>
    dplyr::mutate(english = if (dplyr::n() > 1) purrr::map_lgl(url, is_english) else FALSE,
                  .by = c(course_code, year, semester_name)) |>
    dplyr::arrange(english) |>
    dplyr::distinct(course_code, year, semester_name, .keep_all = TRUE) |>
    dplyr::select(-english)
}

uit_page_is_english <- function(url) {
  polite_pause()
  tryCatch(grepl("Course code:", as.character(site_html(url)), fixed = TRUE),
           error = function(e) FALSE)
}

#' The semester a USN plan page is for
#'
#' "Planen gjelder for: Undervisningsstart høst 2025", on an English page
#' (a course taught in English) "Applies to: Course start autumn 2025".
#'
#' @param html Text of the page (read_usn_live_html())
#' @return List of `year` (integer) and `sem` ("høst" or "vår"), NA when the
#'   page has no such line
usn_page_term <- function(html) {
  m <- stringr::str_match(html, stringr::regex(
    "(?:Undervisningsstart|Course start)\\s+(høst|vår|autumn|spring)\\s+(\\d{4})", ignore_case = TRUE))
  sem <- c(høst = "høst", vår = "vår", autumn = "høst", spring = "vår")[tolower(m[, 2])]
  list(year = as.integer(m[, 3]), sem = unname(sem))
}

# The banner of a semester without a plan of its own, which shows another
# semester's plan, in Norwegian and English
USN_NOT_READY <- "ikke klar ennå|plan for this semester is not available yet"

#' Resolve URLs for a batch of USN courses using a single browser session
#'
#' This function reuses a single Chrome session for all URL validations,
#' navigating via hash changes instead of creating new sessions. This is
#' much faster than creating a new session per URL.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param max_version Maximum version number to try per course (default: 3; every
#'   URL found so far is version 1 or 2)
#' @param delay Seconds to wait after each page is shown, before it is read and
#'   the next one asked for: the config's `request_delay` (the Crawl-delay of
#'   USN's robots.txt), at least 2 for the page to render
#' @param checkpoint_path The URL checkpoint folder, or NULL: the offerings
#'   resolved are appended to it every `save_every` offerings
#' @param .progress Show progress bar
#' @return df with url and url_version columns added
resolve_urls_usn_batch <- function(df, max_version = 3, checkpoint_path = NULL,
                                   delay = get_institution_config("usn")$request_delay,
                                   .progress = TRUE, save_every = 20) {
  if (nrow(df) == 0) {
    return(df |> dplyr::mutate(url = NA_character_, html = NA_character_))
  }

  # Start a single browser session at the base URL. read_html_live() needs
  # chromote, which rvest only suggests; naming it here puts it in renv.lock
  rlang::check_installed("chromote")
  base_url <- "https://www.usn.no/studier/studie-og-emneplaner/"
  if (!robots_allows(base_url)) stop(robots_refusal(base_url), call. = FALSE)
  session <- rvest::read_html_live(base_url)

  # Initial wait for page to fully load
  Sys.sleep(3)

  # Initialize results storage
  results <- vector("list", nrow(df))
  if (.progress) pb <- cli::cli_progress_bar(total = nrow(df))
  pending <- list()   # offerings resolved, not yet in the checkpoint
  save <- function() {
    if (!is.null(checkpoint_path)) checkpoint_append(dplyr::bind_rows(pending), checkpoint_path)
    pending <<- list()
  }
  on.exit(save())   # also after an error or an interrupt

  for (i in seq_len(nrow(df))) {
    course_id <- df$course_id[i]
    course_code <- df$course_code[i]
    year <- df$year[i]
    semester <- df$semester_name[i]

    # Convert semester to USN format
    sem <- dplyr::case_match(
      semester,
      "Vår" ~ "VÅR",
      "Høst" ~ "HØST",
      .default = toupper(semester)
    )

    # Normalize expected semester for comparison
    expected_sem_norm <- dplyr::case_match(
      tolower(semester),
      "vår" ~ "vår",
      "høst" ~ "høst",
      .default = tolower(semester)
    )

    # Default result
    result_url <- NA_character_
    result_html <- NA_character_

    # Try version numbers
    for (version in seq_len(max_version)) {
      hash <- glue::glue("#/emne/{course_code}_{version}_{year}_{sem}")
      full_url <- paste0(base_url, hash)

      tryCatch({
        # Navigate by changing the hash (much faster than new session)
        session$session$Runtime$evaluate(
          sprintf("window.location.hash = '%s';", hash)
        )

        # Wait for content to render and for the site's crawl delay
        polite_pause(pmax(delay %||% 2, 2))

        # Extract Shadow DOM content
        html_content <- read_usn_live_html(session)

        if (!is.null(html_content)) {
          # Early exit: if course code not in content, version doesn't exist.
          # Versions are sequential, so higher versions won't exist either.
          if (!stringr::str_detect(html_content, stringr::fixed(course_code))) {
            break
          }

          # Check for the expected year/semester
          shown <- usn_page_term(html_content)

          if (!is.na(shown$year)) {
            displayed_year <- shown$year
            displayed_sem <- shown$sem

            if (displayed_year == year && displayed_sem == expected_sem_norm) {
              # The notice that the semester's plan is not ready stays from the
              # page shown before when that page showed this same plan (a
              # semester without one); a reload clears it
              if (grepl(USN_NOT_READY, html_content)) {
                session$session$Page$reload()
                polite_pause(pmax(delay %||% 5, 5))
                html_content <- read_usn_live_html(session) %||% html_content
              }
              result_url <- full_url
              result_html <- html_content
              break
            }

            # Early exit: if version 1's teaching start is after requested year,
            # the course didn't exist yet. No need to check higher versions.
            if (version == 1 && displayed_year > year) {
              break
            }
          }
        }
      }, error = function(e) {
        message("Error checking version ", version, " for ", course_code, ": ", e$message)
      })
    }

    results[[i]] <- list(url = result_url, html = result_html)

    pending[[length(pending) + 1]] <- tibble::tibble(course_id = course_id, url = result_url, html = result_html)
    if (length(pending) >= save_every) save()

    if (.progress) cli::cli_progress_update(id = pb)
  }

  # Clean up the browser session
  tryCatch(
    session$session$close(),
    error = function(e) NULL
  )

  # Add results to dataframe
  df |>
    dplyr::mutate(
      url = purrr::map_chr(results, ~ .x$url %||% NA_character_),
      html = purrr::map_chr(results, ~ .x$html %||% NA_character_)
    )
}

#' Read rendered content from a USN LiveHTML session
#'
#' USN renders course content inside Shadow DOM (web components), which
#' standard html_text() cannot access. This function executes JavaScript
#' to recursively traverse shadow roots and extract text content.
#'
#' The caller is responsible for:
#' 1. Creating the session with rvest::read_html_live()
#' 2. Waiting for content to render before calling this function
#' 3. Closing the session when done
#'
#' @param session A LiveHTML session from rvest::read_html_live()
#' @return Character string with rendered page content, or NULL on error
#' @export
read_usn_live_html <- function(session) {

  # Wrap in IIFE to avoid const re-declaration errors on repeated calls
  shadow_js <- "(function() {
  // Block-level elements that should have newlines after them (like html_text2)
  const blockElements = new Set([
    'P', 'DIV', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6',
    'LI', 'TR', 'BR', 'SECTION', 'ARTICLE', 'HEADER', 'FOOTER',
    'BLOCKQUOTE', 'PRE', 'HR', 'DT', 'DD'
  ]);

  function getTextWithShadow(node) {
    let text = '';
    if (node.shadowRoot) {
      text += getTextWithShadow(node.shadowRoot);
    }
    for (const child of node.childNodes) {
      if (child.nodeType === Node.TEXT_NODE) {
        text += child.textContent;
      } else if (child.nodeType === Node.ELEMENT_NODE) {
        text += getTextWithShadow(child);
        // Add newline after block elements for readable formatting
        if (blockElements.has(child.tagName)) {
          text += '\\n';
        }
      }
    }
    return text;
  }

  // Navigate to the course content in nested shadow DOMs
  var usnStudy = document.querySelector('usn-study');
  if (usnStudy && usnStudy.shadowRoot) {
    var subjectEl = usnStudy.shadowRoot.querySelector('usn-study-subject');
    if (subjectEl && subjectEl.shadowRoot) {
      return getTextWithShadow(subjectEl.shadowRoot);
    }
  }
  return '';
})();"

  tryCatch({
    js_result <- session$session$Runtime$evaluate(shadow_js)
    content <- js_result$result$value

    if (is.null(content) || content == "") {
      return(NULL)
    }

    content
  }, error = function(e) {
    message("Error in read_usn_live_html: ", conditionMessage(e))
    NULL
  })
}
