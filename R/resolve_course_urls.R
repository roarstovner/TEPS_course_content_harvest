#' Resolve course URLs that can't be determined from metadata alone
#'
#' Some institutions (like USN) require trial-and-error to discover valid URLs.
#' This function takes courses with NA urls and attempts to discover them.
#'
#' @param df A tibble with course_id and url columns
#' @param checkpoint_path The institution's URL checkpoint folder
#'   (checkpoint_dir(inst, "urls")); its sibling "discovery" holds the course
#'   codes UiT, hivolda and NHH have looked up (discover_codes()). Set to NULL to
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
    } else if (inst == "himolde") {
      resolve_urls_himolde_batch(df)
    } else if (inst == "phs") {
      resolve_urls_phs_batch(df)
    } else if (inst == "fhs") {
      resolve_urls_fhs_batch(df, .progress = .progress, checkpoint = discovery)
    } else if (inst == "nhh") {
      resolve_urls_nhh_batch(df, .progress = .progress, checkpoint = discovery)
    } else if (inst == "nmh") {
      resolve_urls_nmh_batch(df)
    } else if (inst == "vid") {
      resolve_urls_vid_batch(df, .progress = .progress, checkpoint = discovery)
    } else if (inst == "fjellhaug") {
      resolve_urls_fjellhaug_batch(df)
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
    semester_name = dplyr::recode_values(tolower(m[, 3]), "haust" ~ "Høst", "vår" ~ "Vår", default = m[, 3]),
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
#' (hsb1209 in ir/). A course taught in English may have its page only in
#' the English tree, /english/studies/courses/{folder}/{year}/{spring|autumn}/
#' (historical-courses/ for historiske-emner/). vortex_course_index() lists
#' the pages of the offerings' years in both trees, and of the year before a
#' spring offering's (a course over two semesters is filed under its autumn;
#' harvest_all() resolves one year at a time); vortex_match_urls() picks the
#' offering's, the teacher-education folders' URL (add_course_url_hiof())
#' first.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index Pages and year folders read, from vortex_course_index()
#' @return df with url filled in where possible
resolve_urls_hiof_batch <- function(df, index = vortex_course_index(
                                      unique(c(df$year, df$year[df$semester_name %in% "Vår"] - 1L)),
                                      c("https://www.hiof.no/studier/emner/", "https://www.hiof.no/english/studies/courses/"),
                                      get_institution_config("hiof")$request_delay, BROWSER_USER_AGENT)) {
  cli::cli_alert_info("HiOF: {nrow(index$pages)} pages in {length(index$read)} year folders")
  vortex_match_urls(df, index, as.character(add_course_url_hiof(df$course_code, df$year, df$semester_name)))
}

#' Resolve URLs for a batch of Høgskolen i Molde courses from its folder listings
#'
#' As HiOF's (resolve_urls_hiof_batch()): a page is
#' /studier/emner/{folder}/{year}/{var|host}/{code}.html, in a department
#' folder DBH does not give (os/, log/, hs/, euv/ for continuing education),
#' and a course taught in English may have its page only in the English tree,
#' /english/studies/courses/{folder}/{year}/{spring|autumn}/ (checked
#' 2026-10-31: adm120). A course over two semesters has one page, under its
#' autumn ("LOG954 Masteroppgave (Høst 2022–Vår 2023)"), so the listings of
#' the year before a spring offering's are read too.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index Pages and year folders read, from vortex_course_index()
#' @return df with url filled in where possible
resolve_urls_himolde_batch <- function(df, index = vortex_course_index(
                                         unique(c(df$year, df$year[df$semester_name %in% "Vår"] - 1L)),
                                         c("https://www.himolde.no/studier/emner/",
                                           "https://www.himolde.no/english/studies/courses/"),
                                         get_institution_config("himolde")$request_delay)) {
  cli::cli_alert_info("HiMolde: {nrow(index$pages)} pages in {length(index$read)} year folders")
  vortex_match_urls(df, index)
}

#' Resolve URLs for a batch of PHS courses from its folder listings
#'
#' As HiMolde's (resolve_urls_himolde_batch()), without department folders:
#' a page is /studier/emner/{year}/{var|host}/{code}.html, from 2022, or
#' /english/studies/courses/{year}/{spring|autumn}/{code}.html. The listings
#' hold every page (checked 2026-11-02: offerings they do not list answer
#' 404), about a quarter of DBH's offerings of 2024 and 2025. A course over
#' two semesters is filed under its autumn.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index Pages and year folders read, from vortex_course_index()
#' @return df with url filled in where possible
resolve_urls_phs_batch <- function(df, index = vortex_course_index(
                                     unique(c(df$year, df$year[df$semester_name %in% "Vår"] - 1L)),
                                     c("https://www.politihogskolen.no/studier/emner/",
                                       "https://www.politihogskolen.no/english/studies/courses/"),
                                     get_institution_config("phs")$request_delay)) {
  cli::cli_alert_info("PHS: {nrow(index$pages)} pages in {length(index$read)} year folders")
  vortex_match_urls(df, index)
}

#' Resolve URLs for a batch of NHH courses from the site's search
#'
#' NHH shows only the plan in force (plan_years "current"), one page per
#' course at a URL named from its title, /emner/{slug}/ (and, for a course
#' taught in English, the same page at /en/courses/{slug}/), which DBH does
#' not give and which changes with the title (checked 2026-10-31: sitemap
#' URLs answer 404). The Norwegian site search, /sok/?q={code}, lists the
#' course pages of both languages with their code ("MRR451 (7,5 Stp)");
#' nhh_search_pages() picks the page of the code. Each code is searched once
#' (discover_codes()), and every offering of the code gets its page.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param .progress Show progress bar
#' @param checkpoint Discovery checkpoint folder (discover_codes()), or NULL
#' @return df with url filled in where possible
resolve_urls_nhh_batch <- function(df, .progress = TRUE, checkpoint = NULL) {
  if (nrow(df) == 0) return(df |> dplyr::mutate(url = NA_character_))
  lookup <- discover_codes(df$course_code, function(code) {
    nhh_search_pages(site_html(paste0("https://www.nhh.no/sok/?q=", utils::URLencode(code, reserved = TRUE))),
                     code)
  }, checkpoint, .progress = .progress)
  lookup <- lookup[!duplicated(lookup$course_code), c("course_code", "url")]
  cli::cli_alert_success("NHH: pages for {nrow(lookup)} of {length(unique(df$course_code))} course codes")
  df |>
    dplyr::select(-url) |>
    dplyr::left_join(lookup, by = "course_code")
}

# The course page of `code` in an NHH search result page: a link around an
# article.coursePageTeaser whose courseCode is the code ("MRR451 (7,5 Stp)";
# not "MRR451-OSL", another course); of several, the first whose title does
# not mark it as no longer taught ("(expired)", "(not offered)", "utgått",
# "ikke tilbudt"). A tibble of course_code, year and semester_name (NA: the
# page serves every offering) and url; no rows if none.
nhh_search_pages <- function(page, code) {
  teaser <- rvest::html_elements(page, xpath = "//a[article[contains(@class, 'coursePageTeaser')]]")
  hit_code <- trimws(sub("\\(.*", "", rvest::html_text2(rvest::html_element(teaser, ".courseCode"))))
  title <- rvest::html_text2(rvest::html_element(teaser, ".listHeading"))
  hit <- which(toupper(hit_code) == toupper(code))
  old <- grepl("expired|not offered|utg\u00e5tt|ikke tilbudt", title[hit], ignore.case = TRUE)
  hit <- c(hit[!old], hit[old])[seq_len(min(1, length(hit)))]
  tibble::tibble(course_code = rep(code, length(hit)), year = NA_integer_, semester_name = NA_character_,
                 url = xml2::url_absolute(rvest::html_attr(teaser[hit], "href"), "https://www.nhh.no/"))
}

#' Resolve URLs for a batch of VID courses from its sitemap
#'
#' VID has one page per course at a URL named from its title,
#' /studier/emner/{slug}-{code} (the code in lower case, a space as "-":
#' PHD-DVP 902 is ...-phd-dvp-902), which DBH does not
#' give; a few codes have a second page, with a numeric id after the code,
#' for another programme or semester (checked 2026-11-01). The sitemap lists
#' every course page. An offering takes the page whose URL ends in its code;
#' of several, the one whose JSON has a plan for the offering's year and
#' semester (each such code's pages are read once, discover_codes()), else
#' the first. The pages hold plans from spring 2024 (VID_FIRST_TERM): an
#' earlier offering gets no URL, without a request.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index The course pages' URLs, from vid_course_index()
#' @param .progress Show progress bar
#' @param checkpoint Discovery checkpoint folder (discover_codes()), or NULL
#' @return df with url filled in where possible
resolve_urls_vid_batch <- function(df, index = NULL, .progress = TRUE, checkpoint = NULL) {
  df$url <- NA_character_
  new <- semester_term(df$year, df$semester_name) >= VID_FIRST_TERM
  if (!any(new)) return(df)
  index <- index %||% vid_course_index()
  slug <- sub("-[0-9]{12,}$", "", sub(".*/", "", index))
  codes <- unique(df$course_code[new])
  pages <- lapply(stats::setNames(codes, codes), function(code)
    index[endsWith(slug, paste0("-", gsub(" ", "-", tolower(code))))])
  terms <- discover_codes(names(pages)[lengths(pages) > 1], function(code) {
    dplyr::bind_rows(lapply(pages[[code]], function(url) {
      polite_pause()
      page <- httr2::resp_body_string(httr2::req_perform(site_request(url)))
      plans <- Filter(vid_has_text, vid_course_json(page)[["plans"]])
      tibble::tibble(course_code = rep(code, length(plans)), year = as.integer(vapply(plans, `[[`, "", "year")),
                     semester_name = vapply(plans, `[[`, "", "semester"), url = rep(url, length(plans)))
    }))
  }, checkpoint, .progress = .progress)
  hit <- dplyr::left_join(df[c("course_code", "year", "semester_name")], terms[!duplicated(terms[1:3]), ],
                          by = c("course_code", "year", "semester_name"))$url
  first <- vapply(pages[df$course_code[new]], function(p) p[1] %||% NA_character_, "")
  df$url[new] <- dplyr::coalesce(hit[new], unname(first))
  cli::cli_alert_success("VID: pages for {sum(!is.na(df$url))} of {nrow(df)} offerings")
  df
}

# The first term VID's course pages have plans for (spring 2024, though many
# spring 2024 entries have no text; checked 2026-11-01), as semester_term().
VID_FIRST_TERM <- semester_term(2024L, "V\u00e5r")

# Every course page in VID's sitemap (/studier/emner/...). A sitemap without
# any is an error, so the harvest stops rather than leave its courses without
# a URL.
vid_course_index <- function(sitemap = "https://www.vid.no/sitemap.xml") {
  xml <- httr2::resp_body_string(httr2::req_perform(site_request(sitemap)))
  urls <- stringr::str_match_all(xml, "<loc>(https://www\\.vid\\.no/studier/emner/[^<]+)</loc>")[[1]][, 2]
  if (!length(urls)) stop("no course page in ", sitemap)
  urls
}

#' Resolve URLs for a batch of NMH courses from its study handbooks
#'
#' NMH publishes a study handbook per cohort, /studiehandboker/startkull-{year}/
#' (from 2021; checked 2026-10-31), with a page per course at a slug of its
#' title (/emner/akkompagnement, /valgemner-2026-27/alexanderteknikk), which
#' DBH does not give. The handbook's course lists give each page's code
#' ("AKKOMP10 - Akkompagnement"); nmh_handbook_index() reads them. An
#' offering takes the page of its code in the handbook of the cohort that
#' starts in its academic year (spring 2026: startkull-2025), else in the
#' latest earlier handbook that lists the code (a course no longer in new
#' cohorts' plans, taught to the students of an earlier one); of two pages of
#' a code in one handbook, the first listed. Offerings before autumn 2021
#' get no URL, without a request.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param index Tibble of cohort, code and url, from nmh_handbook_index()
#' @return df with url filled in where possible
resolve_urls_nmh_batch <- function(df, index = NULL) {
  df$.cohort <- academic_start_year(df$year, df$semester_name)
  if (is.null(index)) {
    index <- if (any(df$.cohort >= NMH_FIRST_COHORT)) nmh_handbook_index() else
      tibble::tibble(cohort = integer(), code = character(), url = character())
  }
  index <- index[order(-index$cohort, seq_len(nrow(index))), ]
  df$.row <- seq_len(nrow(df))
  hit <- dplyr::inner_join(df[c(".row", "course_code", ".cohort")],
                           dplyr::mutate(index, course_code = code),
                           by = dplyr::join_by(course_code, .cohort >= cohort), multiple = "first")
  df$url <- hit$url[match(df$.row, hit$.row)]
  cli::cli_alert_success("NMH: pages for {sum(!is.na(df$url))} of {nrow(df)} offerings")
  dplyr::select(df, -".cohort", -".row")
}

# The first cohort with a study handbook on nmh.no (startkull-2021).
NMH_FIRST_COHORT <- 2021L

# Every course page of NMH's study handbooks, from the course lists the
# study-plan sitemaps name (/studiehandboker/startkull-{year}/emner and the
# electives' /valgemner*, about 20 lists): a tibble of cohort, code (upper
# case) and url, in list order. A list that cannot be read is an error, so
# the harvest stops rather than leave its courses without a URL.
nmh_handbook_index <- function(sitemap = "https://nmh.no/sitemaps-1-sitemap.xml") {
  locs <- function(url) {
    polite_pause()
    xml <- httr2::resp_body_string(httr2::req_perform(site_request(url)))
    stringr::str_match_all(xml, "<loc>([^<]+)</loc>")[[1]][, 2]
  }
  maps <- grep("section-studyplans_content", locs(sitemap), value = TRUE)
  if (!length(maps)) stop("no study-plan sitemap in ", sitemap)
  lists <- unique(grep("/studiehandboker/startkull-\\d{4}/(emner|valgemner[^/]*)$",
                       unlist(lapply(maps, locs)), value = TRUE))
  dplyr::bind_rows(lapply(lists, function(url) {
    polite_pause()
    nmh_list_pages(site_html(url), url)
  }))
}

# The course pages on one of NMH's course lists: the links below the list
# whose text starts with a code ("AKKOMP10 - Akkompagnement"). A page for
# several codes ("BARENS20/21", "FRINST10A/B") counts for each: a part of
# digits or letters only takes the first code's stem.
nmh_list_pages <- function(page, list_url) {
  a <- rvest::html_elements(page, "main a[href]")
  href <- rvest::html_attr(a, "href")
  m <- stringr::str_match(rvest::html_text2(a), "^\\s*([A-Za-zÆØÅæøå0-9]+(?:\\s*/\\s*[A-Za-z0-9]+)*)\\s+[-–]\\s")
  keep <- startsWith(href, paste0(list_url, "/")) & !is.na(m[, 2])
  parts <- strsplit(toupper(m[keep, 2]), "\\s*/\\s*")
  code <- unlist(lapply(parts, function(p) {
    stem <- c(digits = sub("\\d+$", "", p[1]), letters = sub("[A-Z]+$", "", p[1]))
    c(p[1], ifelse(grepl("^\\d+$", p[-1]), paste0(stem["digits"], p[-1]),
                   ifelse(grepl("^[A-Z]+$", p[-1]), paste0(stem["letters"], p[-1]), p[-1])))
  }))
  tibble::tibble(cohort = as.integer(stringr::str_match(list_url, "startkull-(\\d{4})")[, 2]),
                 code = code, url = rep(href[keep], lengths(parts)))
}

#' The page of each offering, from a Vortex site's folder listings
#'
#' The page of the offering's code (the file name, lower case; else without
#' Norwegian letters, ø as o; else also without dashes, æ as e), year and semester; of several, the one at `built` (hiof: the teacher-education
#' folders), then a Norwegian one, then one outside historiske-emner and
#' historical-courses. A spring offering without a page takes its course's
#' page of the autumn before: HiOF and HiMolde file a course over two
#' semesters ("Høst 2013–Vår 2014") under its autumn only, and a page for the
#' autumn alone gives the spring offering no plan (`page_end_term`). An
#' offering without a page whose year folder at `built` was not read (its
#' listing failed) keeps that URL; else its URL is NA.
#'
#' @param df A tibble with course_code, year, semester_name
#' @param index List of `pages` (tibble of url, code, year, semester) and
#'   `read` (the year folders read in full)
#' @param built The offerings' URLs built from DBH's columns, or NULL
#' @return df with url
vortex_match_urls <- function(df, index, built = NULL) {
  p <- index$pages
  p <- p[order(!p$url %in% built, grepl("/english/", p$url),
               grepl("/historiske-emner/|/historical-courses/", p$url)), ]
  key <- paste(p$code, p$year, p$semester)
  code <- tolower(df$course_code)
  # the code, else the code without Norwegian letters, as Vortex names a page
  # (HiMolde: BØK705 at bok705.html; HiOF keeps them, løk101)
  ascii <- stringr::str_replace_all(code, c("ø" = "o", "å" = "a", "æ" = "ae"))
  # else both without dashes and with æ as e (PHS: EL-KRISE at elkrise.html,
  # LEDLÆRUTV at ledlerutv.html; checked 2026-11-03)
  loose <- function(x) gsub("-", "", stringr::str_replace_all(x, c("ø" = "o", "å" = "a", "æ" = "e")))
  # only where no other code has the same loose form (hiof LFPTP-DX/LFPTPDX)
  loose_key <- paste(loose(p$code), p$year, p$semester)
  lc <- loose(code)
  lc[lc %in% names(which(tapply(code, lc, \(x) length(unique(x))) > 1))] <- NA
  page <- function(i, year, semester) dplyr::coalesce(p$url[match(paste(code[i], year, semester), key)],
                                                      p$url[match(paste(ascii[i], year, semester), key)],
                                                      p$url[match(ifelse(is.na(lc[i]), NA, paste(lc[i], year, semester)), loose_key, incomparables = NA)])
  url <- page(seq_along(code), df$year, semester_to_url(df$semester_name))
  spring <- is.na(url) & df$semester_name %in% "Vår"
  url[spring] <- page(which(spring), df$year[spring] - 1L, "host")
  if (!is.null(built)) {
    unread <- is.na(url) & !sub("[^/]+/[^/]+$", "", built) %in% index$read
    url[unread] <- built[unread]
  }
  df$url <- url
  df
}

#' A Vortex site's course pages of some years, from its folder listings
#'
#' Walks each root three levels down (faculty, department, the
#' historiske-emner folders) to the year folders, and reads each year folder
#' of `years` page by page (?page=2, ...) until a page lists no new course
#' page. HiOF: about 20 pages a listing page, some 1000 requests for all
#' years. A year folder that lists only its semester folders (HiMolde) is
#' read through them. A year folder whose listing fails is not in `read`. The
#' English tree's semester folders are spring/ and autumn/.
#'
#' @param years Calendar years of the offerings
#' @param root The course folders, such as the Norwegian and the English tree
#' @param delay Seconds between requests (polite_pause())
#' @param user_agent The user agent (hiof: BROWSER_USER_AGENT)
#' @return List of `pages`, a tibble of url, code (lower case), year and
#'   semester ("var", "host"), and `read`, the year folders read in full
vortex_course_index <- function(years, root, delay = NULL, user_agent = TEPS_USER_AGENT) {
  links <- function(url) {
    polite_pause(delay)
    page <- tryCatch(rvest::read_html(httr2::resp_body_string(httr2::req_perform(
      site_request(url, user_agent)))), error = function(e) NULL)
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

  # A folder's course pages, page by page, with attributes `ok` (every
  # listing page read) and `first` (the first page's links)
  listing <- function(folder, rx) {
    pages <- character()
    first <- character()
    for (p in 1:100) {
      l <- links(if (p == 1) folder else paste0(folder, "?page=", p))
      if (is.null(l)) break
      if (p == 1) first <- l
      new <- setdiff(below(l, folder, rx), pages)
      if (!length(new)) return(structure(pages, ok = TRUE, first = first))
      pages <- c(pages, new)
    }
    structure(pages, ok = FALSE, first = first)
  }
  read <- character()
  found <- list()
  for (yf in unique(year_folders)) {
    pages <- listing(yf, "^(var|host|spring|autumn)/[^/]+\\.html$")
    # a year folder that lists only its semester folders (HiMolde's os/2025/):
    # their listings
    sem <- below(attr(pages, "first"), yf, "^(var|host|spring|autumn)/$")
    if (!length(pages) && length(sem)) {
      sp <- lapply(sem, listing, rx = "^[^/]+\\.html$")
      pages <- structure(unlist(sp) %||% character(), ok = all(vapply(sp, attr, logical(1), "ok")))
    }
    if (attr(pages, "ok")) read <- c(read, yf)
    found[[yf]] <- as.vector(pages)
  }
  url <- unlist(found, use.names = FALSE) %||% character()
  m <- stringr::str_match(url, "/(\\d{4})/(var|host|spring|autumn)/([^/]+)\\.html$")
  list(pages = tibble::tibble(url = url, code = tolower(vapply(m[, 4], utils::URLdecode, "", USE.NAMES = FALSE)),
                              year = as.integer(m[, 2]),
                              semester = unname(c(var = "var", host = "host", spring = "var", autumn = "host")[m[, 3]])),
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
    sem <- dplyr::recode_values(
      semester,
      "Vår" ~ "VÅR",
      "Høst" ~ "HØST",
      default = toupper(semester)
    )

    # Normalize expected semester for comparison
    expected_sem_norm <- dplyr::recode_values(
      tolower(semester),
      "vår" ~ "vår",
      "høst" ~ "høst",
      default = tolower(semester)
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

#' Resolve URLs for a batch of FHS courses from each code's versions
#'
#' Forsvarets høgskole's course pages are on Forsvaret's site,
#' /utdanning/emner/{CODE}/{year}-{HØST|VÅR}, a page per version of the plan
#' from autumn 2018, for the semester it starts in: a course taught every year
#' may have pages for some years only (MILM1701: autumn 2020, 2021, 2023 and
#' 2025; checked 2026-11-04). The code's page, /utdanning/emner/{CODE},
#' redirects to the latest, which lists them all ("Emnehistorikk"); a code without pages
#' answers 404. Each code's page is read once (discover_codes()). An offering
#' takes the latest version that starts in or before its semester: the one
#' in force.
#'
#' @param df A tibble with course_code, year, semester_name columns
#' @param .progress Show progress bar
#' @param checkpoint Discovery checkpoint folder (discover_codes()), or NULL
#' @return df with url filled in where possible
resolve_urls_fhs_batch <- function(df, .progress = TRUE, checkpoint = NULL) {
  base <- "https://www.forsvaret.no/utdanning/emner/"
  pages <- discover_codes(df$course_code, function(code) {
    page <- tryCatch(site_html(utils::URLencode(paste0(base, code))), httr2_http_404 = function(e) xml2::read_html("<p></p>"))
    links <- rvest::html_elements(page, "ul[aria-labelledby='toggleSemesters'] a")
    m <- stringr::str_match(rvest::html_attr(links, "href"), "^(\\d{4})-(HØST|VÅR)$")
    tibble::tibble(course_code = code, year = as.integer(m[, 2]),
                   semester_name = ifelse(m[, 3] == "VÅR", "Vår", "Høst"),
                   url = utils::URLencode(paste0(base, code, "/", m[, 1])))[!is.na(m[, 1]), ]
  }, checkpoint, .progress = .progress)
  pages$term <- semester_term(pages$year, pages$semester_name)
  term <- semester_term(df$year, df$semester_name)
  df$url <- vapply(seq_len(nrow(df)), function(i) {
    p <- pages[pages$course_code == df$course_code[i] & pages$term <= term[i], ]
    if (nrow(p)) p$url[which.max(p$term)] else NA_character_
  }, "")
  cli::cli_alert_success("FHS: pages for {sum(!is.na(df$url))} of {nrow(df)} offerings")
  df
}

#' Resolve URLs for a batch of Fjellhaug courses from its sitemaps
#'
#' Fjellhaug (fih.fjellhaug.no, Craft CMS) shows each course's plan in force,
#' without a year, at /emne/{slug} or, for most courses taught in English,
#' /en/subjects/{slug} only; the slug is the code in lower case, for a few
#' courses followed by the title (/emne/btm2003n-markusevangeliet-10-ects-stp),
#' which DBH does not give (checked 2026-11-05). fjellhaug_course_index()
#' lists the pages. An offering takes the page of its code; DBH's codes of
#' the Danish campuses and of some variants carry a suffix the site does not
#' have (BTM2008-DK1, BMM1001-IS, EVU1001-EVU), and take the page of the code
#' without it, whose facts box names the campuses ("Aarhus, København"). Of a
#' Norwegian and an English page of one code, the Norwegian.
#'
#' @param df A tibble with a course_code column
#' @param index The course pages' URLs, from fjellhaug_course_index()
#' @return df with url filled in where possible
resolve_urls_fjellhaug_batch <- function(df, index = fjellhaug_course_index()) {
  index <- index[order(grepl("/en/", index, fixed = TRUE))]
  code <- toupper(sub("-.*", "", sub(".*/", "", index)))
  df$url <- index[match(sub("-[A-Z]+[0-9]*$", "", toupper(df$course_code)), code)]
  cli::cli_alert_success("Fjellhaug: pages for {sum(!is.na(df$url))} of {nrow(df)} offerings")
  df
}

# Every course page in Fjellhaug's subject sitemaps, Norwegian and English
# (/emne/..., /en/subjects/...). The study programmes' pages link no other
# live course page (checked 2026-11-05: their one extra link, /emne/heb1504,
# answers 404). A site without any is an error, so the harvest stops rather
# than leave its courses without a URL.
fjellhaug_course_index <- function(root = "https://fih.fjellhaug.no") {
  locs <- function(url) {
    polite_pause()
    xml <- httr2::resp_body_string(httr2::req_perform(site_request(url)))
    stringr::str_match_all(xml, "<loc>([^<]+)</loc>")[[1]][, 2]
  }
  maps <- c(locs(paste0(root, "/sitemaps-1-sitemap.xml")), locs(paste0(root, "/en/sitemaps-1-sitemap.xml")))
  pages <- unlist(lapply(grep("-section-subjects-", maps, value = TRUE), locs))
  pages <- unique(grep(paste0("^", root, "/(emne|en/subjects)/[^/]+$"), pages, value = TRUE))
  if (!length(pages)) stop("no course page in the sitemaps of ", root)
  pages
}
