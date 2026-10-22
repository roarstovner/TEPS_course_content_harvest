add_course_url <- function(df) {
  df |>
    dplyr::mutate(
      url = dplyr::case_match(
        institution,

        "oslomet" ~ add_course_url_oslomet(course_code, year, semester_name),
        "uia"     ~ add_course_url_uia(course_code, year, semester_name),
        "ntnu"    ~ add_course_url_ntnu(course_code, year, semester_name),
        "inn"     ~ add_course_url_inn(course_code, year, semester_name),

        "hivolda" ~ NA_character_,  # Hivolda requires URL discovery via resolve_course_urls()
        "hiof"    ~ NA_character_,  # HiOF's faculty folders come from its listings via resolve_course_urls()
        "hvl"     ~ add_course_url_hvl(course_code, year, semester_name),

        "mf"      ~ add_course_url_mf(course_code),
        "nla"     ~ add_course_url_nla(course_code, year, semester_name),

        "nord"    ~ add_course_url_nord(course_code, year, semester_name),
        "nih"     ~ add_course_url_nih(course_code, year, semester_name),

        "uib"     ~ add_course_url_uib(course_code, year, semester_name),
        "uio"     ~ NA_character_,  # UiO's department folders come from its sitemap via resolve_course_urls()
        "uis"     ~ add_course_url_uis(course_code_raw),
        "usn"     ~ NA_character_,  # USN requires version discovery via resolve_course_urls()
        "uit"     ~ NA_character_,  # UiT requires document ID discovery via resolve_course_urls()
        "nmbu"    ~ add_course_url_nmbu(course_code),

        .default  = NA_character_
      )
    )
}

# The year in the URL is the academic year it starts ("/2025": 2025/26), so a
# spring offering reads the year before (url_year = "academic"). DBH's "-A"
# codes (MGLU1101-A) are the site's code without the suffix.
add_course_url_ntnu <- function(course_code, year, semester) {
  glue::glue("https://www.ntnu.no/studier/emner/{sub('-A$', '', toupper(course_code))}/",
             "{academic_start_year(year, semester)}")
}

# DBH's variant suffixes "-G", "-G2" and "-D" (MA-015-G, TFL011-G2, PRA090-D)
# are not in the site's code: the page is at the code without it (checked
# 2026-10-21: ma-015, tfl011, pra090; never the code with the suffix). The
# trailing version number ("-1") is already off in `course_code`.
add_course_url_uia <- function(course_code, year, semester) {
  code <- tolower(sub("-(G[0-9]?|D)$", "", course_code, ignore.case = TRUE))
  glue::glue("https://www.uia.no/studier/emner/{year}/{semester_to_url(semester)}/{code}.html")
}

# UiT URL generation removed - requires document ID discovery via resolve_course_urls()
# Active page /utdanning/aktivt/emne/{CODE} has a <select> dropdown with historical document IDs

# A code with a space (the design semester blocks, "BAR 3") is percent-encoded:
# a raw space made the request fail as a timeout; www4 has no page for them.
add_course_url_uib <- function(course_code, year, semester) {
  sem_code <- dplyr::case_match(semester, "Vår" ~ "V", "Høst" ~ "H", .default = "H")
  code <- gsub(" ", "%20", tolower(course_code), fixed = TRUE)
  glue::glue("https://www4.uib.no/studier/emner/{code}?start_semester={year}{sem_code}")
}

add_course_url_nord <- function(course_code, year, semester) {
  # NORD accepts HØST/VÅR (Norwegian characters) as semester params.
  # ASCII "HOEST" falls back to a generic page without year-specific content.
  # The site maps each semester to its academic year automatically
  # (e.g. VÅR 2023 → 2022/23, HØST 2023 → 2023/24).
  # Must be percent-encoded for httr2 (server returns 400 on raw UTF-8).
  url_semester <- dplyr::case_match(semester, "Vår" ~ "V%C3%85R", "Høst" ~ "H%C3%98ST", .default = "H%C3%98ST")
  glue::glue("https://www.nord.no/studier/emner/{tolower(course_code)}?year={year}&semester={url_semester}")
}

# "Emneplan for studieåret 2025/2026" is at /2025/: a spring offering reads
# the year before (url_year = "academic")
add_course_url_hvl <- function(course_code, year, semester) {
  glue::glue("https://www.hvl.no/studier/studieprogram/emne/{academic_start_year(year, semester)}/{course_code}")
}

# The teacher-education folders only: resolve_urls_hiof_batch() finds every
# faculty's page and prefers this URL when the course is in several folders
add_course_url_hiof <- function(course_code, year, semester) {
  is_historical <- year < 2021 | (year == 2021 & semester == "Vår")

  dplyr::if_else(
    is_historical,
    glue::glue("https://www.hiof.no/studier/emner/historiske-emner/lu/{year}/{semester_to_url(semester)}/{tolower(course_code)}.html"),
    glue::glue("https://www.hiof.no/studier/emner/lusp/lusp/{year}/{semester_to_url(semester)}/{tolower(course_code)}.html")
  )
}

# Hivolda URL generation removed - requires URL discovery via resolve_course_urls()
# Base page /emne/{CODE} lists semester-specific version URLs with numeric IDs

# The site only accepts "HØST": "/2022/HØST" is the plan of study year
# 2022/2023, so a spring offering reads the year before (url_year =
# "academic"); VÅR URLs return 404
add_course_url_oslomet <- function(course_code, year, semester) {
  glue::glue("https://student.oslomet.no/studier/-/studieinfo/emne/{toupper(course_code)}/",
             "{academic_start_year(year, semester)}/HØST")
}

add_course_url_inn <- function(course_code, year, semester) {
  dplyr::if_else(
    year >= 2022,
    glue::glue("https://studiekatalog.edutorium.no/inn/nb/emne/{course_code}/{year}-{semester_to_url(semester)}"),
    NA_character_
  )
}

add_course_url_mf <- function(course_code) {
  glue::glue("https://mf.no/studier/emner/{tolower(course_code)}")
}

add_course_url_nla <- function(course_code, year, semester) {
  # NLA uses academic year ranges: Autumn 2023 → 2023-2024, Spring 2024 → 2023-2024
  start_year <- dplyr::if_else(semester == "Vår", year - 1L, year)
  end_year <- start_year + 1L
  glue::glue("https://www.nla.no/for-studenter/Studie-%20og%20emneplaner/emneplan/{course_code}/{start_year}-{end_year}")
}

add_course_url_nih <- function(course_code, year, semester) {
  # Vortex names the page without the Norwegian letters: KRØ100 → kro100
  # (verified for ø on 2026-10-21; å → a and æ → ae follow Vortex's rule)
  slug <- tolower(course_code) |>
    stringr::str_replace_all(c("ø" = "o", "å" = "a", "æ" = "ae"))
  glue::glue("https://www.nih.no/studier/emner/{year}/{semester_to_url(semester)}/{slug}.html")
}

add_course_url_uis <- function(course_code_raw) {
  # UiS URLs use the raw course code with dash replaced by underscore (e.g., BYG100-1 → BYG100_1)
  # The /nb/student/course/ path covers more courses than /nb/course/
  code <- gsub("-", "_", course_code_raw)
  glue::glue("https://www.uis.no/nb/student/course/{toupper(code)}")
}

# USN URL generation removed - requires version discovery via resolve_course_urls()

add_course_url_nmbu <- function(course_code) {
  # The site names the page without the Norwegian letters, as Vortex does
  # (MILJØ300 → MILJO300; with Ø, raw or percent-encoded, it answers 404;
  # verified 2026-10-22)
  slug <- toupper(course_code) |>
    stringr::str_replace_all(c("Ø" = "O", "Å" = "A", "Æ" = "AE"))
  glue::glue("https://www.nmbu.no/emne/{slug}")
}
