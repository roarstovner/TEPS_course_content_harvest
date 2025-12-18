add_course_url <- function(df) {
  df |>
    dplyr::mutate(
      url = dplyr::case_match(
        institution_short,
        
        "oslomet" ~ add_course_url_oslomet(Emnekode, Årstall, Semesternavn),
        "uia"     ~ add_course_url_uia(Emnekode, Årstall, Semesternavn),
        "ntnu"    ~ add_course_url_ntnu(Emnekode, Årstall),
        "inn"     ~ add_course_url_inn(Emnekode, Årstall, Semesternavn),
        
        "hivolda" ~ add_course_url_hivolda(Emnekode),
        "hiof"    ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hvl"     ~ add_course_url_hvl(Emnekode),
        
        "mf"      ~ add_course_url_mf(Emnekode),
        "nla"     ~ add_course_url_nla(Emnekode, Årstall),
        
        "nord"    ~ add_course_url_nord(Emnekode),
        "nih"     ~ add_course_url_nih(Emnekode, Årstall, Semesternavn),
        
        "uib"     ~ add_course_url_uib(Emnekode),
        "uio"     ~ add_course_url_uio(Emnekode, Avdelingsnavn),
        "uis"     ~ add_course_url_uis(Emnekode),
        "usn"     ~ add_course_url_usn(Emnekode, Årstall, Semesternavn),
        "uit"     ~ add_course_url_uit(Emnekode),
        "nmbu"    ~ add_course_url_nmbu(Emnekode),
        
        .default  = NA_character_
      )
    )
}

add_course_url_ntnu <- function(course_code, year) {
  glue::glue("https://www.ntnu.no/studier/emner/{toupper(course_code)}/{year}")
}

add_course_url_uia <- function(course_code, year, semester) {
  sem <- ifelse(
    semester == "Vår",  "var",
    ifelse(semester == "Høst", "host", tolower(semester))
  )
  glue::glue("https://www.uia.no/studier/emner/{year}/{sem}/{tolower(course_code)}.html")
}

add_course_url_uit <- function(course_code) {
  glue::glue("https://uit.no/utdanning/aktivt/emne/{toupper(course_code)}")
}

add_course_url_uib <- function(course_code) {
  glue::glue("https://www4.uib.no/studier/emner/{tolower(course_code)}")
}

add_course_url_nord <- function(course_code) {
  glue::glue("https://www.nord.no/studier/emner/{tolower(course_code)}")
}

add_course_url_hvl <- function(course_code) {
  glue::glue("https://www.hvl.no/studier/studieprogram/emne/{course_code}")
}

add_course_url_hiof <- function(course_code, year, semester) {
  is_historical <- year < 2021 | (year == 2021 & semester == "Vår")
  sem <- dplyr::case_match(semester, "Vår" ~ "var", "Høst" ~ "host")
  
  dplyr::if_else(
    is_historical,
    glue::glue("https://www.hiof.no/studier/emner/historiske-emner/lu/{year}/{sem}/{tolower(course_code)}.html"),
    glue::glue("https://www.hiof.no/studier/emner/lusp/lusp/{year}/{sem}/{tolower(course_code)}.html")
  )
}

add_course_url_hivolda <- function(course_code) {
  glue::glue("https://www.hivolda.no/emne/{course_code}")
}

add_course_url_uio <- function(course_code, faculty_name) {
  uio_map <- list(
    "Biologisk institutt" = c("matnat", "ibv"),
    "Det utdanningsvitenskapelige fakultet" = c("uv", "uv"),
    "Fysisk institutt" = c("matnat", "fys"),
    "Institutt for arkeologi, konservering og historiske studier" = c("hf", "iakh"),
    "Institutt for biovitenskap" = c("matnat", "ibv"),
    "Institutt for geofag" = c("matnat", "geofag"),
    "Institutt for informatikk" = c("matnat", "ifi"),
    "Institutt for lingvistiske og nordiske studier" = c("hf", "iln"),
    "Institutt for litteratur, områdestudier og europeiske språk" = c("hf", "ilos"),
    "Institutt for lærerutdanning og skoleforskning" = c("uv", "ils"),
    "Institutt for medier og kommunikasjon" = c("hf", "imk"),
    "Institutt for molekylær biovitenskap" = c("matnat", "ibv"),
    "Institutt for nordistikk og litteraturvitenskap" = c("hf", "iln"),
    "Institutt for sosiologi og samfunnsgeografi" = c("sv", "sv"),
    "Kjemisk institutt" = c("matnat", "kjemi"),
    "Klassisk og romansk institutt" = c("hf", "ifikk"),
    "Matematisk institutt" = c("matnat", "math"),
    "Naturfagsenteret" = c("matnat", "naturfagsenteret")
  )
  
  fac_slug  <- faculty_name |> purrr::map_chr(\(x) uio_map[[x]][1] %||% NA_character_)
  inst_slug <- faculty_name |> purrr::map_chr(\(x) uio_map[[x]][2] %||% NA_character_)
  
  glue::glue("https://www.uio.no/studier/emner/{fac_slug}/{inst_slug}/{toupper(course_code)}/")
}

add_course_url_oslomet <- function(course_code, year, semester) {
  sem <- ifelse(
    semester == "Vår",  "var",
    ifelse(semester == "Høst", "host", tolower(semester))
  )
  glue::glue("https://student.oslomet.no/studier/-/studieinfo/emne/{toupper(course_code)}/{year}/{sem}")
}

add_course_url_inn <- function(course_code, year, semester) {
  sem <- ifelse(
    semester == "Vår",  "var",
    ifelse(semester == "Høst", "host", tolower(semester))
  )
  glue::glue("https://studiekatalog.edutorium.no/inn/nb/emne/{course_code}/{year}-{sem}")
}

add_course_url_mf <- function(course_code) {
  glue::glue("https://mf.no/studier/emner/{tolower(course_code)}")
}

add_course_url_nla <- function(course_code, year) {
  glue::glue("https://www.nla.no/studietilbud/emner/{year}/{tolower(course_code)}/")
}

add_course_url_nih <- function(course_code, year, semester) {
  sem <- ifelse(
    semester == "Vår",  "var",
    ifelse(semester == "Høst", "host", tolower(semester))
  )
  glue::glue("https://www.nih.no/studier/emner/{year}/{sem}/{tolower(course_code)}.html")
}

add_course_url_uis <- function(course_code) {
  glue::glue("https://www.uis.no/nb/course/{toupper(course_code)}")
}

add_course_url_usn <- function(course_code, year, semester) {
  sem <- ifelse(
    semester == "Vår",  "var",
    ifelse(semester == "Høst", "host", tolower(semester))
  )
  glue::glue("https://www.usn.no/studier/studie-og-emneplaner/#/emne/{course_code}_{year}_{sem}")
}

add_course_url_nmbu <- function(course_code) {
  glue::glue("https://www.nmbu.no/emne/{toupper(course_code)}")
}
