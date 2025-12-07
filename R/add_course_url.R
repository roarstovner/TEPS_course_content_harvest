add_course_url <- function(df) {
  df |>
    dplyr::mutate(
      url = dplyr::case_match(
        institution_short,
        
        "hiof"    ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hivolda" ~ add_course_url_hivolda(Emnekode),
        "uio"     ~ add_course_url_uio(Emnekode, Avdelingsnavn),
        "ntnu"    ~ add_course_url_ntnu(Emnekode),
        "uia"     ~ add_course_url_uia(Emnekode, Årstall, Semesternavn),
        "uit"     ~ add_course_url_uit(Emnekode),
        "uib"     ~ add_course_url_uib(Emnekode),
        "nord"    ~ add_course_url_nord(Emnekode),
        "hvl"     ~ add_course_url_hvl(Emnekode),
        
        .default  = NA_character_
      )
    )
}


add_course_url_ntnu <- function(course_code) {
  glue::glue("https://www.ntnu.no/studier/emner/{toupper(course_code)}")
}

add_course_url_uia <- function(course_code, year, semester) {
  # enkel mapping av semester til slug
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


add_course_url_hiof <- function(course_code, year, semester){
  is_historical <- year < 2021 | (year == 2021 & semester == "Vår")
  semester <- dplyr::case_match(semester, "Vår" ~ "var", "Høst" ~ "host")
  if_else(
    is_historical,
    glue::glue("https://www.hiof.no/studier/emner/historiske-emner/lu/{year}/{semester}/{tolower(course_code)}.html"),
    glue::glue("https://www.hiof.no/studier/emner/lusp/lusp/{year}/{semester}/{tolower(course_code)}.html")
  )
}

add_course_url_hivolda <- function(course_code){
  glue::glue("https://www.hivolda.no/emne/{course_code}")
}

add_course_url_uio <- function(course_code, faculty_name) {
  # Map fra Avdelingsnavn -> (faculty_slug, inst_slug)
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
    "Institutt for sosiologi og samfunnsgeografi" = c("sv", "iss"),
    "Kjemisk institutt" = c("matnat", "kjemi"),
    "Klassisk og romansk institutt" = c("hf", "ifikk"),
    "Matematisk institutt" = c("matnat", "math"),
    "Naturfagsenteret" = c("matnat", "naturfagsenteret")
  )
  
  # Sørg for at key-strengene faktisk blir tolket som UTF-8
  names(uio_map) <- enc2utf8(names(uio_map))
  faculty_name   <- enc2utf8(faculty_name)
  
  n <- length(course_code)
  fac_slug  <- character(n)
  inst_slug <- character(n)
  
  for (i in seq_len(n)) {
    key  <- faculty_name[i]
    pair <- uio_map[[key]]
    if (is.null(pair)) {
      fac_slug[i]  <- NA_character_
      inst_slug[i] <- NA_character_
    } else {
      fac_slug[i]  <- pair[1]
      inst_slug[i] <- pair[2]
    }
  }
  
  glue::glue(
    "https://www.uio.no/studier/emner/{fac_slug}/{inst_slug}/{toupper(course_code)}/"
  )
}
