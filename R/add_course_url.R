add_course_url <- function(df) {
  df |>
    mutate(
      url = case_match(
        institution_short,
        "hiof" ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hivolda" ~ add_course_url_hivolda(Emnekode),
        "uio"    ~ add_course_url_uio(Emnekode, Avdelingsnavn),
        .default = NA_character_
      )
    )
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
  
  uio_map <- list(
    "Biologisk institutt"                                         = c("matnat", "ibv"),
    "Det utdanningsvitenskapelige fakultet"                       = c("uv", "uv"),
    "Fysisk institutt"                                            = c("matnat", "fys"),
    "Institutt for arkeologi, konservering og historiske studier" = c("hf", "iakh"),
    "Institutt for biovitenskap"                                  = c("matnat", "ibv"),
    "Institutt for geofag"                                        = c("matnat", "geofag"),
    "Institutt for informatikk"                                   = c("matnat", "ifi"),
    "Institutt for lingvistiske og nordiske studier"              = c("hf", "iln"),
    
    # OBS: denne tok du rett fra konsollen, ikke fra kopiert tekst
    "Institutt for litteratur, områdestudier og europeiske språk" = c("hf", "ilos"),
    
    "Institutt for lærerutdanning og skoleforskning"              = c("uv", "ils"),
    "Institutt for medier og kommunikasjon"                       = c("hf", "imk"),
    "Institutt for molekylær biovitenskap"                        = c("matnat", "ibv"),
    "Institutt for nordistikk og litteraturvitenskap"             = c("hf", "iln"),
    "Institutt for sosiologi og samfunnsgeografi"                 = c("sv", "iss"),
    "Kjemisk institutt"                                           = c("matnat", "kjemi"),
    "Klassisk og romansk institutt"                               = c("hf", "ilos"),
    "Matematisk institutt"                                        = c("matnat", "math"),
    "Naturfagsenteret"                                            = c("matnat", "naturfag")
  )
  
  
  pair <- uio_map[[faculty_name]]
  
  # Hvis instituttet ikke finnes i listen → returner NA
  if (is.null(pair)) return(NA_character_)
  
  fac_slug  <- pair[1]
  inst_slug <- pair[2]
  
  glue::glue("https://www.uio.no/studier/emner/{fac_slug}/{inst_slug}/{toupper(course_code)}/")
}
