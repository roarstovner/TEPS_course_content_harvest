add_course_url <- function(df) {
  df |>
    mutate(
      url = case_match(
        institution_short,
        "hiof" ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hivolda" ~ add_course_url_hivolda(Emnekode),
        "uio"    ~ add_course_url_uio(Emnekode, Avdelingsnavn, Navn),
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

add_course_url_uio <- function(course_code, faculty_name, field_name) {
  
  # Fakultets-slugs
  uio_fac_map <- c(
    "det humanistiske fakultet"                  = "hf",
    "det juridiske fakultet"                     = "jus",
    "det matematisk-naturvitenskapelige fakultet"= "matnat",
    "det medisinske fakultet"                    = "med",
    "det odontologiske fakultet"                 = "odont",
    "det samfunnsvitenskapelige fakultet"        = "sv",
    "det teologiske fakultet"                    = "teologi",
    "det utdanningsvitenskapelige fakultet"      = "uv"
  )
  
  # Institutt-slugs
  uio_inst_map <- list(
    # HF
    "institutt for arkeologi, konservering og historiske studier" = c("hf","iakh"),
    "institutt for kulturstudier og orientalske sprak"            = c("hf","ikos"),
    "institutt for filosofi, ide- og kunsthistorie og klassiske sprak" = c("hf","ifikk"),
    "institutt for litteratur, omradestudier og europeiske sprak" = c("hf","ilos"),
    "institutt for lingvistiske og nordiske studier"              = c("hf","iln"),
    "institutt for medier og kommunikasjon"                       = c("hf","imk"),
    # SV
    "institutt for sosiologi og samfunnsgeografi"                 = c("sv","iss"),
    "institutt for statsvitenskap"                                = c("sv","statsvitenskap"),
    "psykologisk institutt"                                       = c("sv","psi"),
    "sosialantropologisk institutt"                               = c("sv","sai"),
    "okonomisk institutt"                                         = c("sv","oekonomi"),
    # MATNAT
    "institutt for informatikk"                                   = c("matnat","ifi"),
    "institutt for geofag"                                        = c("matnat","geofag"),
    "kjemisk institutt"                                           = c("matnat","kjemi"),
    "fysisk institutt"                                            = c("matnat","fys"),
    "matematisk institutt"                                        = c("matnat","math"),
    "institutt for biovitenskap"                                  = c("matnat","ibv"),
    "farmasoytisk institutt"                                      = c("matnat","farmasi"),
    "institutt for teoretisk astrofysikk"                         = c("matnat","astro"),
    "institutt for teknologisystemer"                             = c("matnat","its"),
    # MED
    "institutt for helse og samfunn"                              = c("med","helsam"),
    "institutt for medisinske basalfag"                           = c("med","imb"),
    "institutt for klinisk medisin"                               = c("med","klinmed"),
    # ODONT
    "institutt for oral biologi"                                  = c("odont","iob"),
    "institutt for klinisk odontologi"                            = c("odont","iko"),
    # UV
    "institutt for larerutdanning og skoleforskning"              = c("uv","ils"),
    "institutt for spesialpedagogikk"                             = c("uv","isp"),
    "institutt for pedagogikk"                                    = c("uv","iped"),
    # Generiske
    "det samfunnsvitenskapelige fakultet"                         = c("sv","sv"),
    "det teologiske fakultet"                                     = c("teologi","tf")
  )
  
  fac_slug  <- uio_fac_map[faculty_name]
  inst_slug <- uio_inst_map[[field_name]][2]
  
  glue::glue(
    "https://www.uio.no/studier/emner/{fac_slug}/{inst_slug}/{toupper(course_code)}/"
  )
}
