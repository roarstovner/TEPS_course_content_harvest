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
  # Fakultets-slugs (fra Avdelingsnavn)
  uio_fac_map <- c(
    "det humanistiske fakultet"                   = "hf",
    "det juridiske fakultet"                      = "jus",
    "det matematisk-naturvitenskapelige fakultet" = "matnat",
    "det medisinske fakultet"                     = "med",
    "det odontologiske fakultet"                  = "odont",
    "det samfunnsvitenskapelige fakultet"         = "sv",
    "det teologiske fakultet"                     = "teologi",
    "det utdanningsvitenskapelige fakultet"       = "uv"
  )
  
  # Institutt-slugs (fra Fagnavn / institutt-navn)
  uio_inst_map <- c(
    # HF
    "institutt for arkeologi, konservering og historiske studier" = "iakh",
    "institutt for kulturstudier og orientalske sprak"            = "ikos",
    "institutt for filosofi, ide- og kunsthistorie og klassiske sprak" = "ifikk",
    "institutt for litteratur, omradestudier og europeiske sprak" = "ilos",
    "institutt for lingvistiske og nordiske studier"              = "iln",
    "institutt for medier og kommunikasjon"                       = "imk",
    # SV
    "institutt for sosiologi og samfunnsgeografi"                 = "iss",
    "institutt for statsvitenskap"                                = "statsvitenskap",
    "psykologisk institutt"                                       = "psi",
    "sosialantropologisk institutt"                               = "sai",
    "okonomisk institutt"                                         = "oekonomi",
    # MATNAT
    "institutt for informatikk"                                   = "ifi",
    "institutt for geofag"                                        = "geofag",
    "kjemisk institutt"                                           = "kjemi",
    "fysisk institutt"                                            = "fys",
    "matematisk institutt"                                        = "math",
    "institutt for biovitenskap"                                  = "ibv",
    "farmasoytisk institutt"                                      = "farmasi",
    "institutt for teoretisk astrofysikk"                         = "astro",
    "institutt for teknologisystemer"                             = "its",
    # MED
    "institutt for helse og samfunn"                              = "helsam",
    "institutt for medisinske basalfag"                           = "imb",
    "institutt for klinisk medisin"                               = "klinmed",
    # ODONT
    "institutt for oral biologi"                                  = "iob",
    "institutt for klinisk odontologi"                            = "iko",
    # UV
    "institutt for larerutdanning og skoleforskning"              = "ils",
    "institutt for spesialpedagogikk"                             = "isp",
    "institutt for pedagogikk"                                    = "iped",
    # Generiske
    "det samfunnsvitenskapelige fakultet"                         = "sv",
    "det teologiske fakultet"                                     = "tf"
  )
  
  fac_slug  <- uio_fac_map[faculty_name]
  inst_slug <- uio_inst_map[field_name]
  
  glue::glue(
    "https://www.uio.no/studier/emner/{fac_slug}/{inst_slug}/{toupper(course_code)}/"
  )
}
