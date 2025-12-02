add_course_url <- function(df) {
  df |>
    mutate(
      url = case_match(
        institution_short,
        "hiof" ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hivolda" ~ add_course_url_hivolda(Emnekode),
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
