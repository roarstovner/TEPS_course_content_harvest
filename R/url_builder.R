library(glue); library(yaml)

validate_cfg <- function(cfg) {
  bad <- vapply(cfg$institutions, function(x) {
    pat <- if (is.null(x$url_pattern)) "" else x$url_pattern
    grepl("\\(", pat, perl = TRUE)  # ikke tillat funksjonskall i YAML
  }, logical(1))
  if (any(bad)) stop("Oppdater institutions.yaml: url_pattern m?? bruke tokens ??? ikke funksjonskall. Feil hos: ",
                     paste(names(which(bad)), collapse=", "))
  invisible(TRUE)
}

custom_url_builders <- list(
  # "inn" = function(course_code, year, semester) sprintf("https://studiekatalog.edutorium.no/inn/nb/emne/%s", toupper(course_code))
)

build_url_one <- function(cfg, inst_short, course_code, year, semester) {
  if (is.na(inst_short) || !nzchar(inst_short)) return(NA_character_)
  if (!is.null(custom_url_builders[[inst_short]]))
    return(custom_url_builders[[inst_short]](course_code, year, semester))
  
  inst <- cfg$institutions[[inst_short]]
  if (is.null(inst)) return(NA_character_)
  pattern <- inst$url_pattern; style <- inst$semester_style %||% "plain"
  if (is.null(pattern) || !nzchar(pattern)) return(NA_character_)
  
  code_tokens <- make_code_tokens(course_code)
  data_env <- c(code_tokens, list(year = as.character(year),
                                  semester = semester,
                                  semester_url = standardize_semester(semester, style)))
  tryCatch(glue::glue_data(.x = data_env, pattern) |> as.character(),
           error = function(e) NA_character_)
}

build_urls <- function(df, cfg) {
  validate_cfg(cfg)
  supported <- names(cfg$institutions)
  df$supported_inst <- !is.na(df$institution_short) & df$institution_short %in% supported
  df$url <- NA_character_
  idx <- which(df$supported_inst)
  if (length(idx)) {
    df$url[idx] <- mapply(
      FUN = build_url_one,
      inst_short = df$institution_short[idx],
      course_code = df$course_code[idx],
      year        = df$year[idx],
      semester    = df$semester_name[idx],
      MoreArgs    = list(cfg = cfg),
      SIMPLIFY    = TRUE, USE.NAMES = FALSE
    )
  }
  df
}
