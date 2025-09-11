### Les YAML filer ###

load_selectors <- function(inst_short, cfg_path = "config/selectors.yaml") {
  stopifnot(file.exists(cfg_path))
  y <- yaml::read_yaml(cfg_path)
  sel <- y$selectors[[inst_short]]
  if (is.null(sel)) stop("Fant ingen selectors for inst='", inst_short, "' i ", cfg_path)
  list(
    fulltext = sel$fulltext %||% NA_character_,
    course_name_no = sel$course_name_no %||% NA_character_
  )
}

