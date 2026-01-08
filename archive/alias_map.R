library(yaml); library(stringi)

norm_utf8  <- function(x) enc2utf8(ifelse(is.na(x), "", x))
norm_ascii <- function(x) stringi::stri_trans_general(norm_utf8(x), "Latin-ASCII")
norm_lower <- function(x) tolower(norm_ascii(x))

load_institution_config <- function(path_yaml) yaml::read_yaml(path_yaml)

apply_alias <- function(df, cfg) {
  aliases <- cfg$aliases; if (is.null(aliases)) aliases <- list()
  aliases <- unlist(aliases, use.names = TRUE)
  if (length(aliases)) names(aliases) <- norm_utf8(names(aliases))
  
  fallback_map <- c(
    "oslomet"="oslomet","agder"="uia","ntnu"="ntnu","innlandet"="inn","ostfold"="hiof",
    "vestlandet"="hvl","mf"="mf","nla"="nla","nord"="nord","idrett"="nih","bergen"="uib",
    "oslo"="uio","stavanger"="uis","sorost"="usn","arktiske"="uit","volda"="hivolda",
    "nmbu"="nmbu","miljo"="nmbu","samisk"="samas","sami " = "samas"
  )
  map_to_short <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    nx <- norm_utf8(x); if (nx %in% names(aliases)) return(aliases[[nx]])
    xl <- norm_lower(x)
    for (k in names(fallback_map)) if (grepl(k, xl, fixed=TRUE)) return(fallback_map[[k]])
    NA_character_
  }
  df$institution_short <- vapply(df$institution_name, map_to_short, character(1))
  df
}
