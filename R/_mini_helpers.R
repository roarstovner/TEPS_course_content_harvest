tpl_sub <- function(pattern, env) {
  for (k in names(env)) {
    pattern <- gsub(paste0("\\{", k, "\\}"), env[[k]], pattern, perl = TRUE)
  }
  pattern
}
code_upper_base <- function(x) toupper(sub("([-_.])[0-9]+$", "", trimws(x)))
code_lower_base <- function(x) tolower(sub("([-_.])[0-9]+$", "", trimws(x)))
code_upper      <- function(x) toupper(trimws(x))
code_lower      <- function(x) tolower(trimws(x))

std_sem_ntnu <- function(s) {
  s <- tolower(s); ifelse(s %in% c("host","haust","h"), "1",
                          ifelse(s %in% c("var","v","vaar"), "2", s))
}
std_sem_uia <- function(s) {
  s <- tolower(s); ifelse(s %in% c("host","haust","h"), "host",
                          ifelse(s %in% c("var","v","vaar"), "var", s))
}
std_sem_hiof <- function(s) {
  s <- tolower(s); ifelse(s %in% c("var","v","vaar","spring"), "var",
                          ifelse(s %in% c("host","haust","h","autumn"), "host", s))
}
std_sem_nih <- function(s) {
  s <- tolower(s); ifelse(s %in% c("host","haust","h","autumn"), "host",
                          ifelse(s %in% c("var","v","vaar","spring"), "var", s))
}
