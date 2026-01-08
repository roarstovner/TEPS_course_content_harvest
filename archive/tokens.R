library(stringi)

norm_no <- function(x) stringi::stri_trans_general(tolower(ifelse(is.na(x),"",x)), "Latin-ASCII")

standardize_semester <- function(semester, style = c("uia","ntnu","inn","url","hiof","nih","plain","oslomet")) {
  style <- match.arg(style)
  s <- norm_no(semester)
  if (style=="uia")  return(ifelse(s %in% c("host","haust","h"), "host", ifelse(s %in% c("var","v","vaar"), "var", s)))
  if (style=="ntnu") return(ifelse(s %in% c("host","haust","h"), "1", ifelse(s %in% c("var","v","vaar"), "2", s)))
  if (style=="hiof") return(ifelse(s %in% c("var","v","vaar","spring"), "var", ifelse(s %in% c("host","haust","h","autumn"), "host", s)))
  if (style=="nih")  return(ifelse(s %in% c("host","haust","h","autumn"), "host", ifelse(s %in% c("var","v","vaar","spring"), "var", s)))
  if (style=="inn")  return(s)
  if (style=="url")  return(utils::URLencode(s, reserved = TRUE))
  if (style=="oslomet") {
    hv <- ifelse(s %in% c("host","haust","h","autumn"), "H??ST", ifelse(s %in% c("var","v","vaar","spring"), "V??R", toupper(s)))
    return(utils::URLencode(hv, reserved = TRUE))
  }
  s
}

canon_remove_trailing_instance1 <- function(x) sub("([\\-_.])1$", "", x, perl=TRUE)
canon_remove_trailing_letter1   <- function(x) sub("([\\-_.])[A-Za-z]$", "", x, perl=TRUE)
canon_remove_trailing_num       <- function(x) sub("([\\-_.])[0-9]+$", "", x, perl=TRUE)

make_code_tokens <- function(code) {
  raw <- trimws(code)
  nodash1   <- canon_remove_trailing_instance1(raw)
  nosfxA    <- canon_remove_trailing_letter1(raw)
  nosfx1A   <- canon_remove_trailing_letter1(canon_remove_trailing_instance1(raw))
  nodashnum <- canon_remove_trailing_num(raw)
  base      <- nodashnum
  list(
    course_code          = raw,
    code_lower           = tolower(raw),
    code_upper           = toupper(raw),
    code_lower_nodash1   = tolower(nodash1),
    code_upper_nodash1   = toupper(nodash1),
    code_lower_nosfxA    = tolower(nosfxA),
    code_upper_nosfxA    = toupper(nosfxA),
    code_lower_nosfx1A   = tolower(nosfx1A),
    code_upper_nosfx1A   = toupper(nosfx1A),
    code_lower_nodashnum = tolower(nodashnum),
    code_upper_nodashnum = toupper(nodashnum),
    code_lower_base      = tolower(base),
    code_upper_base      = toupper(base)
  )
}
