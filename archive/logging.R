`%||%` <- function(a,b) if (!is.null(a)) a else b
log_info <- function(...) cat("[INFO]", format(Sys.time(), "%H:%M:%S"), ..., "\n")
log_warn <- function(...) cat("[WARN]", format(Sys.time(), "%H:%M:%S"), ..., "\n")
