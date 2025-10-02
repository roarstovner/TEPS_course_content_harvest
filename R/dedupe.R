dedupe_urls_by_first_year <- function(df) {
  if (!nrow(df)) return(df)
  df <- df[order(df$year, df$institution_short, df$course_code), ]
  dup <- duplicated(df$url)
  df[!dup, , drop = FALSE]
}
