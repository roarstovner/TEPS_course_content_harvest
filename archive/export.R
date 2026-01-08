export_institution_counts <- function(df, out_dir) {
  inst_counts <- sort(table(df$institution_name), decreasing = TRUE)
  inst_counts_df <- data.frame(institution_name = names(inst_counts),
                               n_rows = as.integer(inst_counts), row.names = NULL)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(inst_counts_df, file.path(out_dir, "institution_counts.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  invisible(inst_counts_df)
}

export_url_list <- function(df, out_dir, out_name = "url_sample_for_manual_check.txt") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, out_name)
  urls <- unique(enc2utf8(df$url)); urls <- urls[!is.na(urls) & nzchar(urls)]
  writeLines(urls, out_path, useBytes = TRUE)
  cat("???? Skrev", length(urls), "URLer til:\n", normalizePath(out_path, winslash = "/"), "\n")
}
