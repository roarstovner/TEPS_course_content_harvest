get_paths <- function(root = getwd(), input_file = "courses.RDS") {
  list(
    path_in_rds      = file.path(root, "data", "input",  input_file),
    path_cfg_urls    = file.path(root, "config", "institutions.yaml"),
    path_cache_rds   = file.path(root, "data", "cache",  "courses_std.RDS"),
    path_out_dir     = file.path(root, "data", "output"),
    path_urls_cache  = file.path(root, "data", "cache",  "urls.RDS"),
    path_check_csv   = file.path(root, "data", "output", "url_check.csv")
  )
}

read_courses <- function(path) {
  if (!file.exists(path)) stop("Finner ikke ", path)
  readRDS(path)
}

write_rds_safe <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path); invisible(path)
}

write_csv_stamp <- function(df, out_dir, prefix) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M")
  fp <- file.path(out_dir, sprintf("%s_%s.csv", prefix, stamp))
  utils::write.csv(df, fp, row.names = FALSE, fileEncoding = "UTF-8")
  fp
}
