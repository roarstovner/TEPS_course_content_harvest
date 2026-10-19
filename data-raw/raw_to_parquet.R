## One-time conversion of the raw store from RDS to Parquet (2026-10-16)
##
## Writes every RDS raw file (data/raw/html_{inst}.RDS, the first harvests,
## and data/raw/harvests/{date}/html_{inst}.RDS) one to one as a run of the
## Parquet store, data/raw/{inst}/{year}/{run}.parquet (write_raw_run()), then
## reads each run back and compares it with its RDS file, column by column and
## row by row; any difference stops the script. A dated file's run is its
## directory. The first harvests (2026-09-30 to 2026-10-06) did not stamp
## harvested_at and were not dated: their run is the day their RDS file was
## written, a name only (harvested_at stays empty). The RDS files are only
## read. They were removed from data/raw on 2026-10-19 and are in the restic
## snapshot tagged pre-parquet; restore them there to rerun this script.
##
## Run once from the repository root: Rscript data-raw/raw_to_parquet.R [dest]
## (dest: data/raw; another folder for a trial run).

source("R/utils.R")

src <- RAW_DIR
dest <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(dest)) dest <- RAW_DIR

first <- Sys.glob(file.path(src, "html_*.RDS"))
dated <- sort(Sys.glob(file.path(src, "harvests", "*", "html_*.RDS")), method = "radix")
rds <- tibble::tibble(
  file = c(first, dated),
  institution = sub("^html_(.*)\\.RDS$", "\\1", basename(file)),
  run = c(format(file.mtime(first), "%Y-%m-%d"), basename(dirname(dated)))
)
stopifnot(!anyDuplicated(rds[c("institution", "run")]))

# The RDS rows as the Parquet store keeps them (write_raw_run()).
as_stored <- function(was) {
  was <- dplyr::rename(was, dplyr::any_of(c(institution = "institution_short")))
  html <- was$html
  was <- dplyr::mutate(was, dplyr::across(dplyr::where(~ inherits(.x, "glue")), as.character))
  was$html_error <- fetch_error_text(was$html_error)
  was$extracted_text[!is.na(html) & nzchar(html)] <- NA
  was
}

for (i in seq_len(nrow(rds))) {
  was <- readRDS(rds$file[i])
  files <- write_raw_run(was, rds$institution[i], rds$run[i], dest)
  now <- read_raw(files)
  exp <- as_stored(was)
  if (!identical(names(now), names(exp)) || nrow(now) != nrow(exp)) {
    stop(rds$file[i], ": columns or rows differ")
  }
  for (col in names(exp)) {
    same <- mapply(identical, now[[col]], exp[[col]])
    if (!identical(class(now[[col]]), class(exp[[col]])) || !all(same)) {
      stop(rds$file[i], ": column ", col, " differs in ", sum(!same), " rows")
    }
  }
  # course_id, url and html as in the RDS file, row by row
  stopifnot(identical(now$course_id, was$course_id), identical(now$url, as.character(was$url)),
            identical(now$html, was$html))
  message(sprintf("%s -> %s/%s/*/%s.parquet: %d rows in %d files, %.1f -> %.1f MB",
                  rds$file[i], dest, rds$institution[i], rds$run[i], nrow(now), length(files),
                  file.size(rds$file[i]) / 1e6, sum(file.size(files)) / 1e6))
}
message(nrow(rds), " RDS files converted")
