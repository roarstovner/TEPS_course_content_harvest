# =========================================================
# Clean output folders: keep ONLY latest files per institution
# - Keeps: course_urls_latest.csv/.txt, candidates_latest.csv/.txt, .gitkeep
# - Deletes: all timestamped CSV/TXT (including per-season variants)
# =========================================================

dry_run  <- FALSE     # set TRUE to preview, FALSE to actually delete
base_dir <- "data/output"

# files to always keep
keepers <- c(
  "course_urls_latest.csv", "course_urls_latest.txt",
  "candidates_latest.csv",  "candidates_latest.txt",
  ".gitkeep"
)

stopifnot(dir.exists(base_dir))

# find institution dirs (one level)
inst_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
inst_dirs <- inst_dirs[file.info(inst_dirs)$isdir]

# helper: choose deletions in a dir
to_delete_in_dir <- function(d) {
  files <- list.files(d, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(files)) return(character(0))
  keep_mask <- basename(files) %in% keepers
  # delete only files (never remove dirs here)
  del <- files[ !keep_mask & !file.info(files)$isdir ]
  del
}

# collect
all_delete <- unlist(lapply(inst_dirs, to_delete_in_dir), use.names = FALSE)

cat(if (dry_run) "[DRY RUN] " else "", "Found ", length(all_delete), " files to delete.\n", sep = "")
if (length(all_delete)) {
  cat("Examples:\n  ", paste(head(all_delete, 10), collapse = "\n  "), "\n", sep = "")
}

# delete
if (!dry_run && length(all_delete)) {
  res <- unlink(all_delete, force = TRUE)
  deleted <- sum(res == 0)
  failed  <- sum(res != 0)
  cat("Deleted ", deleted, " files. Failed: ", failed, "\n", sep = "")
} else {
  cat("Nothing deleted (dry-run or nothing to remove).\n")
}

# show remaining files per institution (quick check)
cat("\nRemaining files per institution (first few shown per dir):\n")
for (d in inst_dirs) {
  keep <- list.files(d, all.files = TRUE, no.. = TRUE)
  if (!length(keep)) next
  cat("- ", basename(d), ":\n", sep = "")
  cat("   ", paste(head(keep, 10), collapse = "\n    "), "\n", sep = "")
}

# Optional: remove empty subfolders inside each institution dir (rare)
# If you want this, set remove_empty <- TRUE
remove_empty <- FALSE
if (remove_empty) {
  for (d in inst_dirs) {
    subs <- list.dirs(d, recursive = TRUE, full.names = TRUE)
    subs <- subs[subs != d]
    # remove empty leaf dirs only
    leafs <- subs[!subsapply(subs, function(x) any(dirname(subs) == x))]
    for (s in rev(leafs)) {
      if (length(list.files(s, all.files = TRUE, no.. = TRUE)) == 0) {
        if (!dry_run) unlink(s, recursive = TRUE, force = TRUE)
      }
    }
  }
  cat("\nEmpty subfolders cleanup done (if any).\n")
}
