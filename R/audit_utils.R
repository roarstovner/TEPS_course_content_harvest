# R/audit_utils.R
# Shared helpers for the per-institution audit harness
# (.claude/skills/audit-institutions). Each check has an R/audit_prepare_{check}.R
# that builds one review packet per institution; R/audit_aggregate.R verifies
# and merges the findings the review agents write back.
#
# Layout per check:
#   data/audit/{check}/packets/{inst}.md    review packet (gitignored, regenerated)
#   data/audit/{check}/manifest.csv         one row per packet
#   data/audit/{check}/sample.csv           (institution, course_id, kind) shown in packets
#   data/audit/{check}/findings/{inst}.json agent output (kept)
#   data/audit/{check}/findings_report.md   aggregated report (kept)

suppressMessages({
  library(dplyr)
  library(stringr)
})

# Allowed `target` / `error_type` values per check. The recipes in
# .claude/skills/audit-institutions/checks/ explain what each value means —
# keep the two in sync.
CANONICAL_SECTIONS <- c("course_content", "learning_outcomes", "teaching_methods",
                        "assessment", "coursework_requirements", "prerequisites",
                        "reading_list")

AUDIT_CHECKS <- list(
  sections = list(
    targets = c(CANONICAL_SECTIONS, "cross_section", "all"),
    error_types = c("missing_section", "truncated", "wrong_content",
                    "merged_sections", "split_section", "boilerplate_only",
                    "empty_placeholder", "field_or_language_junk", "duplicate",
                    "formatting_noise", "other")
  ),
  fulltext = list(
    targets = c(CANONICAL_SECTIONS, "metadata", "page_chrome", "whole_text"),
    error_types = c("missing_content", "junk_included", "wrong_page",
                    "wrong_year", "empty_or_failed", "formatting", "truncated",
                    "duplicate_content", "other")
  ),
  anonymization = list(
    targets = c("person_name", "email", "phone", "admin_date", "boilerplate",
                "content_text", "structure", "all"),
    error_types = c("pii_leak", "over_removal", "text_corruption",
                    "boilerplate_left", "admin_date_left", "structure_loss",
                    "other")
  )
)

audit_dir <- function(check) file.path("data", "audit", check)

# Institutions requested on the command line (empty = all).
audit_args_institutions <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args[!startsWith(args, "--")]
}

audit_trunc <- function(x, n) {
  if (length(x) == 0 || is.na(x)) x <- ""
  if (nchar(x) > n) {
    paste0(substr(x, 1, n), "\n…[truncated ", nchar(x) - n, " chars]")
  } else x
}

# Four-backtick fence so text that itself contains ``` cannot close the block.
audit_fence <- function(x) c("````", x, "````")

# Stratified sample for one institution: up to `suspect_n` highest-scoring
# suspects plus `random_n` random non-suspects, one course per `dedup_key`
# (distinct plan text), so near-identical offerings do not crowd the packet.
#
# `pool` has columns course_id, dedup_key, score (0 = not suspect).
audit_sample <- function(pool, suspect_n, random_n, seed = 42) {
  set.seed(seed)
  susp <- pool |>
    filter(score > 0) |>
    arrange(desc(score)) |>
    distinct(dedup_key, .keep_all = TRUE) |>
    head(suspect_n)
  rand_pool <- pool |>
    filter(score == 0, !dedup_key %in% susp$dedup_key) |>
    distinct(dedup_key, .keep_all = TRUE)
  rand <- rand_pool |> slice_sample(n = min(random_n, nrow(rand_pool)))
  tibble(course_id = c(susp$course_id, rand$course_id),
         kind = c(rep("SUSPECT", nrow(susp)), rep("RANDOM", nrow(rand))))
}

# Standard packet header. `what` is a one-paragraph description of what the
# agent audits in this packet.
audit_packet_header <- function(check, inst, selected, what) {
  cfg <- AUDIT_CHECKS[[check]]
  c(
    sprintf("# %s audit packet — %s", check, inst),
    "",
    sprintf("%d courses: %d suspects + %d random controls.", nrow(selected),
            sum(selected$kind == "SUSPECT"), sum(selected$kind == "RANDOM")),
    "",
    what,
    "",
    sprintf("Allowed `target` values: %s", paste0("`", cfg$targets, "`", collapse = ", ")),
    "",
    sprintf("Allowed `error_type` values: %s",
            paste0("`", cfg$error_types, "`", collapse = ", ")),
    ""
  )
}

# Write the packets' index files. When only some institutions were rebuilt,
# rows for the other institutions are kept.
audit_write_index <- function(check, manifest, sample) {
  dir <- audit_dir(check)
  merge_csv <- function(new, path) {
    if (file.exists(path)) {
      old <- read.csv(path, stringsAsFactors = FALSE)
      if ("institution_short" %in% names(old)) {   # files from before #182
        old <- rename(old, institution = institution_short)
      }
      new <- bind_rows(filter(old, !institution %in% new$institution), new)
    }
    new <- arrange(new, institution)
    write.csv(new, path, row.names = FALSE)
  }
  merge_csv(manifest, file.path(dir, "manifest.csv"))
  merge_csv(sample, file.path(dir, "sample.csv"))
  cat(sprintf("\nWrote %d packet(s) to %s/packets/ (manifest.csv, sample.csv updated)\n",
              nrow(manifest), dir))
}

# Fail early when an input is older than the file it is derived from.
audit_require_fresh <- function(path, newer_than, hint) {
  if (!file.exists(path)) stop(path, " not found. ", hint, call. = FALSE)
  for (src in newer_than) {
    if (file.exists(src) && file.mtime(src) > file.mtime(path)) {
      stop(path, " is older than ", src, ". ", hint, call. = FALSE)
    }
  }
  invisible(TRUE)
}
