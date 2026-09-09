# R/prepare_section_review.R
# Build per-institution review packets for the section-extraction QA agents
# (issue #200, stage A of the section-QA plan).
#
# Each packet is a self-contained markdown file an in-Claude-Code review agent
# reads to audit one institution's section extraction. It contains a stratified
# sample of course offerings: deterministic SUSPECTS (from
# sections_qa_suspects.RDS) plus a RANDOM control slice, each shown as
#   (a) the full anonymized course_plan  — ground truth, and
#   (b) the extractor's sections_raw rows — what to audit.
#
# The agent compares (b) against (a) using section_codebook.yml as the rubric
# and emits findings per section_review_findings_schema.md. No API calls happen
# here — this only prepares input files.
#
# Outputs:
#   data/section_review/packets/{inst}.md
#   data/section_review/manifest.csv
#
# Run:  Rscript R/prepare_section_review.R

suppressMessages({
  library(dplyr)
  library(stringr)
})

# ── Tunables ─────────────────────────────────────────────────────────────────
SUSPECT_N   <- 20     # suspect courses per institution (distinct plans)
RANDOM_N    <- 8      # random control courses per institution (distinct plans)
PLAN_TRUNC  <- 8000   # max chars of course_plan shown
SECT_TRUNC  <- 5000   # max chars of each section raw_text shown. Must stay
                      # above the p99 of the audited prose sections (~4.4k) so
                      # the packet does not create spurious "truncated"
                      # findings. reading_list (p99 ~28k) is still capped — its
                      # length is expected, judge it as such.
SEED        <- 42

out_dir <- "data/section_review/packets"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load ─────────────────────────────────────────────────────────────────────
sec      <- readRDS("data/sections_raw.RDS")
suspects <- readRDS("data/sections_qa_suspects.RDS")
plans    <- readRDS("data/courses_with_plan_id.RDS") |>
  select(course_id, institution_short, course_plan, plan_content_id)

flag_cols <- c("flag_empty", "flag_short", "flag_long", "flag_blob",
               "flag_leak", "flag_dup_in_course", "flag_boilerplate")

# Per-(course, section) compact flag label, e.g. "leak->coursework_requirements"
susp_lbl <- suspects |>
  rowwise() |>
  mutate(flags = {
    on <- flag_cols[c_across(all_of(flag_cols))]
    on <- str_remove(on, "^flag_")
    if ("leak" %in% on && nzchar(leak_sections))
      on[on == "leak"] <- paste0("leak->", leak_sections)
    paste(on, collapse = ",")
  }) |>
  ungroup() |>
  select(course_id, section, flags)

# Course-level suspicion score (sum of flags across its sections)
course_score <- suspects |>
  group_by(course_id, institution_short) |>
  summarise(n_flags = sum(n_flags), .groups = "drop")

trunc_note <- function(x, n) {
  x <- x %||% ""
  if (is.na(x)) x <- ""
  if (nchar(x) > n) paste0(substr(x, 1, n), "\n…[truncated ", nchar(x) - n,
                           " chars]") else x
}

# ── Per-institution packet builder ───────────────────────────────────────────
institutions <- sort(unique(sec$institution_short))
manifest <- list()

for (inst in institutions) {
  set.seed(SEED)

  inst_plans <- plans |> filter(institution_short == inst)
  if (nrow(inst_plans) == 0) next

  # Suspect courses: highest score first, one per distinct plan.
  susp_ids <- course_score |>
    filter(institution_short == inst) |>
    inner_join(inst_plans, by = c("course_id", "institution_short")) |>
    arrange(desc(n_flags)) |>
    distinct(plan_content_id, .keep_all = TRUE) |>
    head(SUSPECT_N) |>
    pull(course_id)

  # Random control: courses with extracted sections, not suspects, distinct plan.
  have_sec_ids <- sec |> filter(institution_short == inst) |> pull(course_id)
  rand_pool <- inst_plans |>
    filter(course_id %in% have_sec_ids, !course_id %in% susp_ids) |>
    distinct(plan_content_id, .keep_all = TRUE)
  rand_ids <- if (nrow(rand_pool) > 0)
    rand_pool |> slice_sample(n = min(RANDOM_N, nrow(rand_pool))) |> pull(course_id)
  else character()

  selected <- tibble(course_id = c(susp_ids, rand_ids),
                     kind = c(rep("SUSPECT", length(susp_ids)),
                              rep("RANDOM",  length(rand_ids))))
  if (nrow(selected) == 0) next

  lines <- c(
    sprintf("# Section-extraction review packet — %s", inst),
    "",
    sprintf("%d courses: %d suspects + %d random controls.",
            nrow(selected), length(susp_ids), length(rand_ids)),
    "",
    "Read `section_codebook.yml` (section definitions = the rubric) and",
    "`section_review_findings_schema.md` (your output format) before starting.",
    "For each course, audit the **Extractor output** against the **Full course",
    "plan** using the codebook definitions. Aggregate recurring problems into",
    "findings; emit one JSON object per the schema.",
    ""
  )

  for (i in seq_len(nrow(selected))) {
    cid  <- selected$course_id[i]
    kind <- selected$kind[i]
    plan_txt <- inst_plans$course_plan[match(cid, inst_plans$course_id)]
    rows <- sec |> filter(course_id == cid) |>
      left_join(susp_lbl, by = c("course_id", "section"))

    lines <- c(lines,
      sprintf("---\n\n## COURSE %d — `%s`  [%s]", i, cid, kind),
      "",
      "### Full course plan (anonymized — ground truth)",
      "",
      "```",
      trunc_note(plan_txt, PLAN_TRUNC),
      "```",
      "",
      "### Extractor output (sections_raw — audit these)",
      ""
    )
    if (nrow(rows) == 0) {
      lines <- c(lines, "_(no sections extracted for this course)_", "")
    } else {
      for (j in seq_len(nrow(rows))) {
        fl <- rows$flags[j]
        flhdr <- if (!is.na(fl) && nzchar(fl)) sprintf("  ⚑ flags: %s", fl) else ""
        lines <- c(lines,
          sprintf("**%s** (%d chars)%s", rows$section[j],
                  nchar(rows$raw_text[j] %||% ""), flhdr),
          "",
          "```",
          trunc_note(rows$raw_text[j], SECT_TRUNC),
          "```",
          "")
      }
    }
  }

  path <- file.path(out_dir, paste0(inst, ".md"))
  writeLines(lines, path)
  manifest[[inst]] <- tibble(
    institution_short = inst,
    n_suspect = length(susp_ids),
    n_random  = length(rand_ids),
    packet    = path
  )
  cat(sprintf("  %-8s %2d suspects + %2d random -> %s\n",
              inst, length(susp_ids), length(rand_ids), path))
}

manifest_df <- bind_rows(manifest)
write.csv(manifest_df, "data/section_review/manifest.csv", row.names = FALSE)
cat(sprintf("\nWrote %d packets + manifest to data/section_review/\n",
            nrow(manifest_df)))
