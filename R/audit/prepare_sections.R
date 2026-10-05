# R/audit/prepare_sections.R
# Build per-institution review packets for the `sections` audit
# (.claude/skills/audit-institutions/checks/sections.md; issue #200).
#
# Each packet is a self-contained markdown file one review agent reads to audit
# one institution's section extraction. It contains a stratified sample of
# course offerings: deterministic SUSPECTS (from sections_qa_suspects.RDS) plus
# a RANDOM control slice, each shown as
#   (a) the full anonymized course_plan  — ground truth, and
#   (b) the extractor's sections_raw rows — what to audit.
#
# Inputs (regenerate in this order if stale):
#   data/interim/course_offerings_full.RDS   targets::tar_make()
#   data/processed/sections_raw.RDS            targets::tar_make()
#   data/audit/sections/sections_qa_suspects.RDS    Rscript R/audit/qa_sections.R
#
# Outputs:
#   data/audit/sections/packets/{inst}.md
#   data/audit/sections/manifest.csv, sample.csv
#
# Run:  Rscript R/audit/prepare_sections.R [inst ...]

source("R/audit/utils.R")

# ── Tunables ─────────────────────────────────────────────────────────────────
# 14/6 keeps packets near ~150 KB, the size Sonnet agents read in full
# (they skimmed 185-325 KB packets in the 2026-10-01 run; see synthesis.md).
SUSPECT_N   <- 14     # suspect courses per institution (distinct plans)
RANDOM_N    <- 6      # random control courses per institution (distinct plans)
ZERO_N      <- 4      # of the suspects: at most this many with no sections at all
PLAN_TRUNC  <- 8000   # max chars of course_plan shown
SECT_TRUNC  <- 5000   # max chars of each section raw_text shown. Must stay
                      # above the p99 of the audited prose sections (~4.4k) so
                      # the packet does not create spurious "truncated"
                      # findings. reading_list (p99 ~28k) is still capped — its
                      # length is expected, judge it as such.
RL_TRUNC    <- 2000   # reading_list cap: long lists are expected, the head shows the format
MAX_KB      <- 150    # packet budget (#233): see the fitting loop below
SEED        <- 42

CHECK   <- "sections"
out_dir <- file.path(audit_dir(CHECK), "packets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load ─────────────────────────────────────────────────────────────────────
audit_require_fresh("data/processed/sections_raw.RDS", "data/interim/course_offerings_full.RDS",
                    "Run targets::tar_make().")
audit_require_fresh("data/audit/sections/sections_qa_suspects.RDS", "data/processed/sections_raw.RDS",
                    "Run Rscript R/audit/qa_sections.R.")

sec      <- readRDS("data/processed/sections_raw.RDS")
suspects <- readRDS("data/audit/sections/sections_qa_suspects.RDS")
plans    <- readRDS("data/interim/course_offerings_full.RDS") |>
  select(course_id, institution, course_plan, plan_content_id,
         Emnekode_raw, Emnenavn, Årstall, Semesternavn)

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
  group_by(course_id) |>
  summarise(score = sum(n_flags), .groups = "drop")

# ── Per-institution packet builder ───────────────────────────────────────────
institutions <- sort(unique(sec$institution))
requested <- audit_args_institutions()
if (length(requested) > 0) institutions <- intersect(institutions, requested)
manifest <- list()
sample   <- list()

for (inst in institutions) {
  inst_plans <- plans |> filter(institution == inst)
  if (nrow(inst_plans) == 0) next

  # Courses with plan text but no sections at all never reach the pre-pass,
  # which flags section rows. Put up to ZERO_N of them first among the
  # suspects so the agent sees what the extractor missed entirely.
  have_sec_ids <- sec |> filter(institution == inst) |> pull(course_id)
  set.seed(SEED)
  zero_ids <- inst_plans |>
    filter(!course_id %in% have_sec_ids, nchar(coalesce(course_plan, "")) > 50) |>
    distinct(plan_content_id, .keep_all = TRUE) |>
    slice_sample(n = ZERO_N) |>
    pull(course_id)

  # Courses with extracted sections are eligible as random controls; suspects
  # are eligible regardless.
  pool <- inst_plans |>
    left_join(course_score, by = "course_id") |>
    mutate(score = coalesce(score, 0) + if_else(course_id %in% zero_ids, 1000, 0)) |>
    filter(score > 0 | course_id %in% have_sec_ids) |>
    transmute(course_id, dedup_key = plan_content_id, score)
  selected <- audit_sample(pool, SUSPECT_N, RANDOM_N, SEED)
  if (nrow(selected) == 0) next

  build <- function(plan_trunc) {
    lines <- audit_packet_header(CHECK, inst, selected, paste(
      "For each course, audit the **Extractor output** against the **Full course",
      "plan** using the definitions in `section_codebook.yml`. Rows already",
      "flagged by the deterministic pre-pass (R/audit/qa_sections.R) are marked `⚑ flags: …`."
    ))

    for (i in seq_len(nrow(selected))) {
      cid  <- selected$course_id[i]
      kind <- selected$kind[i]
      meta <- inst_plans[match(cid, inst_plans$course_id), ]
      plan_txt <- meta$course_plan
      rows <- sec |> filter(course_id == cid) |>
        left_join(susp_lbl, by = c("course_id", "section"))

      lines <- c(lines,
        sprintf("---\n\n## COURSE %d — `%s`  [%s]", i, cid, kind),
        "",
        sprintf("- %s (%s) · %s %s", meta$Emnekode_raw, meta$Emnenavn,
                meta$Semesternavn, meta$Årstall),
        "",
        "### Full course plan (anonymized — ground truth)",
        "",
        audit_fence(audit_trunc(plan_txt, plan_trunc)),
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
            audit_fence(audit_trunc(rows$raw_text[j], if (rows$section[j] ==
                                    "reading_list") RL_TRUNC else SECT_TRUNC)),
            "")
        }
      }
    }
    lines
  }

  # Fit MAX_KB: shrink the plan text first, then drop random controls (keep
  # at least 2); suspects always stay.
  plan_trunc <- PLAN_TRUNC
  repeat {
    lines <- build(plan_trunc)
    if (sum(nchar(lines, "bytes") + 1) <= MAX_KB * 1024) break
    if (plan_trunc > 3000) {
      plan_trunc <- max(3000, round(plan_trunc * 0.8))
    } else if (sum(selected$kind == "RANDOM") > 2) {
      selected <- selected[-max(which(selected$kind == "RANDOM")), ]
    } else break
  }

  path <- file.path(out_dir, paste0(inst, ".md"))
  writeLines(lines, path)
  manifest[[inst]] <- tibble(
    institution = inst,
    n_suspect = sum(selected$kind == "SUSPECT"),
    n_random  = sum(selected$kind == "RANDOM"),
    packet    = path
  )
  sample[[inst]] <- mutate(selected, institution = inst, .before = 1)
  cat(sprintf("  %-8s %2d suspects + %2d random, plan cut %d, %3.0f KB -> %s\n", inst,
              manifest[[inst]]$n_suspect, manifest[[inst]]$n_random, plan_trunc,
              sum(nchar(lines, "bytes") + 1) / 1024, path))
}

audit_write_index(CHECK, bind_rows(manifest), bind_rows(sample))
