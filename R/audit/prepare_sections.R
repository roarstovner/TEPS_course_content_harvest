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
#   data/course_offerings_full.RDS   Rscript R/run_dedup.R
#   data/sections_raw.RDS            Rscript R/run_extract_sections.R
#   data/sections_qa_suspects.RDS    Rscript R/audit/qa_sections.R
#
# Outputs:
#   data/audit/sections/packets/{inst}.md
#   data/audit/sections/manifest.csv, sample.csv
#
# Run:  Rscript R/audit/prepare_sections.R [inst ...]

source("R/audit/utils.R")

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

CHECK   <- "sections"
out_dir <- file.path(audit_dir(CHECK), "packets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load ─────────────────────────────────────────────────────────────────────
audit_require_fresh("data/sections_raw.RDS", "data/course_offerings_full.RDS",
                    "Run Rscript R/run_extract_sections.R.")
audit_require_fresh("data/sections_qa_suspects.RDS", "data/sections_raw.RDS",
                    "Run Rscript R/audit/qa_sections.R.")

sec      <- readRDS("data/sections_raw.RDS")
suspects <- readRDS("data/sections_qa_suspects.RDS")
plans    <- readRDS("data/course_offerings_full.RDS") |>
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

  # Courses with extracted sections are eligible as random controls; suspects
  # are eligible regardless.
  have_sec_ids <- sec |> filter(institution == inst) |> pull(course_id)
  pool <- inst_plans |>
    left_join(course_score, by = "course_id") |>
    mutate(score = coalesce(score, 0)) |>
    filter(score > 0 | course_id %in% have_sec_ids) |>
    transmute(course_id, dedup_key = plan_content_id, score)
  selected <- audit_sample(pool, SUSPECT_N, RANDOM_N, SEED)
  if (nrow(selected) == 0) next

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
      audit_fence(audit_trunc(plan_txt, PLAN_TRUNC)),
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
          audit_fence(audit_trunc(rows$raw_text[j], SECT_TRUNC)),
          "")
      }
    }
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
  cat(sprintf("  %-8s %2d suspects + %2d random -> %s\n", inst,
              manifest[[inst]]$n_suspect, manifest[[inst]]$n_random, path))
}

audit_write_index(CHECK, bind_rows(manifest), bind_rows(sample))
