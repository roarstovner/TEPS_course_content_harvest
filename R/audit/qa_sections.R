# R/audit/qa_sections.R
# Deterministic QA pre-pass over data/processed/sections_raw.RDS (issue #197, feeds #192).
#
# Surfaces *suspect* section rows with cheap, rule-based heuristics so that
# (a) obvious extraction bugs become chainlink issues directly and
# (b) the LLM review agents can be seeded with concrete anomalies instead of
#     rediscovering them.
#
# It does NOT decide correctness — it triages. Every flag is a candidate for
# human/LLM review.
#
# Inputs:
#   data/processed/sections_raw.RDS          (course_id, institution, section, raw_text)
#   data/interim/course_offerings_full.RDS (course_id, institution, course_plan, ...)
#                                  — used as full-text ground truth for coverage
#                                    and "one section swallowed the whole plan".
#
# Outputs:
#   data/audit/sections/sections_qa_suspects.RDS  per-row flags (for seeding review agents)
#   data/audit/sections/sections_qa_report.md     human-readable summary (for a chainlink issue)
#   + a summary printed to stdout
#
# Run:  Rscript R/audit/qa_sections.R

suppressMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
})

source("R/section_heading_map.R")  # section_heading_patterns

# ── Tunables ─────────────────────────────────────────────────────────────────
EMPTY_MAX_CHARS   <- 15     # squished text shorter than this -> empty/placeholder
BLOB_FRAC         <- 0.85   # one section >= this fraction of whole plan -> blob
OUTLIER_MAD_Z     <- 3.5    # robust z (on log nchar) beyond this -> length outlier
BOILERPLATE_MIN   <- 25     # identical section text shared by >= this many courses
LEAK_MIN_PATTERN  <- 10     # only treat heading patterns this long as leak signals
PLACEHOLDER_RX    <- "^(ingen|inged|none|n/?a|-|–|\\.|ikkje|ikke)\\.?$"

# ── Load ─────────────────────────────────────────────────────────────────────
cat("Loading sections_raw.RDS ...\n")
sec <- readRDS("data/processed/sections_raw.RDS") |>
  mutate(
    txt   = str_squish(raw_text),
    nchar = nchar(txt),
    norm  = tolower(txt)
  )

cat("Loading course_offerings_full.RDS (ground-truth lengths) ...\n")
plans <- readRDS("data/interim/course_offerings_full.RDS") |>
  transmute(course_id,
            plan_nchar = nchar(str_squish(course_plan %||% "")))

# ── Flag 1: empty / placeholder ──────────────────────────────────────────────
sec <- sec |>
  mutate(
    flag_empty = nchar < EMPTY_MAX_CHARS |
      str_detect(norm, PLACEHOLDER_RX)
  )

# ── Flag 2: length outliers within institution × section ─────────────────────
# Robust z-score on log(nchar) using median/MAD. Computed on non-empty rows.
sec <- sec |>
  group_by(institution, section) |>
  mutate(
    .lognc = if_else(nchar > 0 & !flag_empty, log(nchar), NA_real_),
    .med   = median(.lognc, na.rm = TRUE),
    .mad   = mad(.lognc, na.rm = TRUE),
    .z     = if_else(.mad > 0, (.lognc - .med) / (1.4826 * .mad), 0),
    flag_short = !flag_empty & !is.na(.z) & .z < -OUTLIER_MAD_Z,
    flag_long  = !is.na(.z) & .z >  OUTLIER_MAD_Z
  ) |>
  ungroup() |>
  select(-.lognc, -.med, -.mad, -.z)

# ── Flag 3: one section swallowed (almost) the whole plan ─────────────────────
sec <- sec |>
  left_join(plans, by = "course_id") |>
  mutate(
    plan_frac = if_else(!is.na(plan_nchar) & plan_nchar > 0,
                        nchar / plan_nchar, NA_real_),
    flag_blob = !is.na(plan_frac) & plan_frac >= BLOB_FRAC & nchar > 200
  )

# ── Flag 4: cross-section heading leak ───────────────────────────────────────
# If a section's text contains a *specific* heading phrase that maps to a
# DIFFERENT canonical section AND that phrase sits at the start of a line, the
# splitter likely failed to cut at that heading (two sections merged). Matching
# at line-start (on the un-squished text) is the key precision lever: it keeps
# real uncut headings ("Arbeidskrav (AK):") and drops passing prose mentions
# ("...iht emnebeskrivelsen", "faglitteratur").
.re_escape <- function(x) str_replace_all(x, "([.^$*+?()\\[\\]{}|\\\\-])", "\\\\\\1")

# Per-section regex: any of that section's specific patterns at a line start
# (allowing leading bullets / numbering / whitespace). (?im) = case-insensitive,
# multi-line so ^ matches at every line.
leak_regex <- section_heading_patterns |>
  filter(!exact, str_length(pattern) >= LEAK_MIN_PATTERN) |>
  group_by(section) |>
  summarise(rx = paste0("(?im)^[\\s>•·*.()\\d–-]*(",
                        paste(.re_escape(pattern), collapse = "|"), ")"),
            .groups = "drop")

# For each canonical section F, flag rows of OTHER sections whose raw_text has
# an F-heading at a line start. 7 vectorised str_detect passes total.
raw_lc <- str_to_lower(sec$raw_text)
raw_lc[is.na(raw_lc)] <- ""
leak_hits <- matrix(FALSE, nrow = nrow(sec), ncol = nrow(leak_regex),
                    dimnames = list(NULL, leak_regex$section))
for (i in seq_len(nrow(leak_regex))) {
  F <- leak_regex$section[i]
  hit <- str_detect(raw_lc, leak_regex$rx[i])
  leak_hits[, i] <- hit & sec$section != F
}
sec$leak_sections <- apply(leak_hits, 1, function(r)
  paste(colnames(leak_hits)[r], collapse = ","))
sec$flag_leak <- nzchar(sec$leak_sections)

# ── Flag 5: same text under >1 section for the same course (mis-bucketing) ────
sec <- sec |>
  group_by(course_id, norm) |>
  mutate(flag_dup_in_course = !flag_empty & n_distinct(section) > 1) |>
  ungroup()

# ── Flag 6: identical section text shared across many courses (boilerplate) ───
sec <- sec |>
  group_by(institution, section, norm) |>
  mutate(dup_text_freq = n()) |>
  ungroup() |>
  mutate(flag_boilerplate = !flag_empty & dup_text_freq >= BOILERPLATE_MIN)

# ── Course-level: has plan text but zero sections extracted ───────────────────
have_sections <- sec |> distinct(course_id) |> pull(course_id)
zero_section_courses <- plans |>
  filter(plan_nchar > 50, !course_id %in% have_sections) |>
  left_join(
    readRDS("data/processed/sections_raw.RDS") |>
      distinct(course_id, institution),  # (empty for these by definition)
    by = "course_id"
  )

# ── Per-row suspect table ────────────────────────────────────────────────────
flag_cols <- c("flag_empty", "flag_short", "flag_long", "flag_blob",
               "flag_leak", "flag_dup_in_course", "flag_boilerplate")

suspects <- sec |>
  mutate(n_flags = rowSums(across(all_of(flag_cols)))) |>
  filter(n_flags > 0) |>
  select(course_id, institution, section, nchar, plan_frac,
         dup_text_freq, leak_sections, all_of(flag_cols), n_flags, raw_text)

saveRDS(suspects, "data/audit/sections/sections_qa_suspects.RDS")

# ── Summaries ────────────────────────────────────────────────────────────────
total_rows <- nrow(sec)

flag_summary <- sec |>
  summarise(across(all_of(flag_cols), sum)) |>
  pivot_longer(everything(), names_to = "flag", values_to = "n") |>
  mutate(pct = round(100 * n / total_rows, 1)) |>
  arrange(desc(n))

by_inst_sec <- sec |>
  group_by(institution, section) |>
  summarise(across(all_of(flag_cols), sum), n = n(), .groups = "drop") |>
  mutate(n_susp = rowSums(across(all_of(flag_cols)))) |>
  arrange(desc(n_susp))

cat("\n=== OVERALL (", total_rows, "section rows) ===\n", sep = "")
print(as.data.frame(flag_summary), row.names = FALSE)

cat("\n=== TOP institution × section by suspect count ===\n")
print(as.data.frame(head(by_inst_sec, 25)), row.names = FALSE)

cat("\nCourses with plan text but ZERO sections:",
    nrow(zero_section_courses), "\n")

# ── Markdown report (for a chainlink issue) ──────────────────────────────────
fmt_tbl <- function(df) {
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  body <- apply(df, 1, \(r) paste(r, collapse = " | "))
  paste(c(paste0("| ", hdr, " |"),
          paste0("| ", sep, " |"),
          paste0("| ", body, " |")), collapse = "\n")
}

leak_examples <- suspects |>
  filter(flag_leak) |>
  count(institution, section, leak_sections, sort = TRUE) |>
  head(20)

report <- c(
  "# Section extraction QA — deterministic pre-pass",
  "",
  sprintf("Generated by `R/audit/qa_sections.R` over %d section rows.", total_rows),
  "Each flag is a *triage candidate*, not a verdict — see thresholds at top of the script.",
  "",
  "## Flag definitions",
  "",
  "- **flag_empty** — squished text < 15 chars or a placeholder (`Ingen.`, `-`, ...)",
  "- **flag_short / flag_long** — robust length outlier (|z|>3.5 on log nchar) within institution×section",
  "- **flag_blob** — one section is ≥ 85% of the whole plan text (splitter failed to cut)",
  "- **flag_leak** — text contains another section's specific heading phrase (merged sections)",
  "- **flag_dup_in_course** — identical text filed under >1 section for the same course",
  "- **flag_boilerplate** — identical section text shared by ≥ 25 courses (template / wrong fallback)",
  "",
  "## Overall counts",
  "",
  fmt_tbl(as.data.frame(flag_summary)),
  "",
  sprintf("Courses with plan text but **zero** sections extracted: **%d**",
          nrow(zero_section_courses)),
  "",
  "## Worst institution × section (by total suspect flags)",
  "",
  fmt_tbl(as.data.frame(head(by_inst_sec, 25))),
  "",
  "## Most common cross-section leaks",
  "",
  fmt_tbl(as.data.frame(leak_examples)),
  "",
  "## Next step",
  "",
  "Seed per-institution LLM review agents with `data/audit/sections/sections_qa_suspects.RDS`",
  "(filter to the institution) plus the section-definition codebook, so they",
  "explain these anomalies and find additional issue types."
)
writeLines(report, "data/audit/sections/sections_qa_report.md")

cat("\nWrote data/audit/sections/sections_qa_suspects.RDS (", nrow(suspects), "rows) and",
    "data/audit/sections/sections_qa_report.md\n")
