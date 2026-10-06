# CLAUDE.md

Agent-only guidance for Claude Code in this repository. Documentation for
people lives in the files below; do not copy it here. When something needs
documenting, write it there and, if agents need to find it, add a pointer here.

## Where things are documented

- `README.qmd` (render with `quarto render README.qmd` → `README.md`): pipeline
  stages, how to run and rebuild each step, section extraction (strategies,
  `section_*` config fields, heading map, cleanup), which data files are
  published and which are internal, institution-specific notes (NORD, NTNU,
  UiO, HiOF, USN, UiT), function reference, troubleshooting.
- `data/data_notes.qmd`: per-institution data quality notes.
- `methods.qmd` (rendered by `tar_make()` to `methods.md`): the choices an
  article's methods section must report, by topic, with numbers from the data.
- `app/course_browser/README.md`, `app/course_browser_ojs/README.md`: the two
  browsers.
- `R/institution_config.R`: per-institution configuration (single source of
  truth). Function contracts are in the roxygen comments in `R/*.R`.
- Skills: `/add-institution`, `/audit-institutions`.
- `CHANGELOG.md` is written by chainlink when an issue is closed.

## Chainlink

`cl` is an alias for `chainlink`. No SessionStart hook is installed
(`.claude/hooks/` is empty), so the session steps below are manual.

- At the start: `chainlink session start`, then
  `chainlink session last-handoff` to see where the previous session stopped.
- Per issue: `chainlink session work <id>`; when done, comment the commit hash
  and before → after numbers, then close it.
- Before ending a long session (an overnight or autonomous run, a long
  conversation, or when the user signs off): run
  `chainlink session end --notes "..."` with what was done (issues, commits),
  what is in progress or blocked, decisions the user must make, unpushed
  commits and their branch, and the next step. A final chat message is not a
  handoff: it can be cut off and is not stored with the issues.
- Run chainlink from the repository root: closing an issue writes
  `CHANGELOG.md` in the current directory. Commit it.

## Rules

- Run the tests after code changes:
  `Rscript -e 'testthat::test_dir("tests/testthat")'`, and check the failure
  count, not just the exit status of a shell chain. CI runs them on push.
- Packages come from `renv.lock`. A new package: `renv::install()`, then
  `renv::snapshot()`, and commit `renv.lock` with the code that uses it.
- `tar_make()` runs steps on crew workers. To debug a step interactively:
  `tar_make(callr_function = NULL, use_crew = FALSE)`.
- Source the R files in the order of README "Quick Start" (or of `_targets.R`):
  `R/institution_config.R` refers to functions in
  `R/fetch_html_cols.R` and `R/extract_fulltext.R`.
- Privacy: `html` and `extracted_text` are raw and hold staff names, e-mails and
  phone numbers. Anything published is built from `anonymize_text()`; sections
  are cut from raw text, so `institution_sections()` must keep anonymizing.
  Only anonymized text goes in `data/processed/` (checked by target
  `privacy_check` and `test-anonymize.R`). See README "Data Files: Published
  and Internal".
- `data/raw/html_{inst}.RDS` and `data/raw/checkpoint/` are the raw harvest: only a
  harvest writes them. To change extracted text, change the config or
  `R/extract_fulltext.R` and run `targets::tar_make()`.
- After changing extraction, the heading map or the anonymizer, rebuild the
  derived data with `targets::tar_make()` and read `tar_read(metrics_check)`
  (README "Rebuilding Derived Data"). Explain every flagged
  change; update the snapshot (`check_pipeline_metrics(update = TRUE)`) only
  for intended ones, in the same commit, and say so in the issue comment.
- A decision that changes what the data mean (frame, years, exclusions,
  anonymization, section rules) gets an entry in `methods.qmd` in the same
  commit: choice, reason, rejected alternatives, consequence, date and issue.
- UiO: never switch to semester URLs (`/h24/`, `/v25/`). They hold logistics,
  not the course plan (README "Institution-Specific Notes").
