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
- The issue database is shared by all worktrees. Run chainlink from the
  worktree that owns the work: closing an issue writes `CHANGELOG.md` in the
  current directory.

## Rules

- Run the tests after code changes:
  `Rscript -e 'testthat::test_dir("tests/testthat")'`.
- Source the R files in the order of README "Quick Start" (or of the `run_*.R`
  scripts): `R/institution_config.R` refers to functions in
  `R/fetch_html_cols.R` and `R/extract_fulltext.R`.
- Privacy: `html` and `extracted_text` are raw and hold staff names, e-mails and
  phone numbers. Anything published is built from `anonymize_text()`; sections
  are cut from raw text, so `R/run_extract_sections.R` must keep anonymizing.
  See README "Data Files: Published and Internal".
- After changing extraction, the heading map or the anonymizer, rebuild the
  derived data (README "Rebuilding Derived Data") and compare per-institution
  counts with the previous build. A whole institution can drop to zero (all
  142 nla courses lost their sections in 06db3ac, fixed in 0d18592).
- UiO: never switch to semester URLs (`/h24/`, `/v25/`). They hold logistics,
  not the course plan (README "Institution-Specific Notes").
