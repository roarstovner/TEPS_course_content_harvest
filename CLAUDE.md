# CLAUDE.md

Agent-only guidance for Claude Code in this repository. Documentation for
people lives in the files below; do not copy it here. When something needs
documenting, write it there and, if agents need to find it, add a pointer here.
This file is public: guidance for one machine or one person's workflow goes in
the gitignored `CLAUDE.local.md`.

## Where things are documented

- `README.qmd` (render with `quarto render README.qmd` → `README.md`): pipeline
  stages, how to run and rebuild each step, section extraction (strategies,
  `section_*` config fields, heading map, cleanup), which data files are
  published and which are internal, the append-only raw store and how to
  finalize a release, sites that show only the current plan,
  institution-specific notes (NORD, NTNU, UiO, HiOF, USN, UiT), function
  reference, troubleshooting.
- `data/data_notes.qmd`: per-institution data quality notes; course-years
  without a plan and why (target `gaps`).
- `methods.qmd` (rendered by `tar_make()` to `methods.md`): the choices an
  article's methods section must report, by topic, with numbers from the data.
- `course_plan_spec.md`: what `course_plan` and the sections hold and what
  stays out (personal data, boilerplate, plan text, not a plan), with its
  known deviations. README "What Each Mechanism May Remove" maps it to code.
- `app/course_browser/README.md`, `app/course_browser_ojs/README.md`: the two
  browsers.
- `R/institution_config.R`: per-institution configuration (single source of
  truth). Function contracts are in the roxygen comments in `R/*.R`.
- Skills: `/add-institution`, `/audit-institutions`.

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
- `data/raw/` is the raw harvest and append-only: only `harvest_all()`
  writes it, and never the same file twice (README "Raw Store and Finalized
  Releases"). Do not overwrite, merge into or `chmod` raw files by hand. To
  change extracted text, change the config or `R/extract_fulltext.R` and run
  `targets::tar_make()`. After a release is finalized, `tar_read(frozen_check)`
  must stay empty.
- After changing extraction, the heading map or the anonymizer, rebuild the
  derived data with `targets::tar_make()` and read `tar_read(metrics_check)`
  (README "Rebuilding Derived Data"). Explain every flagged
  change; update the snapshot (`check_pipeline_metrics(update = TRUE)`) only
  for intended ones, in the same commit, and say so in the commit message.
- A decision that changes what the data mean (frame, years, exclusions,
  anonymization, section rules) gets an entry in `methods.qmd` in the same
  commit: choice, reason, rejected alternatives, consequence and date.
- Every rule that keeps or removes text must fit `course_plan_spec.md`.
  `exclude`, `post_fn` and `anonymize_text()` also cut `course_plan`, so they
  may remove only personal data and boilerplate; plan text that should stay
  out of the sections goes on the section side (`.drop`, `.clean_sections()`).
  A change to the spec gets a `methods.qmd` entry; a fix of one of its known
  deviations removes it from the list.
- UiO: never switch to semester URLs (`/h24/`, `/v25/`). They hold logistics,
  not the course plan (README "Institution-Specific Notes").
