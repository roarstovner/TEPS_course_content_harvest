# Fulltext audit — synthesis (pilot: uio, nih, steiner)

Pilot run 2026-09-30, Sonnet review agents, 8–10 courses per institution.
8 findings, all pass mechanical verification. Report: `findings_report.md`.

## Summary

- Extraction is faithful for all three: no wrong-page or wrong-year content
  at uio and nih; the full plan is captured with readable structure.
- **UiO appends a generic exam-links block** ("Mer om eksamen ved UiO …
  Hvordan bruke KI som student … Andre veiledninger og ressurser") to every
  text. The same block shows up in the sections and anonymization audits
  (below), so fixing it at extraction fixes all three.
- **NIH: 103 of 184 offerings return 404.** Cause unknown — see below.
- **Steiner PDFs** carry page numbers and hard line wraps (cosmetic).
- The pre-pass flagged almost nothing (2 suspects in 3 institutions). The
  random controls did the work; flag thresholds may be too strict to be
  useful for clean institutions.

## Cross-institution patterns

- **Facts box not captured** (uio, nih — low): credits, level and language
  sit outside the configured selector. DBH metadata has the same fields, so
  this only matters if page-level facts are wanted.
- **Reading lists live elsewhere** (nih; also mf and uio in the sections
  audit): the page only links to an archive/Leganto list, so no literature is
  in `extracted_text`. Not an extraction bug; a data limitation to document.

## Fix list

1. **uio: cut the exam-links tail** — `post_fn` in `R/institution_config.R`
   that removes from "Mer om eksamen ved UiO" to the end. Example:
   `uio_SVLEP1000-1_2025_spring_1`. Removes the block from `extracted_text`,
   `course_plan` and the assessment section at once.
2. **nih: investigate the 404s.** Verified distribution: spread over all
   years (2021: 3, 2022: 10, 2023: 25, 2024: 41, 2025: 24) and 101 of 103 are
   Norwegian-language courses, so the agent's hypothesis (English courses need
   the English path) does not hold. Check `add_course_url_nih()` against a few
   failing codes, e.g. `nih_LKI152-1_2024_spring_2`.
3. **steiner: strip bare page-number lines** (`^\s*\d{1,3}\s*$`) and rejoin
   wrapped lines in a `post_fn` (low).

## Rejected / needs a decision

- **steiner "wrong page" (agent: high).** `M-PEL2.1`/`M-PEL2.2` get the PDF
  sections for PEL1.4/PEL1.5. That is deliberate: `.steiner_remap_pel_emne()`
  in `R/harvest_strategies.R` maps them because the PDF numbers the emner
  differently from DBH. Not a bug unless the mapping itself is wrong — worth a
  human check against the programme description.
