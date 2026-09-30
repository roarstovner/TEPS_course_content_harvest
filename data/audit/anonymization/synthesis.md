# Anonymization audit — synthesis (pilot: uio, mf, nih)

Pilot run 2026-09-30, Opus review agents, 8–22 courses per institution; the
agents also scanned all offerings of their institution. 7 findings, all pass
mechanical verification. Report: `findings_report.md`.

## Summary

- **No personal data left in `course_plan`** at any of the three: no names,
  e-mails or phone numbers (uio 96, mf 51, nih 81 offerings scanned), and no
  course content over-removed.
- **Inflected semester labels survive**: "høsten 2025", "våren 2026",
  "høsten2025". Verified: `anonymize_fulltext("nih", "Pensumliste for høsten
  2025. Høst 2025 WISEflow våren 2026")` keeps "høsten 2025" and "våren 2026".
  Affects nih (31 of 81 plans) and uio (PROF1015).
- Institution boilerplate left in (counts verified on `course_offerings_full.RDS`): UiO's exam-links block (92 of 96 plans),
  MF's exam-date table (27 of 51) and library notice (41 of 51).
- **Outside this check but found through it:** `sections_raw` is *not*
  anonymized — see "Cross-check with other audits".

## Cross-institution patterns

- **Season regex in `.anon_generic()`** only matches the bare word before a
  year (`(høst|vår|…)\s+\d{4}`), so inflected forms and glued years pass
  (nih, uio). One generic fix: `(høst|vår|haust)(?:en|a)?\s*\d{4}`. The same
  gap exists in `normalize_plan_text()`, where it can split otherwise
  identical plans across plan ids.
- **Hyphenated academic years** ("Studiehåndbok 2023-2024") are not matched
  by the `\d{4}/\d{2,4}` rule (nih: 14 of 81 plans).

## Fix list

1. `.anon_generic()`: inflected/glued season + year (nih, uio). Add test cases
   in `tests/testthat/test-anonymize.R`.
2. `.anon_uio()`: drop the exam-links tail — or better, fix it once at
   extraction (fulltext fix 1), which removes it here too.
3. `.anon_mf()`: remove the "Eksamensdatoer" block up to "Læringsutbytte"
   (e.g. `mf_SAM1080L-1_2025_spring_1`) and the "Tilgang til litteratur"
   notice.
4. `.anon_generic()`: remove a parenthesised e-mail as one unit so "Ta kontakt
   med () eller … ()" does not remain (uio, 4 of 96).
5. `.anon_generic()`: hyphenated academic years.

## Cross-check with other audits

- **`sections_raw` contains personal data.** Verified: 69 section rows carry
  e-mail addresses (mf 51, uio 18) while no `course_plan` does.
  `R/run_extract_sections.R` passes raw `html` and un-anonymized
  `extracted_text` to `extract_sections()`. Section text must be anonymized
  (e.g. run `anonymize_fulltext()` over `raw_text`) before it is published or
  sent to an external model.
