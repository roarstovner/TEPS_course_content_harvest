# Anonymization audit, FHS pilot (2026-11-04)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/anonymization/packets/fhs.md`
(28 courses: 16 suspects, 12 random), findings
`data/audit/anonymization/findings-codex/fhs.json`, report
`data/audit/anonymization/findings-codex_report.md`.

## Summary

- No personal data left: `phone` flags are numbers in reference URLs,
  `name_like` flags organisations, places and literature authors.
- `privacy_hits` 0 of 46.

## Fix list (no new harvest needed)

1. medium, widespread: the year under the facts-box label "Studieår" stays
   (`Studieår\n2021\nUtdanninger`; fhs_LED4101-1_2022_autumn_1, every plan).
   Where: `R/anonymize.R`, a rule for "Studieår" followed by a year on the
   next line (FHS handler or generic).
2. low: semester removal leaves "Gjeldende litteraturliste for finner du i
   Leganto" (fhs_ING3506-1_2025_autumn_1, 4 plans). Where: cleanup after
   the season/year rule in `.anon_generic()`.

## Verdict

**ready for full harvest** (no high finding, `privacy_hits` 0).
