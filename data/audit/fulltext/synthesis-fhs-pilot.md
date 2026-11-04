# Fulltext audit, FHS pilot (2026-11-04)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/fulltext/packets/fhs.md`
(28 courses: 20 suspects, 8 random), findings
`data/audit/fulltext/findings-codex/fhs.json`, report
`data/audit/fulltext/findings-codex_report.md`. Build of 2026-11-04 17:24.

## Summary

- No findings. The facts box, the untitled introduction ("Hva lærer du") and
  all accordions are captured; uncaptured lines are only the "Oppdatert"
  stamp and the versions menu.
- The 19 `year` flags are false alarms: spring offerings read against an
  autumn-start academic year (11), and earlier plan versions still in force
  (8, `resolve_urls_fhs_batch()`).
- The one `short` flag (CBU1506) is a short page.

## Gate (fulltext numbers)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| fetched | 0.46 | 100 | 0.80 | TRUE |
| plan | 0.46 | 100 | 0.80 | TRUE |

Reason, on the site: 2018 0 of 47 and 2019 3 of 47 course-years have a page
(codes from before the reorganisation of FHS's education, e.g. M5-023,
SEF1001MAD, VBU1-LED; the site starts in autumn 2018 and has no page for
them), against 36 of 39 in 2022 and 40 of 45 in 2025, whose missing codes are
linked from no programme page and answer 404 (checked 2026-11-04, see
`data/data_notes.qmd`). Every fetched page gives a plan.

## Verdict

**ready with known limits**: `fetched` and `plan` 0.46 < 0.80, because
DBH's pre-2019 course codes have no page on the site.
