# Fulltext audit, HiMolde pilot (2026-11-06)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/fulltext/packets/himolde.md`
(28 courses: 1 suspect, 27 random), findings
`data/audit/fulltext/findings-codex/himolde.json`, report
`data/audit/fulltext/findings-codex_report.md`. Build of 2026-11-06 16:54.

## Summary

- No findings. The title, the facts list and every part of the plan are
  captured; uncaptured lines are the breadcrumb, the FS fetch stamp and the
  course coordinator (excluded on purpose).
- The one `short` flag (VPL023 spring 2019) is a page shell, which the build
  drops.
- Of the 78 fetched pilot offerings, 26 give no plan: 25 are spring
  offerings whose only page is the autumn before's, for that autumn alone
  (`page_end_term`; 12 English, 13 Norwegian tree), 1 is the page shell.
  The reviewer judged these exclusions right.

## Gate (fulltext numbers)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| fetched | 0.56 | 100 | 0.80 | TRUE |
| plan | 0.48 | 100 | 0.80 | TRUE |

Reason, on the site: the course-years with a page are 2018 10 of 25 (the site
starts in autumn 2018), 2019 16, 2022 16, 2025 14. The year listings of 2022
and 2025, read again on 2026-11-06 (`vortex_course_index()`, both trees, 768
pages), hold no page for the course-years without one: JUR216, JUR500,
PMP300, SCM410, LOG220N, IBE781, LOG951, VPH725, the PhD courses (DRL, IDR8xx)
have none in their year; SND102, SNH102, VIVO-2 and IDA730 have one only in
2022, ADM499 only in 2025. Direct URLs in the department folders answer 404.
`plan` is below `fetched` by 8 course-years whose only page is another
term's or the page shell (above; PET301, SP16-4, VAK701, VPL023 2019, ADM120N, MAT100N, VHS734 2022, TRA500 2025).

## Verdict

**ready with known limits**: `fetched` 0.56 and `plan` 0.48 < 0.80, because
DBH lists course-years the site has no page for (before autumn 2018, PhD and
some master's and continuing-education courses, codes in other years only),
and spring offerings whose only page is the previous autumn's.
