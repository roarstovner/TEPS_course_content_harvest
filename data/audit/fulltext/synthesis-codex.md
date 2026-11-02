# fulltext audit (pilot, Codex reviewers): synthesis

Run 2026-11-02, `--pilot`, reviewers `codex exec -m gpt-6.1-sol`, findings in
`findings-codex/`, report `findings-codex_report.md`. Covers nhh (recheck
after e059487) and vid (this run); khio is the review of 2026-11-01, kept as
it was (0 findings, ready pending gate numbers).

## Summary

- nhh: the 2 formatting findings of 2026-10-31 (webLink URLs doubled or
  glued, spaces lost around emphasis) are gone: e059487 fixed them. 28/28
  courses read, no content loss.
- nhh: 1 new "high" finding, rejected (below): a packet label, not an
  extraction defect.
- vid: 1 low finding: an isolated page number "34" inside an assessment
  sentence (vid_BAVER2030-1_2024_autumn_1). 19 empty suspects are pages without a
  plan; 3 failed fetches are soft-404s of courses with no published plan.

## Cross-institution patterns

None.

## Prioritised fix list

- vid (low): strip lone page numbers between sentences in the PDF/JSON plan
  text (`post_fn` or the html_fn of vid). Cosmetic.

## Changes since the previous run

nhh: gone `whole_text/formatting`, `teaching_methods/formatting` (fixed by
e059487). New: `whole_text/empty_or_failed` (rejected).

## Rejected findings

- nhh `empty_or_failed` (high): the packet said "Plan in the build: none: a
  page shell" for 25 pages with full plans. The build has NHH's 2025 offerings
  but gives them no page, because the pilot run's pages are 2026/27 and the
  DBH snapshot has no 2026 (docs/harvesting.md "Sites that show only the
  current plan"); `no_plan_reason()` fell through to its default "page shell".
  Packet label fixed in the working tree (`R/audit/prepare_fulltext.R`, not
  committed).

## Verdict (--pilot, fulltext)

Gate numbers (`pilot_gate()`, build 2026-11-02):

| inst | metric | value | n | threshold | review |
|---|---|---|---|---|---|
| khio | fetched | 0.96 | 100 | 0.80 | FALSE |
| khio | plan | 0.00 | 100 | 0.80 | TRUE |
| nhh | fetched | 0.58 | 100 | 0.80 | TRUE |
| nhh | plan | 0.00 | 100 | 0.80 | TRUE |
| vid | fetched | 0.70 | 100 | 0.80 | TRUE |
| vid | plan | 0.44 | 100 | 0.80 | TRUE |

- **khio: ready with known limits.** `plan` 0.00: current-plan site, 2026/27
  pages join no DBH 2025 offering until a snapshot with 2026 (a timing
  limit, not a defect). Fetch failures: 4 doctoral codes with 404 (site).
- **nhh: ready with known limits.** `plan` 0.00 as khio. `fetched` 0.58: 81
  of 195 offerings have no URL (site search finds no page: NHH Executive
  courses and translator exams), user decision pending since 2026-10-31.
- **vid: ready with known limits**, provided the running VID retry harvest
  does not show fetch errors that a retry should fix. `fetched` 0.70 and
  `plan` 0.44: the 3 sampled failed fetches are soft-404s of courses
  without a published plan and the 19 sampled empty pages have no plan text
  (no plan text lost); the other failures were not reviewed one by one. The numbers are from the build before the retry harvest.
