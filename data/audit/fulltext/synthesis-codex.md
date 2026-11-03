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

## phs (pilot, 2026-11-03)

Run 2026-11-03 on the build of 2026-11-03 03:23, `--pilot`, reviewer
`codex exec -m gpt-6.1-sol`; 28 courses (0 suspects, 28 random controls),
all read. **0 findings.** The 16 plans kept are clean (breadcrumbs and the FS
update stamp left out, as configured); the 12 pages without a plan in the
build match their "Plan in the build" reason (earlier version or page
shell).

Gate numbers (`pilot_gate("phs")`, 100 course-years):

| metric | value | n | threshold | review |
|---|---|---|---|---|
| fetched | 0.28 | 100 | 0.80 | TRUE |
| plan | 0.12 | 100 | 0.80 | TRUE |

- `fetched` .28: 72 course-years have no URL. PHS's year-folder listings
  hold every page (unlisted offerings answer 404, checked 2026-11-02): DBH
  lists course-years the site has no page for (2022: 2 of 50; most EVU and
  master offerings have only programme plans, which the user decided
  2026-11-02 not to use).
- `plan` .12: of the 28 course-years with a page, 16 get no plan, by rule:
  9 are spring 2025 offerings whose only page is the autumn 2024 one,
  "(Høst 2024)" without "–Vår 2025" (AVHØRUTD, AVMISBA, ETTERFORDY, FLEFOL,
  INND-PENG, RUSM-EKST, RUSM-POL, SAMETIKK01, SAMVI-ØVE; `page_end_term`,
  an earlier version); 7 are the English NCFI861-* pages, which hold the
  title and facts but no plan (5 titled "null"; 3J and 3L have a real
  title). The pilot stored them before `fetch_html_cols_single_phs()` (soft
  404) existed: it treats all 7 as not found and all 29 Norwegian pages as
  found (checked on the stored HTML), and the shell rule or `page_end_term`
  already keeps them out of the data. Two-semester pages "(Høst 2024–Vår
  2025)" do give the spring offering its plan (FOREBYGG02, IPO871, NBKRISE,
  OPERATIV02, PATRULJE01, STRAFF01).

**Verdict phs (fulltext): ready with known limits** — `fetched` .28 and
`plan` .12 below .80, both on the site (no page for most DBH course-years;
earlier-version and plan-less English pages), no fix removes them.
