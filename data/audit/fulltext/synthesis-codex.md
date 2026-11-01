# fulltext audit (pilot, Codex reviewers): synthesis

Run 2026-11-01, `--pilot`, reviewers `codex exec -m gpt-6.1-sol`, findings
in `findings-codex/`, report `findings-codex_report.md`. Covers khio (this
run) and nhh (the review from 2026-10-31, kept as it was).

## Summary

- khio: 28 pages from the pilot run (2026-11-01, the plans the site showed
  then) plus 3 failed fetches. No findings. The orchestrator's check agrees:
  no page has uncaptured text, every page has its facts list and a
  "Vurdering" section, and on the raw pages the text of `main` is the same as
  that of `main section.content` (the selector loses nothing). None of the
  190 pages has a literature, prerequisite or admission section, so those
  sections are missing from the site, not from the extraction.
- khio fetch failures: 8 offerings, 4 course codes, all doctoral-programme
  components (DG900-1, DG930-1, DG950-1 in the sample), HTTP 404 on
  `/emner/{code}-{version}`, the same URL pattern as the 190 pages that were
  fetched. These are likely parts of the doctoral programme with no public
  course page; none of the four codes has a page in any other offering.
- nhh: 2 findings from 2026-10-31, both formatting (verified ✓). The medium one
  (NHH `<webLink>` nodes) was reviewed before commit e059487 (2026-10-31
  13:10), which changes how those nodes are extracted. It needs a recheck
  after the next build; there is no new review here.

## Cross-institution patterns

None (two institutions, with no shared root cause).

## Prioritised fix list

None for khio. For nhh, see the 2026-10-31 report. After the next build,
check whether e059487 removed the webLink finding (nhh_BUS476-1_2025_autumn_2,
nhh_TECH3-1_2025_autumn_1).

## Rejected findings

None.

## Verdict (--pilot)

The gate numbers (`pilot_gate()`: `fetched`, `plan`) are pending a build: the
last `tar_make()` (2026-10-31 00:06) is older than the khio and nhh pilot
runs, and judging their year coverage needs a DBH snapshot that lists
2026/27.

- **khio: ready for full harvest (fulltext), pending the gate numbers.** No
  finding. The 4 doctoral course codes with a 404 are a site limit (expected
  as "404" gaps, to be noted in `data/data_notes.qmd`), not a URL-builder
  defect.
- **nhh**: verdict as in the 2026-10-31 review, likewise pending the build and
  gate numbers.
