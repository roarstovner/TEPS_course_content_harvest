# Fulltext audit, pilots of 2026-11-21: DMMH, AHO, LDH, Oslo Nye Høyskole, Barratt Due

Run 2026-11-21 on the build of 2026-11-21 (10:21-20:21, commit d77a254),
`--pilot` samples. Review agents: Codex `gpt-6.1-sol`, one per institution,
28 courses each. Findings: `findings-codex/{dmmh,aho,ldh,onh,barrattdue}.json`,
report `findings-codex_report.md`. All 3 findings of the five passed
mechanical verification (the `?` row in the report is a stale mf report).
Oslo Nye Høyskole's packet reads the pilot run itself (`pilot_run_rows()`):
its pages are of 2026/27 and the build gives them to no offering yet.

Each finding was checked against its packet, the raw store and the code:
**3 real** (1 high, 2 low), **0 rejected**. The high one needs a harvest.

## Summary

- **DMMH, AHO, Oslo Nye Høyskole: no findings.** Plans complete, facts and
  sections readable, no page chrome; DMMH's 19 year flags all correct.
- **LDH, high: the BIS23 study plan's last course takes in the programme
  appendix.** `ldh_BSYP-503-1_2025_autumn_1` has 14,664 characters: its
  plan, then "Studiets overordnede læringsutbytter" and the national
  competence areas. Re-splitting the 8 PDFs the pilot stored gives the same
  result only for BIS23 (its last course; BIS20-22 end at the assessment,
  ~4,500 characters).
- **LDH, low: PDF page chrome.** Standalone page numbers (MALSY-100,
  VIB-101, MOPSY-100, MOPNA-100) and "<programme> H2025. Sist oppdatert
  dd.mm.yy" footers (6 of 25 PDF course texts) stay in the text.
- **Barratt Due, low: a drop cap split from its word** ("S" / "tudenter som
  har fullført emnet ...", `barrattdue_VERK2100-1_2026_autumn_1`), one of 28.

## Cross-institution patterns

PDF page chrome is the one pattern shared with earlier runs (the report's
"page_chrome / junk_included" row: hiof, ldh, mf, steiner, uis, usn): each
PDF reader removes its own footers, and LDH's misses the forms above.

## Prioritised fix list

A. **LDH PDF split, before LDH's full harvest (high + low; needs a
   harvest).** `ldh_pdf_courses()` (`R/harvest_strategies.R`) ends a course
   at the next course heading, an upper-case plan heading, "Referanser" or
   "Vedlegg": add BIS23's mixed-case programme headings ("Studiets
   overordnede læringsutbytter"), and drop standalone page-number lines and
   the "... Sist oppdatert dd.mm.yy" footer lines, which the current footer
   rule (text, spaces, number on one line) does not match. The harvest
   stores the split text and the build reads it as stored
   (`read_harvest()` drops the PDF bytes), so this changes only harvests
   made after it; LDH's full harvest writes a new run for the pilot's
   offerings too. Examples `ldh_BSYP-503-1_2025_autumn_1`,
   `ldh_MALSY-100-1_2025_autumn_1`. Effect: BSYP-503 (BIS23) back to its
   own plan; footers out of `course_plan` and the sections.

B. **Barratt Due drop cap (low, rebuild-only).** Join a single capital
   letter block to the paragraph after it, if the page marks it as a drop
   cap; check the raw HTML of VERK2100 first.

## Changes since the previous run

The five institutions are new; no earlier rows.

## Rejected findings

None. The reviewers' own rejections, kept: LDH's two year flags point at
earlier HTML plans the build already leaves out; Barratt Due's long flags
(DID1, DID2) are detailed sub-course descriptions; Oslo Nye Høyskole's
INT1140 year flag is a false alarm.

## Verdict

Gate numbers of this check (build of 2026-11-21; `PILOT_GATE`: fetched and
plan 0.80):

| Institution | fetched | plan | Verdict |
|---|---|---|---|
| dmmh | **0.76** (n 100) | **0.69** (100) | **ready with known limits** |
| aho | **0.69** (100) | **0.68** (100) | **ready with known limits** |
| ldh | **0.25** (100) | **0.25** (100) | **not ready**: batch A first (needs the harvest); the numbers are a known limit |
| onh | 0.82 (100) | 0 (100) | **ready with known limits** |
| barrattdue | **0.648** (71) | **0.648** (71) | **ready with known limits** |

- **DMMH**: plan by year 2015 3/25, 2019 18/25, 2022 24/25, 2025 24/25
  (0.88 without 2015). Plans before autumn 2017 are sent by e-mail only
  (DMMH's note; the catalogue redirects to its search). After 2015, 7
  course-years 404 at the semester URL: bachelor theses (BHBAC3910, 3930,
  3970, 3980, BMBAC3900) and some master or continuing-education courses
  (BNFOR3560, MVSPE6100, VUVBH6000); the catalogue search finds no page for
  BHBAC3910 or MVSPE6100 (checked 2026-11-21): not published.
  BHFOR3520 2019 is fetched with no plan (a later version).
- **AHO**: 31 course-years have no page: 12 spring-2025 numeric codes, PhD
  courses, the *4106/*5106 electives, MARV/MURB/SOD studio modules,
  SELVPROG, VALG6. The resolver reads both trees of the whole sitemap, and
  the year folder listing /studier/emner/2025/var/ (fetched 2026-11-21) has
  none of the missing spring codes: DBH lists more than the site publishes.
- **LDH**: plan by year 2019 0/34, 2022 9/33, 2025 16/33. The site has the
  current cohorts only (H2025/H2026 master plans, BIS20-26); a course in
  semester 2+ of a plan has no earlier cohort's plan, and 13 codes are in no
  plan on the site (continuing-education and older codes); 2019 lists only
  codes of the older plans. Checked against `ldh_plan_index()` 2026-11-21.
- **Oslo Nye Høyskole**: `plan` 0 is by design until a DBH snapshot has its
  autumn 2026 offerings (the pilot pages are of 2026/27; the snapshot of
  2026-11-08 has spring 2026 only). The 28 pilot pages read as complete
  plans. The 18 without a page are codes not in the sitemap (medicine-prep
  MB*, INT practice, electives).
- **Barratt Due**: the 25 course-years without a page are the 25 codes not
  in the studies sitemap (UTPR* projects, UNGFIL, EXHINS, VU-*, EKS*,
  practice); the probe of 2026-11-20 found no pages for them.

DMMH, AHO, Oslo Nye Høyskole and Barratt Due are ready on fulltext: what
their numbers miss is not on the site, and no finding needs a harvest. LDH
needs batch A before its full harvest; its numbers are otherwise the site's.
