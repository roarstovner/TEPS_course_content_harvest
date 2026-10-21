# Fulltext audit — synthesis (pilot: 6 institutions, after the URL fixes)

Run 2026-10-21 with `--pilot` on the build of commit a8ba3a5 (after the URL
fixes of 2026-10-21 and the pilot retry harvests): hiof, nih, uib, uia,
usn, uit. Each packet holds only offerings from the institution's pilot
sample (`data/pilot/{inst}.csv`): 28 pages plus up to 4 failed fetches. One
Sonnet review agent per institution (medium effort); packets 67–155 KB;
every agent read all its courses. 24 findings for these 6 after
verification (0 ✗, one rejected). The other institutions' findings in
`findings_report.md` are from earlier runs; the synthesis of the 2026-10-17
pilot run (ntnu, oslomet, inn, nord, hvl, hivolda, nla) is in git history
(a8ba3a5).

## Summary

- Where a page holds the plan, extraction is faithful at all six: whole
  plan, right course, readable structure. The uncaptured page text is
  breadcrumbs, year pickers, contact boxes and FS timestamps.
- **HiOF's continuing-education folder (`videre/`) shows a later plan
  under an earlier year folder**: `videre/2019/var/lskno22215.html` holds
  "(Vår 2020)", `videre/2022/host/lskas10116.html` holds "(Høst 2023)". The
  folder year is not the plan's year there. The only high finding left.
- The low `fetched` shares have reasons on the sites, found by the URL-fix
  helpers of 2026-10-21 (issues of each institution): course-years DBH
  lists that the site has no page for.
- Reading lists are often not on the page (UiT's Leganto lists, NIH's
  "Pensumlista for …" links): a property of the sites, not of the
  extraction.
- Encoding damage `¿` in the source at UiT and USN (2019 pages), as at
  NLA, OsloMet and HVL before.

## Cross-institution patterns

- **FS footer and table-of-contents chrome** in the text: "Sist hentet fra
  FS … <timestamp>" (hiof, uia), "Innholdsfortegnelse"/"Table of contents"
  plus the heading list (uia, usn). Low; anonymization already removes the
  timestamps, and the sections drop the list.
- **Reading list not on the page** (uit new layout, nih, usn placeholders):
  not fixable in the extraction.

## Prioritised fix list

1. **hiof: drop a plan whose year is later than the offering's** (high,
   occasional; `hiof_LSKNO22215-1_2019_spring_1`,
   `hiof_LSKAS10116-1_2022_autumn_1`). Where: the hiof entry in
   `R/institution_config.R`, a `page_term` reading the title's
   "(Vår|Høst YYYY)", so the existing later-version rule applies (as for
   Nord, INN, UiS). Rebuild only. Count how many HiOF offerings it
   concerns, by folder.
2. **usn: check `usn_BABLU-6_…`** (medium, uncertain): DBH's BABLU-6 is
   fetched as USN's `BABLU_3` and the plan shows code "BABLU". Probably
   right (USN's own version numbers differ from DBH's), but confirm.
3. **Low noise** (hiof, uia, usn, uit): the FS footer, the doubled language
   line and repeated credit-reduction rows (uia), the glued Leganto labels
   (usn), the 2008 timetable fragment (uit; its names are the anonymization
   check's finding). Low; only where a section is affected.

## Changes since the previous run

The six were not in the 2026-10-17 pilot fulltext run (they were flagged
for URLs first). Against the full run of 2026-10-13 (teacher education),
no persisting finding.

## Rejected findings

- **uib, "page fetched but no course description" (was high)**: UiB's
  "Inga emnebeskriving tilgjengeleg" pages give only the facts box and
  headings, but the pipeline already treats them as no plan: none of the
  three examples (`uib_TPBAPAT-0_2010_autumn_1`,
  `uib_ODO-KP-SEM7D-0_2019_autumn_1`, `uib_NOR-U3NN-0_2019_autumn_2`) is in
  `course_plans`, and they are why UiB's `plan` (.46) is below `fetched`
  (.52). Removed from `findings/uib.json`.
- **nih, "MAS- ESSAY gives an illegal URL"**: an essay registration with no
  page in any year (probed 2026-10-21, issue of the NIH URL fix); kept as
  low, no action.

## Verdict (`--pilot`, fulltext: `fetched`, `plan`)

| inst | fetched | plan | verdict |
|---|---|---|---|
| hiof | .50 ⚑ (n 100) | .49 ⚑ | not ready |
| nih | .22 ⚑ | .22 ⚑ | ready with known limits |
| uib | .52 ⚑ | .46 ⚑ | ready with known limits |
| uia | .77 ⚑ | .76 ⚑ | ready with known limits |
| usn | .58 ⚑ | .57 ⚑ | ready with known limits |
| uit | .67 ⚑ | .66 ⚑ | ready with known limits |

- **hiof — not ready.** The wrong-year plans of `videre/` (fix 1, rebuild
  only). `fetched` .50 < .80: the site has no page for most of the 2007
  codes (18 of 25), for PhD courses (no PhD folder) and for Akademi for
  scenekunst after 2021; checked against the folder listings (data notes).
- **nih — ready with known limits.** `fetched`/`plan` .22 < .80: NIH has no
  page for the 2021–2022 course-years outside teacher education (the year
  indexes list 5 and 22 pages) and most 2025 misses are the other semester
  of a course with a page.
- **uib — ready with known limits.** `fetched` .52 < .80: UiB keeps no page
  for courses it no longer offers (25 of 48), PhD courses (14), theses and
  milestones (4), UNIS courses (2) and design blocks with a space in the
  code (3). `plan` .46: six pages say there is no description. The pilot's
  404s of 2026-10-17 were transient for 29 course-years: follow the full
  harvest with a build and `harvest_all(institutions = "uib", retry =
  "not_found")` (README).
- **uia — ready with known limits.** `fetched` .77 < .80: 2020 is the
  site's first year, and six '-G' courses have no page with or without the
  suffix.
- **usn — ready with known limits.** `fetched` .58 < .80: 21 course-years
  show another semester's plan under the "not ready" banner (left out by
  the rule that an offering gets its own year's plan or none, confirmed by
  the user 2026-10-21), 10 show a cohort plan from before USN, 11 are
  empty pages. Fix 2 is a check, not a blocker.
- **uit — ready with known limits.** `fetched` .67 < .80: 21 course-years
  have a course page but no document for DBH's year (documents end
  earlier, start later or skip the year) and 12 codes have no page; no
  plan is carried forward (user 2026-10-21).
