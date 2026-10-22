# Fulltext audit — synthesis (pilot, 2026-10-22: hiof re-run; uio, mf, nmbu; the seven of 2026-10-17)

Three runs on 2026-10-22, all with `--pilot` and review agents in the Codex
CLI (gpt-6.1-sol, reasoning effort high); every agent read all its courses.
All findings pass mechanical verification (0 ✗).

- **hiof** (build of c8c0b29), after the fixes from the audit of
  2026-10-21: 28 pages of the pilot sample (152 KB).
- **uio, mf, nmbu**, the sites that show only the current plan, before
  their snapshot harvests: their first pilot audit of all courses (their
  earlier audits, 2026-10-05 to 10-13, covered teacher education). The
  pilot pages (run of 2026-10-17, 2026/27 plans) join no offering until the
  DBH snapshot has 2026, so the packets read them straight from the pilot
  run, each matched to the DBH row of 2025 it was fetched for (packet
  builder changed for this, 81f52b9). 28 pages plus 3–6 failed fetches each
  (116–137 KB).

- **ntnu, oslomet, inn, nord, hvl, nla** (build of 784e658), after the
  fetch fixes that followed their audit of 2026-10-17 (nla one page per
  code, hvl English-page wordings, nord English-only stubs; INN's
  later-version rule): 28 pages each plus up to 5 failed fetches (56–133
  KB). hivolda was fully harvested on 2026-10-19 and is not repeated here.

The 2026-10-21 run of hiof, nih, uib, uia, usn and uit (Sonnet agents) is in
git history (098445b); its verdicts for the other five stand. The
2026-10-17 synthesis is in git history (a8ba3a5).

## Summary

- Where a page holds the plan, extraction is faithful at all four: whole
  plan, right course, readable structure.
- **nmbu: Norwegian pages that only point to the English plan** ("Se
  engelsk emnebeskrivelse", "See English text" under every heading; 2 of 28
  sampled pages). NMBU has no `english_page`, so the harvest would keep the
  pointer and lose the plan; as a current-only site it cannot fetch it
  later. The one finding that a harvest decides.
- **uio: a login page saved as a fetched page** (GEO9630, both offerings:
  UiO Weblogin, ledger outcome `ok`). The course page is not public; the
  build should treat a login page as no plan.
- **The seven of 2026-10-17: the fetch fixes hold.** nla's pages hold the
  plan of the requested academic year, hvl's English-only stubs and nord's
  are gone, and no wrong page or wrong year reaches `course_plan`: the
  pages the agents flagged (inn's catalogue search pages and later
  versions, nord's 2026/27 descriptions under 2019 offerings, oslomet's
  error page FADUFA) all have no plan in the build (findings removed,
  below).
- hiof: the two wrong-year pages of `videre/` no longer get a plan
  (`page_term` and the later-version rule); the finding is removed.
- Noise, all rebuilt from the raw store: uio's shared "Mer om eksamen ved
  UiO" navigation block (22 of 26 texts), discontinuation notices outside
  the selector, "BinaryValueReference{…}" in the teaching-semester field;
  mf's glued headings and list items and its library-access block; nmbu's
  glued module labels and the "KI-kategoriene" link label; hiof's
  "Innholdsfortegnelse" and FS footer.

## Cross-institution patterns

- **Pointer pages to an English plan**: nmbu now, as hvl, nord and ntnu
  before. The `english_page` mechanism (notice, from, to) serves all four.
- **Glued inline text** (mf, nmbu): inline elements without a separator
  in the HTML lose their boundary in the text; a reader issue, not a
  fetch issue.

## Prioritised fix list

1. **nmbu: `english_page`** (high, occasional; `nmbu_M60-IES-1_2025_autumn_1`,
   `nmbu_NOVA-416-1_2025_autumn_1`). Where: NMBU's entry in
   `R/institution_config.R`, then fetch the pilot's pointer pages again.
   Needed before the NMBU harvest. Also check the 404 of MILJØ300 (a
   non-ASCII code: URL encoding).
2. **uio: login page as no plan** (high, rare; `uio_GEO9630-1_2025_autumn_1`),
   and the navigation block, status notices and "BinaryValueReference"
   (medium). Rebuild only.
3. **mf, nmbu: glued inline text and stock boilerplate** (medium, low).
   Rebuild only.
4. **ntnu: the exam tab's comments on temporary changes** (medium, 6 of
   28: the footnote marks stay, their explanations are not captured;
   `ntnu_MV2013-1_2019_autumn_1`). Rebuild only.
5. **Page chrome and encoding** (low): hiof "Innholdsfortegnelse" and FS
   footer; ntnu "course-details-portlet" and C1 control characters; oslomet
   "Emnehistorikk", "Timeplan"; inn "Startsemestre" list and status banner;
   hvl "Mer om hjelpemidler"; nord an introduction repeated under
   "Beskrivelse av emnet".

## Changes since the previous run

- hiof: the wrong-year plans fixed; the page chrome persists.
- The seven of 2026-10-17: gone are nla's empty and wrong-year pages, hvl's
  English stubs and soft-404 text, nord's stubs and page chrome, oslomet's
  empty pages; persisting are inn's later versions and search pages and
  nord's 2026/27 pages under 2019 offerings (now without a plan), ntnu's
  missing exam-tab comments; new are low chrome findings.
- uio, mf, nmbu: the findings of 2026-10-13 (teacher education: wrong-year
  metadata, reading-list gaps, duplicates) are not seen in the new sample,
  which is of other courses and other pages; that is not proof they are
  fixed.

## Rejected findings

- **inn "wrong page" and "wrong year", nord "wrong year", oslomet "wrong
  page" (high)**: every offering they cite (inn_1AKT01, inn_2PRLU20-29,
  inn_3JUS330, inn_INN3024, inn_KSAM120; nord_RET1002, nord_RE214E,
  nord_MFL240, nord_BE207E, nord_KL305S of spring 2019;
  oslomet_FADUFA-1_2018_spring_1) has no `course_plan` in the build of
  784e658: the later-version rules (inn, nord) and the error-page rule
  leave them without a plan, as intended. The packet shows
  `extracted_text`, which keeps the page. Removed from the findings files.
- **hiof, "wrong year" (high)**: both pages hold a later plan, but
  `course_plan` is empty for both offerings in the current build: the
  later-version rule leaves them without a plan, as intended. Removed from
  `findings/hiof.json`.

## Verdict (`--pilot`, fulltext: `fetched`, `plan`)

| inst | fetched | plan | verdict |
|---|---|---|---|
| hiof | .50 ⚑ (n 100) | .47 ⚑ | ready with known limits |
| uio | .83 (n 100) | .02 ⚑ | ready with known limits |
| mf | .83 (n 100) | .05 ⚑ | ready with known limits |
| nmbu | .64 ⚑ (n 100) | .01 ⚑ | not ready |
| nih, uib, uia, usn, uit | | | ready with known limits (2026-10-21) |
| ntnu | .79 ⚑ (n 100) | .79 ⚑ | ready with known limits |
| oslomet | 1.00 | .93 | ready |
| inn | 1.00 | .64 ⚑ | ready with known limits |
| nord | .99 | .82 | ready |
| hvl | .79 ⚑ | .79 ⚑ | ready with known limits |
| nla | .79 ⚑ | .76 ⚑ | ready with known limits |
| hivolda | .77 ⚑ | .77 ⚑ | ready with known limits (2026-10-17; harvested 2026-10-19) |

- **hiof — ready with known limits.** `fetched` .50 < .80: the site has no
  page for most of the 2007 codes (18 of 25), for PhD courses (no PhD
  folder) and for Akademi for scenekunst after 2021 (data notes). `plan`
  .47: in addition, later plans under an earlier year folder are left out.
- **uio — ready with known limits.** `plan` .02: the pilot pages are
  2026/27 plans that join an offering only once DBH has 2026 (by design for
  a current-only site). The 404s are 10 codes (special-curriculum `*SP`
  codes, PhD and seminar codes) and 14 offerings have no URL; GEO9630 is
  behind a login (fix 2, rebuild only). English-taught courses: 29 of 41
  pages serve the English plan at the plain URL, 8 more have the whole
  Norwegian plan; 2 discontinued PSY courses have their plan only on
  index-eng.html (no future offerings).
- **mf — ready with known limits.** `plan` .05 as at uio. 24 offerings
  (12 PHI language courses) answer 403 to every user agent, a browser's
  too: access-restricted pages (data notes, 2026-10-17); 9 are 404s of
  discontinued courses.
- **nmbu — not ready.** Fix 1: pointer pages to the English plan, which
  only the harvest can fetch. `fetched` .64 < .80: the 67 404s are 36
  codes, mostly not ordinary course pages (UNIS Svalbard AB-/AG- codes,
  VU- continuing education, SPE- codes, PhD, test codes; one of them has a
  page in another offering); MILJØ300 to check (fix 1). `plan` .01 as at
  uio.
- **ntnu — ready with known limits.** `fetched`/`plan` .79 < .80 (21
  course-years): spring/summer 2007 offerings read 2006/07, which the site
  does not have; shells of courses not taught that year; a PhD thesis code
  (2026-10-17).
- **oslomet — ready.** No flag; FADUFA's error page gets no plan.
- **inn — ready with known limits.** `plan` .64: pages that show only a
  later version, and codes the catalogue does not know (search page); both
  get no plan.
- **nord — ready.** No flag; the English-only stubs fixed.
- **hvl — ready with known limits.** `fetched`/`plan` .79: soft 404s ("Vi
  kunne ikkje finne emnet") for PhD, test, practicum and
  continuing-education codes the site has no page for.
- **nla — ready with known limits.** `fetched` .79: 21 course-years whose
  every URL answers 404 (site limit); `plan` .76: years absent from the
  page's JSON.
