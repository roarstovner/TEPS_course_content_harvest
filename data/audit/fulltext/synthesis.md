# Fulltext audit — synthesis (pilot, 2026-10-22: hiof re-run; uio, mf, nmbu)

Two runs on 2026-10-22, both with `--pilot` and review agents in the Codex
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

The 2026-10-21 run of hiof, nih, uib, uia, usn and uit (Sonnet agents) is in
git history (098445b); its verdicts for the other five stand. The seven
audited on 2026-10-17 (ntnu, oslomet, inn, nord, hvl, hivolda, nla) were not
re-audited: their `fetched` shares are unchanged on the current build and
`plan` moved only at inn (.59 → .64, from the INN later-version fixes made
after that audit).

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
4. **hiof: page chrome** (low).

## Changes since the previous run

- hiof: the wrong-year plans fixed; the page chrome persists.
- uio, mf, nmbu: the findings of 2026-10-13 (teacher education: wrong-year
  metadata, reading-list gaps, duplicates) are not seen in the new sample,
  which is of other courses and other pages; that is not proof they are
  fixed.

## Rejected findings

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
| ntnu, oslomet, inn, nord, hvl, hivolda, nla | | | as 2026-10-17 (numbers unchanged) |

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
