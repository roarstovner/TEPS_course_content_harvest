# Fulltext audit — synthesis (pilot re-run: hiof, after the fixes)

Run 2026-10-22 with `--pilot` on the build of commit c8c0b29, after the
fixes from the pilot audit of 2026-10-21: only hiof, whose verdict was "not
ready" (a later plan under an earlier year folder). The packet holds 28
pages of the pilot sample (3 suspects, 25 random; 152 KB). One review agent
in the Codex CLI (gpt-6.1-sol, reasoning effort high); it read all 28
courses. The 2026-10-21 run of all six (hiof, nih, uib, uia, usn, uit) used
Sonnet agents; its synthesis is in git history (098445b), and its verdicts
for the other five stand (table below).

## Summary

- Where a HiOF page holds the plan, extraction is faithful: whole plan,
  right course, readable structure. The only uncaptured page text is
  breadcrumbs.
- The two wrong-year pages of `videre/` are still found in
  `extracted_text`, but the build no longer gives them a plan:
  `page_term` reads "(Vår|Høst YYYY)" from the title and the later-version
  rule leaves `course_plan` empty for both. The finding is removed (below).
- Low: the "Innholdsfortegnelse" label and the FS timestamp ("Sist hentet
  fra FS …") stay in the text of every page. The sections drop the first,
  the anonymizer the second.

## Prioritised fix list

1. **Low noise** (hiof): "Innholdsfortegnelse" and the FS footer in
   `extracted_text`; only worth a rule if a section is affected (none is).

## Changes since the previous run

- Fixed: the wrong-year plans (`hiof_LSKNO22215-1_2019_spring_1`,
  `hiof_LSKAS10116-1_2022_autumn_1`) have no `course_plan`; HiOF's `plan`
  share fell from .49 to .47 as a result.
- Persisting: the page chrome (low).

## Rejected findings

- **hiof, "wrong year" (high)**: both pages hold a later plan, but
  `course_plan` is empty for both offerings in the current build (checked
  in `course_offerings_full.RDS`): the later-version rule leaves them
  without a plan, as intended. The packet shows `extracted_text`, which
  keeps the page. Removed from `findings/hiof.json`.

## Verdict (`--pilot`, fulltext: `fetched`, `plan`)

| inst | fetched | plan | verdict |
|---|---|---|---|
| hiof | .50 ⚑ (n 100) | .47 ⚑ | ready with known limits |
| nih | .22 ⚑ | .22 ⚑ | ready with known limits (2026-10-21) |
| uib | .52 ⚑ | .46 ⚑ | ready with known limits (2026-10-21) |
| uia | .77 ⚑ | .76 ⚑ | ready with known limits (2026-10-21) |
| usn | .58 ⚑ | .57 ⚑ | ready with known limits (2026-10-21) |
| uit | .67 ⚑ | .66 ⚑ | ready with known limits (2026-10-21) |

- **hiof — ready with known limits.** `fetched` .50 < .80: the site has no
  page for most of the 2007 codes (18 of 25), for PhD courses (no PhD
  folder) and for Akademi for scenekunst after 2021, checked against the
  folder listings (data notes). `plan` .47: in addition, the later plans
  under an earlier year folder are left out.
- The reasons for the other five are in the synthesis of 2026-10-21 (git
  history, 098445b).
