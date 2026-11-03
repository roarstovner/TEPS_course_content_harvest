# anonymization audit (pilot, Codex reviewers): synthesis

Run 2026-11-02, `--pilot`, reviewers `codex exec -m gpt-6.1-sol`, findings in
`findings-codex/`, report `findings-codex_report.md`. Institutions: khio, nhh,
vid (28 courses each, all read in full). Build of 2026-11-02.

KHiO and NHH show only the current plan: their pilot runs (2026-11-01,
2026-10-31) hold 2026/27 pages, which the build gives to no DBH 2025
offering. Their packets therefore anonymize the pilot run's own text
(`pilot_run_rows()`, as the fulltext packets already did); the build has no
KHiO or NHH plan yet, so `privacy_hits` = 0 with n = 0 says nothing for them.

## Summary

- No personal data left in any of the 84 plans (no names, e-mails, phones).
- khio: 1 finding (medium): the programme version year in "Inngår i
  studieprogram / Bachelorstudium i billedkunst (2022)" stays in 27/28 plans.
  It dates a programme version, not a person; whether it is an admin date to
  remove or part of the programme name is a spec question (low stakes).
- nhh: 4 findings in free English text, none a privacy leak:
  - over-removal: "2008-2009 financial crisis" removed (nhh_FIE448-1_2025_autumn_1, a random
    control). `.drop_academic_year()` (R/anonymize.R) removes any
    consecutive-year pair from 1990, so a content range of two consecutive
    years goes too. Generic rule: affects every institution.
  - admin dates left in English ("Aug 23rd to Sept 6th", "due date: Feb 1st",
    "For 2025 it will be at NHH").
  - clock times in dot or am/pm form left ("12.00", "3-6pm CET");
    `.clock_regex` matches only `HH:MM`.
  - text corruption (low): removing "09:00"/"14:00" leaves "held between at
    the first day ... and on the third day".
- vid: 0 findings; all 5 flags were false alarms (a regulation dated
  "6. januar 2017" correctly kept, biblical/English titles).

## Cross-institution patterns

The NHH findings come from generic rules: `.drop_academic_year()` (content
year pairs) and `.clock_regex` (English clock and date formats). Fixing them
touches every institution and needs `preview_change()`.

## Prioritised fix list (not done here; fixing is a separate task)

1. `R/anonymize.R` `.drop_academic_year()`: keep a consecutive-year pair
   followed by a content noun ("2008-2009 financial crisis") — or accept it
   as a known deviation. nhh; check others with `preview_change()`.
2. `R/anonymize.R` clock rule: English "3-6pm", dotted "12.00" when preceded
   by "at"/"kl."; and drop the dangling "at"/"on" with the time
   (ECN434, FIN547, nhh_FIE401-1_2025_autumn_1). nhh, likely uib/uio English plans.
3. English admin dates ("Aug 23rd", "Feb 1st"): low priority; not personal data.
4. khio "(2022)" programme version year: decide in `course_plan_spec.md`.

## Rejected findings

None.

## Verdict (--pilot, anonymization)

`privacy_hits`: khio 0 (n 0), nhh 0 (n 0), vid 0 (n 44). No confirmed
high-severity finding.

- **khio: ready for full harvest** (anonymization). No personal data on the
  sample; the gate number is vacuous until the build has KHiO plans.
- **nhh: ready for full harvest** (anonymization). Four medium/low
  generic-rule findings go to fix issues; they need no new harvest.
- **vid: ready for full harvest** (anonymization). No finding,
  `privacy_hits` 0 of 44.

## phs (pilot, 2026-11-03)

Run 2026-11-03, `--pilot`, reviewer `codex exec -m gpt-6.1-sol`; 28 courses
(5 suspects, 23 controls), all read. No personal data and no admin dates
left; the 4 `removed` flags are the semester labels of very short (shell)
pages, the `name_like` flag is the course title "Core concepts in Digital
Investigation & Forensics".

- **medium, rare, kept** — over-removal of a content date:
  "Riksadvokaten utga [−høsten 2023−] retningslinjer for politiavhør av
  barn" (phs_AVMISBA-1_2025_spring_1). Reproduced with `anonymize_text()`
  on the sentence. Cause (checked): `.anon_generic()` drops season + year;
  the government-document guard does not know Riksadvokaten and looks only
  before the date, while "retningslinjer" follows it. Fix in
  `R/anonymize.R` (guard), rebuild only. This offering has no plan in the
  pilot build (an earlier version), so no published text is hit yet.

`privacy_hits` 0 (n 12, threshold 0).

**Verdict phs (anonymization): ready** (one medium fix issue).
