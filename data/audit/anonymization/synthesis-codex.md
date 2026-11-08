# Anonymization audit, build of 2026-11-06: 13 institutions

Run 2026-11-08 on the build of 2026-11-06 for hiof, hvl, inn, mf, nih, nla,
nmbu, oslomet, steiner, uib, uio, uis and usn. Review agents: Codex
`gpt-6.1-sol`, one per institution. Findings: `findings-codex/{inst}.json`,
report `findings-codex_report.md`. The other institutions in the report (fhs,
himolde, khio, nhh, phs, vid) are older runs, handled before and not covered
here. The pilot synthesis of 2026-11-02 that this file replaces is in git
history. Harvests were running: the checks below read the packets, the code
(`R/anonymize.R`, examples run through `anonymize_text()`) and the
teacher-education frame of the build (`data/processed/teacher_ed/`), no
whole-institution reads.

The 13 reports hold 30 findings: 2 high, 16 medium, 12 low (steiner: none).
Each high and medium finding was checked against its packet and, for the root
cause, against the code: 16 real, 2 a known deviation of the spec, 0
rejected. Of the lows, 11 are real and one is half rejected (below).

**Redacted quotes.** The repository is public, so the staff names the
reviewers quoted are replaced by `[name]` in `findings-codex/hiof.json`,
`uib.json` and `usn.json` (and, left over from the HiMolde pilot, two in the
description of `himolde.json`). A redacted quote no longer occurs verbatim in
the packet, so the aggregator marks these four findings ✗ (hiof, uib, usn,
himolde). The ✗ is expected: each quote was checked by hand against the
unredacted packet before redacting and is real.

## Summary

- Personal data left in (high): staff names under role labels the
  anonymizer has no rule for. HiOF "Foreleser"/"Lecturer" lines (5 of 28
  plans; 20 HiOF offerings carry a name_label flag); UiB staff blocks under
  "Faglig ansvar", "Hovedansvarlige", "Innledere", "Undervisere",
  "Academic responsibility", "Course coordinator" (9 of 28, clinical PhD
  courses and one law course); USN "Course Owner: Professor … Lecturer:
  Associate Professor …" (2 of 28). None of these occurs in the
  teacher-education frame (14,727 plans, 0 matches): the leaks are in the
  all-course frame, which is internal until a release takes it.
- Administrative dates left (medium): long-form dates outside approval
  contexts (session dates, registration and application deadlines,
  withdrawal deadlines, reading-list availability) at hvl, uib, nih, mf, uio,
  hiof; English "fall 2024" (nih); a semester kept in a course title because
  "læreplanteori" sets off the government-document guard (nih).
- Over-removal (medium): dates of cited documents and video running times in
  reading lists (hiof, nla, oslomet, uis), including dates inside URLs, which
  breaks the link; and HiOF's literature-update rule takes the sentence after
  the stamp with it.
- Stranded words after a removal (low; mf, nih, uib, hiof, nmbu): "Anbefalt
  litteratur for.", "ECTS)()", "også i.".
- Two findings (uio, mf) are conditions written with a season and a year,
  the spec's known deviation; one question for the user goes to the
  sections-audit decision issue.

## Cross-institution patterns

1. **Role labels missing from the staff rules** (hiof, uib, usn; himolde in
   its pilot). `.staff_roles` (R/anonymize.R) has no lecturer, foreleser,
   course owner, undervisere, hovedansvarlige, innledere, faglig ansvar or
   academic responsibility. Where a label is matched, `.role_heading_name_regex`
   takes one name line, so a list of names under a heading, separated by
   blank lines (UiB), stays. HiOF's own rule matches only "Foreleser(e)
   studieåret …:"; on the pages "Lecturer spring 2016:" and "Foreleser høst
   2017:" the generic semester rule removes the season and year, which leaves
   "Lecturer: <name>". Checked: the four quoted layouts pass through
   `anonymize_text()` unchanged apart from the semester.
2. **Long-form dates are removed only after approval words**
   (`.approval_date_regex`), and `.long_dates_regex` needs a final year.
   Schedules and deadlines keep their dates at six institutions: "Påmeldingsfrist
   13. september 2025." (nih), "Undervisningsperiode 23.-26. oktober 2023."
   (uib), "15-16 September and 6-7 October", "1th September" (hvl), "innen
   trekkfristen 1. mai/1. november." (mf, all 8 plans; 22 teacher-ed plans),
   "Innleveringsfrister: 1. juni og 15. desember" (uio), "Litteratur vil være
   tilgjengelig innen 1. juni 2020." (hiof). One rule keyed on a deadline or
   session label, with day ranges and an optional year, covers them.
3. **Reference dates and URLs are not protected.** The dd.mm.yyyy rule
   protects a date after a comma or inside a path only when a digit, dot or
   slash precedes it: "(01.12.2016).pdf" in a URL (oslomet),
   "Rapport_UDA_7.6.2011.pdf" and "(18.11.2014)" after a newspaper title
   (hiof) go. The clock rule removes "(16:00)", a video's running time after
   a YouTube URL (nla; checked). `.not_gov` looks back on the same line only,
   so a framework-plan date on the line after the UDIR URL goes and leaves
   "()" (nla; checked: on one line the date stays). A consecutive-year range
   that ends a reference line counts as an academic year: "i Norge i alderen
   18-70 år, 2010-11" (uis), "(2016-17)" (hiof). Protecting whole URL tokens
   before the date and clock rules, and allowing one line break in the
   government-document look-back, fixes most of them.
4. **Cleanup after removals** handles only "Pensumlist… for.", and empty
   brackets only after a space: "Kjernelitteratur for." / "Anbefalt
   litteratur for." (nih), "ECTS)()" and "Tema for:" (uib), "høst- og
   tidlig." (hiof), "også i." (nmbu).

## Prioritised fix list (not done here; fixing is a separate task)

All in `R/anonymize.R` and `tests/testthat/test-anonymize.R`; each batch then
`preview_change()` on the affected institutions, one rebuild for the lot,
`metrics_check` and `frozen_check` rows explained.

**A. Staff names under role labels (high; hiof, uib, usn).**
- `.anon_hiof()`: generalise the lecturer rule to `^(?:Forelesere?|Lecturers?)`
  with any semester or academic-year text before the colon, the names on the
  line or the next; run before the generic semester rule
  (hiof_SFB50314-1_2016_spring_1, hiof_SFB70019-1_2019_spring_2).
- `.anon_uib()`: a staff block from "Faglig ansvar", "Hovedansvarlige:",
  "Innledere", "Undervisere:", "Kurskoordinator" up to the next section
  heading ("Kjernelitteratur", "Seminarer:", "Program", "Emneevaluering"),
  name lines only, with "Dr." and blank lines between; the inline English
  labels "Academic res(p)onsibility <unit>: <names>." and "Course coordinator
  <unit>: <name>" (uib_NEUROSYSM930-0_2024_autumn_1,
  uib_CCBIONEUR911-0_2025_autumn_1, uib_JUR601-0_2025_spring_1).
- `.staff_roles`: add "course owner" and "lecturer" (the title before the
  name is already read by `.title_regex`), so USN's line goes in the generic
  inline rule (usn_PN-SYS9000-1_2023_spring_1, usn_PN-SYS9000-1_2021_spring_1).
  Check "Senior Lecturer" in reference lists with `preview_change()`.
- Also add the new labels to the check of the shareable files
  (`.personal_data_patterns`), so `privacy_check` sees them once the
  all-course frame is published.

**B. Administrative dates left (medium; hvl, uib, nih, mf, uio, hiof, nla, inn).**
- A generic rule: a deadline or session label (påmeldingsfrist, frist,
  søknadsfrist, trekkfrist, innleveringsfrist(er), undervisningsperiode,
  "Dato:", "application deadline", "dates for the course", "tilgjengelig
  innen") followed by long-form dates and day ranges ("23.-26. oktober",
  "15-16 September", "1th September"), year optional
  (nih_PHD641-1_2025_autumn_1, uib_CCBIO906-0_2023_spring_1,
  hvl_PHD907-1_2018_spring_1, mf_RL1016-1_2025_autumn_1,
  uio_NOR4091-1_2025_autumn_1).
- `.season_regex`: add "fall" (nih_FRI301-1_2024_autumn_1).
- `.gov_doc_regex`: "læreplan" only as a word or the document's name, not
  inside "læreplanteori" or "Læreplanarbeid" (nih_LKI106-1_2024_spring_2).
- Smaller: "Revised May 2019" (nla_3GJ302-1_2020_spring_1), "kl. 17 - 20.00"
  (hour-only start of a kl. range, hvl_NYB801-1_2021_spring_2), "oktober
  2015" after "oppstartsamling i" and the glued "Universityvåren 2019" (hiof),
  compact ddmmyy in INN's credit-overlap "Dato" column
  (inn_2SPGL1-1-1_2025_spring_1).

**C. Over-removal in reading lists (medium; hiof, nla, oslomet, uis, nmbu).**
- Mask URL tokens before the date and clock rules
  (oslomet_ARK1400-1_2018_spring_2, hiof_LURLE20213-1_2017_spring_1).
- A parenthesised dd.mm.yyyy after a reference title stays, as after a comma
  (hiof_LURLE20213-1_2016_spring_1).
- A running time in brackets after a URL or a video cue stays
  (nla_3KL106-1_2020_spring_1, all four sampled 3KL104/3KL106 offerings;
  count the change with `preview_change()`).
- `.not_gov`: allow one line break in the look-back (nla_BLKO4-1_2019_spring_1).
- A consecutive-year range at the end of a reference line, after a comma or
  in brackets after a title, stays unless an academic-year cue precedes it
  (uis_MGL2090-1_2018_autumn_2).
- `.anon_hiof()` "Litteratur… sist oppdatert": remove the stamp sentence
  only, not the rest of the line (hiof_LURLE20213-1_2016_spring_1).
- Not a finding, seen in a control: "Første gang emnet undervises er
  studieåret 2025/26." loses its year (nmbu_PPFD201-1_2025_autumn_2), while
  the season a course starts from stays (`.course_start`, decided
  2026-10-28). The same guard for an academic year after "første gang".

**D. Stranded words and leftover boilerplate (low; nih, uib, hiof, nmbu, inn).**
- Extend the bare-notice cleanup to "Kjernelitteratur for." and "Anbefalt
  litteratur for." (nih_PHD646-1_2025_spring_1); "Tema for <season year>:" to
  "Tema:" and "ECTS)()" (uib_INF367-0_2025_autumn_1,
  uib_GOV216-0_2023_spring_2); "også i." with its sentence
  (nmbu_PPRA201-1_2025_autumn_1); "høst- og tidlig vårsemester 2016" keeps
  "vårsemester" (hiof_LSPUBPD15-1_2015_autumn_2).
- "(Lesedato:.20)": the clock rule takes "12:05" from a malformed access date
  (hiof_HSASO20118-1_2020_autumn_1).
- The address fragment "på@gfi.uib.no" left by two glued adviser addresses
  (uib_ENERGI322-0_2024_autumn_1).
- UiB reading-list notice variants: "vil vere klar 1. august …" without
  "innan" and the English "The reading list will be ready before …"
  (uib_MOL100-0_2024_autumn_1, uib_INFO284-0_2022_autumn_1).
- INN's "Feilmelding / Notice: Undefined index …" PHP banner before a plan
  (inn_2MNK172-2-1_2022_spring_1): `.anon_inn()`, or the institution's
  `exclude` if the banner has its own element.

## Changes since the previous run

First anonymization audit of these 13 institutions in `findings-codex/`; the
report's comparison with `HEAD` covers only himolde (unchanged).

## Known deviation, not a new defect

- uio_SPA4192-1_2025_autumn_1, "Muntlig eksamen er obligatorisk for deg som
  fikk opptak fra og med [høsten 2021]": this exact wording is in the spec's
  known deviations (a condition written with a season and a year loses both).
- mf_SAM2080L-1_2025_autumn_1, "[Vårsemesteret 2026] har SAM2080L skriftlig
  skoleeksamen …": an exception for one semester, a condition by the spec
  ("Exception for academic year 2020-2021: Spring." stays), so the season and
  year should stay; the same known deviation. The reviewer's fix ("I ett
  semester") would put words in the plan that the page does not have.
  Whether to add this wording to the deviation or guard it is a question for
  the user.

## Rejected (in part)

- hiof_HSASO20118-1_2020_autumn_1 (low): "(Lesedato: 12.05.20)" staying is
  not a defect. An access date belongs to the citation, like a document's
  date; the spec removes administrative dates of the course, not reference
  metadata. The malformed "(Lesedato:.20)" in the same finding is real
  (batch D).

## Questions for the user

1. Conditions written with a season and a year (mf, uio above): extend the
   known deviation to the MF wording, or guard each wording so the season and
   year stay?
2. Yearless deadlines that recur every year (MF "trekkfristen 1. mai/1.
   november", 22 teacher-ed plans; UiO "Innleveringsfrister: 1. juni og 15.
   desember"): the spec puts deadlines out, and batch B removes them; they do
   not date the offering, so keeping them would also be defensible.
