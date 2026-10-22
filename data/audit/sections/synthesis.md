# Sections audit — synthesis (pilot re-runs of 2026-10-22: hiof, uit, usn)

Two rounds on 2026-10-22, both with `--pilot` and review agents in the Codex
CLI (gpt-6.1-sol, reasoning effort high); every agent read all its courses;
all findings pass mechanical verification (0 ✗). The 2026-10-21 run
(Sonnet agents) is in git history (098445b); its verdicts for nih, uib and
uia stand.

- **Round 1** (build of c8c0b29): hiof, uit, usn, after the fixes from
  2026-10-21. 21 findings.
- **Round 2** (build of 4b0b546): uit, usn, after the fixes of round 1:
  coursework-gate sentences leave `assessment` at every institution and
  USN's inline split is on (4a6ee84); on a USN page with a table of
  contents only whole headings open sections (4b0b546). Same seed: UiT's
  packet holds the same 20 courses; USN's suspects changed with the fixes
  (19 courses, 7 suspects, 12 random).

## Summary

- **The fixes hold.** HiOF's "Litteratur:" outcome label stays in
  `learning_outcomes`; UiT's gate sentences ("Tilgang til slutteksamen
  forutsetter godkjente øvelser.", the English "Assignments:" block of
  BNS-1006) are in `coursework_requirements`; USN's arbeidskrav 2/3 and gate
  sentences left `assessment`; SMERTE100's content no longer opens
  `assessment` (97 USN pages had a content line open a section; 70 changed,
  none lost a section).
- **gpt-6.1-sol reads the spec more strictly than the Sonnet agents did**:
  it rates as high any sentence the spec puts in another section, whatever
  heading it stands under ("A section holds the plan text about its part,
  whatever heading the page puts it under", `course_plan_spec.md`). The
  ratings are re-judged against `findings_schema.md` below, so the
  institutions are judged alike across runs and models.
- **One high finding left**: a gate wording the new rule does not cover
  (`uit_BED-6001-1_2008_autumn_1`: "For å kunne fremstille
  prosjektoppgaven til vurdering må deltakerne ha fått godkjent på
  mellomperiodearbeidet."), so the plan has no coursework section. Fix in
  progress (count first, rebuild only).
- Everything else is a sentence under another part's heading (attendance in
  a teaching or Praksis block, prior knowledge in an admission field,
  content phrased as abilities), exam logistics in `assessment`,
  placeholders, or library-list headers.

## Cross-institution patterns

- **Requirements under the teaching heading** (hiof: "Det gis to
  obligatoriske veiledningstimer per oppgave."; uit HEL-3050; usn 050-E11,
  NETPRL: attendance and pass/fail rules in a Praksis block): the
  sentence rule reads `assessment` only. A Praksis section of its own is
  the open question for these.
- **Exam logistics inside `assessment`** (hiof, uit, usn): resit and appeal
  rules, examiner arrangements, UiT's English "Re-sit examination" block
  and exam-portal sentence, "Det kan bli mindre endringar i planen."
- **Admission field holding prior knowledge** (uit "Opptakskrav" /
  "Admission requirements": "minimum 60 studiepoeng i norsk", "minimum 25
  credits mathematics") and the reverse, admission text in
  `prerequisites` (usn "Generelle opptakskrav.", "Realkompetanse kan
  vurderes."): the known deviation in `course_plan_spec.md`.
- **Placeholder reading lists** (uit 2008 "Se hjemmeside", "Ta kontakt med
  instituttet/fakultetet", the Leganto notice; hiof "Ny pensumbok
  vurderes."): the open placeholder issue.

## Prioritised fix list

1. **uit: gate wording "… til vurdering må … godkjent"** (high, rare;
   `uit_BED-6001-1_2008_autumn_1`). Where: `.gate_sentence_regex` in
   `R/extract_sections.R`. Count across the build first.
2. **uit English language field** (medium): "Language of instruction and
   examination" appended to `teaching_methods`
   (`uit_BIO-3529-2_2025_autumn_2`); map it to `.drop` as the Norwegian
   label is. With the English "Re-sit examination" block and the
   exam-portal sentence.
3. **Exam logistics and plan-change reservations in `assessment`**
   (medium; hiof, uit, usn).
4. **Low and medium, no rule planned**: requirements under the teaching
   heading, prior knowledge in other fields, content phrased as abilities,
   placeholders, library-list headers in USN reading lists ("Fysikk 0
   (LR-FYS1200) Kongsberg"), course-evaluation notices in
   `teaching_methods`.

## Changes since the previous run

| inst | fixed | persisting | new |
|---|---|---|---|
| hiof (round 1) | "Litteratur:" sub-heading, assessment noise | 2007 gate sentence and formative assessment in `assessment` | mandatory supervision in `teaching_methods`, inline logistics, placeholders |
| uit (round 2) | gate sentences of GEO-2007, BNS-1006, FRI-2580 | ODO-6302 layout, mixed content, prior knowledge in other fields, exam language, placeholders, language field, logistics | BED-6001 gate wording (high) |
| usn (round 2) | SMERTE100 boundary, arbeidskrav 2/3 and gate sentences in `assessment` | Praksis blocks in `teaching_methods`, library headers | NORVU103 nynorsk sentence left in `assessment`, admission text in `prerequisites`, plan-change reservation |

## Re-rated findings

Severity follows `findings_schema.md`: high is "wrong data a downstream
analysis would silently use"; medium is "partial loss, recurring noise
inside otherwise-correct text". The 2026-10-21 run rated the same classes
low or medium at nih, uib and uia; judged alike:

- **One sentence missing from a coursework section that exists**
  (high → medium): hiof's supervision sentence and 2007 gate sentence,
  uit HEL-3050's attendance in `teaching_methods`, usn NORVU103's
  "Teksten skal vere på nynorsk, og blir vurdert til bestått/ ikkje
  bestått.", usn 050-E11 and NETPRL's Praksis attendance rules.
- **Mixed content under one heading** (high → medium): ODO-6302's one long
  "Om emnet" block (accepted as an unusual layout on 2026-10-21); lecture
  hours (BIO-3515) and abilities (END-3602) in the page's content text
  need a sentence classifier, at every institution.
- **Prior knowledge outside `prerequisites`** (high → medium): LER-3101 and
  END-3602 under the admission field, RUS-3950 and BIO-3607 under teaching
  and content; rare, one sentence each.
- **Exam language without an assessment row** (high → low): BIO-8806's page
  has no assessment form.

Kept high: a plan without a coursework section while the page states its
requirements (BED-6001).

## Rejected findings

None.

## Verdict (`--pilot`, sections: `assessment`, `coursework`, `no_sections`, `unmapped_headings`)

| inst | assessment | coursework | no_sections | unmapped | verdict |
|---|---|---|---|---|---|
| hiof | 1.00 (n 47) | .91 | 0 | .24 (n 774) | ready |
| nih | 1.00 (n 22) | .91 | 0 | .01 (n 156) | ready (2026-10-21) |
| uib | 1.00 (n 46) | .83 | 0 | .37 (n 1774) | ready (2026-10-21) |
| uia | 1.00 (n 76) | .88 | 0 | .32 (n 877) | ready (2026-10-21) |
| usn | .96 (n 57) | .93 | 0 | .03 (n 578) | ready |
| uit | .98 (n 66) | .68 ⚑ | 0 | .33 (n 760) | not ready (fix 1 in progress) |

- **hiof — ready.** No number flagged; medium and low findings only after
  re-rating.
- **usn — ready.** No number flagged; the high findings of round 1 are
  fixed and the rest are medium and low after re-rating. `unmapped_headings`
  .03: Annet, Evaluering av emnet, Miscellaneous (none plan text).
- **uit — not ready, until fix 1.** `coursework` .68 < .70 (.62 before
  4a6ee84): the sample plans still without a coursework section have no
  coursework text on the page (mostly 2008, a known limit), except
  BED-6001 (fix 1). Rebuild only.

All fixes are rebuilt from the raw store and need no new harvest.

### Most frequent unmapped headings

Unchanged from 2026-10-21 for hiof and uit (facts box, programme lists,
course evaluation, contact, year picker, admission; none should map). usn:
Annet, Evaluering av emnet, Miscellaneous.
