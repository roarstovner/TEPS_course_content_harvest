# Sections audit — synthesis (pilot re-run: hiof, uit, usn, after the fixes)

Run 2026-10-22 with `--pilot` on the build of commit c8c0b29, after the
fixes from the pilot audit of 2026-10-21 (HiOF "Litteratur:" sub-heading,
UiT 2008 arbeidskrav and language label, USN placeholders and notice,
unmapped headings counted for the text reader): the three institutions
whose sections verdict was "not ready". Packets of 14–20 plans from the
pilot sample (122–150 KB). One review agent per institution in the Codex
CLI (gpt-6.1-sol, reasoning effort high); every agent read all its courses.
21 findings, all pass mechanical verification (0 ✗). The 2026-10-21 run
(Sonnet agents) is in git history (098445b); its verdicts for nih, uib and
uia stand.

## Summary

- The fixes hold: HiOF's "Litteratur:" outcome label stays in
  `learning_outcomes` (its flag is now a false alarm), UiT's language
  label is gone from the Norwegian pages, and USN's placeholder and generic
  notice are gone.
- **gpt-6.1-sol reads the spec more strictly than the Sonnet agents did**:
  it rates as high any sentence the spec puts in another section, whatever
  the heading it stands under ("A section holds the plan text about its
  part, whatever heading the page puts it under", `course_plan_spec.md`).
  That gives 9 high findings against 4 before, most of them single
  sentences. The ratings below are re-judged against the severity scale of
  `findings_schema.md` (see "Re-rated findings").
- **Two real problems remain**:
  - **Coursework gate sentences left in `assessment`** (uit, usn). The
    inline split moves labelled blocks ("Arbeidskrav:"), not a sentence
    such as "… må være godkjente før studenten kan framstille seg til
    eksamen", "Tilgang til slutteksamen forutsetter godkjente øvelser.",
    "The assignments have to be approved … before students are allowed to
    sit the final exam". In the whole build, 44 of the 197 UiT plans
    without a coursework section have such a sentence in `assessment`,
    and 19 plans with one leave a gate sentence behind.
  - **usn: course content at the start of `assessment`**
    (`usn_SMERTE100-1_2022_autumn_1`, text fallback, no headings read): the
    last topic of the content ("Hovedtema 16: Etikk og smerte …") opens the
    assessment section, and two semester lines close the coursework
    section. Cause not found yet.
- The USN practice-course findings of 2026-10-21 persist (arbeidskrav 2/3
  in `assessment`, attendance and pass/fail rules in the Praksis block);
  turning on USN's inline split is the open follow-up.

## Cross-institution patterns

- **Gate sentences outside the coursework section** (uit, usn, hiof 2007;
  NTNU and Nord before): the same class as "requirements outside
  `assessment`" of 2026-10-21. A sentence-level rule in
  `.split_inline_coursework()` (a sentence that makes approval, attendance
  or a passed test a condition for the exam moves to
  `coursework_requirements`) would serve several institutions. Count it in
  the whole build first; a section rule gets a `methods.qmd` entry.
- **Exam logistics inside `assessment`** (hiof 5 of 19, uit 10 of 20):
  resit and appeal rules, examiner arrangements, UiT's English "Re-sit
  examination" block and its exam-portal link sentence. The spec keeps
  them out; the standalone Norwegian headings are dropped already.
- **Admission field holding prior knowledge** (uit "Opptakskrav" /
  "Admission requirements": "minimum 60 studiepoeng i norsk", "minimum 25
  credits mathematics"): the field is unmapped as admission, so the
  prior-knowledge sentence is not in `prerequisites`. The reverse of the
  known deviation (admission rules kept in prerequisites).
- **Placeholder reading lists** (uit: "Se hjemmeside", "Ta kontakt med
  instituttet/fakultetet", the Leganto notice; hiof "Ny pensumbok
  vurderes."; hiof "Ingen arbeidskrav i emnet."): the open placeholder
  issue.

## Prioritised fix list

1. **Gate sentences in `assessment`** (high, occasional; uit, usn). Where:
   `R/extract_sections.R`, `.split_inline_coursework()`, a sentence pattern
   beside the label patterns. Examples `uit_GEO-2007-1_2008_autumn_1`,
   `uit_BNS-1006-1_2022_spring_1`, `usn_OKLI3014-1_2018_autumn_1`. Expected:
   up to 44 more UiT plans with a coursework section; at USN with the
   inline split turned on.
2. **usn: content in `assessment` on a text-fallback page** (high, one
   sample plan; `usn_SMERTE100-1_2022_autumn_1`). Where: the USN text
   reader or the text fallback. Find the cause and count how many USN plans
   it affects.
3. **usn practice courses** (high, persisting; `usn_BS-PLVI30-V-1_2018_autumn_3`,
   `usn_050-E11-1_2018_autumn_1`): the open USN inline-split follow-up, and
   the open question whether praksis is a section of its own.
4. **uit English language field** (medium): "Language of instruction and
   examination" is appended to `teaching_methods` on English pages
   (`uit_BIO-3529-2_2025_autumn_2`); map it to `.drop` as the Norwegian
   label is.
5. **Exam logistics in `assessment`** (medium): uit "Re-sit examination"
   and the exam-portal sentence, hiof inline resit and appeal rules.
6. **Low and medium, no rule planned**: admission field holding prior
   knowledge (uit), placeholders (uit, hiof), library-list metadata in USN
   reading lists ("Fysikk 0 (LR-FYS1200) Kongsberg"), course-evaluation
   notices in `teaching_methods` (uit).

## Changes since the previous run

| inst | fixed | persisting | new |
|---|---|---|---|
| hiof | "Litteratur:" sub-heading, assessment noise | 2007 gate sentence and formative assessment in `assessment` | mandatory supervision in `teaching_methods`, inline logistics, placeholders, exam-design rules under other headings |
| uit | 2008 arbeidskrav blocks (split), Norwegian language label | ODO-6302 layout, admission field, placeholders, gate sentences | English language field, exam logistics, prerequisites in other fields, exam language without an assessment row |
| usn | placeholder reading list, generic notice | practice courses, library metadata | SMERTE100 boundary, resit rules, "Generelle opptakskrav." as prerequisites |

## Re-rated findings

Severity follows `findings_schema.md`: high is "wrong data a downstream
analysis would silently use"; medium is "partial loss, recurring noise
inside otherwise-correct text". The 2026-10-21 run rated the same classes
low or medium at nih, uib and uia; to judge alike:

- **hiof, 2007 gate sentence and Underveisvurdering in `assessment`
  (high → medium)**: the coursework section of both plans holds the
  requirements; `assessment` repeats "Godkjente arbeidskrav er et vilkår
  for framstilling til eksamen." Formative assessment is assessment
  design. Noise in otherwise-correct text.
- **hiof, mandatory supervision in `teaching_methods` (high → medium)**:
  one sentence ("Det gis to obligatoriske veiledningstimer per oppgave.")
  missing from a coursework section that exists; partial loss.
- **uit, mixed content under one heading (high → medium)**: ODO-6302's one
  long "Om emnet" block was accepted as an unusual layout on 2026-10-21;
  lecture hours (BIO-3515) and abilities (END-3602) in the page's own
  content text would need a sentence classifier, at every institution.
- **uit, prior knowledge outside `prerequisites` (high → medium)**:
  LER-3101 and END-3602 under the admission field, RUS-3950 and BIO-3607
  under teaching and content; rare, one sentence each.
- **uit, exam language without an assessment row (high → low)**:
  BIO-8806's page has no assessment form; the exam-language line alone.

Kept high: the gate sentences at uit and usn (a plan without a coursework
section while the page states its requirements), SMERTE100 (course content
in `assessment`), USN's practice courses.

## Rejected findings

None.

## Verdict (`--pilot`, sections: `assessment`, `coursework`, `no_sections`, `unmapped_headings`)

| inst | assessment | coursework | no_sections | unmapped | verdict |
|---|---|---|---|---|---|
| hiof | 1.00 (n 47) | .91 | 0 | .24 (n 774) | ready |
| nih | 1.00 (n 22) | .91 | 0 | .01 (n 156) | ready (2026-10-21) |
| uib | 1.00 (n 46) | .83 | 0 | .37 (n 1774) | ready (2026-10-21) |
| uia | 1.00 (n 76) | .88 | 0 | .32 (n 877) | ready (2026-10-21) |
| usn | .96 (n 57) | .93 | 0 | .03 (n 584) | not ready |
| uit | .98 (n 66) | .62 ⚑ | 0 | .33 (n 760) | not ready |

- **hiof — ready.** No number flagged; medium and low findings only after
  re-rating.
- **usn — not ready.** Fix 2 (SMERTE100, cause unknown) and fix 3
  (practice courses), with fix 1. `unmapped_headings` is now counted: .03,
  "Annet", "Evaluering av emnet", "Miscellaneous" (none plan text).
- **uit — not ready.** Fix 1. `coursework` .62 < .70: of the 25 sample
  plans without a coursework section, 2 have a gate sentence in
  `assessment` (`uit_GEO-2007-1_2008_autumn_1`,
  `uit_FRI-2580-1_2019_autumn_1`) and 23 have no coursework text on the
  page (mostly 2008, a known limit). With fix 1 the share would be about
  .65; the rest is on the site.

All three fixes are rebuilt from the raw store and need no new harvest.

### Most frequent unmapped headings

Unchanged from 2026-10-21 for hiof and uit (facts box, programme lists,
course evaluation, contact, year picker, admission; none should map). usn:
Annet, Evaluering av emnet, Miscellaneous.
