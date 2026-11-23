# Sections audit, pilots of 2026-11-21 (DMMH, LDH, Barratt Due) and re-audit of 2026-11-22 (with AHO)

Run 2026-11-21 on the build of 2026-11-21 (10:21-20:21, commit d77a254),
`--pilot` samples. Review agents: Codex `gpt-6.1-sol`, one per institution.
Findings: `findings-codex/{dmmh,ldh,barrattdue}.json`, report
`findings-codex_report.md`. All 15 findings of the three passed mechanical
verification (the two ✗ rows in the report are stale ntnu and himolde
reports from earlier runs).

- **AHO**: its reviewer was stopped before it wrote a file (the audit was
  briefly switched to a newer model, then kept on gpt-6.1-sol without a
  rerun, user 2026-11-21). AHO's sections verdict waits for the next audit.
- **Oslo Nye Høyskole**: no packet. Its pilot pages are of 2026/27 and the
  DBH snapshot of 2026-11-08 has its spring 2026 offerings only, so the
  build gives them no plan; the audit waits for a snapshot with autumn 2026.

Each finding was checked against its packet and the code
(`R/extract_sections.R`, `R/blocks.R`, `R/section_heading_map.R`, the
`section_*` fields in `R/institution_config.R`): **15 real** (8 fix batches
below), **0 rejected**. Severities are re-rated where noted. Every fix but
one is rebuild-only; the one that needs a harvest (LDH's PDF split, batch A)
is filed under the fulltext audit, whose finding it is.

## Summary

- **Barratt Due, high: the course description lands in teaching_methods.**
  The site's column "Undervisning og læringsformer" holds the course's
  description ("Emnet grunnleggende skuespillerteknikk skal gi studenten
  ... Emnet inneholder skuespillerteknikk og karakterarbeid."); teaching
  methods are under "Organisering og arbeidsformer". Only 3 of 20 sampled
  plans have a course_content section (the three with an "Innhold" heading).
- **Barratt Due and LDH, high: prerequisites in a facts block, read by no
  section.** Barratt Due's "Krav til forkunnskaper: Bestått HIST11 ..." is in
  the facts column "Kort om emnet" (12 of 20 plans name required courses; no
  plan has a prerequisites section); LDH's PDF plans list "Krav til
  forkunnskaper: BSY-100, ..." in the metadata lines that
  `section_text_header` strips (5 of 20).
- **Barratt Due, high: DID1's four sub-courses lost.** "Delemner" and the
  sub-course titles are unmapped headings, so their descriptions are in no
  section.
- **DMMH, high: coursework gates left in assessment** in 5 of 17 plans (PL1/PF2
  requirements before placement, "Arbeidskrav som være bestått for å få gå
  opp til eksamen:", equivalence blocks); DMMH has no
  `section_inline_coursework`.
- **Medium, at both new catalogue institutions: stock logistics in
  assessment.** "Sensorordning: Intern og ekstern" in every DMMH assessment;
  examiner, resit and deadline sentences in all 20 Barratt Due assessments.
  Neither has entries in `.logistics_sentences` (as NMH and KHiO had none
  before 2026-11-16).
- **LDH**: the BIS23 plan's last course (BSYP-503) takes in the programme
  appendix; PDF footers and page numbers in sections; placement
  arrangements under "Innhold".

## Cross-institution patterns

| Pattern | Institutions | Root cause (checked) | Batch |
|---|---|---|---|
| Prerequisites in a facts or metadata block | barrattdue, ldh (as nmh before 2026-11-16) | `sectionize()` drops `facts` blocks (`R/extract_sections.R`, line 50); Barratt Due's whole "Kort om emnet" column is `facts`; LDH's `section_text_header` strips "Krav til forkunnskaper" with the PDF's metadata lines. NMH's fix is the model: `section_label_selector` with "krav til forkunnskaper" → `.admission`. DMMH already keeps Forkunnskapskrav out of `facts` | B |
| A heading whose text does not say what is under it | barrattdue ("Undervisning og læringsformer" = description), ldh ("Innhold" of placement courses = placement arrangements) | heading-based assignment; Barratt Due's is site-wide (an institution `section_headings` entry), LDH's recurs over the BSYP placement courses of every cohort | C, G |
| Stock admin sentences with no institution entry | dmmh, barrattdue | no `dmmh` or `barrattdue` in `.logistics_sentences`, `.logistics_blocks`; the `sensorordning` heading map row is `.drop` but DMMH's narrow table gives "Sensorordning:" as a row, not a heading; `.progression_regex` needs "Eksamen må være bestått for at studenten skal kunne fortsette" | E |
| Placeholder wording not in the list | dmmh, ldh | `.placeholder_phrases` lacks "Se avsnittet «…» i studieplanens generelle del." (8 of 17 DMMH plans; as course_content, learning_outcomes and teaching_methods in VUBHP6000) and "I emnet inngår ingen obligatorisk aktivitet." | F |

## Prioritised fix list

A. **LDH PDF split (needs a harvest; filed with the fulltext audit).**
   `ldh_pdf_courses()` (`R/harvest_strategies.R`) ends a course at an
   upper-case programme heading; BIS23's mixed-case "Studiets overordnede
   læringsutbytter" is not one, so BSYP-503's assessment runs on for 10,537
   characters (`ldh_BSYP-503-1_2025_autumn_1`; the 2022 offering is
   correct). The same function leaves standalone page numbers and
   "<programme> H2025. Sist oppdatert dd.mm.yy" footers (MALSY-100,
   MISY-100, MOPSY-100). Both are in the harvest's text, which the build
   does not re-split (`read_harvest()` drops the PDF bytes).

B. **Prerequisites from labelled facts (high; barrattdue, ldh).**
   Barratt Due: read the "Kort om emnet" column as labelled fields
   (`section_label_selector`, "krav til forkunnskaper" → `.admission`; the
   others map nowhere) instead of `facts`. LDH: the same for its HTML facts
   card, and in the text reader let a "Krav til forkunnskaper:" line open
   prerequisites instead of stripping it with the header. Examples
   `barrattdue_HIST2100-1_2026_autumn_1`, `barrattdue_KAM4200-1_2026_autumn_1`,
   `ldh_BSYP-501-1_2022_autumn_2`, `ldh_BSYP-300-1_2025_spring_3`. Effect:
   prerequisites for ~60% of Barratt Due plans and the LDH placement
   courses; the admission filter keeps "Bestått opptaksprøve ..." out.

C. **Barratt Due headings (high).** `section_headings`: "undervisning og
   læringsformer" → course_content (check the 3 sampled pages that also have
   "Innhold", e.g. `barrattdue_ENT4100-1_2026_autumn_1`, whose Innhold mixes
   teaching in); "delemner" → course_content, with the sub-course titles as
   sub-headings (`barrattdue_DID1-1_2026_autumn_1`). Effect: course_content
   for nearly every plan.

D. **DMMH inline coursework (high).** `section_inline_coursework = TRUE`,
   and check that `.split_inline_coursework()` reads DMMH's gate leads
   ("Arbeidskrav som [må] være bestått for å få gå opp til eksamen:"),
   placement-start conditions and the "OPG1: F-TF1 og F-TF4" row after an
   intervening paragraph. Examples `dmmh_BKKKK1013-1_2022_autumn_1`,
   `dmmh_BLPRA100-1_2025_autumn_1`, `dmmh_BHFOR3561-1_2025_autumn_1`.

E. **Stock logistics (medium; dmmh, barrattdue).** Entries in
   `.logistics_sentences` / `.logistics_blocks`: DMMH's "Sensorordning:" row,
   examiner roles and resubmission deadlines (`dmmh_MVPLK6000-1_2025_autumn_1`);
   Barratt Due's examiner and resit sentences and dated deadlines ("Frist for
   innlevering av årsrapport 15.mai", `barrattdue_HINS1300-1_2026_autumn_1`).
   Barratt Due's "Gjennomført undervisning og arbeidskrav er gyldig inntil to
   semestre etter ordinær eksamen." (5 plans, under "Organisering og
   arbeidsformer") moves to coursework_requirements; "Eksamen må være
   bestått for at studenten skal kunne fortsette PPU-studiet 2. år." joins
   `.progression_regex` (`barrattdue_DID1-1_2026_autumn_1`, PED1).

F. **Placeholders (medium/low; dmmh, ldh).** The two wordings above in
   `.placeholder_phrases` (`dmmh_VUBHP6000-1_2025_autumn_1`,
   `ldh_BSY-221-1_2025_spring_2`).

G. **LDH placement arrangements (medium, re-rated from high).** "Emnet
   omfatter praksisforberedende studier ..., hvorav normalt 8 uker (240
   timer) er tilstedeværelse i klinisk kontekst." stays in course_content in
   7 BSYP offerings with no teaching_methods row. The text is kept, and for
   a placement course it is partly what the course is; the wording recurs
   over the BSYP courses of each cohort, so a move by meaning fits the rule
   of 2026-11-11 if the fix keeps the course's subject in course_content
   (`ldh_BSYP-200-1_2025_autumn_3`).

H. **DMMH prerequisites of a practice course (low, re-rated from medium).**
   BLPRA100's Forkunnskapskrav holds only an attendance and placement-access
   rule. One course in the sample: a known deviation (a mixed paragraph under
   a mapped heading) unless the fix finds the wording in other DMMH
   practice courses.

## Changes since the previous run

The three institutions are new; the report's comparison with `HEAD` has no
earlier rows for them.

## Rejected findings

None. The reviewers' own rejections, kept: GRUSKU5100's short reading list
("Deles ut av faglærer") and VERK2100's long one are real; DMMH's
duplicate/short flags on pointers are placeholders (batch F), not copied
content; VIB-101's coursework "Praksisstudier" is a real requirement.

## Re-audit 2026-11-22 (after the fixes)

Batches B-F and H were fixed and rebuilt (build of 2026-11-22, 00:18-10:40);
G stays as a known deviation; A is in the harvest code and takes effect
with LDH's full harvest. Re-run with the newest Codex model, `gpt-6.2-sol`
(user 2026-11-21: new audits use the newest Sol model), on fresh packets of
the same seed: dmmh, ldh, barrattdue and, for the first time, aho
(`findings-codex/{inst}.json`; the 2026-11-21 findings are the baseline in
git). All 31 findings passed mechanical verification. gpt-6.2-sol reads
more strictly than gpt-6.1-sol, so new findings are partly the model's,
not new defects.

- **Fixed**: DMMH's coursework gates in assessment (the reviewer finds the
  gates in coursework_requirements and the graded practical exams in
  assessment); Barratt Due's description now course_content, DID's
  sub-courses kept; LDH's prerequisites ("The 20 plans retain substantive
  prerequisites"); the placeholders; DMMH's "Sensorordning:" rows.
- **DMMH, left** (no confirmed high after re-rating): approval-only
  sentences of placement reports and self-evaluations in assessment or
  teaching_methods ("Praksisrapport (praksisrapport vurderes til
  godkjent/ikke godkjent).", `dmmh_BNPRA200-1_2019_spring_1`; medium: the
  report's evaluation is part of how the placement is assessed, and the
  requirement is in coursework too); one-off mixed paragraphs under a
  mapped heading ("Prosjektskissen er fagets arbeidskrav." in course
  content, `dmmh_MLVFM5020-1_2019_autumn_1`; known deviation); one-off
  scheduling ("Oppgaven gis ut den første dagen, ..."; known deviation);
  a reading-list pointer "Her finner du pensumliste for studieåret" (low).
- **LDH, left**: BSYP-503's appendix and the PDF footers (fixed in the
  harvest code; gone after the full harvest); the placement sentence (known
  deviation, 2026-11-21); "Praksisstudier med godkjent arbeidskrav vurderes
  til bestått/ikke bestått." names the requirement in the assessment
  sentence (medium; one sentence, design and gate together); goals under
  "Hensikt" in course_content (the aims-as-outcomes known deviation);
  MALSY-100's minimum of ten examinations in teaching_methods (medium).
- **Barratt Due, left**: prerequisites still missing in 3 of 20: ENT2100
  and SPIN5110 ("Gjennomført 1.studieår bachelor i utøvende musikk.", a
  form `.real_prerequisite_regex` lacks) and KAM4300 (the page gives
  "Bestått KAM3300 ..." with no label); the validity sentence in its plural
  form ("... er gyldige i to semestre ...", 2 plans); DID's "Obligatorisk
  kurs: instrumentalteknikk og kroppsbevissthet" in course_content; "Avsluttende
  vurdering: Mappevurdering" in the facts column, not in assessment
  (GRUSKU5100, HINS1200); elective admission and capacity rules in course
  content (KAMFOR5100, SPIN5110); the HINS/KAM activity lists in course
  content (the known deviation of 2026-11-21, a decision for the user);
  resit rules that say what the resit is (kept on purpose).
- **AHO** (first review): high, prerequisites lost under the unmapped
  access headings "Generell adgangsinformasjon for emnet" / "Admission
  requirements" ("Bestått alle emner i GK1 og GK2. Bestått studiodelen av
  GK3 og GK4.", 6 controls; `aho_80 162-1_2025_spring_1`,
  `aho_61 131-1_2025_autumn_3`); high, coursework left in assessment under
  an "Obligatorisk arbeidskrav" sub-heading or as attendance gates
  (`aho_61 121-1_2025_spring_1`, `aho_60 541-1_2025_autumn_2`,
  `aho_70 154-1_2025_autumn_3`): AHO has no `section_inline_coursework`,
  which explains coursework 0.28; medium, Design Studio's track structure
  (a track's "LEARNING OUTCOMES" line inside course_content,
  `aho_70 505-1_2025_autumn_3`); medium, stock logistics ("Besvarelsen
  leveres i Wiseflow. Du må gjøre deg kjent med innlogging ..."), assignment
  lists left in teaching_methods, a sentence in both course_content and
  teaching_methods (PHD-READING); low, a Leganto pointer ("Pensumlisten
  publiseres i Leganto."), material costs.

## Re-audit 2026-11-23 (AHO and Barratt Due, after the fixes)

The fixes for AHO's access field and exam description and for Barratt
Due's prerequisite and final-assessment fields were rebuilt in the build
of 2026-11-23 (08:36-18:59, with both full harvests). Re-run on
`gpt-6.2-sol`, fresh packets of the same seed with `--pilot` (the sample's
courses; AHO's suspects changed with the fixes, so 7 of its 20 plans are
new); the 2026-11-22 findings are the baseline in git. 14 findings, all
verified.

- **AHO, fixed**: prerequisites under the access headings (the sample has
  21 plans with prerequisites, 1 before; the reviewer finds one clause
  missing in one plan); coursework under the exam description's gate
  heading; the stock Wiseflow notices; the Leganto pointer.
- **AHO, left**: coursework gates written in the teaching text or under
  an "Attendance:" label, not under a gate heading ("Students are required
  to attend at least 80% of the main course events", "Final reviews are
  mandatory", "Alle oppgaver må være levert for å bli sensurert i kurset",
  "The student must submit all assignments in the course to receive final
  assessment.", `aho_70 509-1_2025_autumn_3`, high): of the sample's 42
  plans without a coursework section, 20 mention attendance or submission,
  some as expectations only ("Fysisk oppmøte til undervisning ... er
  forventet og nødvendig for å oppfylle læringsutbytte.", a stock wording),
  which is no gate; this is coursework 0.37. One-plan highs: Design
  Studio's tracks carry their own "LEARNING OUTCOMES" / "EVALUATION"
  lines inside course_content (`aho_70 505-1_2025_autumn_3`); Interaction
  Design 2 loses "have taken Interaction Design 1" for international
  students in a sentence that also gives admission priorities
  (`aho_DES4016-1_2025_autumn_3`). Teaching sentences in course
  descriptions (VALG3, "Kurset er lagt opp som et verkstedkurs ...") are
  the known deviation of mixed paragraphs under a mapped heading. Medium:
  "i Wiseflow" inside the exam-form sentence, examiner notices; low:
  material costs, a sentence in both course_content and teaching_methods.
  The PhD reading course's assignment list stays in teaching_methods
  (medium); its dates go with the anonymizer fix of 2026-11-23.
- **Barratt Due, fixed**: the prerequisites of ENT2100, SPIN5110 and
  KAM4300 (missing in 3 of 20, now 1); the validity sentence's plural;
  DID's "Obligatorisk kurs"; "Avsluttende vurdering" in assessment.
- **Barratt Due, left**: DID1's prerequisite field "Bachelorgrad i musikk
  og Opptak til Praktisk-pedagogisk utdanning" is dropped as admission,
  though it names a degree (high, one plan); HIST1100's coursework field
  opens with three sentences on what the writing module teaches (high, one
  plan; mixed paragraph under a mapped heading, known deviation); the PPU
  plans' resit policy "Den delen som ikke er bestått må gjennomføres på
  nytt. Deretter gis det ny samlet vurdering." (medium, 3 plans, a stock
  wording); elective admission and capacity rules in course content
  (KAMFOR5100, SPIN5110; medium, as before); the HINS/KAM activity lists in
  course content (high as rated, but the user kept them on 2026-11-23: the
  known deviation); ALEX5100's assessment names its parts while their
  lengths are in the coursework field (medium; the page's own layout).

## Verdict

Gate numbers of this check (build of 2026-11-22 for DMMH and LDH, of
2026-11-23 for AHO and Barratt Due; `PILOT_GATE`: assessment 0.90,
coursework 0.70, unmapped_headings at most 0.65):

| Institution | assessment | coursework | no_sections | unmapped_headings | Verdict |
|---|---|---|---|---|---|
| dmmh | 1.00 (n 69) | 0.94 (69) | 0 | 0.50 (n 2262) | **ready for full harvest**: medium and low left, rebuild-only |
| aho | 1.00 (n 68) | **0.37** (68) | 0 | 0.22 (n 330) | **ready with known limits** (full harvest done): coursework gates in the teaching text (high, rebuild-only); one-plan highs left |
| ldh | 1.00 (n 25) | **0.36** (25) | 0 | 0 (n 20) | **ready with known limits**: coursework 0.36 (the site's PDFs, below); the appendix and footers go with the full harvest |
| barrattdue | 1.00 (n 46) | 1.00 (46) | 0 | 0.098 (n 471) | **ready** (full harvest done): DID1's prerequisites (one plan) and medium left, rebuild-only |
| onh | – | – | – | – | **open**: waits for a DBH snapshot with autumn 2026 |

The verdicts of 2026-11-21 (all three not ready, AHO open) and of
2026-11-22 (AHO and Barratt Due not ready) are in git history. The notes below are from that run and still hold (LDH's coursework
is 0.36 now: the "ingen obligatorisk aktivitet" row no longer counts).

- **LDH coursework 0.40** has a cause on the site: the bachelor study-plan
  PDFs (BIS20-23) have no coursework block; their placement courses name the
  requirement only in the assessment sentence ("Praksisstudier med godkjent
  arbeidskrav vurderes til bestått/ikke bestått.", 4 of the 8 plans in the
  sample that mention coursework without a section; the others are the same
  sentence or an assessment part, `ldh_BSY-102-1_2025_autumn_1`). The
  reviewer found no coursework text lost.
- **DMMH unmapped headings** (0.50, under the threshold) are the facts
  list's labels (Studienivå, Studiepoeng, Undervisningssemester, ...) and
  "Øvrig informasjon" (course-replacement notes, plan text without a
  section).
- **Barratt Due unmapped headings**: "Mer om emnet" (the h2 over the
  accordions, no text of its own), "Delemner" and the sub-course titles
  (batch C).
- No plan in the three samples has no sections (`no_sections` 0).

DMMH and Barratt Due need no new harvest for their section fixes: the full
harvest would store the same pages. LDH's batch A changes the harvest's
text and should come before LDH's full harvest.
