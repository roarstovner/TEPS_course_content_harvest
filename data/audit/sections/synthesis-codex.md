# Sections audit, build of 2026-11-05: ntnu, uia, hivolda, uit, nord, phs, fhs

Run 2026-11-05 after the build of 17:52 (`plan_sections.RDS` 17:40). Review
agents: Codex `gpt-6.1-sol`, one per institution, packets of 14 suspects + up
to 6 random controls, plus an attendance appendix per packet (below).
Findings: `findings-codex/`, report `findings-codex_report.md`. NTNU and UiB
harvests were running; only light R reads were done.

The reviewers rate almost everything `high`. The severities below are
re-rated after checking the packets and `data/processed/plan_sections.RDS`.

## Summary

- **The widened attendance rule moves only attendance requirements.** Of 117
  distinct moved texts the reviewers judged (one per text, covering ~2,240
  sentences in ~2,120 plans), none is a negation, optional attendance,
  practice-placement attendance or a description of teaching. 3 NTNU texts
  (14 plans) move a sentence whose antecedent stays behind ("Det er krav om
  80 % oppmøte på disse seminarene."). 2 hivolda plans keep the moved
  sentence in teaching_methods too.
- **The PHS fixes hold** (attendance and feedback on arbeidskrav in
  coursework, workload out, learning-outcomes intro in course content;
  no sampled portfolio sentence). **The FHS heading fixes hold**
  (prerequisites, teaching methods).
- **High, new:** 421 NTNU plans (2019-2025) have a course_content that says
  only "Se engelsk versjon" (and variants): the `english_page` notice in
  `R/institution_config.R` matches "see english version" and "se engelsk
  (beskrivelse|tekst)" only, so the English plan is not fetched and the pointer
  is emitted as sections (1,491 pointer rows in 452 plans).
- **High:** attendance requirements the rule does not reach stay in
  teaching_methods and assessment, in many plans at ntnu, uia, uit, nord and
  hivolda ("Det er derfor obligatorisk oppmøte i undervisningen.",
  "Undervisningen er obligatorisk, og krever minimum 80 % oppmøte.",
  "Det vil derfor være krav om obligatorisk deltakelse i undervisningen.").
- **High:** nord's unmapped `Adgangsregulering` block is dropped, and with it
  prior-course requirements ("Bestått 90 stp. i masterfaget og bestått
  FoU-oppgave."): ~122 plans lose them from prerequisites.

## Attendance appendix

`tmp/attendance_appendix.R` took the sentences in coursework_requirements
that the new `.attendance_sentence_regex` matches and the old one (before
ff88f97) does not, and appended to each packet up to 25 distinct texts
(those with ikke/not/valgfri/anbefalt/praksis/kan/dersom first, then the most
frequent, then random) with plan context. Counts are an upper bound on the
moves (some were under a coursework heading already).

| inst | moved sentences | plans | distinct texts | reviewed | wrong |
|---|---|---|---|---|---|
| ntnu | 774 | 665 | 100 | 25 | 2 (antecedent lost; 3 texts, 14 plans in all) |
| hivolda | 749 | 739 | 94 | 21 | 0 (2 reported cut at "jf." rejected) |
| uit | 282 | 282 | 68 | 18 | 0 |
| uia | 248 | 241 | 60 | 20 | 0 |
| nord | 107 | 105 | 32 | 18 | 0 |
| phs | 83 | 81 | 23 | 13 | 0 |
| fhs | 5 | 5 | 2 | 2 | 0 |

Own check of all distinct texts: all are compulsory attendance (incl. NTNU's
"Det er obligatorisk oppmøte første forelesning.", 266 plans, which is an
attendance rule; and "Det er obligatorisk deltakelse på samlinger, med inntil
20% fravær."). No negation and no praksis sentence moved.

## Cross-institution patterns

1. **Attendance requirements left outside coursework** (ntnu, uia, uit, nord,
   hivolda, fhs, phs). A crude sentence count (compulsory/krav + attendance
   word, no negation, no praksis) finds such sentences in teaching_methods in
   ~1,470 ntnu, ~700 uia, ~650 uit, ~600 nord, ~360 hivolda plans, and in
   assessment in ~730 ntnu, ~540 uit, ~240 nord plans (some are descriptive:
   "krever aktiv deltakelse"). Root: `.attendance_sentence_regex` needs "Det
   er" first or a %/"alle"; it misses "Det er derfor obligatorisk oppmøte",
   "Undervisningen er obligatorisk, og krever minimum 80 % oppmøte", "Det vil
   derfor være krav om ...", "80% obligatorisk oppmøte.", English "Attendance
   to lectures and seminars is mandatory.", sentences over 120 chars
   (hivolda "..., jamfør Forskrift ..."), and a following absence sanction
   (phs_ETF-MISHAN-1_2024_autumn_1 "Dersom fravær overstiger 20 prosent, mister
   studenten retten ..."). Examples: ntnu_HBIOA2002-1_2021_autumn_1,
   uia_PED420-1_2021_autumn_1, uit_JUR-3901-1_2012_spring_1,
   fhs_FHS8004-1_2022_autumn_1.
2. **Pointer and placeholder sections** (ntnu high, uit, hivolda low): ntnu
   "Se engelsk versjon" (above); uit "Contact the faculty.", "Se fagbeskrivelse
   med pensum"; hivolda "Sjå engelsk versjon" (2 plans).
3. **Admission and progression text in the wrong section** (nord, uit, uia,
   fhs, hivolda): nord Adgangsregulering (lost prior-course requirements);
   uit law courses keep progression rules in assessment and admission text
   in course_content ("Kan ikke tas som enkeltemne."); hivolda Didaktikk 2
   files "Bestått didaktikkemne 1." as coursework; uia
   uia_FIL908N-1_2025_spring_1 keeps "I tillegg kreves dokumentasjon av minimum
   to års yrkespraksis." as prerequisites.
4. **Heading containers override meaning** (fhs, nord, ntnu, hivolda):
   teaching arrangements in course_content, assessment design in
   teaching_methods (ntnu_MOL3901-2_2022_autumn_1,
   nord_PRA5015-1_2024_autumn_3).
5. **Workload in teaching_methods**: uia 3,187 plans ("Forventet
   arbeidsomfang i emnet er omtrent 267 timer."), ntnu 292, uit 278; the spec
   puts workload outside the sections. The PHS workload rule is PHS-only.
6. **Resit and examiner logistics in assessment** (uit, ntnu, nord, fhs): the
   spec's known-deviation class.

## Prioritised fix list

1. **ntnu english_page notice** (`R/institution_config.R`, ntnu
   `english_page$notice`; `.placeholder_phrases` in `R/extract_sections.R`):
   add "se (den )?engelske? (versjon(en)?|emnebeskrivelse|beskrivelsen)",
   "Emnet undervises på engelsk. Se engelsk ...". 421 plans; needs English
   page requests (harvest retry for those offerings), best before NTNU's
   running harvest goes further. ntnu_TØL4010-1_2019_autumn_1,
   ntnu_IMT4895-1_2025_autumn_1.
2. **Attendance coverage** (`.attendance_sentence_regex`, the absence
   sanction as a gate in `.split_inline_coursework()`): the variants above.
   Check with `preview_change()` that no negation ("Det vil ikke være noe
   obligatorisk oppmøte", uit, 27 plans) or praksis sentence moves.
3. **nord Adgangsregulering** (`R/section_heading_map.R`, or the
   admission-vs-prerequisite split): keep the prior-course sentences in
   prerequisites, drop admission. nord_MAT5006-1_2022_spring_2,
   nord_NAT5003-1_2022_spring_2, nord_NO204L-1_2019_autumn_1.
4. Medium: workload sentences out of teaching_methods (uia, ntnu, uit);
   uit placeholder rows; uit/hivolda admission and progression text; fhs
   unlabelled prior-course text in course_content.
5. uia anonymizer over-cut: course_plan of uia_HEL611-1_2020/2021 and
   uia_ST-301-1_2025_autumn_1 holds only title and credits while sections
   hold the plan (3 plans in all 7 institutions); reviewer suspects `.anon_uia`
   staff removal running past a heading. Fulltext/anonymization audit item.

## Rejected findings

- hivolda "ATT 11/15 cut at jf.": an artefact of the appendix's sentence
  split; the extracted coursework keeps the whole sentence ("..., jf. Forskrift
  om opptak, studium og eksamen ved Høgskulen i Volda, §4-6.") and teaching
  keeps nothing of it (hivolda_NO111S-1_2020_autumn_1). Deleted from the JSON.
- The remaining ✗ (ntnu, appendix ids not in sample.csv) is real and kept,
  re-rated low.
- uit exam-language block dropped: `Undervisnings- og eksamensspråk` → .drop
  is deliberate (facts-box language, closed 2026-10-31); a sentence that sets
  the answer language ("Studenten kan svare på eksamen på et skandinavisk
  språk eller på finsk.") could go to assessment: a spec decision, low.
