# Sections audit, pilots of 2026-11-21: DMMH, LDH, Barratt Due (AHO open)

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

## Verdict

Gate numbers of this check (build of 2026-11-21; `PILOT_GATE`: assessment
0.90, coursework 0.70, unmapped_headings at most 0.65):

| Institution | assessment | coursework | no_sections | unmapped_headings | Verdict |
|---|---|---|---|---|---|
| dmmh | 1.00 (n 69) | 0.93 (69) | 0 | 0.50 (n 2262) | **not ready**: batch D (high); E, F medium. All rebuild-only |
| aho | 1.00 (n 68) | **0.28** (68) | 0 | 0.275 (n 356) | **open**: no review yet; coursework 0.28 unexplained |
| ldh | 1.00 (n 25) | **0.40** (25) | 0 | 0 (n 20) | **not ready**: batch B (high, rebuild-only) and A (needs the harvest) |
| barrattdue | 1.00 (n 46) | 1.00 (46) | 0 | 0.105 (n 526) | **not ready**: batches B, C (high); E medium. All rebuild-only |
| onh | – | – | – | – | **open**: waits for a DBH snapshot with autumn 2026 |

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
