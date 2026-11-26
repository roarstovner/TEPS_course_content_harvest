# Sections audit, close-out of five added institutions (2026-11-25)

Run 2026-11-25 on the build of 2026-11-25 (finished 05:15) for Fjellhaug,
KHiO, NHH, NMH and HiMolde, whose full harvests are done. Not `--pilot`: the
pilot samples of the three current-plan sites (Fjellhaug, KHiO, NHH) are of
2025, which has no plan, so all five packets sample the whole build
(`prepare_sections.R ... --tag closeout`, packets `packets/closeout/`).
Review agents: Codex `gpt-6.2-sol`, one per institution. Findings:
`findings-closeout/{inst}.json`, report `findings-closeout_report.md`. All 38
findings passed mechanical verification; each high was then checked against
its packet, and the Fjellhaug headings against the whole build (below).

First sections audit of Fjellhaug and NHH (NHH's pilot had fulltext and
anonymization only); re-audit of KHiO and NMH after the fixes of 2026-11-16
to the 2026-11-15 findings, and of HiMolde after its pilot fixes
(2026-11-06) and the bold "Arbeidskrav I" sub-heading fix.

## Gate numbers (whole build, read 2026-11-25)

`gate_numbers()` on every offering of the harvested years (the current-plan
sites: 2026), with the build's processed files. Sections judges assessment,
coursework, no_sections and unmapped_headings.

| inst | n plans | assessment (≥ .90) | coursework (≥ .70) | no_sections | unmapped_headings (≤ .65) |
|---|---|---|---|---|---|
| fjellhaug | 212 | **.726** | **.613** | 0 | .123 (n 2,246) |
| khio | 347 | .997 | .974 | 0 | .000 (n 1,690) |
| nhh | 432 | .975 | .792 | 0 | .323 (n 5,344) |
| nmh | 2,842 | 1.000 | .961 | 0 | .003 (n 30,271) |
| himolde | 2,716 | .958 | **.662** | 0 | .156 (n 36,801) |

- Fjellhaug, assessment and coursework: the unmapped headings explain them
  (finding F1). Most frequent unmapped headings on its 213 pages of 2026:
  "Studiepoengreduksjon" 155 (credit overlap, not plan text), "Exam and
  Assessment" 48, "Attendance and absence" 32, "Oppmøte og fravær" 27 (+3
  with "(gjelder samlingsbasert BTM)"), "Politiattest" 6, "Øvrige
  bestemmelser" 2, "Organisering og arbeidmåter", "Organisering og
  arbeidsmåte", "Organisering og undervisning" 1 each.
- HiMolde, coursework .662: as in the pilot (.625), most plans have no
  coursework text (pilot reading, 2026-11-06); SYD204 is an extractor miss
  (H1).
- NHH unmapped .323: the semester heading ("Høst 2026"), "Oppsummering",
  "Inngår i studiene", "Computer tools"/"Dataverktøy", "Restricted
  access"/"Adgangsbegrensing", "Retake"/"Gjentaksordning": facts and
  logistics, not section text.
- NMH top unmapped (0.3 %): "Masteroppgave", lettered component lines "A)
  Didaktikk ...", "Kort om emne", "Nødvendig utstyr", "Conference Practices".

## Verified findings by institution

### Fjellhaug: not ready (rebuild-only fixes)

- **F1, high (found in the whole build, not by the reviewer): "Exam and
  Assessment" is unmapped, so all 48 plans with the English page layout (BMM 38, TMM 5, EX 4, GRE 1) have no
  assessment and no coursework section**; the text under it ("Course
  Requirements:", "Final Assessment:", the exam table) is dropped
  (BMM1003). "Oppmøte og fravær" / "Attendance and absence" are unmapped
  too: 34 of the 67 plans with one have no coursework section; PT1501's
  attendance conditions ("For å kunne få sluttvurdering i emnet må studenten
  ha fullført følgende krav") are lost (reviewer's finding). Fix:
  `R/section_heading_map.R`: "exam and assessment" → assessment, "oppmøte og
  fravær" (with its bracket), "attendance and absence" →
  coursework_requirements, the three "Organisering og ..." variants →
  teaching_methods; then check "Course Requirements:" / "Final Assessment:"
  split the English block.
- **F2, high: graded assessment filed under coursework_requirements** (7 of
  17 sampled). The exam table ("Vurderingsform Varighet Varighetstype ...",
  PT2005, KRLE1001N, KRLE1003N) and the "Sluttvurdering" line or prose
  (BTM1002, BTM2010, PT1501) come after the Arbeidskrav list inside the
  coursework block and stay there; BTM1002 and PT1501 keep no assessment.
  Fix: a Fjellhaug boundary in the coursework block at "Sluttvurdering" and
  at the exam table's header row (`section_*` fields / `R/blocks.R`, or a
  sub-heading pattern in the heading map).
- F3, medium (re-rated from high: one-course wordings, known deviation
  "paragraph that mixes meanings"): coursework sentences under "Organisering
  og arbeidsmåter" ("Som et arbeidskrav skal studentene i emnet delta ved en
  apologetikkonferanse.", PT2005; draft submission, BTM2006).
- F4, medium: the exam-aid label "Hjelpemiddeliste ved eksamen" /
  "Hjelpemiddelliste" is dropped, leaving "Liste A", "Ingen" bare in
  assessment (BTM1002, BTM2501, KRLE1001N); the spec keeps permitted aids
  with their heading. Fix: label pattern in `R/section_heading_map.R`.
- F5, medium: GRE1002's "Tekster" sub-heading (assigned passages, page
  counts) stays in course_content, not reading_list.
- Low: "Ingen" appended to a prerequisite (PT1501); the stock sentence "Det
  er faglærer som bestemmer innhold og tidsfrister for gjennomføring av
  arbeidskrav." left in assessment (BTM2503-DK1, HEB1501); the stock resit
  notice "Ved stryk får studentene anledning til å ta ny eksamen i perioden
  for ny/utsatt eksamen." (BTM2001, BTM2006, BTM2009); the "Les mer" button
  label (GRE1002; also a fulltext finding).

### KHiO: ready with known limits

The 2026-11-15 highs are fixed: DA502's English gate is in
coursework_requirements and its repeated outcomes are gone from
teaching_methods (packet course 15). Remaining, all re-rated:

- IM210 ("... dokumenteres i en egnet skriftlig og visuell presentasjon som
  grunnlag for vurdering.") in course_content; IM209, IM210, DA502 teaching
  arrangements in course_content: known deviation (mixed paragraph, wording
  in one course). Re-rated from high/medium to low.
- MK522: "Deltakelse på teoriforelesninger, skrivepraksis-workshops og
  veiledninger er obligatorisk." stays in teaching_methods, its
  coursework_requirements has "aktiv deltakelse": medium (re-rated from
  high; one course).
- PPU800: formative-assessment paragraph under "Vurdering" with the final
  assessment: low, it is the plan's own assessment heading.
- Examiner details inside design sentences (MK522, RE103): known deviation
  (stock wordings at KHiO since 2026-11-16).

### NHH: ready with known limits (medium fixes)

- N1, medium (re-rated from high: a rule, not design): "Compulsory activities
  (work requirements) is valid for one semester after the semester it was
  obtained." in assessment (BEA512, where it repeats; ECS530). A stock
  NHH sentence: coursework-validity/resit rule, to coursework or out.
- N2, medium: assessment logistics, no NHH rules yet ("The assignment will
  be distributed in January. The submission deadline will be in April.",
  ECS509, ECS530, FIN539B).
- N3, medium: the regulations paragraph after the permitted aids ("All in
  accordance with Supplementary provisions to the Regulations ...", ECO422,
  BED2, BUS446).
- N4, medium (widespread): labels "Permitted Support Material", "Grading
  Scale", "Vurderingsuttrykk" dropped, leaving "Calculator", "A-F" bare
  (ECO422, ENE423, FSK504). Fix: `section_heading_patterns$label` in
  `R/section_heading_map.R`.
- N5, low: teaching passages under "Topics" (EBO4, ECN435; MAB4 with its own
  "Pedagogical Approach:" line): known deviation, except MAB4's explicit
  sub-heading.

### NMH: not ready on two highs (rebuild-only)

The 2026-11-15 findings (English headings, "D: Støtteemner", summary
fields, footer and stock sentences) are fixed. New:

- **M1, high: body headings read as text, so a section runs on.**
  KUNSTHO71B: "Avsluttende vurdering" in the body is not a heading; the
  grading criteria and scale stay in coursework_requirements and assessment
  keeps only the summary line. ALEX20: "Pensumlitteratur" and "Anbefalt
  litteratur" after "Ny vurdering" stay in assessment; no reading_list.
  Neither appears in the page's heading list. Fix: the NMH html reader
  (`R/blocks.R` / `section_*` fields) for these paragraph headings.
- M2, medium (re-rated from high): MPTEO73 2021 "Pensum: Omtrent 1000 sider
  felles pensum som oppgis av faglærer." in the body, not the summary list,
  stays in course_content (the 2026-11-16 fix noted 17 pages with the list
  elsewhere).
- M3, medium (re-rated from high): ENSLED22 "Opptakskrav: 120 stp. ...,
  gjennomført Ensembleledelse I eller tilsvarende ..." in course_content, no
  prerequisites.
- M4, medium (re-rated from high): MUSPED55's two assessment routes ("-
  NMH", "- NTNU") merged without their labels; its gate sentence twice in
  coursework (low).
- Known deviation: mixed prose under correct headings (FOLKDANS20,
  KPKUNST70, KUNSTHO71B outcomes); deadlines and platform lines in
  assessment ("Frist: Innen 10. juni leveres rapporten i NMHs digitale
  eksamenssystem (Inspera).", PEDUT52, MPHO22, MPTEO73) are medium for an
  NMH logistics rule; enrolment sentences in course_content (IMPRO27, CBE,
  FOLKDANS20) medium.

### HiMolde: not ready on one high (rebuild-only)

- **H1, high: SYD204 (2021, 2022, 2023) puts both coursework requirements in
  teaching_methods**; coursework_requirements holds only "Arbeidskrav I".
  The second item's title ("Det nyfødte barnet") is an unmapped sub-heading
  that sends the text back to teaching_methods. A variant of the bold
  "Arbeidskrav I-IV" layout fixed earlier; fix in `sectionize()` /
  `R/blocks.R` (keep a coursework group open across item titles).
- Medium: MHS704 thesis guidance repeated under content and assessment, and
  "Introduksjon" / "Innholdsfortegnelse" sub-headings under "Eksamen" switch
  out of assessment; prerequisites that are admission pointers or "No
  specific recommended previous knowledge" (TRA715, TRA725, IDR208); TRA715
  "Course evaluation" block (unmapped) holds the home-exam statement.
- TRA715, medium (re-rated from high, one course): the make-up rule after
  the attendance threshold stays in teaching_methods.
- Low: empty structured fields ("Andel: %", "Varighet: -"); "Inspera"
  platform suffix; reading-list pointers ("Litteratur oppgis i høgskolens
  læringsplattform ved semesterstart."); a "Pensumoversikt" widget variant
  (SYD204 2022).

## Prioritised fix list (all rebuild-only)

1. Fjellhaug heading map (F1): 48 plans gain assessment and coursework,
   ~34 coursework. Largest effect.
2. Fjellhaug coursework/assessment boundary (F2) and aid labels (F4).
3. NMH paragraph headings in the body (M1).
4. HiMolde coursework items with titles (H1).
5. Assessment labels kept (NHH N4, Fjellhaug F4): the same
   `section_heading_patterns$label` list.
6. NHH stock sentences (N1-N3), NMH logistics and enrolment sentences.

## Rejected findings

None; severities re-rated as marked.

## Verdicts

- Fjellhaug: **not ready** (F1, F2; flagged assessment .726 and coursework
  .613 have a reason, and the fix is rebuild-only).
- KHiO: **ready with known limits**.
- NHH: **ready with known limits** (medium fixes N1-N4).
- NMH: **not ready** (M1), rebuild-only.
- HiMolde: **not ready** (H1), rebuild-only; coursework .662 on the site
  (plans without coursework text) apart from SYD204.

## Re-run of Fjellhaug, NHH and NMH (2026-11-26)

After the section fixes of 2026-11-25/26 (Fjellhaug's English layout,
attendance headings, final assessment after the arbeidskrav; NHH's validity
rule, regulations pointer and logistics; NMH's h3 sub-headings mapped by
whole text; the scripture references in the anonymizer) and the build of
2026-11-26. Same command (`prepare_sections.R fjellhaug nhh nmh --tag
closeout`, fixed seed); the samples differ in part because the data changed.
Codex `gpt-6.2-sol`. 20 findings, all verified mechanically; each high
checked against its packet and, where it could recur, the whole build.

Gate numbers, whole build (Fjellhaug and NHH 2026, NMH 2021-2026):

| inst | n plans | assessment (≥ .90) | coursework (≥ .70) | no_sections | unmapped_headings (≤ .65) |
|---|---|---|---|---|---|
| fjellhaug | 215 | .986 (was .726) | .833 (was .613) | 0 | .075 (was .123; n 2,216) |
| nhh | 432 | .975 | .792 | 0 | .323 (n 5,344) |
| nmh | 2,842 | 1.000 | .961 | 0 | .003 (n 30,988) |

Fixed since 2026-11-25 (gone from the change table): Fjellhaug's coursework
merged with and truncating the final assessment, assessment truncated,
course-content noise; NHH's assessment wrong content and truncation and the
cross-section validity rule; NMH's merged sections under h3 sub-headings,
assessment truncated, coursework duplicates.

Highs kept, with what they are:

- Fjellhaug BTM2506 (control): "Emnet består av totalt 40
  undervisningstimer i form av forelesninger og seminarer med obligatorisk
  oppmøte, ..." moved to coursework by the attendance-sentence rule, while
  the absence limit after it ("... som ikke må ha et fravær på mer enn
  30%") stayed in teaching methods. The rule's exception for a list of
  teaching forms with attendance in it does not catch "i form av ... med
  obligatorisk oppmøte". One plan in the build (the sentence occurs in no
  other Fjellhaug plan): low, a fix item.
- NMH DISTUD78 (2 plans): an h3 "Praksis" inside "Arbeidskrav" maps to
  practice and teaching methods, so the participation requirement under it
  leaves coursework. Only DISTUD78 has "Praksis" under "Arbeidskrav" among
  NMH's 1,241 plans: low.
- NHH RET1: the page repeats "Undervisningsopplegg" inside the outcomes
  field, so its first teaching line stays in learning outcomes (flagged
  leak->teaching_methods). The only NHH plan of 418 with that line: low.

Rejected as findings (the spec keeps the text where the page puts it: a
paragraph that mixes meanings stays with its heading when its wording
occurs in one course, `course_plan_spec.md` "The sections"): Fjellhaug's
course-specific gates under "Organisering og arbeidsmåter" (PT2005's
conference, draft submissions, PT1501's retreat) and the BTM2506 portfolio
instructions under the arbeidskrav; NHH MAB4's pedagogy inside "Topics",
BUS444N's gate and registration text under its prerequisites heading,
RET1's exam aids under "Litteratur"; NMH's admission statement in course
content, MPTEO73's reading note under "Innhold", and teaching details in
NMH's "Kort om emnet" and "Innhold". The coding project segments these
(the harvest delivers the page's words). Logistics left in assessment
(NHH, NMH, Fjellhaug's stock resit sentence) are the extraneous text the
spec accepts there (user 2026-11-11). Placeholders: Fjellhaug PT1501's
"Ingen" after a real prerequisite, NHH SOL23's pointer and the TEST page.

Verdict (sections): Fjellhaug, NHH and NMH ready. No gate number of this
check is flagged, and no confirmed high is left: the three highs kept affect
one course each and are low fix items. For Fjellhaug, with the fulltext
fix in (BTM1003 and RLE1001 now read /emne/btm1003 and /emne/rle1001) and
the anonymizer's scripture fix: ready with known limits. fetched .785 and
plan .782 (n 275) are under .80 because 62 course-years have no page in
the site's subject sitemaps.
