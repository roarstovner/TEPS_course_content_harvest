# Sections audit, build of 2026-11-06: 14 institutions

Run 2026-11-08 on the build of 2026-11-06 for hiof, hvl, inn, mf, nih, nla,
nmbu, oslomet, steiner, uib, uio, uis, usn and vid. Review agents: Codex
`gpt-6.1-sol`, one per institution, packets of up to 14 suspects + 6 random
controls. Findings: `findings-codex/{inst}.json`, report
`findings-codex_report.md` (all findings of these 14 verified ✓; the two ✗
rows are stale ntnu/himolde reports from earlier runs). The other eight
institutions in the report (fhs, hivolda, nord, ntnu, phs, uia, uit, himolde)
were handled after their runs of 2026-11-05/06 and are not covered here. The
synthesis of the 2026-11-05 run that this file replaces is in git history.
Harvests were running; only the packets and the code were read (no R).

The 14 reports hold 54 high or medium findings. Each was checked against its
packet and the code (`R/section_heading_map.R`, `R/extract_sections.R`,
`R/blocks.R`, the `section_*` fields in `R/institution_config.R`):
**42 real** (cause found in the code; in fix batches A-H below), **9 spec
questions** (the spec says what goes where, but the fix needs a choice by the
user; see "Questions for the user") and **3 rejected**. Severities below are
re-rated; the reviewers mark most things high.

## Summary

- **High, two unmapped assessment headings lose design text.** NMBU's
  "Om bruk av KI" (every sampled NMBU plan) and UiB's "Karakterskala"
  (14 of 20 UiB packet entries) map to no section, so sectionize() drops the
  AI rules and the grading scale. The spec puts both in assessment.
- **High, outgoing progression rules sit in prerequisites.** NLA's
  "Progresjonskrav" maps to prerequisites, but its text says what passing
  *this* course permits later ("Emnet må være bestått for å kunne fortsette på
  Kunst og Håndverk 2 (GLU 1-7)."), which is the whole prerequisites row in
  11 sampled arts-and-crafts plans. VID writes the same kind of rule in course
  content and assessment.
- **Medium, the most widespread kind: exam logistics in running assessment
  text** (resit, appeal, withdrawal, examiner and deadline sentences), at 11 of
  the 14. Several are stock sentences repeated over many plans (HVL's 13
  engineering theses, all nine MF courses, UiS MGL2120), which per-institution
  noise patterns can remove, as was done for HiMolde on 2026-11-08.
- **Medium, gate and attendance rules miss a few wordings**: "Alle
  læringsaktiviteter er obligatoriske." (NLA, 11 plans), "For å kunne
  fremstille seg til eksamen forutsettes det at obligatoriske aktiviteter er
  godkjent" (VID), and attendance sentences that only mention "praksis" as
  context (UiO NOR4515).
- **Mixed-meaning paragraphs under a correctly mapped heading** (teaching or
  assessment design inside "Kort om emnet", "Om dette emnet", "Innledning") are
  the commonest cross-section finding (14 institutions in the report). Where a
  sentence recurs over many plans, a sentence move fixes it (batch G); the
  one-off wordings are a question for the user.
- Rejected: OsloMet practicum plans without sections (a documented choice),
  INN PUB3000's duplicate (in the source), VID MAPBU9010's project plan in both
  coursework and assessment (the page says both).

## Cross-institution patterns

| Pattern | Institutions | Root cause | Batch |
|---|---|---|---|
| Resit, appeal, withdrawal, examiner, deadline sentences in assessment | hiof, hvl, inn, mf, nmbu, oslomet, steiner, uib, uio, uis, vid | `.section_noise` has no patterns for these wordings; they share paragraphs with design, so no `.drop` heading catches them | D |
| Admin and admission sentences in non-exam sections (programme line, free-of-charge notice, course evaluation, "Adgang til undervisning", insurance, contact pointers, workload budgets) | hvl, inn, uio, uis | `.logistics_sentences` has no entries for inn, uio, uis; hvl's lacks these wordings | D |
| Outgoing progression rule as a prerequisite | nla, vid | "progresjonskrav" → prerequisites in the heading map; no rule tells an outgoing condition from an incoming one | E |
| Admission sentence in prerequisites or lost with an admission block | nmbu, uio, usn | `.admission_line_regex` misses "Emnet er obligatorisk for studenter tatt opp til ..."; UiO's "Opptak til emnet" is `.drop`, not `.admission` | E |
| Pointer-only rows kept as sections | hiof, oslomet, uib | `.placeholder_phrases` misses the wordings | B |
| Gate or attendance sentence left in teaching or assessment | nla, vid, uio, steiner, oslomet | `.attendance_sentence_regex` (no "læringsaktiviteter"; the `praksis` lookahead excludes any sentence naming praksis), `.gate_sentence_regex` (no "forutsettes det at") | C |
| Teaching or assessment design under the content heading, stock sentences | hvl, nla, mf, inn, uis, usn | `.sentence_moves` has entries only for phs, uit, fhs, hivolda | G |
| The same, one-off wordings | inn, mf, nih, nmbu, oslomet, steiner, uib, uio, vid, usn | as above; a rule per wording | user |
| Text reader takes a label for a top-level heading, or misses one | uis, usn | `.text_blocks()` (`R/blocks.R`) needs a blank line before a heading; it has no scope for numbered arbeidskrav blocks | F |

## Prioritised fix list

Each batch is one root cause or one code area, for one helper. Check each
change with `preview_change()` before the build; one build for all.

### A. Unmapped assessment headings (high)

- Where: `R/section_heading_map.R`, `section_heading_patterns`: add
  `"om bruk av ki"` → assessment (exact) and `"karakterskala"` → assessment
  (exact; at UiO it is a sub-heading that already stays as a line, so it should
  then stay too).
- Institutions: nmbu (every 2025 plan has the block), uib (most plans; UiO
  uses the sub-heading too).
- Examples: `nmbu_MATH100-1_2025_autumn_1` ("K1 - Ingen bruk av KI" lost),
  `nmbu_PPXP100-1_2025_autumn_1`; `uib_INF367-0_2025_spring_1` ("Ved sensur
  vert karakterskalaen A-F nytta." lost), `uib_ARK212-0_2021_spring_1`.
- Expected effect: NMBU assessment gains the AI category; UiB assessment
  gains the grading scale. Spec check: AI rules and the grading scale are
  assessment design. The NMBU link label "Her finner du KI-kategoriene
  beskrevet" (spec table) must stay out: check whether it sits under the
  heading and add it to `.section_noise$nmbu` if so. UiB's "Karakterskala"
  over "Informasjonen fins ikkje på norsk." must go (batch B).

### E. Prerequisites: outgoing progression and admission (high)

- Where: `R/extract_sections.R`: the prerequisites cleanup in
  `.clean_sections()` / `.strip_admission_lines()`, `.admission_line_regex`;
  `R/institution_config.R` uio `section_headings`.
- Outgoing progression (nla, vid): a sentence whose subject is this course
  and whose condition is to continue to or start on something later ("Emnet
  må være bestått for å kunne fortsette på ...", "Emnet må være bestått for å
  kunne starte på avsluttende praktisk-teologisk utdanning (APTU).", "Studenter
  kan, ved å gjennomføre dette emnet og emnet ... gå videre til ...") leaves
  every section. "En må ha fulgt Kunst og håndverk 1 for å kunne fortsette på
  Kunst og håndverk 2" is outgoing in the KH1 plans that hold it
  (`nla_GL5KH1-1_2022_autumn_1`); the same sentence in a KH2 plan would be a
  prerequisite, so drop it only when the course it names first is this course
  (title) or keep "Emnet ..." as the only generic form. Examples:
  `nla_MGL1KH101-1_2017_autumn_2`, `nla_4MGL1NO201-1_2021_spring_2`,
  `vid_MATRS5115-1_2025_spring_1`, `vid_BASJE1010-1_2024_autumn_1`.
- Admission (nmbu): "Emnet er obligatorisk for studenter tatt opp til
  studieprogrammet LUR." into `.admission_line_regex` (example
  `nmbu_PPPE301-1_2025_autumn_1`, `nmbu_PPFD301-1_2025_autumn_1`).
- UiO (low): "Opptak til emnet" as `.admission` for uio, so a sentence naming
  a completed subject group survives ("Opptak på emnet forutsetter at
  programmets progresjonskrav er oppfylt og at 80-gruppen i geofag er
  fullført.", `uio_GEO5930L-2_2025_autumn_1`); `.real_prerequisite_regex` needs
  "fullført" or the subject group for it.
- USN (low): "Ingen forkunnskaper." followed by teaching resources
  (`usn_MBVA100-1_2022_autumn_2`): the resource sentence goes to
  teaching_methods.
- Expected effect: NLA prerequisites rows of the KH and Norsk courses shrink
  to their real prerequisites or go; the spec puts outgoing rules in no
  section. Implement the progression rule as its own regex, not in
  `.sentence_moves` (batch G edits that list).

### D. Exam logistics and admin sentences, per institution (medium)

- Where: `R/extract_sections.R`, `.section_noise` (assessment and coursework)
  and `.logistics_sentences` (any section), new entries per institution.
- Assessment logistics, stock sentences first:
  - hvl: "Ved ikke bestått karakter på bacheloroppgaven, gis gruppen anledning
    til å levere en forbedret versjon i påfølgende semester." (13 engineering
    theses, `hvl_ELE351-1_2025_autumn_1`); "Intern og ekstern sensor"
    (`hvl_JOR508-1_2022_autumn_1`).
  - mf: "For ikke å bruke et eksamensforsøk, må studenten aktivt trekke sin
    vurderingsmelding til emnet innen trekkfristen ..." (all nine courses),
    "Sensurfristen er normalt 3-6 uker etter innleveringsfrist.", the pointer
    "Informasjon om innleveringsfrister, innleveringsprosedyren ... er
    tilgjengelig på eksamenssidene" (`mf_AVH5055-1_2025_autumn_1`,
    `mf_PED5610-1_2025_autumn_1`). The gate sentence before it ("Studenter som
    ikke oppfyller studiekravene, kan ikke gå opp til eksamen i emnet.") stays.
  - uis: attempt and resit rules ("En kandidat har som hovedregel ett forsøk
    på en utøvende eksamen, bachelor- eller masteroppgaven.",
    `uis_MGL2120-1_2025_autumn_1`, `uis_LFYBAC-1_2018_autumn_1`).
  - inn: "Sensureres av faglærer og én ekstern sensor.", "Eksamensresultatet
    kan (ikke) påklages." (`inn_AUD3003-1_2025_spring_1`,
    `inn_MUSB2900-1_2025_autumn_1`).
  - nmbu: "Dersom studenten ikke består en praksisperiode, kan samme periode
    gjennomføres bare en gang til.", the suitability notice, the
    responsibility sentence (`nmbu_PPRA301-1_2025_autumn_1`).
  - oslomet: submission calendar and exam-plan pointers ("Siste frist for
    innlevering vil fremgå av eksamensplanen."), resit entitlements, ODA
    publication agreement (`oslomet_ØAMAS5900-1_2025_autumn_1`,
    `oslomet_SSA5910-1_2021_autumn_1`).
  - steiner: examiner, diploma, appeal pointer, resubmission after F
    (`steiner_M-PEL2.2_2025_autumn_1`).
  - uib: the "Fristar" deadline/template block and "Sensor og veileder skal
    være til stede ved den offentlige presentasjonen."
    (`uib_BIO399-0_2025_autumn_1`).
  - uio: "Eksamenskommisjonen består av to sensorer, hvorav minst én er
    ekstern. Veileder er ikke med i kommisjonen.", Inspera delivery
    (`uio_MDID4009-1_2025_autumn_1`, `uio_GEO5930L-2_2025_autumn_1`).
  - vid: "VID arrangerer inntil tre utsatte prøver. ...", "Utsatt eksamen"
    paragraphs (`vid_BASP1035L-1_2025_spring_2`, `vid_MARUS5020-1_2024_autumn_1`).
  - hiof: the English resubmission and appeal paragraphs
    (`hiof_SFB50314-1_2016_spring_1`).
- Admin sentences in other sections (`.logistics_sentences`):
  - hvl: the "Studieprogram: ..." line in course_content (13 engineering
    plans, `hvl_BYG350-1_2025_autumn_1`) and "Utdanningen er gratis og utgjør
    7,5 studiepoeng." (`hvl_VSFO803-1_2025_autumn_1`); programme lists and
    costs are plan text in no section.
  - inn: "Opptakskravet er generell studiekompetanse/realkompetanse." in
    course_content, the course-evaluation sentence in teaching_methods, the
    "Økonomi" label and the project-money sentence (`inn_DOKHIS-1_2023_autumn_3`,
    `inn_MUSB2900-1_2025_autumn_1`, `inn_AUD3003-1_2025_spring_1`).
  - uio: "Adgang til undervisning: Undervisningen er kun åpen for studenter
    med opptak i emnet.", the contact pointer, insurance advice
    (`uio_MDID4009-1_2025_autumn_1`, `uio_BIOS5000-1_2025_autumn_1`).
  - uis: the workload budget in teaching_methods ("Anslag over tidsforbruk",
    "Forventet timeinnsats (413 timer): Undervisning: 77 timer ...",
    `uis_MGL2120-1_2021_autumn_1`, `uis_MGL2311-1_2021_autumn_1`).
- Spec check: logistics and the examiner arrangement are out of assessment;
  a resit sentence that also says what the resit consists of is design and
  stays. Wordings that share a sentence with design, or occur once, may be
  left and recorded as known deviations (question 2 below).

### C. Gate and attendance wordings (medium)

- Where: `R/extract_sections.R`, `.attendance_sentence_regex`,
  `.gate_sentence_regex`.
- "Alle læringsaktiviteter er obligatoriske." (nla, 11 plans,
  `nla_MGL1KH101-1_2018_autumn_1`): add "læringsaktivitet" to the "alle?
  (undervisning|samling|seminar|forelesning)\w* er obligatorisk" branch.
- "For å kunne fremstille seg til eksamen forutsettes det at obligatoriske
  aktiviteter er godkjent og praksis er bestått." (`vid_PTUKUN2014-1_2025_spring_1`):
  "forutsettes det at" after the exam-access clause.
- The leading `praksis` lookahead of `.attendance_sentence_regex` excludes
  "Som ein del av praksisen i emnet er det obligatorisk å delta i minst 8 av
  10 undervisningssamlingar." (`uio_NOR4515-1_2025_autumn_1`): exempt
  sentences whose attendance object is teaching (samling, seminar,
  undervisning) rather than the placement. Keep attendance in praksis itself
  out.
- Low: "Studenter som uten gyldig grunn ikke har lagt inn tekster innen
  fristene i arbeidsmappen, får ikke levere vurderingsmappe."
  (`steiner_M-NOR1_3_2025_autumn_1`); "Should a student exceed the maximum
  limit for absence, extra assignments will be required ..."
  (`oslomet_TANNPRAINN1A-1_2018_autumn_2`; "exceed" comes before "absence",
  and the placement lookahead excludes it).
- Expected effect: a few hundred sentences at most move to coursework; check
  every moved distinct text in `preview_change()` for negations and
  practice-placement attendance, as in the attendance appendix of 2026-11-05.

### G. Sentence moves for stock mixed-meaning sentences (medium)

- Where: `R/extract_sections.R`, `.sentence_moves` (new entries).
- hvl: "Arbeidet utføres normalt i grupper og kan være et samarbeid med en
  bedrift ..." course_content → teaching_methods (13 engineering theses,
  `hvl_ELE351-1_2025_autumn_1`, `hvl_MAS354-1_2024_autumn_1`).
- usn: "Praksis vurderes til bestått/ikke bestått." teaching_methods →
  assessment (9 PPU offerings, `usn_PPUFRI3-1_2021_autumn_1`,
  `usn_PPUEN3-1_2020_autumn_1`); a grade is assessment design.
- nla: "Grunnet Covid-19 situasjoen kan deler av (evt. hele) undervisningen
  bli gjennomført digitalt. Dette vil bli spesifisert i undervisningsplanen
  for emnet." course_content → teaching_methods (`nla_4MGL1NO201-1_2021_spring_2`,
  `nla_MGL1KH101-1_2020_autumn_1`).
- mf: "Undervisning i emnet gis i form av ..." course_content →
  teaching_methods (`mf_RL5065-1_2025_autumn_1`, `mf_PPU1010-1_2025_autumn_1`);
  "Studenten må normalt ha bestått emner tilsvarende 40 studiepoeng på
  masternivå ... før oppmelding til masteroppgaven." and "For å melde seg opp
  i emnet, må studenten ha bestått eksamen ..." course_content → prerequisites
  (`mf_AVH5055-1_2025_autumn_1`, `mf_PPU1010-1_2025_autumn_1`).
- uis: "Dette emnet har mappevurdering som eksamensform. Til vurdering vel
  studenten to av dei tre skriftlege arbeidskrava ..." coursework_requirements
  → assessment (MGL2120, `uis_MGL2120-1_2025_autumn_1`).
- inn: "Arbeidskravene fordrer utprøving i sosialpedagog-rollen."
  teaching_methods → coursework_requirements (`inn_SOSP3002-1_2025_spring_1`).
- Before adding a move, count its wording in the build (`preview_change()`):
  add it only where it recurs; one-off wordings belong to question 1 below.

### F. Text reader: heading boundaries (medium)

- Where: `R/blocks.R`, `.text_blocks()`.
- uis: "Arbeidsmengd" right after a content sentence, with no blank line
  before it, is not a heading, so the workload block stays in course_content
  (`uis_MGL2120-1_2025_autumn_1`, `uis_MGL2120-1_2024_autumn_1`). Accept an
  exact `.drop` heading line without a preceding blank line, for this heading
  or for uis.
- usn (nursing, 3 offerings; not teacher education): standalone "INNHOLD",
  "VURDERING" labels inside numbered Arbeidskrav blocks open top-level
  sections, so the requirements' instructions land in assessment
  (`usn_050-E16-1_2022_spring_1`, `usn_051-E16-1_2022_spring_1`). Keep them
  inside the open numbered requirement up to the next requirement or real
  heading.

### B. Pointer-only rows (medium)

- Where: `R/extract_sections.R`, `.placeholder_phrases`.
- hiof: "Felleslitteratur (pedagogikk og fagdidaktikk) fremgår av
  emnebeskrivelsen for Pedagogikk i praktisk-pedagogisk utdanning." with
  "Litteratur spesifikt for emnet: Opplyses ved studiestart og gis til dels i
  kompendium." (the PPU didaktikk courses, `hiof_LPKR116-1_2019_autumn_1`,
  `hiof_LPDR216-1_2022_spring_1`); "Oppdatert litteraturliste vil være
  tilgjengelig i Leganto ved semesterstart." (`hiof_LSV2MAT20-1_2024_spring_1`).
- oslomet: "Se omtale av praksis i programplanen" (and the "praksisi" typo),
  "Se nærmere omtale ...", "Se forkunnskapskrav beskrevet under Innhold og
  oppbygging" (`oslomet_YFHDPRA3-1_2022_spring_2`, `oslomet_ØAMAS5900-1_2025_autumn_1`).
- uib: "Informasjonen fins ikkje på norsk." (also "TInformasjonen"), "Sjå dei
  engelske nettsidene.", "Not relevant." (`uib_INTH321A-0_2025_spring_1`,
  `uib_INTH356-0_2023_autumn_1`).
- Expected effect: the rows go. UiB pages whose every section is the notice
  become shells, so the offering has no plan (spec: a pointer to the English
  page); question 4 below.

### H. Reader configuration, single courses (low)

- mf: "Arbeidsform og organisering" inside the Studiekrav `<details>` is no
  sub-heading, so teaching lines stay in coursework (`mf_RL1014-1_2025_autumn_1`);
  `section_subheading_selector` (`div.wp-block-group p`) misses it; check the
  page's HTML for the element.
- vid: an "Annet" label inside the assessment block carries prerequisites,
  coursework instructions and meeting arrangements (`vid_MATRS5115-1_2025_spring_1`);
  "Annet" elsewhere holds exam design (BATEOL3030), so no global mapping.
- hiof (2013 layout): "4 timer forelesning + øvinger pr. uke." under the
  unmapped "Undervisningssemester" heading is lost (`hiof_ITF10306-1_2013_spring_1`).

## Questions for the user

1. **Mixed-meaning paragraphs, one-off wordings.** The spec assigns text by
   meaning, whatever the heading. Nine findings are wordings that occur in
   one course: aims written as outcomes under the content heading
   (`inn_SOSP3002-1_2025_spring_1`), subject descriptions inside learning
   outcomes and capability sentences in content (`mf_PPU1010-1_2025_autumn_1`,
   `mf_RL5075-1_2025_autumn_1`), teaching sentences in "Kort om emnet" (nih
   `nih_LKI236-1_2024_spring_2`, uio `uio_NOR4515-1_2025_autumn_1`,
   `uio_PROMO4-1_2025_autumn_4`) or "Mål og innhold" (`uib_INF367-0_2025_spring_1`),
   thesis design in content or coursework (`nmbu_M30-LUN-1_2025_autumn_1`,
   `oslomet_SSA5910-1_2021_autumn_1`, `steiner_M-PEL2.2_2025_autumn_1`,
   `uib_LOGO345-0_2018_spring_1`), practice assessment under "Gjennomføring av
   praksis" (`uio_HIS4015L-1_2025_autumn_1`). A rule per wording, or a known
   deviation in `course_plan_spec.md` ("mixed paragraphs under a mapped
   heading stay with the heading"), with a `methods.qmd` entry?
2. **Resit and examiner wordings at more institutions.** The spec's known
   deviation names UiT, NTNU, Nord, FHS (and OsloMet's and HiOF's resit
   sentences). Batch D removes the stock sentences; extend the deviation to
   the wordings it leaves at hvl, inn, mf, nmbu, oslomet, steiner, uib, uio,
   uis and vid?
3. **OsloMet practicum plans (rejected here).** `oslomet_M1GP4000-1_2025_autumn_2`,
   `oslomet_M5GP1000-1_2020_autumn_1` have no sections: their pages point to
   the programme-wide Fagplan, which covers all study years without markup
   between them and is not read by choice (`data/data_notes.qmd`, 2026-10-15).
   The reviewer asks to read it. Keep the choice?
4. **UiB courses described only in English.** After batch B, UiB pages that
   say only "Informasjonen fins ikkje på norsk." have no plan. UiB has no
   `english_page` setting; following the English page needs a new harvest of
   those pages. Worth it?

## Rejected findings

- **oslomet, all, missing_section (high)** — the practicum plans without
  sections: a documented choice (question 3), not an extractor error.
- **inn, teaching_methods, duplicate (high)** — `inn_PUB3000-1_2024_autumn_2`
  has the same 2,093 characters in course_content and teaching_methods. The
  packet's plan is cut before the teaching field, but the same page copies its
  Kunnskap list into Ferdigheter verbatim; the duplicate is most likely in the
  source, and a rule against identical rows would hide it rather than fix it.
- **vid, coursework_requirements, wrong_content (high)** —
  `vid_MAPBU9010-1_2025_autumn_1` lists "Prosjektplan" under Obligatoriske
  aktiviteter and says "Mappevurderingen består av et arbeidskrav og en
  prosjektplan." The extractor follows the page in both places; reconciling
  them would mean interpreting the page.
