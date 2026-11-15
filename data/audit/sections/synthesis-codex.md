# Sections audit, build of 2026-11-15: NMH, UiT, KHiO

Run 2026-11-15 on the build of 2026-11-15 01:21 for nmh and uit (`--pilot`
samples) and khio (not `--pilot`: its pilot sample is 2025 and the site holds
only the 2026 plan, so the pilot gate is empty). Review agents: Codex
`gpt-6.1-sol`, one per institution, packets of 14 suspects + 6 random
controls (nmh 6 + 14). Findings: `findings-codex/{nmh,uit,khio}.json`,
report `findings-codex_report.md`. All 26 findings of the three passed
mechanical verification (the two ✗ rows in the report are stale ntnu and
himolde reports from earlier runs). Harvests were running; only the packets
and the code were read (no R), so the gate numbers below are the latest
recorded, not re-read on this build. The synthesis of the 2026-11-08 run
that this file replaces is in git history.

Each finding was checked against its packet and the code
(`R/section_heading_map.R`, `R/extract_sections.R`, `R/blocks.R`, the
`section_*` fields in `R/institution_config.R`): **20 fixes** (batches A-H
below; one of them waits on question 2), **1 spec question** (question 1),
**5 known deviations or known limits** (the user's decisions of 2026-11-11
and earlier verdicts), **0 rejected**.
Severities are re-rated; the reviewers mark most things high. Every fix is
rebuild-only: none needs a new harvest.

## Summary

- **NMH, high: plan text lost at headings the map does not know.** The
  English page of CONCOM70 loses its outcomes, overview, structure and
  requirements (four unmapped headings); MTMUSTER70 loses "D: Støtteemner",
  a lettered component the page marks as an h2, both under Innhold and under
  Arbeidskrav.
- **NMH, high: the summary fields before the first heading are read by no
  section.** "Forhåndskrav: Bestått 1. avdeling ..." / "DIG10/MUSTEK10 ...
  eller tilsvarende" is the only prerequisite text on the page, so no NMH
  plan in the sample has a prerequisites section (0 of 20); "Pensum: ..."
  notes are lost the same way.
- **KHiO and UiT, high: coursework left in assessment or teaching in three
  wordings** the gate rules do not read: KHiO's English "All course
  requirements must be approved in order for a student to receive a final
  course assessment." and the label "The course requirements are"; a UiT
  2008 numbered gate whose sentence the source splits with a blank line.
- **Medium, the most widespread: stock admin text in NMH and KHiO
  sections.** NMH's programme footer "Inngår i studieprogram" ends the last
  section in 14 of 20 plans; its stock resit and auto-registration sentences
  recur in nearly all; KHiO's examiner sentence is in 12 of 20. Neither
  institution has entries in `.logistics_sentences`.
- **UiT** is otherwise as before: prerequisites inside the dropped
  "Opptakskrav" block, evaluation notices, logistics (a known deviation),
  and the exam-language field (a spec question, again).

## Cross-institution patterns

| Pattern | Institutions | Root cause (checked) | Batch |
|---|---|---|---|
| Coursework wording no rule reads | khio, uit | `.gate_sentence_regex` English branch needs "in order to"/"before"; `.coursework_heading_regex` has no article or "are"; UiT: `.unwrap_lines()` does not join a sentence across a blank line, and the second half opens with "skal", which the gate regex rejects at sentence start | C, D |
| Stock admin sentences with no institution entry | nmh, khio | no `nmh`/`khio` in `.logistics_sentences`, `.logistics_blocks`, `.section_noise`; `.progression_regex` has no "Bestått semester er en forutsetning for å fortsette"; `.examiner_sentence_regex` needs "sensor" | E |
| Prerequisites read by no section | nmh, uit | NMH: the summary lines sit before the first h2, where `sectionize()` has no open section; UiT: "Opptakskrav" → `.drop` (map row "opptakskrav"), so its real prior-knowledge sentences go with the admission text | B, F |
| Unmapped heading closes the section | nmh | `sectionize()` drops the text under an unmapped `heading` (by design); "Learning objectives", "Overview", "Structure", "Course requirements" and lettered component h2s are in no map row | A |
| One-off mixed paragraphs under a mapped heading | nmh, uit | by meaning, not heading; known deviation (user 2026-11-11) | — |

## Prioritised fix list

**A. NMH headings (high).** `R/section_heading_map.R`: exact rows for NMH's
English pages, "learning objectives" → learning_outcomes, "overview" →
course_content, "structure" → teaching_methods, "course requirements" →
coursework_requirements. A lettered component heading ("D: Støtteemner",
`^[A-H][.:] `) should stay a line of the open section instead of closing it
(an NMH `section_*` field or the html reader in `R/blocks.R`); the line form
"D. Praksis" under Innhold should open the practice overlay
(`.is_practice_heading()` strips no letter prefix; the text-line practice
rule in `sectionize()` acts only inside teaching_methods). Examples:
`nmh_CONCOM70-1_2022_autumn_1` (four sections lost), `nmh_MTMUSTER70-1_2022_autumn_1`
(component D lost twice), `nmh_DISTUD70-2_2022_autumn_1` (practice overlay).
Effect: CONCOM70 gains learning_outcomes, course_content, teaching_methods
and coursework; the unmapped-heading share falls further.

**B. NMH summary fields (high).** The lines between the facts box and "Kort
om emnet" ("Gjennomføres:", "Avsluttende vurdering:", "Forhåndskrav:",
"Undervisningsspråk:", "Pensum:") are in no section; in VEILED53 an extra
"Kort om emnet" heading puts them inside course_content. Read them as
labelled fields: "Forhåndskrav" → `.admission` (as Nord's
`section_headings = c("adgangsregulering" = ".admission")`), so
`.admission_prerequisites()` keeps "Bestått 1. avdeling ..." and "DIG10/MUSTEK10
... eller tilsvarende" and drops "Opptak til mastergradsstudiet i ..."; "Pensum"
→ reading_list (see question 2); the rest → `.drop`. Also extend the fhs
sentence move "Emnet bygger på ..." to nmh (it requires a code with 3+
digits; NMH writes "UTHO13"). Examples: `nmh_UTHO23-1_2022_spring_1`,
`nmh_MUSTEK22-3_2025_autumn_1`, `nmh_MPTEO73-1_2021_autumn_1`,
`nmh_VEILED53-1_2022_autumn_1`. Effect: NMH gets prerequisites where the page
names prior courses (3 of 20 in the sample), and the summary leaves VEILED53's
course_content.

**C. KHiO English coursework (high).** `R/extract_sections.R`:
`.gate_sentence_regex`, English branch: accept "in order for (a|the)
student to receive ... assessment"; `.coursework_heading_regex`: accept
"The course requirements are" (article, trailing "are"). Examples:
`khio_DA514-2_2026_autumn_1` (requirements list in teaching_methods, gate in
assessment), `khio_DA502-2_2026_autumn_1`. Effect: both English plans get a
coursework section, as the 18 Norwegian ones in the sample already do.

**D. UiT 2008 numbered gate split by a blank line (high).** In
`uit_BED-2017-1_2008_autumn_2` the source reads "1) Ved semesterstart får
studenter beskjed fra faglærer om obligatoriske" / blank line / "skal løses
... krav til antall godkjente oppgaver for å gå opp til skriftlig eksamen."
`.inline_coursework_regex` ("\\d\\) [^.\\n]{0,200}godkjente oppgaver") wants
it on one line, and the gate regex rejects a sentence that opens with "skal".
Join a numbered item's continuation across one blank line when the line ends
without punctuation and the next opens in lower case (`.unwrap_lines()` or
the 2008 UiT rule). Effect: BED-2017 and any 2008 plan with the same break
get a coursework section.

**E. Stock admin text at NMH and KHiO (medium).** Section side only
(`course_plan` keeps it; the spec puts programmes, registration and resit
rules in no section). NMH: the footer block "Inngår i studieprogram" with
its programme lines (`.logistics_blocks$nmh`, or an NMH reader boundary
mapped to `.drop`); "For ny vurdering gjelder samme ordning som for ordinær
vurdering." / "New assessment ..."; "Studenten meldes automatisk opp til
undervisning og vurdering i emnet ..."; the pointer "For oversikt over ...
se ”Organisering” under beskrivelsen av det enkelte studieprogram."
(`.logistics_sentences$nmh`). KHiO: "Vurderingen foretas av emneansvarlig og
kan eventuelt gjøres i samarbeid med ..." (12 of 20), the notice "OBS: det
kan komme mindre endringer i emnebeskrivelsen ..." (`.logistics_sentences$khio`)
and "Bestått semester er en forutsetning for å fortsette studiet."
(`.progression_regex`). Other NMH resit wordings (DRAVHA90's committee
procedures) join the known deviation for resit rules. Examples:
`nmh_UTHO23-1_2022_spring_1`, `nmh_AKULYD10-1_2025_autumn_1`,
`khio_DE101-1_2026_autumn_1`, `khio_MK523-1_2026_autumn_1`. Effect: the last
section of ~70% of NMH plans loses the footer; assessment loses the stock
sentences.

**F. UiT prerequisites in "Opptakskrav" and content (medium).** Set
`section_headings = c("opptakskrav" = ".admission")` for uit, so
`.admission_prerequisites()` keeps "For studenter som tar emnet som
videreutdanning er forkunnskapskravet fullført lærerutdanning med minimum 60
studiepoeng i norsk." and drops the admission rules; add a uit sentence move
course_content → prerequisites for "Det forutsettes (grunnleggende)
kunnskaper i ...". Examples: `uit_LER-3101-1_2025_spring_1`,
`uit_BIO-3607-1_2008_autumn_1` (and FIN-1025, MBI-3016 from the run of
2026-11-05). Check with `preview_change()`: UiT's Opptakskrav blocks are
long, and `.real_prerequisite_regex` keeps any line with "studiepoeng".

**G. UiT cleanup (low).** `.evaluation_part_regex` reads only an
"Emneevaluering" heading: add "Kvalitetssikring" and the stock "Emnet
evalueres ..." / "Emnet vil bli evaluert ..." sentences in teaching_methods
(`uit_LER-3101-1_2025_spring_1`, `uit_LRU-3909-1_2018_spring_2`,
`uit_REI-3901-1_2025_autumn_1`); the reading-list contact notice "Kontakt
studiekonsulenten for informasjon om tilgjengelige prosjektoppgaver." as a
placeholder (`uit_FYS-3730-1_2008_autumn_1`); intake-capacity sentences
(`uit_HEL-6308-1_2019_autumn_1`) and programme placement under "Mål"
("Valgfritt masteremne. Kan blant annet brukes som støtteemne ...",
`uit_ARK-3021-1_2008_autumn_1`) if they recur.

**H. KHiO section text repeated under a second heading (low).** RE305
repeats its teaching methods under the assessment heading, DA502 its
outcomes under teaching methods; the source does it, and the copy says
nothing about the second section. A non-practice section whose whole text
equals an earlier section's could go in `.clean_sections()`. Examples:
`khio_RE305-1_2026_autumn_1`, `khio_DA502-2_2026_autumn_1`. RE305 is then
left with no assessment section, which is what its page says.

## Changes since the previous run

Only uit has a baseline (`HEAD`: the run of 2026-11-05 on uit's whole build,
a different sample, so the comparison is by target and error type only).
Gone: a plan without sections (BIO-3331), placeholder rows, admission text in
course_content, the law courses' second gate. Persisting: the
exam-language field (question 1), logistics in assessment (known deviation),
prior knowledge under "Opptakskrav" (F), evaluation notices (G). New: the
blank-line gate (D), "Det forutsettes ..." in content (F), the reading-list
contact notice (G), practice in ODO-6302 (known limit). nmh and khio have no
earlier sections audit.

## Known deviations and limits (no fix)

- One-off mixed paragraphs under a mapped heading (user 2026-11-11, known
  deviation in `course_plan_spec.md`): NMH MTMUSTER70's teaching paragraphs
  under Innhold, KPKUNST70's portfolio sentence and DRAVHA90's thesis format
  in course_content; UiT STA-3003's "Three mandatory homework sets." and
  BIO-2606's seminar description in teaching_methods.
- Resit, postponement and venue sentences in UiT assessment (HEL-6308,
  SPL-6900, ARK-3021): the UiT known deviation for resit rules.
- `uit_ODO-6302-1_2019_autumn_1`, one-block layout (accepted 2026-10-21):
  its learning_outcomes holding the arbeidskrav and its "Praksis:" list
  without an overlay (two reviewer highs, re-rated low).

## Questions for the user

1. **UiT's "Undervisnings- og eksamensspråk" field** goes whole (`.drop`,
   methods.qmd 2026-10-22: a language fact). The spec puts the exam language
   in assessment, and some fields say more than "Norsk": "Examination
   questions will be given in English, but may be answered either in English
   or a Scandinavian language." (STA-3003, FYS-3730, JUR-3902,
   UNIPED-300). Raised as low on 2026-11-05 and found again. Options: (a)
   keep the drop and add a known deviation; (b) move the field's sentences
   that name the exam or the answer language to assessment (a sentence move
   from `.drop`, as HiOF's). Suggested: (b).
2. **NMH "Pensum: Omtrent 1000 sider felles pensum som oppgis av
   faglærer."**: a reading load with no list. reading_list (it says how much
   is read) or no section (a pointer)? Suggested: reading_list; B follows
   the answer.

## Rejected findings

None. The reviewers' own rejections hold: NMH's leak->reading_list flags in
DRVIT90 and KPKUNST70 (essay sources, portfolio documentation), UiT's
repetition flags on real teaching, prerequisites and exam tables, KHiO's 12
DE/IM templates as boilerplate (they repeat, but they are plan text).

## Verdict

Sections only; an institution's verdict for its full harvest is the weakest
of its three checks. None of the fixes needs a new harvest.

| inst | assessment | coursework | no_sections | unmapped_headings | source |
|---|---|---|---|---|---|
| nmh | 1.00 (n 61) | .967 | 0 | .028 (n 635) | build of 2026-11-14 |
| uit | .985 (n 66) | .727 | 0 | .332 (n 760) | 2026-10-22 / 2026-10-28 |
| khio | — | — | — | — | pilot gate empty (n 0) |

Thresholds: assessment ≥ .90, coursework ≥ .70, unmapped_headings ≤ .65; no
number flagged.

- **nmh: not ready.** No gate number flagged, but confirmed highs on the
  sample: plan text lost at unmapped headings (A) and prerequisites read by
  no section (B). Ready after A and B; E is medium.
- **uit: not ready**, by one high: the 2008 gate split by a blank line (D),
  a single plan in the sample; everything else is medium or low, a known
  deviation, or question 1. Ready after D.
- **khio: not ready.** Its English-taught plans keep coursework in assessment
  and teaching (C; 2 of 20 in the sample, the only English ones). The gate
  says nothing (n 0). Ready after C; E and H are medium and low.
