# Sections audit: UiA and NIH English-tree pages (2026-11-25)

Scope: only plans whose page comes from the Vortex English tree
(`/english/studies/courses/{year}/{spring|autumn}/`). The build of
2026-11-24 added these pages for offerings whose Norwegian URL answered 404
(`resolve_urls_english_tree_batch()`; `methods.qmd`, "UiA and NIH: an
offering whose Norwegian page answered 404 takes its page in the English
tree"). Inputs: the build that finished 2026-11-25 05:15;
`sections_qa_suspects.RDS` regenerated 2026-11-25.

Packets: `Rscript R/audit/prepare_sections.R uia nih --tag english
--url-pattern /english/` -> `packets/english/` (gitignored),
`manifest-english.csv`, `sample-english.csv`. Reviewers: Codex,
`gpt-6.2-sol`, one per institution. Report: `findings-english_report.md`
(11 findings, all passed the mechanical check).

| institution | English offerings | English plans (source pages) | packet |
|---|---|---|---|
| uia | 1,743 | 970 | 14 suspects + 6 random, 112 KB |
| nih | 148 | 127 | 13 suspects + 6 random, 124 KB |

Every English plan has sections (none in the no-sections list). Section
shares on English pages match the Norwegian pages of the same institution,
except coursework requirements (UiA 0.82 vs 0.91, NIH 0.86 vs 0.90) and
prerequisites at NIH (0.46 vs 0.26, mostly placeholder rows, see 1). UiA has
no reading lists on either tree (Leganto, expected).

## 1. Summary

- The English headings are read and mapped correctly at both institutions:
  no unmapped heading carried plan text, the learning-outcome groups
  (Knowledge/Skills/General competence) stay whole, and "Examination
  requirements" / "Examinations" separate coursework from assessment at UiA.
- No high-severity defect specific to the English tree. The verified findings
  are English counterparts of Norwegian wordings the cleanup already handles
  (placeholders, NIH's archive notice), stock logistics sentences, and one
  existing rule (admission blocks dropped) that also costs some required
  prior-course text.
- Largest by count: UiA's Canvas pointers in coursework/assessment (267
  plans, low), NIH's English placeholders (~50 plans, low), the dropped
  admission block that names a required course or credits (36 plans, medium).

## 2. Cross-institution patterns

- **English placeholders not in `.placeholder_phrases`** (UiA, NIH): the
  Norwegian forms ("nei", "ingen emner", NIH's "Ettersom vi er i en
  overgangsfase mellom to systemer ...") are there; their English versions
  are not.
- **Admission headings dropped whole** (UiA, NIH): `admission to the course`
  and `admission requirement if given as a free-standing course` map to
  `.drop` in `R/section_heading_map.R`, like the Norwegian `opptakskrav` and
  `opptak til emnet`. A required prior course or credit count written only
  there is lost. Not English-specific: the Norwegian pages do the same.

## 3. Prioritised fix list

1. **English placeholders** (low, ~52 plans). `R/extract_sections.R`
   `.placeholder_phrases`: add `no`, `no subjects?\.?` (NIH: 24 prerequisite
   rows such as "No subjects."), `core reading(?: for)?`, `reading list for`
   (NIH, 3 rows), and the English NIH archive notice "as we are in a
   transition phase between two systems, we refer to the Study Handbook ..."
   next to the Norwegian one (NIH, 28 reading-list rows). Examples:
   `nih_SPM123-1_2026_spring_1`, `nih_THP301-1_2026_autumn_1`,
   `nih_MAS449-1_2023_autumn_1`, `uia_IS-608-1_2026_spring_2` (bare "No",
   the only UiA plan).
2. **Admission block with required prior study** (medium, 36 English plans:
   UiA 31, NIH 5 of FRI300-312). Map the two English headings (and, for
   consistency, UiA's/NIH's Norwegian ones) to `.admission`, as UiT, Nord and
   others do in `section_headings`, so `.admission_prerequisites()` keeps the
   sentences that name prior courses or credits ("BE-110 Introduction to
   management Accounting or equivalent in addition to Higher Education
   entrance qualifications.", "Individual course applicants must have
   completed 60 ECTS credits in outdoor studies to qualify for admission.")
   and drops who may apply; `.admission_only_regex` needs English wordings
   ("Programstudents ...", "Higher Education Entrance Qualification",
   "Subject to availability"). A change to what prerequisites hold: needs a
   `methods.qmd` entry. Examples: `nih_FRI310-1_2026_autumn_2`,
   `uia_BE-318-1_2026_autumn_1`, `uia_MF-203-1_2025_spring_1`.
3. **NIH English gate wordings** (medium, about 8-10 NIH English plans).
   `.gate_sentence_regex` (`R/extract_sections.R`) does not catch "The
   student forfeits the right to take the final examination ... if the given
   group assignment ... are not approved" (8 plans have it in teaching
   methods) nor an approval-only item in assessment ("Group assignment in the
   format of oral presentation during course. Approved/not approved."). In
   `nih_MAS449-1_2023_autumn_1` the plan then has no coursework row at all.
   Of the 18 NIH English plans without a coursework row, 10 mention
   approval or compulsory work.
4. **Stock logistics sentences** (low). Per-institution entries in
   `.logistics_sentences` / `.section_noise`:
   UiA "More information will be given in Canvas ...", "Please refer to
   Canvas for more information." (267 plans; `uia_BE-318-1_2026_autumn_1`,
   `uia_HI-408-1_2026_autumn_1`); UiA "Due to grade requirements for state
   authorised auditors, a regular examination will be held both autumn and
   spring." and similar (40 plans; `uia_ME-437-1_2026_autumn_1`); NIH
   "The project assignment must be submitted in Wiseflow during a designated
   submission period ..." (18 plans; `nih_SPM123-1_2026_spring_1`). Check
   first whether the Norwegian pages have the same sentences, so one rule
   covers both.

## 4. Changes since the previous run

First audit of the English-tree pages; no baseline.

## 5. Rejected findings

- uia: HI-408/MM-922 coursework "in teaching methods": the text is a
  description of the learning activities and a participation expectation
  under the page's teaching heading; the approval gate itself is in
  coursework_requirements. Text under another heading stays in that section
  (`course_plan_spec.md`).
- uia: EN-212/EN-465 outcome or thesis sentences in course content: mixed
  prose follows its heading by design; sentence-level reassignment is left
  to the later lifting step (`.clean_sections()` roxygen).
- nih: "Introduction" carrying teaching format and exam language into
  course content: same reason; the exam-language move works on language
  headings only, by design.
- nih (part of the gate finding): MA531/THP111 mention the mandatory
  assignment or quiz in teaching methods while coursework_requirements
  already holds the gate: a mention of the activity, not lost or misfiled
  text.
