# Sections audit, FHS pilot (2026-11-04)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/sections/packets/fhs.md`
(19 courses: 10 suspects, 9 random), findings
`data/audit/sections/findings-codex/fhs.json`, report
`data/audit/sections/findings-codex_report.md`. All 8 findings verified.

## Summary

- The introduction ("Hva lærer du" and the text before the first accordion)
  lands in `course_content`, once, not lost or duplicated
  (fhs_ING3512-1_2025_autumn_1, fhs_ING0321-1_2025_autumn_1). The heading
  line "Hva lærer du" itself stays as the first line of the section.
- The accordion "Arbeidskrav og vurdering" is cut wrongly in both
  directions; this is why `coursework` is 0.46.
- All fixes are on the section side (heading map, sub-label split) and need
  only a rebuild from the raw store, no new harvest.

## Gate (sections numbers)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| assessment | 0.957 | 46 | 0.90 | FALSE |
| coursework | 0.457 | 46 | 0.70 | TRUE |
| no_sections | 0 | 46 | – | – |
| unmapped_headings | 0.00 | 608 | 0.65 | FALSE |

`no_sections`: none. The two plans without assessment (ING0321, ING2306
2025) are finding 2.

## Prioritised fix list (checked in the packet and `R/section_heading_map.R`)

1. **high**, common: inline "Arbeidskrav: ..." lines inside "Arbeidskrav og
   vurdering" (mapped to assessment) stay in assessment, and the plan gets
   no coursework section (fhs_ING3512-1_2025_autumn_1,
   fhs_MILM3301-1_2025_autumn_1). Also the unmapped sub-heading
   "Obligatorisk arbeidskrav" in fhs_LOG3105-1_2025_autumn_2,
   fhs_LED2404-1_2022_spring_2. Cause of `coursework` 0.46. Where: split the
   "Arbeidskrav:" label inside that accordion into coursework_requirements
   (the map's comment says gate sentences move alone; label lines do not).
2. **high**, occasional: after a "Obligatorisk arbeidskrav:" sub-heading the
   rest of the accordion ("Vurderingsform: ...", "Varighet", "Karakterskala")
   goes to coursework_requirements and the plan has no assessment
   (fhs_ING0321-1_2025_autumn_1, fhs_ING2306-1_2025_autumn_1; the 2 plans of
   the `assessment` number). Where: end the coursework part at
   "Vurderingsform:"/"Eksamen" inside the FHS accordion.
3. medium: unmapped sub-heading "Spesielle forhold knyttet til vurderingene"
   leaves assessment conditions in course_content
   (fhs_ING1501-1_2022_autumn_1, fhs_ING2505-1_2022_autumn_3). Where:
   heading map -> assessment.
4. medium: "Utfyllende informasjon om emnet" -> course_content sometimes
   carries an assessed deliverable, organisation or the course-evaluation
   notice (fhs_LOG3105-1_2025_autumn_2, fhs_OPS2203-1_2022_autumn_1).
   Mapping as intended; the evaluation notice could go to `.clean_sections()`.
5. low: placeholders "Blir publisert", "TBN", "Må bestås" as sections
   (fhs_CBU1506-1_2019_autumn_2).

## Rejected or downgraded

- "teaching_methods holds mandatory attendance" (reviewer: high): the
  text is under "Læringsaktiviteter" on the page; the same holds at other
  institutions; low at most.
- "prerequisites missing" (reviewer: high): the prior-knowledge sentences
  sit in the introduction with no heading; not an extraction error.

## Verdict

**not ready** by the skill's criteria: confirmed high findings 1-2 (plan
text in the wrong section; `coursework` 0.46 caused by them). Both are
section-side fixes rebuilt from raw; the full harvest would fetch the same
pages, so nothing here requires a new harvest.
