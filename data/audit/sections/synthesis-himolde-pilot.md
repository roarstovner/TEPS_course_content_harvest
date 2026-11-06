# Sections audit, HiMolde pilot (2026-11-06)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/sections/packets/himolde.md`
(20 courses: 4 suspects, 16 random), findings
`data/audit/sections/findings-codex/himolde.json`, report
`data/audit/sections/findings-codex_report.md`. Build of 2026-11-06 16:54.
All 8 findings verified (quotes in the packet); severities revised below.

## Summary

- Headings map well (`unmapped_headings` 0.17; the English headings mapped
  on 2026-10-31 work). Every plan has sections.
- The `Pensum` part is almost never a reading list: the list is in Leganto,
  and the page shows a link label, a notice or the Leganto widget: about 35 of the
  build's 40 himolde reading_list rows are such text (spec: a pointer is not
  a section). Fixed in the working tree, uncommitted (below).
- One plan puts four numbered coursework requirements ("Arbeidskrav I: …")
  under the teaching methods as bold paragraphs; they stay in
  teaching_methods (high, SKD204). Not fixed: shared sub-heading code.
- `coursework` 0.625 is mostly real: 16 of the 18 plans have no coursework
  text on the page, ADM280 2022 says "Ingen.", SKD204 is the case above.

## Gate (sections numbers)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| assessment | 0.979 | 48 | 0.90 | FALSE |
| coursework | 0.625 | 48 | 0.70 | TRUE |
| no_sections | 0 | 48 | – | – |
| unmapped_headings | 0.170 | 687 | 0.65 | FALSE |

`no_sections`: none. The plan without assessment (LOG904-100 2019) is one
paragraph with no exam part. Unmapped headings (all of them, 5): "Emnet er
tilknyttet følgende studieprogram" / "The course is connected to the
following study programs" (facts: programme list), "Reduksjon i studiepoeng"
(overlap; its English form maps to `.drop`), "Evaluering av emnet" / "Course
evaluation" (course evaluation, plan text in no section). None is a part of
the plan's sections.

## Prioritised fix list (checked in the packet and the code)

1. **high**, rare: numbered bold sub-headings "Arbeidskrav I: Pre/post
   klinikk, reproduktiv helse" … "Arbeidskrav IV: Prosjekt barn" inside
   "Undervisnings- og læringsformer" (himolde_SKD204-1_2022_spring_1, 7,074
   chars of teaching_methods, no coursework section). Cause: `.sub_blocks()`
   (`R/blocks.R`) maps a `<p>` sub-heading only by an exact heading
   (`.exact_section()`); an all-bold one naming no section is a group label
   that returns to the parent section, and the coursework lead-in rule needs
   a closing colon. A fix must also keep the bold labels under each
   requirement ("Vurderingskriterier:") in coursework_requirements. Shared
   code (HiOF has the same layout): left for a separate issue.
2. medium, widespread: reading_list holds only Leganto link labels and
   notices ("Klikk her for pensumoversikt", "Click here for reading list",
   "The current reading list for … can be found in Leganto", "TBA",
   "Pensumoversikt Fant ingen pensumliste for dette emnet") or the Leganto
   widget with the list titles (himolde_LOG904-185-1_2025_autumn_1,
   himolde_VBU730-1_2022_autumn_1, himolde_SKD204-1_2022_spring_1). Drafted:
   placeholder phrases in `.placeholder_phrases` (`R/extract_sections.R`) and
   the widget in himolde's `exclude` (widget = boilerplate, spec).
3. medium, occasional: exam logistics inside assessment fields (weekday
   deadlines and start times, LOG904-140 2022, LOG904-131 2025; the exam
   system "Inspera", SAF230 2025). Not fixed; would be a himolde entry in
   `.logistics_sentences`.
4. medium, rare: the guest lecturer's name at the end of course_content
   (LOG904-130 2018): personal data, fixed with the anonymization finding.
5. medium (reviewer: high), pipeline-wide: text placed by heading, not by
   meaning (organisation sentences in course_content, ØKL110 2022; outcome
   sentences in course_content, VPL033 2019; assessment-focus sentences in
   teaching_methods, BØK411 2019). The same pattern is in the report for 5
   other institutions; no plan text is lost.
6. low: "The course will not be offered in the" left at the start of
   course_content (ADM420 2025, its semester removed).

## Rejected or downgraded

- "prerequisites duplicates course_content" (reviewer: high), LOG904-185
  2025: the page itself repeats the content under "Recommended knowledge";
  the extractor is faithful. Low, a site error.
- MAR100's Leganto pointer is already dropped: correct.

## Verdict

**not ready** by the skill's criteria: one confirmed high finding (fix 1,
coursework text in teaching_methods, 1 of 48 plans). `coursework` 0.625 is a
known limit (the plans have no coursework requirements). Every fix is on the
section side and is rebuilt from the raw store: nothing here needs a new
harvest.
