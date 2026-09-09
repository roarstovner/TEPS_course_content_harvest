# Candidate extra course-plan sections (#201)

Reconnaissance feeding a decision: should any course-plan section type *beyond*
the 7 canonical ones (course_content, learning_outcomes, teaching_methods,
assessment, coursework_requirements, prerequisites, reading_list) become its own
canonical section? The user is especially interested in **praksis** (placement).

## Method

`R/scan_candidate_sections.R` collected the heading texts of a sample (≤120
courses) per institution and tested each against candidate regexes, counting the
**share of sampled courses** carrying that heading.

**Caveats.**
- Covers only the 14 `html_headings`/`accordion_nord` institutions. **Not
  scanned:** usn, hivolda, steiner (`text_split` — flat text, no heading DOM),
  nla (`json`), samas (`noop`). usn's rendered text *does* contain a "Praksis"
  label, so praksis is undercounted for the teacher-ed institutions.
- The scan ran before `.collect_heading_candidates` was taught inn's
  `div.label` selector, so **inn shows 0 % spuriously** (its overlap/language
  facts were not seen). Re-run to refresh inn.
- A heading absent here can still exist as inline sub-text (e.g. UiA's "Faget i
  praksis" lives *inside* the Innhold block, not as a top-level heading).

## Findings — prevalence (% of sampled courses with the heading)

| Candidate | Institutions present | Median % where present | Notable |
|---|---|---|---|
| **studiepoeng overlap** (studiepoengreduksjon / overlappende emner / emnet overlapper) | **8** | 35 % | uib 92, ntnu 72, nord 62, uia 42, uio 27, uit 15, nih 5, oslomet 2 |
| **praksis** | 2 | 79 % | hiof 100, nord 58 (+ usn, not scanned) |
| **undervisningsspråk** | 2 | 68 % | uit 90, uia 45 |
| **kostnader** (utgifter) | 1 | 98 % | nord 98 |
| **arbeidsomfang** (workload) | 1 | 100 % | hiof 100 |

## Recommendation per candidate

- **studiepoeng overlap — strongest case for a new canonical section.** Most
  widespread (8 institutions), and structurally valuable: it records which other
  courses a course overlaps with / reduces credits against, i.e. an explicit
  *curriculum-graph* edge that nothing else in the dataset captures. Worth
  promoting to a canonical section (`credit_overlap`) and adding heading
  patterns ("studiepoengreduksjon", "overlappende emner", "emnet overlapper").

- **praksis — real but narrow; matches the user's hypothesis ("svært få").**
  Concentrated in teacher-education-heavy institutions (hiof, nord, usn). As a
  *discrete* section it is rare; elsewhere placement is described inside
  teaching_methods or as an inline sub-block (UiA "Faget i praksis"). Options:
  (a) keep folded in teaching_methods and answer praksis questions by filtering;
  (b) add a `praksis` canonical section that is populated only where present.
  Given the narrow footprint, **(a) is the lower-cost default**; (b) is justified
  only if praksis is a primary research target. **User decision required.**

- **undervisningsspråk** — language of instruction. Almost certainly already in
  the DBH metadata columns; extracting from text would be **redundant**. Skip
  unless DBH coverage is found lacking.

- **kostnader / arbeidsomfang** — each present at a single institution. Too
  narrow to canonicalise; leave as unmapped. Revisit only on specific demand.

## Suggested next step

If the user greenlights it, add **credit_overlap** as an 8th canonical section
(heading-map patterns + codebook variable), since it is both widespread and
otherwise-uncaptured. Hold on a praksis section pending the user's call on
whether placement is a primary research target.
