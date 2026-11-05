# sections audit — aggregated findings

10 findings across 1 institutions (from 1 agent reports in `data/audit/sections/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 10 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

_(none)_

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| fhs | 7 | 8 | prerequisites / missing_section; course_content / wrong_content; assessment / wrong_content; cross_section / empty_placeholder | prerequisites / merged_sections; coursework_requirements / wrong_content; cross_section / duplicate; coursework_requirements / missing_section | coursework_requirements / merged_sections; teaching_methods / wrong_content; course_content / merged_sections |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| missing_section | high | 3 |
| wrong_content | high | 2 |
| wrong_content | medium | 2 |
| duplicate | medium | 1 |
| empty_placeholder | low | 1 |
| merged_sections | high | 1 |

## By target

| target | n |
| --- | --- |
| prerequisites | 3 |
| course_content | 2 |
| coursework_requirements | 2 |
| cross_section | 2 |
| assessment | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 18 | All 18 packet entries (14 suspects and four random controls) were read in full as displayed; packet-builder truncation limits comparison of omitted passages and is not an extraction defect. Main sections and all displayed learning-outcome groups are generally preserved, but prerequisite loss, mix... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fhs | prerequisites | missing_section | high | common | fhs_ING3330-1_2025_spring_2; fhs_ING3511-1_2021_autumn_1;... | Add guarded sentence patterns for explicit builds-on statements followed by course codes, moving the prior-knowledge statement into prere... |
| ✓ | fhs | course_content | wrong_content | high | common | fhs_LED3201-1_2022_spring_1; fhs_LOG3105-1_2025_autumn_2;... | Route FHS supplemental and initial paragraphs by explicit bounded labels and lead-ins: Praktisk organisering og arbeidsformer and Pedagog... |
| ✓ | fhs | prerequisites | merged_sections | high | occasional | fhs_OPG3701-1_2023_spring_1; fhs_SPR2710-1_2023_spring_1 | Make the FHS reader recognise the missing organisation/teaching labels as boundaries, mapping the teaching block to teaching_methods. Add... |
| ✓ | fhs | prerequisites | missing_section | high | rare | fhs_SPR2721-1_2023_spring_1 | For FHS Opptakskrav blocks, retain sentences explicitly requiring prior knowledge or passed named courses as prerequisites, and discard o... |
| ✓ | fhs | coursework_requirements | wrong_content | high | rare | fhs_OPG3302-1_2022_autumn_1 | Within FHS thesis Arbeidskrav blocks, move the final written deliverable and its word-limit paragraphs into assessment. Keep proposal app... |
| ✓ | fhs | coursework_requirements | missing_section | high | rare | fhs_ING3511-1_2021_autumn_1 | Recognise the bounded compulsory-supervision sentence as coursework_requirements and remove it from assessment while preserving the repor... |
| ✓ | fhs | course_content | wrong_content | medium | common | fhs_OPG3701-1_2023_spring_1; fhs_AR301-1_2023_spring_1; f... | Recognise the FHS Målgruppe label and terminate course_content there. Map admission-only target-group blocks to .drop, stopping at the ne... |
| ✓ | fhs | assessment | wrong_content | medium | common | fhs_AR301-1_2023_spring_1; fhs_SPR2721-1_2023_spring_1; f... | Remove FHS examiner-arrangement and resit sentences and assessment-semester-only notices. Parse tabular rows to omit Tidspunkt and its co... |
| ✓ | fhs | cross_section | duplicate | medium | rare | fhs_OPG3701-1_2023_spring_1 | For this FHS gate, move the project-description approval sentence only to coursework_requirements and retain supervision arrangements onl... |
| ✓ | fhs | cross_section | empty_placeholder | low | common | fhs_CBU1506-1_2019_autumn_2; fhs_SPR2723-1_2024_autumn_1;... | Add whole-row, section-aware placeholder rules for Blir publisert, Avklares, TBN. and Kunngjøres, and bare Må bestås in outcomes/teaching... |
