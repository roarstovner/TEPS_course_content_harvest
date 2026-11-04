# sections audit — aggregated findings

8 findings across 1 institutions (from 1 agent reports in `data/audit/sections/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 8 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

_(none)_

## By error type

| error_type | severity | n |
| --- | --- | --- |
| wrong_content | high | 3 |
| merged_sections | high | 2 |
| empty_placeholder | low | 1 |
| missing_section | high | 1 |
| wrong_content | medium | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 2 |
| course_content | 2 |
| coursework_requirements | 1 |
| cross_section | 1 |
| prerequisites | 1 |
| teaching_methods | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 19 | All 19 packet entries were read in full, including all displayed plan and extractor text; packet-builder truncation limits comparisons of omitted source passages and is not an extraction defect. The untitled introductions are retained in course_content without visible duplication, and learning-ou... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fhs | assessment | wrong_content | high | common | fhs_ING3512-1_2025_autumn_1; fhs_MILM3301-1_2025_autumn_1... | Enable bounded inline-coursework handling for FHS in page_sections(): recognise Arbeidskrav/Arbeidskravene and obligatory-submission lead... |
| ✓ | fhs | coursework_requirements | merged_sections | high | common | fhs_ING0321-1_2025_autumn_1; fhs_ING2306-1_2025_autumn_1;... | Add a bounded reverse split for coursework_requirements: Vurderingsform with substantive text starts assessment and takes its duration, g... |
| ✓ | fhs | teaching_methods | wrong_content | high | common | fhs_CBU2502-1_2025_autumn_1; fhs_ING2506-1_2022_autumn_1;... | Extend deterministic paragraph/item routing for FHS Læringsaktiviteter: separate explicitly obligatory attendance, labs, tests and absenc... |
| ✓ | fhs | prerequisites | missing_section | high | common | fhs_ING2506-1_2022_autumn_1; fhs_ING2505-1_2022_autumn_3;... | Extend .split_inline_prerequisites() with guarded FHS sentence patterns for Emnet bygger på followed by named course codes and Emnet bygg... |
| ✓ | fhs | assessment | wrong_content | high | occasional | fhs_LOG3105-1_2025_autumn_2; fhs_LED2404-1_2022_spring_2 | Add an exact obligatorisk arbeidskrav -> coursework_requirements entry to R/section_heading_map.R; trailing-colon normalization already e... |
| ✓ | fhs | course_content | merged_sections | high | occasional | fhs_ING1501-1_2022_autumn_1; fhs_ING2505-1_2022_autumn_3 | Map the exact special-assessment heading to assessment and preserve its substantive assessment-design conditions. Add bounded FHS cleanup... |
| ✓ | fhs | course_content | wrong_content | medium | common | fhs_LOG3105-1_2025_autumn_2; fhs_CBU2502-1_2025_autumn_1;... | Keep section_initial = course_content to preserve the introduction, but classify supplemental paragraphs separately: route research-propo... |
| ✓ | fhs | cross_section | empty_placeholder | low | occasional | fhs_CBU1506-1_2019_autumn_2; fhs_LOG3105-1_2025_autumn_2 | Add whole-row, section-aware placeholder rules for Blir publisert and TBN/TBN. in reading_list and bare Må bestås in learning_outcomes/te... |
