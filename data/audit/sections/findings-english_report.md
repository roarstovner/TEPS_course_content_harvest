# sections audit — aggregated findings

11 findings across 2 institutions (from 2 agent reports in `data/audit/sections/findings-english`; model: gpt-6.2-sol).
Mechanical verification: 11 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| empty_placeholder | low | 2 |
| formatting_noise | low | 2 |
| truncated | high | 2 |
| wrong_content | high | 2 |
| wrong_content | medium | 2 |
| formatting_noise | medium | 1 |

## By target

| target | n |
| --- | --- |
| cross_section | 6 |
| assessment | 2 |
| prerequisites | 2 |
| teaching_methods | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| nih | gpt-6.2-sol | 19 | All 19 offerings yielded sections, the English headings mapped consistently, and the learning outcomes were intact. The 12 empty flags identified placeholder rows, while the long flag prompted a closer look at FRI310, where the extractor missed an entry requirement under the admission heading. Th... |
| uia | gpt-6.2-sol | 20 | UiA's English headings map reliably, both examination headings usually separate graded assessment from coursework gates, and the learning-outcome groups are intact. The two empty flags on bare 'No' are valid, while three 'Lectures.' empty flags and ten boilerplate flags mark genuine teaching meth... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nih | cross_section | wrong_content | high | common | nih_MAS449-1_2023_autumn_1; nih_MA531-1_2026_autumn_1; ni... | Extend the gate rules to recognise ‘forfeits the right to take the final examination’, approval-only group presentations corroborated by ... |
| ✓ | uia | teaching_methods | wrong_content | high | common | uia_HI-408-1_2026_autumn_1; uia_MM-922-1_2026_spring_1 | For UiA, move explicit participation requirements and the HI-408 assignment-detail block into coursework when the approved task names mat... |
| ✓ | uia | prerequisites | truncated | high | occasional | uia_MF-203-1_2025_spring_1; uia_BE-318-1_2026_autumn_1; u... | Route this exact heading to .admission and retain verbatim sentences with an explicit prior-course code or equivalent subject-matter requ... |
| ✓ | nih | prerequisites | truncated | high | rare | nih_FRI310-1_2026_autumn_2 | For NIH's English pages, route ‘Admission to the course’ through .admission and make .admission_prerequisites() retain English sentences ... |
| ✓ | nih | cross_section | wrong_content | medium | common | nih_MAS449-1_2023_autumn_1; nih_MA531-1_2026_autumn_1; ni... | Use NIH-specific sentence or clause rules for the observed introductions. Route explicit exam-language options to assessment and identifi... |
| ✓ | nih | assessment | formatting_noise | medium | common | nih_SPM123-1_2026_spring_1; nih_SPM104-2_2026_spring_3; n... | Strip the observed English task-release and submission-window sentences and add clause-level rules for WISEflow delivery. When a sentence... |
| ✓ | uia | cross_section | wrong_content | medium | occasional | uia_EN-212-1_2024_spring_1; uia_EN-465-1_2025_spring_1 | Match the explicit EN-212 learning-outcome and thesis-specification sentence starters and route their sentences to outcomes and assessmen... |
| ✓ | nih | cross_section | empty_placeholder | low | widespread | nih_SPM123-1_2026_spring_1; nih_THP301-1_2026_autumn_1; n... | Add whole-row patterns for the observed prerequisite negations, bare reading-list labels and stock archive redirect. Scope the patterns t... |
| ✓ | uia | cross_section | formatting_noise | low | common | uia_BE-318-1_2026_autumn_1; uia_MF-203-1_2025_spring_1; u... | Strip the UiA standalone 'More information will be given/provided in/on Canvas' and 'Please refer to Canvas for more information' sentenc... |
| ✓ | uia | cross_section | empty_placeholder | low | rare | uia_IS-608-1_2026_spring_2 | Suppress standalone 'No' and 'No.' case-insensitively in prerequisite and coursework-requirement rows. Keep full negative statements and ... |
| ✓ | uia | assessment | formatting_noise | low | rare | uia_ME-437-1_2026_autumn_1 | Filter standalone UiA sentences stating that a regular exam will be held in specified semesters. Preserve duration, assessment form, and ... |
