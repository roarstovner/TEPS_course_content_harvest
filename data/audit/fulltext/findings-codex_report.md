# fulltext audit — aggregated findings

2 findings across 1 institutions (from 2 agent reports in `data/audit/fulltext/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 2 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| formatting | low | 1 |
| formatting | medium | 1 |

## By target

| target | n |
| --- | --- |
| teaching_methods | 1 |
| whole_text | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| khio | gpt-6.1-sol | 31 | Reviewed all 31 packet entries: 28 complete extracted plans and three failed-fetch records. The 28 random controls show readable, course-specific plans with facts boxes, no uncaptured page text and no substantive extraction defects; the absence of pre-pass flags is consistent with manual review, ... |
| nhh | gpt-6.1-sol | 28 | All 28 packet courses were read in full, including all eight suspects; substantive plans retain their headings, paragraphs, learning outcomes, assessment and facts, with no demonstrated loss of substantive page content. The four uncaptured suspects omit staff names/contact roles and next-semester... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nhh | whole_text | formatting | medium | common | nhh_BUS476-1_2025_autumn_2; nhh_TECH3-1_2025_autumn_1; nh... | Normalize NHH weblink nodes before block extraction: turn each into an ordinary anchor with href as its attribute and linkname as its vis... |
| ✓ | nhh | teaching_methods | formatting | low | rare | nhh_FIE401-1_2025_autumn_1 | Preserve whitespace around inline elements when reading nested list items, for example by flattening emphasis elements into text nodes wh... |
