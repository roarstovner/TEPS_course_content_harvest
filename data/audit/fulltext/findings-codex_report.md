# fulltext audit — aggregated findings

2 findings across 2 institutions (from 3 agent reports in `data/audit/fulltext/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 2 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| nhh | 2 | 1 |  | whole_text / empty_or_failed | whole_text / formatting; teaching_methods / formatting |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| empty_or_failed | high | 1 |
| junk_included | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 1 |
| whole_text | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| khio | gpt-6.1-sol | 31 | Reviewed all 31 packet entries: 28 complete extracted plans and three failed-fetch records. The 28 random controls show readable, course-specific plans with facts boxes, no uncaptured page text and no substantive extraction defects; the absence of pre-pass flags is consistent with manual review, ... |
| nhh | gpt-6.1-sol | 28 | All 28 courses were reviewed, including 11 suspects. Extraction generally preserves the available plan and readable structure: uncaptured flags reflect excluded staff/contact boxes, next-term links or alternative link serialization; the three short suspects are sparse or expired/pointer source pa... |
| vid | gpt-6.1-sol | 31 | Reviewed all 28 fetched-page examples and three failed-fetch examples: the eight retained control plans are complete and readable apart from one minor numeric artefact. The 19 empty suspects, one short shell and the uncaptured flag show no omitted plan body or unjustified exclusion; the three sof... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nhh | whole_text | empty_or_failed | high | widespread | nhh_BAN427-1_2025_autumn_1; nhh_FIE420-1_2025_autumn_1; n... | For current-plan pilot audits, trace the same fetched page through its actual build academic-year assignment before reporting plan availa... |
| ✓ | vid | assessment | junk_included | low | rare | vid_BAVER2030-1_2024_autumn_1 | Confirm the number against the stored BAVER2030 article block, then remove the confirmed artefact with a narrowly scoped VID postprocesso... |
