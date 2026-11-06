# fulltext audit — aggregated findings

2 findings across 2 institutions (from 6 agent reports in `data/audit/fulltext/findings-codex`; model: gpt-6.1-sol).
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
| fhs | gpt-6.1-sol | 28 | Fulltext extraction is faithful across all 28 reviewed courses (20 suspects and 8 random controls): the facts box, Hva lærer du introductions and other prose before the accordions are captured, the accordion sections are readable, and the uncaptured lines contain only update timestamps and versio... |
| himolde | gpt-6.1-sol | 28 | The 28 packet entries (one suspect and 27 random controls) show no confirmed fulltext extraction defect: retained plans preserve the visible course sections, facts boxes and readable structure, while omitted page text consists of navigation, timestamps, staff names and utility links. The short-te... |
| khio | gpt-6.1-sol | 31 | Reviewed all 31 packet entries: 28 complete extracted plans and three failed-fetch records. The 28 random controls show readable, course-specific plans with facts boxes, no uncaptured page text and no substantive extraction defects; the absence of pre-pass flags is consistent with manual review, ... |
| nhh | gpt-6.1-sol | 28 | All 28 courses were reviewed, including 11 suspects. Extraction generally preserves the available plan and readable structure: uncaptured flags reflect excluded staff/contact boxes, next-term links or alternative link serialization; the three short suspects are sparse or expired/pointer source pa... |
| phs | gpt-6.1-sol | 28 | All 28 course entries were read in full as presented in the packet; no supported fulltext defects were found in the 16 retained plans, whose visible content and metadata are cleanly structured, with only breadcrumbs and update information omitted. The 12 excluded pages match the stated earlier-ve... |
| vid | gpt-6.1-sol | 31 | Reviewed all 28 fetched-page examples and three failed-fetch examples: the eight retained control plans are complete and readable apart from one minor numeric artefact. The 19 empty suspects, one short shell and the uncaptured flag show no omitted plan body or unjustified exclusion; the three sof... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nhh | whole_text | empty_or_failed | high | widespread | nhh_BAN427-1_2025_autumn_1; nhh_FIE420-1_2025_autumn_1; n... | For current-plan pilot audits, trace the same fetched page through its actual build academic-year assignment before reporting plan availa... |
| ✓ | vid | assessment | junk_included | low | rare | vid_BAVER2030-1_2024_autumn_1 | Confirm the number against the stored BAVER2030 article block, then remove the confirmed artefact with a narrowly scoped VID postprocesso... |
