# anonymization audit — aggregated findings

1 findings across 1 institutions (from 1 agent reports in `data/audit/anonymization/findings-closeout`; model: gpt-6.2-sol).
Mechanical verification: 1 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| over_removal | medium | 1 |

## By target

| target | n |
| --- | --- |
| content_text | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fjellhaug | gpt-6.2-sol | 28 | Across all 28 packet entries, including eight random controls, the visible course plans contained no confirmed personal data leaks or offering-specific administrative dates. The name_like flags picked up legitimate terms such as Old Testament and church or institution names, and the three admin_d... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fjellhaug | content_text | over_removal | medium | occasional | fjellhaug_BMM1006-1_2026_autumn_1; fjellhaug_BTM2517-DK1_... | Before clock stripping, temporarily mask numeric spans in Bible references introduced by recognized book names or abbreviations, includin... |
