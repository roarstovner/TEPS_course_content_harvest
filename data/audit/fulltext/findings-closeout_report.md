# fulltext audit — aggregated findings

2 findings across 1 institutions (from 1 agent reports in `data/audit/fulltext/findings-closeout`; model: gpt-6.2-sol).
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
| junk_included | low | 1 |
| wrong_page | high | 1 |

## By target

| target | n |
| --- | --- |
| page_chrome | 1 |
| whole_text | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fjellhaug | gpt-6.2-sol | 17 | The sampled current plans are generally readable and complete, and all eight random controls match their course codes. Eight of nine dup_code suspects show the expected base-code plan shared across offering variants, while BTM1003-1 receives the RLE1003N plan. Two Greek courses retain a read-more... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fjellhaug | whole_text | wrong_page | high | rare | fjellhaug_BTM1003-1_2026_autumn_1 | Check the fetched Fjellhaug facts-box code against the normalized offering code, allowing explicitly verified aliases. On mismatch, try t... |
| ✓ | fjellhaug | page_chrome | junk_included | low | occasional | fjellhaug_GRE1002-1_2026_autumn_1; fjellhaug_GRE1003-1_20... | Add a Fjellhaug DOM exclusion scoped to the disclosure trigger with normalized label Les mer. Keep the associated content panel and verif... |
