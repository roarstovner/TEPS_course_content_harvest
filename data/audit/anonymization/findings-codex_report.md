# anonymization audit — aggregated findings

5 findings across 2 institutions (from 3 agent reports in `data/audit/anonymization/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 5 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| admin_date_left | medium | 3 |
| over_removal | medium | 1 |
| text_corruption | low | 1 |

## By target

| target | n |
| --- | --- |
| admin_date | 3 |
| content_text | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| khio | gpt-6.1-sol | 28 | All 28 courses were read in full, including both suspects and 26 random controls. Both name_like flags are false alarms caused by capitalized course, event and institution names; no private-person names, contact details, content removal or anonymization damage were found, and all courses have zer... |
| nhh | gpt-6.1-sol | 28 | All 28 plans and their removed spans were reviewed: no personal-data leaks were found, semester-year labels and the contact email were removed, and literature authors, publication years, NOU identifiers and paragraph structure were generally preserved. The pre-pass flags were false alarms: name-l... |
| vid | gpt-6.1-sol | 28 | No anonymization defects were found after reading all 28 courses in full, including the 23 random controls: no institutional staff names, personal contact details, administrative dates, or targeted boilerplate remain. All five pre-pass flags are false alarms: BASOB2032 cites the government regula... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | khio | admin_date | admin_date_left | medium | widespread | khio_BK306-1_2025_autumn_1; khio_DA550-2_2025_autumn_1; k... | Add and register .anon_khio() in R/anonymize.R with a multiline rule scoped to the programme-name line immediately following 'Inngår i st... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_BEA512-1_2025_autumn_1; nhh_ACC421E-1_2025_autumn_1; ... | Add narrowly scoped scheduling rules for For YYYY it will be at location clauses, due date: English-month ordinal expressions, and from w... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_ECN434-1_2025_autumn_1; nhh_FIN547-1_2025_autumn_1 | Extend clock removal with context-constrained dot-separated hours/minutes and am/pm ranges, using cues such as at, from, exam, classes an... |
| ✓ | nhh | content_text | over_removal | medium | rare | nhh_FIE448-1_2025_autumn_1 | Protect year ranges directly modifying historical-event terms such as financial crisis before applying .drop_academic_year(); for ambiguo... |
| ✓ | nhh | content_text | text_corruption | low | rare | nhh_FIE401-1_2025_autumn_1 | Before deleting individual clock values, rewrite the specific between TIME at/on DAY and TIME at/on DAY construction as from DAY to DAY, ... |
