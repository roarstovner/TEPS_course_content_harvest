# anonymization audit — aggregated findings

8 findings across 4 institutions (from 5 agent reports in `data/audit/anonymization/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 8 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 3 | fhs, khio, nhh | medium |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| admin_date_left | medium | 4 |
| over_removal | medium | 2 |
| text_corruption | low | 2 |

## By target

| target | n |
| --- | --- |
| admin_date | 4 |
| content_text | 4 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 28 | All 28 packet entries were read in full: administrative study-year values survive in every plan, including all 12 random controls, and semester removal breaks the literature-list sentence in four plans. No confirmed personal-data leaks were found: phone flags concern numeric identifiers in refere... |
| khio | gpt-6.1-sol | 28 | All 28 courses were read in full, including both suspects and 26 random controls. Both name_like flags are false alarms caused by capitalized course, event and institution names; no private-person names, contact details, content removal or anonymization damage were found, and all courses have zer... |
| nhh | gpt-6.1-sol | 28 | All 28 plans and their removed spans were reviewed: no personal-data leaks were found, semester-year labels and the contact email were removed, and literature authors, publication years, NOU identifiers and paragraph structure were generally preserved. The pre-pass flags were false alarms: name-l... |
| phs | gpt-6.1-sol | 28 | Anonymization generally works well across all 28 packet records: no personal-data leaks, residual administrative dates, or removal-induced text/structure damage were observed in the supplied text. All five suspect flags are false alarms: four removal-share flags reflect correct semester-year remo... |
| vid | gpt-6.1-sol | 28 | No anonymization defects were found after reading all 28 courses in full, including the 23 random controls: no institutional staff names, personal contact details, administrative dates, or targeted boilerplate remain. All five pre-pass flags are false alarms: BASOB2032 cites the government regula... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fhs | admin_date | admin_date_left | medium | widespread | fhs_LED4101-1_2022_autumn_1; fhs_CBU1506-1_2019_autumn_2;... | Add an FHS handler or a narrowly anchored generic rule matching a Studieår label followed by a standalone four-digit year on the next lin... |
| ✓ | khio | admin_date | admin_date_left | medium | widespread | khio_BK306-1_2025_autumn_1; khio_DA550-2_2025_autumn_1; k... | Add and register .anon_khio() in R/anonymize.R with a multiline rule scoped to the programme-name line immediately following 'Inngår i st... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_BEA512-1_2025_autumn_1; nhh_ACC421E-1_2025_autumn_1; ... | Add narrowly scoped scheduling rules for For YYYY it will be at location clauses, due date: English-month ordinal expressions, and from w... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_ECN434-1_2025_autumn_1; nhh_FIN547-1_2025_autumn_1 | Extend clock removal with context-constrained dot-separated hours/minutes and am/pm ranges, using cues such as at, from, exam, classes an... |
| ✓ | nhh | content_text | over_removal | medium | rare | nhh_FIE448-1_2025_autumn_1 | Protect year ranges directly modifying historical-event terms such as financial crisis before applying .drop_academic_year(); for ambiguo... |
| ✓ | phs | content_text | over_removal | medium | rare | phs_AVMISBA-1_2025_spring_1 | Extend the government-document guard to recognize Riksadvokaten as an official publisher and preserve season-year publication dates in bo... |
| ✓ | fhs | content_text | text_corruption | low | common | fhs_ING3506-1_2025_autumn_1; fhs_MILM1704-1_2025_autumn_2... | Add a narrowly scoped cleanup after semester removal that rewrites Gjeldende litteraturliste for finner du i Leganto to Gjeldende littera... |
| ✓ | nhh | content_text | text_corruption | low | rare | nhh_FIE401-1_2025_autumn_1 | Before deleting individual clock values, rewrite the specific between TIME at/on DAY and TIME at/on DAY construction as from DAY to DAY, ... |
