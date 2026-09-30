# fulltext audit — aggregated findings

8 findings across 3 institutions (from 3 agent reports in `data/audit/fulltext/findings`; model: sonnet).
Mechanical verification: 8 passed (✓), 0 failed (✗), 0 not checkable (?, packet or sample.csv missing).
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
| junk_included | low | 2 |
| missing_content | low | 2 |
| empty_or_failed | medium | 1 |
| formatting | low | 1 |
| missing_content | medium | 1 |
| wrong_page | high | 1 |

## By target

| target | n |
| --- | --- |
| whole_text | 3 |
| metadata | 2 |
| page_chrome | 2 |
| reading_list | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| nih | sonnet | 10 | Extraction with .fs-body is clean and faithful for the fetched pages: no chrome, no wrong-year or wrong-page content, and uncaptured text is only breadcrumb, title, contact name and FS timestamp. The two 'short' suspects are false alarms (courses genuinely lack a 'Kort om emnet' section). Remaini... |
| steiner | sonnet | 8 | Steiner text comes from PDF course plans and is largely complete (learning outcomes, content, work requirements, assessment all present) with a consistent facts box. Problems are one clear wrong-course mismatch and recurring PDF artefacts (page numbers, hard line wraps, stray page-break blank lin... |
| uio | sonnet | 8 | UiO extraction works well: all 8 fetched pages yield the full plan (description, learning outcomes, admission, teaching, exam) with readable structure, and no pre-pass flags fired. The only systematic defect is a generic UiO-wide exam-links boilerplate block appended to every text; the facts box ... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | steiner | whole_text | wrong_page | high | rare | steiner_M-PEL2.2_2025_spring_1 | In the pdf_split routine, verify that the 'Emnekode og emnenavn' line in the split text contains the requested code (normalised, e.g. PEL... |
| ✓ | nih | reading_list | missing_content | medium | common | nih_LKI221-1_2024_autumn_1; nih_LKI105-1_2023_spring_1; n... | Inspect the raw html of a course for a literature link/iframe; if a URL exists, fetch it in a custom strategy, or at least drop empty tra... |
| ✓ | nih | whole_text | empty_or_failed | medium | common | nih_LKI152-1_2024_spring_2 | Check the 404 list by language and status; for ENG courses try the English site path (e.g. nih.no/en/studies/courses/...) in R/add_course... |
| ✓ | nih | metadata | missing_content | low | widespread | nih_LKI105-1_2023_spring_1; nih_LKI510-1_2025_autumn_2 | Check the page HTML for the facts-box container and switch nih to selector_mode 'multi' with both .fs-body and the facts-box selector, if... |
| ✓ | steiner | page_chrome | junk_included | low | widespread | steiner_M-NAT1_2_2025_spring_1; steiner_M-NOR1_1_2025_spr... | Add a post-processing step for steiner that removes lines matching ^\s*\d{1,3}\s*$ and rejoins the text across the page break. |
| ✓ | steiner | whole_text | formatting | low | widespread | steiner_M-SAM1_1_2025_spring_1; steiner_M-MAT1_1_2025_spr... | Add a post_fn that joins wrapped lines within a paragraph (line not ending in punctuation followed by lowercase start), dehyphenates, nor... |
| ✓ | uio | page_chrome | junk_included | low | widespread | uio_PROMO5-1_2025_spring_3; uio_SVLEP1000-1_2025_spring_1... | Add a post_fn for uio in R/institution_config.R that removes everything from the line 'Mer om eksamen ved UiO' to the end of the text (in... |
| ✓ | uio | metadata | missing_content | low | widespread | uio_SVLEP1000-1_2025_spring_1; uio_TYSK4091-1_2025_spring_1 | Only if facts-box data is wanted from the page: add a second selector (selector_mode multi) for the UiO course-facts element; otherwise r... |
