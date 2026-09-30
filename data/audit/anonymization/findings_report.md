# anonymization audit — aggregated findings

7 findings across 3 institutions (from 3 agent reports in `data/audit/anonymization/findings`; model: opus).
Mechanical verification: 7 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
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
| text_corruption | low | 3 |
| admin_date_left | medium | 2 |
| boilerplate_left | low | 1 |
| boilerplate_left | medium | 1 |

## By target

| target | n |
| --- | --- |
| admin_date | 4 |
| boilerplate | 2 |
| structure | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| mf | opus | 22 | Personal data is removed reliably for MF. Every offering (51/51 in a full-data scan) ends in the same 'Emneansvarlig' contact block, and .anon_mf() cuts it off together with the names, e-mails and marketing text. No names, e-mails or titles are left, and no course content is removed. The main gap... |
| nih | opus | 8 | Anonymization is safe for NIH. The packet and a scan of all 81 offerings found no names, e-mails or phone numbers, and nothing was over-removed (only whitespace changes). The one real gap is administrative semester-year stamps in inflected form ('høsten 2024', 'våren 2025', 'høsten2025') on the r... |
| uio | opus | 12 | Anonymization works well for UiO: the staff names and e-mails in GEO5930L/MAT5930L are removed, the 8 random controls have no PII, admin dates or over-removal, and the name_like flags (ENG4790, EDID4009) are false alarms caused by course titles and the name of the APA manual. The artifact flags a... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | mf | admin_date | admin_date_left | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_PPU1020-1_2025_autumn_1; ... | In .anon_mf(), strip the whole block before the generic step: stringr::str_remove("(?m)^Eksamensdatoer\\s*\\n[\\s\\S]*?(?=^Læringsutbytte... |
| ✓ | uio | boilerplate | boilerplate_left | medium | widespread | uio_PROMO2-1_2025_autumn_4; uio_HIS4095L-1_2025_spring_1;... | In .anon_uio(), strip the block as the tail of the text. It is always the last block (290 chars in Norwegian, 359 in English): str_remove... |
| ✓ | nih | admin_date | admin_date_left | medium | common | nih_LKI236-1_2025_spring_1; nih_LKI510-1_2025_autumn_2 | Add .anon_nih() to the switch in .anon_institution() with str_replace_all("(?mi)^(Pensumlist[ea]\|Anbefalt litteratur) for (?:høsten\|våren... |
| ✓ | mf | boilerplate | boilerplate_left | low | widespread | mf_RL1016-1_2025_autumn_1; mf_SAM1050-1_2025_autumn_1; mf... | In .anon_mf(), add stringr::str_remove("Tilgang til litteratur\\s*\\n[\\s\\S]*?lokale folkebibliotek\\.\\s*") and stringr::str_remove_all... |
| ✓ | mf | admin_date | text_corruption | low | occasional | mf_PPU1010-1_2025_autumn_1 | In .anon_generic(), extend the lookbehind alternative to take an optional coordinated second season: (?i)(?<=(?:semester\|undervisning\|opp... |
| ✓ | uio | structure | text_corruption | low | occasional | uio_GEO5930L-2_2025_spring_1; uio_MAT5930L-2_2025_spring_1 | In .anon_generic(), before the bare-email regex, remove a parenthesised e-mail as one unit: str_remove_all("\\s*\\(\\s*[\\w.+-]+@[\\w.-]+... |
| ✓ | mf | admin_date | text_corruption | low | rare | mf_SAM2080L-1_2025_spring_1 | In .anon_generic(): (1) add a trailing \b to the season group of the lookbehind rule, and use [: \t]{0,3} instead of [:\s]{0,3} so it can... |
