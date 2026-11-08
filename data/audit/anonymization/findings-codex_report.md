# anonymization audit — aggregated findings

41 findings across 17 institutions (from 19 agent reports in `data/audit/anonymization/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 37 passed (✓), 4 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

| institution | target | error_type | severity | problem | evidence |
| --- | --- | --- | --- | --- | --- |
| hiof | person_name | pii_leak | high | evidence not_found | Lecturer: [name], [name], [name], [name] ([name], [name] |
| uib | person_name | pii_leak | high | evidence not_found | Faglig ansvar Hovedansvarlige: [name] [name] Innledere Dr. [name] Dr. [name] |
| himolde | person_name | pii_leak | medium | evidence not_found | Lecturer:[name], Technical University of Denmark |
| usn | person_name | pii_leak | medium | evidence not_found | Course Owner: Professor [name] Lecturer: Associate Professor [name] |

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 12 | fhs, himolde, hiof, hvl, inn, khio, mf, nhh, nih, nla, uib, uio | medium |
| content_text | text_corruption | 9 | fhs, himolde, hiof, mf, nhh, nih, nmbu, uib, uio | medium |
| content_text | over_removal | 6 | hiof, nhh, nla, oslomet, phs, uis | medium |
| person_name | pii_leak | 4 | himolde, hiof, uib, usn | high |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| himolde | 3 | 3 | person_name / pii_leak; admin_date / admin_date_left; content_text / text_corruption |  |  |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| admin_date_left | medium | 11 |
| over_removal | medium | 8 |
| text_corruption | low | 8 |
| admin_date_left | low | 6 |
| boilerplate_left | low | 2 |
| pii_leak | high | 2 |
| pii_leak | medium | 2 |
| text_corruption | medium | 2 |

## By target

| target | n |
| --- | --- |
| admin_date | 17 |
| content_text | 17 |
| person_name | 4 |
| boilerplate | 2 |
| email | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 28 | All 28 packet entries were read in full: administrative study-year values survive in every plan, including all 12 random controls, and semester removal breaks the literature-list sentence in four plans. No confirmed personal-data leaks were found: phone flags concern numeric identifiers in refere... |
| himolde | gpt-6.1-sol | 28 | Reviewed all 20 suspects and 8 random controls: lecturer names remain in two suspects, while the other 18 name_like flags are false alarms caused by course terminology, programme titles and organisation names. Semester labels and the email address are correctly removed, and reference authors, pub... |
| hiof | gpt-6.1-sol | 28 | All 28 packet entries were read in full as supplied, including their removed spans; packet truncation limits visibility beyond the displayed material. Standard coordinator blocks, emails and FS timestamps are removed successfully, and none of the eight random controls shows a confirmed defect (co... |
| hvl | gpt-6.1-sol | 28 | All 28 courses were read in full; no personal-data leaks, course-content over-removal, or structure loss were found. The phone flag is the content year range 20102014, and the name_like flags identify course/programme names or organisations such as Nautical Institute, Ungt Entreprenørskap and Ole... |
| inn | gpt-6.1-sol | 28 | Anonymization generally works well: all 28 courses were reviewed in full, with no confirmed personal-data leaks, course-content over-removal, or structure damage, and all eight random controls were clean. Two offerings retain compact administrative overlap dates and two retain technical error ban... |
| khio | gpt-6.1-sol | 28 | All 28 courses were read in full, including both suspects and 26 random controls. Both name_like flags are false alarms caused by capitalized course, event and institution names; no private-person names, contact details, content removal or anonymization damage were found, and all courses have zer... |
| mf | gpt-6.1-sol | 8 | All eight random controls were read in full; the packet contains no suspects and the pre-pass flags no offerings. No personal data leaks or inappropriate removal of substantive course content were found, and exam-date blocks were correctly removed. However, yearless administrative withdrawal dead... |
| nhh | gpt-6.1-sol | 28 | All 28 plans and their removed spans were reviewed: no personal-data leaks were found, semester-year labels and the contact email were removed, and literature authors, publication years, NOU identifiers and paragraph structure were generally preserved. The pre-pass flags were false alarms: name-l... |
| nih | gpt-6.1-sol | 28 | All 28 courses were reviewed in full, including 20 suspects and eight random controls. No personal-data leaks or substantive course-content losses were found: the five removed flags reflect correct semester removal from short plans, and all name_like flags concern departments, organisations, prog... |
| nla | gpt-6.1-sol | 28 | All 28 packet entries were read in full as supplied, including every removed span; packet truncation limits conclusions about omitted portions of long plans. Academic-year headers are consistently removed, no personal-data leaks were confirmed, and all eight random controls show no confirmed anon... |
| nmbu | gpt-6.1-sol | 8 | Anonymization works well in all eight random controls: staff names, including multiple coordinators, and administrative academic years are removed, with no visible personal-data leaks or inappropriate course-content removal. All pre-pass flag counts are zero; manual review found one minor incompl... |
| oslomet | gpt-6.1-sol | 28 | All 28 packet courses were reviewed, including all 20 suspects and eight random controls; no personal-data leak was confirmed in the displayed material, and coordinator names are correctly removed in both suspects and controls. Most flags are false alarms: name_like hits are course/programme titl... |
| phs | gpt-6.1-sol | 28 | Anonymization generally works well across all 28 packet records: no personal-data leaks, residual administrative dates, or removal-induced text/structure damage were observed in the supplied text. All five suspect flags are false alarms: four removal-share flags reflect correct semester-year remo... |
| steiner | gpt-6.1-sol | 8 | All eight random-control courses were read in full, including every removed span; no anonymization problems were found. The pre-pass raised no flags, and manual review found no surviving personal data, administrative dates, targeted boilerplate, content over-removal, text corruption or structure ... |
| uib | gpt-6.1-sol | 28 | All 28 packet courses were reviewed in full as presented, including every removal span. Staff names and administrative course dates each survive in nine suspect offerings, concentrated in clinical PhD courses plus a law teaching list; none of the eight random controls has these leaks, so the samp... |
| uio | gpt-6.1-sol | 10 | All 10 packet entries were reviewed in full, including all displayed removed spans; HIS1400L is capped by the packet builder. Both name_like flags are false positives from course titles and organisation/publication names, and no personal-data leaks were found in suspects or controls. Email and da... |
| uis | gpt-6.1-sol | 28 | All 28 packet entries were reviewed in full, including their displayed removed spans; no confirmed personal-data leaks or residual administrative date stamps were found. The suspect flags are false alarms in the displayed material: phone/name_like matches are bibliographic numbers, links, authors... |
| usn | gpt-6.1-sol | 28 | All 28 packet entries were read in full, including all displayed removed spans; packet truncations limit conclusions about omitted text. Anonymization generally preserves course content and removes approval blocks and semester dating, but two Systems Thinking offerings retain staff names under En... |
| vid | gpt-6.1-sol | 28 | No anonymization defects were found after reading all 28 courses in full, including the 23 random controls: no institutional staff names, personal contact details, administrative dates, or targeted boilerplate remain. All five pre-pass flags are false alarms: BASOB2032 cites the government regula... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✗ | hiof | person_name | pii_leak | high | common | hiof_SFB50314-1_2016_spring_1; hiof_SFB50912-1_2017_autum... | In .anon_hiof(), remove complete line-anchored Foreleser/Forelesere/Lecturer/Lecturers contact lines, allowing optional semester or acade... |
| ✗ | uib | person_name | pii_leak | high | common | uib_NEUROSYSM930-0_2024_autumn_1; uib_CCBIONEUR911-0_2025... | Add bounded UiB staff-section rules for Faglig ansvar, Hovedansvarlige, Innledere and Undervisere that consume all staff entries until th... |
| ✓ | fhs | admin_date | admin_date_left | medium | widespread | fhs_LED4101-1_2022_autumn_1; fhs_CBU1506-1_2019_autumn_2;... | Add an FHS handler or a narrowly anchored generic rule matching a Studieår label followed by a standalone four-digit year on the next lin... |
| ✓ | khio | admin_date | admin_date_left | medium | widespread | khio_BK306-1_2025_autumn_1; khio_DA550-2_2025_autumn_1; k... | Add and register .anon_khio() in R/anonymize.R with a multiline rule scoped to the programme-name line immediately following 'Inngår i st... |
| ✓ | mf | admin_date | admin_date_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), match yearless Norwegian day-and-month dates, including slash-separated alternatives, specifically in clauses about withdr... |
| ✓ | uib | admin_date | admin_date_left | medium | common | uib_NEUROSYSM930-0_2024_autumn_1; uib_CCBIO906-0_2023_spr... | Extend .anon_uib() with context-bound removal for Norwegian month-name dates and day ranges in Undervisningsperiode, dated Undervisningss... |
| ✗ | himolde | person_name | pii_leak | medium | occasional | himolde_LOG904-130-1_2018_autumn_1; himolde_IDR804-019-1_... | Add and register a .anon_himolde() handler that removes a line beginning Lecturer: with an inline value, or a standalone Lecturer: headin... |
| ✓ | hiof | admin_date | admin_date_left | medium | occasional | hiof_LSPUBPD15-1_2015_autumn_2; hiof_ITF50020-1_2021_spri... | Add contextual HiØ rules for oppstartsamling i <month> <year> and Litteratur vil være tilgjengelig innen <date>. Recognize a semester/yea... |
| ✓ | hiof | content_text | over_removal | medium | occasional | hiof_LURLE20213-1_2017_spring_1; hiof_LRLE20213-1_2017_sp... | Protect full URL tokens before date removal. Within HiØ literature sections, retain dates/ranges attached to bibliographic entries and ap... |
| ✓ | hvl | admin_date | admin_date_left | medium | occasional | hvl_ANS800-1_2023_spring_1; hvl_NYB801-1_2021_spring_2; h... | Add context-bound removal of named-month dates and day ranges in teaching-session, course-start and application-deadline clauses, coverin... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_BEA512-1_2025_autumn_1; nhh_ACC421E-1_2025_autumn_1; ... | Add narrowly scoped scheduling rules for For YYYY it will be at location clauses, due date: English-month ordinal expressions, and from w... |
| ✓ | nhh | admin_date | admin_date_left | medium | occasional | nhh_ECN434-1_2025_autumn_1; nhh_FIN547-1_2025_autumn_1 | Extend clock removal with context-constrained dot-separated hours/minutes and am/pm ranges, using cues such as at, from, exam, classes an... |
| ✓ | nih | admin_date | admin_date_left | medium | occasional | nih_LKI106-1_2024_spring_2; nih_KRØ101-1_2023_autumn_1; n... | Strip terminal parenthesised offering semesters from identifiable course-title lines before applying the government-document date guard. ... |
| ✓ | nih | admin_date | admin_date_left | medium | occasional | nih_PHD641-1_2025_autumn_1; nih_PHD642-1_2025_autumn_1; n... | Add a line-scoped rule for Påmeldingsfrist followed by .long_dates_regex, removing the deadline value and terminal punctuation while pres... |
| ✓ | nih | admin_date | admin_date_left | medium | occasional | nih_FRI300-1_2024_autumn_1; nih_FRI301-1_2024_autumn_1; n... | Recognise case-insensitive fall alongside autumn in the season-year removal patterns, with a word boundary and adjacent year. Remove any ... |
| ✓ | nla | content_text | over_removal | medium | occasional | nla_3KL106-1_2020_spring_1; nla_3KL104-1_2020_spring_1; n... | Protect parenthesised m:ss or mm:ss durations following video-reference URLs or explicit video/duration cues before applying clock remova... |
| ✗ | usn | person_name | pii_leak | medium | occasional | usn_PN-SYS9000-1_2023_spring_1; usn_PN-SYS9000-1_2021_spr... | Add a USN rule matching the complete staff-only line beginning Course Owner: and containing Lecturer:, or extend the role-aware generic r... |
| ✓ | hiof | content_text | over_removal | medium | rare | hiof_LURLE20213-1_2016_spring_1 | Bound the HiØ literature-update rule to the matched update phrase and its date, with an optional terminating full stop, instead of consum... |
| ✓ | mf | content_text | text_corruption | medium | rare | mf_SAM2080L-1_2025_autumn_1 | Before generic date removal, add a scoped .anon_mf() replacement for a sentence-initial semester-year phrase followed by har and the cour... |
| ✓ | nhh | content_text | over_removal | medium | rare | nhh_FIE448-1_2025_autumn_1 | Protect year ranges directly modifying historical-event terms such as financial crisis before applying .drop_academic_year(); for ambiguo... |
| ✓ | nla | content_text | over_removal | medium | rare | nla_BLKO4-1_2019_spring_1 | Protect dates within a recognised government-document reference, including a date on an immediately following line after a ministry/UDIR ... |
| ✓ | oslomet | content_text | over_removal | medium | rare | oslomet_ARK1400-1_2018_spring_2 | Protect complete URL tokens from generic date removal, or exclude matches whose positions fall inside URL spans, while retaining removal ... |
| ✓ | phs | content_text | over_removal | medium | rare | phs_AVMISBA-1_2025_spring_1 | Extend the government-document guard to recognize Riksadvokaten as an official publisher and preserve season-year publication dates in bo... |
| ✓ | uio | content_text | text_corruption | medium | rare | uio_SPA4192-1_2025_autumn_1 | Add a deterministic .anon_uio() rule before generic date removal that handles these two cohort-qualified examination sentences together, ... |
| ✓ | uis | content_text | over_removal | medium | rare | uis_MGL2090-1_2018_autumn_2 | Protect reference entries and survey-period ranges before the generic academic-year substitutions. For an otherwise ambiguous consecutive... |
| ✓ | fhs | content_text | text_corruption | low | common | fhs_ING3506-1_2025_autumn_1; fhs_MILM1704-1_2025_autumn_2... | Add a narrowly scoped cleanup after semester removal that rewrites Gjeldende litteraturliste for finner du i Leganto to Gjeldende littera... |
| ✓ | himolde | content_text | text_corruption | low | common | himolde_IBE321-1_2025_autumn_1; himolde_TRA730-1_2025_aut... | For this exact reading-list notice, remove for together with its semester-year value, yielding The current reading list can be found in L... |
| ✓ | inn | admin_date | admin_date_left | low | occasional | inn_2SPGL1-1-1_2025_spring_1; inn_2SPGL1-2-1_2025_autumn_1 | In .anon_inn(), identify the Dato column within Emnet overlapper med tables and remove valid six-digit ddmmyy cell values, preserving the... |
| ✓ | inn | boilerplate | boilerplate_left | low | occasional | inn_2MNK172-2-1_2022_spring_1; inn_2MNK172S-2-1_2022_spri... | Add a narrowly anchored .anon_inn() rule removing the Feilmelding heading and following Notice: Undefined index diagnostic line for eac_c... |
| ✓ | nih | content_text | text_corruption | low | occasional | nih_PHD646-1_2025_spring_1; nih_PHD641-1_2025_autumn_1; n... | Extend the anchored post-semester cleanup to complete lines consisting only of Kjernelitteratur for or Anbefalt litteratur for, optional ... |
| ✓ | uib | email | text_corruption | low | occasional | uib_ENERGI322-0_2024_autumn_1; uib_ENERGI322-0_2023_spring_1 | Use Unicode-aware local-part and address boundaries, explicitly handle concatenated adviser addresses, and repeat address cleanup to a fi... |
| ✓ | uib | content_text | text_corruption | low | occasional | uib_GOV216-0_2023_spring_2; uib_INF367-0_2025_autumn_1; u... | Remove enclosing parentheses when their entire contents are an administrative semester label, including parentheses attached to a precedi... |
| ✓ | uib | boilerplate | boilerplate_left | low | occasional | uib_INFO284-0_2022_autumn_1; uib_MOL100-0_2024_autumn_1 | Extend the anchored UiB publication-notice patterns to cover vil vere klar followed directly by a date and the observed English ready bef... |
| ✓ | himolde | admin_date | admin_date_left | low | rare | himolde_LOG904-140-1_2022_autumn_1 | Add a colon-time rule accepting a single-digit hour when explicitly introduced by a scheduling phrase such as starting at; remove the phr... |
| ✓ | hiof | admin_date | admin_date_left | low | rare | hiof_HSASO20118-1_2020_autumn_1 | Remove complete parenthesized Lesedato/accessed/retrieved-date fields before government-date guards and clock removal, accepting the obse... |
| ✓ | hiof | content_text | text_corruption | low | rare | hiof_LSPUBPD15-1_2015_autumn_2 | For coordinated season expressions in course scheduling, remove the year while retaining the seasonal duration (høst- og tidlig vårsemest... |
| ✓ | hvl | admin_date | admin_date_left | low | rare | hvl_NYB801-1_2021_spring_2 | Extend the kl./klokka clock-range pattern to accept an hour-only start when followed by a range separator and a valid clock endpoint, inc... |
| ✓ | nhh | content_text | text_corruption | low | rare | nhh_FIE401-1_2025_autumn_1 | Before deleting individual clock values, rewrite the specific between TIME at/on DAY and TIME at/on DAY construction as from DAY to DAY, ... |
| ✓ | nla | admin_date | admin_date_left | low | rare | nla_3GJ302-1_2020_spring_1 | Add an anchored, case-insensitive rule for standalone English revision/update stamps such as Revised May 2019, supporting English month n... |
| ✓ | nmbu | content_text | text_corruption | low | rare | nmbu_PPRA201-1_2025_autumn_1 | Before generic year removal, add a narrowly anchored rule in .anon_nmbu() to remove the complete sentence beginning “Det vil imidlertid o... |
| ✓ | uio | admin_date | admin_date_left | low | rare | uio_NOR4091-1_2025_autumn_1 | In .anon_uio(), strip day-and-written-month date lists following the anchored Innleveringsfrister label, including lists joined by og and... |
