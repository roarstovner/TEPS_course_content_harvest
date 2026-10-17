# anonymization audit — aggregated findings

34 findings across 16 institutions (from 18 agent reports in `data/audit/anonymization/findings`; model: opus).
Mechanical verification: 34 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

**Stale reports** (older than their packet — not from this run, or the agent never wrote its file): nih

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 9 | hiof, hivolda, inn, mf, nla, nord, uib, uit, usn | medium |
| boilerplate | boilerplate_left | 6 | hiof, hvl, inn, mf, ntnu, usn | medium |
| content_text | text_corruption | 5 | nla, nmbu, ntnu, uib, usn | medium |
| content_text | over_removal | 3 | hiof, nord, uis | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hivolda | 0 | 1 |  | admin_date / admin_date_left |  |
| hvl | 1 | 2 | boilerplate / boilerplate_left | admin_date / other |  |
| inn | 2 | 3 |  | all / other; boilerplate / boilerplate_left; admin_date / admin_date_left | content_text / text_corruption; boilerplate / other |
| nla | 0 | 2 |  | content_text / text_corruption; admin_date / admin_date_left |  |
| nord | 2 | 2 | content_text / over_removal; admin_date / admin_date_left |  |  |
| ntnu | 1 | 2 |  | boilerplate / boilerplate_left; content_text / text_corruption | content_text / over_removal |
| oslomet | 2 | 1 |  | person_name / pii_leak | admin_date / admin_date_left; content_text / other |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| text_corruption | low | 10 |
| admin_date_left | low | 9 |
| boilerplate_left | low | 5 |
| over_removal | low | 3 |
| admin_date_left | medium | 1 |
| boilerplate_left | medium | 1 |
| other | high | 1 |
| other | low | 1 |
| over_removal | medium | 1 |
| pii_leak | medium | 1 |
| text_corruption | medium | 1 |

## By target

| target | n |
| --- | --- |
| admin_date | 14 |
| content_text | 11 |
| boilerplate | 6 |
| all | 1 |
| email | 1 |
| person_name | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 28 | Personal data is removed reliably at HiOF. Every Emneansvarlig name in the packet is gone, whether one name or two, on the label line or the line below. The packet has no e-mail addresses or phone numbers, and a scan of all 820 published hiof plans finds none either. The phone and name_like flags... |
| hivolda | opus | 20 | Anonymization works well for HiVolda. None of the 20 plans keeps a staff name, e-mail or phone number, and a scan of all 667 offerings found none either. Apart from the functional address studentsorvis@hivolda.no, the only text removed is the semester-year value after 'Undervisningssemester:'/'Ek... |
| hvl | opus | 28 | Anonymization works well for HVL. None of the 28 plans keeps a name, e-mail, phone number or administrative date, and no course content was removed. All 11 name_like flags are false alarms: programme and course names, tool names, Ole Bull Akademiet, and in PHD911 scholars named as subject matter ... |
| inn | opus | 20 | No personal data in any of the 20 courses, and none in a scan of all 729 INN plans (0 e-mails, 0 phone numbers, staff labels only as generic roles: INN pages have no contact block). Semester-year lists are removed, and content years ('1896-1945', 'Cohen, 1996') are kept. The 'removed' flag (cours... |
| mf | opus | 8 | Personal data is removed reliably for MF. .anon_mf() cuts the trailing 'Emneansvarlig' contact block in all 24/26 offerings that have one (the other 2 have no contact block), including 'Etternavn, Fornavn' names, titles, e-mails and pages with several coordinators. The 'Eksamensdatoer' block is n... |
| nih | opus | 8 | Anonymization works well for NIH: none of the 8 packet courses (all random controls, since the pre-pass flagged nothing among the 81 offerings) contains names, e-mails, phone numbers or admin dates, and the only removals are semester-year stamps on the reading-list line ('Pensumliste for våren 20... |
| nla | opus | 28 | Anonymization works well for NLA. No personal data is left in the 28 courses (the truncated tails of 4 were read from plans recomputed with the current R/anonymize.R), and a scan of all 311 NLA plans finds no e-mails, phones, staff names, contact blocks or year-stamped semester labels. All 10 sus... |
| nmbu | opus | 8 | Anonymization works well for NMBU: in all 8 courses (and in the 4 other offerings with text, checked by rerunning anonymize_text() on extracted_text) the only personal data is the 'Emneansvarlig:Name' header line, which .anon_nmbu() removes, including lines with two names and four-part names. Not... |
| nord | opus | 28 | Anonymization works well for nord. None of the 28 courses, and none of the 176 pilot offerings in a separate scan, has a name, e-mail or phone number left: nord pages have no contact block, and 'emneansvarlig' and 'faglærer' occur only as roles. All 8 name_like flags are false alarms on reference... |
| ntnu | opus | 28 | No personal data or administrative dates are left in any of the 28 plans: the Kontaktinformasjon cut removes every coordinator and teacher name, including names with particles ('[navn]'), and the 'Undervisningsstart' lines and session semester labels ('- Høst 2019') go, while both 'Vurderingsordn... |
| oslomet | opus | 28 | In the 28 packet courses no personal data is left and nothing is over-removed. The one staff name (an 'Emneansvarlig' block, course 6) is stripped. Every removed span is an academic year, a semester or cohort label, or an approval date. All pre-pass flags are false alarms: name_like hits English ... |
| steiner | opus | 8 | Anonymization works correctly for Steiner: the plans (split from subject PDFs) contain no staff names, e-mails, phone numbers or administrative dates, and the only text .anon_steiner() removes is sequential bare PDF page numbers (e.g. 9/10/11, 12-15), with no course content lost and line structur... |
| uia | opus | 28 | Anonymization works well for UiA: in all 28 courses the course-coordinator names (single and multi-name 'Emneansvarlige:' blocks, hyphenated and three-part names), breadcrumb, '(Høst YYYY)' title stamps and the FS timestamp (Bokmål and Nynorsk) are removed, no e-mail, phone or admin date is left,... |
| uib | opus | 27 | There is no personal data in any UiB plan. The packet has none, and a search of all 1473 anonymized UiB offerings found 0 e-mails, 0 phone numbers and no role label followed by a name, because UiB's Emneansvarlig fields name committees and departments, not people. The pre-pass name_label and name... |
| uio | opus | 10 | Anonymization works well for UiO. None of the 10 sampled plans has personal data or admin dates left. Across all 47 offerings, the current code removes the staff names UiO pages do hold ('Ta kontakt med <navn> eller <navn>' in FYS5930L, '<navn> (<e-post>)' in GEO5930L/MAT5930L); the leak an earli... |
| uis | opus | 28 | Anonymization works well at uis. None of the 28 sampled plans has personal data left, and full-text scans of all 2728 plans find no e-mail, role+name, '(Role)' staff line or name-only line outside reading lists. All three staff-list layouts (HTML 'Fagperson(er)', PDF 'Fagpersoner'/'Fagpersonar' b... |
| uit | opus | 28 | Anonymization works well for UiT. None of the 28 sampled plans has personal data left in, and a scan of all 3154 UiT plans found no role-plus-name lines and no e-mail addresses. The one course leader in the sample ('Emneleder … @uit.no', PFF-1020 2018) was removed, as were exam dates and times, t... |
| usn | opus | 28 | None of the 28 sampled plans has personal data or an approval stamp left. The name_label, name_like and admin_date flags are false alarms: the journal title 'The Reading teacher', reference authors, an article title ending in 'Teacher Education', 'Godkjent av Dekan' with no name after it, and the... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | all | other | high | occasional | inn_INN3024-1_2023_autumn_1; inn_2M352-1_2023_spring_1 | Add page_year to the inn entry of R/institution_config.R, read from the shown version: its earliest 'Undervisningssemestre'/'Teaching sem... |
| ✓ | mf | boilerplate | boilerplate_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), replace the library rule with stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\s*\\n... |
| ✓ | uib | content_text | text_corruption | medium | widespread | uib_RELMIN643-0_2025_autumn_1; uib_HIS250L-0_2022_autumn_... | In .anon_generic(), extend the pattern to consume a whole season list after the label, for example (?i)(?<=(?:semester\|…)[:\s]{0,3})SEASO... |
| ✓ | hiof | admin_date | admin_date_left | medium | common | hiof_LMBKRLE10221-1_2022_spring_2; hiof_LMBPED40217-1_202... | In .anon_hiof(), change the pattern to (?m)^[ \t]*Litteratur\w*(?: er)?(?: sist)? (?:oppdater\|endret\|gjort tilgjengelig)[^\n]* so it also... |
| ✓ | hiof | content_text | over_removal | medium | occasional | hiof_LMUSAM11221-1_2022_spring_2; hiof_LMBNOR40117-1_2021... | Guard the pattern so it does not match next to a slash: (?<![/\w])\d{4}/\d{2,4}\b(?!/). Tested: it leaves 'files/2019/06/27/' and 'view/3... |
| ✓ | oslomet | person_name | pii_leak | medium | rare | oslomet_ACIT4080-1_2025_autumn_1 | (1) In .anon_oslomet(), replace the one-line rule with a rule for the whole block: str_remove('(?ms)^Emneansvarlige?[ \t]*\n.*?(?=^Emneov... |
| ✓ | uib | content_text | text_corruption | low | widespread | uib_ENG125-0_2018_spring_1; uib_HIS250L-0_2020_spring_1; ... | Before the whitespace cleanup at the end of .anon_generic(), add str_replace_all("(?m)^[ \\t]*[.,;:][ \\t]*", ""), str_replace_all("[ \\t... |
| ✓ | hiof | content_text | over_removal | low | common | hiof_LMUENG10417-1_2019_spring_2; hiof_LMUSAM11221-1_2022... | Exempt ranges in square brackets and ranges right after a document number: add the lookbehinds (?<!\[)(?<!(?:Meld\. St\. \|St\.meld\. nr\.... |
| ✓ | hvl | admin_date | other | low | common | hvl_MAS538-1_2025_autumn_1; hvl_PHD911-1_2025_autumn_1; h... | Make the rule consistent with its own intent. Add the English labels to the lookbehind: `(?<=(?i:semester of (?:instruction\|assessment)\|s... |
| ✓ | uib | content_text | text_corruption | low | common | uib_FRANDI111-0_2017_autumn_1; uib_NODI101-0_2017_spring_... | Match the whole word: (høst\|vår\|haust\|autumn\|spring\|sommer\|summer)(?:semester(?:et)?\|en\|a)?\b, followed by an optional [.,]. This removes... |
| ✓ | uit | admin_date | text_corruption | low | common | uit_LER-1402-1_2020_spring_1; uit_LER-2304-1_2024_spring_... | In .anon_generic(): (1) let the season+year rule consume a preceding `[ \t]*[-–,][ \t]*` when the label is the last thing on the line, an... |
| ✓ | usn | boilerplate | boilerplate_left | low | common | usn_MG2EN7-1_2022_autumn_1; usn_MG2EN6-1_2021_autumn_1; u... | In .anon_usn(), widen the lookahead to '(?=[\p{Lu}\d"«(‘’'“”]\|https?:)'. Add a second rule for lowercase-led titles, restricted to labels... |
| ✓ | hivolda | admin_date | admin_date_left | low | occasional | hivolda_MEI304-1_2022_autumn_1; hivolda_DRHS900-1_2016_au... | In .anon_generic() (or .anon_hivolda()), drop the bracketed year after a week reference and keep the weeks: stringr::str_replace_all("(?i... |
| ✓ | hvl | boilerplate | boilerplate_left | low | occasional | hvl_PHD911-1_2025_spring_1; hvl_BØA3100-1_2025_spring_1 | In .anon_hvl(), also remove (a) `(?m)^The (?:module\|course) is not offered in the academic year \d{4}/\d{4}\.?[ \t]*(?:\n\|$)` and (b) `[ ... |
| ✓ | inn | boilerplate | boilerplate_left | low | occasional | inn_6EV323-1_2025_spring_3; inn_2M352-1_2023_spring_1; in... | In .anon_inn(), before the semester-line rule: stringr::str_remove("^\\s*Statusmelding\\n(?:Kun tilgjengelig på[^\\n]*)\\n"). Add a test ... |
| ✓ | nla | content_text | text_corruption | low | occasional | nla_TAM301-1_2022_autumn_1; nla_IKS104-2_2025_autumn_2 | In .anon_generic(), add a guard like .not_gov: keep an academic-year range when 'gjennomført\|avlagt\|bestått\|tatt' plus up to 20 character... |
| ✓ | nmbu | content_text | text_corruption | low | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPFD201-1_2025_autumn_2 | Make the academic-year patterns also take the preceding horizontal whitespace, e.g. "[ \\t]*\\b\\d{4}/\\d{2,4}\\b" (and the same for the ... |
| ✓ | ntnu | boilerplate | boilerplate_left | low | occasional | ntnu_FY8918-1_2019_autumn_1; ntnu_ØK8000-1_2025_autumn_1;... | In .anon_ntnu(), extend the rule to the link label: stringr::str_remove_all("Andre sider om emnet\\s*(?:Faglærers emneside\\s*)?"). Add t... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2022_autumn_1; u... | In .anon_uia(), extend the title pattern to an optional range: str_remove_all("\\((?:Høst\|Vår\|Haust)\\s+\\d{4}(?:\\s*[–-]\\s*(?:Høst\|Vår\|... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2020_autumn_1; u... | In .anon_generic(), let the lookbehind rule consume a coordinated second season: '(?i)(?<=(?:semester\|undervisning\|oppstart\|startsemester... |
| ✓ | uib | admin_date | admin_date_left | low | occasional | uib_HIS250L-0_2020_spring_1; uib_HIS250L-0_2019_spring_1;... | Add to .anon_generic(): str_remove_all("(?i)\\b\\d{1,2}\\.\\s*(?:januar\|februar\|mars\|april\|mai\|juni\|juli\|august\|september\|oktober\|novembe... |
| ✓ | uio | email | text_corruption | low | occasional | uio_BIOS5000-1_2025_autumn_1 | In .anon_generic(), let the bare-email rule also remove the horizontal space before the address: str_remove_all("[ \\t]*\\b[\\w.+-]+@[\\w... |
| ✓ | uis | content_text | over_removal | low | occasional | uis_MGLT43-1_2025_autumn_1; uis_MGLT43-1_2023_autumn_1; u... | (a) In .anon_generic(), put the guard before the match: paste0(.not_gov, "\\b(?i:", .season_regex, ")\\s*\\d{4}\\b"), and do the same for... |
| ✓ | uit | admin_date | admin_date_left | low | occasional | uit_SAM-3981-1_2017_autumn_1 | Decide the policy and record it in methods.qmd. If cohort and plan-validity years count as administrative, add `gjelder fra(?: og med)?\|(... |
| ✓ | hiof | boilerplate | boilerplate_left | low | rare | hiof_LMBKRO10117-1_2019_spring_2 | In .anon_hiof(), before the generic pass, remove whole revision-note lines with str_remove_all("(?m)^[ \\t]*\\d{1,2}\\.\\d{1,2}\\.\\d{4}:... |
| ✓ | inn | admin_date | admin_date_left | low | rare | inn_IDTD2002-1_2025_autumn_1 | In .anon_inn(), remove a date that fills a tab-separated table cell: stringr::str_remove_all("(?m)(?<=\\t)\\d{1,2}\\.\\d{1,2}\\.(?:\\d{4}... |
| ✓ | mf | admin_date | admin_date_left | low | rare | mf_SAM2080L-1_2025_autumn_1 | In .anon_generic(), make two changes. (1) Before the lookbehind rule, add stringr::str_replace_all("(?i)\\b((?:høst\|vår\|haust)semester(?:... |
| ✓ | nla | admin_date | admin_date_left | low | rare | nla_3GJ322-1_2019_autumn_1; nla_GL5SA3-1_2017_autumn_1 | Add 'start in the' to the lookbehind alternatives of the season-after-label rule (a fixed-length alternative, so it is valid in ICU). Add... |
| ✓ | nord | content_text | over_removal | low | rare | nord_BE207E-1_2019_spring_3 | In that rule, (1) match the label as a word of its own and list the compounds that should still match. One way is to turn it into str_rep... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | Either accept it as a one-off typo in the source, or add a typo-tolerant rule to .anon_generic() that catches a year label closing a line... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | In .anon_generic(), remove a day.month date only when a weekday comes right before it, which never happens with credits or section number... |
| ✓ | ntnu | content_text | text_corruption | low | rare | ntnu_PG8301-1_2007_autumn_1 | Remove the clause with its label before the season rule, in .anon_generic() (the wording is not NTNU-specific): stringr::str_remove_all(p... |
| ✓ | usn | content_text | text_corruption | low | rare | usn_MG2EN3-1_2024_autumn_1; usn_MG1NO2-1_2022_spring_1 | In .anon_generic(), let both rules also take the horizontal space before the match: prefix the patterns with '[ \t]*'. The space after th... |
| ✓ | usn | admin_date | admin_date_left | low | rare | usn_MG2EN4-1_2020_spring_2 | In .anon_generic(), change the admin-year rule to: str_replace_all(paste0("((?i:opprettet\|oppdatert\|revidert\|vedtatt\|godkjent(?: og akkre... |
