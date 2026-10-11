# anonymization audit — aggregated findings

29 findings across 14 institutions (from 18 agent reports in `data/audit/anonymization/findings`; model: opus).
Mechanical verification: 29 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

**Stale reports** (older than their packet — not from this run, or the agent never wrote its file): nih

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 7 | hiof, mf, nord, oslomet, uib, uit, usn | medium |
| boilerplate | boilerplate_left | 4 | hiof, hvl, mf, usn | medium |
| content_text | over_removal | 4 | hiof, nord, ntnu, uis | medium |
| content_text | text_corruption | 4 | inn, nmbu, uib, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hvl | 1 | 1 |  | boilerplate / boilerplate_left | boilerplate / text_corruption |
| nla | 2 | 0 |  |  | admin_date / admin_date_left; content_text / over_removal |
| ntnu | 1 | 1 | content_text / over_removal |  |  |
| oslomet | 2 | 2 | admin_date / admin_date_left | content_text / other | boilerplate / boilerplate_left |
| uio | 1 | 1 | email / text_corruption |  |  |
| uis | 2 | 1 | content_text / over_removal |  | admin_date / admin_date_left |
| uit | 4 | 2 | admin_date / admin_date_left | admin_date / text_corruption | person_name / pii_leak; content_text / text_corruption; content_text / over_removal |
| usn | 3 | 3 | boilerplate / boilerplate_left; admin_date / admin_date_left | content_text / text_corruption | content_text / over_removal |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| text_corruption | low | 8 |
| admin_date_left | low | 6 |
| over_removal | low | 4 |
| boilerplate_left | low | 3 |
| over_removal | medium | 2 |
| text_corruption | medium | 2 |
| admin_date_left | medium | 1 |
| boilerplate_left | medium | 1 |
| other | high | 1 |
| other | low | 1 |

## By target

| target | n |
| --- | --- |
| content_text | 13 |
| admin_date | 10 |
| boilerplate | 5 |
| email | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 28 | Personal data is removed reliably at HiOF. Every Emneansvarlig name in the packet is gone, whether one name or two, on the label line or the line below. The packet has no e-mail addresses or phone numbers, and a scan of all 820 published hiof plans finds none either. The phone and name_like flags... |
| hivolda | opus | 20 | Anonymization works well for HiVolda: none of the 20 plans keeps any personal data, the only text removed is the semester-year value after 'Undervisningssemester:'/'Eksamenssemester:', and content dates ('22. juli 2011', lovdata id '2014-12-04-1697', word counts, reference years) are kept; a scan... |
| hvl | opus | 28 | Anonymization works well for HVL. None of the 28 packet courses keeps a person's name, e-mail, phone number or administrative date stamp, and none lost course content: reference editors ('Eilifsen, Margareth (red.)'), 'Kunnskapsløftet 2020', 'fagfornyinga (2020)', 'One Ocean 2021' and LK20 all st... |
| inn | opus | 26 | No personal data or administrative dates are left in any of the 26 courses: INN plans have no staff or contact blocks. Semester-year labels and overlap-table dates are removed, and content years ('Læreplan for KRLE 2015', '2500-3000 ord') are kept. The pre-pass flags are all false alarms. All 56 ... |
| mf | opus | 8 | Personal data is removed reliably for MF. .anon_mf() cuts the trailing 'Emneansvarlig' contact block in all 24/26 offerings that have one (the other 2 have no contact block), including 'Etternavn, Fornavn' names, titles, e-mails and pages with several coordinators. The 'Eksamensdatoer' block is n... |
| nih | opus | 8 | Anonymization works well for NIH: none of the 8 packet courses (all random controls, since the pre-pass flagged nothing among the 81 offerings) contains names, e-mails, phone numbers or admin dates, and the only removals are semester-year stamps on the reading-list line ('Pensumliste for våren 20... |
| nla | opus | 12 | Anonymization works well for NLA. No personal data is left in the 12 courses (read in full, including the truncated reading lists, from plans recomputed with the current R/anonymize.R), nor in a scan of all 249 NLA plans: no e-mails, phone numbers, staff names, contact blocks, approval stamps, se... |
| nmbu | opus | 8 | Anonymization works well for NMBU: in all 8 courses (and in the 4 other offerings with text, checked by rerunning anonymize_text() on extracted_text) the only personal data is the 'Emneansvarlig:Name' header line, which .anon_nmbu() removes, including lines with two names and four-part names. Not... |
| nord | opus | 17 | Anonymization works well for nord: none of the 17 courses has a name, e-mail or phone number left, and a scan of all 3489 nord plans finds no e-mail, phone number or role-plus-name line (nord pages have no staff or contact block). All name_label and name_like flags are false alarms. They match 'n... |
| ntnu | opus | 28 | No personal data or administrative dates are left in any of the 28 plans. Every 'Emneansvarlig/koordinator' and 'Faglærere' name (single, multiple and hyphenated) is removed with the Kontaktinformasjon block, and in all 3,791 NTNU offerings course_plan contains no 'Kontaktinformasjon', 'Emneansva... |
| oslomet | opus | 28 | No personal data is left. The only staff names in the raw text (5 'Emneansvarlig' blocks) are stripped by .anon_oslomet(), there are no e-mails or phone numbers, and every span removed in the packet is an admin year, a semester or cohort label, an approval date or a bare ';' line, so nothing in t... |
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
| ✓ | inn | boilerplate | other | high | common | inn_2MPEL171S-3-1_2022_spring_1; inn_2MKRLE5101-1-1_2022_... | Before anonymizing, store a flag on the plan or offering row, e.g. plan_fallback = grepl('Emnebeskrivelsen for valgt semester er ikke pub... |
| ✓ | mf | boilerplate | boilerplate_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), replace the library rule with stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\s*\\n... |
| ✓ | uib | content_text | text_corruption | medium | widespread | uib_RELMIN643-0_2025_autumn_1; uib_HIS250L-0_2022_autumn_... | In .anon_generic(), extend the pattern to consume a whole season list after the label, for example (?i)(?<=(?:semester\|…)[:\s]{0,3})SEASO... |
| ✓ | hiof | admin_date | admin_date_left | medium | common | hiof_LMBKRLE10221-1_2022_spring_2; hiof_LMBPED40217-1_202... | In .anon_hiof(), change the pattern to (?m)^[ \t]*Litteratur\w*(?: er)?(?: sist)? (?:oppdater\|endret\|gjort tilgjengelig)[^\n]* so it also... |
| ✓ | inn | content_text | text_corruption | medium | common | inn_2MPEL171S-3-1_2022_spring_1; inn_2MPEL171-3-1_2024_au... | Remove the replacement from .anon_inn(), since the published text should keep the page's 'Norsk/Engelsk'. If dedup still needs it, apply ... |
| ✓ | ntnu | content_text | over_removal | medium | common | ntnu_PPU4759-1_2017_spring_1; ntnu_PPU4773-1_2017_autumn_... | In .anon_ntnu(), remove each session block up to the next scheme instead of cutting to the end: str_remove_all("(?ms)^(?:Ordinær\|Utsatt\|N... |
| ✓ | hiof | content_text | over_removal | medium | occasional | hiof_LMUSAM11221-1_2022_spring_2; hiof_LMBNOR40117-1_2021... | Guard the pattern so it does not match next to a slash: (?<![/\w])\d{4}/\d{2,4}\b(?!/). Tested: it leaves 'files/2019/06/27/' and 'view/3... |
| ✓ | uib | content_text | text_corruption | low | widespread | uib_ENG125-0_2018_spring_1; uib_HIS250L-0_2020_spring_1; ... | Before the whitespace cleanup at the end of .anon_generic(), add str_replace_all("(?m)^[ \\t]*[.,;:][ \\t]*", ""), str_replace_all("[ \\t... |
| ✓ | hiof | content_text | over_removal | low | common | hiof_LMUENG10417-1_2019_spring_2; hiof_LMUSAM11221-1_2022... | Exempt ranges in square brackets and ranges right after a document number: add the lookbehinds (?<!\[)(?<!(?:Meld\. St\. \|St\.meld\. nr\.... |
| ✓ | oslomet | content_text | other | low | common | oslomet_M1GNA2100-1_2022_autumn_1; oslomet_M1GNA2200-1_20... | Use the same rule as in the admin_date finding: in .anon_oslomet(), str_replace_all with regex (?<=[\p{L}\d.,:)]);+(?=[\p{L}\d]) -> ' ', ... |
| ✓ | uib | content_text | text_corruption | low | common | uib_FRANDI111-0_2017_autumn_1; uib_NODI101-0_2017_spring_... | Match the whole word: (høst\|vår\|haust\|autumn\|spring\|sommer\|summer)(?:semester(?:et)?\|en\|a)?\b, followed by an optional [.,]. This removes... |
| ✓ | uit | admin_date | text_corruption | low | common | uit_LER-1402-1_2020_spring_1; uit_LER-2304-1_2024_spring_... | In .anon_generic(): (1) let the season+year rule consume a preceding `[ \t]*[-–,][ \t]*` when the label is the last thing on the line, an... |
| ✓ | usn | boilerplate | boilerplate_left | low | common | usn_MG2EN7-1_2022_autumn_1; usn_MG2EN6-1_2021_autumn_1; u... | In .anon_usn(), widen the lookahead to '(?=[\p{Lu}\d"«(‘’'“”]\|https?:)'. Add a second rule for lowercase-led titles, restricted to labels... |
| ✓ | hvl | boilerplate | boilerplate_left | low | occasional | hvl_MGBMU241-1_2025_spring_1 | In .anon_hvl(), widen the alternation to `(?m)^Emnet (?:tilbys ikke\|vert ikkje tilbydd\|blir ikkje tilbydd) (?:studieåret )?\d{4}/\d{4}\.?... |
| ✓ | nmbu | content_text | text_corruption | low | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPFD201-1_2025_autumn_2 | Make the academic-year patterns also take the preceding horizontal whitespace, e.g. "[ \\t]*\\b\\d{4}/\\d{2,4}\\b" (and the same for the ... |
| ✓ | nord | admin_date | admin_date_left | low | occasional | nord_PO118LS-1_2020_autumn_2; nord_HI109LS-1_2021_spring_... | In .anon_generic(), after the dd.mm.yyyy rule, also remove two-digit-year dates with a strict day and month: str_remove_all("\\b(?:0[1-9]... |
| ✓ | nord | content_text | over_removal | low | occasional | nord_NAT1002-1_2022_spring_3; nord_SAM1001-1_2021_autumn_... | Add a negative lookahead to the date rule in .anon_generic(): "\\d{1,2}\\.\\d{1,2}\\.\\d{4}(?!\\s*nr\\.?\\s*\\d)". Dates that cite a law ... |
| ✓ | ntnu | content_text | over_removal | low | occasional | ntnu_KJ3950-1_2015_spring_1; ntnu_BI3950-2_2017_spring_1 | Add '(?:utfyllende )?regler\|reglement' to .gov_doc_regex. That fixes 'utfyllende regler for realfagsstudiene av 01.08.2015', which is wit... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2022_autumn_1; u... | In .anon_uia(), extend the title pattern to an optional range: str_remove_all("\\((?:Høst\|Vår\|Haust)\\s+\\d{4}(?:\\s*[–-]\\s*(?:Høst\|Vår\|... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2020_autumn_1; u... | In .anon_generic(), let the lookbehind rule consume a coordinated second season: '(?i)(?<=(?:semester\|undervisning\|oppstart\|startsemester... |
| ✓ | uib | admin_date | admin_date_left | low | occasional | uib_HIS250L-0_2020_spring_1; uib_HIS250L-0_2019_spring_1;... | Add to .anon_generic(): str_remove_all("(?i)\\b\\d{1,2}\\.\\s*(?:januar\|februar\|mars\|april\|mai\|juni\|juli\|august\|september\|oktober\|novembe... |
| ✓ | uio | email | text_corruption | low | occasional | uio_BIOS5000-1_2025_autumn_1 | In .anon_generic(), let the bare-email rule also remove the horizontal space before the address: str_remove_all("[ \\t]*\\b[\\w.+-]+@[\\w... |
| ✓ | uis | content_text | over_removal | low | occasional | uis_MGLT43-1_2025_autumn_1; uis_MGLT43-1_2023_autumn_1; u... | (a) In .anon_generic(), put the guard before the match: paste0(.not_gov, "\\b(?i:", .season_regex, ")\\s*\\d{4}\\b"), and do the same for... |
| ✓ | uit | admin_date | admin_date_left | low | occasional | uit_SAM-3981-1_2017_autumn_1 | Decide the policy and record it in methods.qmd. If cohort and plan-validity years count as administrative, add `gjelder fra(?: og med)?\|(... |
| ✓ | hiof | boilerplate | boilerplate_left | low | rare | hiof_LMBKRO10117-1_2019_spring_2 | In .anon_hiof(), before the generic pass, remove whole revision-note lines with str_remove_all("(?m)^[ \\t]*\\d{1,2}\\.\\d{1,2}\\.\\d{4}:... |
| ✓ | mf | admin_date | admin_date_left | low | rare | mf_SAM2080L-1_2025_autumn_1 | In .anon_generic(), make two changes. (1) Before the lookbehind rule, add stringr::str_replace_all("(?i)\\b((?:høst\|vår\|haust)semester(?:... |
| ✓ | oslomet | admin_date | admin_date_left | low | rare | oslomet_M5GNA2100-1_2022_autumn_1 | (1) In .anon_oslomet(), before the generic rules run, turn glued semicolons into spaces: str_replace_all with regex (?<=[\p{L}\d.,:)]);+(... |
| ✓ | usn | content_text | text_corruption | low | rare | usn_MG2EN3-1_2024_autumn_1; usn_MG1NO2-1_2022_spring_1 | In .anon_generic(), let both rules also take the horizontal space before the match: prefix the patterns with '[ \t]*'. The space after th... |
| ✓ | usn | admin_date | admin_date_left | low | rare | usn_MG2EN4-1_2020_spring_2 | In .anon_generic(), change the admin-year rule to: str_replace_all(paste0("((?i:opprettet\|oppdatert\|revidert\|vedtatt\|godkjent(?: og akkre... |
