# anonymization audit — aggregated findings

32 findings across 17 institutions (from 18 agent reports in `data/audit/anonymization/findings`; model: gpt-6.1-sol, opus).
Mechanical verification: 32 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 9 | hivolda, inn, mf, nla, nord, uia, uib, uit, usn | medium |
| content_text | text_corruption | 6 | hiof, nih, nla, nmbu, ntnu, uib | low |
| boilerplate | boilerplate_left | 5 | hvl, inn, mf, ntnu, uib | medium |
| content_text | over_removal | 3 | nord, uis, usn | low |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 2 | 1 | content_text / text_corruption |  | person_name / pii_leak |
| uib | 4 | 4 | admin_date / admin_date_left; boilerplate / text_corruption; boilerplate / boilerplate_left | content_text / text_corruption | person_name / pii_leak |
| uit | 3 | 1 | admin_date / admin_date_left |  | person_name / pii_leak; content_text / over_removal |
| usn | 3 | 2 | admin_date / admin_date_left; content_text / over_removal |  | person_name / pii_leak |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| admin_date_left | low | 11 |
| text_corruption | low | 9 |
| boilerplate_left | low | 4 |
| over_removal | low | 3 |
| admin_date_left | medium | 1 |
| boilerplate_left | medium | 1 |
| other | high | 1 |
| other | low | 1 |
| pii_leak | medium | 1 |

## By target

| target | n |
| --- | --- |
| admin_date | 13 |
| content_text | 9 |
| boilerplate | 7 |
| all | 1 |
| email | 1 |
| person_name | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | gpt-6.1-sol | 28 | All 28 courses were reviewed: no personal-data leaks, residual administrative dates, substantive content loss or structure loss were confirmed in the packet; staff names, including the free-text lecturer line, are correctly removed while reference authors and subject matter remain. The seven susp... |
| hivolda | opus | 20 | Anonymization works well for HiVolda. None of the 20 plans keeps a staff name, e-mail or phone number, and a scan of all 667 offerings found none either. Apart from the functional address studentsorvis@hivolda.no, the only text removed is the semester-year value after 'Undervisningssemester:'/'Ek... |
| hvl | opus | 28 | Anonymization works well for HVL. None of the 28 plans keeps a name, e-mail, phone number or administrative date, and no course content was removed. All 11 name_like flags are false alarms: programme and course names, tool names, Ole Bull Akademiet, and in PHD911 scholars named as subject matter ... |
| inn | opus | 20 | No personal data in any of the 20 courses, and none in a scan of all 729 INN plans (0 e-mails, 0 phone numbers, staff labels only as generic roles: INN pages have no contact block). Semester-year lists are removed, and content years ('1896-1945', 'Cohen, 1996') are kept. The 'removed' flag (cours... |
| mf | opus | 8 | Personal data is removed reliably for MF. .anon_mf() cuts the trailing 'Emneansvarlig' contact block in all 24/26 offerings that have one (the other 2 have no contact block), including 'Etternavn, Fornavn' names, titles, e-mails and pages with several coordinators. The 'Eksamensdatoer' block is n... |
| nih | opus | 21 | Anonymization works well for NIH: none of the 21 packet courses (all random controls; the pre-pass flagged nothing) contains staff names, e-mails, phone numbers or administrative dates, and no course content was removed (unyeared 'vårsemesteret', '80 %', '6000-8000 ord', 'APA-manual (7. utg.)' al... |
| nla | opus | 28 | Anonymization works well for NLA. No personal data is left in the 28 courses (the truncated tails of 4 were read from plans recomputed with the current R/anonymize.R), and a scan of all 311 NLA plans finds no e-mails, phones, staff names, contact blocks or year-stamped semester labels. All 10 sus... |
| nmbu | opus | 8 | Anonymization works well for NMBU: in all 8 courses (and in the 4 other offerings with text, checked by rerunning anonymize_text() on extracted_text) the only personal data is the 'Emneansvarlig:Name' header line, which .anon_nmbu() removes, including lines with two names and four-part names. Not... |
| nord | opus | 28 | Anonymization works well for nord. None of the 28 courses, and none of the 176 pilot offerings in a separate scan, has a name, e-mail or phone number left: nord pages have no contact block, and 'emneansvarlig' and 'faglærer' occur only as roles. All 8 name_like flags are false alarms on reference... |
| ntnu | opus | 28 | No personal data or administrative dates are left in any of the 28 plans: the Kontaktinformasjon cut removes every coordinator and teacher name, including names with particles ('[navn]'), and the 'Undervisningsstart' lines and session semester labels ('- Høst 2019') go, while both 'Vurderingsordn... |
| oslomet | opus | 28 | In the 28 packet courses no personal data is left and nothing is over-removed. The one staff name (an 'Emneansvarlig' block, course 6) is stripped. Every removed span is an academic year, a semester or cohort label, or an approval date. All pre-pass flags are false alarms: name_like hits English ... |
| steiner | opus | 8 | Anonymization works correctly for Steiner: the plans (split from subject PDFs) contain no staff names, e-mails, phone numbers or administrative dates, and the only text .anon_steiner() removes is sequential bare PDF page numbers (e.g. 9/10/11, 12-15), with no course content lost and line structur... |
| uia | opus | 28 | Anonymization works well for UiA. In all 28 courses the course-coordinator names are removed, both the 'Emneansvarlig: Name' form on one line and the label with the name on the next line, and multi-name 'Emneansvarlige:' blocks. The '(Høst YYYY)' and '(Høst 2022–Vår 2023)' title stamps, the FS ti... |
| uib | gpt-6.1-sol | 28 | All 28 courses were read in full: no remaining personal data or substantive course-content loss was found, and the previously reported MEDEPI staff name is now removed. All eight reviewed name_like flags are false alarms for historical figures, programme/organisation names or technical terminolog... |
| uio | opus | 10 | Anonymization works well for UiO. None of the 10 sampled plans has personal data or admin dates left. Across all 47 offerings, the current code removes the staff names UiO pages do hold ('Ta kontakt med <navn> eller <navn>' in FYS5930L, '<navn> (<e-post>)' in GEO5930L/MAT5930L); the leak an earli... |
| uis | opus | 28 | Anonymization works well at uis. None of the 28 sampled plans has personal data left, and full-text scans of all 2728 plans find no e-mail, role+name, '(Role)' staff line or name-only line outside reading lists. All three staff-list layouts (HTML 'Fagperson(er)', PDF 'Fagpersoner'/'Fagpersonar' b... |
| uit | gpt-6.1-sol | 28 | Reviewed all 28 packet entries (17 suspects and 11 random controls); no surviving personal data, substantive content loss or anonymization-induced structure damage was found in the supplied text, although two plans have explicit packet truncation markers. All ten name_like flags are false alarms ... |
| usn | gpt-6.1-sol | 28 | No personal-data leak is visible in the 28 fully read packet entries (20 suspects and eight random controls), and standard staff-name, approval, semester and cohort removals generally work. Visible phone-like numbers are publication dates, and name_like flags are explained by reference authors, o... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | all | other | high | occasional | inn_INN3024-1_2023_autumn_1; inn_2M352-1_2023_spring_1 | Add page_year to the inn entry of R/institution_config.R, read from the shown version: its earliest 'Undervisningssemestre'/'Teaching sem... |
| ✓ | mf | boilerplate | boilerplate_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), replace the library rule with stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\s*\\n... |
| ✓ | uib | admin_date | admin_date_left | medium | occasional | uib_FSKLI901-0_2025_spring_1; uib_FSKLI901-0_2025_autumn_1 | In .anon_uib(), remove weekday-plus-date rows within the explicitly introduced meeting schedule in Undervisningssemester. Accept full and... |
| ✓ | oslomet | person_name | pii_leak | medium | rare | oslomet_ACIT4080-1_2025_autumn_1 | (1) In .anon_oslomet(), replace the one-line rule with a rule for the whole block: str_remove('(?ms)^Emneansvarlige?[ \t]*\n.*?(?=^Emneov... |
| ✓ | nih | content_text | text_corruption | low | widespread | nih_THP311-1_2025_autumn_1; nih_LKI205-1_2025_autumn_1; n... | In .anon_generic(), before the generic season-year removal, drop a leading 'for ' when the season-year ends the sentence: str_remove_all(... |
| ✓ | hiof | content_text | text_corruption | low | common | hiof_HSSHV40317-1_2025_autumn_1; hiof_IRF02022-1_2022_aut... | Before generic date removal, add a narrowly scoped rewrite in .anon_hiof() for the complete 'Gjeldende litteraturliste for <semester-year... |
| ✓ | hvl | admin_date | other | low | common | hvl_MAS538-1_2025_autumn_1; hvl_PHD911-1_2025_autumn_1; h... | Make the rule consistent with its own intent. Add the English labels to the lookbehind: `(?<=(?i:semester of (?:instruction\|assessment)\|s... |
| ✓ | hivolda | admin_date | admin_date_left | low | occasional | hivolda_MEI304-1_2022_autumn_1; hivolda_DRHS900-1_2016_au... | In .anon_generic() (or .anon_hivolda()), drop the bracketed year after a week reference and keep the weeks: stringr::str_replace_all("(?i... |
| ✓ | hvl | boilerplate | boilerplate_left | low | occasional | hvl_PHD911-1_2025_spring_1; hvl_BØA3100-1_2025_spring_1 | In .anon_hvl(), also remove (a) `(?m)^The (?:module\|course) is not offered in the academic year \d{4}/\d{4}\.?[ \t]*(?:\n\|$)` and (b) `[ ... |
| ✓ | inn | boilerplate | boilerplate_left | low | occasional | inn_6EV323-1_2025_spring_3; inn_2M352-1_2023_spring_1; in... | In .anon_inn(), before the semester-line rule: stringr::str_remove("^\\s*Statusmelding\\n(?:Kun tilgjengelig på[^\\n]*)\\n"). Add a test ... |
| ✓ | nla | content_text | text_corruption | low | occasional | nla_TAM301-1_2022_autumn_1; nla_IKS104-2_2025_autumn_2 | In .anon_generic(), add a guard like .not_gov: keep an academic-year range when 'gjennomført\|avlagt\|bestått\|tatt' plus up to 20 character... |
| ✓ | nmbu | content_text | text_corruption | low | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPFD201-1_2025_autumn_2 | Make the academic-year patterns also take the preceding horizontal whitespace, e.g. "[ \\t]*\\b\\d{4}/\\d{2,4}\\b" (and the same for the ... |
| ✓ | ntnu | boilerplate | boilerplate_left | low | occasional | ntnu_FY8918-1_2019_autumn_1; ntnu_ØK8000-1_2025_autumn_1;... | In .anon_ntnu(), extend the rule to the link label: stringr::str_remove_all("Andre sider om emnet\\s*(?:Faglærers emneside\\s*)?"). Add t... |
| ✓ | uib | boilerplate | text_corruption | low | occasional | uib_HTEK101-0_2022_autumn_1; uib_ENG120-0_2025_autumn_1; ... | Replace the ambiguous full-stop scan with explicit numeric and named-month date alternatives, allowing capitalised month names, horizonta... |
| ✓ | uib | boilerplate | boilerplate_left | low | occasional | uib_AHKR391-0_2019_autumn_1; uib_PRPSYK301C-0_2022_autumn_2 | Extend the anchored reading-list publication-sentence grammar to allow optional normalt and the alternative vere tilgjengeleg på uib.no f... |
| ✓ | uio | email | text_corruption | low | occasional | uio_BIOS5000-1_2025_autumn_1 | In .anon_generic(), let the bare-email rule also remove the horizontal space before the address: str_remove_all("[ \\t]*\\b[\\w.+-]+@[\\w... |
| ✓ | uis | content_text | over_removal | low | occasional | uis_MGLT43-1_2025_autumn_1; uis_MGLT43-1_2023_autumn_1; u... | (a) In .anon_generic(), put the guard before the match: paste0(.not_gov, "\\b(?i:", .season_regex, ")\\s*\\d{4}\\b"), and do the same for... |
| ✓ | uit | admin_date | admin_date_left | low | occasional | uit_BED-6001-1_2008_autumn_1; uit_HEL-6308-1_2019_autumn_... | Extend the generic season-year pattern to accept English spring/autumn/fall semester YYYY. Add contextual UiT rules that remove full day/... |
| ✓ | usn | admin_date | admin_date_left | low | occasional | usn_FM1DANS-1_2022_autumn_1; usn_SMERTE100-1_2022_autumn_1 | Add approval-context date matching for dd.mm. followed by horizontal whitespace and a four-digit year, and for dd.mm.yyyy immediately fol... |
| ✓ | inn | admin_date | admin_date_left | low | rare | inn_IDTD2002-1_2025_autumn_1 | In .anon_inn(), remove a date that fills a tab-separated table cell: stringr::str_remove_all("(?m)(?<=\\t)\\d{1,2}\\.\\d{1,2}\\.(?:\\d{4}... |
| ✓ | mf | admin_date | admin_date_left | low | rare | mf_SAM2080L-1_2025_autumn_1 | In .anon_generic(), make two changes. (1) Before the lookbehind rule, add stringr::str_replace_all("(?i)\\b((?:høst\|vår\|haust)semester(?:... |
| ✓ | nla | admin_date | admin_date_left | low | rare | nla_3GJ322-1_2019_autumn_1; nla_GL5SA3-1_2017_autumn_1 | Add 'start in the' to the lookbehind alternatives of the season-after-label rule (a fixed-length alternative, so it is valid in ICU). Add... |
| ✓ | nord | content_text | over_removal | low | rare | nord_BE207E-1_2019_spring_3 | In that rule, (1) match the label as a word of its own and list the compounds that should still match. One way is to turn it into str_rep... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | Either accept it as a one-off typo in the source, or add a typo-tolerant rule to .anon_generic() that catches a year label closing a line... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | In .anon_generic(), remove a day.month date only when a weekday comes right before it, which never happens with credits or section number... |
| ✓ | ntnu | content_text | text_corruption | low | rare | ntnu_PG8301-1_2007_autumn_1 | Remove the clause with its label before the season rule, in .anon_generic() (the wording is not NTNU-specific): stringr::str_remove_all(p... |
| ✓ | uia | admin_date | admin_date_left | low | rare | uia_HEL617-1_2025_autumn_1 | Low priority. Either add to .anon_uia() a removal for '(?i)\b(?:is\|will be) offered\s+\d{1,2}(?:\s*[-–]\s*\d{1,2})?\.?\s+<month>,?\s+\d{4... |
| ✓ | uib | content_text | text_corruption | low | rare | uib_DIGI615-0_2025_autumn_2 | Before generic email removal, add a narrowly scoped UiB rewrite for utsatt/utsett innleveringsfrist til followed by an email: retain the ... |
| ✓ | uib | boilerplate | text_corruption | low | rare | uib_ENGMAU655-0_2025_spring_1 | In .anon_uib(), remove the complete administrative sentence matching Emnet byrjar å gå frå plus a semester and year before generic season... |
| ✓ | usn | admin_date | admin_date_left | low | rare | usn_NORVU103-1_2018_autumn_1 | Allow an optional hyphen or dash, with horizontal whitespace, between a complete season word and its two- or four-digit semester year. Re... |
| ✓ | usn | admin_date | admin_date_left | low | rare | usn_SMERTE100-1_2022_autumn_1 | In USN Endringsbeskrivelse blocks, remove year values in explicitly identified list-replacement notes matching a year followed by erstatt... |
| ✓ | usn | content_text | over_removal | low | rare | usn_OKLI3014-1_2018_autumn_1 | Protect Journalføring and its inflections/compounds from the Journal item-type alternative, for example with Journal(?!føring), before ap... |
