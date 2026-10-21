# anonymization audit — aggregated findings

34 findings across 17 institutions (from 18 agent reports in `data/audit/anonymization/findings`; model: opus).
Mechanical verification: 34 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 9 | hivolda, inn, mf, nla, nord, uia, uib, uit, usn | low |
| content_text | text_corruption | 5 | hiof, nih, nla, nmbu, ntnu | low |
| boilerplate | boilerplate_left | 5 | hvl, inn, mf, ntnu, uib | medium |
| person_name | pii_leak | 5 | hiof, oslomet, uib, uit, usn | medium |
| content_text | over_removal | 4 | nord, uis, uit, usn | low |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 3 | 2 |  | person_name / pii_leak; content_text / text_corruption | admin_date / admin_date_left; content_text / over_removal; boilerplate / boilerplate_left |
| nih | 0 | 1 |  | content_text / text_corruption |  |
| uia | 1 | 1 |  | admin_date / admin_date_left | admin_date / text_corruption |
| uib | 2 | 4 | admin_date / admin_date_left | person_name / pii_leak; boilerplate / text_corruption; boilerplate / boilerplate_left | content_text / text_corruption |
| uit | 2 | 3 | admin_date / admin_date_left | person_name / pii_leak; content_text / over_removal | admin_date / text_corruption |
| usn | 3 | 3 | admin_date / admin_date_left | person_name / pii_leak; content_text / over_removal | boilerplate / boilerplate_left; content_text / text_corruption |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| admin_date_left | low | 10 |
| text_corruption | low | 7 |
| pii_leak | medium | 6 |
| boilerplate_left | low | 4 |
| over_removal | low | 4 |
| boilerplate_left | medium | 1 |
| other | high | 1 |
| other | low | 1 |

## By target

| target | n |
| --- | --- |
| admin_date | 11 |
| content_text | 9 |
| boilerplate | 6 |
| person_name | 6 |
| all | 1 |
| email | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 28 | Anonymization works well at HiOF. All 28 packet courses lose their Emneansvarlig/Emneansvarlige names, whether the name is on the label line or the line below, along with the '(Høst yyyy)' title stamps, 'Gjeldende litteraturliste for <semester>' labels, 'Litteraturlist* sist oppdatert' stamps and... |
| hivolda | opus | 20 | Anonymization works well for HiVolda. None of the 20 plans keeps a staff name, e-mail or phone number, and a scan of all 667 offerings found none either. Apart from the functional address [e-post], the only text removed is the semester-year value after 'Undervisningssemester:'/'Ek... |
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
| uib | opus | 28 | Anonymization works well for UiB: e-mails (studierettleiar@...), the FSKLI901 'Emneansvarlig <name>' line and most reading-list date boilerplate are removed, and no course content was lost. All 8 name_like flags are false alarms (Adam Smith, David Hume and method names are subject matter). One re... |
| uio | opus | 10 | Anonymization works well for UiO. None of the 10 sampled plans has personal data or admin dates left. Across all 47 offerings, the current code removes the staff names UiO pages do hold ('Ta kontakt med <navn> eller <navn>' in FYS5930L, '<navn> (<e-post>)' in GEO5930L/MAT5930L); the leak an earli... |
| uis | opus | 28 | Anonymization works well at uis. None of the 28 sampled plans has personal data left, and full-text scans of all 2728 plans find no e-mail, role+name, '(Role)' staff line or name-only line outside reading lists. All three staff-list layouts (HTML 'Fagperson(er)', PDF 'Fagpersoner'/'Fagpersonar' b... |
| uit | opus | 28 | Anonymization works well for UiT's current layouts (2019-2025): the semester label and exam hand-in/hand-out dates go, nothing else is touched, and no personal data appears in any of the 13 random controls. The one real gap is the 2008-2011 English-language layout, where the closing timetable blo... |
| usn | opus | 28 | The USN handler does most of its job. Semester labels, the usual 'Godkjent emneplan dd.mm.yyyy Name, role' stamps, 'Name\nInstituttnestleder' blocks, cohort labels and Leganto widget text are removed in every sampled plan, and no reference authors or content years are lost. The phone, name_like a... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | all | other | high | occasional | inn_INN3024-1_2023_autumn_1; inn_2M352-1_2023_spring_1 | Add page_year to the inn entry of R/institution_config.R, read from the shown version: its earliest 'Undervisningssemestre'/'Teaching sem... |
| ✓ | mf | boilerplate | boilerplate_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), replace the library rule with stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\s*\\n... |
| ✓ | usn | person_name | pii_leak | medium | common | usn_REG3000D-1_2019_autumn_1; usn_NETPRL-1_2022_spring_1;... | Widen .approved_by_name_regex to '((?i:godkjent\|approved)(?:[ \t]+\p{Ll}+)?[ \t]+(?:av\|ved\|by)(?:[ \t]+(?:dekan(?:en)?\|prodekan\|institutt... |
| ✓ | hiof | person_name | pii_leak | medium | rare | hiof_SFB24011-1_2019_spring_1 | In .anon_hiof(), before the generic pass, remove staff label lines and the name lines that follow them, e.g. str_remove_all("(?m)^[ \\t]*... |
| ✓ | oslomet | person_name | pii_leak | medium | rare | oslomet_ACIT4080-1_2025_autumn_1 | (1) In .anon_oslomet(), replace the one-line rule with a rule for the whole block: str_remove('(?ms)^Emneansvarlige?[ \t]*\n.*?(?=^Emneov... |
| ✓ | uib | person_name | pii_leak | medium | rare | uib_MEDEPI-0_2025_autumn_1 | Add 'administrativt ansvarlig' (and 'administrativt ansvarleg') to the heading alternatives of .role_heading_name_regex. Alternatively, i... |
| ✓ | uit | person_name | pii_leak | medium | rare | uit_SVF-3904-1_2008_autumn_1 | In .anon_uit(), widen the block rule to '(?s)(?:^\|\n)(?:Undervisning\|Lectures\|Teaching) (?:Høst\|Vår\|Haust\|Autumn\|Spring\|Fall) \d{4}.*$' a... |
| ✓ | usn | person_name | pii_leak | medium | rare | usn_SMERTE100-1_2022_autumn_1 | Add 'fagansvarlig', 'fagansvarleg' and 'programkoordinator' (or a '\p{L}*koordinator' compound) to .staff_roles. Keep the inline rule's b... |
| ✓ | nih | content_text | text_corruption | low | widespread | nih_THP311-1_2025_autumn_1; nih_LKI205-1_2025_autumn_1; n... | In .anon_generic(), before the generic season-year removal, drop a leading 'for ' when the season-year ends the sentence: str_remove_all(... |
| ✓ | hvl | admin_date | other | low | common | hvl_MAS538-1_2025_autumn_1; hvl_PHD911-1_2025_autumn_1; h... | Make the rule consistent with its own intent. Add the English labels to the lookbehind: `(?<=(?i:semester of (?:instruction\|assessment)\|s... |
| ✓ | uit | content_text | over_removal | low | common | uit_BED-6001-1_2008_autumn_1; uit_BED-2017-1_2008_autumn_... | End the block rule at a lookahead: '(?s)(?:^\|\n)Undervisning (?:Høst\|Vår\|Haust) \d{4}.*?(?=\n?Ansvarlig enhet\|\n?Responsible unit\|\z)'. I... |
| ✓ | hivolda | admin_date | admin_date_left | low | occasional | hivolda_MEI304-1_2022_autumn_1; hivolda_DRHS900-1_2016_au... | In .anon_generic() (or .anon_hivolda()), drop the bracketed year after a week reference and keep the weeks: stringr::str_replace_all("(?i... |
| ✓ | hvl | boilerplate | boilerplate_left | low | occasional | hvl_PHD911-1_2025_spring_1; hvl_BØA3100-1_2025_spring_1 | In .anon_hvl(), also remove (a) `(?m)^The (?:module\|course) is not offered in the academic year \d{4}/\d{4}\.?[ \t]*(?:\n\|$)` and (b) `[ ... |
| ✓ | inn | boilerplate | boilerplate_left | low | occasional | inn_6EV323-1_2025_spring_3; inn_2M352-1_2023_spring_1; in... | In .anon_inn(), before the semester-line rule: stringr::str_remove("^\\s*Statusmelding\\n(?:Kun tilgjengelig på[^\\n]*)\\n"). Add a test ... |
| ✓ | nla | content_text | text_corruption | low | occasional | nla_TAM301-1_2022_autumn_1; nla_IKS104-2_2025_autumn_2 | In .anon_generic(), add a guard like .not_gov: keep an academic-year range when 'gjennomført\|avlagt\|bestått\|tatt' plus up to 20 character... |
| ✓ | nmbu | content_text | text_corruption | low | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPFD201-1_2025_autumn_2 | Make the academic-year patterns also take the preceding horizontal whitespace, e.g. "[ \\t]*\\b\\d{4}/\\d{2,4}\\b" (and the same for the ... |
| ✓ | ntnu | boilerplate | boilerplate_left | low | occasional | ntnu_FY8918-1_2019_autumn_1; ntnu_ØK8000-1_2025_autumn_1;... | In .anon_ntnu(), extend the rule to the link label: stringr::str_remove_all("Andre sider om emnet\\s*(?:Faglærers emneside\\s*)?"). Add t... |
| ✓ | uib | boilerplate | text_corruption | low | occasional | uib_HTEK101-0_2022_autumn_1; uib_ENG120-0_2025_autumn_1; ... | In the .anon_uib() Litteraturlist regex use '\d\.(?=\d\|\h+\p{L})' (any letter, \h for horizontal whitespace including NBSP), and add test... |
| ✓ | uib | boilerplate | boilerplate_left | low | occasional | uib_PRPSYK301C-0_2022_autumn_2; uib_AHKR391-0_2019_autumn_1 | Widen the .anon_uib() pattern to 'Litteraturlist\w*(?: for [\p{L} ]{1,30}?)? vil (?:normalt )?(?:ver[ae] (?:klare?\|tilgjengele?g)(?: på \... |
| ✓ | uio | email | text_corruption | low | occasional | uio_BIOS5000-1_2025_autumn_1 | In .anon_generic(), let the bare-email rule also remove the horizontal space before the address: str_remove_all("[ \\t]*\\b[\\w.+-]+@[\\w... |
| ✓ | uis | content_text | over_removal | low | occasional | uis_MGLT43-1_2025_autumn_1; uis_MGLT43-1_2023_autumn_1; u... | (a) In .anon_generic(), put the guard before the match: paste0(.not_gov, "\\b(?i:", .season_regex, ")\\s*\\d{4}\\b"), and do the same for... |
| ✓ | uit | admin_date | admin_date_left | low | occasional | uit_SVF-6311-1_2008_autumn_2; uit_HEL-6308-1_2019_autumn_... | Extend the semester-label rule to '(?i:spring\|autumn\|fall)[ \t]+semester[ \t]+\d{4}', and in .anon_uit() remove the year from the 'Søknad... |
| ✓ | usn | admin_date | admin_date_left | low | occasional | usn_NORVU103-1_2018_autumn_1; usn_SMERTE100-1_2022_autumn... | In .anon_generic(), make the season+year rule accept '[ \t-]?' between the season and a 2- or 4-digit year. Let the dd.mm.yyyy rule match... |
| ✓ | hiof | content_text | text_corruption | low | rare | hiof_LU15SAMG07-1_2007_autumn_2 | Optional: in the academic-year removal, also take a directly preceding 'i '/'for ' when the year is mid-sentence, e.g. (?:\b(?:i\|for)[ \t... |
| ✓ | inn | admin_date | admin_date_left | low | rare | inn_IDTD2002-1_2025_autumn_1 | In .anon_inn(), remove a date that fills a tab-separated table cell: stringr::str_remove_all("(?m)(?<=\\t)\\d{1,2}\\.\\d{1,2}\\.(?:\\d{4}... |
| ✓ | mf | admin_date | admin_date_left | low | rare | mf_SAM2080L-1_2025_autumn_1 | In .anon_generic(), make two changes. (1) Before the lookbehind rule, add stringr::str_replace_all("(?i)\\b((?:høst\|vår\|haust)semester(?:... |
| ✓ | nla | admin_date | admin_date_left | low | rare | nla_3GJ322-1_2019_autumn_1; nla_GL5SA3-1_2017_autumn_1 | Add 'start in the' to the lookbehind alternatives of the season-after-label rule (a fixed-length alternative, so it is valid in ICU). Add... |
| ✓ | nord | content_text | over_removal | low | rare | nord_BE207E-1_2019_spring_3 | In that rule, (1) match the label as a word of its own and list the compounds that should still match. One way is to turn it into str_rep... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | Either accept it as a one-off typo in the source, or add a typo-tolerant rule to .anon_generic() that catches a year label closing a line... |
| ✓ | nord | admin_date | admin_date_left | low | rare | nord_PRO9011-1_2025_autumn_1 | In .anon_generic(), remove a day.month date only when a weekday comes right before it, which never happens with credits or section number... |
| ✓ | ntnu | content_text | text_corruption | low | rare | ntnu_PG8301-1_2007_autumn_1 | Remove the clause with its label before the season rule, in .anon_generic() (the wording is not NTNU-specific): stringr::str_remove_all(p... |
| ✓ | uia | admin_date | admin_date_left | low | rare | uia_HEL617-1_2025_autumn_1 | Low priority. Either add to .anon_uia() a removal for '(?i)\b(?:is\|will be) offered\s+\d{1,2}(?:\s*[-–]\s*\d{1,2})?\.?\s+<month>,?\s+\d{4... |
| ✓ | uib | admin_date | admin_date_left | low | rare | uib_FSKLI901-0_2025_spring_1; uib_FSKLI901-0_2025_autumn_1 | In .anon_uib(), remove lines matching '(?m)^(?:mandag\|tirsdag\|onsdag\|torsdag\|fredag)\h+\d{1,2}\.\h*\p{L}+\.?\h*\d{4}[^\n]*\n?'. Alternati... |
| ✓ | usn | content_text | over_removal | low | rare | usn_OKLI3014-1_2018_autumn_1 | In the .anon_usn() Leganto rule, add 'føring' to the negative lookahead '(?!s(?!\p{L})\|ation\|ist\|wise\|ing\|føring)'. Add a test that 'Jour... |
