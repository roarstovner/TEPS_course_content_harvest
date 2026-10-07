# anonymization audit — aggregated findings

39 findings across 15 institutions (from 18 agent reports in `data/audit/anonymization/findings`; model: opus).
Mechanical verification: 39 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| admin_date | admin_date_left | 9 | hiof, mf, nla, nord, oslomet, uib, uis, uit, usn | medium |
| content_text | over_removal | 7 | hiof, nla, nord, ntnu, uis, uit, usn | medium |
| boilerplate | boilerplate_left | 4 | hiof, mf, oslomet, usn | medium |
| content_text | text_corruption | 4 | inn, nmbu, uib, uit | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| mf | 3 | 2 | boilerplate / boilerplate_left; admin_date / admin_date_left |  | admin_date / text_corruption |
| nih | 1 | 0 |  |  | admin_date / admin_date_left |
| uio | 2 | 1 |  | email / text_corruption | structure / text_corruption; boilerplate / boilerplate_left |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| over_removal | low | 8 |
| admin_date_left | low | 7 |
| text_corruption | low | 7 |
| admin_date_left | medium | 5 |
| over_removal | medium | 3 |
| text_corruption | medium | 3 |
| boilerplate_left | low | 2 |
| boilerplate_left | medium | 2 |
| other | high | 1 |
| pii_leak | high | 1 |

## By target

| target | n |
| --- | --- |
| content_text | 17 |
| admin_date | 14 |
| boilerplate | 6 |
| email | 1 |
| person_name | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 28 | Personal data is removed reliably at HiOF. Every Emneansvarlig name in the packet is gone, whether one name or two, on the label line or the line below. The packet has no e-mail addresses or phone numbers, and a scan of all 820 published hiof plans finds none either. The phone and name_like flags... |
| hivolda | opus | 20 | Anonymization works well for HiVolda: none of the 20 plans keeps any personal data, the only text removed is the semester-year value after 'Undervisningssemester:'/'Eksamenssemester:', and content dates ('22. juli 2011', lovdata id '2014-12-04-1697', word counts, reference years) are kept; a scan... |
| hvl | opus | 28 | Out-of-packet PII leak (cannot be a verified finding): hvl_MGBNA141-1_2022_spring_1 in data/processed/course_plans.RDS keeps two staff names as 'Bergen: <name>' / 'Stord: <name>' lines under 'Hjelpemidler ved eksamen'; no rule matches campus-label + name lines, and privacy_check patterns miss the... |
| inn | opus | 26 | No personal data or administrative dates are left in any of the 26 courses: INN plans have no staff or contact blocks. Semester-year labels and overlap-table dates are removed, and content years ('Læreplan for KRLE 2015', '2500-3000 ord') are kept. The pre-pass flags are all false alarms. All 56 ... |
| mf | opus | 8 | Personal data is removed reliably for MF. .anon_mf() cuts the trailing 'Emneansvarlig' contact block in all 24/26 offerings that have one (the other 2 have no contact block), including 'Etternavn, Fornavn' names, titles, e-mails and pages with several coordinators. The 'Eksamensdatoer' block is n... |
| nih | opus | 8 | Anonymization works well for NIH: none of the 8 packet courses (all random controls, since the pre-pass flagged nothing among the 81 offerings) contains names, e-mails, phone numbers or admin dates, and the only removals are semester-year stamps on the reading-list line ('Pensumliste for våren 20... |
| nla | opus | 12 | No personal data is left in any of the 12 courses, nor in a scan of all 249 NLA plans (no e-mails, phone numbers, staff names, approval stamps or semester-year labels). Every pre-pass flag is a false alarm caused by reference content in the truncated reading lists: a date inside a vatican.va URL ... |
| nmbu | opus | 8 | Anonymization works well for NMBU: in all 8 courses (and in the 4 other offerings with text, checked by rerunning anonymize_text() on extracted_text) the only personal data is the 'Emneansvarlig:Name' header line, which .anon_nmbu() removes, including lines with two names and four-part names. Not... |
| nord | opus | 17 | Anonymization works well for nord: none of the 17 courses has a name, e-mail or phone number left, and a scan of all 3489 nord plans finds no e-mail, phone number or role-plus-name line (nord pages have no staff or contact block). All name_label and name_like flags are false alarms. They match 'n... |
| ntnu | opus | 28 | No personal data or admin dates are left in any of the 28 sampled plans. The cut at 'Kontaktinformasjon' removes every 'Emneansvarlig/koordinator' and 'Faglærere' name. Across all 3,791 NTNU offerings, 3,790 texts have that heading, no staff label survives in course_plan (the 47 'faglærere' hits ... |
| oslomet | opus | 28 | No personal data leaks: there are no e-mails or phones, and the few 'Emneansvarlig' + name blocks in the raw text are all stripped by .anon_oslomet(). Nothing is over-removed; the removed spans are only Studieår/semester labels, '(2019 HØST )' cohort tags and bare semicolons. The main gap is admi... |
| steiner | opus | 8 | Anonymization works correctly for Steiner: the plans (split from subject PDFs) contain no staff names, e-mails, phone numbers or administrative dates, and the only text .anon_steiner() removes is sequential bare PDF page numbers (e.g. 9/10/11, 12-15), with no course content lost and line structur... |
| uia | opus | 28 | Anonymization works well for UiA: in all 28 courses the course-coordinator names (single and multi-name 'Emneansvarlige:' blocks, hyphenated and three-part names), breadcrumb, '(Høst YYYY)' title stamps and the FS timestamp (Bokmål and Nynorsk) are removed, no e-mail, phone or admin date is left,... |
| uib | opus | 27 | There is no personal data in any UiB plan. The packet has none, and a search of all 1473 anonymized UiB offerings found 0 e-mails, 0 phone numbers and no role label followed by a name, because UiB's Emneansvarlig fields name committees and departments, not people. The pre-pass name_label and name... |
| uio | opus | 10 | OUTSIDE THE SAMPLE: uio_FYS5930L-2_2025_autumn_1 leaks two staff names into published course_plans.RDS and plan_sections.RDS ('Ta kontakt med [navn] eller [navn]'), because .anon_uio() only strips names followed by '(e-mail)'. The 10 sampled courses are clean: the exam-lin... |
| uis | opus | 28 | No personal data leaks in the 28 sampled courses: staff lists (HTML 'Fagperson(er)', old PDF 'Fagpersoner/Fagpersonar' with '-Name (Role)'), OpenURL resolver links and their user_ip are removed completely, and reference authors are kept. The main defect is the 2020-2025 PDF version stamp ('CODE_1... |
| uit | opus | 28 | In the modern UiT layout (2012 onwards) anonymization works: the 'Emneleder <name> <e-mail>' line and exam dates are removed, and none of the 8 random controls has personal data left in. The real problems are staff names and logistics dates left in the 'Undervisning' timetable block of the 2008-2... |
| usn | opus | 28 | No e-mails, phone numbers or staff names are left in the 28 sampled plans. The name_label flags are journal titles ('The Reading teacher The Reading teacher'), the phone flags (all offerings) are spaced ISBNs and YYYYMMDD dates, and 4-digit approval/signature lines ('Godkjent emneplan 23.01.2018 ... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | boilerplate | other | high | common | inn_2MPEL171S-3-1_2022_spring_1; inn_2MKRLE5101-1-1_2022_... | Before anonymizing, store a flag on the plan or offering row, e.g. plan_fallback = grepl('Emnebeskrivelsen for valgt semester er ikke pub... |
| ✓ | uit | person_name | pii_leak | high | rare | uit_EXF-0723-1_2008_autumn_1 | In .anon_uit(), remove the old-layout timetable block from its heading to the end of the text: str_remove("(?s)\\n?Undervisning[ \\t]{2,}... |
| ✓ | mf | boilerplate | boilerplate_left | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In .anon_mf(), replace the library rule with stringr::str_remove("(?s)(?:Litteraturlisten for[^\\n]*\\s*)*Tilgang til litteratur:?\\s*\\n... |
| ✓ | nla | admin_date | admin_date_left | medium | widespread | nla_MGL1KRLE201A-1_2020_spring_2; nla_MGL5PE101-1_2024_au... | Add the em dash to the dash class in both places: `\b\d{4}\s?[-–—]\s?\d{2,4}\b` in .anon_generic() and `(\d{4})\s?[-–—]\s?(\d{2,4})` in .... |
| ✓ | ntnu | content_text | over_removal | medium | widespread | ntnu_SPRÅK1001-2_2014_spring_1; ntnu_PPU4721-1_2014_sprin... | In .anon_ntnu(), replace the cut-to-end with targeted removals. (1) Remove only the name block: str_remove("Kontaktinformasjon[\\s\\S]*?(... |
| ✓ | oslomet | admin_date | admin_date_left | medium | widespread | oslomet_M5GNA2100-1_2019_spring_2; oslomet_M1GNA2200-1_20... | In .anon_oslomet(), or in .anon_generic() for reuse elsewhere, remove long-form dates that follow an approval keyword in the same sentenc... |
| ✓ | uib | content_text | text_corruption | medium | widespread | uib_RELMIN643-0_2025_autumn_1; uib_HIS250L-0_2022_autumn_... | In .anon_generic(), extend the pattern to consume a whole season list after the label, for example (?i)(?<=(?:semester\|…)[:\s]{0,3})SEASO... |
| ✓ | uis | admin_date | admin_date_left | medium | widespread | uis_MGL3066-1_2025_autumn_1; uis_MGLT43-1_2024_autumn_1; ... | In .anon_uis(), make the EMNE prefix optional, or add str_remove_all("(?m)^[ \\t]*\\S+_\\d+[ \\t]+(?:Bokmål\|Nynorsk\|Engelsk\|English)[ \\t... |
| ✓ | usn | admin_date | admin_date_left | medium | widespread | usn_MG1EN3-1_2024_autumn_1; usn_MG1NO5-1_2020_autumn_2; u... | In .anon_usn(), replace the date-stamp rule with a whole-line rule for the approval stamp: a short line that starts with an optional 'Emn... |
| ✓ | hiof | admin_date | admin_date_left | medium | common | hiof_LMBKRLE10221-1_2022_spring_2; hiof_LMBPED40217-1_202... | In .anon_hiof(), change the pattern to (?m)^[ \t]*Litteratur\w*(?: er)?(?: sist)? (?:oppdater\|endret\|gjort tilgjengelig)[^\n]* so it also... |
| ✓ | inn | content_text | text_corruption | medium | common | inn_2MPEL171S-3-1_2022_spring_1; inn_2MPEL171-3-1_2024_au... | Remove the replacement from .anon_inn(), since the published text should keep the page's 'Norsk/Engelsk'. If dedup still needs it, apply ... |
| ✓ | oslomet | boilerplate | boilerplate_left | medium | common | oslomet_M1GNA2100-1_2022_autumn_1; oslomet_M5GNO1300-1_20... | In .anon_oslomet(), after the two existing rules: str_replace_all('(?<=[\\p{L}\\d.,:)]) ?;(?=[\\p{L}\\d«(])', ' '), then str_remove_all('... |
| ✓ | uit | content_text | text_corruption | medium | common | uit_PFF-1020-1_2018_autumn_1; uit_LRU-3300-1_2018_autumn_... | (1) Send '\\b\\d{4}/\\d{2,4}\\b' through .drop_academic_year() (generalised to accept '/'), so that only consecutive years (2023/24, 2023... |
| ✓ | usn | content_text | over_removal | medium | common | usn_MG1BO2-1_2020_autumn_2; usn_MG1BO1-1_2022_autumn_1; u... | Tighten the academic-year rule in .anon_generic()/.drop_academic_year(). (1) Match with '(?<![\\d-])\\d{4}\\s?[-–]\\s?\\d{2,4}\\b(?![-–]\... |
| ✓ | hiof | content_text | over_removal | medium | occasional | hiof_LMUSAM11221-1_2022_spring_2; hiof_LMBNOR40117-1_2021... | Guard the pattern so it does not match next to a slash: (?<![/\w])\d{4}/\d{2,4}\b(?!/). Tested: it leaves 'files/2019/06/27/' and 'view/3... |
| ✓ | oslomet | admin_date | admin_date_left | low | widespread | oslomet_M5GNA2100-1_2019_spring_2; oslomet_M1GNA2100-1_20... | In .anon_generic(), widen the season suffix to (?:en\|a\|semesteret\|semesteret\|semester)? in the '(?:høst\|vår\|haust\|sommer)(?:en\|a)?\s*\d{4... |
| ✓ | uib | content_text | text_corruption | low | widespread | uib_ENG125-0_2018_spring_1; uib_HIS250L-0_2020_spring_1; ... | Before the whitespace cleanup at the end of .anon_generic(), add str_replace_all("(?m)^[ \\t]*[.,;:][ \\t]*", ""), str_replace_all("[ \\t... |
| ✓ | hiof | content_text | over_removal | low | common | hiof_LMUENG10417-1_2019_spring_2; hiof_LMUSAM11221-1_2022... | Exempt ranges in square brackets and ranges right after a document number: add the lookbehinds (?<!\[)(?<!(?:Meld\. St\. \|St\.meld\. nr\.... |
| ✓ | uib | content_text | text_corruption | low | common | uib_FRANDI111-0_2017_autumn_1; uib_NODI101-0_2017_spring_... | Match the whole word: (høst\|vår\|haust\|autumn\|spring\|sommer\|summer)(?:semester(?:et)?\|en\|a)?\b, followed by an optional [.,]. This removes... |
| ✓ | uis | content_text | over_removal | low | common | uis_MGL3800-1_2023_spring_1; uis_MGL2050-1_2021_autumn_1;... | Replace the slash rule with str_replace_all("(?<![/.\\w])\\d{4}\\s?/\\s?\\d{2,4}\\b(?!/)", .drop_academic_year), widening the pattern in ... |
| ✓ | uit | content_text | over_removal | low | common | uit_LRU-3000-1_2015_autumn_1; uit_PFF-1020-1_2018_autumn_... | Fix (1) of the URL finding (routing yyyy/nn through .drop_academic_year()) already keeps 'Epub 2015/05/15'. For dd.mm.yyyy, skip dates di... |
| ✓ | uit | content_text | over_removal | low | common | uit_LER-2313-1_2023_autumn_2; uit_LER-2304-1_2024_spring_... | Change it to '(?i)\\b(høst\|vår\|haust\|autumn\|spring)\\s*\\d{2}\\b(?!\\s*(?:stp\|sp\|studiepoeng\|ects\|credits)\\b)'. Add a test: 'LER-2303 Na... |
| ✓ | usn | boilerplate | boilerplate_left | low | common | usn_MG1EN7-1_2023_autumn_1; usn_MG1BO1-1_2024_autumn_1 | In .anon_usn(), add Government Document\|Legislation\|E-book\|E-Book\|Research Dataset\|Dataset\|Video\|Other\|Journal\|Conference Proceeding\|News... |
| ✓ | hvl | boilerplate | text_corruption | low | occasional | hvl_LUPEM102-1_2024_spring_1; hvl_MGBMU241-1_2025_spring_... | Add `hvl = .anon_hvl(txt)` to .anon_institution(). Have .anon_hvl() drop the whole notice line while the year is still there to anchor on... |
| ✓ | nmbu | content_text | text_corruption | low | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPFD201-1_2025_autumn_2 | Make the academic-year patterns also take the preceding horizontal whitespace, e.g. "[ \\t]*\\b\\d{4}/\\d{2,4}\\b" (and the same for the ... |
| ✓ | nord | admin_date | admin_date_left | low | occasional | nord_PO118LS-1_2020_autumn_2; nord_HI109LS-1_2021_spring_... | In .anon_generic(), after the dd.mm.yyyy rule, also remove two-digit-year dates with a strict day and month: str_remove_all("\\b(?:0[1-9]... |
| ✓ | nord | content_text | over_removal | low | occasional | nord_NAT1002-1_2022_spring_3; nord_SAM1001-1_2021_autumn_... | Add a negative lookahead to the date rule in .anon_generic(): "\\d{1,2}\\.\\d{1,2}\\.\\d{4}(?!\\s*nr\\.?\\s*\\d)". Dates that cite a law ... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2022_autumn_1; u... | In .anon_uia(), extend the title pattern to an optional range: str_remove_all("\\((?:Høst\|Vår\|Haust)\\s+\\d{4}(?:\\s*[–-]\\s*(?:Høst\|Vår\|... |
| ✓ | uia | admin_date | text_corruption | low | occasional | uia_NO-505-1_2024_autumn_1; uia_NO-505-1_2020_autumn_1; u... | In .anon_generic(), let the lookbehind rule consume a coordinated second season: '(?i)(?<=(?:semester\|undervisning\|oppstart\|startsemester... |
| ✓ | uib | admin_date | admin_date_left | low | occasional | uib_HIS250L-0_2020_spring_1; uib_HIS250L-0_2019_spring_1;... | Add to .anon_generic(): str_remove_all("(?i)\\b\\d{1,2}\\.\\s*(?:januar\|februar\|mars\|april\|mai\|juni\|juli\|august\|september\|oktober\|novembe... |
| ✓ | uis | content_text | over_removal | low | occasional | uis_MGLT43-1_2025_autumn_1; uis_MGLT43-1_2023_autumn_1; u... | In .anon_generic() (or .anon_uis()), split the text at the first '(?m)^Litteratur\\s*$' heading. Apply only the personal-data rules (e-ma... |
| ✓ | uit | content_text | over_removal | low | occasional | uit_LRU-3000-1_2016_spring_1; uit_LRU-3000-1_2016_autumn_... | Before the academic-year step, mask ranges that follow a parliamentary-document cue: '(?i)(?:Meld\\.?\\s*St\\.?\|St\\.?\\s*meld\\.?\|Storti... |
| ✓ | uit | admin_date | admin_date_left | low | occasional | uit_LER-3500-1_2023_autumn_1; uit_NOR-3133-1_2023_autumn_... | In .anon_uit(), add str_replace_all("(?m)^(Pensumliste for [^\\n]*?)[ \\t]*[-–]?[ \\t]*\\b[HV]\\d{2}[ \\t]*$", "\\1"). Keep it anchored t... |
| ✓ | hiof | boilerplate | boilerplate_left | low | rare | hiof_LMBKRO10117-1_2019_spring_2 | In .anon_hiof(), before the generic pass, remove whole revision-note lines with str_remove_all("(?m)^[ \\t]*\\d{1,2}\\.\\d{1,2}\\.\\d{4}:... |
| ✓ | mf | admin_date | admin_date_left | low | rare | mf_SAM2080L-1_2025_autumn_1 | In .anon_generic(), make two changes. (1) Before the lookbehind rule, add stringr::str_replace_all("(?i)\\b((?:høst\|vår\|haust)semester(?:... |
| ✓ | nla | content_text | over_removal | low | rare | nla_MGL1KRLE201A-1_2020_spring_2; nla_MGL5KRLE201A-1_2020... | Skip durations: add a bounded negative lookbehind for duration labels, e.g. `(?<!(?i:lengde\|varighet\|spilletid\|length\|duration):?\s)\b\d{... |
| ✓ | uio | email | text_corruption | low | rare | uio_BIOS5000-1_2025_autumn_1 | In .anon_generic(), let the bare-email removal absorb the preceding horizontal space: str_remove_all("[ \\t]*\\b[\\w.+-]+@[\\w.-]+\\.[a-z... |
| ✓ | uit | admin_date | admin_date_left | low | rare | uit_EXF-0723-1_2008_autumn_1 | The same block removal as the pii_leak finding (strip '\nUndervisning[ \t]{2,}' to end of text in .anon_uit()) removes all of this. Do no... |
| ✓ | usn | admin_date | admin_date_left | low | rare | usn_LR-FYS2500-1_2021_spring_1 | In .anon_usn(), remove a month-year prefix at the start of a line and keep the change text: str_remove_all("(?im)^(?:januar\|februar\|mars\|... |
