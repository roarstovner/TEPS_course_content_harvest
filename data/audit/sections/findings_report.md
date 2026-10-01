# sections audit — aggregated findings

119 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: sonnet).
Mechanical verification: 88 passed (✓), 1 failed (✗), 30 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

| institution | target | error_type | severity | problem | evidence |
| --- | --- | --- | --- | --- | --- |
| hvl | assessment | formatting_noise | medium | evidence not_found | Karakterskala A-F, der F er ikkje greidd. Alle Mer om hjelpemiddel |

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 10 | hvl, inn, mf, nla, nmbu, nord, ntnu, oslomet, steiner, uib | high |
| assessment | wrong_content | 6 | hivolda, ntnu, uib, uis, uit, usn | high |
| learning_outcomes | truncated | 6 | inn, mf, ntnu, steiner, uis, uit | high |
| reading_list | boilerplate_only | 5 | hiof, mf, nih, nla, usn | high |
| prerequisites | boilerplate_only | 5 | nmbu, nord, ntnu, oslomet, uio | medium |
| prerequisites | empty_placeholder | 4 | hiof, nih, nla, uib | low |
| learning_outcomes | formatting_noise | 4 | hiof, mf, steiner, uib | medium |
| reading_list | missing_section | 4 | nord, ntnu, oslomet, uio | medium |
| all | missing_section | 3 | oslomet, uia, uis | high |
| course_content | formatting_noise | 3 | nih, uib, uit | high |
| cross_section | merged_sections | 3 | hivolda, uia, uis | high |
| learning_outcomes | merged_sections | 3 | uis, uit, usn | high |
| reading_list | wrong_content | 3 | hivolda, mf, uis | high |
| teaching_methods | missing_section | 3 | hivolda, mf, nla | high |
| coursework_requirements | formatting_noise | 3 | steiner, uit, usn | low |
| teaching_methods | wrong_content | 3 | nord, steiner, uit | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hvl | 8 | 2 | all / empty_placeholder | assessment / formatting_noise | assessment / merged_sections; assessment / boilerplate_only; prerequisites / merged_sections; coursework_requirements / boilerplate_only; coursework_requirements / wrong_content; course_content / boilerplate_only; reading_list / missing_section |
| nord | 5 | 7 | assessment / merged_sections; assessment / formatting_noise; prerequisites / boilerplate_only | assessment / boilerplate_only; teaching_methods / wrong_content; reading_list / missing_section; all / other | coursework_requirements / missing_section; prerequisites / empty_placeholder |
| oslomet | 5 | 7 | all / empty_placeholder | assessment / duplicate; cross_section / missing_section; all / missing_section; reading_list / missing_section; prerequisites / boilerplate_only; assessment / formatting_noise | assessment / missing_section; cross_section / duplicate; prerequisites / empty_placeholder; learning_outcomes / formatting_noise |
| uia | 2 | 4 |  | cross_section / merged_sections; all / missing_section; prerequisites / wrong_content; coursework_requirements / wrong_content | cross_section / formatting_noise; coursework_requirements / merged_sections |
| uis | 12 | 12 | cross_section / merged_sections; reading_list / wrong_content; learning_outcomes / truncated | learning_outcomes / merged_sections; all / missing_section; cross_section / other; coursework_requirements / other; reading_list / split_section; reading_list / formatting_noise; learning_outcomes / split_section; assessment / wrong_content; cross_section / formatting_noise | assessment / merged_sections; assessment / formatting_noise; course_content / merged_sections; reading_list / empty_placeholder; prerequisites / merged_sections; coursework_requirements / wrong_content; teaching_methods / missing_section; learning_outcomes / wrong_content; course_content / formatting_noise |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 16 |
| merged_sections | high | 11 |
| boilerplate_only | medium | 9 |
| missing_section | medium | 9 |
| missing_section | high | 8 |
| wrong_content | high | 8 |
| wrong_content | low | 8 |
| wrong_content | medium | 7 |
| formatting_noise | medium | 6 |
| empty_placeholder | low | 5 |
| formatting_noise | high | 5 |
| truncated | medium | 5 |
| boilerplate_only | high | 3 |
| other | medium | 3 |
| empty_placeholder | medium | 2 |
| merged_sections | medium | 2 |
| split_section | high | 2 |
| truncated | high | 2 |
| boilerplate_only | low | 1 |
| duplicate | low | 1 |
| duplicate | medium | 1 |
| empty_placeholder | high | 1 |
| field_or_language_junk | medium | 1 |
| missing_section | low | 1 |
| other | high | 1 |
| truncated | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 29 |
| learning_outcomes | 16 |
| reading_list | 16 |
| prerequisites | 15 |
| coursework_requirements | 10 |
| teaching_methods | 10 |
| cross_section | 9 |
| course_content | 8 |
| all | 6 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 28 | Section extraction for HiOF is very good: all seven canonical sections are split correctly at the headings, arbeidskrav are correctly filed under coursework_requirements, learning outcomes keep all three Kunnskap/Ferdigheter/Generell kompetanse groups, and admin blocks (Arbeidsomfang, Praksis, Se... |
| hivolda | sonnet | 23 | Section extraction is poor for hivolda. The plan's fixed boilerplate blocks (metadata header, Sensorordning, Evaluering, Maksimumstal, Emneansvarleg, Vurderingsform table, Godkjent av) are swallowed by neighbouring sections, and the 'Generell kompetanse' learning-outcome group is regularly split ... |
| hvl | sonnet | 28 | HVL section extraction is largely correct: all seven headings map to the right canonical sections, learning outcomes keep all sub-groups, and arbeidskrav land in coursework_requirements rather than assessment. The recurring defects are a UI link and an orphaned aids value ('Alle', 'Ingen') append... |
| inn | sonnet | 20 | Section splitting for inn is mostly good: course_content, teaching_methods, coursework_requirements and prerequisites land in the right sections. Two systematic defects remain. On English-language plans, learning_outcomes is reduced to the one-line intro sentence (the Knowledge/Skills/General com... |
| mf | sonnet | 20 | MF section extraction is systematically wrong: obligatory activities are filed under assessment, course_content is only a heading stub while the real description is dropped, reading_list never contains a reading list (only library boilerplate plus the course-responsible person's name/email), and ... |
| nih | sonnet | 28 | Section extraction works very well for NIH: headings (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur) map cleanly, arbeidskrav are correctly kept out of assessment, and learning outcomes are complete. The only real issues are that rea... |
| nla | sonnet | 28 | Section extraction works well for NLA: course_content, learning_outcomes, teaching_methods, coursework_requirements and assessment are cleanly and completely split, with arbeidskrav correctly kept out of assessment. Remaining problems are a dropped teaching_methods section on nynorsk plans, a boi... |
| nmbu | sonnet | 10 | Core sections (course_content, learning_outcomes, teaching_methods, reading_list, assessment body) are extracted cleanly and completely on NMBU, including full Kunnskap/Ferdigheter/Generell kompetanse groups. Two systematic defects: the 'Obligatorisk aktivitet' block is never extracted (no course... |
| nord | sonnet | 28 | Learning outcomes, course_content and teaching_methods are generally extracted cleanly and completely. The main defect is that Nord's combined 'vurdering' block is filed wholesale as assessment, so coursework requirements (arbeidskrav, obligatorisk deltakelse) are never separated, and assessment ... |
| ntnu |  | 28 | NTNU's h3-heading-based extractor correctly identifies and labels the five main sections present (course_content, learning_outcomes, teaching_methods, coursework_requirements, prerequisites), and section boundaries are accurate. The two critical defects are (1) the assessment section universally ... |
| oslomet | sonnet | 28 | Regular emneplaner (all 8 random controls) are split well: learning outcomes complete across Kunnskap/Ferdigheter/Generell kompetanse, arbeidskrav correctly separated from assessment. The suspects are almost all Praksis/shell courses where the harvested plan is only a pointer or a long fagplan, y... |
| steiner | sonnet | 14 | Section boundaries are mostly right (arbeidskrav correctly in coursework_requirements, exam text in assessment, no leakage of the flagged kind). Main defects: the course metadata table is prepended to learning_outcomes, some LO bullets and whole deleksamen lines are dropped, and Innhold sub-theme... |
| uia | sonnet | 28 | Heading-based extraction works well for UiA: learning outcomes keep all three competence groups, 'Vilkår for å gå opp til eksamen' goes to coursework_requirements, and the boilerplate/empty pre-pass flags are false alarms (identical practice-course texts, legitimately short sections). Remaining i... |
| uib |  | 28 | UiB's details/summary accordion + h2 hybrid extraction correctly opens accordion sections and maps headings to canonical sections for the vast majority of courses. Core academic sections (course_content, learning_outcomes, teaching_methods, coursework_requirements, assessment) are extracted with ... |
| uio | sonnet | 13 | Core sections (course_content, learning_outcomes, prerequisites, teaching_methods, assessment) are split cleanly at UiO's fixed headings, but coursework_requirements and reading_list are never emitted, and several generic boilerplate blocks plus un-anonymized contact details contaminate the secti... |
| uis | sonnet | 28 | UiS section extraction is unreliable. Many headings that UiS uses (Forkunnskapskrav, Eksamen / vurdering, Åpent for, Emneevaluering, Praksis, Overlapping, Fagperson(er), Kontakt) are not cut as boundaries, so text is merged into the preceding section; metadata headers, staff names and PDF page-br... |
| uit |  | 28 | UiT extraction quality is severely degraded by a single structural failure: the heading-splitter treats 'Hva lærer du' (the sub-heading that introduces the learning outcomes section) as a continuation of 'Innhold', then fails to cut at 'Undervisnings- og eksamensspråk' / 'Undervisning' / 'Kvalite... |
| usn | sonnet | 28 | Two distinct output regimes: the 8 random controls (LR/LH praksis, MG1/MG2 subject courses) split reasonably, but all 20 suspects (teacher-education PEL / Norwegian MG courses) are badly broken: learning_outcomes becomes a 10-30k char blob containing metadata, the reading list and the course-list... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | hivolda | reading_list | wrong_content | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL5-10NO2A-... | Add patterns mapping 'Om emnet' to course_content and 'Forkunnskapskrav...' to prerequisites in R/section_heading_map.R. Treat 'Pensum:' ... |
| ✓ | hivolda | cross_section | merged_sections | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7KRLE-1... | In R/section_heading_map.R, map 'Kunnskap(ar)', 'Ferdigheit(er)' and 'Generell kompetanse' to learning_outcomes (sub-headings, never boun... |
| ✓ | hivolda | coursework_requirements | merged_sections | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | In R/extract_sections.R (.clean_sections) or the heading map, treat 'Sensorordning', 'Evaluering og kvalitetssikring', 'Minimumstal', 'Ma... |
| ✓ | hivolda | assessment | missing_section | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | Add a hivolda rule starting a new assessment section at a line beginning with 'Vurderingsform Gruppering'. Also split out the arbeidskrav... |
| ✓ | inn | learning_outcomes | truncated | high | widespread | inn_2ML351-1_2024_autumn_1; inn_2ENL512-1-1_2023_spring_1... | In R/section_heading_map.R / R/extract_sections.R, treat Knowledge, Skills, General competence (and Norwegian equivalents) as sub-heading... |
| ✓ | mf | coursework_requirements | wrong_content | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | Add a pattern '^Obligatoriske aktiviteter' (and 'Listen over obligatoriske aktiviteter') mapped to coursework_requirements in R/section_h... |
| ✓ | mf | course_content | missing_section | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | In R/extract_sections.R add a preamble rule for MF: text between the 'Timeplan' line (end of the Emneinfo/Studieprogramtilhørighet block)... |
| ✓ | mf | reading_list | boilerplate_only | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_PED5610-1_2025_autumn_1; ... | Strip the fixed 'Tilgang til litteratur' paragraph and the stub line 'Litteraturlisten for ...' in .clean_sections(); emit no reading_lis... |
| ✓ | mf | reading_list | formatting_noise | high | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1014-1_2025_autumn_1; mf_... | In .clean_sections() (or an MF hook) truncate every section at the first line matching '^Emneansvarlig$' and drop everything after it. |
| ✓ | nmbu | prerequisites | formatting_noise | high | widespread | nmbu_PPRA301-1_2025_autumn_1; nmbu_MATH100-1_2025_spring_... | In R/extract_sections.R (.clean_sections or an nmbu pre-step) cut the plan at the first line matching '^Studieår:' and delete lines match... |
| ✓ | nmbu | coursework_requirements | missing_section | high | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_PPPE301-1_2025_spring_... | Add a pattern '^obligatoriske? aktivitet(er)?$' mapped to coursework_requirements in R/section_heading_map.R, and verify it stops at the ... |
| ✓ | nord | assessment | merged_sections | high | widespread | nord_PEL1001-1_2022_autumn_1; nord_KHV1004-1_2021_autumn_... | Add a Nord-specific post-split step in .clean_sections()/extract_sections.R that splits the assessment text by line/paragraph and moves l... |
| ? | ntnu | assessment | wrong_content | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2023_autumn_... | Strip lines matching the pattern 'Ordinær/Utsatt eksamen - (Høst\|Vår\|Sommer) \d{4}' and subsequent logistics rows (Karakter, Dato, Tid, S... |
| ? | ntnu | assessment | formatting_noise | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4621-1_2019_autumn_... | Add a post-processing step that drops any text block matching the regex 'function\s+\w+\s*\(' or 'const\s+\w+\s*=' from the assessment se... |
| ? | ntnu | teaching_methods | merged_sections | high | widespread | ntnu_PPU4621-1_2018_autumn_1; ntnu_PPU4623-1_2020_autumn_... | Add a split rule: within the teaching_methods text block, identify the paragraph starting with 'NTNU er tillagt et sertifiseringsansvar' ... |
| ? | uib | course_content | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Strip nodes matching the semester-picker before text extraction: remove any element whose text matches the pattern /^Vel emnebeskrivelse ... |
| ? | uib | assessment | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Add a post_fn that strips content from 'Vurderingsordning' onwards (inclusive) and strips the footer lines 'Dette bør du vite om eksamen'... |
| ✓ | uis | learning_outcomes | merged_sections | high | widespread | uis_LENG270-1_2015_autumn_1; uis_MGD3400-1_2022_autumn_3;... | Inspect the actual DOM level of these headings and set section_heading_selector/level for uis accordingly (or include h3/.paragraph--with... |
| ✓ | uis | cross_section | merged_sections | high | widespread | uis_MENG350-1_2014_autumn_1; uis_MGD3400-1_2022_autumn_3;... | Add admin-heading patterns (åpent for\|åpen for\|ope for, emneevaluering, overlapping, kontakt, fagperson) mapped to a drop/ignore marker s... |
| ? | uit | learning_outcomes | merged_sections | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisnings- og eksamensspråk', 'Undervisning', 'Kvalitetssikring', 'Kvalitetssikring av emnet', and 'Eksamen' as closing-boundary... |
| ? | uit | assessment | wrong_content | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Assign the 'Eksamen' heading to assessment as its opening boundary (extracting everything from 'Vurderingsform:…' through 'Kontinuasjonse... |
| ? | uit | cross_section | missing_section | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisning' as a heading pattern mapped to teaching_methods in the heading map. This is a prerequisite change alongside fixing the... |
| ✓ | usn | learning_outcomes | merged_sections | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE3-1_2021_spring_2; u... | For USN, drop the Innholdsfortegnelse block and the front-matter (Emnekode...Ansvarlig) before splitting, and add/prioritize heading patt... |
| ✓ | usn | reading_list | boilerplate_only | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE2-1_2023_spring_1; u... | Remove the 'Emnet inngår i følgende studier' mapping to reading_list (map it to an ignored/drop section), and ensure 'Litteratur' (and 'O... |
| ✓ | usn | assessment | wrong_content | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE1-2_2021_autumn_2; u... | Fix the splitting order (see blob finding), make 'Generell kompetanse' a sub-heading that stays in learning_outcomes, strip 'Godkjent emn... |
| ✓ | hivolda | teaching_methods | missing_section | high | common | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | Add 'Praktisk organisering( og arbeidsmåtar)?' to the teaching_methods patterns in R/section_heading_map.R. |
| ✓ | mf | learning_outcomes | truncated | high | common | mf_PPU1015-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1; m... | Anchor sub-heading patterns in R/section_heading_map.R to whole lines ('^(KUNNSKAP\|FERDIGHETER\|GENERELL KOMPETANSE)$', case-insensitive) ... |
| ✓ | oslomet | all | empty_placeholder | high | common | oslomet_M1GP3000-1_2023_autumn_3; oslomet_M5GP4200-1_2025... | In .clean_sections() (R/extract_sections.R) drop rows whose text matches ^(Se (under )?.{0,40}fagplan(en)?\.?\s*)+$ so they become missin... |
| ✓ | oslomet | cross_section | missing_section | high | common | oslomet_M1GP4200-1_2025_autumn_3; oslomet_M5GP4200-1_2025... | Add heading patterns in R/section_heading_map.R for the Fagplan headings (Organisering og arbeidsmåter/Undervisning/Veiledning -> teachin... |
| ✓ | uio | coursework_requirements | missing_section | high | common | uio_HIS1200L-1_2025_spring_1; uio_HIS4015L-1_2025_spring_... | Add heading patterns (Obligatoriske aktiviteter/komponenter/forhold i undervisningen, Faglige krav for å kunne avlegge eksamen, Arbeidskr... |
| ✓ | uis | reading_list | wrong_content | high | common | uis_LENG360-1_2019_autumn_1; uis_MENG250-1_2014_autumn_1;... | Match reading_list headings only on exact, whole-line headings ('Litteratur', 'Pensum', 'Pensumlitteratur', 'Pensumliste'), not startsWit... |
| ✓ | uis | all | missing_section | high | common | uis_LMLIMAS-1_2017_spring_1; uis_MENG165-1_2014_autumn_1;... | Debug extract_sections_html for these course_ids and make the text_split fallback fire when the heading path yields zero rows (headings: ... |
| ? | uit | coursework_requirements | merged_sections | high | common | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Add 'Mer info om vurderingsform' as a secondary split point that closes coursework_requirements and opens assessment. Alternatively, stri... |
| ✓ | uis | learning_outcomes | split_section | high | occasional | uis_LENG115-1_2017_autumn_1 | Pre-clean PDF text before splitting: remove running headers ('Emne XXX, BOKMÅL, ... versjon'), 'side N' lines, 'Powered by TCPDF', and de... |
| ? | uit | assessment | merged_sections | high | occasional | uit_LRU-2642-1_2014_spring_1; uit_PFF-3102-1_2014_autumn_... | Detect the pattern 'Følgende arbeidskrav må være godkjent før man kan fremstille seg for eksamen:' (the older phrasing) as a secondary tr... |
| ? | uit | course_content | merged_sections | high | occasional | uit_LER-1353-1_2023_spring_1; uit_LRU-2642-1_2014_spring_... | Map 'Hva lærer du' and 'Etter bestått emne skal studentene ha følgende læringsresultat' (older phrasing) as opening boundaries for learni... |
| ✓ | mf | learning_outcomes | wrong_content | high | rare | mf_PRA1005-1_2025_autumn_1 | Same as truncation fix: anchor sub-heading patterns to whole lines and ensure 'Læringsutbytte' always starts learning_outcomes. |
| ✓ | mf | reading_list | wrong_content | high | rare | mf_PPU1015-1_2025_autumn_1 | Anchor the reading_list pattern to a full-line heading ('^Litteraturliste$'); never match bullet lines. |
| ✓ | nord | assessment | boilerplate_only | high | rare | nord_RL211L-1_2020_autumn_3 | After stripping COVID notices (see other finding), drop assessment rows that become empty instead of emitting the notice. |
| ✓ | uio | teaching_methods | other | high | rare | uio_MAT5930L-2_2025_spring_1; uio_PROF3025-1_2025_spring_1 | Run extract_sections on the anonymized course_plan (or apply anonymize_fulltext to each section) and add a check in R/audit/qa_sections.R... |
| ✓ | uis | reading_list | split_section | high | rare | uis_MGL4300-1_2021_autumn_1 | Once a reading_list heading has been seen, do not allow switching to other sections except through exact whole-line headings; check which... |
| ✗ | hvl | assessment | formatting_noise | medium | widespread | hvl_MGUSA102-1_2022_spring_1; hvl_MGBSA101-1_2024_spring_... | In R/extract_sections.R .clean_sections() (or an hvl-specific post step), drop lines matching ^(Mer\|Meir) om hjelpemiddel(er)?$. Either m... |
| ✓ | inn | assessment | field_or_language_junk | medium | widespread | inn_2ML351-1_2024_autumn_1; inn_2ENL51-7-1_2024_autumn_1;... | Add a pattern for 'Language of instruction( and examination)?' / 'Undervisnings- og eksamensspråk' mapped to a dropped/ignored section an... |
| ✓ | inn | assessment | truncated | medium | widespread | inn_2MKRLE171-1-1_2023_spring_1; inn_2ENL51-8-1_2023_autu... | Map 'Form of assessment' (and 'Examination') to assessment in R/section_heading_map.R so the repeated heading continues the same section ... |
| ✓ | mf | course_content | empty_placeholder | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_PRA1001-1_2025_autumn_1; ... | Remap the heading to coursework_requirements and drop heading-only rows in .clean_sections() in R/extract_sections.R (text remaining afte... |
| ✓ | mf | learning_outcomes | formatting_noise | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf_... | Add 'Overlappende emner' as a discard/boundary heading in R/section_heading_map.R; remove the 'Litteraturliste' stub line. |
| ✓ | mf | assessment | formatting_noise | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1014-1_2025_autumn_1; m... | Cut assessment at 'Eksamensdatoer' (or drop lines from 'Eksamensdato:'/'Oppgaven utleveres:' to 'Sensur kunngjøres innen:' plus the follo... |
| ✓ | nih | reading_list | boilerplate_only | medium | widespread | nih_LKI120-1_2021_autumn_2; nih_LKI110-1_2023_autumn_1; n... | In .clean_sections() (R/extract_sections.R), drop reading_list rows that match ^(se emnearkivet\|pensum(lista\|liste) for (høsten\|våren) \d... |
| ✓ | nla | reading_list | boilerplate_only | medium | widespread | nla_4MGL5MA103-1_2025_spring_1; nla_MGL1NO201-1_2025_autu... | Either capture the href of the 'her' link into reading_list, or drop the row in .clean_sections() when the text matches '^Litteratur og f... |
| ? | ntnu | reading_list | missing_section | medium | widespread | ntnu_PPU4729-1_2016_autumn_1; ntnu_HIST3485-1_2019_spring... | Add 'Kursmateriell' to the heading-to-section map, mapped to reading_list. Also consider 'Faglig innhold: Kursmateriell' if the heading a... |
| ✓ | oslomet | reading_list | missing_section | medium | widespread | oslomet_M5GNA3100-1_2019_autumn_2; oslomet_MLEST2100-1_20... | Extend the oslomet fetch/selector to include the pensum block (or fetch the pensum endpoint) and map 'Pensum' in R/section_heading_map.R ... |
| ✓ | uia | cross_section | merged_sections | medium | widespread | uia_PRA1-301-1_2025_autumn_1; uia_PRA2-302-1_2024_spring_... | Add a pattern for '^Faget i praksis$' in R/section_heading_map.R that starts a new (discarded or teaching_methods) block, placed before t... |
| ? | uib | prerequisites | merged_sections | medium | widespread | uib_HIS302L-0_2025_autumn_1; uib_HIS303L-0_2025_autumn_1;... | Map 'Krav til studierett' to no canonical section (drop it). Add a post_fn for prerequisites that removes lines consisting solely of 'Ing... |
| ✓ | uio | assessment | boilerplate_only | medium | widespread | uio_PROMO8-1_2025_autumn_4; uio_PROMO4-1_2025_autumn_4; u... | In .clean_sections() (or .anon_uio()), cut everything from the line 'Mer om eksamen ved UiO' to the end, and optionally drop the standalo... |
| ✓ | uio | prerequisites | boilerplate_only | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_NOR1000-1_2025_spring_1... | Strip the two standard Studentweb/studieprogrammer sentences in .clean_sections() for uio; consider splitting 'Opptak til emnet' from 'Ob... |
| ✓ | uis | coursework_requirements | other | medium | widespread | uis_MGL3066-1_2025_autumn_1; uis_MGL3120-1_2022_autumn_1;... | Map '^fagperson' to an ignore marker in R/section_heading_map.R so the staff list is discarded at the section stage. |
| ? | uit | prerequisites | wrong_content | medium | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Post-process the prerequisites field to strip sentences matching the pattern 'Når eksamen i … er bestått, kan studenten framstille seg ti... |
| ✓ | hiof | reading_list | boilerplate_only | medium | common | hiof_LMBNOR10417-1_2025_autumn_1; hiof_LMUMAT40117-1_2025... | In .clean_sections() (R/extract_sections.R), drop reading_list rows that match /Leganto/ and are shorter than ~150 chars (or set them to ... |
| ✓ | hivolda | assessment | wrong_content | medium | common | hivolda_MGL5-10EN3C-1_2021_spring_2; hivolda_MGL5-10EN1A-... | Keep the whole 'Vilkår for å framstille seg til eksamen' block together as coursework_requirements up to 'Sensorordning'. Fix the EN3C he... |
| ✓ | hvl | all | empty_placeholder | medium | common | hvl_MGPRA10-1_2025_spring_1; hvl_MGPRA5-1_2025_spring_1; ... | In .clean_sections(), after normalizing, treat text matching ^(-\|Ingen)([\s\n]+(-\|Ingen))*$ as empty and drop the row, consistently for a... |
| ✓ | mf | teaching_methods | missing_section | medium | common | mf_RL1014-1_2025_autumn_1; mf_PRA1005-1_2025_autumn_1; mf... | Add '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R, ending the section at 'Om studiet'/'Oblig... |
| ✓ | nmbu | prerequisites | boilerplate_only | medium | common | nmbu_PPXP100-1_2025_autumn_1 | Do not map 'Opptakskrav' to prerequisites, and suppress the prerequisites row when nothing remains after removing metadata/CSS. |
| ✓ | nord | assessment | formatting_noise | medium | common | nord_NAT2004-1_2020_spring_2; nord_MUS2003-1_2019_autumn_... | In .clean_sections() (or the Nord anonymization handler) drop paragraphs starting with 'MERK: Våren', 'På bakgrunn av Covid-19', 'Endring... |
| ✓ | nord | prerequisites | boilerplate_only | medium | common | nord_MAT1007-1_2024_autumn_3; nord_KHV1004-1_2021_autumn_... | Add a Nord filter in .clean_sections(): drop prerequisites that match 'Opptak skjer på bakgrunn', 'Kan tas som frittstående', 'Ingen utov... |
| ? | ntnu | prerequisites | boilerplate_only | medium | common | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2018_autumn_... | Add a post-processing filter: if the prerequisites text exactly matches the known NTNU boilerplate strings ('Beståtte eksamener i henhold... |
| ✓ | oslomet | assessment | duplicate | medium | common | oslomet_M5GP3000-1_2023_autumn_4; oslomet_M1GP3200-1_2024... | De-duplicate identical paragraphs when merging multiple blocks into one section in .clean_sections(). |
| ✓ | oslomet | prerequisites | boilerplate_only | medium | common | oslomet_M5GNA3100-1_2019_autumn_2; oslomet_M1GMU3100-1_20... | Drop prerequisites rows matching ^Se (under )?.*fagplan and strip admission-only paragraphs (Opptak, søkere, valgfag for aktive studenter... |
| ✓ | steiner | learning_outcomes | formatting_noise | medium | common | steiner_M-PEL1_2_2025_spring_1; steiner_M-PEL1_3_2025_spr... | In .clean_sections()/steiner strategy, drop the preamble before the first heading (or strip lines beginning 'Emnekode og ', 'Emnenavn', '... |
| ✓ | steiner | learning_outcomes | truncated | medium | common | steiner_M-PEL1_2_2025_spring_1; steiner_M-PEL1_1_2025_spr... | Restrict sub-heading stripping to lines that exactly equal 'Kunnskap', 'Ferdigheter', 'Generell kompetanse' (anchored ^...$), and do not ... |
| ✓ | steiner | teaching_methods | wrong_content | medium | common | steiner_M-PEL1_3_2025_spring_1; steiner_M-MAT1_1_2025_spr... | In R/section_heading_map.R only match 'Arbeidsmåter' / 'Undervisnings- og arbeidsformer' as teaching_methods when the line is the whole h... |
| ✓ | uis | learning_outcomes | truncated | medium | common | uis_MGL4066-1_2025_autumn_3; uis_MGD3400-1_2022_autumn_3;... | Start learning_outcomes at the first node after the 'Læringsutbytte' heading node, and do not treat 'Kunnskap', 'Ferdigheter', 'Generell ... |
| ✓ | uis | cross_section | other | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL4400-1_2023_autumn_1;... | Drop all text before the first mapped heading, or assign the unheaded introduction to course_content. Strip the lines 'Emnekode:', 'Vekti... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In .clean_sections() (or the reading_list cleaner) remove tokens matching 'https://bibsys-ur\.userservices[^\s]*' and the following conti... |
| ✓ | hivolda | course_content | wrong_content | medium | occasional | hivolda_MGL5-10SA2B-1_2023_autumn_1; hivolda_MGL5-10SA2B-... | Map 'Studentane skal ha/kunne...' and the 'Kunnskapar' sub-heading to learning_outcomes. Review the SA2B heading variants and map them to... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Extend the prerequisites pattern to '^Forkunnskap(er\|skrav)' and allow 'Label: text' on one line. |
| ✓ | nla | teaching_methods | missing_section | medium | occasional | nla_MGL1NO201-1_2025_autumn_1; nla_4MGL1NO201-1_2025_autu... | Make the teaching-methods heading pattern tolerant: 'arbeids-? og undervis(n)?ingsformer' (optionally also 'undervisning'), mapped to tea... |
| ✓ | nord | teaching_methods | wrong_content | medium | occasional | nord_REL1005-1_2021_autumn_1; nord_RL211L-1_2020_autumn_3 | Optionally move paragraphs starting with 'Arbeidskrav' / 'Arbeidskrava' from teaching_methods to coursework_requirements; include Nynorsk... |
| ✓ | nord | all | other | medium | occasional | nord_SP154L-1_2018_autumn_3; nord_HI109LS-1_2016_autumn_2... | Exclude the overlap block ('Med overlapp menes ...') in the Nord selector or pre-processing so such courses get NA extracted_text rather ... |
| ? | ntnu | assessment | missing_section | medium | occasional | ntnu_PPU4700-1_2012_autumn_1; ntnu_PPU4611-1_2018_autumn_1 | When no assessment row is extracted but the course metadata contains a non-empty 'Vurderingsordning' field, emit a diagnostic flag so the... |
| ✓ | steiner | assessment | truncated | medium | occasional | steiner_M-NOR1_3_2025_spring_1; steiner_M-MAT1_2_2025_spr... | When removing the 'Eksamensform/Hjelpemidler/Sensorordning/Vurderingsuttrykk' labels, remove only exact label lines; keep any line carryi... |
| ✓ | uia | all | missing_section | medium | occasional | uia_EN-156-1_2023_autumn_1; uia_NAT115-1_2023_autumn_1; u... | Re-fetch these 2023-autumn plans and inspect the HTML; fix selector or year-specific structure in R/institution_config.R (uia). Exclude c... |
| ? | uib | assessment | wrong_content | medium | occasional | uib_SOS340-L-0_2025_spring_1 | Add a post_fn strip for lines matching 'Vi opplever problemer med å hente inn eksamensinformasjon' and 'Lukk'. Alternatively, identify th... |
| ✓ | uis | assessment | wrong_content | medium | occasional | uis_LHIS145-1_2019_autumn_1 | Treat the 'Vilkår ...' heading as opening coursework_requirements until the next mapped heading; do not let 'Obligatorisk aktivitet' or '... |
| ? | uit | learning_outcomes | truncated | medium | occasional | uit_LER-3903-1_2024_autumn_1; uit_LER-3913-1_2024_autumn_... | Investigate and raise (or remove) any field-length cap applied to sections_raw text. Once the merge is fixed the correct learning_outcome... |
| ✓ | oslomet | all | missing_section | medium | rare | oslomet_M5GEN1200-1_2023_autumn_1; oslomet_M5GNT3200-1_20... | Check the fulltext step for these courses (selector missed content or page genuinely empty); flag courses with plans lacking any content ... |
| ✓ | uio | reading_list | missing_section | medium | rare | uio_HIS4015L-1_2025_spring_1 | Add 'Pensum' to reading_list patterns in R/section_heading_map.R and allow the UiO splitter to cut at it when it occurs within Undervisni... |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_2MPRA171S-4-1_2022_spring_1; inn_2MPRA171-1-1_2022_sp... | In .clean_sections() strip the fixed header strings 'Vurderingsordning Karakterskala Gruppe/individuell Varighet Hjelpemidler Andel Komme... |
| ✓ | inn | reading_list | empty_placeholder | low | widespread | inn_2ML351-1_2024_autumn_1; inn_2MPRA171-1-1_2022_spring_1 | In .clean_sections() drop reading_list rows matching '^(No reading list available\|Ingen pensumliste tilgjengelig)' and prerequisites matc... |
| ✓ | nla | prerequisites | empty_placeholder | low | widespread | nla_4MGL5MA103-1_2025_spring_1; nla_4MGL5KRLE101-1_2025_a... | Treat prerequisites matching '^Se programplan\.?$' as empty and do not emit a row in .clean_sections(). |
| ✓ | nmbu | assessment | formatting_noise | low | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_FYS100-1_2025_spring_1... | In .clean_sections() (or an nmbu-specific post_fn) remove lines matching 'Karakterregel:.*' from assessment and collapse runs of blank li... |
| ✓ | nord | reading_list | missing_section | low | widespread | nord_REL1005-1_2021_autumn_1; nord_PEL1001-1_2022_autumn_1 | Check whether the Nord page has a pensum element and add it to the selector; otherwise document that reading_list is unavailable for nord... |
| ✓ | steiner | assessment | formatting_noise | low | widespread | steiner_M-PEL1_1_2025_spring_1; steiner_M-NAT1_2_2025_spr... | Either strip all five exam sub-headings consistently (add 'Sensorordning'), or better keep all labels; optionally drop Sensorordning/Hjel... |
| ? | uib | cross_section | duplicate | low | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | No change needed to the extractor. Note this duplication in data documentation so downstream consumers are not confused by the repeated c... |
| ? | uit | course_content | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Strip the literal string 'Hva lærer du' (and its variants) from the tail of the course_content field during post-processing (post_fn), or... |
| ? | uit | coursework_requirements | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3902-1_2024_autumn_... | Add a post_fn (or pre_fn cleaning step) that strips the literal strings 'UiTs samleside om eksamen', 'Mer info om arbeidskrav', and 'Mer ... |
| ✓ | nih | prerequisites | empty_placeholder | low | common | nih_LKI110-1_2025_autumn_1; nih_LKI226-1_2025_autumn_1; n... | Map "Hvem kan ta dette emnet?" to an ignored/admin bucket in R/section_heading_map.R, and have .clean_sections() drop prerequisites rows ... |
| ✓ | nord | assessment | formatting_noise | low | common | nord_PO118LS-1_2025_autumn_3; nord_MAT1007-1_2024_autumn_3 | Strip trailing lines equal to 'Ingen'/'Ingen.' and lines matching aid/place logistics ('Eksamenssted følger', 'Hjelpemidler', calculator/... |
| ? | ntnu | teaching_methods | boilerplate_only | low | common | ntnu_PPU4623-1_2021_autumn_1; ntnu_PPU4625-1_2021_autumn_... | Add a post-processing blocklist for the NTNU certification-responsibility paragraph: match on 'NTNU er tillagt et sertifiseringsansvar' a... |
| ✓ | oslomet | assessment | formatting_noise | low | common | oslomet_M5GNA3100-1_2019_autumn_2; oslomet_M1GNO3100-1_20... | Remove standalone 'Se (under) Vurdering/eksamen.' lines and replace stray ';' separators with newlines in the oslomet pre/post-processing... |
| ✓ | steiner | cross_section | formatting_noise | low | common | steiner_M-NOR1_1_2025_spring_1; steiner_M-NAT1_2_2025_spr... | In .clean_sections() remove lines matching ^\s*\d{1,3}\s*$ before splitting and for all sections. |
| ✓ | uia | coursework_requirements | wrong_content | low | common | uia_MA-441-1_2024_autumn_3; uia_PRA1-301-1_2025_autumn_1;... | Optional sentence-level post-step for uia moving sentences matching 'krav om .*(tilstedeværelse\|deltagelse\|frammøte)' to coursework_requi... |
| ? | uib | prerequisites | empty_placeholder | low | common | uib_RELV302L-0_2025_autumn_1; uib_HIS301L-0_2025_autumn_1... | Collapse multiple 'Ingen'/'\-' values in prerequisites to a single token, or to NULL/empty if all constituent fields are null placeholders. |
| ? | uib | assessment | wrong_content | low | common | uib_NOLI103-L-0_2025_autumn_1; uib_NORAN204-L-0_2025_autu... | Add this exact boilerplate sentence to the list of patterns stripped in the assessment post_fn (e.g. regex: /Klokkeslett for oppstart av ... |
| ? | uib | assessment | wrong_content | low | common | uib_HIDID112-0_2025_autumn_1; uib_HIS302L-0_2025_autumn_1... | Identify the 'Hjelpemiddel til eksamen' sub-section within the assessment panel and drop it, or add its null-value strings ('Ingen', '-')... |
| ✓ | uis | cross_section | formatting_noise | low | common | uis_MGL1041-1_2018_autumn_2; uis_LMHIMAS-1_2018_spring_1;... | Add regex removal of '^\s*side \d+\s*$', '^\s*EMNE \S+ .*Versjon.*$', 'Powered by TCPDF.*' and trim leading whitespace per line in .clean... |
| ✓ | usn | coursework_requirements | formatting_noise | low | common | usn_MG2PE2-1_2023_spring_1 | In .clean_sections() remove a leading line identical to the section heading and collapse consecutive duplicate lines. |
| ? | ntnu | learning_outcomes | truncated | low | occasional | ntnu_PPU4623-1_2019_autumn_1; ntnu_PPU4625-1_2019_autumn_... | Increase or remove the character limit on the sections_raw text field. The truncation point is consistent across affected courses suggest... |
| ✓ | steiner | coursework_requirements | formatting_noise | low | occasional | steiner_M-NOR1_3_2025_spring_1 | Only strip the exact section heading line 'Arbeidskrav' (anchored) and keep sub-titles. |
| ✓ | uia | prerequisites | wrong_content | low | occasional | uia_MA-172-1_2022_autumn_4; uia_MA-219-1_2020_autumn_1; u... | Narrow the prerequisites pattern so 'Opptakskrav hvis/om tilbudt som enkeltemne' is not matched (map to a discarded block). |
| ? | uib | course_content | wrong_content | low | occasional | uib_SPLA106-0_2025_autumn_1; uib_NOLISP300-L-0_2025_autum... | No change required; this is correctly handled. Document in data notes that SPLA106-type short courses may have thin course_content becaus... |
| ? | uib | learning_outcomes | formatting_noise | low | occasional | uib_RELV107-0_2025_autumn_2 | Add a learning_outcomes post_fn that collapses paragraph breaks that split a sentence (i.e., where a paragraph ends mid-word or without t... |
| ? | uit | prerequisites | wrong_content | low | occasional | uit_LER-2152-1_2025_spring_1 | Strip paragraphs matching 'Studiepoengreduksjon' (and the credit-reduction boilerplate that follows) in the prerequisites post_fn, or add... |
| ? | uit | teaching_methods | wrong_content | low | occasional | uit_LRU-3300-1_2016_autumn_1; uit_LRU-2642-1_2014_spring_... | Add patterns for 'For nærmere informasjon om praksis, se egen praksisplan' and 'Emnet evalueres muntlig eller skriftlig minimum en gang h... |
| ✓ | usn | teaching_methods | formatting_noise | low | occasional | usn_LR-PRA3000-1_2021_autumn_1; usn_LH-PRA4000-1_2021_aut... | Add a pattern for '^Obligatorisk aktivitet( og krav til tilstedeværelse)?$' mapped to coursework_requirements in R/section_heading_map.R ... |
| ✓ | hiof | prerequisites | empty_placeholder | low | rare | hiof_LMUMUS11423-1_2025_spring_2 | In .clean_sections(), remove lines that are exactly 'Ingen'/'Ingen.'/'-' from prerequisites text, and drop the row if nothing remains. |
| ✓ | hiof | learning_outcomes | formatting_noise | low | rare | hiof_LMUMUS11423-1_2025_spring_2; hiof_LMUMUS10323-1_2025... | Insert a newline or space between an adjacent heading-like word (Kunnskap/Ferdigheter/Generell kompetanse) and 'Studenten'/'Kandidaten' i... |
| ✓ | mf | learning_outcomes | wrong_content | low | rare | mf_RL1013-1_2025_autumn_1 | Do not map 'Delemne X:' headings outside the Læringsutbytte block to learning_outcomes; keep them as course_content and preserve document... |
| ✓ | nih | course_content | formatting_noise | low | rare | nih_LKI226-1_2025_autumn_1 | For nih, add a pre_fn in R/institution_config.R that inserts a newline for <br> and between adjacent block elements before text extraction. |
| ✓ | nla | assessment | formatting_noise | low | rare | nla_MGL1NO102-1_2025_autumn_1; nla_4MGL5KRLE101-1_2025_au... | In .clean_sections() remove lines consisting solely of 'Bokmål' or 'Nynorsk'. |
