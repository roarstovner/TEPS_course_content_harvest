# sections audit — aggregated findings

120 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: sonnet).
Mechanical verification: 56 passed (✓), 0 failed (✗), 64 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

**Stale reports** (older than their packet — not from this run, or the agent never wrote its file): nord, oslomet, uia

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 9 | inn, mf, nla, nmbu, nord, ntnu, steiner, uib, uis | high |
| learning_outcomes | truncated | 6 | inn, mf, ntnu, steiner, uis, uit | high |
| prerequisites | empty_placeholder | 6 | hiof, nih, nla, nord, oslomet, uib | low |
| assessment | wrong_content | 5 | hivolda, ntnu, uib, uit, usn | high |
| reading_list | boilerplate_only | 5 | hiof, mf, nih, nla, usn | high |
| learning_outcomes | formatting_noise | 5 | hiof, mf, oslomet, steiner, uib | medium |
| assessment | merged_sections | 4 | hvl, nord, uis, uit | high |
| course_content | formatting_noise | 4 | nih, uib, uis, uit | high |
| teaching_methods | missing_section | 4 | hivolda, mf, nla, uis | high |
| prerequisites | boilerplate_only | 4 | nmbu, nord, ntnu, uio | medium |
| assessment | missing_section | 3 | hivolda, ntnu, oslomet | high |
| coursework_requirements | merged_sections | 3 | hivolda, uia, uit | high |
| coursework_requirements | missing_section | 3 | nmbu, nord, uio | high |
| coursework_requirements | wrong_content | 3 | hvl, mf, uis | high |
| reading_list | wrong_content | 3 | hivolda, mf, uis | high |
| coursework_requirements | formatting_noise | 3 | steiner, uit, usn | low |
| prerequisites | merged_sections | 3 | hvl, uib, uis | medium |
| reading_list | missing_section | 3 | hvl, ntnu, uio | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 7 | 3 |  | reading_list / boilerplate_only; prerequisites / empty_placeholder; learning_outcomes / formatting_noise | assessment / merged_sections; coursework_requirements / missing_section; prerequisites / other; assessment / formatting_noise; teaching_methods / wrong_content; reading_list / truncated; reading_list / empty_placeholder |
| hivolda | 10 | 7 | reading_list / wrong_content; teaching_methods / missing_section; coursework_requirements / merged_sections; assessment / wrong_content | cross_section / merged_sections; assessment / missing_section; course_content / wrong_content | assessment / boilerplate_only; learning_outcomes / merged_sections; course_content / missing_section; prerequisites / merged_sections; learning_outcomes / truncated; reading_list / missing_section |
| inn | 11 | 5 | learning_outcomes / truncated; assessment / truncated; assessment / formatting_noise | assessment / field_or_language_junk; reading_list / empty_placeholder | course_content / merged_sections; learning_outcomes / merged_sections; teaching_methods / missing_section; coursework_requirements / merged_sections; reading_list / wrong_content; reading_list / boilerplate_only; prerequisites / missing_section; cross_section / other |
| nla | 9 | 4 | reading_list / boilerplate_only | teaching_methods / missing_section; prerequisites / empty_placeholder; assessment / formatting_noise | assessment / field_or_language_junk; prerequisites / boilerplate_only; coursework_requirements / boilerplate_only; assessment / missing_section; course_content / truncated; learning_outcomes / truncated; assessment / empty_placeholder; cross_section / other |
| nmbu | 10 | 4 | prerequisites / boilerplate_only | prerequisites / formatting_noise; coursework_requirements / missing_section; assessment / formatting_noise | course_content / merged_sections; learning_outcomes / missing_section; prerequisites / merged_sections; assessment / merged_sections; assessment / truncated; coursework_requirements / merged_sections; teaching_methods / merged_sections; reading_list / wrong_content; cross_section / formatting_noise |
| nord | 9 | 5 | coursework_requirements / missing_section; assessment / formatting_noise; prerequisites / boilerplate_only; prerequisites / empty_placeholder | assessment / merged_sections | assessment / wrong_content; prerequisites / missing_section; teaching_methods / wrong_content; course_content / split_section; reading_list / missing_section |
| oslomet | 10 | 5 | all / empty_placeholder; cross_section / duplicate; learning_outcomes / formatting_noise | assessment / missing_section; prerequisites / empty_placeholder | all / missing_section; teaching_methods / empty_placeholder; prerequisites / boilerplate_only; cross_section / wrong_content; learning_outcomes / field_or_language_junk; course_content / formatting_noise; assessment / formatting_noise |
| steiner | 6 | 7 | learning_outcomes / truncated; coursework_requirements / formatting_noise | learning_outcomes / formatting_noise; teaching_methods / wrong_content; assessment / truncated; assessment / formatting_noise; cross_section / formatting_noise | assessment / merged_sections; all / formatting_noise; teaching_methods / missing_section; course_content / merged_sections |
| uia | 8 | 2 |  | cross_section / formatting_noise; coursework_requirements / merged_sections | assessment / merged_sections; prerequisites / boilerplate_only; prerequisites / empty_placeholder; course_content / merged_sections; learning_outcomes / merged_sections; learning_outcomes / truncated; assessment / boilerplate_only; cross_section / missing_section |
| uio | 5 | 5 | coursework_requirements / missing_section; reading_list / missing_section; assessment / boilerplate_only; prerequisites / boilerplate_only; teaching_methods / other |  |  |
| usn | 11 | 5 | assessment / wrong_content | learning_outcomes / merged_sections; reading_list / boilerplate_only; teaching_methods / formatting_noise; coursework_requirements / formatting_noise | coursework_requirements / merged_sections; assessment / formatting_noise; reading_list / wrong_content; reading_list / merged_sections; course_content / truncated; teaching_methods / merged_sections; learning_outcomes / truncated; prerequisites / empty_placeholder; cross_section / formatting_noise; assessment / missing_section |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 16 |
| merged_sections | high | 12 |
| boilerplate_only | medium | 9 |
| empty_placeholder | low | 8 |
| missing_section | high | 8 |
| wrong_content | high | 8 |
| wrong_content | low | 8 |
| missing_section | medium | 7 |
| wrong_content | medium | 7 |
| formatting_noise | high | 5 |
| formatting_noise | medium | 5 |
| boilerplate_only | low | 4 |
| truncated | medium | 4 |
| merged_sections | medium | 3 |
| truncated | high | 3 |
| boilerplate_only | high | 2 |
| duplicate | low | 2 |
| empty_placeholder | medium | 2 |
| merged_sections | low | 2 |
| empty_placeholder | high | 1 |
| field_or_language_junk | medium | 1 |
| missing_section | low | 1 |
| other | high | 1 |
| truncated | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 28 |
| learning_outcomes | 17 |
| prerequisites | 17 |
| reading_list | 14 |
| coursework_requirements | 13 |
| course_content | 12 |
| teaching_methods | 10 |
| cross_section | 7 |
| all | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 28 | Section extraction for HiOF is very good: all seven canonical sections are split correctly at the headings, arbeidskrav are correctly filed under coursework_requirements, learning outcomes keep all three Kunnskap/Ferdigheter/Generell kompetanse groups, and admin blocks (Arbeidsomfang, Praksis, Se... |
| hivolda | sonnet | 23 | Section extraction is poor for hivolda. The plan's fixed boilerplate blocks (metadata header, Sensorordning, Evaluering, Maksimumstal, Emneansvarleg, Vurderingsform table, Godkjent av) are swallowed by neighbouring sections, and the 'Generell kompetanse' learning-outcome group is regularly split ... |
| hvl |  | 28 | HVL's h3-based extraction is structurally sound — the seven headings are consistently mapped to the right canonical sections, and no sections are mislabeled in a gross sense. The dominant quality problem is that the 'Hjelpemidler ved eksamen' sub-section is universally merged into the assessment ... |
| inn | sonnet | 20 | Section splitting for inn is mostly good: course_content, teaching_methods, coursework_requirements and prerequisites land in the right sections. Two systematic defects remain. On English-language plans, learning_outcomes is reduced to the one-line intro sentence (the Knowledge/Skills/General com... |
| mf | sonnet | 20 | MF section extraction is systematically wrong: obligatory activities are filed under assessment, course_content is only a heading stub while the real description is dropped, reading_list never contains a reading list (only library boilerplate plus the course-responsible person's name/email), and ... |
| nih | sonnet | 28 | Section extraction works very well for NIH: headings (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur) map cleanly, arbeidskrav are correctly kept out of assessment, and learning outcomes are complete. The only real issues are that rea... |
| nla | sonnet | 28 | Section extraction works well for NLA: course_content, learning_outcomes, teaching_methods, coursework_requirements and assessment are cleanly and completely split, with arbeidskrav correctly kept out of assessment. Remaining problems are a dropped teaching_methods section on nynorsk plans, a boi... |
| nmbu | sonnet | 10 | Core sections (course_content, learning_outcomes, teaching_methods, reading_list, assessment body) are extracted cleanly and completely on NMBU, including full Kunnskap/Ferdigheter/Generell kompetanse groups. Two systematic defects: the 'Obligatorisk aktivitet' block is never extracted (no course... |
| nord | sonnet | 28 | Core sections (course_content, learning_outcomes, teaching_methods) are extracted cleanly and completely for Nord, and the 'boilerplate', 'short' and most 'leak' flags are false alarms or symptoms of one real problem. The real problem is that coursework_requirements is never emitted: arbeidskrav ... |
| ntnu |  | 28 | NTNU's h3-heading-based extractor correctly identifies and labels the five main sections present (course_content, learning_outcomes, teaching_methods, coursework_requirements, prerequisites), and section boundaries are accurate. The two critical defects are (1) the assessment section universally ... |
| oslomet | sonnet | 28 | Ordinary OsloMet subject courses (random controls) split well: all five-six canonical sections are present and correct, including arbeidskrav correctly kept out of assessment and Kunnskap/Ferdigheter/Generell kompetanse kept inside learning_outcomes. All 20 suspects are the Praksis (M1GP/M5GP) co... |
| steiner | sonnet | 14 | Section boundaries are mostly right (arbeidskrav correctly in coursework_requirements, exam text in assessment, no leakage of the flagged kind). Main defects: the course metadata table is prepended to learning_outcomes, some LO bullets and whole deleksamen lines are dropped, and Innhold sub-theme... |
| uia | sonnet | 28 | Section extraction for UiA works well: headings map cleanly to the codebook sections, learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, and Vilkår for å gå opp til eksamen lands in coursework_requirements rather than assessment. Almost all pre-pass flags (boilerplate, em... |
| uib |  | 28 | UiB's details/summary accordion + h2 hybrid extraction correctly opens accordion sections and maps headings to canonical sections for the vast majority of courses. Core academic sections (course_content, learning_outcomes, teaching_methods, coursework_requirements, assessment) are extracted with ... |
| uio | sonnet | 13 | Core sections (course_content, learning_outcomes, prerequisites, teaching_methods, assessment) are split cleanly at UiO's fixed headings, but coursework_requirements and reading_list are never emitted, and several generic boilerplate blocks plus un-anonymized contact details contaminate the secti... |
| uis |  | 28 | Extraction quality for UiS is poor-to-moderate, with two dominant structural failures affecting the majority of courses. First, a systematic page-break split in older PDF-sourced plans causes the learning_outcomes section to be truncated at the first page-boundary keyword (typically 'Kunnskap'), ... |
| uit |  | 28 | UiT extraction quality is severely degraded by a single structural failure: the heading-splitter treats 'Hva lærer du' (the sub-heading that introduces the learning outcomes section) as a continuation of 'Innhold', then fails to cut at 'Undervisnings- og eksamensspråk' / 'Undervisning' / 'Kvalite... |
| usn | sonnet | 28 | Two distinct output regimes: the 8 random controls (LR/LH praksis, MG1/MG2 subject courses) split reasonably, but all 20 suspects (teacher-education PEL / Norwegian MG courses) are badly broken: learning_outcomes becomes a 10-30k char blob containing metadata, the reading list and the course-list... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | hivolda | reading_list | wrong_content | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL5-10NO2A-... | Add patterns mapping 'Om emnet' to course_content and 'Forkunnskapskrav...' to prerequisites in R/section_heading_map.R. Treat 'Pensum:' ... |
| ✓ | hivolda | cross_section | merged_sections | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7KRLE-1... | In R/section_heading_map.R, map 'Kunnskap(ar)', 'Ferdigheit(er)' and 'Generell kompetanse' to learning_outcomes (sub-headings, never boun... |
| ✓ | hivolda | coursework_requirements | merged_sections | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | In R/extract_sections.R (.clean_sections) or the heading map, treat 'Sensorordning', 'Evaluering og kvalitetssikring', 'Minimumstal', 'Ma... |
| ✓ | hivolda | assessment | missing_section | high | widespread | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | Add a hivolda rule starting a new assessment section at a line beginning with 'Vurderingsform Gruppering'. Also split out the arbeidskrav... |
| ? | hvl | assessment | merged_sections | high | widespread | hvl_LUPEKI203-1_2025_autumn_1; hvl_MGUMA101-1_2025_autumn... | Add 'Hjelpemidler ved eksamen' / 'Hjelpemiddel ved eksamen' to the heading map as a sentinel that closes the assessment section without o... |
| ✓ | inn | learning_outcomes | truncated | high | widespread | inn_2ML351-1_2024_autumn_1; inn_2ENL512-1-1_2023_spring_1... | In R/section_heading_map.R / R/extract_sections.R, treat Knowledge, Skills, General competence (and Norwegian equivalents) as sub-heading... |
| ✓ | mf | coursework_requirements | wrong_content | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | Add a pattern '^Obligatoriske aktiviteter' (and 'Listen over obligatoriske aktiviteter') mapped to coursework_requirements in R/section_h... |
| ✓ | mf | course_content | missing_section | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | In R/extract_sections.R add a preamble rule for MF: text between the 'Timeplan' line (end of the Emneinfo/Studieprogramtilhørighet block)... |
| ✓ | mf | reading_list | boilerplate_only | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_PED5610-1_2025_autumn_1; ... | Strip the fixed 'Tilgang til litteratur' paragraph and the stub line 'Litteraturlisten for ...' in .clean_sections(); emit no reading_lis... |
| ✓ | mf | reading_list | formatting_noise | high | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1014-1_2025_autumn_1; mf_... | In .clean_sections() (or an MF hook) truncate every section at the first line matching '^Emneansvarlig$' and drop everything after it. |
| ✓ | nmbu | prerequisites | formatting_noise | high | widespread | nmbu_PPRA301-1_2025_autumn_1; nmbu_MATH100-1_2025_spring_... | In R/extract_sections.R (.clean_sections or an nmbu pre-step) cut the plan at the first line matching '^Studieår:' and delete lines match... |
| ✓ | nmbu | coursework_requirements | missing_section | high | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_PPPE301-1_2025_spring_... | Add a pattern '^obligatoriske? aktivitet(er)?$' mapped to coursework_requirements in R/section_heading_map.R, and verify it stops at the ... |
| ? | nord | coursework_requirements | missing_section | high | widespread | nord_PEL1001-1_2025_autumn_3; nord_REL1005-1_2021_autumn_... | Add a nord-specific post-split step in .clean_sections() (R/extract_sections.R). Move paragraphs or lines that start with 'Arbeidskrav', ... |
| ? | ntnu | assessment | wrong_content | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2023_autumn_... | Strip lines matching the pattern 'Ordinær/Utsatt eksamen - (Høst\|Vår\|Sommer) \d{4}' and subsequent logistics rows (Karakter, Dato, Tid, S... |
| ? | ntnu | assessment | formatting_noise | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4621-1_2019_autumn_... | Add a post-processing step that drops any text block matching the regex 'function\s+\w+\s*\(' or 'const\s+\w+\s*=' from the assessment se... |
| ? | ntnu | teaching_methods | merged_sections | high | widespread | ntnu_PPU4621-1_2018_autumn_1; ntnu_PPU4623-1_2020_autumn_... | Add a split rule: within the teaching_methods text block, identify the paragraph starting with 'NTNU er tillagt et sertifiseringsansvar' ... |
| ? | oslomet | all | empty_placeholder | high | widespread | oslomet_M1GP3000-1_2023_autumn_3; oslomet_M5GP4200-1_2025... | In extract_sections for oslomet, detect sections whose text is just 'Se fagplanen.'/'Se under ... i fagplanen.' and drop them; for Praksi... |
| ? | oslomet | assessment | missing_section | high | widespread | oslomet_M1GP3000-1_2023_autumn_3; oslomet_M5GP3000-1_2023... | Add heading patterns for 'Organisering og arbeidsmåter' (teaching_methods) and 'Vurdering' (assessment) and ensure the Fagplan body is in... |
| ? | uib | course_content | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Strip nodes matching the semester-picker before text extraction: remove any element whose text matches the pattern /^Vel emnebeskrivelse ... |
| ? | uib | assessment | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Add a post_fn that strips content from 'Vurderingsordning' onwards (inclusive) and strips the footer lines 'Dette bør du vite om eksamen'... |
| ? | uis | learning_outcomes | truncated | high | widespread | uis_LPRA40-1_2017_autumn_1; uis_LPRA50-1_2015_autumn_1; u... | Strip 'side N' page-number lines and 'EMNE .* Versjon .*' running-header lines from the extracted text before section splitting, so the K... |
| ? | uis | reading_list | wrong_content | high | widespread | uis_LPRA40-1_2017_autumn_1; uis_LPRA50-1_2016_autumn_1; u... | Same fix as learning_outcomes truncation: strip page-number/running-header noise. Additionally, add a guard so that text that matches the... |
| ? | uis | assessment | merged_sections | high | widespread | uis_MGL3122-1_2021_autumn_2; uis_MGL1051-1_2022_autumn_1;... | Add 'Vilkår for å gå opp til eksamen/vurdering' and 'Obligatoriske krav' as explicit section-boundary headings mapped to coursework_requi... |
| ? | uit | learning_outcomes | merged_sections | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisnings- og eksamensspråk', 'Undervisning', 'Kvalitetssikring', 'Kvalitetssikring av emnet', and 'Eksamen' as closing-boundary... |
| ? | uit | assessment | wrong_content | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Assign the 'Eksamen' heading to assessment as its opening boundary (extracting everything from 'Vurderingsform:…' through 'Kontinuasjonse... |
| ? | uit | cross_section | missing_section | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisning' as a heading pattern mapped to teaching_methods in the heading map. This is a prerequisite change alongside fixing the... |
| ✓ | usn | learning_outcomes | merged_sections | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE3-1_2021_spring_2; u... | For USN, drop the Innholdsfortegnelse block and the front-matter (Emnekode...Ansvarlig) before splitting, and add/prioritize heading patt... |
| ✓ | usn | reading_list | boilerplate_only | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE2-1_2023_spring_1; u... | Remove the 'Emnet inngår i følgende studier' mapping to reading_list (map it to an ignored/drop section), and ensure 'Litteratur' (and 'O... |
| ✓ | usn | assessment | wrong_content | high | widespread | usn_MG1PE2-1_2020_spring_2; usn_MG2PE1-2_2021_autumn_2; u... | Fix the splitting order (see blob finding), make 'Generell kompetanse' a sub-heading that stays in learning_outcomes, strip 'Godkjent emn... |
| ✓ | hivolda | teaching_methods | missing_section | high | common | hivolda_MGL5-10NO2A-1_2024_spring_1; hivolda_MGL1-7SP1-1_... | Add 'Praktisk organisering( og arbeidsmåtar)?' to the teaching_methods patterns in R/section_heading_map.R. |
| ✓ | mf | learning_outcomes | truncated | high | common | mf_PPU1015-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1; m... | Anchor sub-heading patterns in R/section_heading_map.R to whole lines ('^(KUNNSKAP\|FERDIGHETER\|GENERELL KOMPETANSE)$', case-insensitive) ... |
| ✓ | uio | coursework_requirements | missing_section | high | common | uio_HIS1200L-1_2025_spring_1; uio_HIS4015L-1_2025_spring_... | Add heading patterns (Obligatoriske aktiviteter/komponenter/forhold i undervisningen, Faglige krav for å kunne avlegge eksamen, Arbeidskr... |
| ? | uis | course_content | merged_sections | high | common | uis_MGL3122-1_2021_autumn_2; uis_MGL1044-1_2025_autumn_1;... | Ensure 'Litteratur', 'Pensum', and 'Lesestoff' are registered as section-boundary headings for the reading_list section in the UiS headin... |
| ? | uis | cross_section | merged_sections | high | common | uis_MGL1044-1_2025_autumn_1; uis_MGL1110-1_2025_autumn_1;... | Audit the UiS CSS selector configuration and heading-recognition pattern against the newer web template. Add 'Læringsutbytte', 'Forkunnsk... |
| ? | uit | coursework_requirements | merged_sections | high | common | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Add 'Mer info om vurderingsform' as a secondary split point that closes coursework_requirements and opens assessment. Alternatively, stri... |
| ? | uit | assessment | merged_sections | high | occasional | uit_LRU-2642-1_2014_spring_1; uit_PFF-3102-1_2014_autumn_... | Detect the pattern 'Følgende arbeidskrav må være godkjent før man kan fremstille seg for eksamen:' (the older phrasing) as a secondary tr... |
| ? | uit | course_content | merged_sections | high | occasional | uit_LER-1353-1_2023_spring_1; uit_LRU-2642-1_2014_spring_... | Map 'Hva lærer du' and 'Etter bestått emne skal studentene ha følgende læringsresultat' (older phrasing) as opening boundaries for learni... |
| ✓ | mf | learning_outcomes | wrong_content | high | rare | mf_PRA1005-1_2025_autumn_1 | Same as truncation fix: anchor sub-heading patterns to whole lines and ensure 'Læringsutbytte' always starts learning_outcomes. |
| ✓ | mf | reading_list | wrong_content | high | rare | mf_PPU1015-1_2025_autumn_1 | Anchor the reading_list pattern to a full-line heading ('^Litteraturliste$'); never match bullet lines. |
| ✓ | uio | teaching_methods | other | high | rare | uio_MAT5930L-2_2025_spring_1; uio_PROF3025-1_2025_spring_1 | Run extract_sections on the anonymized course_plan (or apply anonymize_fulltext to each section) and add a check in R/audit/qa_sections.R... |
| ? | hvl | assessment | boilerplate_only | medium | widespread | hvl_MGPRA10-1_2025_autumn_1; hvl_MGPRA15-1_2025_autumn_1;... | In the post_fn for assessment, strip trailing content matching the pattern /\n-\s*\nMer om hjelpemidler.*$/i (and Norwegian variants 'Mei... |
| ✓ | inn | assessment | field_or_language_junk | medium | widespread | inn_2ML351-1_2024_autumn_1; inn_2ENL51-7-1_2024_autumn_1;... | Add a pattern for 'Language of instruction( and examination)?' / 'Undervisnings- og eksamensspråk' mapped to a dropped/ignored section an... |
| ✓ | inn | assessment | truncated | medium | widespread | inn_2MKRLE171-1-1_2023_spring_1; inn_2ENL51-8-1_2023_autu... | Map 'Form of assessment' (and 'Examination') to assessment in R/section_heading_map.R so the repeated heading continues the same section ... |
| ✓ | mf | course_content | empty_placeholder | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_PRA1001-1_2025_autumn_1; ... | Remap the heading to coursework_requirements and drop heading-only rows in .clean_sections() in R/extract_sections.R (text remaining afte... |
| ✓ | mf | learning_outcomes | formatting_noise | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf_... | Add 'Overlappende emner' as a discard/boundary heading in R/section_heading_map.R; remove the 'Litteraturliste' stub line. |
| ✓ | mf | assessment | formatting_noise | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1014-1_2025_autumn_1; m... | Cut assessment at 'Eksamensdatoer' (or drop lines from 'Eksamensdato:'/'Oppgaven utleveres:' to 'Sensur kunngjøres innen:' plus the follo... |
| ✓ | nih | reading_list | boilerplate_only | medium | widespread | nih_LKI120-1_2021_autumn_2; nih_LKI110-1_2023_autumn_1; n... | In .clean_sections() (R/extract_sections.R), drop reading_list rows that match ^(se emnearkivet\|pensum(lista\|liste) for (høsten\|våren) \d... |
| ✓ | nla | reading_list | boilerplate_only | medium | widespread | nla_4MGL5MA103-1_2025_spring_1; nla_MGL1NO201-1_2025_autu... | Either capture the href of the 'her' link into reading_list, or drop the row in .clean_sections() when the text matches '^Litteratur og f... |
| ? | ntnu | reading_list | missing_section | medium | widespread | ntnu_PPU4729-1_2016_autumn_1; ntnu_HIST3485-1_2019_spring... | Add 'Kursmateriell' to the heading-to-section map, mapped to reading_list. Also consider 'Faglig innhold: Kursmateriell' if the heading a... |
| ? | uib | prerequisites | merged_sections | medium | widespread | uib_HIS302L-0_2025_autumn_1; uib_HIS303L-0_2025_autumn_1;... | Map 'Krav til studierett' to no canonical section (drop it). Add a post_fn for prerequisites that removes lines consisting solely of 'Ing... |
| ✓ | uio | assessment | boilerplate_only | medium | widespread | uio_PROMO8-1_2025_autumn_4; uio_PROMO4-1_2025_autumn_4; u... | In .clean_sections() (or .anon_uio()), cut everything from the line 'Mer om eksamen ved UiO' to the end, and optionally drop the standalo... |
| ✓ | uio | prerequisites | boilerplate_only | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_NOR1000-1_2025_spring_1... | Strip the two standard Studentweb/studieprogrammer sentences in .clean_sections() for uio; consider splitting 'Opptak til emnet' from 'Ob... |
| ? | uis | assessment | formatting_noise | medium | widespread | uis_LPRA40-1_2019_autumn_1; uis_LPRA50-1_2018_autumn_1; u... | Add 'Åpen for' and 'Emneevaluering' as stop-headings for the assessment section in PDF-sourced plans. Strip runs of whitespace from PDF t... |
| ? | uit | prerequisites | wrong_content | medium | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Post-process the prerequisites field to strip sentences matching the pattern 'Når eksamen i … er bestått, kan studenten framstille seg ti... |
| ✓ | hiof | reading_list | boilerplate_only | medium | common | hiof_LMBNOR10417-1_2025_autumn_1; hiof_LMUMAT40117-1_2025... | In .clean_sections() (R/extract_sections.R), drop reading_list rows that match /Leganto/ and are shorter than ~150 chars (or set them to ... |
| ✓ | hivolda | assessment | wrong_content | medium | common | hivolda_MGL5-10EN3C-1_2021_spring_2; hivolda_MGL5-10EN1A-... | Keep the whole 'Vilkår for å framstille seg til eksamen' block together as coursework_requirements up to 'Sensorordning'. Fix the EN3C he... |
| ? | hvl | prerequisites | merged_sections | medium | common | hvl_MGUNA501-1_2025_spring_1; hvl_MGBNO201-1_2025_spring_... | Map 'Anbefalte forkunnskaper' / 'Tilrådde forkunnskapar' to the same prerequisites section but prefix the captured text with a marker (e.... |
| ✓ | mf | teaching_methods | missing_section | medium | common | mf_RL1014-1_2025_autumn_1; mf_PRA1005-1_2025_autumn_1; mf... | Add '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R, ending the section at 'Om studiet'/'Oblig... |
| ✓ | nmbu | prerequisites | boilerplate_only | medium | common | nmbu_PPXP100-1_2025_autumn_1 | Do not map 'Opptakskrav' to prerequisites, and suppress the prerequisites row when nothing remains after removing metadata/CSS. |
| ? | nord | assessment | formatting_noise | medium | common | nord_NAT2004-1_2020_spring_2; nord_MUS2003-1_2019_autumn_... | In .clean_sections() or a Nord handler, drop paragraphs that match 'Covid-19', 'Midlertidig forskrift', 'koronaepidemien', 'Å generere be... |
| ? | nord | prerequisites | boilerplate_only | medium | common | nord_KHV1004-1_2021_autumn_1; nord_MAT1007-1_2024_autumn_... | Filter prerequisites lines matching 'generell studiekompetanse', 'Kan tas som frittstående', 'forbeholdt', 'Opptak til studieprogrammet',... |
| ? | ntnu | prerequisites | boilerplate_only | medium | common | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2018_autumn_... | Add a post-processing filter: if the prerequisites text exactly matches the known NTNU boilerplate strings ('Beståtte eksamener i henhold... |
| ✓ | steiner | learning_outcomes | formatting_noise | medium | common | steiner_M-PEL1_2_2025_spring_1; steiner_M-PEL1_3_2025_spr... | In .clean_sections()/steiner strategy, drop the preamble before the first heading (or strip lines beginning 'Emnekode og ', 'Emnenavn', '... |
| ✓ | steiner | learning_outcomes | truncated | medium | common | steiner_M-PEL1_2_2025_spring_1; steiner_M-PEL1_1_2025_spr... | Restrict sub-heading stripping to lines that exactly equal 'Kunnskap', 'Ferdigheter', 'Generell kompetanse' (anchored ^...$), and do not ... |
| ✓ | steiner | teaching_methods | wrong_content | medium | common | steiner_M-PEL1_3_2025_spring_1; steiner_M-MAT1_1_2025_spr... | In R/section_heading_map.R only match 'Arbeidsmåter' / 'Undervisnings- og arbeidsformer' as teaching_methods when the line is the whole h... |
| ? | uis | reading_list | empty_placeholder | medium | common | uis_MGL3122-1_2021_autumn_2; uis_LPRA20-1_2015_autumn_2; ... | Fix the Litteratur heading-boundary issue (see course_content merged_sections finding). Post-extraction: suppress reading_list rows whose... |
| ? | uis | prerequisites | merged_sections | medium | common | uis_MGL1110-1_2025_autumn_1; uis_LPRA40-1_2019_autumn_1 | For the HTML template issue: fix the nested section boundary detection so 'Vilkår for å gå opp til eksamen/vurdering' fires before the Fo... |
| ? | uis | teaching_methods | missing_section | medium | common | uis_MGL3122-1_2021_autumn_2; uis_MGL1051-1_2022_autumn_1;... | Add 'Arbeidsformer' and 'Arbeids- og undervisningsformer' as headings mapped to teaching_methods in the UiS heading map. Ensure the selec... |
| ✓ | hivolda | course_content | wrong_content | medium | occasional | hivolda_MGL5-10SA2B-1_2023_autumn_1; hivolda_MGL5-10SA2B-... | Map 'Studentane skal ha/kunne...' and the 'Kunnskapar' sub-heading to learning_outcomes. Review the SA2B heading variants and map them to... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Extend the prerequisites pattern to '^Forkunnskap(er\|skrav)' and allow 'Label: text' on one line. |
| ✓ | nla | teaching_methods | missing_section | medium | occasional | nla_MGL1NO201-1_2025_autumn_1; nla_4MGL1NO201-1_2025_autu... | Make the teaching-methods heading pattern tolerant: 'arbeids-? og undervis(n)?ingsformer' (optionally also 'undervisning'), mapped to tea... |
| ? | ntnu | assessment | missing_section | medium | occasional | ntnu_PPU4700-1_2012_autumn_1; ntnu_PPU4611-1_2018_autumn_1 | When no assessment row is extracted but the course metadata contains a non-empty 'Vurderingsordning' field, emit a diagnostic flag so the... |
| ✓ | steiner | assessment | truncated | medium | occasional | steiner_M-NOR1_3_2025_spring_1; steiner_M-MAT1_2_2025_spr... | When removing the 'Eksamensform/Hjelpemidler/Sensorordning/Vurderingsuttrykk' labels, remove only exact label lines; keep any line carryi... |
| ? | uib | assessment | wrong_content | medium | occasional | uib_SOS340-L-0_2025_spring_1 | Add a post_fn strip for lines matching 'Vi opplever problemer med å hente inn eksamensinformasjon' and 'Lukk'. Alternatively, identify th... |
| ? | uis | coursework_requirements | wrong_content | medium | occasional | uis_LPRA50-1_2015_autumn_1; uis_LPRA50-1_2016_autumn_1 | Ensure the coursework_requirements section captures all content from 'Vilkår for å gå opp til eksamen/vurdering' through the next major h... |
| ? | uis | learning_outcomes | wrong_content | medium | occasional | uis_MGL1322-1_2025_autumn_1; uis_MGL1110-1_2025_autumn_1 | Same fix as the HTML cross_section merged_sections finding: register 'Læringsutbytte' as a hard section boundary in the newer template. V... |
| ? | uit | learning_outcomes | truncated | medium | occasional | uit_LER-3903-1_2024_autumn_1; uit_LER-3913-1_2024_autumn_... | Investigate and raise (or remove) any field-length cap applied to sections_raw text. Once the merge is fixed the correct learning_outcome... |
| ✓ | uio | reading_list | missing_section | medium | rare | uio_HIS4015L-1_2025_spring_1 | Add 'Pensum' to reading_list patterns in R/section_heading_map.R and allow the UiO splitter to cut at it when it occurs within Undervisni... |
| ? | hvl | coursework_requirements | boilerplate_only | low | widespread | hvl_MGUMA101-1_2025_autumn_1; hvl_LUPEKI101-1_2025_autumn... | Add a post_fn for coursework_requirements that strips text matching the recurring boilerplate pattern starting with 'De obligatoriske lær... |
| ? | hvl | reading_list | missing_section | low | widespread | hvl_MGUMA201-1_2025_autumn_1; hvl_MGBNO201-1_2025_spring_... | Document in institution_config.R that reading_list is not available from HVL's emneplan pages. No code change needed, but consider adding... |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_2MPRA171S-4-1_2022_spring_1; inn_2MPRA171-1-1_2022_sp... | In .clean_sections() strip the fixed header strings 'Vurderingsordning Karakterskala Gruppe/individuell Varighet Hjelpemidler Andel Komme... |
| ✓ | inn | reading_list | empty_placeholder | low | widespread | inn_2ML351-1_2024_autumn_1; inn_2MPRA171-1-1_2022_spring_1 | In .clean_sections() drop reading_list rows matching '^(No reading list available\|Ingen pensumliste tilgjengelig)' and prerequisites matc... |
| ✓ | nla | prerequisites | empty_placeholder | low | widespread | nla_4MGL5MA103-1_2025_spring_1; nla_4MGL5KRLE101-1_2025_a... | Treat prerequisites matching '^Se programplan\.?$' as empty and do not emit a row in .clean_sections(). |
| ✓ | nmbu | assessment | formatting_noise | low | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_FYS100-1_2025_spring_1... | In .clean_sections() (or an nmbu-specific post_fn) remove lines matching 'Karakterregel:.*' from assessment and collapse runs of blank li... |
| ? | oslomet | cross_section | duplicate | low | widespread | oslomet_M1GP4100-1_2025_spring_4; oslomet_M5GP3200-1_2024... | Filter stub-only sections in .clean_sections() (drop text matching ^Se (under .*)?fagplanen\.?$) before emitting rows. |
| ✓ | steiner | assessment | formatting_noise | low | widespread | steiner_M-PEL1_1_2025_spring_1; steiner_M-NAT1_2_2025_spr... | Either strip all five exam sub-headings consistently (add 'Sensorordning'), or better keep all labels; optionally drop Sensorordning/Hjel... |
| ? | uib | cross_section | duplicate | low | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | No change needed to the extractor. Note this duplication in data documentation so downstream consumers are not confused by the repeated c... |
| ? | uit | course_content | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Strip the literal string 'Hva lærer du' (and its variants) from the tail of the course_content field during post-processing (post_fn), or... |
| ? | uit | coursework_requirements | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3902-1_2024_autumn_... | Add a post_fn (or pre_fn cleaning step) that strips the literal strings 'UiTs samleside om eksamen', 'Mer info om arbeidskrav', and 'Mer ... |
| ? | hvl | coursework_requirements | wrong_content | low | common | hvl_MGUMA101-1_2025_autumn_1; hvl_MGUMA102-1_2025_autumn_... | This is an authoring issue in the source; no clean programmatic fix is possible without risking dropping real arbeidskrav. The downstream... |
| ? | hvl | all | empty_placeholder | low | common | hvl_MGPRA10-1_2025_autumn_1; hvl_MGPRA5-1_2025_autumn_1; ... | In the post_fn (or a shared normaliser), convert extracted text that is exactly '-' (after trimming whitespace) to NA/NULL, so empty-plac... |
| ✓ | nih | prerequisites | empty_placeholder | low | common | nih_LKI110-1_2025_autumn_1; nih_LKI226-1_2025_autumn_1; n... | Map "Hvem kan ta dette emnet?" to an ignored/admin bucket in R/section_heading_map.R, and have .clean_sections() drop prerequisites rows ... |
| ? | ntnu | teaching_methods | boilerplate_only | low | common | ntnu_PPU4623-1_2021_autumn_1; ntnu_PPU4625-1_2021_autumn_... | Add a post-processing blocklist for the NTNU certification-responsibility paragraph: match on 'NTNU er tillagt et sertifiseringsansvar' a... |
| ✓ | steiner | cross_section | formatting_noise | low | common | steiner_M-NOR1_1_2025_spring_1; steiner_M-NAT1_2_2025_spr... | In .clean_sections() remove lines matching ^\s*\d{1,3}\s*$ before splitting and for all sections. |
| ? | uia | coursework_requirements | merged_sections | low | common | uia_PRA1-301-1_2025_autumn_1; uia_PRA2-302-1_2024_spring_1 | Optionally add a UiA sentence-level post-step moving sentences matching 'krav om .* (tilstedeværelse\|deltagelse)' to coursework_requireme... |
| ? | uib | prerequisites | empty_placeholder | low | common | uib_RELV302L-0_2025_autumn_1; uib_HIS301L-0_2025_autumn_1... | Collapse multiple 'Ingen'/'\-' values in prerequisites to a single token, or to NULL/empty if all constituent fields are null placeholders. |
| ? | uib | assessment | wrong_content | low | common | uib_NOLI103-L-0_2025_autumn_1; uib_NORAN204-L-0_2025_autu... | Add this exact boilerplate sentence to the list of patterns stripped in the assessment post_fn (e.g. regex: /Klokkeslett for oppstart av ... |
| ? | uib | assessment | wrong_content | low | common | uib_HIDID112-0_2025_autumn_1; uib_HIS302L-0_2025_autumn_1... | Identify the 'Hjelpemiddel til eksamen' sub-section within the assessment panel and drop it, or add its null-value strings ('Ingen', '-')... |
| ? | uis | course_content | formatting_noise | low | common | uis_LPRA40-1_2019_autumn_1; uis_LPRA50-1_2018_autumn_1; u... | Add a pre-processing step for UiS PDF sources that removes lines matching 'side \d+' and 'EMNE [A-Z0-9_]+ BOKMÅL Versjon \d{2}\.\w+\.\d{4... |
| ✓ | usn | coursework_requirements | formatting_noise | low | common | usn_MG2PE2-1_2023_spring_1 | In .clean_sections() remove a leading line identical to the section heading and collapse consecutive duplicate lines. |
| ? | hvl | course_content | boilerplate_only | low | occasional | hvl_LUPEMP100-1_2025_autumn_1; hvl_LUPEMP200-1_2025_autumn_1 | Add a pre_fn or post_fn for course_content that strips lines matching 'Emnet vert ikkje tilbydd' (and Bokmål variant 'Emnet tilbys ikke'). |
| ? | nord | assessment | merged_sections | low | occasional | nord_REL1005-1_2021_autumn_1; nord_KHV1004-1_2021_autumn_1 | Map Nord's pensum/litteratur and 'Andre krav' headings to reading_list or an explicit drop in R/section_heading_map.R. Strip a trailing l... |
| ? | ntnu | learning_outcomes | truncated | low | occasional | ntnu_PPU4623-1_2019_autumn_1; ntnu_PPU4625-1_2019_autumn_... | Increase or remove the character limit on the sections_raw text field. The truncation point is consistent across affected courses suggest... |
| ? | oslomet | prerequisites | empty_placeholder | low | occasional | oslomet_M5GNA3100-1_2019_autumn_2; oslomet_M1GNO3100-1_20... | Drop sections that consist only of a 'Se ...' cross-reference; strip such lines from otherwise real sections. |
| ? | oslomet | learning_outcomes | formatting_noise | low | occasional | oslomet_M1GMU3100-1_2020_autumn_1; oslomet_M1GMU2200-1_20... | Normalise 'a¿' -> 'å' (and similar) in text cleanup and convert inline-element boundaries to spaces instead of ';'. |
| ✓ | steiner | coursework_requirements | formatting_noise | low | occasional | steiner_M-NOR1_3_2025_spring_1 | Only strip the exact section heading line 'Arbeidskrav' (anchored) and keep sub-titles. |
| ? | uib | course_content | wrong_content | low | occasional | uib_SPLA106-0_2025_autumn_1; uib_NOLISP300-L-0_2025_autum... | No change required; this is correctly handled. Document in data notes that SPLA106-type short courses may have thin course_content becaus... |
| ? | uib | learning_outcomes | formatting_noise | low | occasional | uib_RELV107-0_2025_autumn_2 | Add a learning_outcomes post_fn that collapses paragraph breaks that split a sentence (i.e., where a paragraph ends mid-word or without t... |
| ? | uis | learning_outcomes | wrong_content | low | occasional | uis_LFYBAC-1_2022_autumn_1; uis_LMAMAS-1_2016_autumn_2 | Add a pattern-based detection for headingless prerequisite sentences (e.g., text matching 'Studenten må ha fullført minst .* stp' or 'God... |
| ? | uit | prerequisites | wrong_content | low | occasional | uit_LER-2152-1_2025_spring_1 | Strip paragraphs matching 'Studiepoengreduksjon' (and the credit-reduction boilerplate that follows) in the prerequisites post_fn, or add... |
| ? | uit | teaching_methods | wrong_content | low | occasional | uit_LRU-3300-1_2016_autumn_1; uit_LRU-2642-1_2014_spring_... | Add patterns for 'For nærmere informasjon om praksis, se egen praksisplan' and 'Emnet evalueres muntlig eller skriftlig minimum en gang h... |
| ✓ | usn | teaching_methods | formatting_noise | low | occasional | usn_LR-PRA3000-1_2021_autumn_1; usn_LH-PRA4000-1_2021_aut... | Add a pattern for '^Obligatorisk aktivitet( og krav til tilstedeværelse)?$' mapped to coursework_requirements in R/section_heading_map.R ... |
| ✓ | hiof | prerequisites | empty_placeholder | low | rare | hiof_LMUMUS11423-1_2025_spring_2 | In .clean_sections(), remove lines that are exactly 'Ingen'/'Ingen.'/'-' from prerequisites text, and drop the row if nothing remains. |
| ✓ | hiof | learning_outcomes | formatting_noise | low | rare | hiof_LMUMUS11423-1_2025_spring_2; hiof_LMUMUS10323-1_2025... | Insert a newline or space between an adjacent heading-like word (Kunnskap/Ferdigheter/Generell kompetanse) and 'Studenten'/'Kandidaten' i... |
| ? | hvl | course_content | boilerplate_only | low | rare | hvl_LUPEKI101-1_2025_autumn_1 | Add a pattern to the post_fn for course_content that strips paragraphs starting with 'Retningsliner for godkjenning' / 'Retningslinjer fo... |
| ✓ | mf | learning_outcomes | wrong_content | low | rare | mf_RL1013-1_2025_autumn_1 | Do not map 'Delemne X:' headings outside the Læringsutbytte block to learning_outcomes; keep them as course_content and preserve document... |
| ✓ | nih | course_content | formatting_noise | low | rare | nih_LKI226-1_2025_autumn_1 | For nih, add a pre_fn in R/institution_config.R that inserts a newline for <br> and between adjacent block elements before text extraction. |
| ✓ | nla | assessment | formatting_noise | low | rare | nla_MGL1NO102-1_2025_autumn_1; nla_4MGL5KRLE101-1_2025_au... | In .clean_sections() remove lines consisting solely of 'Bokmål' or 'Nynorsk'. |
| ? | nord | prerequisites | empty_placeholder | low | rare | nord_REL1005-1_2021_autumn_1 | Treat text that is only dots, dashes, 'Ingen', 'Ingen.' or 'Ingen utover opptakskravet.' as empty and drop the row. |
| ? | uia | cross_section | formatting_noise | low | rare | uia_MA-172-1_2022_autumn_4 | In .clean_sections() in R/extract_sections.R, blank out lines matching ^\s+$ and collapse 3+ consecutive newlines to two. |
