# sections audit — aggregated findings

68 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: opus, sonnet).
Mechanical verification: 68 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 8 | hiof, hivolda, inn, ntnu, steiner, uib, uio, uit | medium |
| prerequisites | empty_placeholder | 4 | nih, nord, uib, uis | low |
| teaching_methods | wrong_content | 4 | nla, nord, ntnu, uio | medium |
| all | missing_section | 3 | oslomet, uia, uit | high |
| coursework_requirements | wrong_content | 3 | uio, uit, usn | high |
| prerequisites | missing_section | 3 | mf, nla, uit | medium |
| reading_list | formatting_noise | 3 | hiof, uis, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 5 | 2 | reading_list / formatting_noise | assessment / formatting_noise | assessment / other; learning_outcomes / other; teaching_methods / truncated; reading_list / empty_placeholder |
| hivolda | 2 | 1 | assessment / formatting_noise |  | assessment / wrong_content |
| hvl | 3 | 3 |  | reading_list / wrong_content; assessment / wrong_content; assessment / empty_placeholder | learning_outcomes / formatting_noise; assessment / formatting_noise; coursework_requirements / empty_placeholder |
| inn | 3 | 4 | assessment / formatting_noise; learning_outcomes / formatting_noise | learning_outcomes / truncated; teaching_methods / boilerplate_only | teaching_methods / merged_sections |
| mf | 4 | 4 | teaching_methods / merged_sections | prerequisites / missing_section; reading_list / missing_section; coursework_requirements / formatting_noise | reading_list / boilerplate_only; prerequisites / merged_sections; teaching_methods / missing_section |
| nih | 2 | 1 |  | prerequisites / empty_placeholder | reading_list / boilerplate_only; prerequisites / missing_section |
| nla | 3 | 2 | prerequisites / missing_section | teaching_methods / wrong_content | learning_outcomes / truncated; coursework_requirements / formatting_noise |
| nmbu | 2 | 1 | teaching_methods / missing_section |  | learning_outcomes / formatting_noise |
| nord | 10 | 6 | course_content / truncated; prerequisites / empty_placeholder; teaching_methods / wrong_content | cross_section / wrong_content; coursework_requirements / split_section; prerequisites / boilerplate_only | assessment / missing_section; cross_section / split_section; prerequisites / truncated; assessment / truncated; coursework_requirements / truncated; learning_outcomes / formatting_noise; all / other |
| ntnu | 8 | 3 | assessment / formatting_noise; teaching_methods / wrong_content | assessment / truncated | assessment / other; assessment / duplicate; teaching_methods / merged_sections; assessment / wrong_content; reading_list / empty_placeholder; prerequisites / boilerplate_only |
| oslomet | 5 | 7 | all / missing_section; teaching_methods / missing_section; course_content / formatting_noise | cross_section / formatting_noise; learning_outcomes / merged_sections; prerequisites / boilerplate_only; coursework_requirements / empty_placeholder | teaching_methods / wrong_content; assessment / formatting_noise |
| steiner | 1 | 2 |  | assessment / formatting_noise; all / other | cross_section / formatting_noise |
| uia | 4 | 4 | all / missing_section | teaching_methods / other; course_content / other; assessment / wrong_content | cross_section / merged_sections; coursework_requirements / wrong_content; learning_outcomes / formatting_noise |
| uib | 5 | 4 | assessment / formatting_noise | course_content / field_or_language_junk; cross_section / truncated; prerequisites / empty_placeholder | course_content / formatting_noise; assessment / wrong_content; assessment / truncated; cross_section / other |
| uio | 5 | 5 | coursework_requirements / wrong_content | assessment / formatting_noise; teaching_methods / wrong_content; coursework_requirements / merged_sections; learning_outcomes / duplicate | coursework_requirements / missing_section; reading_list / merged_sections; assessment / field_or_language_junk; assessment / truncated |
| uis | 7 | 5 | cross_section / formatting_noise | reading_list / formatting_noise; course_content / boilerplate_only; course_content / duplicate; prerequisites / empty_placeholder | cross_section / wrong_content; reading_list / split_section; cross_section / merged_sections; teaching_methods / truncated; course_content / truncated; assessment / formatting_noise |
| uit | 11 | 10 | assessment / merged_sections | all / missing_section; cross_section / merged_sections; course_content / wrong_content; assessment / formatting_noise; prerequisites / missing_section; coursework_requirements / wrong_content; assessment / duplicate; reading_list / boilerplate_only; teaching_methods / formatting_noise | learning_outcomes / merged_sections; assessment / wrong_content; coursework_requirements / merged_sections; prerequisites / wrong_content; course_content / formatting_noise; learning_outcomes / truncated; cross_section / missing_section; coursework_requirements / formatting_noise; course_content / merged_sections; teaching_methods / wrong_content |
| usn | 5 | 4 | reading_list / formatting_noise | coursework_requirements / formatting_noise; coursework_requirements / wrong_content; teaching_methods / merged_sections | cross_section / formatting_noise; learning_outcomes / other; assessment / formatting_noise; reading_list / empty_placeholder |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 12 |
| empty_placeholder | low | 6 |
| formatting_noise | medium | 6 |
| wrong_content | low | 6 |
| boilerplate_only | low | 5 |
| missing_section | medium | 5 |
| merged_sections | medium | 3 |
| wrong_content | high | 3 |
| wrong_content | medium | 3 |
| duplicate | low | 2 |
| merged_sections | high | 2 |
| missing_section | high | 2 |
| missing_section | low | 2 |
| other | low | 2 |
| truncated | medium | 2 |
| duplicate | medium | 1 |
| field_or_language_junk | medium | 1 |
| merged_sections | low | 1 |
| other | medium | 1 |
| split_section | medium | 1 |
| truncated | high | 1 |
| truncated | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 14 |
| teaching_methods | 11 |
| prerequisites | 9 |
| coursework_requirements | 8 |
| course_content | 7 |
| reading_list | 6 |
| cross_section | 5 |
| all | 4 |
| learning_outcomes | 4 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 16 | Section extraction for HiOF is accurate in all 16 courses, including both random controls. The h2 headings split cleanly. Kunnskap/Ferdigheter/Generell kompetanse groups are all kept in learning_outcomes, arbeidskrav always land in coursework_requirements, Praksis is in teaching_methods, and Sens... |
| hivolda | sonnet | 20 | Section extraction works well for hivolda: course_content, learning_outcomes (all three sub-groups kept), teaching_methods, coursework_requirements and prerequisites are correctly cut and clean in all 20 courses, including the 6 random controls. The only recurring defect is that assessment is a f... |
| hvl | sonnet | 19 | Section extraction works well for HVL: the standardised heading set (Innhald og oppbygging, Læringsutbytte, Krav til forkunnskapar, Undervisnings- og læringsformer, Obligatorisk læringsaktivitet, Vurderingsform) maps cleanly, learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse gro... |
| inn | sonnet | 20 | Section extraction for inn is largely correct: learning outcomes are complete, arbeidskrav/obligatoriske aktiviteter land in coursework_requirements, 'Ingen' prerequisites are dropped, and all 14 suspects (Praksisstudium courses, pass/fail) are false alarms (leak->* flags come from bullet topic l... |
| mf | sonnet | 10 | Extraction is accurate for MF: course_content, coursework_requirements, assessment and learning_outcomes are cleanly cut and complete in all 6 random controls, and sub-headings in learning outcomes do not truncate anything. Two real defects: an inline 'Arbeidsform og organisering' block is absorb... |
| nih | sonnet | 20 | Section extraction for NIH works very well: every heading (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen) maps to the right section, arbeidskrav stay out of assessment, plagiarism/WISEflow notices are removed and placeholder-only reading lists are cor... |
| nla | sonnet | 10 | Section extraction works well for NLA: the pages have clean, consistent headings and all seven-section content lands in the right place, with arbeidskrav correctly kept out of assessment and nested Kunnskap/Ferdigheter sub-headings kept inside learning_outcomes. The pre-pass flags on the four sus... |
| nmbu | sonnet | 8 | Section extraction works well for NMBU: all 8 plans have correct, complete and clean sections, with sub-headings kept inside learning outcomes and Obligatorisk aktivitet correctly separated from assessment. Both pre-pass flags are false alarms; the only loss is text under unmapped headings (Laeri... |
| nord | sonnet | 20 | Nord's uniform page structure is read well: headings map correctly, learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, and admission/cost/evaluation boilerplate is dropped. The main weakness is the assessment/coursework_requirements split inside the Eksamensbeskrivelse bl... |
| ntnu | sonnet | 19 | Section extraction works well for NTNU: headings are read and mapped correctly, learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, and all 14 suspects plus the 5 controls land in the right sections. The 'boilerplate' flags are false alarms (all 14 suspects are PPU Fagdida... |
| oslomet | sonnet | 17 | Where sections are extracted, the section split is good: learning outcomes, arbeidskrav and assessment land correctly and the leak-> flags are false alarms. The main defect is that the 4th-year/praksis course family (M5GP*) yields no sections at all although the headings were read and mapped. Sma... |
| steiner | sonnet | 13 | Section extraction works well for Steiner: learning outcomes (all three sub-groups), content, coursework requirements (Arbeidskrav) and assessment are cut correctly in all 13 courses, including the arbeidskrav-vs-assessment split. Only minor noise (page numbers, retained exam-logistics lines) and... |
| uia | sonnet | 20 | Section extraction works well for UiA pages that carry the standard Læringsutbytte/Innhold/Undervisnings- og læringsformer/Vilkår/Eksamen headings: sub-headings stay in learning_outcomes, arbeidskrav go to coursework_requirements and prerequisites are clean. Most pre-pass flags are false alarms. ... |
| uib | sonnet | 12 | Section extraction works well for UiB: the fixed heading set is mapped correctly, all seven sections land where the codebook puts them, and learning-outcome sub-groups and arbeidskrav are kept intact in the 11 normal plans. Remaining issues are a heading-only plan emitted as content, exam-semeste... |
| uio | sonnet | 11 | UiO extraction is largely sound: course_content, learning_outcomes, prerequisites and reading_list are correct and admission text is dropped. Recurring defects are the unmapped Eksamensspråk/Karakterskala sub-headings appended to assessment, and bold-label lines (not detected as headings) that le... |
| uis | sonnet | 16 | Section splitting is largely correct for UiS: learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, prerequisites are clean, and the 'Vilkår for å gå opp til eksamen' block is separated from assessment. Most pre-pass flags (empty/boilerplate teaching_methods, blob on the MGL... |
| uit | sonnet | 19 | Modern-format UiT pages (Om emnet / Hva lærer du / Undervisning og pensum / Eksamen, ~2013 onwards) are split well, but arbeidskrav handling and prerequisites are weak. Legacy FS pages (2008-2012, headings Innhold / Læringsutbytte / Undervisningsform / Eksamensform / Pensum) go through the text f... |
| usn | sonnet | 18 | USN section extraction is largely correct: all seven sections land in the right place for the 18 plans, including learning outcomes with all three sub-groups and arbeidskrav separated from assessment. Remaining defects are cosmetic: Leganto UI text in reading_list, and sub-heading lines left in c... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | uit | coursework_requirements | wrong_content | high | widespread | uit_LRU-1502-1_2012_autumn_2; uit_LRU-3640-1_2018_autumn_... | In the assessment block split off paragraphs starting 'Følgende arbeidskrav må være ...' (up to 'Eksamen består av'/'Eksamen og vurdering... |
| ✓ | nord | cross_section | wrong_content | high | common | nord_REL1004-1_2019_autumn_2; nord_MUS2015-1_2024_autumn_... | In the coursework splitter for Nord, treat a label paragraph (e.g. 'Obligatorisk deltakelse (OD)', 'Obligatorisk arbeid (OA):') together ... |
| ✓ | oslomet | all | missing_section | high | common | oslomet_M5GP4000-1_2025_autumn_2; oslomet_M5GP2000-1_2024... | Debug sectionize() for M5GP4000-1 interactively with tar_make(callr_function = NULL) and find which step empties the table. Then fix the ... |
| ✓ | uit | all | missing_section | high | common | uit_NOR-3930-1_2009_autumn_1; uit_RUS-3950-1_2008_autumn_... | Add a legacy line-label splitter for uit (text fallback keyed on line-start labels Innhold, Læringsutbytte, Mål, Undervisningsform, Eksam... |
| ✓ | uit | cross_section | merged_sections | high | common | uit_SOA-3905-1_2011_spring_1; uit_SOA-3905-1_2011_autumn_... | Map Undervisningsform -> teaching_methods, Undervisnings- og eksamensspråk -> .drop (or language), Vurderingsordning/Eksamensform -> asse... |
| ✓ | uit | assessment | merged_sections | high | common | uit_LRU-1400-1_2010_autumn_2; uit_LRU-1400-1_2011_spring_... | Same legacy splitter as above: cut at line-start 'Pensum' and map to reading_list; verify that 'Innhold' maps to course_content (not read... |
| ✓ | uit | course_content | wrong_content | high | occasional | uit_LRU-1400-1_2010_autumn_2; uit_LRU-1400-1_2011_spring_1 | Fix the legacy splitter so text between Innhold and Læringsutbytte goes to course_content; add test with LRU-1400. |
| ✓ | inn | learning_outcomes | truncated | high | rare | inn_2MNK172S-2-1_2022_spring_1 | Add 'Ferdighetsmål' (and nynorsk variants 'Ferdigheitsmål', 'Kunnskapsmål', 'Generell kompetanse') as sub-headings mapped to learning_out... |
| ✓ | mf | reading_list | missing_section | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1; mf_... | Check the MF page source for where the literature list is rendered and extend the MF fulltext/section_* selector to include it; otherwise... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_KJM5930L-2_2025_spring_1; uio_HIS1200L-1_2025_spring_... | Map `Eksamensspråk` and `Karakterskala` (and 'Hjelpemidler til eksamen', already .drop) to .drop in R/section_heading_map.R so their text... |
| ✓ | usn | reading_list | formatting_noise | medium | widespread | usn_MG1PE3-1_2021_spring_2; usn_MG2RL3-1_2019_spring_2; u... | Add a USN reading-list cleanup that strips the lines '^Click to view interactive reading list in Leganto' (and splits the glued section h... |
| ✓ | nord | course_content | truncated | medium | common | nord_MAT5006-1_2022_spring_2; nord_MAT5002-1_2023_autumn_1 | For nord, put non-empty preamble text between the title/code lines and the first heading into course_content (prepend), after removing th... |
| ✓ | ntnu | assessment | truncated | medium | common | ntnu_PPU4625-1_2023_spring_1; ntnu_PPU4681-1_2022_spring_1 | Check the block reader for NTNU Eksamen with multiple 'Vurderingsordning:' headings: emit the 'Vurderingsordning: X / Karakter: Y' lines ... |
| ✓ | oslomet | teaching_methods | missing_section | medium | common | oslomet_M5GSF2110-1_2022_spring_2; oslomet_M5GKP2100-1_20... | Add patterns (optionally prefixed 'Fagets ') to R/section_heading_map.R: '^Fagets arbeids- og undervisningsformer' -> teaching_methods, '... |
| ✓ | uia | all | missing_section | medium | common | uia_PED428-1_2023_autumn_1; uia_IDR418-1_2023_autumn_1; u... | Flag plans whose extracted text has no mapped content heading (only .drop/unmapped headings) as 'no_plan_content' in the fulltext/harvest... |
| ✓ | uio | coursework_requirements | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_PROF1005-1_2025_spring_1 | Add 'Sensurordning' and 'Sensurering' (with optional trailing colon) as sub-headings mapped to assessment in R/section_heading_map.R, pla... |
| ✓ | uio | teaching_methods | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_KJM5050-1_2025_spring_1 | Treat colon-terminated bold label lines under Undervisning as sub-headings in the block reader (R/blocks.R / section_* config for uio) an... |
| ✓ | uis | cross_section | formatting_noise | medium | common | uis_LENG116-1_2015_autumn_1; uis_LFYMAS-1_2017_spring_1; ... | For uis, strip lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$' and lines equal to '<emnekode> - <titl... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In the uis cleanup, remove tokens matching 'https?://bibsys-ur\.userservices\.exlibrisgroup\.com/\S*(?:\s*\n?\S*rft[._]\S*)*' up to 'View... |
| ✓ | uit | assessment | formatting_noise | medium | common | uit_SOA-3905-1_2011_spring_1; uit_LRU-1400-1_2010_autumn_... | In .clean_sections() (or as an anonymizer-independent text cleanup for uit) strip lines matching '^</?div' / 'fsemnetext', the 'Dato for ... |
| ✓ | uit | prerequisites | missing_section | medium | common | uit_LER-1510-1_2025_spring_1; uit_LER-1152F-1_2018_autumn... | Add line-start split for 'Forkunnskapskrav(, anbefalte forkunnskaper)?' inside course_content and map to prerequisites; map 'Opptakskrav'... |
| ✓ | uit | assessment | duplicate | medium | common | uit_LER-1510-1_2025_spring_1; uit_LER-1710-1_2022_autumn_... | Cut the Eksamen block at the line 'Obligatoriske arbeidskrav' (route that table to coursework_requirements or drop it as duplicate of 'Me... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern '^Forkunnskapskrav\b:?' mapped to prerequisites in R/section_heading_map.R, with the rest of the line kept as se... |
| ✓ | nord | coursework_requirements | split_section | medium | occasional | nord_MAT5006-1_2022_autumn_1 | Attach a paragraph that follows a paragraph ending in ':' (or consisting only of 'Godkjent/ikke godkjent') to the preceding paragraph bef... |
| ✓ | oslomet | cross_section | formatting_noise | medium | occasional | oslomet_MGVM4100-1_2022_autumn_1; oslomet_M5GNO1300-1_202... | Inspect the raw HTML of these two courses to find the source of ';'. Strip a ';' that is directly adjacent to a word boundary or at end o... |
| ✓ | hvl | reading_list | wrong_content | medium | rare | hvl_MGBPØ201-1_2023_spring_1 | Add a line-start rule in sectionize()/.clean_sections() that moves lines beginning with 'Anbefalt litteratur' / 'Pensum' / 'Litteratur:' ... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Add a line-start pattern for '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R (and make sure th... |
| ✓ | oslomet | learning_outcomes | merged_sections | medium | rare | oslomet_M5GKP2100-1_2019_spring_2 | In .clean_sections() for OsloMet, cut learning_outcomes at the first line starting 'Etter fullført emne har studenten', and move the prec... |
| ✓ | steiner | all | other | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Check the URL built for M-PEL2.2 in R/add_course_url.R and compare the emnekode in the page text with course_code; flag mismatches in the... |
| ✓ | uib | course_content | field_or_language_junk | medium | rare | uib_NOLI216-0_2018_autumn_2 | In .clean_sections() (R/extract_sections.R) drop a section whose text consists solely of lines that match known UiB heading labels (the h... |
| ✓ | uio | coursework_requirements | merged_sections | medium | rare | uio_SVLEP3090-1_2025_spring_1 | In R/section_heading_map.R map 'Seminarundervisning', 'Deltakelse i seminarer' and 'Individuell veiledning' to teaching_methods, and 'Fra... |
| ✓ | hivolda | assessment | formatting_noise | low | widespread | hivolda_MGL1-7MA2B-1_2022_spring_1; hivolda_MGL5-10SA1B-1... | Emit table cells with a separator (e.g. ' \| ') for hivolda's assessment table in R/blocks.R, and optionally strip the fixed grade-scale p... |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_2MSF171S-2-1_2022_spring_1; inn_2MNK172S-2-1_2022_spr... | Read the exam table as a table block and drop it (or store it as separate structured columns) when building the assessment section, or st... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_2MEN171S-1-1_2024_spring_1; inn_2MNF5101-4-1_2022_spr... | Keep the sub-heading lines as text inside learning_outcomes (or a consistent marker) instead of dropping them. |
| ✓ | nla | teaching_methods | wrong_content | low | widespread | nla_MGL1PE201-1_2025_autumn_1; nla_4MGL1MA101-1_2025_autu... | Map 'Praksis' to .drop (or to unmapped) for NLA, or only keep it when it has more than a 'Se/Sjå ... praksisplan/studieplan' reference (s... |
| ✓ | nmbu | teaching_methods | missing_section | low | widespread | nmbu_FYS100-1_2025_spring_1; nmbu_PPFD201-1_2025_autumn_2 | Map `Undervisningstider` and `Læringsstøtte` to teaching_methods in R/section_heading_map.R (optionally scoped to NMBU), after checking t... |
| ✓ | inn | teaching_methods | boilerplate_only | low | common | inn_2MSF171S-2-1_2022_spring_1; inn_2MEN171S-1-1_2024_spr... | Map a standalone 'Praksis' heading to .drop (or strip the shared paragraph in .clean_sections()), keeping it only if course-specific prac... |
| ✓ | mf | coursework_requirements | formatting_noise | low | common | mf_RL1010L-1_2025_autumn_2; mf_RL1014-1_2025_autumn_1 | In .clean_sections() for mf, strip the paragraph starting 'Studenter som ikke oppfyller de obligatoriske aktivitetene' and the 'Ved oppme... |
| ✓ | nord | prerequisites | boilerplate_only | low | common | nord_KRO2001-1_2018_autumn_2; nord_KRO1002-1_2018_spring_2 | Add the sentence to the boilerplate/placeholder list so the prerequisites row is not emitted (or is marked as 'none'). |
| ✓ | nord | teaching_methods | wrong_content | low | common | nord_MUS1002-1_2018_autumn_2; nord_MAT2005-1_2019_spring_2 | Keep the mapping but drop pure reference lines ('Se egen plan/Kompetanseguiden ...') in .clean_sections(); acceptable otherwise. |
| ✓ | ntnu | teaching_methods | wrong_content | low | common | ntnu_PPU4625-1_2023_spring_1; ntnu_SOS2020-1_2017_autumn_1 | Accept as source-level; optionally document in data notes. Not worth a heading-map change. |
| ✓ | uia | teaching_methods | other | low | common | uia_NAT115-1_2022_autumn_1; uia_ERN403-1_2025_autumn_1; u... | Decide one target for 'Faget i praksis' (teaching_methods per the current map is reasonable) and make the UiA reader recognise it also as... |
| ✓ | uib | assessment | formatting_noise | low | common | uib_FRAN307L-0_2021_autumn_1; uib_TYS307L-0_2018_autumn_1... | Map 'Vurderingssemester' to .drop in R/section_heading_map.R (like 'Hjelpemiddel til eksamen'), and check where the semester word is adde... |
| ✓ | uib | prerequisites | empty_placeholder | low | common | uib_NOLI103-L-0_2018_spring_1; uib_NOLI250-L-0_2017_spring_1 | Strip block lines that are only 'Ingen'/'-'/'Ikkje relevant' when combining multiple blocks for the same section; optionally keep the hea... |
| ✓ | uis | course_content | duplicate | low | common | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1;... | In .clean_sections(), drop a paragraph in course_content that is a verbatim prefix of, or contained in, another paragraph of the same sec... |
| ✓ | uit | reading_list | boilerplate_only | low | common | uit_LER-1152F-1_2018_autumn_2; uit_LER-1510-1_2025_spring... | Add the 'pensumliste foreligge 15. juni/desember' and 'via Leganto' / 'Pensumliste for X' sentences to the boilerplate-strip list in .cle... |
| ✓ | uit | teaching_methods | formatting_noise | low | common | uit_LRU-1502-1_2012_autumn_2; uit_LER-1510-1_2025_spring_1 | Map 'Undervisnings- og eksamensspråk' and 'Kvalitetssikring (av emnet)' to .drop and strip 'For nærmere informasjon om praksis, se egen p... |
| ✓ | usn | coursework_requirements | formatting_noise | low | common | usn_MG2PE3-1_2021_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Configure the USN section_* fields so headings are read from the page, or have the fallback drop a line that exactly matches a heading al... |
| ✓ | usn | teaching_methods | merged_sections | low | common | usn_MG2RL3-1_2019_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Add 'Praksis' to the heading map as teaching_methods (strip the heading line), or treat it as a sub-heading of Læringsaktiviteter. |
| ✓ | hiof | reading_list | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2021_autumn_1; hiof_LMBNOR10417-1_2020... | In .clean_sections(), broaden the reading_list pattern to '^\s*Litteratur\w*(?:\s+er)?\s+sist\s+oppdater\w*[^\n]*\s*' (case-insensitive).... |
| ✓ | hiof | assessment | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2024_autumn_1; hiof_LMBNOR10417-1_2023... | Add a hiof entry to .section_noise in R/extract_sections.R that removes whole lines only: '(?m)^(?:Oppgaven vurderes av (?:en )?ekstern o... |
| ✓ | hvl | assessment | wrong_content | low | occasional | hvl_MGBPØ201-1_2023_spring_1 | For assessment blocks whose text starts with 'Obligatorisk oppmøte', move that line to coursework_requirements; or leave as is since it o... |
| ✓ | nih | prerequisites | empty_placeholder | low | occasional | nih_LKI130-1_2025_autumn_1; nih_LKI110-1_2025_autumn_1; n... | Make the placeholder rule in R/extract_sections.R (.placeholder_phrases) match '^Ingen( emner)?( i programmet)?\.?$' so all variants are ... |
| ✓ | nord | prerequisites | empty_placeholder | low | occasional | nord_MAT5006-1_2022_spring_2; nord_MAT5003-1_2022_spring_2 | In .clean_sections() drop lines with no alphanumeric characters (e.g. '.', '-') and treat a section with only such lines as empty. |
| ✓ | oslomet | prerequisites | boilerplate_only | low | occasional | oslomet_MGMH2100-1_2019_spring_2; oslomet_M1KP4200-1_2025... | Drop prerequisites that match '^Se (programplanen\|fagplanen)' and treat 'Eksterne søkere' paragraphs after a dropped Opptakskrav heading ... |
| ✓ | oslomet | coursework_requirements | empty_placeholder | low | occasional | oslomet_MGVM4100-1_2020_autumn_2; oslomet_MGVM4100-1_2023... | Treat '^Ingen( arbeidskrav)?\.?$' and '^Se (fag\|program)planen\.?$' as empty and drop them, or keep them consistently and document it. Th... |
| ✓ | steiner | assessment | formatting_noise | low | occasional | steiner_M-MAT1_1_2025_spring_1; steiner_M-MAT1_2_2025_spr... | In .clean_sections() (R/extract_sections.R) drop lines matching ^\d{1,3}$ at the end of a section, and drop the Hjelpemidler/Sensorordnin... |
| ✓ | uis | course_content | boilerplate_only | low | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | Add exact '.drop' rows for 'arbeidsmengd' and 'arbeidsmengde' in R/section_heading_map.R (or move the hours line to teaching_methods). |
| ✓ | uis | prerequisites | empty_placeholder | low | occasional | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1 | Strip standalone 'Ingen'/'Ingen.' lines when other text is present in the section, and drop the sub-heading label line. |
| ✓ | usn | coursework_requirements | wrong_content | low | occasional | usn_MG2RL3-1_2019_spring_2; usn_MG1RL2-1_2019_autumn_2 | Map '^Utgifter i emnet' to .drop in R/section_heading_map.R. |
| ✓ | hvl | assessment | empty_placeholder | low | rare | hvl_MGBPØ401-1_2021_spring_1; hvl_MGBPØ201-1_2021_spring_1 | Add 'Det er ingen vurdering i emnet' to the placeholder patterns so the row is dropped or flagged; low priority. |
| ✓ | nla | prerequisites | missing_section | low | rare | nla_4MGL1NO-M-1_2025_spring_1; nla_4MGL5FOU201-1_2025_aut... | Add a pattern '^progresjonskrav' mapped to prerequisites in R/section_heading_map.R. |
| ✓ | ntnu | assessment | formatting_noise | low | rare | ntnu_HIST2045-1_2021_spring_1 | Map 'Ny eksamen -' to .drop like the other exam-date sub-headings (or strip the 'Vekting ... Eksamenssystem ...' row). |
| ✓ | oslomet | course_content | formatting_noise | low | rare | oslomet_MGPE3300-1_2020_spring_2 | Check the encoding of the raw HTML; map '¿' to an en dash in text cleanup for OsloMet if the raw page really contains it. |
| ✓ | uia | course_content | other | low | rare | uia_NO-157-1_2022_autumn_1 | Add 'Innhaldsliste' to the .drop pattern and place it before the course_content pattern (or anchor the Innhald pattern with $). |
| ✓ | uia | assessment | wrong_content | low | rare | uia_PRA044-1_2024_autumn_1 | Low priority: optionally move sentences matching 'må ... være oppfylt\|fremmøte\|tilstedeværelse' from assessment to coursework_requirement... |
| ✓ | uib | cross_section | truncated | low | rare | uib_RELV105-0_2021_spring_1 | Check which pattern removes the 'koronasituasjonen'/'koronavirus' sentences and restrict it to page-noise rather than course-plan text. |
| ✓ | uio | learning_outcomes | duplicate | low | rare | uio_SVLEP3090-1_2025_spring_1 | Optionally dedupe exact repeated lines within a section in .clean_sections(); otherwise accept as source noise. |
