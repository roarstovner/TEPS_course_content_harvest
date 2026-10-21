# sections audit — aggregated findings

73 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: sonnet).
Mechanical verification: 73 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 11 | hiof, hivolda, hvl, inn, nord, ntnu, oslomet, steiner, uib, uio, uit | medium |
| prerequisites | boilerplate_only | 6 | hvl, nla, nord, ntnu, oslomet, uia | medium |
| assessment | wrong_content | 5 | hiof, nih, ntnu, uio, uit | high |
| prerequisites | empty_placeholder | 4 | hivolda, oslomet, uib, uis | low |
| coursework_requirements | wrong_content | 4 | hvl, nord, uia, uio | medium |
| teaching_methods | merged_sections | 3 | mf, ntnu, usn | high |
| reading_list | empty_placeholder | 3 | hivolda, uit, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 2 | 3 | assessment / formatting_noise | learning_outcomes / wrong_content; assessment / wrong_content | reading_list / formatting_noise |
| nih | 1 | 1 |  | assessment / wrong_content | prerequisites / empty_placeholder |
| uia | 4 | 2 |  | prerequisites / boilerplate_only; coursework_requirements / wrong_content | all / missing_section; teaching_methods / other; course_content / other; assessment / wrong_content |
| uib | 3 | 4 | prerequisites / empty_placeholder | learning_outcomes / merged_sections; assessment / formatting_noise; teaching_methods / duplicate | course_content / field_or_language_junk; assessment / truncated |
| uit | 8 | 7 | assessment / wrong_content; reading_list / empty_placeholder; teaching_methods / formatting_noise; prerequisites / missing_section | cross_section / merged_sections; assessment / formatting_noise; learning_outcomes / wrong_content | assessment / boilerplate_only; reading_list / boilerplate_only; coursework_requirements / formatting_noise; all / other |
| usn | 4 | 6 | teaching_methods / merged_sections; reading_list / formatting_noise | reading_list / empty_placeholder; coursework_requirements / boilerplate_only; cross_section / wrong_content; teaching_methods / duplicate | coursework_requirements / formatting_noise; coursework_requirements / wrong_content |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 16 |
| boilerplate_only | low | 8 |
| empty_placeholder | low | 7 |
| wrong_content | low | 5 |
| wrong_content | medium | 5 |
| formatting_noise | medium | 4 |
| duplicate | low | 3 |
| empty_placeholder | medium | 3 |
| merged_sections | medium | 3 |
| missing_section | medium | 3 |
| wrong_content | high | 3 |
| boilerplate_only | medium | 2 |
| duplicate | medium | 2 |
| merged_sections | high | 2 |
| missing_section | low | 2 |
| other | low | 2 |
| missing_section | high | 1 |
| other | medium | 1 |
| truncated | medium | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 18 |
| prerequisites | 14 |
| teaching_methods | 11 |
| course_content | 6 |
| coursework_requirements | 6 |
| reading_list | 6 |
| cross_section | 5 |
| learning_outcomes | 5 |
| all | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 18 | HiOF extraction is good: 17 of 18 courses split correctly, arbeidskrav land in coursework_requirements, Praksis in teaching_methods, and logistics headings are dropped. One real, high-impact defect: a learning-outcome sub-heading 'Litteratur:' is mapped to reading_list and moves most of the learn... |
| hivolda | sonnet | 19 | Section extraction works well for HVO: the fixed heading structure (Om emnet, Læringsutbytte, Praktisk organisering, Vilkår for å framstille seg til eksamen, exam table) maps cleanly, learning outcomes keep all three sub-groups, and arbeidskrav land in coursework_requirements. Only low-severity n... |
| hvl | sonnet | 20 | HVL extraction is largely accurate: headings map cleanly to the seven sections, learning outcomes keep all sub-groups, and arbeidskrav land in coursework_requirements rather than assessment. Remaining issues are minor: logistics/cost text in teaching_methods, a practice-description block filed as... |
| inn | sonnet | 20 | Section extraction for inn works well: all 20 courses (Norwegian and English page variants) have content, outcomes, methods, coursework (arbeidskrav / obligatoriske aktiviteter) and assessment in the right sections, with nothing lost; placeholders such as 'Ingen pensumliste' are correctly not emi... |
| mf | sonnet | 10 | Extraction is accurate for MF: course_content, coursework_requirements, assessment and learning_outcomes are cleanly cut and complete in all 6 random controls, and sub-headings in learning outcomes do not truncate anything. Two real defects: an inline 'Arbeidsform og organisering' block is absorb... |
| nih | sonnet | 20 | Section extraction for NIH works very well on all 20 random controls: the fixed heading set (Kort om emnet, Forkrav, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur) maps correctly, Arbeidskrav stay in coursework_requirements, admission text (Hvem ka... |
| nla | sonnet | 16 | Norwegian-language NLA plans (about 13 of 16) are extracted very well: the headings are regular, arbeidskrav stay out of assessment, and Kunnskap/Ferdigheter/Generell kompetanse stay inside learning_outcomes. The real defects are in the English-language plans (3GJ*), where several headings are un... |
| nmbu | sonnet | 8 | Section extraction works well for NMBU: all 8 plans have correct, complete and clean sections, with sub-headings kept inside learning outcomes and Obligatorisk aktivitet correctly separated from assessment. Both pre-pass flags are false alarms; the only loss is text under unmapped headings (Laeri... |
| nord | sonnet | 20 | Nord's uniform page structure gives mostly correct sections; most pre-pass flags (short, empty, leak->reading_list) are false alarms. Real problems are coursework requirements (Arbeidskrav) landing in teaching_methods or assessment when they sit outside the standard Eksamensbeskrivelse lines, dup... |
| ntnu | sonnet | 20 | Section splitting works well for NTNU: headings are mapped correctly, sub-headings in learning outcomes survive, and ordinary plans come out clean. The main weaknesses are plans whose text is only a pointer to an English page (sections hold 'See English version.'), and arbeidskrav text filed unde... |
| oslomet | sonnet | 16 | Section extraction works well for OsloMet: headings map correctly, learning outcomes keep all three sub-groups, and arbeidskrav land in coursework_requirements. Remaining problems are exam-logistics and regulation boilerplate inside assessment, admission text in prerequisites, placeholders, and o... |
| steiner | sonnet | 13 | Section extraction works well for Steiner: learning outcomes (all three sub-groups), content, coursework requirements (Arbeidskrav) and assessment are cut correctly in all 13 courses, including the arbeidskrav-vs-assessment split. Only minor noise (page numbers, retained exam-logistics lines) and... |
| uia | sonnet | 20 | Section extraction works very well for UiA: the standard headings (Læringsutbytte, Innhold, Undervisnings- og læringsformer, Vilkår for å gå opp til eksamen, Eksamen) map correctly, sub-headings stay inside learning_outcomes, arbeidskrav go to coursework_requirements and the 'Ingen' placeholder i... |
| uib | sonnet | 20 | UiB section extraction works well: the page has a fixed, regular heading set, all mapped correctly, and coursework vs. assessment, prerequisites and learning outcomes (with Kunnskap/Ferdigheter sub-groups) are intact. Remaining defects are small: a stray 'Hjelpemiddel til eksamen' label in assess... |
| uio | sonnet | 10 | UiO extraction is mostly sound: course_content, learning_outcomes (sub-groups kept), prerequisites and reading_list are correct, and admission text is dropped. The recurring defects are the unmapped Karakterskala sub-heading appended to assessment, and coursework gates (arbeidskrav, reflection no... |
| uis | sonnet | 16 | Section splitting is largely correct for UiS: learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, prerequisites are clean, and the 'Vilkår for å gå opp til eksamen' block is separated from assessment. Most pre-pass flags (empty/boilerplate teaching_methods, blob on the MGL... |
| uit | sonnet | 20 | Modern (2018+) pages and the old 2008 format split cleanly at their headings; most flags are false alarms from thin 2008 source pages. Real problems are one long specialist-education plan swallowed into course_content with its arbeidskrav filed as learning_outcomes, arbeidskrav text left in asses... |
| usn | sonnet | 16 | USN extraction is mostly correct: course content, learning outcomes (all sub-groups), prerequisites and the assessment/coursework split are right in the large majority of the 16 plans, and most pre-pass flags (leak->X, blob, empty, short) are false alarms. Real defects are concentrated in long pr... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nla | cross_section | missing_section | high | common | nla_3GJ322-1_2019_autumn_1; nla_3GJ302-1_2025_autumn_1 | Add patterns in R/section_heading_map.R: ^introduction$ -> course_content, ^teaching (and\|&) learning methods$ -> teaching_methods, ^fina... |
| ✓ | usn | cross_section | wrong_content | high | occasional | usn_BS-PLVI30-V-1_2018_autumn_3; usn_050-E11-1_2018_autumn_1 | Add a line-start rule in sectionize()/.clean_sections() that moves paragraphs starting '^Arbeidskrav \d' (to the next non-arbeidskrav hea... |
| ✓ | usn | teaching_methods | merged_sections | high | occasional | usn_050-E11-1_2018_autumn_1 | Map the USN heading '^Praksis$' in R/section_heading_map.R and split its paragraphs: attendance/absence to coursework_requirements, 'vurd... |
| ✓ | hiof | learning_outcomes | wrong_content | high | rare | hiof_SFT11608-1_2022_autumn_1 | For hiof, make sub-headings (sub) found while inside learning_outcomes not switch section (stay in learning_outcomes) unless they are Kun... |
| ✓ | ntnu | assessment | wrong_content | high | rare | ntnu_LGU14008-A_2019_autumn_1 | In .clean_sections() or sectionize(), move paragraphs of the assessment block that start with 'Obligatoriske arbeidskrav' / 'Arbeidskrav'... |
| ✓ | uit | cross_section | merged_sections | high | rare | uit_ODO-6302-1_2019_autumn_1 | Add line-start heading patterns for 'Arbeidskrav' (-> coursework_requirements) in R/section_heading_map.R and allow these in-text heading... |
| ✓ | mf | reading_list | missing_section | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1; mf_... | Check the MF page source for where the literature list is rendered and extend the MF fulltext/section_* selector to include it; otherwise... |
| ✓ | oslomet | assessment | formatting_noise | medium | widespread | oslomet_ØAADM6000-1_2018_autumn_1; oslomet_BYGG2200-1_202... | Map 'Hjelpemidler ved eksamen' to .drop (or strip it in .clean_sections()) for OsloMet, keeping Vurderingsuttrykk if the grading scale is... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_RELDID4009-1_2025_sprin... | Map `Karakterskala` to .drop in R/section_heading_map.R like the sibling Eksamensspråk, Hjelpemidler and Adgang til ny eller utsatt eksam... |
| ✓ | uit | reading_list | empty_placeholder | medium | widespread | uit_BED-2017-1_2008_autumn_2; uit_ARK-3021-1_2008_autumn_... | Drop rows matching these exact UiT placeholder strings in .clean_sections() (reading_list: 'Ta kontakt med instituttet/fakultetet', 'Se h... |
| ✓ | nord | coursework_requirements | wrong_content | medium | common | nord_MET2000-1_2022_spring_1; nord_PRO9011-1_2025_autumn_... | In .clean_sections()/sectionize() add a block-level rule: a paragraph starting 'Arbeidskrav:', 'Coursework requirements:' or 'AK/' (and t... |
| ✓ | ntnu | cross_section | empty_placeholder | medium | common | ntnu_IMT4127-1_2022_autumn_1; ntnu_SMF1081-1_2022_spring_... | In .clean_sections() drop section text matching a pointer regex (^(see\|se\|samme som)\b.{0,40}(english\|engelsk\|NORD\d+)...$) so no row is ... |
| ✓ | uio | coursework_requirements | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_KJM5050-1_2025_spring_1 | Add 'Obligatoriske (forhold\|komponenter)' patterns mapped to coursework_requirements in R/section_heading_map.R, and let the uio section_... |
| ✓ | uis | cross_section | formatting_noise | medium | common | uis_LENG116-1_2015_autumn_1; uis_LFYMAS-1_2017_spring_1; ... | For uis, strip lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$' and lines equal to '<emnekode> - <titl... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In the uis cleanup, remove tokens matching 'https?://bibsys-ur\.userservices\.exlibrisgroup\.com/\S*(?:\s*\n?\S*rft[._]\S*)*' up to 'View... |
| ✓ | uit | assessment | wrong_content | medium | common | uit_HIS-1007-1_2008_autumn_1; uit_FHO9043-1_2019_autumn_1... | Extend the inline-label splitter in .clean_sections()/sectionize() to recognise 'Beskriving av arbeidskrav' and 'obligatoriske ... godkje... |
| ✓ | usn | reading_list | empty_placeholder | medium | common | usn_MD-F3M2000-1_2018_autumn_1; usn_MM-MAC5080-1_2018_aut... | In .clean_sections() (R/extract_sections.R) drop a reading_list whose whole text matches '^The reading list is not published yet\.?$' (an... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern '^Forkunnskapskrav\b:?' mapped to prerequisites in R/section_heading_map.R, with the rest of the line kept as se... |
| ✓ | uia | prerequisites | boilerplate_only | medium | occasional | uia_ORG966-1_2025_autumn_1; uia_ORG937-1_2025_autumn_1 | In .clean_sections() (or a UiA post_fn for sections) drop prerequisites paragraphs matching 'opptak\|rangeres\|studieplass\|udir\.no\|betinge... |
| ✓ | uio | assessment | wrong_content | medium | occasional | uio_KJM5050-1_2025_spring_1; uio_KJM5930L-2_2025_spring_1 | Extend the existing whole-item coursework-gate move (#283) to UiO assessment, with patterns such as 'arbeidskrav', 'obligatoriske aktivit... |
| ✓ | usn | coursework_requirements | boilerplate_only | medium | occasional | usn_BS-PLVI30-V-1_2018_autumn_3 | Add a cleanup (.drop or boilerplate regex) in .clean_sections() removing the block starting at a line '^Skikkethetsvurdering' through the... |
| ✓ | usn | teaching_methods | duplicate | medium | occasional | usn_050-E11-1_2018_autumn_1 | Handle together with the Praksis heading fix; optionally dedupe paragraphs in teaching_methods identical to course_content in .clean_sect... |
| ✓ | hvl | coursework_requirements | wrong_content | medium | rare | hvl_LSU301-1_2022_autumn_1 | Add a pattern for 'PRAKSIS ... STUDIEÅR' headings in R/section_heading_map.R (map to course_content or teaching_methods), so it ends the ... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Add a line-start pattern for '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R (and make sure th... |
| ✓ | nord | assessment | missing_section | medium | rare | nord_SOS5003-1_2022_autumn_3 | When the entire assessment text would be moved to coursework_requirements, or the line contains exam words (eksamen, hjemmeeksamen, oppga... |
| ✓ | nord | prerequisites | duplicate | medium | rare | nord_PRO9011-1_2025_autumn_1; nord_PRO9011-1_2025_spring_1 | De-duplicate near-identical paragraphs within a section in .clean_sections() (normalised-text or high-similarity match). Optionally route... |
| ✓ | ntnu | teaching_methods | merged_sections | medium | rare | ntnu_NORX2001-1_2025_autumn_2; ntnu_SARB8002-1_2007_autumn_1 | Add paragraph-level routing in .clean_sections(): move lines in teaching_methods that start with 'Obligatoriske arbeidskrav' to coursewor... |
| ✓ | oslomet | prerequisites | truncated | medium | rare | oslomet_BIOB3210-1_2025_autumn_2 | Inspect the blocks for this page and check that the last list line before 'Læringsutbytte' is not treated as a heading or admission .drop. |
| ✓ | steiner | all | other | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Check the URL built for M-PEL2.2 in R/add_course_url.R and compare the emnekode in the page text with course_code; flag mismatches in the... |
| ✓ | uib | learning_outcomes | merged_sections | medium | rare | uib_VIT217-0_2025_autumn_1 | Make the UiB block reader split on 'Arbeids- og undervisningsformer' wherever it occurs (also inside the Emnebeskrivelse container), and ... |
| ✓ | hivolda | learning_outcomes | formatting_noise | low | widespread | hivolda_MGL5-10SA1B-1_2025_spring_1; hivolda_MID112-1_201... | Add a leading-line strip in .clean_sections() (or an hivolda-specific rule) for lines matching '^I samsvar med det nasjonale kvalifikasjo... |
| ✓ | hivolda | assessment | formatting_noise | low | widespread | hivolda_ENG144-1_2022_spring_1; hivolda_JOP100-1_2025_aut... | Drop the fixed header row when flattening the HVO exam table (or reformat rows as labelled fields), so only data rows remain. |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_3JUS330-1_2023_autumn_1; inn_ØKA1026-1_2025_spring_1;... | Drop the table header line and the table data row (or keep only the exam form and grade scale) in .clean_sections() for INN assessment, a... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_REGF2009-1_2025_autumn_1; inn_KS4102-1_2023_autumn_1;... | Keep the sub-heading lines as text inside learning_outcomes (Kunnskap / Ferdigheter / Generell kompetanse and the English equivalents). |
| ✓ | nmbu | teaching_methods | missing_section | low | widespread | nmbu_FYS100-1_2025_spring_1; nmbu_PPFD201-1_2025_autumn_2 | Map `Undervisningstider` and `Læringsstøtte` to teaching_methods in R/section_heading_map.R (optionally scoped to NMBU), after checking t... |
| ✓ | uit | teaching_methods | formatting_noise | low | widespread | uit_LER-3101-1_2025_spring_1; uit_LRU-3909-1_2018_spring_... | Map 'Undervisnings- og eksamensspråk' and 'Language of instruction and examination' to .drop in R/section_heading_map.R (and 'Kvalitetssi... |
| ✓ | hvl | teaching_methods | formatting_noise | low | common | hvl_NHB2-1_2019_spring_1; hvl_FK215-1_2022_autumn_1 | Add HVL-specific drop rules in .clean_sections()/.drop for lines on 'Kr \d+', 'materialpenger', 'egenandel', 'Eksamen blir avholdt' and '... |
| ✓ | mf | coursework_requirements | formatting_noise | low | common | mf_RL1010L-1_2025_autumn_2; mf_RL1014-1_2025_autumn_1 | In .clean_sections() for mf, strip the paragraph starting 'Studenter som ikke oppfyller de obligatoriske aktivitetene' and the 'Ved oppme... |
| ✓ | nla | teaching_methods | empty_placeholder | low | common | nla_4HSM211-1_2019_autumn_1; nla_GL17KRØ1-1_2017_autumn_1... | In .clean_sections() in R/extract_sections.R, drop lines that match ^(ingen\|ingen\.\|-\|nei)$ (case-insensitive) when they are the whole te... |
| ✓ | nla | teaching_methods | wrong_content | low | common | nla_MGL1KØ101-1_2025_autumn_1 | In .clean_sections(), drop paragraphs that consist only of a reference ('^(se\|sjå\|see)\b.*(praksis\|plan)') and keep 'Praksis' text otherw... |
| ✓ | nord | prerequisites | boilerplate_only | low | common | nord_IKT1013-1_2019_autumn_2; nord_ORG5011-1_2022_spring_... | Add admission/opptakskrav phrases ('oppfylle gjeldende opptakskrav', 'tatt opp som student', 'Se informasjon om opptakskrav', 'ingen fork... |
| ✓ | nord | assessment | formatting_noise | low | common | nord_LED5018-1_2022_autumn_2; nord_ENT5011-1_2022_autumn_... | Strip the heading line 'Hjelpemidler til eksamen' (and optionally drop the aids paragraph per spec), and add the 'Det tas forbehold om en... |
| ✓ | ntnu | assessment | formatting_noise | low | common | ntnu_MET420-A_2025_spring_1; ntnu_TS200114-1_2022_autumn_1 | In .clean_sections(), drop assessment lines matching 'Hjelpemidler til eksamen', 'oppmeldingsfrist', 'Eksamenssystem' and calculator mode... |
| ✓ | oslomet | assessment | boilerplate_only | low | common | oslomet_PVUB7000-1_2019_autumn_1; oslomet_BLH3140-1_2019_... | Map 'Skikkethetsvurdering' to .drop and add a .clean_sections() line filter for 'ny/utsatt eksamen' regulation sentences. |
| ✓ | oslomet | prerequisites | empty_placeholder | low | common | oslomet_CORO7100-2_2018_autumn_1; oslomet_MSLV4500-1_2018... | Add 'Ingen info' to the placeholder patterns so the row is not emitted (also trailing 'Ingen info' in assessment). |
| ✓ | uib | assessment | formatting_noise | low | common | uib_PEDA130-0_2024_autumn_2; uib_EXFAC00SK-0_2025_autumn_... | Map 'Hjelpemiddel til eksamen' to .drop (or drop the line and its value when the value is Ingen/Ikkje relevant), per the spec's line betw... |
| ✓ | uis | course_content | duplicate | low | common | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1;... | In .clean_sections(), drop a paragraph in course_content that is a verbatim prefix of, or contained in, another paragraph of the same sec... |
| ✓ | uit | assessment | formatting_noise | low | common | uit_END-3602-1_2025_autumn_1; uit_BIO-3529-2_2025_autumn_2 | Strip the 'Everything you need to know about before, during, and after the exam ...' line in .clean_sections(); map 'Re-sit examination' ... |
| ✓ | usn | reading_list | formatting_noise | low | common | usn_OKLI3014-1_2018_autumn_1; usn_MAR4020-1_2022_autumn_1... | Strip the first line matching '^.+ \([A-Z0-9-]+\) [A-ZÆØÅ][a-zæøå]+$' and lines equal to 'In compendium' in reading_list cleanup. |
| ✓ | hvl | assessment | formatting_noise | low | occasional | hvl_ØAM112-1_2019_autumn_1; hvl_NHB2-1_2019_autumn_1; hvl... | Drop lines matching 'Tid (og sted )?for (eksamen\|ut- og innlevering)' and 'opplyst på (Studentweb\|emnets sider)' in assessment cleaning. |
| ✓ | hvl | all | empty_placeholder | low | occasional | hvl_Å-MU2A-1_2019_spring_3; hvl_ØKB3119-1_2017_autumn_1; ... | Treat '_', '-' and bare 'Ja'/'Nei' as empty in the placeholder filter (keep the longer 'Ja, vert spesifisert' variant as a short requirem... |
| ✓ | inn | teaching_methods | other | low | occasional | inn_LAN2300-1_2025_autumn_1; inn_2MKRLE171-6-1_2023_spring_1 | Map the heading 'Emneevaluering – kvalitetssystem' to .drop in R/section_heading_map.R; consider dropping the generic Praksisstudiet para... |
| ✓ | nla | course_content | formatting_noise | low | occasional | nla_3GJ302-1_2025_autumn_1; nla_GL17NO2-1_2017_autumn_1 | Insert a space or newline when joining adjacent inline or block nodes in the reader (R/blocks.R), or add a regex after extraction that sp... |
| ✓ | nla | prerequisites | boilerplate_only | low | occasional | nla_3GJ302-1_2025_autumn_1 | Add an English pattern such as '^view the admission requirements' (and 'admission requirements for') to the prerequisites boilerplate fil... |
| ✓ | ntnu | prerequisites | boilerplate_only | low | occasional | ntnu_LGU14008-A_2019_autumn_1; ntnu_PPU4725-1_2007_autumn_1 | In .clean_sections(), drop sentences in prerequisites matching admission patterns ('plasser er forbeholdt', '^Opptak til', 'Krever opptak... |
| ✓ | oslomet | prerequisites | boilerplate_only | low | occasional | oslomet_PVUB7000-1_2019_autumn_1; oslomet_PVUB7000-1_2019... | Optionally drop paragraphs starting with 'Opptakskrav' / mentioning 'prioritert ved opptak' / 'generell studiekompetanse' in prerequisites. |
| ✓ | steiner | assessment | formatting_noise | low | occasional | steiner_M-MAT1_1_2025_spring_1; steiner_M-MAT1_2_2025_spr... | In .clean_sections() (R/extract_sections.R) drop lines matching ^\d{1,3}$ at the end of a section, and drop the Hjelpemidler/Sensorordnin... |
| ✓ | uib | prerequisites | empty_placeholder | low | occasional | uib_AHKR391-0_2019_autumn_1; uib_ENGMAU655-0_2025_spring_1 | In .clean_sections(), drop lines that are only a placeholder ('Ingen', 'Ingen.', '-') in prerequisites; optionally prefix the 'Anbefalte ... |
| ✓ | uis | course_content | boilerplate_only | low | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | Add exact '.drop' rows for 'arbeidsmengd' and 'arbeidsmengde' in R/section_heading_map.R (or move the hours line to teaching_methods). |
| ✓ | uis | prerequisites | empty_placeholder | low | occasional | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1 | Strip standalone 'Ingen'/'Ingen.' lines when other text is present in the section, and drop the sub-heading label line. |
| ✓ | hiof | assessment | wrong_content | low | rare | hiof_LU15SAMG07-1_2007_autumn_2 | Add 'underveisvurdering' mapped to coursework_requirements (or .drop) in R/section_heading_map.R, keeping 'sluttvurdering' in assessment;... |
| ✓ | hiof | assessment | formatting_noise | low | rare | hiof_LSKNO22215-1_2019_spring_1 | Add a line pattern to the section noise in R/extract_sections.R removing '(?m)^Plagiatkontroll.*$' (and 'Plagiering og avskrift av faglit... |
| ✓ | hivolda | reading_list | empty_placeholder | low | rare | hivolda_PPY121-1_2016_autumn_1 | Extend the reading_list placeholder pattern to cover '^Sjå pensumliste for emnet her$'. |
| ✓ | hivolda | prerequisites | empty_placeholder | low | rare | hivolda_DRHS902-1_2019_spring_1 | Treat '^Ingen\.? (Sjå\|Se) opptakskrav' as a placeholder in prerequisites cleaning. |
| ✓ | hvl | prerequisites | boilerplate_only | low | rare | hvl_ØAM112-1_2019_autumn_1 | Drop prerequisites lines equal to 'Studiets opptakskrav' / 'Opptakskrav' (also 'Studierett på Master ...' type lines if the spec treats t... |
| ✓ | nih | assessment | wrong_content | low | rare | nih_FRI304-1_2025_spring_1; nih_PPU405-1_2025_autumn_1 | Optional: in .clean_sections() move lines starting with 'Oppmøtekrav' or matching '^\d+ ?% (frammøte\|oppmøte\|deltakelse)' from assessment... |
| ✓ | ntnu | course_content | duplicate | low | rare | ntnu_MET420-A_2025_spring_1 | Add exact-duplicate paragraph removal within a section in .clean_sections(). Optionally add the 'Tar forbehold om endringer' disclaimer t... |
| ✓ | oslomet | course_content | boilerplate_only | low | rare | oslomet_TOLKDIAPBT-1_2018_autumn_1 | Add a line filter for 'Vedtatt i ... Godkjent av rektor' in .clean_sections(). |
| ✓ | uia | coursework_requirements | wrong_content | low | rare | uia_PSY914-G_2022_autumn_1 | Low priority: in .clean_sections() move sentences matching '^Forventet arbeidsomfang\|^Arbeidsomfang' from coursework_requirements to teac... |
| ✓ | uib | teaching_methods | duplicate | low | rare | uib_MUV295-0_2025_autumn_1 | No change needed. At most, deduplicate identical paragraphs across sections within a course in .clean_sections(). |
| ✓ | uio | course_content | other | low | rare | uio_HIS4015L-1_2025_spring_1 | Require the leak heading match to be a whole line, or word-bounded, in R/audit/qa_sections.R. No pipeline change needed. |
| ✓ | uit | prerequisites | missing_section | low | rare | uit_LER-3101-1_2025_spring_1; uit_RUS-3950-1_2008_autumn_1 | Treat a standalone 'Forkunnskapskrav' line as a heading for prerequisites in the text-label splitter; optionally move 'Anbefalte forkunns... |
| ✓ | uit | learning_outcomes | wrong_content | low | rare | uit_ARK-3021-1_2008_autumn_1 | No pipeline change recommended; accept as source noise. |
