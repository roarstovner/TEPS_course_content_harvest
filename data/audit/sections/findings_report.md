# sections audit — aggregated findings

73 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: opus, sonnet).
Mechanical verification: 73 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 9 | hiof, hivolda, hvl, inn, nord, ntnu, oslomet, steiner, uio | medium |
| prerequisites | boilerplate_only | 5 | hvl, nla, nord, ntnu, oslomet | low |
| prerequisites | empty_placeholder | 5 | hivolda, nih, oslomet, uib, uis | low |
| assessment | wrong_content | 4 | ntnu, uia, uio, uit | high |
| coursework_requirements | wrong_content | 4 | hvl, nord, uio, usn | medium |
| coursework_requirements | formatting_noise | 3 | mf, uit, usn | low |
| reading_list | formatting_noise | 3 | hiof, uis, usn | medium |
| teaching_methods | merged_sections | 3 | mf, ntnu, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hivolda | 1 | 4 | assessment / formatting_noise | learning_outcomes / formatting_noise; reading_list / empty_placeholder; prerequisites / empty_placeholder |  |
| hvl | 3 | 5 |  | coursework_requirements / wrong_content; teaching_methods / formatting_noise; assessment / formatting_noise; prerequisites / boilerplate_only; all / empty_placeholder | reading_list / wrong_content; assessment / wrong_content; assessment / empty_placeholder |
| inn | 4 | 3 | assessment / formatting_noise; learning_outcomes / formatting_noise | teaching_methods / other | learning_outcomes / truncated; teaching_methods / boilerplate_only |
| nla | 2 | 5 | teaching_methods / wrong_content | cross_section / missing_section; teaching_methods / empty_placeholder; course_content / formatting_noise; prerequisites / boilerplate_only | prerequisites / missing_section |
| nord | 6 | 5 | assessment / formatting_noise | coursework_requirements / wrong_content; assessment / missing_section; prerequisites / duplicate; prerequisites / boilerplate_only | assessment / wrong_content; course_content / truncated; prerequisites / missing_section; prerequisites / empty_placeholder; teaching_methods / wrong_content |
| ntnu | 3 | 6 | assessment / formatting_noise | cross_section / empty_placeholder; assessment / wrong_content; teaching_methods / merged_sections; prerequisites / boilerplate_only; course_content / duplicate | assessment / truncated; teaching_methods / wrong_content |
| oslomet | 7 | 6 | prerequisites / boilerplate_only | assessment / formatting_noise; assessment / boilerplate_only; prerequisites / empty_placeholder; prerequisites / truncated; course_content / boilerplate_only | all / missing_section; cross_section / formatting_noise; teaching_methods / missing_section; learning_outcomes / merged_sections; coursework_requirements / empty_placeholder; course_content / formatting_noise |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 16 |
| boilerplate_only | low | 9 |
| empty_placeholder | low | 9 |
| missing_section | medium | 6 |
| formatting_noise | medium | 5 |
| other | low | 5 |
| wrong_content | medium | 5 |
| wrong_content | low | 3 |
| duplicate | low | 2 |
| merged_sections | medium | 2 |
| boilerplate_only | medium | 1 |
| duplicate | medium | 1 |
| empty_placeholder | medium | 1 |
| field_or_language_junk | medium | 1 |
| merged_sections | low | 1 |
| missing_section | high | 1 |
| missing_section | low | 1 |
| other | medium | 1 |
| truncated | low | 1 |
| truncated | medium | 1 |
| wrong_content | high | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 17 |
| prerequisites | 15 |
| teaching_methods | 10 |
| course_content | 8 |
| coursework_requirements | 7 |
| reading_list | 7 |
| all | 4 |
| cross_section | 3 |
| learning_outcomes | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 16 | Section extraction for HiOF is accurate in all 16 courses, including both random controls. The h2 headings split cleanly. Kunnskap/Ferdigheter/Generell kompetanse groups are all kept in learning_outcomes, arbeidskrav always land in coursework_requirements, Praksis is in teaching_methods, and Sens... |
| hivolda | sonnet | 19 | Section extraction works well for HVO: the fixed heading structure (Om emnet, Læringsutbytte, Praktisk organisering, Vilkår for å framstille seg til eksamen, exam table) maps cleanly, learning outcomes keep all three sub-groups, and arbeidskrav land in coursework_requirements. Only low-severity n... |
| hvl | sonnet | 20 | HVL extraction is largely accurate: headings map cleanly to the seven sections, learning outcomes keep all sub-groups, and arbeidskrav land in coursework_requirements rather than assessment. Remaining issues are minor: logistics/cost text in teaching_methods, a practice-description block filed as... |
| inn | sonnet | 20 | Section extraction for inn works well: all 20 courses (Norwegian and English page variants) have content, outcomes, methods, coursework (arbeidskrav / obligatoriske aktiviteter) and assessment in the right sections, with nothing lost; placeholders such as 'Ingen pensumliste' are correctly not emi... |
| mf | sonnet | 10 | Extraction is accurate for MF: course_content, coursework_requirements, assessment and learning_outcomes are cleanly cut and complete in all 6 random controls, and sub-headings in learning outcomes do not truncate anything. Two real defects: an inline 'Arbeidsform og organisering' block is absorb... |
| nih | sonnet | 20 | Section extraction for NIH works very well: every heading (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen) maps to the right section, arbeidskrav stay out of assessment, plagiarism/WISEflow notices are removed and placeholder-only reading lists are cor... |
| nla | sonnet | 16 | Norwegian-language NLA plans (about 13 of 16) are extracted very well: the headings are regular, arbeidskrav stay out of assessment, and Kunnskap/Ferdigheter/Generell kompetanse stay inside learning_outcomes. The real defects are in the English-language plans (3GJ*), where several headings are un... |
| nmbu | sonnet | 8 | Section extraction works well for NMBU: all 8 plans have correct, complete and clean sections, with sub-headings kept inside learning outcomes and Obligatorisk aktivitet correctly separated from assessment. Both pre-pass flags are false alarms; the only loss is text under unmapped headings (Laeri... |
| nord | sonnet | 20 | Nord's uniform page structure gives mostly correct sections; most pre-pass flags (short, empty, leak->reading_list) are false alarms. Real problems are coursework requirements (Arbeidskrav) landing in teaching_methods or assessment when they sit outside the standard Eksamensbeskrivelse lines, dup... |
| ntnu | sonnet | 20 | Section splitting works well for NTNU: headings are mapped correctly, sub-headings in learning outcomes survive, and ordinary plans come out clean. The main weaknesses are plans whose text is only a pointer to an English page (sections hold 'See English version.'), and arbeidskrav text filed unde... |
| oslomet | sonnet | 16 | Section extraction works well for OsloMet: headings map correctly, learning outcomes keep all three sub-groups, and arbeidskrav land in coursework_requirements. Remaining problems are exam-logistics and regulation boilerplate inside assessment, admission text in prerequisites, placeholders, and o... |
| steiner | sonnet | 13 | Section extraction works well for Steiner: learning outcomes (all three sub-groups), content, coursework requirements (Arbeidskrav) and assessment are cut correctly in all 13 courses, including the arbeidskrav-vs-assessment split. Only minor noise (page numbers, retained exam-logistics lines) and... |
| uia | sonnet | 20 | Section extraction works well for UiA pages that carry the standard Læringsutbytte/Innhold/Undervisnings- og læringsformer/Vilkår/Eksamen headings: sub-headings stay in learning_outcomes, arbeidskrav go to coursework_requirements and prerequisites are clean. Most pre-pass flags are false alarms. ... |
| uib | sonnet | 13 | Section extraction for uib works well: heading map is accurate, sections are complete and clean in all random controls and most suspects, and admin headings are dropped correctly. Remaining problems are a heading-only page whose heading list was emitted as course_content, and corona-related sente... |
| uio | sonnet | 10 | UiO extraction is mostly sound: course_content, learning_outcomes (sub-groups kept), prerequisites and reading_list are correct, and admission text is dropped. The recurring defects are the unmapped Karakterskala sub-heading appended to assessment, and coursework gates (arbeidskrav, reflection no... |
| uis | sonnet | 16 | Section splitting is largely correct for UiS: learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, prerequisites are clean, and the 'Vilkår for å gå opp til eksamen' block is separated from assessment. Most pre-pass flags (empty/boilerplate teaching_methods, blob on the MGL... |
| uit | sonnet | 20 | The modern UiT layout (Om emnet / Hva lærer du / Undervisning og pensum / Eksamen with Obligatoriske arbeidskrav sub-headings) is split well: outcomes are complete, reading lists and arbeidskrav land in the right sections. Recurring weaknesses are the Forkunnskapskrav paragraph left inside course... |
| usn | sonnet | 18 | USN section extraction is largely correct: all seven sections land in the right place for the 18 plans, including learning outcomes with all three sub-groups and arbeidskrav separated from assessment. Remaining defects are cosmetic: Leganto UI text in reading_list, and sub-heading lines left in c... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nla | cross_section | missing_section | high | common | nla_3GJ322-1_2019_autumn_1; nla_3GJ302-1_2025_autumn_1 | Add patterns in R/section_heading_map.R: ^introduction$ -> course_content, ^teaching (and\|&) learning methods$ -> teaching_methods, ^fina... |
| ✓ | ntnu | assessment | wrong_content | high | rare | ntnu_LGU14008-A_2019_autumn_1 | In .clean_sections() or sectionize(), move paragraphs of the assessment block that start with 'Obligatoriske arbeidskrav' / 'Arbeidskrav'... |
| ✓ | mf | reading_list | missing_section | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1; mf_... | Check the MF page source for where the literature list is rendered and extend the MF fulltext/section_* selector to include it; otherwise... |
| ✓ | oslomet | assessment | formatting_noise | medium | widespread | oslomet_ØAADM6000-1_2018_autumn_1; oslomet_BYGG2200-1_202... | Map 'Hjelpemidler ved eksamen' to .drop (or strip it in .clean_sections()) for OsloMet, keeping Vurderingsuttrykk if the grading scale is... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_RELDID4009-1_2025_sprin... | Map `Karakterskala` to .drop in R/section_heading_map.R like the sibling Eksamensspråk, Hjelpemidler and Adgang til ny eller utsatt eksam... |
| ✓ | usn | reading_list | formatting_noise | medium | widespread | usn_MG1PE3-1_2021_spring_2; usn_MG2RL3-1_2019_spring_2; u... | Add a USN reading-list cleanup that strips the lines '^Click to view interactive reading list in Leganto' (and splits the glued section h... |
| ✓ | nord | coursework_requirements | wrong_content | medium | common | nord_MET2000-1_2022_spring_1; nord_PRO9011-1_2025_autumn_... | In .clean_sections()/sectionize() add a block-level rule: a paragraph starting 'Arbeidskrav:', 'Coursework requirements:' or 'AK/' (and t... |
| ✓ | ntnu | cross_section | empty_placeholder | medium | common | ntnu_IMT4127-1_2022_autumn_1; ntnu_SMF1081-1_2022_spring_... | In .clean_sections() drop section text matching a pointer regex (^(see\|se\|samme som)\b.{0,40}(english\|engelsk\|NORD\d+)...$) so no row is ... |
| ✓ | uia | all | missing_section | medium | common | uia_PED428-1_2023_autumn_1; uia_IDR418-1_2023_autumn_1; u... | Flag plans whose extracted text has no mapped content heading (only .drop/unmapped headings) as 'no_plan_content' in the fulltext/harvest... |
| ✓ | uio | coursework_requirements | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_KJM5050-1_2025_spring_1 | Add 'Obligatoriske (forhold\|komponenter)' patterns mapped to coursework_requirements in R/section_heading_map.R, and let the uio section_... |
| ✓ | uis | cross_section | formatting_noise | medium | common | uis_LENG116-1_2015_autumn_1; uis_LFYMAS-1_2017_spring_1; ... | For uis, strip lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$' and lines equal to '<emnekode> - <titl... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In the uis cleanup, remove tokens matching 'https?://bibsys-ur\.userservices\.exlibrisgroup\.com/\S*(?:\s*\n?\S*rft[._]\S*)*' up to 'View... |
| ✓ | uit | prerequisites | missing_section | medium | common | uit_LER-3211-1_2024_spring_1; uit_LER-1123-1_2023_spring_... | Add a line-start split for 'Forkunnskapskrav' within the Om emnet block for uit (or let the reader treat that label as a sub-heading) so ... |
| ✓ | uit | prerequisites | missing_section | medium | common | uit_LER-3211-1_2024_spring_1; uit_LRU-1271-1_2012_spring_... | For uit, map 'Opptakskrav' to prerequisites when the text names a course/credit/previous-year requirement, or drop only when it is pure a... |
| ✓ | uit | assessment | boilerplate_only | medium | common | uit_LER-3211-1_2024_spring_1; uit_LER-1123-1_2023_spring_... | Keep the exam form and duration but strip empty 'Dato:' fields and 'Karakterregel:' tails in .clean_sections() for uit, or mark the row a... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern '^Forkunnskapskrav\b:?' mapped to prerequisites in R/section_heading_map.R, with the rest of the line kept as se... |
| ✓ | uio | assessment | wrong_content | medium | occasional | uio_KJM5050-1_2025_spring_1; uio_KJM5930L-2_2025_spring_1 | Extend the existing whole-item coursework-gate move (#283) to UiO assessment, with patterns such as 'arbeidskrav', 'obligatoriske aktivit... |
| ✓ | uit | assessment | wrong_content | medium | occasional | uit_LRU-1271-1_2012_spring_1 | In the uit Eksamen block, extend the arbeidskrav chunk until the next line-start 'Eksamen og vurdering' / 'Eksamen består av' instead of ... |
| ✓ | hvl | coursework_requirements | wrong_content | medium | rare | hvl_LSU301-1_2022_autumn_1 | Add a pattern for 'PRAKSIS ... STUDIEÅR' headings in R/section_heading_map.R (map to course_content or teaching_methods), so it ends the ... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Add a line-start pattern for '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R (and make sure th... |
| ✓ | nord | assessment | missing_section | medium | rare | nord_SOS5003-1_2022_autumn_3 | When the entire assessment text would be moved to coursework_requirements, or the line contains exam words (eksamen, hjemmeeksamen, oppga... |
| ✓ | nord | prerequisites | duplicate | medium | rare | nord_PRO9011-1_2025_autumn_1; nord_PRO9011-1_2025_spring_1 | De-duplicate near-identical paragraphs within a section in .clean_sections() (normalised-text or high-similarity match). Optionally route... |
| ✓ | ntnu | teaching_methods | merged_sections | medium | rare | ntnu_NORX2001-1_2025_autumn_2; ntnu_SARB8002-1_2007_autumn_1 | Add paragraph-level routing in .clean_sections(): move lines in teaching_methods that start with 'Obligatoriske arbeidskrav' to coursewor... |
| ✓ | oslomet | prerequisites | truncated | medium | rare | oslomet_BIOB3210-1_2025_autumn_2 | Inspect the blocks for this page and check that the last list line before 'Læringsutbytte' is not treated as a heading or admission .drop. |
| ✓ | steiner | all | other | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Check the URL built for M-PEL2.2 in R/add_course_url.R and compare the emnekode in the page text with course_code; flag mismatches in the... |
| ✓ | uib | course_content | field_or_language_junk | medium | rare | uib_NOLI216-0_2018_autumn_2 | In sectionize()/.clean_sections() drop any section whose lines are all (after trimming) recognised headings from the heading map (or the ... |
| ✓ | hivolda | learning_outcomes | formatting_noise | low | widespread | hivolda_MGL5-10SA1B-1_2025_spring_1; hivolda_MID112-1_201... | Add a leading-line strip in .clean_sections() (or an hivolda-specific rule) for lines matching '^I samsvar med det nasjonale kvalifikasjo... |
| ✓ | hivolda | assessment | formatting_noise | low | widespread | hivolda_ENG144-1_2022_spring_1; hivolda_JOP100-1_2025_aut... | Drop the fixed header row when flattening the HVO exam table (or reformat rows as labelled fields), so only data rows remain. |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_3JUS330-1_2023_autumn_1; inn_ØKA1026-1_2025_spring_1;... | Drop the table header line and the table data row (or keep only the exam form and grade scale) in .clean_sections() for INN assessment, a... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_REGF2009-1_2025_autumn_1; inn_KS4102-1_2023_autumn_1;... | Keep the sub-heading lines as text inside learning_outcomes (Kunnskap / Ferdigheter / Generell kompetanse and the English equivalents). |
| ✓ | nmbu | teaching_methods | missing_section | low | widespread | nmbu_FYS100-1_2025_spring_1; nmbu_PPFD201-1_2025_autumn_2 | Map `Undervisningstider` and `Læringsstøtte` to teaching_methods in R/section_heading_map.R (optionally scoped to NMBU), after checking t... |
| ✓ | uit | teaching_methods | formatting_noise | low | widespread | uit_LER-3211-1_2024_spring_1; uit_LRU-1502-1_2012_autumn_... | Map 'Kvalitetssikring (av emnet)' and 'Undervisnings- og eksamensspråk' to .drop in R/section_heading_map.R, and strip the sentence 'For ... |
| ✓ | hvl | teaching_methods | formatting_noise | low | common | hvl_NHB2-1_2019_spring_1; hvl_FK215-1_2022_autumn_1 | Add HVL-specific drop rules in .clean_sections()/.drop for lines on 'Kr \d+', 'materialpenger', 'egenandel', 'Eksamen blir avholdt' and '... |
| ✓ | mf | coursework_requirements | formatting_noise | low | common | mf_RL1010L-1_2025_autumn_2; mf_RL1014-1_2025_autumn_1 | In .clean_sections() for mf, strip the paragraph starting 'Studenter som ikke oppfyller de obligatoriske aktivitetene' and the 'Ved oppme... |
| ✓ | nla | teaching_methods | empty_placeholder | low | common | nla_4HSM211-1_2019_autumn_1; nla_GL17KRØ1-1_2017_autumn_1... | In .clean_sections() in R/extract_sections.R, drop lines that match ^(ingen\|ingen\.\|-\|nei)$ (case-insensitive) when they are the whole te... |
| ✓ | nla | teaching_methods | wrong_content | low | common | nla_MGL1KØ101-1_2025_autumn_1 | In .clean_sections(), drop paragraphs that consist only of a reference ('^(se\|sjå\|see)\b.*(praksis\|plan)') and keep 'Praksis' text otherw... |
| ✓ | nord | prerequisites | boilerplate_only | low | common | nord_IKT1013-1_2019_autumn_2; nord_ORG5011-1_2022_spring_... | Add admission/opptakskrav phrases ('oppfylle gjeldende opptakskrav', 'tatt opp som student', 'Se informasjon om opptakskrav', 'ingen fork... |
| ✓ | nord | assessment | formatting_noise | low | common | nord_LED5018-1_2022_autumn_2; nord_ENT5011-1_2022_autumn_... | Strip the heading line 'Hjelpemidler til eksamen' (and optionally drop the aids paragraph per spec), and add the 'Det tas forbehold om en... |
| ✓ | ntnu | assessment | formatting_noise | low | common | ntnu_MET420-A_2025_spring_1; ntnu_TS200114-1_2022_autumn_1 | In .clean_sections(), drop assessment lines matching 'Hjelpemidler til eksamen', 'oppmeldingsfrist', 'Eksamenssystem' and calculator mode... |
| ✓ | oslomet | assessment | boilerplate_only | low | common | oslomet_PVUB7000-1_2019_autumn_1; oslomet_BLH3140-1_2019_... | Map 'Skikkethetsvurdering' to .drop and add a .clean_sections() line filter for 'ny/utsatt eksamen' regulation sentences. |
| ✓ | oslomet | prerequisites | empty_placeholder | low | common | oslomet_CORO7100-2_2018_autumn_1; oslomet_MSLV4500-1_2018... | Add 'Ingen info' to the placeholder patterns so the row is not emitted (also trailing 'Ingen info' in assessment). |
| ✓ | uia | teaching_methods | other | low | common | uia_NAT115-1_2022_autumn_1; uia_ERN403-1_2025_autumn_1; u... | Decide one target for 'Faget i praksis' (teaching_methods per the current map is reasonable) and make the UiA reader recognise it also as... |
| ✓ | uib | prerequisites | empty_placeholder | low | common | uib_NOLI103-L-0_2017_spring_1; uib_NOLI250-L-0_2017_spring_1 | Strip placeholder-only blocks ('Ingen', 'Ingen.', '-') before joining blocks of the same section in .clean_sections(). |
| ✓ | uis | course_content | duplicate | low | common | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1;... | In .clean_sections(), drop a paragraph in course_content that is a verbatim prefix of, or contained in, another paragraph of the same sec... |
| ✓ | uit | reading_list | boilerplate_only | low | common | uit_LER-3211-1_2024_spring_1; uit_LER-3101-1_2025_spring_1 | Add 'Du kan se og få tilgang til deler av pensum via Leganto' and '^Pensumliste for ' to the boilerplate-strip list so such rows become e... |
| ✓ | uit | coursework_requirements | formatting_noise | low | common | uit_LER-1123-1_2023_spring_1; uit_LER-1104-1_2024_autumn_1 | In .clean_sections() remove '\tKarakterregel:' tails and standalone 'Godkjent – ikke godkjent' lines for coursework_requirements. |
| ✓ | usn | coursework_requirements | formatting_noise | low | common | usn_MG2PE3-1_2021_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Configure the USN section_* fields so headings are read from the page, or have the fallback drop a line that exactly matches a heading al... |
| ✓ | usn | teaching_methods | merged_sections | low | common | usn_MG2RL3-1_2019_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Add 'Praksis' to the heading map as teaching_methods (strip the heading line), or treat it as a sub-heading of Læringsaktiviteter. |
| ✓ | hiof | reading_list | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2021_autumn_1; hiof_LMBNOR10417-1_2020... | In .clean_sections(), broaden the reading_list pattern to '^\s*Litteratur\w*(?:\s+er)?\s+sist\s+oppdater\w*[^\n]*\s*' (case-insensitive).... |
| ✓ | hiof | assessment | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2024_autumn_1; hiof_LMBNOR10417-1_2023... | Add a hiof entry to .section_noise in R/extract_sections.R that removes whole lines only: '(?m)^(?:Oppgaven vurderes av (?:en )?ekstern o... |
| ✓ | hvl | assessment | formatting_noise | low | occasional | hvl_ØAM112-1_2019_autumn_1; hvl_NHB2-1_2019_autumn_1; hvl... | Drop lines matching 'Tid (og sted )?for (eksamen\|ut- og innlevering)' and 'opplyst på (Studentweb\|emnets sider)' in assessment cleaning. |
| ✓ | hvl | all | empty_placeholder | low | occasional | hvl_Å-MU2A-1_2019_spring_3; hvl_ØKB3119-1_2017_autumn_1; ... | Treat '_', '-' and bare 'Ja'/'Nei' as empty in the placeholder filter (keep the longer 'Ja, vert spesifisert' variant as a short requirem... |
| ✓ | inn | teaching_methods | other | low | occasional | inn_LAN2300-1_2025_autumn_1; inn_2MKRLE171-6-1_2023_spring_1 | Map the heading 'Emneevaluering – kvalitetssystem' to .drop in R/section_heading_map.R; consider dropping the generic Praksisstudiet para... |
| ✓ | nih | prerequisites | empty_placeholder | low | occasional | nih_LKI130-1_2025_autumn_1; nih_LKI110-1_2025_autumn_1; n... | Make the placeholder rule in R/extract_sections.R (.placeholder_phrases) match '^Ingen( emner)?( i programmet)?\.?$' so all variants are ... |
| ✓ | nla | course_content | formatting_noise | low | occasional | nla_3GJ302-1_2025_autumn_1; nla_GL17NO2-1_2017_autumn_1 | Insert a space or newline when joining adjacent inline or block nodes in the reader (R/blocks.R), or add a regex after extraction that sp... |
| ✓ | nla | prerequisites | boilerplate_only | low | occasional | nla_3GJ302-1_2025_autumn_1 | Add an English pattern such as '^view the admission requirements' (and 'admission requirements for') to the prerequisites boilerplate fil... |
| ✓ | ntnu | prerequisites | boilerplate_only | low | occasional | ntnu_LGU14008-A_2019_autumn_1; ntnu_PPU4725-1_2007_autumn_1 | In .clean_sections(), drop sentences in prerequisites matching admission patterns ('plasser er forbeholdt', '^Opptak til', 'Krever opptak... |
| ✓ | oslomet | prerequisites | boilerplate_only | low | occasional | oslomet_PVUB7000-1_2019_autumn_1; oslomet_PVUB7000-1_2019... | Optionally drop paragraphs starting with 'Opptakskrav' / mentioning 'prioritert ved opptak' / 'generell studiekompetanse' in prerequisites. |
| ✓ | steiner | assessment | formatting_noise | low | occasional | steiner_M-MAT1_1_2025_spring_1; steiner_M-MAT1_2_2025_spr... | In .clean_sections() (R/extract_sections.R) drop lines matching ^\d{1,3}$ at the end of a section, and drop the Hjelpemidler/Sensorordnin... |
| ✓ | uis | course_content | boilerplate_only | low | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | Add exact '.drop' rows for 'arbeidsmengd' and 'arbeidsmengde' in R/section_heading_map.R (or move the hours line to teaching_methods). |
| ✓ | uis | prerequisites | empty_placeholder | low | occasional | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1 | Strip standalone 'Ingen'/'Ingen.' lines when other text is present in the section, and drop the sub-heading label line. |
| ✓ | uit | all | other | low | occasional | uit_SAM-3940-1_2011_autumn_1; uit_FRA-3903-1_2011_autumn_... | No extractor change; optionally exclude plans whose full text is under ~300 chars from section-level analysis so they are not counted as ... |
| ✓ | usn | coursework_requirements | wrong_content | low | occasional | usn_MG2RL3-1_2019_spring_2; usn_MG1RL2-1_2019_autumn_2 | Map '^Utgifter i emnet' to .drop in R/section_heading_map.R. |
| ✓ | hivolda | reading_list | empty_placeholder | low | rare | hivolda_PPY121-1_2016_autumn_1 | Extend the reading_list placeholder pattern to cover '^Sjå pensumliste for emnet her$'. |
| ✓ | hivolda | prerequisites | empty_placeholder | low | rare | hivolda_DRHS902-1_2019_spring_1 | Treat '^Ingen\.? (Sjå\|Se) opptakskrav' as a placeholder in prerequisites cleaning. |
| ✓ | hvl | prerequisites | boilerplate_only | low | rare | hvl_ØAM112-1_2019_autumn_1 | Drop prerequisites lines equal to 'Studiets opptakskrav' / 'Opptakskrav' (also 'Studierett på Master ...' type lines if the spec treats t... |
| ✓ | ntnu | course_content | duplicate | low | rare | ntnu_MET420-A_2025_spring_1 | Add exact-duplicate paragraph removal within a section in .clean_sections(). Optionally add the 'Tar forbehold om endringer' disclaimer t... |
| ✓ | oslomet | course_content | boilerplate_only | low | rare | oslomet_TOLKDIAPBT-1_2018_autumn_1 | Add a line filter for 'Vedtatt i ... Godkjent av rektor' in .clean_sections(). |
| ✓ | uia | course_content | other | low | rare | uia_NO-157-1_2022_autumn_1 | Add 'Innhaldsliste' to the .drop pattern and place it before the course_content pattern (or anchor the Innhald pattern with $). |
| ✓ | uia | assessment | wrong_content | low | rare | uia_PRA044-1_2024_autumn_1 | Low priority: optionally move sentences matching 'må ... være oppfylt\|fremmøte\|tilstedeværelse' from assessment to coursework_requirement... |
| ✓ | uib | assessment | truncated | low | rare | uib_RELV105-0_2021_spring_1 | Find the rule that removes these sentences (grep for 'korona' in R/extract_sections.R and R/anonymize*.R); either keep the text or remove... |
| ✓ | uio | course_content | other | low | rare | uio_HIS4015L-1_2025_spring_1 | Require the leak heading match to be a whole line, or word-bounded, in R/audit/qa_sections.R. No pipeline change needed. |
| ✓ | uit | reading_list | empty_placeholder | low | rare | uit_SOA-3905-1_2010_spring_1 | Add '^se hjemmeside\.?$' (and similar 'Se emnets hjemmeside') to the placeholder patterns so the row is not emitted. |
