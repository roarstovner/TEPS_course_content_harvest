# sections audit — aggregated findings

65 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: opus, sonnet).
Mechanical verification: 65 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 7 | hiof, hivolda, inn, nord, ntnu, steiner, uio | medium |
| assessment | wrong_content | 5 | hvl, nord, uia, uio, uit | medium |
| prerequisites | empty_placeholder | 4 | nih, nord, uib, uis | low |
| prerequisites | missing_section | 4 | mf, nla, nord, uit | medium |
| coursework_requirements | formatting_noise | 3 | mf, uit, usn | low |
| teaching_methods | wrong_content | 3 | nla, nord, ntnu | low |
| reading_list | formatting_noise | 3 | hiof, uis, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| nord | 6 | 6 | course_content / truncated; prerequisites / empty_placeholder; teaching_methods / wrong_content | assessment / wrong_content; prerequisites / missing_section; assessment / formatting_noise | cross_section / wrong_content; coursework_requirements / split_section; prerequisites / boilerplate_only |
| uib | 4 | 3 | course_content / field_or_language_junk; prerequisites / empty_placeholder | assessment / truncated | assessment / formatting_noise; cross_section / truncated |
| uio | 5 | 4 | assessment / formatting_noise; coursework_requirements / wrong_content | assessment / wrong_content; course_content / other | teaching_methods / wrong_content; coursework_requirements / merged_sections; learning_outcomes / duplicate |
| uit | 10 | 8 | prerequisites / missing_section; teaching_methods / formatting_noise; reading_list / boilerplate_only | assessment / boilerplate_only; assessment / wrong_content; reading_list / empty_placeholder; coursework_requirements / formatting_noise; all / other | all / missing_section; cross_section / merged_sections; assessment / merged_sections; course_content / wrong_content; assessment / formatting_noise; coursework_requirements / wrong_content; assessment / duplicate |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 13 |
| empty_placeholder | low | 7 |
| missing_section | medium | 7 |
| wrong_content | low | 6 |
| formatting_noise | medium | 5 |
| wrong_content | medium | 5 |
| boilerplate_only | low | 4 |
| other | low | 4 |
| merged_sections | medium | 2 |
| missing_section | low | 2 |
| truncated | medium | 2 |
| boilerplate_only | medium | 1 |
| duplicate | low | 1 |
| field_or_language_junk | medium | 1 |
| merged_sections | low | 1 |
| missing_section | high | 1 |
| other | medium | 1 |
| truncated | high | 1 |
| truncated | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 16 |
| prerequisites | 10 |
| teaching_methods | 10 |
| course_content | 7 |
| reading_list | 7 |
| coursework_requirements | 6 |
| all | 4 |
| learning_outcomes | 3 |
| cross_section | 2 |

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
| nord | sonnet | 20 | Section extraction works well for Nord: headings are consistent and the codebook sections land in the right place, including OD/AK moved to coursework_requirements. Remaining issues are leftover gate fragments in assessment, dropped intro paragraphs, and Adgangsregulering (unmapped) sometimes hol... |
| ntnu | sonnet | 19 | Section extraction works well for NTNU: headings are read and mapped correctly, learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, and all 14 suspects plus the 5 controls land in the right sections. The 'boilerplate' flags are false alarms (all 14 suspects are PPU Fagdida... |
| oslomet | sonnet | 17 | Where sections are extracted, the section split is good: learning outcomes, arbeidskrav and assessment land correctly and the leak-> flags are false alarms. The main defect is that the 4th-year/praksis course family (M5GP*) yields no sections at all although the headings were read and mapped. Sma... |
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
| ✓ | oslomet | all | missing_section | high | common | oslomet_M5GP4000-1_2025_autumn_2; oslomet_M5GP2000-1_2024... | Debug sectionize() for M5GP4000-1 interactively with tar_make(callr_function = NULL) and find which step empties the table. Then fix the ... |
| ✓ | inn | learning_outcomes | truncated | high | rare | inn_2MNK172S-2-1_2022_spring_1 | Add 'Ferdighetsmål' (and nynorsk variants 'Ferdigheitsmål', 'Kunnskapsmål', 'Generell kompetanse') as sub-headings mapped to learning_out... |
| ✓ | mf | reading_list | missing_section | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1; mf_... | Check the MF page source for where the literature list is rendered and extend the MF fulltext/section_* selector to include it; otherwise... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_RELDID4009-1_2025_sprin... | Map `Karakterskala` to .drop in R/section_heading_map.R like the sibling Eksamensspråk, Hjelpemidler and Adgang til ny eller utsatt eksam... |
| ✓ | usn | reading_list | formatting_noise | medium | widespread | usn_MG1PE3-1_2021_spring_2; usn_MG2RL3-1_2019_spring_2; u... | Add a USN reading-list cleanup that strips the lines '^Click to view interactive reading list in Leganto' (and splits the glued section h... |
| ✓ | nord | assessment | wrong_content | medium | common | nord_SAM1007-1_2020_autumn_2; nord_MAT5006-1_2022_spring_... | In the coursework-gate mover (see #283) move the whole OD/AK item up to the next item label (Oppgave (OP), Sammensatt vurdering, Hjemmeek... |
| ✓ | nord | course_content | truncated | medium | common | nord_ENG5003-1_2022_spring_2; nord_MAT5006-1_2022_spring_... | For nord, treat the paragraph(s) between the course code line and the first heading as course_content (prepend to the Beskrivelse section... |
| ✓ | nord | prerequisites | missing_section | medium | common | nord_SPD5002-1_2022_spring_2; nord_ENG5003-1_2022_spring_... | Map 'Adgangsregulering' to prerequisites only as a fallback when Forkunnskapskrav is empty or '.', or keep its non-admission sentences (c... |
| ✓ | ntnu | assessment | truncated | medium | common | ntnu_PPU4625-1_2023_spring_1; ntnu_PPU4681-1_2022_spring_1 | Check the block reader for NTNU Eksamen with multiple 'Vurderingsordning:' headings: emit the 'Vurderingsordning: X / Karakter: Y' lines ... |
| ✓ | oslomet | teaching_methods | missing_section | medium | common | oslomet_M5GSF2110-1_2022_spring_2; oslomet_M5GKP2100-1_20... | Add patterns (optionally prefixed 'Fagets ') to R/section_heading_map.R: '^Fagets arbeids- og undervisningsformer' -> teaching_methods, '... |
| ✓ | uia | all | missing_section | medium | common | uia_PED428-1_2023_autumn_1; uia_IDR418-1_2023_autumn_1; u... | Flag plans whose extracted text has no mapped content heading (only .drop/unmapped headings) as 'no_plan_content' in the fulltext/harvest... |
| ✓ | uio | coursework_requirements | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_KJM5050-1_2025_spring_1 | Add 'Obligatoriske (forhold\|komponenter)' patterns mapped to coursework_requirements in R/section_heading_map.R, and let the uio section_... |
| ✓ | uis | cross_section | formatting_noise | medium | common | uis_LENG116-1_2015_autumn_1; uis_LFYMAS-1_2017_spring_1; ... | For uis, strip lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$' and lines equal to '<emnekode> - <titl... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In the uis cleanup, remove tokens matching 'https?://bibsys-ur\.userservices\.exlibrisgroup\.com/\S*(?:\s*\n?\S*rft[._]\S*)*' up to 'View... |
| ✓ | uit | prerequisites | missing_section | medium | common | uit_LER-3211-1_2024_spring_1; uit_LER-1123-1_2023_spring_... | Add a line-start split for 'Forkunnskapskrav' within the Om emnet block for uit (or let the reader treat that label as a sub-heading) so ... |
| ✓ | uit | prerequisites | missing_section | medium | common | uit_LER-3211-1_2024_spring_1; uit_LRU-1271-1_2012_spring_... | For uit, map 'Opptakskrav' to prerequisites when the text names a course/credit/previous-year requirement, or drop only when it is pure a... |
| ✓ | uit | assessment | boilerplate_only | medium | common | uit_LER-3211-1_2024_spring_1; uit_LER-1123-1_2023_spring_... | Keep the exam form and duration but strip empty 'Dato:' fields and 'Karakterregel:' tails in .clean_sections() for uit, or mark the row a... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern '^Forkunnskapskrav\b:?' mapped to prerequisites in R/section_heading_map.R, with the rest of the line kept as se... |
| ✓ | oslomet | cross_section | formatting_noise | medium | occasional | oslomet_MGVM4100-1_2022_autumn_1; oslomet_M5GNO1300-1_202... | Inspect the raw HTML of these two courses to find the source of ';'. Strip a ';' that is directly adjacent to a word boundary or at end o... |
| ✓ | uio | assessment | wrong_content | medium | occasional | uio_KJM5050-1_2025_spring_1; uio_KJM5930L-2_2025_spring_1 | Extend the existing whole-item coursework-gate move (#283) to UiO assessment, with patterns such as 'arbeidskrav', 'obligatoriske aktivit... |
| ✓ | uit | assessment | wrong_content | medium | occasional | uit_LRU-1271-1_2012_spring_1 | In the uit Eksamen block, extend the arbeidskrav chunk until the next line-start 'Eksamen og vurdering' / 'Eksamen består av' instead of ... |
| ✓ | hvl | reading_list | wrong_content | medium | rare | hvl_MGBPØ201-1_2023_spring_1 | Add a line-start rule in sectionize()/.clean_sections() that moves lines beginning with 'Anbefalt litteratur' / 'Pensum' / 'Litteratur:' ... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Add a line-start pattern for '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R (and make sure th... |
| ✓ | oslomet | learning_outcomes | merged_sections | medium | rare | oslomet_M5GKP2100-1_2019_spring_2 | In .clean_sections() for OsloMet, cut learning_outcomes at the first line starting 'Etter fullført emne har studenten', and move the prec... |
| ✓ | steiner | all | other | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Check the URL built for M-PEL2.2 in R/add_course_url.R and compare the emnekode in the page text with course_code; flag mismatches in the... |
| ✓ | uib | course_content | field_or_language_junk | medium | rare | uib_NOLI216-0_2018_autumn_2 | In sectionize()/.clean_sections() drop any section whose lines are all (after trimming) recognised headings from the heading map (or the ... |
| ✓ | hivolda | assessment | formatting_noise | low | widespread | hivolda_MGL1-7MA2B-1_2022_spring_1; hivolda_MGL5-10SA1B-1... | Emit table cells with a separator (e.g. ' \| ') for hivolda's assessment table in R/blocks.R, and optionally strip the fixed grade-scale p... |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_2MSF171S-2-1_2022_spring_1; inn_2MNK172S-2-1_2022_spr... | Read the exam table as a table block and drop it (or store it as separate structured columns) when building the assessment section, or st... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_2MEN171S-1-1_2024_spring_1; inn_2MNF5101-4-1_2022_spr... | Keep the sub-heading lines as text inside learning_outcomes (or a consistent marker) instead of dropping them. |
| ✓ | nla | teaching_methods | wrong_content | low | widespread | nla_MGL1PE201-1_2025_autumn_1; nla_4MGL1MA101-1_2025_autu... | Map 'Praksis' to .drop (or to unmapped) for NLA, or only keep it when it has more than a 'Se/Sjå ... praksisplan/studieplan' reference (s... |
| ✓ | nmbu | teaching_methods | missing_section | low | widespread | nmbu_FYS100-1_2025_spring_1; nmbu_PPFD201-1_2025_autumn_2 | Map `Undervisningstider` and `Læringsstøtte` to teaching_methods in R/section_heading_map.R (optionally scoped to NMBU), after checking t... |
| ✓ | uit | teaching_methods | formatting_noise | low | widespread | uit_LER-3211-1_2024_spring_1; uit_LRU-1502-1_2012_autumn_... | Map 'Kvalitetssikring (av emnet)' and 'Undervisnings- og eksamensspråk' to .drop in R/section_heading_map.R, and strip the sentence 'For ... |
| ✓ | inn | teaching_methods | boilerplate_only | low | common | inn_2MSF171S-2-1_2022_spring_1; inn_2MEN171S-1-1_2024_spr... | Map a standalone 'Praksis' heading to .drop (or strip the shared paragraph in .clean_sections()), keeping it only if course-specific prac... |
| ✓ | mf | coursework_requirements | formatting_noise | low | common | mf_RL1010L-1_2025_autumn_2; mf_RL1014-1_2025_autumn_1 | In .clean_sections() for mf, strip the paragraph starting 'Studenter som ikke oppfyller de obligatoriske aktivitetene' and the 'Ved oppme... |
| ✓ | nord | teaching_methods | wrong_content | low | common | nord_KRO1004-1_2022_autumn_1; nord_SAM1007-1_2020_autumn_... | Strip 'ingen praksis' and 'se egne emneplaner' placeholders from the Praksis block. Move or flag attendance/obligatory text to coursework... |
| ✓ | ntnu | teaching_methods | wrong_content | low | common | ntnu_PPU4625-1_2023_spring_1; ntnu_SOS2020-1_2017_autumn_1 | Accept as source-level; optionally document in data notes. Not worth a heading-map change. |
| ✓ | uia | teaching_methods | other | low | common | uia_NAT115-1_2022_autumn_1; uia_ERN403-1_2025_autumn_1; u... | Decide one target for 'Faget i praksis' (teaching_methods per the current map is reasonable) and make the UiA reader recognise it also as... |
| ✓ | uib | prerequisites | empty_placeholder | low | common | uib_NOLI103-L-0_2017_spring_1; uib_NOLI250-L-0_2017_spring_1 | Strip placeholder-only blocks ('Ingen', 'Ingen.', '-') before joining blocks of the same section in .clean_sections(). |
| ✓ | uis | course_content | duplicate | low | common | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1;... | In .clean_sections(), drop a paragraph in course_content that is a verbatim prefix of, or contained in, another paragraph of the same sec... |
| ✓ | uit | reading_list | boilerplate_only | low | common | uit_LER-3211-1_2024_spring_1; uit_LER-3101-1_2025_spring_1 | Add 'Du kan se og få tilgang til deler av pensum via Leganto' and '^Pensumliste for ' to the boilerplate-strip list so such rows become e... |
| ✓ | uit | coursework_requirements | formatting_noise | low | common | uit_LER-1123-1_2023_spring_1; uit_LER-1104-1_2024_autumn_1 | In .clean_sections() remove '\tKarakterregel:' tails and standalone 'Godkjent – ikke godkjent' lines for coursework_requirements. |
| ✓ | usn | coursework_requirements | formatting_noise | low | common | usn_MG2PE3-1_2021_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Configure the USN section_* fields so headings are read from the page, or have the fallback drop a line that exactly matches a heading al... |
| ✓ | usn | teaching_methods | merged_sections | low | common | usn_MG2RL3-1_2019_spring_2; usn_MG2NA2-1_2022_autumn_1; u... | Add 'Praksis' to the heading map as teaching_methods (strip the heading line), or treat it as a sub-heading of Læringsaktiviteter. |
| ✓ | hiof | reading_list | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2021_autumn_1; hiof_LMBNOR10417-1_2020... | In .clean_sections(), broaden the reading_list pattern to '^\s*Litteratur\w*(?:\s+er)?\s+sist\s+oppdater\w*[^\n]*\s*' (case-insensitive).... |
| ✓ | hiof | assessment | formatting_noise | low | occasional | hiof_LMBNOR10417-1_2024_autumn_1; hiof_LMBNOR10417-1_2023... | Add a hiof entry to .section_noise in R/extract_sections.R that removes whole lines only: '(?m)^(?:Oppgaven vurderes av (?:en )?ekstern o... |
| ✓ | hvl | assessment | wrong_content | low | occasional | hvl_MGBPØ201-1_2023_spring_1 | For assessment blocks whose text starts with 'Obligatorisk oppmøte', move that line to coursework_requirements; or leave as is since it o... |
| ✓ | nih | prerequisites | empty_placeholder | low | occasional | nih_LKI130-1_2025_autumn_1; nih_LKI110-1_2025_autumn_1; n... | Make the placeholder rule in R/extract_sections.R (.placeholder_phrases) match '^Ingen( emner)?( i programmet)?\.?$' so all variants are ... |
| ✓ | nord | prerequisites | empty_placeholder | low | occasional | nord_MAT5006-1_2022_spring_2; nord_MAT5003-1_2022_spring_2 | Drop blocks that are only punctuation ('.', '-', 'Ingen') before concatenating blocks within a section. |
| ✓ | oslomet | prerequisites | boilerplate_only | low | occasional | oslomet_MGMH2100-1_2019_spring_2; oslomet_M1KP4200-1_2025... | Drop prerequisites that match '^Se (programplanen\|fagplanen)' and treat 'Eksterne søkere' paragraphs after a dropped Opptakskrav heading ... |
| ✓ | oslomet | coursework_requirements | empty_placeholder | low | occasional | oslomet_MGVM4100-1_2020_autumn_2; oslomet_MGVM4100-1_2023... | Treat '^Ingen( arbeidskrav)?\.?$' and '^Se (fag\|program)planen\.?$' as empty and drop them, or keep them consistently and document it. Th... |
| ✓ | steiner | assessment | formatting_noise | low | occasional | steiner_M-MAT1_1_2025_spring_1; steiner_M-MAT1_2_2025_spr... | In .clean_sections() (R/extract_sections.R) drop lines matching ^\d{1,3}$ at the end of a section, and drop the Hjelpemidler/Sensorordnin... |
| ✓ | uis | course_content | boilerplate_only | low | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | Add exact '.drop' rows for 'arbeidsmengd' and 'arbeidsmengde' in R/section_heading_map.R (or move the hours line to teaching_methods). |
| ✓ | uis | prerequisites | empty_placeholder | low | occasional | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1 | Strip standalone 'Ingen'/'Ingen.' lines when other text is present in the section, and drop the sub-heading label line. |
| ✓ | uit | all | other | low | occasional | uit_SAM-3940-1_2011_autumn_1; uit_FRA-3903-1_2011_autumn_... | No extractor change; optionally exclude plans whose full text is under ~300 chars from section-level analysis so they are not counted as ... |
| ✓ | usn | coursework_requirements | wrong_content | low | occasional | usn_MG2RL3-1_2019_spring_2; usn_MG1RL2-1_2019_autumn_2 | Map '^Utgifter i emnet' to .drop in R/section_heading_map.R. |
| ✓ | hvl | assessment | empty_placeholder | low | rare | hvl_MGBPØ401-1_2021_spring_1; hvl_MGBPØ201-1_2021_spring_1 | Add 'Det er ingen vurdering i emnet' to the placeholder patterns so the row is dropped or flagged; low priority. |
| ✓ | nla | prerequisites | missing_section | low | rare | nla_4MGL1NO-M-1_2025_spring_1; nla_4MGL5FOU201-1_2025_aut... | Add a pattern '^progresjonskrav' mapped to prerequisites in R/section_heading_map.R. |
| ✓ | nord | assessment | formatting_noise | low | rare | nord_SPD5002-1_2025_autumn_1 | Add 'Selvvalgt litteratur' to the heading map as reading_list, or drop it as a label. |
| ✓ | ntnu | assessment | formatting_noise | low | rare | ntnu_HIST2045-1_2021_spring_1 | Map 'Ny eksamen -' to .drop like the other exam-date sub-headings (or strip the 'Vekting ... Eksamenssystem ...' row). |
| ✓ | oslomet | course_content | formatting_noise | low | rare | oslomet_MGPE3300-1_2020_spring_2 | Check the encoding of the raw HTML; map '¿' to an en dash in text cleanup for OsloMet if the raw page really contains it. |
| ✓ | uia | course_content | other | low | rare | uia_NO-157-1_2022_autumn_1 | Add 'Innhaldsliste' to the .drop pattern and place it before the course_content pattern (or anchor the Innhald pattern with $). |
| ✓ | uia | assessment | wrong_content | low | rare | uia_PRA044-1_2024_autumn_1 | Low priority: optionally move sentences matching 'må ... være oppfylt\|fremmøte\|tilstedeværelse' from assessment to coursework_requirement... |
| ✓ | uib | assessment | truncated | low | rare | uib_RELV105-0_2021_spring_1 | Find the rule that removes these sentences (grep for 'korona' in R/extract_sections.R and R/anonymize*.R); either keep the text or remove... |
| ✓ | uio | course_content | other | low | rare | uio_HIS4015L-1_2025_spring_1 | Require the leak heading match to be a whole line, or word-bounded, in R/audit/qa_sections.R. No pipeline change needed. |
| ✓ | uit | reading_list | empty_placeholder | low | rare | uit_SOA-3905-1_2010_spring_1 | Add '^se hjemmeside\.?$' (and similar 'Se emnets hjemmeside') to the placeholder patterns so the row is not emitted. |
