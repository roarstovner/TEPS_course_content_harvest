# sections audit — aggregated findings

88 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: opus, sonnet).
Mechanical verification: 76 passed (✓), 0 failed (✗), 12 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 8 | hivolda, hvl, inn, ntnu, oslomet, uib, uis, usn | medium |
| learning_outcomes | formatting_noise | 5 | hvl, inn, nmbu, nord, uia | low |
| assessment | wrong_content | 4 | hivolda, ntnu, uib, uit | high |
| teaching_methods | wrong_content | 4 | nord, ntnu, oslomet, uit | high |
| teaching_methods | merged_sections | 3 | inn, mf, ntnu | high |
| assessment | truncated | 3 | nord, uib, uio | low |
| reading_list | empty_placeholder | 3 | hiof, ntnu, usn | low |
| course_content | formatting_noise | 3 | oslomet, uib, uit | medium |
| cross_section | formatting_noise | 3 | steiner, uis, usn | medium |
| teaching_methods | missing_section | 3 | mf, nmbu, oslomet | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 3 | 5 |  | assessment / other; learning_outcomes / other; teaching_methods / truncated; reading_list / formatting_noise; reading_list / empty_placeholder | reading_list / boilerplate_only; prerequisites / empty_placeholder; learning_outcomes / formatting_noise |
| hivolda | 7 | 2 | assessment / wrong_content | assessment / formatting_noise | reading_list / wrong_content; cross_section / merged_sections; teaching_methods / missing_section; coursework_requirements / merged_sections; assessment / missing_section; course_content / wrong_content |
| hvl | 2 | 3 | assessment / formatting_noise | learning_outcomes / formatting_noise; coursework_requirements / empty_placeholder | all / empty_placeholder |
| inn | 5 | 3 | assessment / formatting_noise | learning_outcomes / formatting_noise; teaching_methods / merged_sections | learning_outcomes / truncated; assessment / field_or_language_junk; assessment / truncated; reading_list / empty_placeholder |
| mf | 12 | 4 | reading_list / boilerplate_only; teaching_methods / missing_section | teaching_methods / merged_sections; prerequisites / merged_sections | coursework_requirements / wrong_content; course_content / empty_placeholder; course_content / missing_section; reading_list / formatting_noise; learning_outcomes / truncated; learning_outcomes / wrong_content; reading_list / wrong_content; learning_outcomes / formatting_noise; assessment / formatting_noise; prerequisites / missing_section |
| nih | 3 | 2 | reading_list / boilerplate_only | prerequisites / missing_section | prerequisites / empty_placeholder; course_content / formatting_noise |
| nla | 4 | 3 |  | prerequisites / missing_section; learning_outcomes / truncated; coursework_requirements / formatting_noise | teaching_methods / missing_section; reading_list / boilerplate_only; prerequisites / empty_placeholder; assessment / formatting_noise |
| nmbu | 4 | 2 |  | teaching_methods / missing_section; learning_outcomes / formatting_noise | prerequisites / formatting_noise; prerequisites / boilerplate_only; coursework_requirements / missing_section; assessment / formatting_noise |
| nord | 7 | 10 | teaching_methods / wrong_content; all / other | assessment / missing_section; cross_section / split_section; course_content / truncated; prerequisites / truncated; assessment / truncated; coursework_requirements / truncated; prerequisites / empty_placeholder; learning_outcomes / formatting_noise | assessment / merged_sections; assessment / formatting_noise; assessment / boilerplate_only; prerequisites / boilerplate_only; reading_list / missing_section |
| ntnu | 8 | 8 | assessment / formatting_noise; teaching_methods / merged_sections; assessment / wrong_content; prerequisites / boilerplate_only | assessment / other; assessment / duplicate; teaching_methods / wrong_content; reading_list / empty_placeholder | reading_list / missing_section; assessment / missing_section; teaching_methods / boilerplate_only; learning_outcomes / truncated |
| oslomet | 7 | 5 | all / missing_section; assessment / formatting_noise | teaching_methods / missing_section; teaching_methods / wrong_content; course_content / formatting_noise | all / empty_placeholder; assessment / duplicate; cross_section / missing_section; reading_list / missing_section; prerequisites / boilerplate_only |
| steiner | 7 | 1 | cross_section / formatting_noise |  | learning_outcomes / formatting_noise; learning_outcomes / truncated; teaching_methods / wrong_content; assessment / truncated; assessment / formatting_noise; coursework_requirements / formatting_noise |
| uia | 4 | 4 | cross_section / merged_sections; all / missing_section; coursework_requirements / wrong_content | learning_outcomes / formatting_noise | prerequisites / wrong_content |
| uib | 6 | 5 | course_content / formatting_noise; assessment / formatting_noise; cross_section / other | assessment / wrong_content; assessment / truncated | assessment / boilerplate_only; prerequisites / boilerplate_only; assessment / duplicate |
| uio | 5 | 5 | coursework_requirements / missing_section | reading_list / merged_sections; coursework_requirements / wrong_content; assessment / field_or_language_junk; assessment / truncated | reading_list / missing_section; assessment / boilerplate_only; prerequisites / boilerplate_only; teaching_methods / other |
| uis | 12 | 7 | reading_list / split_section; cross_section / merged_sections; cross_section / formatting_noise | cross_section / wrong_content; teaching_methods / truncated; course_content / truncated; assessment / formatting_noise | learning_outcomes / merged_sections; reading_list / wrong_content; learning_outcomes / truncated; all / missing_section; cross_section / other; coursework_requirements / other; reading_list / formatting_noise; learning_outcomes / split_section; assessment / wrong_content |
| usn | 5 | 5 |  | cross_section / formatting_noise; learning_outcomes / other; assessment / formatting_noise; reading_list / empty_placeholder; reading_list / formatting_noise | learning_outcomes / merged_sections; reading_list / boilerplate_only; assessment / wrong_content; teaching_methods / formatting_noise; coursework_requirements / formatting_noise |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 16 |
| formatting_noise | medium | 9 |
| merged_sections | high | 6 |
| truncated | low | 6 |
| empty_placeholder | low | 5 |
| truncated | medium | 5 |
| wrong_content | low | 5 |
| wrong_content | medium | 5 |
| merged_sections | medium | 4 |
| missing_section | high | 4 |
| missing_section | low | 4 |
| other | low | 4 |
| wrong_content | high | 3 |
| missing_section | medium | 2 |
| other | medium | 2 |
| boilerplate_only | high | 1 |
| boilerplate_only | low | 1 |
| boilerplate_only | medium | 1 |
| duplicate | medium | 1 |
| field_or_language_junk | medium | 1 |
| merged_sections | low | 1 |
| split_section | high | 1 |
| split_section | medium | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 22 |
| teaching_methods | 12 |
| cross_section | 10 |
| learning_outcomes | 10 |
| reading_list | 9 |
| coursework_requirements | 8 |
| prerequisites | 8 |
| course_content | 6 |
| all | 3 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | opus | 20 | Section extraction for HiOF is accurate. In all 20 courses the h2 headings split cleanly into the seven sections, arbeidskrav always land in coursework_requirements (never in assessment), prerequisites are clean, and Sensorordning/Evaluering/Arbeidsomfang are dropped. Every pre-pass flag is a fal... |
| hivolda | sonnet | 20 | Section extraction works well for hivolda: course_content, learning_outcomes (all three sub-groups kept), teaching_methods, coursework_requirements and prerequisites are correctly split and clean in all 20 courses, including the 6 random controls. The main defect is that assessment is only the ra... |
| hvl | sonnet | 20 | HVL section extraction is accurate: all seven canonical sections map to the right headings, arbeidskrav land in coursework_requirements (not assessment), and the Hjelpemiddel / 'Meir om hjelpemiddel' cruft is already stripped. Every pre-pass flag (boilerplate, empty, short, leak->teaching_methods... |
| inn | sonnet | 20 | Section extraction for inn works well in this packet: learning outcomes are complete, arbeidskrav/obligatoriske aktiviteter are correctly filed as coursework_requirements, 'Ingen' prerequisites and the 'Ingen pensumliste' placeholder are dropped, and no section is missing or truncated. The pre-pa... |
| mf | sonnet | 20 | Extraction is mostly accurate for MF: course_content, coursework_requirements, assessment and learning_outcomes are cleanly cut and complete. The main defects are that reading_list is always generic library-access boilerplate (no actual literature), and that two inline sections (Arbeidsform, Fork... |
| nih | sonnet | 20 | Section splitting for NIH is accurate: all headings (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen) map cleanly, arbeidskrav land in coursework_requirements, and WISEflow/plagiarism notes are removed. The main weakness is the reading_list section, whi... |
| nla | sonnet | 10 | Section extraction works very well for NLA: all ten courses have correct, complete and clean course_content, learning_outcomes, teaching_methods, coursework_requirements and assessment, and arbeidskrav are correctly kept out of assessment. The pre-pass 'leak' flags on the four suspects are false ... |
| nmbu | sonnet | 8 | Section extraction works very well for NMBU: all eight courses have correct, complete sections, CSS/metadata junk is excluded, and obligatory activities are correctly routed to coursework_requirements. Both pre-pass flags are false alarms (a short but real prerequisite 'PPXP100'; a 'leak->reading... |
| nord | sonnet | 20 | The accordion_nord strategy gets the main section boundaries right (learning_outcomes, teaching_methods, coursework vs assessment split) for most courses, but silently loses text: intro paragraphs from course_content, some prerequisites, and in two controls the entire assessment through the covid... |
| ntnu | sonnet | 20 | NTNU section splitting is mostly sound: course_content, learning_outcomes (all Kunnskap/Ferdigheter/Generell kompetanse groups survive) and coursework_requirements are correct, and the 14 suspects are mostly false alarms (the 'boilerplate' flag fires because the same PPU course is offered in seve... |
| oslomet | opus | 20 | For ordinary emneplaner (all 6 random controls and the Norsk/Spesialpedagogikk suspects) the h2-based split works: learning outcomes stay whole across Kunnskap/Ferdigheter/Generell kompetanse, arbeidskrav and assessment are cleanly separated, and the pre-pass flags (boilerplate on reused Norsk em... |
| steiner | sonnet | 13 | Section extraction works well for Steiner: all 13 plans have clean, correctly bounded learning_outcomes, course_content, coursework_requirements (Arbeidskrav kept out of assessment) and assessment, with sensor/aids/ny-utsatt-eksamen logistics correctly dropped. All pre-pass flags (leak->X, long) ... |
| uia | sonnet | 20 | Heading-based splitting works well for UiA: learning outcomes are complete, 'Vilkår for å gå opp til eksamen' correctly goes to coursework_requirements, and the boilerplate flags are false alarms (short but genuine prerequisites/assessment, identical text across sibling practice courses). The rea... |
| uib | sonnet | 20 | Section splitting is largely correct: learning_outcomes, teaching_methods, coursework_requirements (obligatory activity correctly kept out of assessment), prerequisites and reading_list are accurate and complete in all 20 courses. The defects are scraping cruft: a semester-picker widget wrapped a... |
| uio | sonnet | 12 | UiO extraction is mostly clean for course_content, learning_outcomes and prerequisites (admission boilerplate is correctly dropped). The main weakness is that coursework_requirements is rarely emitted: obligatory activities under 'Undervisning' or 'Eksamen' stay in teaching_methods/assessment. A ... |
| uis | opus | 20 | For UiS the core splits work in both extraction paths: learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, prerequisites are clean, and 'Vilkår for å gå opp til eksamen/vurdering' reliably separates coursework_requirements from assessment. The defects sit in the FS-PDF tex... |
| uit |  | 28 | UiT extraction quality is severely degraded by a single structural failure: the heading-splitter treats 'Hva lærer du' (the sub-heading that introduces the learning outcomes section) as a continuation of 'Innhold', then fails to cut at 'Undervisnings- og eksamensspråk' / 'Undervisning' / 'Kvalite... |
| usn | opus | 20 | USN section boundaries are correct: in all 20 courses each canonical section gets the right text, arbeidskrav and obligatorisk aktivitet go to coursework_requirements, graded exams go to assessment, and Hjelpemidler and the programme footer are dropped. All 14 suspects were flagged only as boiler... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | mf | reading_list | boilerplate_only | high | widespread | mf_RL1014-1_2025_autumn_1; mf_SAM1000L-1_2025_autumn_1; m... | In .clean_sections() for mf, strip the 'Litteraturlisten for ...' line and the 'Tilgang til litteratur' paragraph and drop reading_list w... |
| ✓ | uio | coursework_requirements | missing_section | high | widespread | uio_HIS4095L-1_2025_spring_1; uio_FYS1120L-1_2025_spring_... | In R/extract_sections.R, add a second-level split inside teaching_methods and assessment at lines matching 'Obligatoriske? (aktivitet\|akt... |
| ? | uit | learning_outcomes | merged_sections | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisnings- og eksamensspråk', 'Undervisning', 'Kvalitetssikring', 'Kvalitetssikring av emnet', and 'Eksamen' as closing-boundary... |
| ? | uit | assessment | wrong_content | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Assign the 'Eksamen' heading to assessment as its opening boundary (extracting everything from 'Vurderingsform:…' through 'Kontinuasjonse... |
| ? | uit | cross_section | missing_section | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisning' as a heading pattern mapped to teaching_methods in the heading map. This is a prerequisite change alongside fixing the... |
| ✓ | ntnu | teaching_methods | merged_sections | high | common | ntnu_PPU4725-1_2014_autumn_1; ntnu_PPU4729-1_2014_spring_... | In ntnu post-processing, move paragraphs of teaching_methods starting with 'For deltajert/detaljert beskrivelse av vurderingen' or 'Vurde... |
| ✓ | uis | cross_section | wrong_content | high | common | uis_MGL3066-1_2025_autumn_1; uis_MGL2120-1_2025_autumn_1;... | For uis text_split, ignore heading candidates until the FS metadata block has passed. For example, add a cfg field (e.g. section_text_ski... |
| ? | uit | coursework_requirements | merged_sections | high | common | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Add 'Mer info om vurderingsform' as a secondary split point that closes coursework_requirements and opens assessment. Alternatively, stri... |
| ✓ | nord | assessment | missing_section | high | occasional | nord_PO111LS-1_2022_spring_1; nord_RL211L-1_2018_autumn_1 | Make the covid/korona noise pattern remove only the covid sentence (sentence-level str_remove) rather than the whole line, or apply it on... |
| ✓ | oslomet | all | missing_section | high | occasional | oslomet_M5GP3200-1_2024_spring_2; oslomet_M5GP1000-1_2023... | (1) In extract_sections(), decide on the fallback after .clean_sections(): if the cleaned output has fewer than 3 rows, try the alternati... |
| ✓ | uis | reading_list | split_section | high | occasional | uis_MGL2050-1_2021_autumn_1; uis_MGL1050-1_2021_autumn_1 | In extract_sections_text(), once current_section == 'reading_list', allow a switch only on an exact whole-line heading match: in FS plans... |
| ? | uit | assessment | merged_sections | high | occasional | uit_LRU-2642-1_2014_spring_1; uit_PFF-3102-1_2014_autumn_... | Detect the pattern 'Følgende arbeidskrav må være godkjent før man kan fremstille seg for eksamen:' (the older phrasing) as a secondary tr... |
| ? | uit | course_content | merged_sections | high | occasional | uit_LER-1353-1_2023_spring_1; uit_LRU-2642-1_2014_spring_... | Map 'Hva lærer du' and 'Etter bestått emne skal studentene ha følgende læringsresultat' (older phrasing) as opening boundaries for learni... |
| ✓ | oslomet | teaching_methods | wrong_content | high | rare | oslomet_M5GRL2200-1_2019_spring_2 | In .clean_sections() in R/extract_sections.R, if a teaching_methods row starts with '^Retten til å avlegge eksamen (forutsetter\|har som v... |
| ✓ | uio | reading_list | merged_sections | high | rare | uio_HIS4015L-1_2025_spring_1 | Add a heading pattern '^Gjennomføring av praksis' and 'Praksis' mapped to teaching_methods in R/section_heading_map.R. Also stop the read... |
| ✓ | hivolda | assessment | formatting_noise | medium | widespread | hivolda_MGL1-7MA2B-1_2022_spring_1; hivolda_MGL5-10SA1B-1... | Enable cell/row breaks for hivolda tables (pre_fn = .add_table_cell_breaks in R/institution_config.R) and strip the fixed header string '... |
| ✓ | inn | assessment | formatting_noise | medium | widespread | inn_2MPRA171-1-1_2022_spring_1; inn_2MPEL171S-2-1_2024_sp... | In .clean_sections(), for assessment remove the line starting with 'Vurderingsordning Karakterskala Gruppe/individuell Varighet Hjelpemid... |
| ✓ | nih | reading_list | boilerplate_only | medium | widespread | nih_LKI236-1_2024_spring_2; nih_LKI110-1_2025_autumn_1; n... | In .clean_sections(), drop reading_list rows for nih matching '^Ettersom vi er i en overgangsfase', '^Pensumlist[ae] for (høsten\|våren)' ... |
| ✓ | ntnu | assessment | formatting_noise | medium | widespread | ntnu_PPU4625-1_2023_spring_1; ntnu_PPU4725-1_2016_autumn_... | Remove <script>/<style> nodes before text extraction for ntnu (pre_fn in institution_config.R), or drop lines starting with 'function tog... |
| ✓ | ntnu | assessment | other | medium | widespread | ntnu_PPU4625-1_2023_spring_1; ntnu_PPU4725-1_2016_autumn_1 | In an ntnu post-processor, drop table-row lines matching '^Karakter .* (Vekting\|Varighet\|Dato\|Sted og rom\|Eksamenssystem)' tokens like 'D... |
| ✓ | oslomet | assessment | formatting_noise | medium | widespread | oslomet_M1GEN2200-1_2020_autumn_1; oslomet_MGPE3300-1_202... | Safest fix: add an oslomet entry to .section_noise that removes the label lines '(?m)^Ny/uts[ae]tt eksamen\s*$' and the stock sentence '(... |
| ✓ | uia | cross_section | merged_sections | medium | widespread | uia_MA-441-1_2024_autumn_3; uia_MA-446-1_2024_spring_1; u... | Add a pattern '^Faget i praksis' in R/section_heading_map.R mapped to teaching_methods (or a discarded block), placed so it terminates co... |
| ✓ | uib | course_content | formatting_noise | medium | widespread | uib_TYSDI201-0_2018_spring_1; uib_FRAN313L-0_2023_autumn_... | In extract_sections_uib() or .clean_sections(), drop lines matching '^>?\s*Vel emnebeskrivelse for semester' and the following '(Neste se... |
| ✓ | uib | assessment | formatting_noise | medium | widespread | uib_TYSDI201-0_2018_spring_1; uib_FRAN307L-0_2024_autumn_... | In .clean_sections() for uib, cut the assessment text at the first line matching '^Vurderingsordning$', and also remove 'Klokkeslett for ... |
| ✓ | uis | cross_section | merged_sections | medium | widespread | uis_EXPAED102-1_2019_autumn_1; uis_MGL3066-1_2025_autumn_... | Add exact ('exact = TRUE') '.drop' rows to section_heading_patterns in R/section_heading_map.R: 'åpen for', 'åpent for', 'ope for', 'emne... |
| ✓ | uis | course_content | truncated | medium | widespread | uis_MGD1110-1_2019_spring_3; uis_MGL2051-1_2025_spring_4;... | Add 'introduksjon' -> course_content (exact = TRUE) in R/section_heading_map.R. For uis text_split, open course_content for the first par... |
| ? | uit | prerequisites | wrong_content | medium | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Post-process the prerequisites field to strip sentences matching the pattern 'Når eksamen i … er bestått, kan studenten framstille seg ti... |
| ✓ | usn | cross_section | formatting_noise | medium | widespread | usn_MG2NA4-1_2022_spring_1; usn_MG2SA8-1_2024_autumn_1; u... | Skip the table of contents in extract_sections_text(): when a trimmed line equals 'Innholdsfortegnelse', take the next non-blank line L (... |
| ✓ | hiof | assessment | other | medium | common | hiof_LMUKRO10417-1_2024_autumn_1; hiof_LMBNOR10417-1_2025... | In R/section_heading_map.R, set exact = FALSE for the .drop rows 'ny/utsatt eksamen' and 'ny og utsatt eksamen'. They already sit above t... |
| ✓ | nord | course_content | truncated | medium | common | nord_MAT5006-1_2022_spring_2; nord_MAT5003-1_2024_autumn_... | In extract_sections_nord(), collect top-of-page paragraphs that precede the first div.ac block (or that are not inside any div.ac) and pr... |
| ✓ | nord | prerequisites | truncated | medium | common | nord_MAT5006-1_2022_spring_2; nord_MAT5006-1_2023_autumn_... | Filter admission sentences, not whole lines, and add the guards 'må ha (deltatt\|fullført\|bestått)', 'bygger på' and bare course names on ... |
| ✓ | ntnu | assessment | duplicate | medium | common | ntnu_PPU4725-1_2014_autumn_1; ntnu_PPU4729-1_2015_autumn_... | Deduplicate consecutive identical exam blocks (split on 'Ordinær eksamen -' / 'Utsatt eksamen -' and keep unique blocks) in .clean_sectio... |
| ✓ | ntnu | teaching_methods | wrong_content | medium | common | ntnu_PPU4625-1_2023_spring_1; ntnu_PPU4681-1_2024_spring_... | Split off the paragraph starting 'NTNU er tillagt et sertifiseringsansvar' from teaching_methods and append it to coursework_requirements... |
| ✓ | oslomet | teaching_methods | missing_section | medium | common | oslomet_M1GNA2100-1_2022_autumn_1; oslomet_MGPE3300-1_202... | Use the same Fagplan fallback as the Praksis finding, limited to teaching_methods. When the emneplan teaching_methods text is a placehold... |
| ✓ | uia | all | missing_section | medium | common | uia_PED428-1_2023_autumn_1; uia_IDR418-1_2023_autumn_1; u... | Re-fetch these plans and inspect the HTML; adjust selector in R/institution_config.R for uia if a different layout is used. Flag courses ... |
| ✓ | uio | assessment | field_or_language_junk | medium | common | uio_TYSK4091-1_2025_spring_1; uio_NOR4515-1_2025_spring_1... | In .clean_sections() or the heading map, drop the 'Eksamensspråk', 'Karakterskala', 'Adgang til ny eller utsatt eksamen' and 'Trykking' b... |
| ✓ | usn | cross_section | formatting_noise | medium | common | usn_MG2RL3-1_2024_autumn_1; usn_MG2SA8-1_2024_autumn_1; u... | In extract_sections_text(), keep a same-section heading line as content only when it is a learning-outcome sub-heading (kunnskap/kunnskap... |
| ✓ | mf | prerequisites | merged_sections | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern for '^Forkunnskapskrav\b:?' in R/section_heading_map.R mapped to prerequisites, splitting the remainder of the l... |
| ✓ | ntnu | assessment | wrong_content | medium | occasional | ntnu_LÆR2003-1_2023_spring_1; ntnu_MGLU1113-1_2019_autumn... | Within the ntnu 'Mer om vurdering' block, split off sub-blocks headed 'Obligatoriske arbeidskrav' / 'Compulsory assignments' into coursew... |
| ✓ | uis | teaching_methods | truncated | medium | occasional | uis_MGL2051-1_2025_spring_4; uis_MGD1110-1_2019_spring_3 | Add 'praksis' -> teaching_methods with exact = TRUE in R/section_heading_map.R. This labels the block correctly in both html_headings and... |
| ? | uit | learning_outcomes | truncated | medium | occasional | uit_LER-3903-1_2024_autumn_1; uit_LER-3913-1_2024_autumn_... | Investigate and raise (or remove) any field-length cap applied to sections_raw text. Once the merge is fixed the correct learning_outcome... |
| ✓ | usn | assessment | formatting_noise | medium | occasional | usn_MG1KR7-1_2021_autumn_2; usn_MG2EN8-1_2023_spring_1 | Add a usn entry to .section_noise in R/extract_sections.R with whole-line patterns '(?m)^(?:Emneplan(?:en)? )?[Gg]odkjent(?: emneplan)?(?... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Ensure 'Arbeidsform og organisering' (with or without trailing colon) is a boundary at any position in the mf plan, cutting it out of cou... |
| ✓ | nord | cross_section | split_section | medium | rare | nord_MAT5006-1_2022_autumn_1 | In .split_inline_coursework(), treat a gate line as continuing until the next blank line / next line starting with a known label (Oppgave... |
| ✓ | nord | teaching_methods | wrong_content | medium | rare | nord_PR114L-1_2020_autumn_3 | Inspect heading texts of PR*L pages; add the missing heading patterns in R/section_heading_map.R (praksisomfang/obligatorisk -> coursewor... |
| ✓ | uio | coursework_requirements | wrong_content | medium | rare | uio_PROF3025-1_2025_spring_1 | Add patterns for '^Sensur(ordning\|ering)' mapped to assessment in R/section_heading_map.R, so the coursework section ends there. |
| ✓ | hiof | learning_outcomes | other | low | widespread | hiof_LMUMAT40117-1_2025_autumn_1; hiof_LMUSAM10417-1_2019... | In extract_sections_html(), when a sub-heading maps to the same section as state$parent_section, append rvest::html_text2(node) to state$... |
| ✓ | hiof | teaching_methods | truncated | low | widespread | hiof_LMUNAT11224-1_2025_spring_2; hiof_LMBMAT41322-1_2024... | Add an exact row 'praksis' -> teaching_methods (exact = TRUE, so 'Plan for praksis' etc. are unaffected) to the teaching_methods block in... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_2MPEL171S-2-1_2024_spring_1; inn_KROI1001-1_2023_autu... | Keep Kunnskap/Ferdigheter/Generell kompetanse (and Kunnskapsmål/Ferdighetsmål) as lines inside learning_outcomes rather than dropping the... |
| ✓ | nla | coursework_requirements | formatting_noise | low | widespread | nla_MGL5NA201-1_2025_spring_1; nla_MGL1NO201-1_2025_autumn_1 | Optionally keep the heading as a prefix ('Vurderingsuttrykk: Godkjent / Ikke godkjent') or drop the pass/fail line in .clean_sections(). |
| ✓ | uib | cross_section | other | low | widespread | uib_TYSDI201-0_2018_spring_1; uib_TYSDI111-0_2019_autumn_1 | In R/audit/qa_sections.R, count the boilerplate flag over distinct course codes rather than course offerings. |
| ✓ | uis | assessment | formatting_noise | low | widespread | uis_LENG115-1_2017_autumn_1; uis_LKJBAC-1_2019_autumn_1; ... | Add a uis entry to .section_noise in R/extract_sections.R that removes the header line '(?m)^\s*(?:Vurderingsform\s+)?Vekt(?:ing)?\s+(?:V... |
| ? | uit | course_content | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Strip the literal string 'Hva lærer du' (and its variants) from the tail of the course_content field during post-processing (post_fn), or... |
| ? | uit | coursework_requirements | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3902-1_2024_autumn_... | Add a post_fn (or pre_fn cleaning step) that strips the literal strings 'UiTs samleside om eksamen', 'Mer info om arbeidskrav', and 'Mer ... |
| ✓ | usn | learning_outcomes | other | low | widespread | usn_MG1KR7-1_2021_autumn_2; usn_MG2SA8-1_2021_autumn_2; u... | In .clean_section_text(), exclude learning-outcome sub-heading patterns (kunnskap, kunnskaper, kunnskapar, ferdigheter, ferdigheiter, gen... |
| ✓ | hiof | reading_list | formatting_noise | low | common | hiof_LMUSAM10417-1_2019_spring_2; hiof_LMUMAT40217-1_2021... | In .clean_sections(), for reading_list rows, remove lines matching (?im)^Litteraturlist(?:en\|e)?\s+(?:er\s+)?sist\s+oppdater\w*\b.*$. Alt... |
| ✓ | hvl | learning_outcomes | formatting_noise | low | common | hvl_MGUSA101-1_2018_spring_1; hvl_MGUSA102-1_2020_spring_... | Either keep Kunnskap/Kunnskapar/Ferdigheiter/Dugleikar/Generell kompetanse as explicit labels on their own line, or strip them uniformly,... |
| ✓ | inn | teaching_methods | merged_sections | low | common | inn_2MNF171-6-1_2024_autumn_1; inn_2MPEL171S-2-1_2024_spr... | Add a pattern for a standalone 'Praksis' heading in R/section_heading_map.R that maps to a dropped/ignored section (or strip the boilerpl... |
| ✓ | nla | prerequisites | missing_section | low | common | nla_MGL5MU201A-1_2025_spring_2; nla_4MGL5MAT-M-1_2025_aut... | Add a 'progresjonskrav' pattern mapped to prerequisites in R/section_heading_map.R (placed so it merges with Forkunnskapskrav). |
| ✓ | nmbu | teaching_methods | missing_section | low | common | nmbu_PPFD201-1_2025_autumn_2; nmbu_PPPE301-1_2025_spring_... | Map '^Læringsstøtte' to teaching_methods in R/section_heading_map.R, or confirm against section_codebook.yml that it is intentionally ign... |
| ✓ | nord | assessment | truncated | low | common | nord_MAT5006-1_2022_spring_2; nord_MAT5003-1_2024_autumn_... | Check how the aids paragraph is located in the HTML and keep it in assessment; limit the 'plagiat' noise pattern to the AI notice sentence. |
| ✓ | ntnu | reading_list | empty_placeholder | low | common | ntnu_PPU4725-1_2016_autumn_1; ntnu_PPU4729-1_2015_autumn_1 | In .clean_sections() drop sections whose text matches '^(Oppgis senere\|Ingen\|-)\.?$' (case-insensitive). |
| ✓ | ntnu | prerequisites | boilerplate_only | low | common | ntnu_PPU4725-1_2016_autumn_1; ntnu_MGLU1113-1_2019_autumn_2 | Blank prerequisites that match known NTNU admission boilerplate ('Opptak til ...', 'Som opptakskrav til studieprogrammet', 'Forkunnskapsk... |
| ✓ | oslomet | course_content | formatting_noise | low | common | oslomet_M5GKP3100-1_2021_autumn_1; oslomet_M5GNO3100-1_20... | In .clean_sections(), strip course_content lines matching '(?m)^Fagplanen (?:som (?:høyrer\|hører\|tilhører) til\|tilhørende) dette emnet er... |
| ✓ | steiner | cross_section | formatting_noise | low | common | steiner_M-MAT1_1_2025_spring_1; steiner_M-NOR1_1_2025_spr... | In .clean_sections() (R/extract_sections.R), or in a Steiner-specific cleanup, drop lines that consist only of 1-3 digits (regex ^\s*\d{1... |
| ✓ | uia | coursework_requirements | wrong_content | low | common | uia_MA-446-1_2024_spring_1; uia_MA-441-1_2024_autumn_3 | Optional sentence-level post-step for uia moving sentences matching 'krav om .*(deltagelse\|frammøte\|tilstedeværelse)' to coursework_requi... |
| ✓ | uib | assessment | formatting_noise | low | common | uib_TYSDI201-0_2020_autumn_1; uib_TYSDI111-0_2019_autumn_... | Map the 'Vurderingssemester' heading to a dropped section in R/section_heading_map.R so its content is discarded, or strip the line in .c... |
| ✓ | uib | assessment | wrong_content | low | common | uib_TYSDI111-0_2018_spring_1; uib_TYSDI111-0_2017_autumn_... | Strip sentences beginning 'Studentane evaluerer undervisninga' from assessment in .clean_sections(). |
| ✓ | uio | assessment | truncated | low | common | uio_FYS1120L-1_2025_spring_1; uio_PROF3025-1_2025_spring_... | Handle both headings the same way: drop them. See the assessment logistics finding. |
| ✓ | usn | reading_list | empty_placeholder | low | common | usn_MG1NA7-1_2021_autumn_2; usn_MG2EN8-1_2022_spring_2; u... | Add '(?:pensum-?/?)?(?:litteratur\|pensum)liste(?:n)? er ikke publisert(?: ennå)?' to .placeholder_phrases. |
| ✓ | usn | reading_list | formatting_noise | low | common | usn_MG2RL3-1_2024_autumn_1; usn_MG2NA4-1_2022_spring_1; u... | Preferred: fix this at the source, in the USN Shadow DOM text extraction or .anon_usn(), by inserting a separator between Leganto item-ty... |
| ✓ | hvl | assessment | formatting_noise | low | occasional | hvl_MGUSA202-1_2020_spring_1; hvl_MGUEN102-1_2022_spring_1 | In .clean_sections(), drop sentences matching '(Tid og st(a\|e)d\|Innleveringstidspunkt).*Studentweb' from assessment. |
| ✓ | mf | teaching_methods | missing_section | low | occasional | mf_BAO2705-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1 | Accept as a limitation, or optionally add a paragraph-level rule for paragraphs starting 'Undervisningen er/består'. |
| ✓ | nih | prerequisites | missing_section | low | occasional | nih_LKI110-1_2025_autumn_1; nih_LKI111-1_2025_autumn_1 | Decide a consistent rule in .clean_sections(): either keep short 'Ingen ...' prerequisites rows for all, or drop all; verify the Forkrav ... |
| ✓ | nmbu | learning_outcomes | formatting_noise | low | occasional | nmbu_PPUT301-1_2025_spring_1; nmbu_PPFD301-1_2025_spring_1 | Either keep the Kunnskap/Ferdigheter/Generell kompetanse labels consistently, or extend the strip pattern to cover 'Generelle ferdigheter... |
| ✓ | nord | coursework_requirements | truncated | low | occasional | nord_KHV1004-1_2022_autumn_1; nord_KHV1007-1_2022_autumn_1 | Where an assessment block starts with a sentence before the first gate line, keep it in assessment; verify it is not removed by the clean... |
| ✓ | nord | all | other | low | occasional | nord_SP154L-1_2018_autumn_3; nord_HI109LS-1_2016_autumn_2... | Check the HTML for these courses; if no plan exists, flag them as no_plan in course_offerings rather than keeping a plan consisting only ... |
| ✓ | uia | learning_outcomes | formatting_noise | low | occasional | uia_ERN502-1_2025_autumn_1; uia_NO-504-1_2024_spring_1 | Keep recognised sub-headings (Kunnskap, Ferdigheter, Generell kompetanse and nynorsk variants) as text lines inside learning_outcomes ins... |
| ✓ | uis | cross_section | formatting_noise | low | occasional | uis_LMAMAS-1_2017_spring_1; uis_LFYMAS-1_2017_spring_1; u... | Before text_split for uis, remove lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$', and remove lines e... |
| ? | uit | prerequisites | wrong_content | low | occasional | uit_LER-2152-1_2025_spring_1 | Strip paragraphs matching 'Studiepoengreduksjon' (and the credit-reduction boilerplate that follows) in the prerequisites post_fn, or add... |
| ? | uit | teaching_methods | wrong_content | low | occasional | uit_LRU-3300-1_2016_autumn_1; uit_LRU-2642-1_2014_spring_... | Add patterns for 'For nærmere informasjon om praksis, se egen praksisplan' and 'Emnet evalueres muntlig eller skriftlig minimum en gang h... |
| ✓ | hiof | reading_list | empty_placeholder | low | rare | hiof_LMUNAT11224-1_2025_spring_2 | Add 'litteratur(?:en\|listen)? vil (?:være\|bli) klar[t]?(?: ved semesterstart)?' to .placeholder_phrases in R/extract_sections.R. |
| ✓ | hivolda | assessment | wrong_content | low | rare | hivolda_MGL5-10EN2A-1_2020_autumn_1 | Accept as a limitation, or add a post-rule that moves sentences about portfolio weighting/delivery as exam from coursework_requirements t... |
| ✓ | hvl | coursework_requirements | empty_placeholder | low | rare | hvl_MGUSA301-1_2018_autumn_2 | In .clean_sections(), strip leading lines that are exactly 'Ingen'/'Ingen.'/'-' when other text follows. |
| ✓ | nla | learning_outcomes | truncated | low | rare | nla_MGL5MU201A-1_2025_spring_2 | In the splitter, only drop the matched heading line itself; do not collapse consecutive heading lines that map to the same section (keep ... |
| ✓ | nord | prerequisites | empty_placeholder | low | rare | nord_PR114L-1_2020_autumn_3 | Drop accordion bodies that match ^(Ingen\|-\|Ingen krav)\.?$ before concatenation in extract_sections_nord(). |
| ✓ | nord | learning_outcomes | formatting_noise | low | rare | nord_RL211L-1_2018_autumn_1 | In .clean_section_text(), replace leading '¿' at line start with '- ' (or strip it). |
| ✓ | uib | assessment | truncated | low | rare | uib_TYSDI201-0_2020_autumn_1 | Check extract_sections_uib() for dropping of year-specific trailing paragraphs; keep all paragraphs inside the Vurderingsformer details b... |
