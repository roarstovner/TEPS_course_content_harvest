# sections audit — aggregated findings

128 findings across 22 institutions (from 22 agent reports in `data/audit/sections/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 126 passed (✓), 2 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

| institution | target | error_type | severity | problem | evidence |
| --- | --- | --- | --- | --- | --- |
| ntnu | coursework_requirements | wrong_content | high | not in packet: ntnu_PSYPRO4800-1_2021_autumn_1, ntnu_BIM1023-1_2022_autumn_1 | Det er krav om 80 % oppmøte på disse seminarene. |
| himolde | course_content | formatting_noise | medium | evidence not_found | Lecturer:[name], Technical University of Denmark |

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| cross_section | wrong_content | 14 | fhs, himolde, hivolda, hvl, inn, mf, nmbu, nord, steiner, uia, uib, uio, uis, uit | high |
| assessment | other | 9 | hiof, hvl, inn, nih, nla, oslomet, uio, uis, vid | medium |
| assessment | wrong_content | 8 | fhs, nmbu, ntnu, phs, steiner, uib, uit, vid | high |
| course_content | wrong_content | 8 | hvl, nih, nla, oslomet, uia, uio, uit, vid | high |
| teaching_methods | wrong_content | 8 | mf, nla, nmbu, ntnu, phs, uia, uit, usn | high |
| coursework_requirements | wrong_content | 7 | fhs, hivolda, nord, ntnu, oslomet, uio, vid | high |
| cross_section | empty_placeholder | 7 | fhs, hivolda, ntnu, oslomet, uib, uit, usn | high |
| prerequisites | wrong_content | 7 | himolde, mf, nla, nmbu, nord, uia, usn | high |
| prerequisites | missing_section | 4 | fhs, nord, uio, uit | high |
| cross_section | other | 3 | inn, mf, uia | high |
| assessment | formatting_noise | 3 | himolde, steiner, vid | medium |
| reading_list | empty_placeholder | 3 | himolde, hiof, nmbu | medium |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| wrong_content | high | 47 |
| wrong_content | medium | 19 |
| other | medium | 12 |
| empty_placeholder | low | 7 |
| missing_section | high | 7 |
| formatting_noise | low | 6 |
| merged_sections | high | 4 |
| truncated | high | 4 |
| duplicate | high | 3 |
| empty_placeholder | medium | 3 |
| duplicate | low | 2 |
| empty_placeholder | high | 2 |
| formatting_noise | medium | 2 |
| other | low | 2 |
| truncated | medium | 2 |
| boilerplate_only | high | 1 |
| boilerplate_only | medium | 1 |
| field_or_language_junk | low | 1 |
| other | high | 1 |
| split_section | high | 1 |
| wrong_content | low | 1 |

## By target

| target | n |
| --- | --- |
| cross_section | 38 |
| assessment | 25 |
| teaching_methods | 16 |
| prerequisites | 15 |
| course_content | 13 |
| coursework_requirements | 11 |
| reading_list | 6 |
| all | 3 |
| learning_outcomes | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 18 | Reviewed all 18 sampled courses (14 suspects, four random controls) in full as displayed, plus both attendance appendix items: 2 correct and 0 wrong (covering all 5 moved sentences/plans); packet-builder truncation is not an extractor defect. The FHS prerequisite and teaching heading fixes hold i... |
| himolde | gpt-6.1-sol | 20 | All 20 courses were reviewed in full as presented in the packet (4 suspects and 16 random controls); packet-builder truncation markers were not treated as extraction loss. Major headings, programme-list boundaries, real prerequisites and all displayed learning-outcome groups are generally preserv... |
| hiof | gpt-6.1-sol | 20 | Section boundaries generally work well: learning-outcome groups remain intact, prerequisites contain prior knowledge rather than admission rules, and coursework gates are separated from graded assessment. The leak flags are false alarms caused by subject-matter phrases about teaching methods and ... |
| hivolda | gpt-6.1-sol | 16 | Reviewed all 16 sampled courses (14 suspects, 2 controls) and all 21 attendance appendix items: 19 correct moves and 2 wrong moves (ATT 11 and ATT 15, cut at jf./Jf.); no residual or duplicated attendance requirements were visible in the sampled sections. Most boilerplate flags reflect legitimate... |
| hvl | gpt-6.1-sol | 17 | Heading-based extraction generally preserves the sections, all learning-outcome groups, and the distinction between assessment and compulsory coursework across all 17 reviewed courses. The boilerplate flags on the 13 engineering bachelor offerings mostly identify legitimate shared outcomes, teach... |
| inn | gpt-6.1-sol | 20 | Section boundaries generally work across all 20 reviewed courses: all learning-outcome groups survive, assessment design tables are retained correctly, and empty prerequisites/reading-list placeholders and trailing metadata are excluded. Most pre-pass flags are false alarms: the practicum labels ... |
| mf | gpt-6.1-sol | 9 | The main sections and learning-outcome groups are generally preserved across all nine courses, but heading-based extraction misses semantic boundaries in both suspects and random controls. MET5010's long teaching-methods flag and PPU1015's short prerequisite flag are length false alarms; RL1014's... |
| nih | gpt-6.1-sol | 20 | Section boundaries and learning-outcome coverage are generally sound across all 20 courses, including the six random controls; empty reading-list headings and archive-only pointers are correctly omitted. All 13 leak flags are lexical false alarms: Arbeidskravsanalyse is a graded portfolio compone... |
| nla | gpt-6.1-sol | 16 | All 16 packet entries were read in full; packet-imposed caps limit completeness checks against source text not displayed. Section boundaries, learning-outcome groups, and the main assessment/coursework distinction generally work, but outgoing progression rules contaminate prerequisites in all 14 ... |
| nmbu | gpt-6.1-sol | 10 | All 10 courses were reviewed in full as presented in the packet; learning-outcome groups are intact and final assessment is generally separated from compulsory activities. All four suspects have false-positive pre-pass flags: PPXP100 is a real prerequisite, the long reading section is legitimate,... |
| nord | gpt-6.1-sol | 20 | Reviewed all 20 sampled courses (14 suspects and 6 random controls) and all 18 attendance appendix items: 18 correct moves and 0 wrong moves; no moved attendance sentence was observed duplicated in a sampled source section. Most boilerplate flags are false alarms on valid repeated teaching/attend... |
| ntnu | gpt-6.1-sol | 20 | All 20 sampled courses (14 suspects and 6 random controls) were read in full: the suspects chiefly expose English-version pointers incorrectly emitted as substantive sections, while empty flags on Seminar, Workshop and Øvinger and boilerplate on genuine mandatory activities are false alarms. All ... |
| oslomet | gpt-6.1-sol | 20 | Reviewed all 20 packet courses (14 suspects and 6 random controls), including all displayed text; packet display truncations were not treated as extraction errors. Four substantive practicum plans lose all sections, while pointer-only rows affect nine vocational-practicum suspects and one random ... |
| phs | gpt-6.1-sol | 14 | Section extraction is generally sound across all 14 sampled courses: the nine suspects have false-positive heading-leak flags (instructor learning outcomes, formative progression criteria, and introductory references to learning outcomes), and the observed PHS attendance, arbeidskrav-feedback, wo... |
| steiner | gpt-6.1-sol | 12 | Core content, all three learning-outcome groups, and most coursework and assessment design are preserved across all 12 courses. All suspect leak flags are false alarms: references to assessment, teaching methods, literature and learning materials describe outcomes, subject matter or compulsory ta... |
| uia | gpt-6.1-sol | 20 | The main heading split is largely intact across all 20 sampled courses, but mandatory requirements still leak into teaching methods, workload contaminates 16 teaching sections, and several local semantic errors remain. All 20 attendance appendix items were judged correct and 0 wrong; the sampled ... |
| uib | gpt-6.1-sol | 20 | Reviewed all 20 packet entries (14 suspects and 6 controls); JUR661 has a packet-builder cap on its full-plan text, which is not an extractor defect. The short/duplicate flags on four INTH offerings expose retained language placeholders, whereas the boilerplate flags on INF367/INF367A and NOSP216... |
| uio | gpt-6.1-sol | 8 | All eight courses were reviewed in full: section boundaries, learning-outcome subgroups, and most prerequisites are preserved, but semantic routing and cleanup remain incomplete in suspects and random controls. HIS4015L’s leak->teaching_methods flag is a lexical false alarm in a description of th... |
| uis | gpt-6.1-sol | 18 | All 18 packet courses were read in full as displayed; packet-builder truncation limits comparisons beyond the displayed text, and no extraction truncation is inferred from those markers. Main section boundaries and all displayed learning-outcome groups are generally preserved, but semantic miscla... |
| uit | gpt-6.1-sol | 20 | Reviewed all 20 sampled courses (14 suspects, 6 random controls) and all 18 attendance appendix items: 18 correct moves, 0 wrong moves. The widened attendance moves are sound and no moved sentences are duplicated in their sampled source sections, but an unflagged control still leaves mandatory at... |
| usn | gpt-6.1-sol | 16 | Most main section boundaries and all visible learning-outcome groups are preserved, but nursing coursework is substantially misallocated and practice sections retain assessment and mandatory-work text in teaching_methods. The repeated PPU course-content, outcomes, coursework and exam-design passa... |
| vid | gpt-6.1-sol | 20 | The 20 packet courses (14 suspects and six controls) generally have sound section boundaries, with all learning-outcome groups preserved and substantive assessment design retained; comparisons are limited to the source text visible in the packet, whose truncation markers are not extraction defect... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | hvl | cross_section | wrong_content | high | widespread | hvl_ELE351-1_2025_autumn_1; hvl_MAS354-1_2024_autumn_1; h... | Add a narrowly matched HVL .sentence_moves rule moving the quoted engineering group/collaboration sentence from course_content to teachin... |
| ✓ | nla | prerequisites | wrong_content | high | widespread | nla_MGL1KH101-1_2017_autumn_2; nla_4MGL1NO201-1_2021_spri... | Add an NLA rule for Progresjonskrav that excludes outgoing conditions such as Emnet må være bestått for å kunne fortsette and En må ha fu... |
| ✓ | nmbu | cross_section | truncated | high | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_PPXP100-1_2025_autumn_... | Add an exact mapping for Om bruk av KI to assessment. Within NMBU AI blocks, route the explicit sentences about KI forming part of course... |
| ✓ | nord | cross_section | wrong_content | high | widespread | nord_MAT5006-1_2022_spring_2; nord_SOS5003-1_2020_autumn_... | Add bounded Nord sentence moves for the repeated supervision and thesis-submission constructions, sending supervision to teaching_methods... |
| ✓ | ntnu | cross_section | empty_placeholder | high | widespread | ntnu_TØL4010-1_2019_autumn_1; ntnu_IMT4895-1_2025_autumn_... | Extend case-insensitive placeholder recognition to se engelsk versjon with optional punctuation and for emnebeskrivelse, and the paired l... |
| ✓ | uit | assessment | truncated | high | widespread | uit_FIN-1025-1_2015_autumn_1; uit_FYS-8804-1_2019_autumn_... | Handle combined language blocks before .drop: move explicit exam-language sentences to assessment and retain a shared bare language value... |
| ✓ | fhs | cross_section | wrong_content | high | common | fhs_LED3201-1_2022_spring_1; fhs_INGOF2230-1_2025_spring_... | Recognise Praktisk organisering og arbeidsformer and explicit teaching lead-ins as teaching boundaries; route supplemental final-delivera... |
| ✓ | fhs | prerequisites | missing_section | high | common | fhs_ING3511-1_2021_autumn_1; fhs_CBU1503-1_2025_spring_1;... | Add guarded sentence patterns for Emnet bygger på/bygger videre på ferdighetene and The course is based on when they explicitly name prio... |
| ✓ | himolde | cross_section | wrong_content | high | common | himolde_ØKL110-1_2022_autumn_1; himolde_VPL033-1_2019_aut... | Add narrowly scoped himolde sentence moves: move the two BØK411 assessment-focus sentences beginning Vurderingen i kurset and Kvaliteten ... |
| ✓ | hivolda | cross_section | wrong_content | high | common | hivolda_PPUD-DID2KH-1_2023_autumn_1; hivolda_PPUD-DID2MU-... | Add a hivolda-specific paragraph relocation in R/extract_sections.R for the observed Ein del av undervisninga ... Felles fagdidaktikk var... |
| ✓ | hivolda | cross_section | wrong_content | high | common | hivolda_PPUD-DID2KH-1_2024_autumn_1; hivolda_PPUD-DID2MU-... | Relocate the exact standalone Bestått didaktikkemne 1. sentence from hivolda coursework_requirements to prerequisites in R/extract_sectio... |
| ✓ | inn | cross_section | wrong_content | high | common | inn_SOSP3002-1_2025_spring_1; inn_SOSP3002-1_2024_spring_... | Extend .split_lo_intro() to INN for the descriptive paragraph before the actual outcome lead, preserving that lead and all three outcome ... |
| ✓ | mf | teaching_methods | wrong_content | high | common | mf_RL5065-1_2025_autumn_1; mf_PPU1010-1_2025_autumn_1; mf... | Add bounded MF sentence rules moving explicit teaching arrangements, such as Undervisning i emnet gis i form av, Det veksles mellom forel... |
| ✓ | nmbu | cross_section | wrong_content | high | common | nmbu_M30-LUN-1_2025_autumn_1; nmbu_PPRA201-1_2025_autumn_... | Add narrowly scoped NMBU sentence routing after sectionization: move thesis submission-format and individual/group-contribution sentences... |
| ✓ | nord | prerequisites | truncated | high | common | nord_MAT5006-1_2022_spring_2; nord_SAM5003-1_2022_spring_... | Read Nord Adgangsregulering blocks as prerequisite candidates, strip programme-admission sentences, retain prior-course/credit/FoU requir... |
| ✓ | nord | prerequisites | wrong_content | high | common | nord_SOS5003-1_2021_autumn_1; nord_PRA5015-1_2024_autumn_... | Add narrowly scoped admission-only patterns for these Nord constructions, including ellers ingen forkunnskapskrav; remove those sentences... |
| ✓ | nord | cross_section | wrong_content | high | common | nord_PRA5015-1_2024_autumn_3; nord_MFL390-1_2021_autumn_1 | Recognize PR - Eksamen and RUTINER VED TVIL OM BESTÅTT PRAKSISOPPLÆRING as assessment sub-blocks, move their assessment-design text to as... |
| ✓ | oslomet | all | missing_section | high | common | oslomet_M1GP4000-1_2025_autumn_2; oslomet_M5GP4000-1_2025... | Configure OsloMet Fagplan as a scoped container whose mapped subheadings can open canonical sections without a mapped parent. Resolve sam... |
| ✓ | steiner | cross_section | wrong_content | high | common | steiner_M-NOR1_2_2025_autumn_1; steiner_M-NOR1_3_2025_aut... | Add Steiner-specific sentence/block reassignment rules: move the introductory paragraph beginning FoU-oppgaven er et individuelt arbeid, ... |
| ✓ | uia | cross_section | wrong_content | high | common | uia_PED420-1_2021_autumn_1; uia_MA-457-1_2025_autumn_1; u... | Add narrowly scoped sentence patterns for explicit mandatory participation, compulsory supervision hours and Det er obligatoriske arbeids... |
| ✓ | uib | cross_section | wrong_content | high | common | uib_INF367-0_2025_spring_1; uib_NOSP216-0_2024_spring_1; ... | Add narrow sentence/block routing rules in R/extract_sections.R for the observed wording: move Masteroppgåva har to deler and its thesis-... |
| ✓ | uio | coursework_requirements | wrong_content | high | common | uio_KJM5050-1_2025_autumn_1; uio_BIOS5000-1_2025_autumn_1... | Add UiO-specific sentence/block routing for the observed compulsory-practice and Nynorsk seminar-attendance formulations. When an explici... |
| ✓ | uis | cross_section | wrong_content | high | common | uis_MGL2120-1_2025_autumn_1; uis_LPRA30-1_2016_spring_2; ... | Add narrowly scoped UiS sentence moves: move explicit final portfolio design and report-completion statements to assessment, exam-languag... |
| ✓ | uit | cross_section | empty_placeholder | high | common | uit_JUR-8004-1_2014_autumn_1; uit_FYS-3820-1_2019_autumn_... | Add anchored whole-placeholder patterns for these observed variants, including repeated placeholder-only paragraphs; remove them before k... |
| ✓ | uit | course_content | wrong_content | high | common | uit_MFA-0004-1_2020_spring_1; uit_MBI-3016-1_2020_spring_... | Apply sentence-level admission/study-right and administrative-deadline cleanup to UiT course_content; first preserve and route any stated... |
| ✓ | uit | cross_section | wrong_content | high | common | uit_JUR-3003-1_2019_spring_1; uit_JUR-1004-1_2018_autumn_... | Add deterministic UiT progression/prior-course sentence rules for bestått 3. avdeling and JUR-0500, examine assessment and course_content... |
| ✓ | uit | cross_section | wrong_content | high | common | uit_JUR-3003-1_2019_spring_1; uit_JUR-3004-1_2018_autumn_... | Keep the coursework block active across its rule-link paragraphs until the explicit Eksamen består av boundary; include the later JUR-300... |
| ✓ | uit | assessment | wrong_content | high | common | uit_JUR-3901-1_2012_spring_1; uit_FIN-1025-1_2015_autumn_... | Add UiT sentence-level resit/postponement, registration and examiner-arrangement cleanup for the observed Norwegian/English variants; rec... |
| ✓ | fhs | cross_section | wrong_content | high | occasional | fhs_FHS8004-1_2022_autumn_1; fhs_MILM2408-1_2025_spring_1... | Strip target-group, security-clearance, fee/accommodation, equipment, dated schedule and workload-only spans from emitted sections. Treat... |
| ✓ | mf | prerequisites | wrong_content | high | occasional | mf_PPU1010-1_2025_autumn_1; mf_AVH5055-1_2025_autumn_1 | Add MF sentence moves from course_content to prerequisites for For å melde seg opp i emnet, må studenten ha bestått eksamen and Studenten... |
| ✓ | mf | cross_section | wrong_content | high | occasional | mf_PPU1010-1_2025_autumn_1; mf_RL5075-1_2025_autumn_1 | Add MF rules that separate each DELEMNE introduction in learning_outcomes from the following Studenten har/Studenten kan outcome groups a... |
| ✓ | nih | course_content | wrong_content | high | occasional | nih_LKI236-1_2024_spring_2; nih_PPU404-1_2024_spring_1; n... | Add NIH-specific course_content-to-teaching_methods moves in .sentence_moves for the observed teaching-only sentence templates: Forelesni... |
| ✓ | nord | cross_section | duplicate | high | occasional | nord_SOS5003-1_2021_autumn_1; nord_SOS5003-1_2020_autumn_2 | Split this deterministic mixed construction at the conjunction: retain Arbeidskrav in coursework_requirements and hjemmeeksamen (3 dager)... |
| ✓ | ntnu | teaching_methods | wrong_content | high | occasional | ntnu_SARB3008-1_2019_spring_1; ntnu_HBIOA2002-1_2021_autu... | Accept mandatory attendance at forkurs, and process an inline Obligatoriske arbeidskrav label with its complete requirement block until a... |
| ✓ | ntnu | teaching_methods | wrong_content | high | occasional | ntnu_MOL3901-2_2022_autumn_1; ntnu_KULT2211-1_2020_autumn_1 | Add sentence-level assessment-design routing for Form of examination and clear grade/exam-form statements, including their grade-adjustme... |
| ✗ | ntnu | coursework_requirements | wrong_content | high | occasional | ntnu_PSYPRO4800-1_2021_autumn_1; ntnu_BIM1023-1_2022_autu... | Detect disse/disse seminarene/disse samlingene and equivalent deictic attendance references before moving a sentence. Preserve the verbat... |
| ✓ | oslomet | course_content | wrong_content | high | occasional | oslomet_SSA5910-1_2021_autumn_1; oslomet_TANNPRAINN1A-1_2... | Add context-specific paragraph routing for OsloMet thesis plans: move monograph/article submission design and length requirements to asse... |
| ✓ | uia | cross_section | other | high | occasional | uia_HEL611-1_2021_autumn_1; uia_ST-301-1_2025_autumn_1 | Compare fulltext and section blocks for these two source IDs; bound .anon_uia staff removal at any structural heading, not only a colon-e... |
| ✓ | uit | prerequisites | missing_section | high | occasional | uit_FIN-1025-1_2015_autumn_1; uit_MBI-3016-1_2020_spring_2 | Before discarding admission blocks, extract paragraphs introduced by Anbefalte forkunnskaper or Recommended prerequisites into prerequisi... |
| ✓ | usn | cross_section | split_section | high | occasional | usn_050-E16-1_2022_spring_1; usn_051-E16-1_2022_spring_1;... | In the USN text reader, track the top-level headings identified from the table of contents and numbered Arbeidskrav blocks. Keep HENSIKT,... |
| ✓ | fhs | coursework_requirements | wrong_content | high | rare | fhs_FHS8004-1_2022_autumn_1 | Extend the bounded attendance matcher to Det er pliktig fremmøte, retaining safeguards for negation, optional participation and assessed ... |
| ✓ | fhs | coursework_requirements | missing_section | high | rare | fhs_ING3511-1_2021_autumn_1 | Recognise a bounded obligatory-supervision sentence as coursework_requirements and remove it from assessment while retaining all graded r... |
| ✓ | fhs | assessment | wrong_content | high | rare | fhs_FHS8004-1_2022_autumn_1 | When an assessment paragraph explicitly defines arbeidskrav and states that they do not count toward the grade, move the defining paragra... |
| ✓ | fhs | coursework_requirements | wrong_content | high | rare | fhs_OPG3302-1_2022_autumn_1 | Split this FHS thesis block at Arbeidet skal presenteres skriftlig i form av en oppgave and route the final thesis and word-limit paragra... |
| ✓ | fhs | prerequisites | merged_sections | high | rare | fhs_SPR2710-1_2023_spring_1 | Within FHS prerequisite blocks, terminate prior-knowledge text at a separate paragraph beginning Emnet er strukturert and route that topi... |
| ✓ | himolde | teaching_methods | merged_sections | high | rare | himolde_SKD204-1_2022_spring_1 | Ensure numbered headings matching ^Arbeidskrav\s+[IVX0-9]+(?:\s*[:.]\|\s) open coursework_requirements, preserving the heading and all sub... |
| ✓ | himolde | prerequisites | wrong_content | high | rare | himolde_LOG904-185-1_2025_autumn_1 | After whitespace normalization, suppress a prerequisites row whose entire text exactly equals course_content, keeping the source text in ... |
| ✓ | inn | teaching_methods | duplicate | high | rare | inn_PUB3000-1_2024_autumn_2 | Add an INN guard for a teaching_methods body identical after whitespace normalization to course_content and beginning Emnet er bygd opp a... |
| ✓ | mf | cross_section | merged_sections | high | rare | mf_RL1014-1_2025_autumn_1 | Expand MF subheading candidates to cover paragraph and h3-h6 headings inside details, and recognize the standalone Arbeidsform og organis... |
| ✓ | nord | prerequisites | missing_section | high | rare | nord_SPD5002-1_2022_spring_2 | Add bounded Norwegian prior-study constructions such as fulgt emnene and gjennomført emnene to the real-prerequisite guard so mixed admis... |
| ✓ | nord | coursework_requirements | truncated | high | rare | nord_REL1004-1_2019_autumn_2 | Add a bounded pattern for excursions described as part of obligatory participation, moving the requirement and its immediately referring ... |
| ✓ | nord | coursework_requirements | wrong_content | high | rare | nord_REL1004-1_2019_autumn_2 | Recognize the bounded contrast construction arbeidskrav ... , andre går inn i mappevurderinga and split at the clause boundary, assigning... |
| ✓ | ntnu | assessment | wrong_content | high | rare | ntnu_TØL4012-1_2024_autumn_1 | Recognize OBLIG/OBLIGer/OBLIG'er as mandatory submissions in sentence-level coursework routing when no grading language applies. Preserve... |
| ✓ | ntnu | cross_section | duplicate | high | rare | ntnu_HBIOA2002-1_2021_autumn_1 | Classify percentages attached to Delta laboratory exercises and other participation requirements as attendance rather than grade weights.... |
| ✓ | oslomet | coursework_requirements | wrong_content | high | rare | oslomet_TANNPRAINN1A-1_2018_autumn_2 | Recognize the anchored English absence-compensation lead-in and move its linked absence/extension paragraph into coursework_requirements ... |
| ✓ | phs | teaching_methods | wrong_content | high | rare | phs_ETF-MISHAN-1_2024_autumn_1 | Extend the deterministic gate handling to move explicit absence sanctions such as this sentence to coursework_requirements, together with... |
| ✓ | phs | assessment | wrong_content | high | rare | phs_HUNDVÅPEN-1_2024_autumn_1 | Split the dog-handler assessment block at the explicit progression and level-check conditions. Move the progression requirement, mandator... |
| ✓ | steiner | assessment | wrong_content | high | rare | steiner_M-NOR1_3_2025_autumn_1 | Extend gate detection with a bounded rule matching the complete sentence beginning Studenter som uten gyldig grunn ikke har lagt inn teks... |
| ✓ | uia | course_content | wrong_content | high | rare | uia_MA-457-1_2025_autumn_1 | Recognize an exact paragraph-start Faget i praksis. label in UiA blocks, split off its following text as a teaching_methods block, and en... |
| ✓ | uia | prerequisites | boilerplate_only | high | rare | uia_FIL908N-1_2025_spring_1 | When a general-admission sentence is removed, also remove its immediately following I tillegg ... documentation/years-of-employment conti... |
| ✓ | uia | prerequisites | wrong_content | high | rare | uia_BIO601-1_2025_autumn_1 | Remove the specific course-content committee-approval formulation from prerequisites and keep a single occurrence under the stated course... |
| ✓ | uia | learning_outcomes | wrong_content | high | rare | uia_BIO601-1_2025_autumn_1 | Add a UiA BIO601-compatible split before the exact After completing the course, the candidate is expected to: lead-in: move the preceding... |
| ✓ | uio | cross_section | wrong_content | high | rare | uio_HIS4015L-1_2025_autumn_1 | Add bounded UiO routing within Gjennomføring av praksis: move the passages introduced by Praksislærer skal vurdere studenten, Lærer skal ... |
| ✓ | uio | prerequisites | missing_section | high | rare | uio_GEO5930L-2_2025_autumn_1 | Before removing UiO admission blocks, extract sentences explicitly requiring a completed subject group or named prior courses into prereq... |
| ✓ | uit | all | missing_section | high | rare | uit_BIO-3331-1_2009_spring_1 | Extend the UiT legacy field reader to recognize these inline field labels, including Course contents, Objective of the course, Teaching m... |
| ✓ | uit | cross_section | wrong_content | high | rare | uit_JUR-3901-1_2012_spring_1 | Recognize these anchored compulsory-lecture and vilkår for å levere ... til bedømmelse forms; move the gate and its attendance-exemption ... |
| ✓ | uit | cross_section | wrong_content | high | rare | uit_MAT-1002-1_2010_spring_1 | Split anchored Tillatte hjelpemidler til eksamen labels inside reading_list and route the heading and following aids list to assessment; ... |
| ✓ | vid | cross_section | merged_sections | high | rare | vid_MATRS5115-1_2025_spring_1 | Recognize standalone Annet labels within VID content blocks before sectionization and split their following paragraphs by explicit cues: ... |
| ✓ | vid | assessment | wrong_content | high | rare | vid_PTUKUN2014-1_2025_spring_1 | Extend .gate_sentence_regex to recognize forutsettes det at after an exam-access clause when the sentence explicitly requires approved ob... |
| ✓ | vid | coursework_requirements | wrong_content | high | rare | vid_MAPBU9010-1_2025_autumn_1 | For VID plans that explicitly say Mappevurderingen består av ..., reconcile the named components with items under Obligatoriske aktivitet... |
| ✓ | hiof | reading_list | empty_placeholder | medium | widespread | hiof_LPKR116-1_2019_autumn_1; hiof_LPDR216-1_2022_spring_... | Extend the whole-section placeholder patterns to recognize the exact Leganto availability notice and the combined Felleslitteratur refere... |
| ✓ | hvl | assessment | other | medium | widespread | hvl_ELE351-1_2025_autumn_1; hvl_BYG350-1_2024_autumn_1; h... | Add narrowly anchored HVL sentence/line filters in .section_noise$hvl for the quoted bachelor resubmission sentence, the JOR508 Ny eksame... |
| ✓ | hvl | course_content | wrong_content | medium | widespread | hvl_BYG350-1_2025_autumn_1; hvl_MAS355-1_2024_autumn_1; h... | In HVL section cleanup, remove lines matching ^Studieprogram: from course_content and narrowly match the standalone Utdanningen er gratis... |
| ✓ | mf | cross_section | other | medium | widespread | mf_PED5610-1_2025_autumn_1; mf_AVH5055-1_2025_autumn_1; m... | Add MF sentence-level cleanup for withdrawal/assessment-registration and exam-attempt consequences, automatic registration, submission-pr... |
| ✓ | oslomet | cross_section | empty_placeholder | medium | widespread | oslomet_YFHDPRA3-1_2022_spring_2; oslomet_YFPRA4-1_2025_s... | Add anchored placeholder patterns for these exact pointer families, including praksis i/praksisi and the longer prerequisite wording. Rem... |
| ✓ | uia | teaching_methods | wrong_content | medium | widespread | uia_MA-457-1_2024_autumn_2; uia_FYS129-G_2020_autumn_2; u... | Add UiA sentence removal for explicit total expected course-workload estimates, including the approximately/ca. forms and unpunctuated li... |
| ✓ | uib | assessment | truncated | medium | widespread | uib_INF367-0_2025_spring_1; uib_ARK212-0_2021_spring_1; u... | Add an exact case-insensitive Karakterskala -> assessment mapping in R/section_heading_map.R, retaining its text alongside the other asse... |
| ✓ | usn | teaching_methods | wrong_content | medium | widespread | usn_PPUFRI3-1_2021_autumn_1; usn_PPUEN3-1_2020_autumn_1; ... | For USN practice blocks, split explicit grading statements and practice assessment criteria into assessment; route explicitly identified ... |
| ✓ | fhs | assessment | wrong_content | medium | common | fhs_FHS8004-1_2022_autumn_1; fhs_AR301-1_2023_spring_1; f... | Remove examiner-arrangement, resit and dated distribution/submission sentences, plus assessment-semester-only notices. Omit Tidspunkt and... |
| ✓ | inn | cross_section | wrong_content | medium | common | inn_SOSP3002-1_2025_spring_1; inn_SOSP3002-1_2024_spring_... | Add narrow INN sentence moves from teaching_methods to coursework_requirements for ^Arbeidskravene fordrer and the AUD sentence containin... |
| ✓ | inn | cross_section | other | medium | common | inn_DOKHIS-1_2023_autumn_3; inn_AUD3003-1_2025_spring_1; ... | Add precise INN cleanup rules in .clean_sections() or .sentence_moves: drop the standalone general-admission sentence from course_content... |
| ✓ | nla | course_content | wrong_content | medium | common | nla_4MGL1NO201-1_2021_spring_2; nla_MGL1NO201-1_2020_autu... | Add an NLA-specific sentence/paragraph move in R/extract_sections.R from course_content to teaching_methods for this COVID digital-teachi... |
| ✓ | nla | teaching_methods | wrong_content | medium | common | nla_MGL1KH101-1_2018_autumn_1; nla_GL17KH1-1_2022_autumn_... | Extend the standalone compulsory-activity recognizer to cover Alle læringsaktiviteter er obligatoriske and move that sentence to coursewo... |
| ✓ | nord | cross_section | wrong_content | medium | common | nord_RET1009-1_2024_spring_2; nord_MFL390-1_2021_autumn_1... | Add Nord-specific logistics removal for dated Første forelesning and Avsluttende Q&A-forelesning lines, the standalone Eksamenssted sente... |
| ✓ | oslomet | assessment | other | medium | common | oslomet_ØAMAS5900-1_2025_autumn_1; oslomet_SSA5910-1_2021... | Add OsloMet sentence-level cleanup for submission-calendar notices, references to exam-plan deadlines, resit eligibility/attempt limits a... |
| ✓ | uib | cross_section | empty_placeholder | medium | common | uib_INTH321A-0_2025_spring_1; uib_INTH356-0_2023_autumn_1... | Extend the anchored placeholder patterns to the observed UiB language notices (including the optional leading T and following Sjå dei eng... |
| ✓ | uio | course_content | wrong_content | medium | common | uio_NOR4515-1_2025_autumn_1; uio_PROMO4-1_2025_autumn_4; ... | Add narrowly scoped UiO paragraph-routing rules for introductions beginning Emnet inneheld followed by a number of praksisdagar and PROMO... |
| ✓ | uio | teaching_methods | other | medium | common | uio_BIOS5000-1_2025_autumn_1; uio_GEO5930L-2_2025_autumn_... | Add UiO sentence filters for the explicit Adgang til undervisning admission restriction, contact-only information sentence, insurance adv... |
| ✓ | uis | assessment | other | medium | common | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2021_autumn_1;... | Add UiS sentence/block cleanup for attempt entitlements, resit submission periods and resit supervision rules, including the quoted Forsk... |
| ✓ | vid | course_content | wrong_content | medium | common | vid_BASJE1010-1_2024_autumn_1; vid_MASJE1010-1_2024_autum... | Add VID-specific paragraph/sentence rules that move the explicit shared-teaching and individual-supervision arrangements from course_cont... |
| ✓ | himolde | reading_list | boilerplate_only | medium | occasional | himolde_SKD204-1_2022_spring_1; himolde_VBU730-1_2022_aut... | Exclude the himolde widget title/status/link-label nodes through its reader configuration while retaining actual reference nodes. After r... |
| ✓ | himolde | assessment | formatting_noise | medium | occasional | himolde_LOG904-140-1_2022_autumn_1; himolde_LOG904-131-1_... | Add himolde cleanup for due-date-only Duration values and weekday/time suffixes, retaining real durations such as 30 minutes (per group).... |
| ✓ | inn | assessment | other | medium | occasional | inn_AUD3003-1_2025_spring_1; inn_MUSB2900-1_2025_autumn_1 | Add sentence-level INN assessment cleanup matching ^Sensureres av faglærer og én ekstern sensor and ^Eksamensresultatet kan (?:ikke )?påk... |
| ✓ | nmbu | prerequisites | wrong_content | medium | occasional | nmbu_PPPE301-1_2025_autumn_1; nmbu_PPFD301-1_2025_autumn_1 | Strip the complete standalone sentence beginning Emnet er obligatorisk for studenter tatt opp til from NMBU prerequisites, including the ... |
| ✓ | nmbu | assessment | wrong_content | medium | occasional | nmbu_PPRA201-1_2025_autumn_1; nmbu_PPRA301-1_2025_autumn_1 | Add NMBU-specific sentence filters for the quoted repeat-attempt rule, the standalone continuous skikkethetsvurdering notice, and the sta... |
| ✓ | ntnu | assessment | wrong_content | medium | occasional | ntnu_MOL3901-2_2022_autumn_1; ntnu_TYSK1402-1_2025_autumn_1 | Add sentence-level NTNU cleanup for these explicit resubmission/repeat-entitlement formulations, preserving ordinary assessment component... |
| ✓ | uia | teaching_methods | wrong_content | medium | occasional | uia_SV-920-1_2020_autumn_1; uia_ORG952-1_2025_spring_1; u... | Strip UiA lines consisting solely of Undervisningsspråk or Litteratur followed by language names from section text. Retain substantive re... |
| ✓ | uio | assessment | other | medium | occasional | uio_MDID4009-1_2025_autumn_1; uio_GEO5930L-2_2025_autumn_1 | Add UiO sentence-level cleanup for delivery-system/deadline and examiner-composition sentences, plus a clause-level removal of the 3–6-we... |
| ✓ | uis | course_content | other | medium | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | For UiS text fallback, recognize an exact standalone Arbeidsmengd/Arbeidsmengde line as a .drop boundary even without a preceding blank l... |
| ✓ | uis | teaching_methods | other | medium | occasional | uis_MGL2120-1_2021_autumn_1; uis_MGL2311-1_2021_autumn_1 | Strip UiS workload spans beginning with Anslag over tidsforbruk or Forventet timeinnsats from teaching_methods, stopping after the contig... |
| ✓ | uit | teaching_methods | wrong_content | medium | occasional | uit_LRU-2580-1_2013_autumn_1; uit_FIN-1025-1_2015_autumn_1 | Extend teaching-method cleanup to the anchored Kvalitetssikring av emnet block and the observed course-evaluation sentences; retain stude... |
| ✓ | vid | assessment | other | medium | occasional | vid_MARUS5020-1_2024_autumn_1; vid_MARUS6021-1_2024_autum... | Add VID sentence-level cleanup for the observed Utsatt eksamen / Ved utsatt eksamen rules, VID arrangerer inntil ... utsatte prøver, and ... |
| ✗ | himolde | course_content | formatting_noise | medium | rare | himolde_LOG904-130-1_2018_autumn_1 | Add a himolde body-line exclusion for standalone Lecturer: staff/affiliation lines before section assignment (or the equivalent narrow se... |
| ✓ | hiof | teaching_methods | truncated | medium | rare | hiof_ITF10306-1_2013_spring_1 | Add a HiOF-specific inline extraction rule that moves explicit lecture/exercise statements such as this hours-per-week sentence from Unde... |
| ✓ | hiof | assessment | other | medium | rare | hiof_SFB50314-1_2016_spring_1 | Add narrowly anchored HiOF English sentence/paragraph filters for the observed In the case of a Fail and If the candidates wish to improv... |
| ✓ | hivolda | coursework_requirements | wrong_content | medium | rare | hivolda_MPRO110-1_2022_autumn_1 | Add a narrow hivolda .section_noise pattern for the quoted standalone Canvas-monitoring notice in R/extract_sections.R; keep it in course... |
| ✓ | steiner | assessment | wrong_content | medium | rare | steiner_M-PEL2.2_2025_autumn_1 | Add Steiner-specific sentence cleanup for the examiner-only, diploma-recording, handbook/learning-platform appeal pointer and failed-assi... |
| ✓ | uib | assessment | wrong_content | medium | rare | uib_BIO399-0_2025_autumn_1 | Add UiB-specific cleanup for the exact Fristar submission-deadline/template block and the standalone sentence Sensor og veileder skal vær... |
| ✓ | usn | prerequisites | wrong_content | medium | rare | usn_MBVA100-1_2022_autumn_2 | Add a narrow USN cleanup rule for a prerequisites block beginning "Ingen forkunnskaper." followed by "Studentene vil få tilgang til": rem... |
| ✓ | himolde | reading_list | empty_placeholder | low | widespread | himolde_LOG904-185-1_2025_autumn_1; himolde_SAF230-1_2025... | Extend whole-row placeholder recognition for these exact Norwegian/English variants, including TBA, and remove a leading Pensumoversikt w... |
| ✓ | nmbu | reading_list | empty_placeholder | low | widespread | nmbu_PPPE301-1_2025_autumn_1; nmbu_PPRA201-1_2025_autumn_... | Extend the anchored whole-row reading-list placeholder patterns to cover Kunngjøres i Canvas, Blir opplyst på Canvas ved oppstart, Oversi... |
| ✓ | fhs | cross_section | empty_placeholder | low | common | fhs_CBU1506-1_2019_autumn_2; fhs_SPR2723-1_2024_autumn_1;... | Add section-aware whole-row placeholder rules for these bare values, suppress Hva lærer du without a body, and recognise a Leganto-only p... |
| ✓ | nla | teaching_methods | formatting_noise | low | common | nla_MGL1KH101-1_2017_autumn_2; nla_MGL1KH101-1_2022_autum... | Extend .praksis_pointer_regex with an anchored match for Det henvises til egen praksisplan, allowing a final period. Remove only this poi... |
| ✓ | uis | reading_list | formatting_noise | low | common | uis_MGL2120-1_2024_autumn_1; uis_MGL2311-1_2021_autumn_1;... | Remove standalone widget item-type labels and View online access labels from UiS reading_list text, including URL-attached and line-wrapp... |
| ✓ | vid | assessment | formatting_noise | low | common | vid_BASJE1010-1_2025_autumn_1; vid_MASJE1010-1_2024_autum... | During VID assessment cleanup, remove a terminal standalone Hjelpemidler tillatt: or Additional Information about Assessment: when no val... |
| ✓ | hivolda | cross_section | empty_placeholder | low | occasional | hivolda_MD201-1_2015_autumn_1; hivolda_MD105-1_2016_spring_1 | Add an anchored (?:se\|sjå) engelsk versjon placeholder variant and suppress learning-outcome rows whose remaining text consists solely of... |
| ✓ | nih | prerequisites | empty_placeholder | low | occasional | nih_MA515-1_2025_autumn_1; nih_KRØ101-1_2025_autumn_1 | Extend the whole-text prerequisite placeholder check to match Ingen krav til emner with the optional suffix på programnivå, and the obser... |
| ✓ | usn | cross_section | empty_placeholder | low | occasional | usn_FKHPRA-1_2023_autumn_1; usn_FKHPR-1_2023_autumn_1; us... | Add anchored placeholder alternatives for "Se gjeldende praksisplan for faglærerutdanningen" and "Som studieprogrammet" to .placeholder_p... |
| ✓ | himolde | course_content | formatting_noise | low | rare | himolde_ADM420-1_2025_spring_1 | Drop standalone himolde availability-notice lines beginning The course will not be offered, including this date-stripped fragment, from c... |
| ✓ | hiof | reading_list | formatting_noise | low | rare | hiof_SFB50314-1_2016_spring_1 | Strip standalone staff-attribution lines beginning Lecturer: from extracted section text, or recognize that role line as an excluded bloc... |
| ✓ | hvl | all | empty_placeholder | low | rare | hvl_PRV002-1_2020_autumn_1 | Add an exact, case-insensitive test placeholder to .placeholder_phrases, using the existing whole-text matching rather than a substring f... |
| ✓ | nih | assessment | other | low | rare | nih_MA515-1_2025_autumn_1 | Add an anchored NIH-specific .section_noise pattern removing the complete sentence beginning Vurderingen gjøres av veileder ved praksisst... |
| ✓ | nla | assessment | other | low | rare | nla_IKF213-1_2019_autumn_1 | Add a narrowly anchored NLA logistics-sentence rule for I samme semester som prosjektoppgaven leveres, skal studentene fremstille seg for... |
| ✓ | nmbu | teaching_methods | wrong_content | low | rare | nmbu_PPFO301-1_2025_autumn_1 | Remove NMBU sentences matching Oversikt over samlingene vil bli lagt fram ... from teaching_methods, preserving profile labels where need... |
| ✓ | oslomet | course_content | field_or_language_junk | low | rare | oslomet_TAB2200-1_2024_autumn_1 | Strip anchored standalone Undervisningsspråk: <language> metadata lines from course_content during section cleanup, preserving substantiv... |
| ✓ | steiner | assessment | formatting_noise | low | rare | steiner_M-MAT1_2_2025_autumn_1 | In the Steiner text reader, recognize an exact standalone Ny/utsatt eksamen line as a .drop boundary even without a preceding blank line.... |
| ✓ | uio | coursework_requirements | duplicate | low | rare | uio_BIOS5000-1_2025_autumn_1 | After inline routing, deduplicate exact normalized requirement sentences within coursework_requirements, preserving their first occurrenc... |
| ✓ | uis | course_content | duplicate | low | rare | uis_LNOR115-1_2021_autumn_1 | Emit same-section text headings as heading boundaries so sectionize() flushes separate chunks before merging; the existing normalized chu... |
