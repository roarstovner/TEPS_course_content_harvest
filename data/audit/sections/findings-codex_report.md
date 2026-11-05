# sections audit — aggregated findings

51 findings across 7 institutions (from 7 agent reports in `data/audit/sections/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 50 passed (✓), 1 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

| institution | target | error_type | severity | problem | evidence |
| --- | --- | --- | --- | --- | --- |
| ntnu | coursework_requirements | wrong_content | high | not in packet: ntnu_PSYPRO4800-1_2021_autumn_1, ntnu_BIM1023-1_2022_autumn_1 | Det er krav om 80 % oppmøte på disse seminarene. |

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| cross_section | wrong_content | 5 | fhs, hivolda, nord, uia, uit | high |
| assessment | wrong_content | 4 | fhs, ntnu, phs, uit | high |
| coursework_requirements | wrong_content | 4 | fhs, hivolda, nord, ntnu | high |
| cross_section | empty_placeholder | 4 | fhs, hivolda, ntnu, uit | high |
| teaching_methods | wrong_content | 4 | ntnu, phs, uia, uit | high |
| prerequisites | missing_section | 3 | fhs, nord, uit | high |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| fhs | 8 | 7 | coursework_requirements / wrong_content; coursework_requirements / missing_section; assessment / wrong_content; prerequisites / missing_section; prerequisites / merged_sections; cross_section / empty_placeholder | cross_section / wrong_content | course_content / wrong_content; cross_section / duplicate |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| wrong_content | high | 27 |
| wrong_content | medium | 7 |
| missing_section | high | 5 |
| truncated | high | 3 |
| duplicate | high | 2 |
| empty_placeholder | high | 2 |
| empty_placeholder | low | 2 |
| boilerplate_only | high | 1 |
| merged_sections | high | 1 |
| other | high | 1 |

## By target

| target | n |
| --- | --- |
| cross_section | 19 |
| prerequisites | 8 |
| assessment | 7 |
| coursework_requirements | 7 |
| teaching_methods | 6 |
| course_content | 2 |
| all | 1 |
| learning_outcomes | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 18 | Reviewed all 18 sampled courses (14 suspects, four random controls) in full as displayed, plus both attendance appendix items: 2 correct and 0 wrong (covering all 5 moved sentences/plans); packet-builder truncation is not an extractor defect. The FHS prerequisite and teaching heading fixes hold i... |
| hivolda | gpt-6.1-sol | 16 | Reviewed all 16 sampled courses (14 suspects, 2 controls) and all 21 attendance appendix items: 19 correct moves and 2 wrong moves (ATT 11 and ATT 15, cut at jf./Jf.); no residual or duplicated attendance requirements were visible in the sampled sections. Most boilerplate flags reflect legitimate... |
| nord | gpt-6.1-sol | 20 | Reviewed all 20 sampled courses (14 suspects and 6 random controls) and all 18 attendance appendix items: 18 correct moves and 0 wrong moves; no moved attendance sentence was observed duplicated in a sampled source section. Most boilerplate flags are false alarms on valid repeated teaching/attend... |
| ntnu | gpt-6.1-sol | 20 | All 20 sampled courses (14 suspects and 6 random controls) were read in full: the suspects chiefly expose English-version pointers incorrectly emitted as substantive sections, while empty flags on Seminar, Workshop and Øvinger and boilerplate on genuine mandatory activities are false alarms. All ... |
| phs | gpt-6.1-sol | 14 | Section extraction is generally sound across all 14 sampled courses: the nine suspects have false-positive heading-leak flags (instructor learning outcomes, formative progression criteria, and introductory references to learning outcomes), and the observed PHS attendance, arbeidskrav-feedback, wo... |
| uia | gpt-6.1-sol | 20 | The main heading split is largely intact across all 20 sampled courses, but mandatory requirements still leak into teaching methods, workload contaminates 16 teaching sections, and several local semantic errors remain. All 20 attendance appendix items were judged correct and 0 wrong; the sampled ... |
| uit | gpt-6.1-sol | 20 | Reviewed all 20 sampled courses (14 suspects, 6 random controls) and all 18 attendance appendix items: 18 correct moves, 0 wrong moves. The widened attendance moves are sound and no moved sentences are duplicated in their sampled source sections, but an unflagged control still leaves mandatory at... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nord | cross_section | wrong_content | high | widespread | nord_MAT5006-1_2022_spring_2; nord_SOS5003-1_2020_autumn_... | Add bounded Nord sentence moves for the repeated supervision and thesis-submission constructions, sending supervision to teaching_methods... |
| ✓ | ntnu | cross_section | empty_placeholder | high | widespread | ntnu_TØL4010-1_2019_autumn_1; ntnu_IMT4895-1_2025_autumn_... | Extend case-insensitive placeholder recognition to se engelsk versjon with optional punctuation and for emnebeskrivelse, and the paired l... |
| ✓ | uit | assessment | truncated | high | widespread | uit_FIN-1025-1_2015_autumn_1; uit_FYS-8804-1_2019_autumn_... | Handle combined language blocks before .drop: move explicit exam-language sentences to assessment and retain a shared bare language value... |
| ✓ | fhs | cross_section | wrong_content | high | common | fhs_LED3201-1_2022_spring_1; fhs_INGOF2230-1_2025_spring_... | Recognise Praktisk organisering og arbeidsformer and explicit teaching lead-ins as teaching boundaries; route supplemental final-delivera... |
| ✓ | fhs | prerequisites | missing_section | high | common | fhs_ING3511-1_2021_autumn_1; fhs_CBU1503-1_2025_spring_1;... | Add guarded sentence patterns for Emnet bygger på/bygger videre på ferdighetene and The course is based on when they explicitly name prio... |
| ✓ | hivolda | cross_section | wrong_content | high | common | hivolda_PPUD-DID2KH-1_2023_autumn_1; hivolda_PPUD-DID2MU-... | Add a hivolda-specific paragraph relocation in R/extract_sections.R for the observed Ein del av undervisninga ... Felles fagdidaktikk var... |
| ✓ | hivolda | cross_section | wrong_content | high | common | hivolda_PPUD-DID2KH-1_2024_autumn_1; hivolda_PPUD-DID2MU-... | Relocate the exact standalone Bestått didaktikkemne 1. sentence from hivolda coursework_requirements to prerequisites in R/extract_sectio... |
| ✓ | nord | prerequisites | truncated | high | common | nord_MAT5006-1_2022_spring_2; nord_SAM5003-1_2022_spring_... | Read Nord Adgangsregulering blocks as prerequisite candidates, strip programme-admission sentences, retain prior-course/credit/FoU requir... |
| ✓ | nord | prerequisites | wrong_content | high | common | nord_SOS5003-1_2021_autumn_1; nord_PRA5015-1_2024_autumn_... | Add narrowly scoped admission-only patterns for these Nord constructions, including ellers ingen forkunnskapskrav; remove those sentences... |
| ✓ | nord | cross_section | wrong_content | high | common | nord_PRA5015-1_2024_autumn_3; nord_MFL390-1_2021_autumn_1 | Recognize PR - Eksamen and RUTINER VED TVIL OM BESTÅTT PRAKSISOPPLÆRING as assessment sub-blocks, move their assessment-design text to as... |
| ✓ | uia | cross_section | wrong_content | high | common | uia_PED420-1_2021_autumn_1; uia_MA-457-1_2025_autumn_1; u... | Add narrowly scoped sentence patterns for explicit mandatory participation, compulsory supervision hours and Det er obligatoriske arbeids... |
| ✓ | uit | cross_section | empty_placeholder | high | common | uit_JUR-8004-1_2014_autumn_1; uit_FYS-3820-1_2019_autumn_... | Add anchored whole-placeholder patterns for these observed variants, including repeated placeholder-only paragraphs; remove them before k... |
| ✓ | uit | course_content | wrong_content | high | common | uit_MFA-0004-1_2020_spring_1; uit_MBI-3016-1_2020_spring_... | Apply sentence-level admission/study-right and administrative-deadline cleanup to UiT course_content; first preserve and route any stated... |
| ✓ | uit | cross_section | wrong_content | high | common | uit_JUR-3003-1_2019_spring_1; uit_JUR-1004-1_2018_autumn_... | Add deterministic UiT progression/prior-course sentence rules for bestått 3. avdeling and JUR-0500, examine assessment and course_content... |
| ✓ | uit | cross_section | wrong_content | high | common | uit_JUR-3003-1_2019_spring_1; uit_JUR-3004-1_2018_autumn_... | Keep the coursework block active across its rule-link paragraphs until the explicit Eksamen består av boundary; include the later JUR-300... |
| ✓ | uit | assessment | wrong_content | high | common | uit_JUR-3901-1_2012_spring_1; uit_FIN-1025-1_2015_autumn_... | Add UiT sentence-level resit/postponement, registration and examiner-arrangement cleanup for the observed Norwegian/English variants; rec... |
| ✓ | fhs | cross_section | wrong_content | high | occasional | fhs_FHS8004-1_2022_autumn_1; fhs_MILM2408-1_2025_spring_1... | Strip target-group, security-clearance, fee/accommodation, equipment, dated schedule and workload-only spans from emitted sections. Treat... |
| ✓ | nord | cross_section | duplicate | high | occasional | nord_SOS5003-1_2021_autumn_1; nord_SOS5003-1_2020_autumn_2 | Split this deterministic mixed construction at the conjunction: retain Arbeidskrav in coursework_requirements and hjemmeeksamen (3 dager)... |
| ✓ | ntnu | teaching_methods | wrong_content | high | occasional | ntnu_SARB3008-1_2019_spring_1; ntnu_HBIOA2002-1_2021_autu... | Accept mandatory attendance at forkurs, and process an inline Obligatoriske arbeidskrav label with its complete requirement block until a... |
| ✓ | ntnu | teaching_methods | wrong_content | high | occasional | ntnu_MOL3901-2_2022_autumn_1; ntnu_KULT2211-1_2020_autumn_1 | Add sentence-level assessment-design routing for Form of examination and clear grade/exam-form statements, including their grade-adjustme... |
| ✗ | ntnu | coursework_requirements | wrong_content | high | occasional | ntnu_PSYPRO4800-1_2021_autumn_1; ntnu_BIM1023-1_2022_autu... | Detect disse/disse seminarene/disse samlingene and equivalent deictic attendance references before moving a sentence. Preserve the verbat... |
| ✓ | uia | cross_section | other | high | occasional | uia_HEL611-1_2021_autumn_1; uia_ST-301-1_2025_autumn_1 | Compare fulltext and section blocks for these two source IDs; bound .anon_uia staff removal at any structural heading, not only a colon-e... |
| ✓ | uit | prerequisites | missing_section | high | occasional | uit_FIN-1025-1_2015_autumn_1; uit_MBI-3016-1_2020_spring_2 | Before discarding admission blocks, extract paragraphs introduced by Anbefalte forkunnskaper or Recommended prerequisites into prerequisi... |
| ✓ | fhs | coursework_requirements | wrong_content | high | rare | fhs_FHS8004-1_2022_autumn_1 | Extend the bounded attendance matcher to Det er pliktig fremmøte, retaining safeguards for negation, optional participation and assessed ... |
| ✓ | fhs | coursework_requirements | missing_section | high | rare | fhs_ING3511-1_2021_autumn_1 | Recognise a bounded obligatory-supervision sentence as coursework_requirements and remove it from assessment while retaining all graded r... |
| ✓ | fhs | assessment | wrong_content | high | rare | fhs_FHS8004-1_2022_autumn_1 | When an assessment paragraph explicitly defines arbeidskrav and states that they do not count toward the grade, move the defining paragra... |
| ✓ | fhs | coursework_requirements | wrong_content | high | rare | fhs_OPG3302-1_2022_autumn_1 | Split this FHS thesis block at Arbeidet skal presenteres skriftlig i form av en oppgave and route the final thesis and word-limit paragra... |
| ✓ | fhs | prerequisites | merged_sections | high | rare | fhs_SPR2710-1_2023_spring_1 | Within FHS prerequisite blocks, terminate prior-knowledge text at a separate paragraph beginning Emnet er strukturert and route that topi... |
| ✓ | nord | prerequisites | missing_section | high | rare | nord_SPD5002-1_2022_spring_2 | Add bounded Norwegian prior-study constructions such as fulgt emnene and gjennomført emnene to the real-prerequisite guard so mixed admis... |
| ✓ | nord | coursework_requirements | truncated | high | rare | nord_REL1004-1_2019_autumn_2 | Add a bounded pattern for excursions described as part of obligatory participation, moving the requirement and its immediately referring ... |
| ✓ | nord | coursework_requirements | wrong_content | high | rare | nord_REL1004-1_2019_autumn_2 | Recognize the bounded contrast construction arbeidskrav ... , andre går inn i mappevurderinga and split at the clause boundary, assigning... |
| ✓ | ntnu | assessment | wrong_content | high | rare | ntnu_TØL4012-1_2024_autumn_1 | Recognize OBLIG/OBLIGer/OBLIG'er as mandatory submissions in sentence-level coursework routing when no grading language applies. Preserve... |
| ✓ | ntnu | cross_section | duplicate | high | rare | ntnu_HBIOA2002-1_2021_autumn_1 | Classify percentages attached to Delta laboratory exercises and other participation requirements as attendance rather than grade weights.... |
| ✓ | phs | teaching_methods | wrong_content | high | rare | phs_ETF-MISHAN-1_2024_autumn_1 | Extend the deterministic gate handling to move explicit absence sanctions such as this sentence to coursework_requirements, together with... |
| ✓ | phs | assessment | wrong_content | high | rare | phs_HUNDVÅPEN-1_2024_autumn_1 | Split the dog-handler assessment block at the explicit progression and level-check conditions. Move the progression requirement, mandator... |
| ✓ | uia | course_content | wrong_content | high | rare | uia_MA-457-1_2025_autumn_1 | Recognize an exact paragraph-start Faget i praksis. label in UiA blocks, split off its following text as a teaching_methods block, and en... |
| ✓ | uia | prerequisites | boilerplate_only | high | rare | uia_FIL908N-1_2025_spring_1 | When a general-admission sentence is removed, also remove its immediately following I tillegg ... documentation/years-of-employment conti... |
| ✓ | uia | prerequisites | wrong_content | high | rare | uia_BIO601-1_2025_autumn_1 | Remove the specific course-content committee-approval formulation from prerequisites and keep a single occurrence under the stated course... |
| ✓ | uia | learning_outcomes | wrong_content | high | rare | uia_BIO601-1_2025_autumn_1 | Add a UiA BIO601-compatible split before the exact After completing the course, the candidate is expected to: lead-in: move the preceding... |
| ✓ | uit | all | missing_section | high | rare | uit_BIO-3331-1_2009_spring_1 | Extend the UiT legacy field reader to recognize these inline field labels, including Course contents, Objective of the course, Teaching m... |
| ✓ | uit | cross_section | wrong_content | high | rare | uit_JUR-3901-1_2012_spring_1 | Recognize these anchored compulsory-lecture and vilkår for å levere ... til bedømmelse forms; move the gate and its attendance-exemption ... |
| ✓ | uit | cross_section | wrong_content | high | rare | uit_MAT-1002-1_2010_spring_1 | Split anchored Tillatte hjelpemidler til eksamen labels inside reading_list and route the heading and following aids list to assessment; ... |
| ✓ | uia | teaching_methods | wrong_content | medium | widespread | uia_MA-457-1_2024_autumn_2; uia_FYS129-G_2020_autumn_2; u... | Add UiA sentence removal for explicit total expected course-workload estimates, including the approximately/ca. forms and unpunctuated li... |
| ✓ | fhs | assessment | wrong_content | medium | common | fhs_FHS8004-1_2022_autumn_1; fhs_AR301-1_2023_spring_1; f... | Remove examiner-arrangement, resit and dated distribution/submission sentences, plus assessment-semester-only notices. Omit Tidspunkt and... |
| ✓ | nord | cross_section | wrong_content | medium | common | nord_RET1009-1_2024_spring_2; nord_MFL390-1_2021_autumn_1... | Add Nord-specific logistics removal for dated Første forelesning and Avsluttende Q&A-forelesning lines, the standalone Eksamenssted sente... |
| ✓ | ntnu | assessment | wrong_content | medium | occasional | ntnu_MOL3901-2_2022_autumn_1; ntnu_TYSK1402-1_2025_autumn_1 | Add sentence-level NTNU cleanup for these explicit resubmission/repeat-entitlement formulations, preserving ordinary assessment component... |
| ✓ | uia | teaching_methods | wrong_content | medium | occasional | uia_SV-920-1_2020_autumn_1; uia_ORG952-1_2025_spring_1; u... | Strip UiA lines consisting solely of Undervisningsspråk or Litteratur followed by language names from section text. Retain substantive re... |
| ✓ | uit | teaching_methods | wrong_content | medium | occasional | uit_LRU-2580-1_2013_autumn_1; uit_FIN-1025-1_2015_autumn_1 | Extend teaching-method cleanup to the anchored Kvalitetssikring av emnet block and the observed course-evaluation sentences; retain stude... |
| ✓ | hivolda | coursework_requirements | wrong_content | medium | rare | hivolda_MPRO110-1_2022_autumn_1 | Add a narrow hivolda .section_noise pattern for the quoted standalone Canvas-monitoring notice in R/extract_sections.R; keep it in course... |
| ✓ | fhs | cross_section | empty_placeholder | low | common | fhs_CBU1506-1_2019_autumn_2; fhs_SPR2723-1_2024_autumn_1;... | Add section-aware whole-row placeholder rules for these bare values, suppress Hva lærer du without a body, and recognise a Leganto-only p... |
| ✓ | hivolda | cross_section | empty_placeholder | low | occasional | hivolda_MD201-1_2015_autumn_1; hivolda_MD105-1_2016_spring_1 | Add an anchored (?:se\|sjå) engelsk versjon placeholder variant and suppress learning-outcome rows whose remaining text consists solely of... |
