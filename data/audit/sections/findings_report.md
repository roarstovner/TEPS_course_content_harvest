# sections audit — aggregated findings

84 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: gpt-6.1-sol, sonnet).
Mechanical verification: 84 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 7 | hivolda, hvl, inn, nord, steiner, uib, uio | medium |
| assessment | wrong_content | 6 | hiof, nih, ntnu, oslomet, uio, uit | high |
| cross_section | wrong_content | 5 | nord, ntnu, oslomet, uit, usn | high |
| coursework_requirements | wrong_content | 4 | hvl, ntnu, uia, uio | high |
| prerequisites | wrong_content | 4 | ntnu, oslomet, uit, usn | high |
| prerequisites | boilerplate_only | 4 | hvl, nla, nord, uia | medium |
| assessment | truncated | 3 | hiof, ntnu, uit | high |
| teaching_methods | merged_sections | 3 | mf, nord, ntnu | high |
| prerequisites | empty_placeholder | 3 | hivolda, uib, uis | low |
| cross_section | empty_placeholder | 3 | hiof, ntnu, oslomet | medium |
| cross_section | other | 3 | hiof, ntnu, usn | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| nord | 5 | 7 | prerequisites / boilerplate_only; assessment / formatting_noise | teaching_methods / merged_sections; coursework_requirements / split_section; cross_section / duplicate; reading_list / missing_section; cross_section / wrong_content | coursework_requirements / wrong_content; assessment / missing_section; prerequisites / duplicate |
| ntnu | 6 | 8 | assessment / wrong_content; teaching_methods / merged_sections; cross_section / empty_placeholder | assessment / truncated; coursework_requirements / wrong_content; cross_section / wrong_content; prerequisites / wrong_content; cross_section / other | prerequisites / boilerplate_only; assessment / formatting_noise; course_content / duplicate |
| oslomet | 6 | 7 | prerequisites / truncated | cross_section / wrong_content; assessment / wrong_content; cross_section / merged_sections; prerequisites / wrong_content; cross_section / empty_placeholder; course_content / formatting_noise | assessment / formatting_noise; assessment / boilerplate_only; prerequisites / empty_placeholder; prerequisites / boilerplate_only; course_content / boilerplate_only |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 13 |
| wrong_content | high | 11 |
| empty_placeholder | low | 9 |
| wrong_content | medium | 8 |
| formatting_noise | medium | 4 |
| missing_section | high | 4 |
| other | medium | 4 |
| boilerplate_only | low | 3 |
| merged_sections | high | 3 |
| merged_sections | medium | 3 |
| truncated | high | 3 |
| wrong_content | low | 3 |
| boilerplate_only | medium | 2 |
| duplicate | low | 2 |
| missing_section | medium | 2 |
| other | low | 2 |
| split_section | high | 2 |
| truncated | medium | 2 |
| duplicate | high | 1 |
| empty_placeholder | medium | 1 |
| field_or_language_junk | medium | 1 |
| missing_section | low | 1 |

## By target

| target | n |
| --- | --- |
| cross_section | 19 |
| assessment | 17 |
| prerequisites | 15 |
| teaching_methods | 11 |
| coursework_requirements | 6 |
| reading_list | 6 |
| course_content | 5 |
| learning_outcomes | 3 |
| all | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | gpt-6.1-sol | 19 | Section boundaries and the displayed learning-outcome groups are generally preserved across the 19 courses, with localized problems in semantic assignment and cleanup. The boilerplate flag on completed practice, leak flags on litteraturstudium, didactics topics, prior-knowledge wording and the Li... |
| hivolda | sonnet | 19 | Section extraction works well for HVO: the fixed heading structure (Om emnet, Læringsutbytte, Praktisk organisering, Vilkår for å framstille seg til eksamen, exam table) maps cleanly, learning outcomes keep all three sub-groups, and arbeidskrav land in coursework_requirements. Only low-severity n... |
| hvl | sonnet | 20 | HVL extraction is largely accurate: headings map cleanly to the seven sections, learning outcomes keep all sub-groups, and arbeidskrav land in coursework_requirements rather than assessment. Remaining issues are minor: logistics/cost text in teaching_methods, a practice-description block filed as... |
| inn | sonnet | 20 | Section extraction for inn works well: all 20 courses (Norwegian and English page variants) have content, outcomes, methods, coursework (arbeidskrav / obligatoriske aktiviteter) and assessment in the right sections, with nothing lost; placeholders such as 'Ingen pensumliste' are correctly not emi... |
| mf | sonnet | 10 | Extraction is accurate for MF: course_content, coursework_requirements, assessment and learning_outcomes are cleanly cut and complete in all 6 random controls, and sub-headings in learning outcomes do not truncate anything. Two real defects: an inline 'Arbeidsform og organisering' block is absorb... |
| nih | sonnet | 20 | Section extraction for NIH works very well on all 20 random controls: the fixed heading set (Kort om emnet, Forkrav, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur) maps correctly, Arbeidskrav stay in coursework_requirements, admission text (Hvem ka... |
| nla | sonnet | 16 | Norwegian-language NLA plans (about 13 of 16) are extracted very well: the headings are regular, arbeidskrav stay out of assessment, and Kunnskap/Ferdigheter/Generell kompetanse stay inside learning_outcomes. The real defects are in the English-language plans (3GJ*), where several headings are un... |
| nmbu | sonnet | 8 | Section extraction works well for NMBU: all 8 plans have correct, complete and clean sections, with sub-headings kept inside learning outcomes and Obligatorisk aktivitet correctly separated from assessment. Both pre-pass flags are false alarms; the only loss is text under unmapped headings (Laeri... |
| nord | gpt-6.1-sol | 20 | Accordion boundaries and all learning-outcome groups are generally retained, but semantic separation is unreliable: coursework and other assessment/teaching text cross section boundaries, admission and exam logistics remain, and a random control loses its reading-list section. SOS5003's duplicate... |
| ntnu | gpt-6.1-sol | 20 | All 20 offerings (14 suspects and 6 controls) were read in full: heading boundaries and learning-outcome groups generally survive, but recurring semantic routing errors leave compulsory work and graded assessment in the wrong sections, including in random controls. Most short/empty flags on subst... |
| oslomet | gpt-6.1-sol | 16 | All 16 courses were reviewed: major section boundaries generally work, all learning-outcome groups survive, and exam aids and grading scales correctly remain in assessment. The four empty flags identify real placeholders; the four learning-outcome/teaching-method leak flags are false alarms from ... |
| steiner | sonnet | 13 | Section extraction works well for Steiner: learning outcomes (all three sub-groups), content, coursework requirements (Arbeidskrav) and assessment are cut correctly in all 13 courses, including the arbeidskrav-vs-assessment split. Only minor noise (page numbers, retained exam-logistics lines) and... |
| uia | sonnet | 20 | Section extraction works very well for UiA: the standard headings (Læringsutbytte, Innhold, Undervisnings- og læringsformer, Vilkår for å gå opp til eksamen, Eksamen) map correctly, sub-headings stay inside learning_outcomes, arbeidskrav go to coursework_requirements and the 'Ingen' placeholder i... |
| uib | sonnet | 20 | UiB section extraction works well: the page has a fixed, regular heading set, all mapped correctly, and coursework vs. assessment, prerequisites and learning outcomes (with Kunnskap/Ferdigheter sub-groups) are intact. Remaining defects are small: a stray 'Hjelpemiddel til eksamen' label in assess... |
| uio | sonnet | 10 | UiO extraction is mostly sound: course_content, learning_outcomes (sub-groups kept), prerequisites and reading_list are correct, and admission text is dropped. The recurring defects are the unmapped Karakterskala sub-heading appended to assessment, and coursework gates (arbeidskrav, reflection no... |
| uis | sonnet | 16 | Section splitting is largely correct for UiS: learning outcomes keep all Kunnskap/Ferdigheter/Generell kompetanse groups, prerequisites are clean, and the 'Vilkår for å gå opp til eksamen' block is separated from assessment. Most pre-pass flags (empty/boilerplate teaching_methods, blob on the MGL... |
| uit | gpt-6.1-sol | 20 | All 20 packet courses were read in full (14 suspects and 6 random controls); most top-level boundaries and all visible knowledge/skills/competence groups survive. Real defects include mixed canonical content, three courses with misplaced coursework, lost or misplaced subject prerequisites and exa... |
| usn | gpt-6.1-sol | 19 | Reviewed all 19 packet entries (7 suspects and 12 random controls): top-level splitting and the displayed learning-outcome groups are generally intact, but substantive allocation errors remain in mixed blocks, including controls. The MG1NA3/UHULV bibliography leaks, 050-E11 internal coursework-la... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nla | cross_section | missing_section | high | common | nla_3GJ322-1_2019_autumn_1; nla_3GJ302-1_2025_autumn_1 | Add patterns in R/section_heading_map.R: ^introduction$ -> course_content, ^teaching (and\|&) learning methods$ -> teaching_methods, ^fina... |
| ✓ | nord | teaching_methods | merged_sections | high | common | nord_MET2000-1_2022_spring_1; nord_SP204L-1_2022_autumn_3... | Read exact internal Arbeidskrav labels as sub-headings for Nord and extend bounded coursework extraction to teaching_methods, including E... |
| ✓ | nord | cross_section | wrong_content | high | common | nord_FOU1001-1_2024_autumn_2; nord_LED5018-1_2022_autumn_... | Add conservative Nord sentence/paragraph routing before cleanup: move explicit method-course and supervision arrangements from course_con... |
| ✓ | ntnu | assessment | wrong_content | high | common | ntnu_HRAD2005-A_2022_spring_3; ntnu_HBIOT2039-1_2025_autu... | Extend the NTNU inline-coursework routing to recognise approval criteria, lists introduced by Obligatoriske arbeidskrav består av, validi... |
| ✓ | ntnu | teaching_methods | merged_sections | high | common | ntnu_IMT4127-1_2022_autumn_1; ntnu_REA0012-1_2022_autumn_... | Apply gate routing to teaching_methods as well as assessment. Recognise Compulsory requirements and Krav for å kunne gå opp til ... as in... |
| ✓ | ntnu | assessment | truncated | high | common | ntnu_TMM4140-1_2007_autumn_1; ntnu_SARB8002-1_2007_autumn... | Add bounded sentence routing for explicit Vurderingsform: labels and statements linking a named component to sluttkarakteren, karakterset... |
| ✓ | hiof | assessment | wrong_content | high | occasional | hiof_LU15SAMG07-1_2007_autumn_2; hiof_LA201BKHV06-1_2007_... | Enable a HiØf sentence-level coursework pass and recognize approval-gate sentences beginning Arbeidskravene or Godkjente arbeidskrav, mov... |
| ✓ | hiof | teaching_methods | wrong_content | high | occasional | hiof_LMBPED40517-1_2025_spring_1; hiof_SFF35021-1_2025_sp... | Add a HiØf rule that moves explicit mandatory seminar/supervision sentences, including Det gis to obligatoriske veiledningstimer per oppg... |
| ✓ | nord | coursework_requirements | split_section | high | occasional | nord_SPD1001-1_2025_spring_1; nord_SPD1001-1_2025_autumn_... | Preserve the context of a moved coursework component: for a line introducing one assignment per module, also move consecutive Modul N tas... |
| ✓ | ntnu | coursework_requirements | wrong_content | high | occasional | ntnu_PG8301-1_2007_autumn_1; ntnu_BA8506-1_2007_autumn_1 | Before finalising NTNU coursework rows, reconcile standalone activity labels with the exam tab using exact normalised names and explicit ... |
| ✓ | ntnu | cross_section | wrong_content | high | occasional | ntnu_IMT4127-1_2022_autumn_1; ntnu_IMT4127-1_2022_spring_1 | Add a narrowly scoped NTNU IMT4127 block correction: route the recommended-knowledge block beginning Security Management Metrics encompas... |
| ✓ | ntnu | prerequisites | wrong_content | high | occasional | ntnu_HBIOT2039-1_2025_autumn_2; ntnu_NORX2001-1_2025_autu... | Extend the admission-only sentence patterns with these bounded exchange-student and partner-university agreement formulations. Drop HBIOT... |
| ✓ | oslomet | cross_section | wrong_content | high | occasional | oslomet_MSLV4500-1_2018_spring_1; oslomet_DATA3730-1_2022... | Add a scoped OsloMet sentence/block reassignment pass after sectionize(): move explicit prior-experience requirements such as Emnet forut... |
| ✓ | uit | cross_section | merged_sections | high | occasional | uit_ODO-6302-1_2019_autumn_1; uit_END-3602-1_2025_autumn_... | Add bounded UiT inline rules for Kunnskap i <discipline>, Kliniske ferdigheter i <discipline> and In detail, the students will, routing t... |
| ✓ | uit | cross_section | wrong_content | high | occasional | uit_ODO-6302-1_2019_autumn_1; uit_BED-6001-1_2008_autumn_... | Extend the UiT gate pass to bounded Arbeidskrav blocks under learning_outcomes and explicit compulsory-attendance sentences under teachin... |
| ✓ | uit | prerequisites | wrong_content | high | occasional | uit_RUS-3950-1_2008_autumn_1; uit_BIO-3607-1_2008_autumn_1 | Normalize forkunn-skaper for label matching and scan teaching_methods for the bounded Anbefalte forkunn-skaper passage, stopping before A... |
| ✓ | usn | cross_section | wrong_content | high | occasional | usn_050-E11-1_2018_autumn_1; usn_NETPRL-1_2022_spring_1; ... | Add narrowly scoped USN routing inside mixed blocks: move complete mandatory-attendance, absence and required-documentation statements fr... |
| ✓ | nord | cross_section | duplicate | high | rare | nord_SOS5003-1_2022_autumn_3 | For a short coordinated description matching Arbeidskrav og hjemmeeksamen, split at the conjunction and assign the verbatim exam phrase i... |
| ✓ | nord | reading_list | missing_section | high | rare | nord_SP204L-1_2022_autumn_3 | Add a conservative inline literature rule for Nord: identify an explicit required-textbook/course-material lead followed by author/title ... |
| ✓ | oslomet | prerequisites | truncated | high | rare | oslomet_BIOB3210-1_2025_autumn_2 | Join continuation lines connected by eller/og before applying sentence-level admission filtering, preserving the whole mixed condition wh... |
| ✓ | uit | prerequisites | missing_section | high | rare | uit_LER-3101-1_2025_spring_1 | Before discarding UiT Opptakskrav blocks, retain sentences explicitly stating forkunnskapskravet with completed subject qualifications or... |
| ✓ | uit | prerequisites | truncated | high | rare | uit_END-3602-1_2025_autumn_1 | Read UiT English Admission requirements blocks for explicit academic qualifications and subject-credit requirements and append those sent... |
| ✓ | uit | assessment | missing_section | high | rare | uit_BIO-8806-1_2008_autumn_1 | Before dropping combined instruction/exam-language blocks, retain explicit sentences about exam-question and answer languages in assessme... |
| ✓ | usn | cross_section | split_section | high | rare | usn_NORVU103-1_2018_autumn_1 | Extend USN inline-coursework routing to carry dependent follow-up sentences about the same assignment, including language and approval sc... |
| ✓ | mf | reading_list | missing_section | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1; mf_... | Check the MF page source for where the literature list is rendered and extend the MF fulltext/section_* selector to include it; otherwise... |
| ✓ | oslomet | assessment | wrong_content | medium | widespread | oslomet_NATA6110-1_2018_autumn_1; oslomet_BYGG2200-1_2025... | Add OsloMet sentence filters for resit/registration/appeal rules, examiner arrangements and standalone assessment-semester statements, in... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_RELDID4009-1_2025_sprin... | Map `Karakterskala` to .drop in R/section_heading_map.R like the sibling Eksamensspråk, Hjelpemidler and Adgang til ny eller utsatt eksam... |
| ✓ | uit | assessment | wrong_content | medium | widespread | uit_FYS-3730-1_2008_autumn_1; uit_BIO-3529-2_2025_autumn_... | Map the exact Re-sit examination heading to .drop and normalize soft hyphens in labels. Add bounded UiT cleanup for inline resit/appeal, ... |
| ✓ | uit | cross_section | wrong_content | medium | widespread | uit_ARK-3021-1_2008_autumn_1; uit_LER-3101-1_2025_spring_... | Add narrowly bounded UiT cleanup for programme reservation/enkeltemne/support-subject declarations, explicit course-evaluation notices, t... |
| ✓ | hiof | cross_section | other | medium | common | hiof_IRD21607-1_2007_autumn_2; hiof_SFT11608-1_2022_autum... | Add narrowly scoped HiØf sentence filters for Ved kontinuasjon, Ved ikke bestått ... taes på nytt, Klageadgang, Intern/ekstern sensur or ... |
| ✓ | nord | assessment | formatting_noise | medium | common | nord_LED5018-1_2022_autumn_2; nord_ENT5011-1_2022_autumn_... | Add narrow Nord cleanup patterns for the quoted StudentWeb notice, the bare Praktisk informasjon og retningslinjer for eksamen pointer, e... |
| ✓ | ntnu | cross_section | empty_placeholder | medium | common | ntnu_NORX2001-1_2025_autumn_2; ntnu_MOL3022-1_2019_autumn... | Extend whole-row placeholder matching with bounded patterns for Samme som followed only by a course code, Oppgis ved semesterstart/kursst... |
| ✓ | ntnu | cross_section | other | medium | common | ntnu_IMT4127-1_2022_autumn_1; ntnu_IDATA2002-1_2025_autum... | Add NTNU sentence-level section cleanup for explicit resit lead-ins (Re-sit:, Utsatt eksamen, Ved utsatt eksamen, Ny/utsatt eksamen, and ... |
| ✓ | uio | coursework_requirements | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_KJM5050-1_2025_spring_1 | Add 'Obligatoriske (forhold\|komponenter)' patterns mapped to coursework_requirements in R/section_heading_map.R, and let the uio section_... |
| ✓ | uis | cross_section | formatting_noise | medium | common | uis_LENG116-1_2015_autumn_1; uis_LFYMAS-1_2017_spring_1; ... | For uis, strip lines matching '(?m)^\s*Emne [A-ZÆØÅ0-9_]+, (?:BOKMÅL\|NYNORSK\|ENGELSK).*versjon.*$' and lines equal to '<emnekode> - <titl... |
| ✓ | uis | reading_list | formatting_noise | medium | common | uis_MGL3066-1_2025_autumn_1; uis_MGL3066-1_2023_autumn_1;... | In the uis cleanup, remove tokens matching 'https?://bibsys-ur\.userservices\.exlibrisgroup\.com/\S*(?:\s*\n?\S*rft[._]\S*)*' up to 'View... |
| ✓ | usn | prerequisites | wrong_content | medium | common | usn_IA4212-1_2019_spring_1; usn_NETPRL-1_2022_spring_1; u... | Extend sentence-level prerequisite cleanup with anchored Norwegian and English admission-pointer patterns, placement-application/contract... |
| ✓ | hiof | assessment | truncated | medium | occasional | hiof_SFT11717-1_2025_spring_1; hiof_SFB51216-1_2022_autumn_1 | Add HiØf sentence rules transferring the English-exam translation alternative from course_content and the portfolio-new-text restriction ... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Add a line-start pattern '^Forkunnskapskrav\b:?' mapped to prerequisites in R/section_heading_map.R, with the rest of the line kept as se... |
| ✓ | nord | prerequisites | boilerplate_only | medium | occasional | nord_IKT1013-1_2019_autumn_2; nord_SOS5003-1_2022_autumn_... | Extend sentence-level admission filtering for these exact formulations, including the source typo gjeldene and the standard gjeldende var... |
| ✓ | oslomet | cross_section | merged_sections | medium | occasional | oslomet_PVUB7000-1_2019_autumn_1; oslomet_PVUB7000-1_2019... | Add exact .drop mappings for Målgruppe and Skikkethetsvurdering in R/section_heading_map.R so their bodies end before the next mapped sub... |
| ✓ | oslomet | prerequisites | wrong_content | medium | occasional | oslomet_PVUB7000-1_2019_autumn_1; oslomet_PVUB7000-1_2019... | Extend .is_admission_sentence() with sentence-level patterns for these observed admission-priority and eligibility formulations. Preserve... |
| ✓ | uia | prerequisites | boilerplate_only | medium | occasional | uia_ORG966-1_2025_autumn_1; uia_ORG937-1_2025_autumn_1 | In .clean_sections() (or a UiA post_fn for sections) drop prerequisites paragraphs matching 'opptak\|rangeres\|studieplass\|udir\.no\|betinge... |
| ✓ | uio | assessment | wrong_content | medium | occasional | uio_KJM5050-1_2025_spring_1; uio_KJM5930L-2_2025_spring_1 | Extend the existing whole-item coursework-gate move (#283) to UiO assessment, with patterns such as 'arbeidskrav', 'obligatoriske aktivit... |
| ✓ | uit | teaching_methods | field_or_language_junk | medium | occasional | uit_BIO-3529-2_2025_autumn_2; uit_END-3602-1_2025_autumn_... | Add explicit handling for the exact English combined language subheading before the denylist and remove its metadata-only label/value fro... |
| ✓ | usn | cross_section | other | medium | occasional | usn_15DIGRAF-1_2019_autumn_1; usn_NORVU103-1_2018_autumn_... | Add narrowly anchored USN section-cleanup rules for generic plan-change reservations, bare regulation cross-references with no assessment... |
| ✓ | hvl | coursework_requirements | wrong_content | medium | rare | hvl_LSU301-1_2022_autumn_1 | Add a pattern for 'PRAKSIS ... STUDIEÅR' headings in R/section_heading_map.R (map to course_content or teaching_methods), so it ends the ... |
| ✓ | mf | teaching_methods | merged_sections | medium | rare | mf_RL1014-1_2025_autumn_1 | Add a line-start pattern for '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R (and make sure th... |
| ✓ | steiner | all | other | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Check the URL built for M-PEL2.2 in R/add_course_url.R and compare the emnekode in the page text with course_code; flag mismatches in the... |
| ✓ | uib | learning_outcomes | merged_sections | medium | rare | uib_VIT217-0_2025_autumn_1 | Make the UiB block reader split on 'Arbeids- og undervisningsformer' wherever it occurs (also inside the Emnebeskrivelse container), and ... |
| ✓ | uit | assessment | truncated | medium | rare | uit_FYS-3730-1_2008_autumn_1 | Retain the exact Oppgaven kan skrives ... sentence from the combined language field and append it to assessment before dropping metadata-... |
| ✓ | hivolda | learning_outcomes | formatting_noise | low | widespread | hivolda_MGL5-10SA1B-1_2025_spring_1; hivolda_MID112-1_201... | Add a leading-line strip in .clean_sections() (or an hivolda-specific rule) for lines matching '^I samsvar med det nasjonale kvalifikasjo... |
| ✓ | hivolda | assessment | formatting_noise | low | widespread | hivolda_ENG144-1_2022_spring_1; hivolda_JOP100-1_2025_aut... | Drop the fixed header row when flattening the HVO exam table (or reformat rows as labelled fields), so only data rows remain. |
| ✓ | inn | assessment | formatting_noise | low | widespread | inn_3JUS330-1_2023_autumn_1; inn_ØKA1026-1_2025_spring_1;... | Drop the table header line and the table data row (or keep only the exam form and grade scale) in .clean_sections() for INN assessment, a... |
| ✓ | inn | learning_outcomes | formatting_noise | low | widespread | inn_REGF2009-1_2025_autumn_1; inn_KS4102-1_2023_autumn_1;... | Keep the sub-heading lines as text inside learning_outcomes (Kunnskap / Ferdigheter / Generell kompetanse and the English equivalents). |
| ✓ | nmbu | teaching_methods | missing_section | low | widespread | nmbu_FYS100-1_2025_spring_1; nmbu_PPFD201-1_2025_autumn_2 | Map `Undervisningstider` and `Læringsstøtte` to teaching_methods in R/section_heading_map.R (optionally scoped to NMBU), after checking t... |
| ✓ | uit | reading_list | empty_placeholder | low | widespread | uit_BED-2017-1_2008_autumn_2; uit_ARK-3021-1_2008_autumn_... | Add reading-list-specific whole-row patterns for these pointer/contact variants, including the complete Leganto access-note plus Pensumli... |
| ✓ | usn | reading_list | formatting_noise | low | widespread | usn_LR-FYS1200-1_2018_autumn_2; usn_PB1090-1_2025_spring_... | Extend USN reading-list cleanup to remove redundant course-title/code identity text and standalone cohort/semester metadata, matching aga... |
| ✓ | hvl | teaching_methods | formatting_noise | low | common | hvl_NHB2-1_2019_spring_1; hvl_FK215-1_2022_autumn_1 | Add HVL-specific drop rules in .clean_sections()/.drop for lines on 'Kr \d+', 'materialpenger', 'egenandel', 'Eksamen blir avholdt' and '... |
| ✓ | mf | coursework_requirements | formatting_noise | low | common | mf_RL1010L-1_2025_autumn_2; mf_RL1014-1_2025_autumn_1 | In .clean_sections() for mf, strip the paragraph starting 'Studenter som ikke oppfyller de obligatoriske aktivitetene' and the 'Ved oppme... |
| ✓ | nla | teaching_methods | empty_placeholder | low | common | nla_4HSM211-1_2019_autumn_1; nla_GL17KRØ1-1_2017_autumn_1... | In .clean_sections() in R/extract_sections.R, drop lines that match ^(ingen\|ingen\.\|-\|nei)$ (case-insensitive) when they are the whole te... |
| ✓ | nla | teaching_methods | wrong_content | low | common | nla_MGL1KØ101-1_2025_autumn_1 | In .clean_sections(), drop paragraphs that consist only of a reference ('^(se\|sjå\|see)\b.*(praksis\|plan)') and keep 'Praksis' text otherw... |
| ✓ | oslomet | cross_section | empty_placeholder | low | common | oslomet_CORO7100-2_2018_autumn_1; oslomet_MSLV4500-1_2018... | Add exact placeholder patterns for these absence and pointer variants; remove standalone placeholder lines at section ends as well as beg... |
| ✓ | uib | assessment | formatting_noise | low | common | uib_PEDA130-0_2024_autumn_2; uib_EXFAC00SK-0_2025_autumn_... | Map 'Hjelpemiddel til eksamen' to .drop (or drop the line and its value when the value is Ingen/Ikkje relevant), per the spec's line betw... |
| ✓ | uis | course_content | duplicate | low | common | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1;... | In .clean_sections(), drop a paragraph in course_content that is a verbatim prefix of, or contained in, another paragraph of the same sec... |
| ✓ | hiof | cross_section | empty_placeholder | low | occasional | hiof_LDIGBHG10122-1_2022_autumn_2; hiof_IRD21607-1_2007_a... | Extend whole-row placeholder matching to ingen arbeidskrav i emnet and ny pensumbok vurderes. Apply the reading-list phrase only to readi... |
| ✓ | hvl | assessment | formatting_noise | low | occasional | hvl_ØAM112-1_2019_autumn_1; hvl_NHB2-1_2019_autumn_1; hvl... | Drop lines matching 'Tid (og sted )?for (eksamen\|ut- og innlevering)' and 'opplyst på (Studentweb\|emnets sider)' in assessment cleaning. |
| ✓ | hvl | all | empty_placeholder | low | occasional | hvl_Å-MU2A-1_2019_spring_3; hvl_ØKB3119-1_2017_autumn_1; ... | Treat '_', '-' and bare 'Ja'/'Nei' as empty in the placeholder filter (keep the longer 'Ja, vert spesifisert' variant as a short requirem... |
| ✓ | inn | teaching_methods | other | low | occasional | inn_LAN2300-1_2025_autumn_1; inn_2MKRLE171-6-1_2023_spring_1 | Map the heading 'Emneevaluering – kvalitetssystem' to .drop in R/section_heading_map.R; consider dropping the generic Praksisstudiet para... |
| ✓ | nla | course_content | formatting_noise | low | occasional | nla_3GJ302-1_2025_autumn_1; nla_GL17NO2-1_2017_autumn_1 | Insert a space or newline when joining adjacent inline or block nodes in the reader (R/blocks.R), or add a regex after extraction that sp... |
| ✓ | nla | prerequisites | boilerplate_only | low | occasional | nla_3GJ302-1_2025_autumn_1 | Add an English pattern such as '^view the admission requirements' (and 'admission requirements for') to the prerequisites boilerplate fil... |
| ✓ | oslomet | course_content | formatting_noise | low | occasional | oslomet_TOLKDIAPBT-1_2018_autumn_1; oslomet_ØAMET1000-1_2... | Add narrowly anchored OsloMet cleanup rules for the observed approval/revision line and the updated-reading-list navigation notice before... |
| ✓ | steiner | assessment | formatting_noise | low | occasional | steiner_M-MAT1_1_2025_spring_1; steiner_M-MAT1_2_2025_spr... | In .clean_sections() (R/extract_sections.R) drop lines matching ^\d{1,3}$ at the end of a section, and drop the Hjelpemidler/Sensorordnin... |
| ✓ | uib | prerequisites | empty_placeholder | low | occasional | uib_AHKR391-0_2019_autumn_1; uib_ENGMAU655-0_2025_spring_1 | In .clean_sections(), drop lines that are only a placeholder ('Ingen', 'Ingen.', '-') in prerequisites; optionally prefix the 'Anbefalte ... |
| ✓ | uis | course_content | boilerplate_only | low | occasional | uis_MGL2120-1_2025_autumn_1; uis_MGL2120-1_2024_autumn_1 | Add exact '.drop' rows for 'arbeidsmengd' and 'arbeidsmengde' in R/section_heading_map.R (or move the hours line to teaching_methods). |
| ✓ | uis | prerequisites | empty_placeholder | low | occasional | uis_LENG115-1_2017_autumn_1; uis_LENG115-1_2018_autumn_1 | Strip standalone 'Ingen'/'Ingen.' lines when other text is present in the section, and drop the sub-heading label line. |
| ✓ | uit | cross_section | formatting_noise | low | occasional | uit_BIO-3529-2_2025_autumn_2; uit_END-3602-1_2025_autumn_1 | Extend UiT’s exam-link exclusion or section cleanup to remove the complete English portal sentence from assessment and coursework_require... |
| ✓ | hivolda | reading_list | empty_placeholder | low | rare | hivolda_PPY121-1_2016_autumn_1 | Extend the reading_list placeholder pattern to cover '^Sjå pensumliste for emnet her$'. |
| ✓ | hivolda | prerequisites | empty_placeholder | low | rare | hivolda_DRHS902-1_2019_spring_1 | Treat '^Ingen\.? (Sjå\|Se) opptakskrav' as a placeholder in prerequisites cleaning. |
| ✓ | hvl | prerequisites | boilerplate_only | low | rare | hvl_ØAM112-1_2019_autumn_1 | Drop prerequisites lines equal to 'Studiets opptakskrav' / 'Opptakskrav' (also 'Studierett på Master ...' type lines if the spec treats t... |
| ✓ | nih | assessment | wrong_content | low | rare | nih_FRI304-1_2025_spring_1; nih_PPU405-1_2025_autumn_1 | Optional: in .clean_sections() move lines starting with 'Oppmøtekrav' or matching '^\d+ ?% (frammøte\|oppmøte\|deltakelse)' from assessment... |
| ✓ | uia | coursework_requirements | wrong_content | low | rare | uia_PSY914-G_2022_autumn_1 | Low priority: in .clean_sections() move sentences matching '^Forventet arbeidsomfang\|^Arbeidsomfang' from coursework_requirements to teac... |
| ✓ | uib | teaching_methods | duplicate | low | rare | uib_MUV295-0_2025_autumn_1 | No change needed. At most, deduplicate identical paragraphs across sections within a course in .clean_sections(). |
| ✓ | uio | course_content | other | low | rare | uio_HIS4015L-1_2025_spring_1 | Require the leak heading match to be a whole line, or word-bounded, in R/audit/qa_sections.R. No pipeline change needed. |
