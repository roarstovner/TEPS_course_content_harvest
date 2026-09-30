# sections audit — aggregated findings

166 findings across 18 institutions (from 18 agent reports in `data/audit/sections/findings`; model: sonnet).
Mechanical verification: 20 passed (✓), 1 failed (✗), 145 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

| institution | target | error_type | severity | problem | evidence |
| --- | --- | --- | --- | --- | --- |
| uio | assessment | boilerplate_only | medium | not in packet: uio_PROMO4-1_2025_spring_1 | Mer om eksamen ved UiO Kildebruk og referanser Hvordan bruke KI som student Tilrettelegging på ek... |

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| learning_outcomes | truncated | 10 | hivolda, inn, mf, nla, ntnu, steiner, uia, uis, uit, usn | high |
| assessment | formatting_noise | 9 | hiof, inn, mf, nord, ntnu, oslomet, uib, uis, usn | high |
| assessment | merged_sections | 7 | hiof, hvl, nmbu, steiner, uia, uis, uit | high |
| prerequisites | boilerplate_only | 7 | nla, nmbu, nord, ntnu, oslomet, uia, uio | high |
| assessment | wrong_content | 6 | hivolda, nord, ntnu, uib, uit, usn | high |
| course_content | merged_sections | 6 | inn, nmbu, steiner, uia, uis, uit | high |
| reading_list | wrong_content | 6 | hivolda, inn, mf, nmbu, uis, usn | high |
| course_content | formatting_noise | 5 | nih, oslomet, uib, uis, uit | high |
| coursework_requirements | merged_sections | 5 | hivolda, inn, nmbu, uit, usn | high |
| prerequisites | merged_sections | 5 | hivolda, hvl, nmbu, uib, uis | high |
| teaching_methods | missing_section | 5 | hivolda, inn, mf, steiner, uis | high |
| prerequisites | empty_placeholder | 5 | nih, nord, uia, uib, usn | low |
| reading_list | missing_section | 5 | hivolda, hvl, nord, ntnu, uio | medium |
| assessment | boilerplate_only | 4 | hivolda, hvl, uia, uio | high |
| learning_outcomes | merged_sections | 4 | hivolda, inn, uia, uit | high |
| reading_list | boilerplate_only | 4 | inn, mf, nih, nla | high |
| assessment | missing_section | 3 | nla, ntnu, usn | high |
| coursework_requirements | missing_section | 3 | hiof, nord, uio | high |
| coursework_requirements | wrong_content | 3 | hvl, mf, uis | high |
| teaching_methods | merged_sections | 3 | nmbu, ntnu, usn | high |
| learning_outcomes | formatting_noise | 3 | mf, oslomet, uib | medium |
| prerequisites | missing_section | 3 | inn, mf, nord | medium |
| teaching_methods | wrong_content | 3 | hiof, nord, uit | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| mf | 11 | 12 | course_content / missing_section; reading_list / boilerplate_only; learning_outcomes / truncated; learning_outcomes / formatting_noise; assessment / formatting_noise; prerequisites / missing_section | coursework_requirements / wrong_content; course_content / empty_placeholder; reading_list / formatting_noise; learning_outcomes / wrong_content; reading_list / wrong_content; teaching_methods / missing_section | course_content / wrong_content; coursework_requirements / missing_section; reading_list / merged_sections; assessment / boilerplate_only; assessment / merged_sections |
| nih | 7 | 3 | reading_list / boilerplate_only; prerequisites / empty_placeholder | course_content / formatting_noise | assessment / boilerplate_only; coursework_requirements / formatting_noise; prerequisites / missing_section; coursework_requirements / wrong_content; teaching_methods / other |
| uio | 9 | 5 | coursework_requirements / missing_section; assessment / boilerplate_only; prerequisites / boilerplate_only | reading_list / missing_section; teaching_methods / other | learning_outcomes / missing_section; course_content / merged_sections; prerequisites / wrong_content; teaching_methods / merged_sections; assessment / merged_sections; teaching_methods / formatting_noise |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| merged_sections | high | 21 |
| wrong_content | high | 14 |
| formatting_noise | low | 12 |
| missing_section | medium | 12 |
| merged_sections | medium | 11 |
| missing_section | high | 10 |
| wrong_content | low | 9 |
| boilerplate_only | low | 8 |
| boilerplate_only | medium | 8 |
| truncated | medium | 7 |
| empty_placeholder | low | 6 |
| formatting_noise | medium | 6 |
| wrong_content | medium | 6 |
| boilerplate_only | high | 5 |
| empty_placeholder | medium | 5 |
| formatting_noise | high | 5 |
| truncated | high | 4 |
| truncated | low | 4 |
| missing_section | low | 3 |
| other | low | 3 |
| duplicate | low | 1 |
| duplicate | medium | 1 |
| empty_placeholder | high | 1 |
| field_or_language_junk | high | 1 |
| field_or_language_junk | medium | 1 |
| other | high | 1 |
| split_section | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 35 |
| prerequisites | 24 |
| learning_outcomes | 23 |
| reading_list | 21 |
| course_content | 20 |
| coursework_requirements | 15 |
| teaching_methods | 14 |
| cross_section | 10 |
| all | 4 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof |  | 28 | The extractor performs well on course_content, learning_outcomes, teaching_methods, and reading_list sections across all 28 courses. The dominant failure mode — present in every single course in the sample — is that the assessment row starts with arbeidskrav items (gate-to-exam requirements) rath... |
| hivolda |  | 28 | The hivolda text-splitting extractor has severe, institution-wide labeling errors. The assessment section is universally captured as mandatory attendance boilerplate rather than the graded exam; the actual graded exam ends up misrouted into reading_list. course_content is missing from most course... |
| hvl |  | 28 | HVL's h3-based extraction is structurally sound — the seven headings are consistently mapped to the right canonical sections, and no sections are mislabeled in a gross sense. The dominant quality problem is that the 'Hjelpemidler ved eksamen' sub-section is universally merged into the assessment ... |
| inn |  | 28 | Extraction quality for inn is critically poor across all 28 reviewed courses. The text-splitter fails to recognise almost every heading as a cut point, causing systematic cascade merges: course_content bleeds the literal heading word 'Læringsutbytte' into its last line; learning_outcomes then ble... |
| mf | sonnet | 20 | MF section extraction is systematically wrong: obligatory activities are filed under assessment, course_content is only a heading stub while the real description is dropped, reading_list never contains a reading list (only library boilerplate plus the course-responsible person's name/email), and ... |
| nih | sonnet | 28 | Section extraction works very well for NIH: headings (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur) map cleanly, arbeidskrav are correctly kept out of assessment, and learning outcomes are complete. The only real issues are that rea... |
| nla |  | 28 | NLA's JSON-based extractor maps most structural sections correctly (course_content, learning_outcomes, teaching_methods, coursework_requirements), but has a systematic and near-universal failure in the assessment section: it captures the exam-language field ("Eksamensspråk") instead of the actual... |
| nmbu |  | 18 | NMBU extraction quality is poor across almost all sections. The h3-heading splitter consistently fails to cut at 'Dette lærer du', causing course_content to swallow the entire learning outcomes block in virtually every course; as a result learning_outcomes is absent from extractor output for ~15 ... |
| nord |  | 28 | Extraction quality for nord is moderate: the descriptive sections (course_content, learning_outcomes, teaching_methods) are usually clean and correctly labelled, but the assessment-cluster is systematically broken. Every reviewed course concatenates arbeidskrav/obligatorisk deltakelse (coursework... |
| ntnu |  | 28 | NTNU's h3-heading-based extractor correctly identifies and labels the five main sections present (course_content, learning_outcomes, teaching_methods, coursework_requirements, prerequisites), and section boundaries are accurate. The two critical defects are (1) the assessment section universally ... |
| oslomet |  | 28 | Two distinct populations. (1) The 20 suspects are praksis/grunnskolelærer offerings whose per-offering emneplan page is a stub: every heading literally reads 'Se fagplanen.', with the real content living in a separate fagplan document that nonetheless appears in extracted_text but is never sectio... |
| steiner |  | 13 | Steiner's text-splitting extraction is largely correct for the sections it does capture (learning_outcomes, course_content, coursework_requirements, assessment), but suffers from three consistent structural defects: (1) the heading word 'Vurdering' is invariably appended as a trailing artefact to... |
| uia |  | 28 | UiA's h2-heading-based extractor performs well on section boundaries for standard academic courses, but has two structural problems that affect most courses: (1) coursework requirements ('Vilkår for å gå opp til eksamen') are consistently merged into the assessment row because both appear under t... |
| uib |  | 28 | UiB's details/summary accordion + h2 hybrid extraction correctly opens accordion sections and maps headings to canonical sections for the vast majority of courses. Core academic sections (course_content, learning_outcomes, teaching_methods, coursework_requirements, assessment) are extracted with ... |
| uio | sonnet | 13 | Core sections (course_content, learning_outcomes, prerequisites, teaching_methods, assessment) are split cleanly at UiO's fixed headings, but coursework_requirements and reading_list are never emitted, and several generic boilerplate blocks plus un-anonymized contact details contaminate the secti... |
| uis |  | 28 | Extraction quality for UiS is poor-to-moderate, with two dominant structural failures affecting the majority of courses. First, a systematic page-break split in older PDF-sourced plans causes the learning_outcomes section to be truncated at the first page-boundary keyword (typically 'Kunnskap'), ... |
| uit |  | 28 | UiT extraction quality is severely degraded by a single structural failure: the heading-splitter treats 'Hva lærer du' (the sub-heading that introduces the learning outcomes section) as a continuation of 'Innhold', then fails to cut at 'Undervisnings- og eksamensspråk' / 'Undervisning' / 'Kvalite... |
| usn |  | 28 | USN extraction quality is mediocre-to-poor: the five sections that map cleanly to USN's flat heading structure (course_content, learning_outcomes, teaching_methods) are generally correct, but every course exhibits a structural boundary failure at the Eksamensformer heading. The text splitter cons... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ? | hiof | assessment | merged_sections | high | widespread | hiof_LMUMAT40117-1_2020_autumn_2; hiof_LMBENG10117-1_2018... | Add 'Arbeidskrav - vilkår for å avlegge eksamen' (and its common variants: 'Arbeidskrav', 'Obligatoriske aktiviteter', 'Obligatorisk læri... |
| ? | hiof | coursework_requirements | missing_section | high | widespread | hiof_LMUMAT40117-1_2021_autumn_1; hiof_LMBENG10117-1_2019... | Map the heading 'Arbeidskrav - vilkår for å avlegge eksamen' (and normalised variants) to coursework_requirements. The sentinel phrase 'A... |
| ? | hivolda | assessment | boilerplate_only | high | widespread | hivolda_MGL1-7MA1A-1_2017_autumn_2; hivolda_MGL1-7MA1A2-1... | Do not map 'Vilkår for å framstille seg til eksamen' to assessment. Map 'Frammøtekrav' lines to coursework_requirements and 'Arbeidskrav'... |
| ? | hivolda | reading_list | wrong_content | high | widespread | hivolda_MGL1-7SP1-1_2025_spring_1; hivolda_MGL1-7MA1A2-1_... | Add a post-processing rule to drop 'Emnet inngår i følgande studieprogram' and 'Godkjent av' lines from all sections. Map the Vurderingsf... |
| ? | hivolda | learning_outcomes | merged_sections | high | widespread | hivolda_MGL1-7MA1A2-1_2019_autumn_2; hivolda_MGL1-7MA2B-1... | Add 'Praktisk organisering og arbeidsmåtar' and 'Arbeids- og undervisningsformer' as heading triggers that close learning_outcomes and op... |
| ? | hivolda | course_content | missing_section | high | widespread | hivolda_MGL1-7MA1A2-1_2019_autumn_2; hivolda_MGL5-10PE4-1... | Add 'Om emnet' as a heading trigger mapped to course_content. Handle the early-appearing 'Pensum:' metadata header by treating it as a ke... |
| ? | hvl | assessment | merged_sections | high | widespread | hvl_LUPEKI203-1_2025_autumn_1; hvl_MGUMA101-1_2025_autumn... | Add 'Hjelpemidler ved eksamen' / 'Hjelpemiddel ved eksamen' to the heading map as a sentinel that closes the assessment section without o... |
| ? | inn | course_content | merged_sections | high | widespread | inn_2MEN5101-2-1_2022_autumn_1; inn_2MNK5101-3-1_2022_aut... | Add 'Læringsutbytte' to the heading pattern list so the splitter cuts before it. Apply the same fix to all other INN section headings lis... |
| ? | inn | learning_outcomes | merged_sections | high | widespread | inn_2MNK5101-7-1_2022_autumn_1; inn_2MPED-3-1_2023_spring... | Add the heading patterns 'Arbeids- og undervisningsformer', 'Undervisnings- og arbeidsformer', 'Læringsaktiviteter', and 'Praksis' to the... |
| ? | inn | teaching_methods | missing_section | high | widespread | inn_2MEN5101-2-1_2022_autumn_1; inn_2NOL51-5-1_2022_autum... | Map the heading pattern 'Arbeids[-\s]og\s+undervisningsformer' (and variants) to teaching_methods in the inn section-heading map. |
| ? | inn | coursework_requirements | merged_sections | high | widespread | inn_2MNK5101-3-1_2022_autumn_1; inn_2NOL51-5-1_2022_autum... | Add 'Eksamen', 'Vurderinger', 'Vurdering', 'Obligatoriske aktiviteter', 'Arbeidskrav' to the heading cut list. Map 'Eksamen'/'Vurderinger... |
| ? | inn | assessment | truncated | high | widespread | inn_2MNK5101-3-1_2022_autumn_1; inn_2NOL51-5-1_2022_autum... | Fix the heading cut for 'Eksamen'/'Vurderinger'/'Vurdering' as described above; also add a post-processing step to strip the raw assessme... |
| ✓ | mf | coursework_requirements | wrong_content | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | Add a pattern '^Obligatoriske aktiviteter' (and 'Listen over obligatoriske aktiviteter') mapped to coursework_requirements in R/section_h... |
| ✓ | mf | course_content | missing_section | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | In R/extract_sections.R add a preamble rule for MF: text between the 'Timeplan' line (end of the Emneinfo/Studieprogramtilhørighet block)... |
| ✓ | mf | reading_list | boilerplate_only | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_PED5610-1_2025_autumn_1; ... | Strip the fixed 'Tilgang til litteratur' paragraph and the stub line 'Litteraturlisten for ...' in .clean_sections(); emit no reading_lis... |
| ✓ | mf | reading_list | formatting_noise | high | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1014-1_2025_autumn_1; mf_... | In .clean_sections() (or an MF hook) truncate every section at the first line matching '^Emneansvarlig$' and drop everything after it. |
| ? | nla | assessment | field_or_language_junk | high | widespread | nla_4MGL1PE101-1_2025_autumn_1; nla_4MGL1PE102-1_2025_aut... | In the NLA JSON extraction function, remap the 'assessment' field to the JSON key for "Avsluttende vurdering" (the main graded-assessment... |
| ? | nla | reading_list | boilerplate_only | high | widespread | nla_4MGL1PE101-1_2025_autumn_1; nla_MGL1PE101-1_2025_autu... | If the NLA JSON does not include actual reading-list content, emit no reading_list row rather than emitting the link text. Optionally cap... |
| ? | nla | assessment | missing_section | high | widespread | nla_4MGL1PE201-1_2025_autumn_1; nla_MGLSPES101-1_2025_aut... | Same fix as the field_or_language_junk finding: remap to the "Avsluttende vurdering" JSON key. |
| ? | nmbu | course_content | merged_sections | high | widespread | nmbu_MATH100-1_2025_autumn_1; nmbu_FYS100-1_2025_autumn_1... | Add 'Dette lærer du', 'Læringsutbytte', and 'Læringsmål' to the heading-to-section map, mapping them to learning_outcomes. Cut course_con... |
| ? | nmbu | learning_outcomes | missing_section | high | widespread | nmbu_MATH100-1_2025_autumn_1; nmbu_PPFD301-1_2025_autumn_... | Same fix as course_content merger: map 'Dette lærer du' to learning_outcomes and cut there. Then emit a learning_outcomes row containing ... |
| ? | nmbu | assessment | merged_sections | high | widespread | nmbu_PPXP100-1_2025_autumn_1; nmbu_PPPE301-1_2025_autumn_... | Map 'Obligatorisk aktivitet' as a boundary that terminates assessment and starts coursework_requirements. Map 'Undervisningstider', 'Fort... |
| ? | nord | coursework_requirements | missing_section | high | widespread | nord_PEL1001-1_2022_autumn_1; nord_SAM1001-1_2018_autumn_... | Add a post-split rule: within the assessment block, move lines/paragraphs matching ^(Arbeidskrav\|Arbeidskrav \(AK\)\|AK:\|Obligatorisk delt... |
| ? | nord | assessment | wrong_content | high | widespread | nord_PEL1003-1_2017_autumn_2; nord_SAM1001-1_2018_autumn_... | After the carve-out rule above runs, the residual assessment row should retain only the graded component(s) (lines with 'teller 100/100',... |
| ? | nord | prerequisites | boilerplate_only | high | widespread | nord_PEL1001-1_2022_autumn_1; nord_KHV1004-1_2021_autumn_... | Drop prerequisites content matching admin patterns: 'Ingen utover opptakskravet', 'Opptak til ...', 'Kan tas som frittstående fag', 'Emne... |
| ? | ntnu | assessment | wrong_content | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2023_autumn_... | Strip lines matching the pattern 'Ordinær/Utsatt eksamen - (Høst\|Vår\|Sommer) \d{4}' and subsequent logistics rows (Karakter, Dato, Tid, S... |
| ? | ntnu | assessment | formatting_noise | high | widespread | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4621-1_2019_autumn_... | Add a post-processing step that drops any text block matching the regex 'function\s+\w+\s*\(' or 'const\s+\w+\s*=' from the assessment se... |
| ? | ntnu | teaching_methods | merged_sections | high | widespread | ntnu_PPU4621-1_2018_autumn_1; ntnu_PPU4623-1_2020_autumn_... | Add a split rule: within the teaching_methods text block, identify the paragraph starting with 'NTNU er tillagt et sertifiseringsansvar' ... |
| ? | steiner | coursework_requirements | formatting_noise | high | widespread | steiner_M-PEL1_2_2025_spring_1; steiner_M-MAT1_1_2025_spr... | Change the coursework_requirements split to stop before the 'Vurdering' heading: use a negative lookahead or trim the trailing 'Vurdering... |
| ? | steiner | assessment | merged_sections | high | widespread | steiner_M-PEL1_2_2025_spring_1; steiner_M-NOR1_1_2025_spr... | After extracting assessment text, strip content from these sub-headings through end of their paragraph: 'Sensurordning', 'Sensorordning',... |
| ? | uia | assessment | merged_sections | high | widespread | uia_EN-153-1_2020_autumn_1; uia_IDR136-1_2022_autumn_1; u... | Add 'Vilkår for å gå opp til eksamen' (and common variants) to the heading map, mapping to coursework_requirements. If this sub-heading a... |
| ? | uia | prerequisites | boilerplate_only | high | widespread | uia_EN-153-1_2020_autumn_1; uia_IDR136-1_2022_autumn_1; u... | Add 'Opptakskrav hvis tilbudt som enkeltemne', 'Tilbys som enkeltemne', 'Tilgang for privatister', and 'Reduksjon i studiepoeng' to a blo... |
| ? | uib | course_content | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Strip nodes matching the semester-picker before text extraction: remove any element whose text matches the pattern /^Vel emnebeskrivelse ... |
| ? | uib | assessment | formatting_noise | high | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | Add a post_fn that strips content from 'Vurderingsordning' onwards (inclusive) and strips the footer lines 'Dette bør du vite om eksamen'... |
| ? | uis | learning_outcomes | truncated | high | widespread | uis_LPRA40-1_2017_autumn_1; uis_LPRA50-1_2015_autumn_1; u... | Strip 'side N' page-number lines and 'EMNE .* Versjon .*' running-header lines from the extracted text before section splitting, so the K... |
| ? | uis | reading_list | wrong_content | high | widespread | uis_LPRA40-1_2017_autumn_1; uis_LPRA50-1_2016_autumn_1; u... | Same fix as learning_outcomes truncation: strip page-number/running-header noise. Additionally, add a guard so that text that matches the... |
| ? | uis | assessment | merged_sections | high | widespread | uis_MGL3122-1_2021_autumn_2; uis_MGL1051-1_2022_autumn_1;... | Add 'Vilkår for å gå opp til eksamen/vurdering' and 'Obligatoriske krav' as explicit section-boundary headings mapped to coursework_requi... |
| ? | uit | learning_outcomes | merged_sections | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisnings- og eksamensspråk', 'Undervisning', 'Kvalitetssikring', 'Kvalitetssikring av emnet', and 'Eksamen' as closing-boundary... |
| ? | uit | assessment | wrong_content | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Assign the 'Eksamen' heading to assessment as its opening boundary (extracting everything from 'Vurderingsform:…' through 'Kontinuasjonse... |
| ? | uit | cross_section | missing_section | high | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Add 'Undervisning' as a heading pattern mapped to teaching_methods in the heading map. This is a prerequisite change alongside fixing the... |
| ? | usn | coursework_requirements | merged_sections | high | widespread | usn_MG1PR1-1_2025_autumn_1; usn_HIOLJ4000-1_2025_spring_1... | Add 'Eksamensformer' and 'Vurderingsformer' as explicit section-boundary patterns. Strip the legal-rights boilerplate sentence ('Studente... |
| ? | usn | assessment | wrong_content | high | widespread | usn_MG1PR1-1_2025_autumn_1; usn_MG1SA3-1_2025_autumn_1; u... | Fix the Eksamensformer boundary (see above). The assessment section should start at 'Eksamensformer'/'Vurderingsformer' and end before 'H... |
| ? | usn | reading_list | wrong_content | high | widespread | usn_MG1PR1-1_2025_autumn_1; usn_MG1KR7-1_2022_autumn_1; u... | Strip the 'Emnet inngår i følgende studier' block and all lines that match '\w+ \(Kull \d{4} (HØST\|VÅR)\)' from reading_list. Additionall... |
| ? | hivolda | assessment | wrong_content | high | common | hivolda_MGL1-7SP1-1_2025_spring_1; hivolda_MGL5-10EN1A-1_... | Map 'Vilkår for å framstille seg til eksamen' directly to coursework_requirements (not assessment). A sub-rule can optionally split on 'F... |
| ? | inn | reading_list | wrong_content | high | common | inn_2MNK5101-3-1_2022_autumn_1; inn_2NOL51-5-1_2022_autum... | Set the pre-heading default bucket to null (discard) rather than reading_list. Alternatively, detect and discard the metadata header bloc... |
| ? | inn | learning_outcomes | truncated | high | common | inn_2MEN5101-2-1_2022_autumn_1; inn_2MNK5101-3-1_2022_aut... | Same fix as the header-metadata reading_list fix: strip or discard the pre-heading metadata block. After that fix, the Kunnskap/Ferdighet... |
| ✓ | mf | learning_outcomes | truncated | high | common | mf_PPU1015-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1; m... | Anchor sub-heading patterns in R/section_heading_map.R to whole lines ('^(KUNNSKAP\|FERDIGHETER\|GENERELL KOMPETANSE)$', case-insensitive) ... |
| ? | nmbu | prerequisites | merged_sections | high | common | nmbu_PPRA301-1_2025_autumn_1; nmbu_PPRA301-1_2025_spring_1 | Ensure 'Vurderingsordning, hjelpemiddel og eksamen' and all its variants are always mapped as a hard section boundary that terminates pre... |
| ? | nmbu | reading_list | wrong_content | high | common | nmbu_M30-LUN-1_2025_autumn_1; nmbu_M60-LUN-1_2025_autumn_... | Trigger reading_list extraction precisely at the 'Pensum' heading and cut it at the next heading. Do not include text from sections that ... |
| ? | oslomet | all | empty_placeholder | high | common | oslomet_M1GP4200-1_2021_autumn_1; oslomet_M1GP1000-1_2018... | Detect placeholder-only sections (regex ^\s*(Se fagplanen\.?\|Ingen\.?\|-)\s*$, case-insensitive) and drop them / mark section absent. Sepa... |
| ? | oslomet | all | missing_section | high | common | oslomet_M1GP4200-1_2021_autumn_1; oslomet_M5GP4200-1_2022... | When a section resolves to placeholder-only, fall back to parsing the embedded fagplan: add fagplan heading synonyms (Innledning->course_... |
| ✓ | uio | coursework_requirements | missing_section | high | common | uio_HIS1200L-1_2025_spring_1; uio_HIS4015L-1_2025_spring_... | Add heading patterns (Obligatoriske aktiviteter/komponenter/forhold i undervisningen, Faglige krav for å kunne avlegge eksamen, Arbeidskr... |
| ? | uis | course_content | merged_sections | high | common | uis_MGL3122-1_2021_autumn_2; uis_MGL1044-1_2025_autumn_1;... | Ensure 'Litteratur', 'Pensum', and 'Lesestoff' are registered as section-boundary headings for the reading_list section in the UiS headin... |
| ? | uis | cross_section | merged_sections | high | common | uis_MGL1044-1_2025_autumn_1; uis_MGL1110-1_2025_autumn_1;... | Audit the UiS CSS selector configuration and heading-recognition pattern against the newer web template. Add 'Læringsutbytte', 'Forkunnsk... |
| ? | uit | coursework_requirements | merged_sections | high | common | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Add 'Mer info om vurderingsform' as a secondary split point that closes coursework_requirements and opens assessment. Alternatively, stri... |
| ? | usn | reading_list | merged_sections | high | common | usn_MG1SA3-1_2025_autumn_1; usn_MG2SA3-1_2025_autumn_1; u... | Strip learning-outcome fragments from the start of reading_list: if reading_list starts with text matching outcome patterns (e.g., 'kan .... |
| ? | uit | assessment | merged_sections | high | occasional | uit_LRU-2642-1_2014_spring_1; uit_PFF-3102-1_2014_autumn_... | Detect the pattern 'Følgende arbeidskrav må være godkjent før man kan fremstille seg for eksamen:' (the older phrasing) as a secondary tr... |
| ? | uit | course_content | merged_sections | high | occasional | uit_LER-1353-1_2023_spring_1; uit_LRU-2642-1_2014_spring_... | Map 'Hva lærer du' and 'Etter bestått emne skal studentene ha følgende læringsresultat' (older phrasing) as opening boundaries for learni... |
| ✓ | mf | learning_outcomes | wrong_content | high | rare | mf_PRA1005-1_2025_autumn_1 | Same as truncation fix: anchor sub-heading patterns to whole lines and ensure 'Læringsutbytte' always starts learning_outcomes. |
| ✓ | mf | reading_list | wrong_content | high | rare | mf_PPU1015-1_2025_autumn_1 | Anchor the reading_list pattern to a full-line heading ('^Litteraturliste$'); never match bullet lines. |
| ? | oslomet | cross_section | wrong_content | high | rare | oslomet_MGMT5100-1_2023_autumn_1 | Add a sanity filter: reject obvious field-junk per section (learning_outcomes that match only a grading scale like 'Bestaatt/ikke bestaat... |
| ✓ | uio | teaching_methods | other | high | rare | uio_MAT5930L-2_2025_spring_1; uio_PROF3025-1_2025_spring_1 | Run extract_sections on the anonymized course_plan (or apply anonymize_fulltext to each section) and add a check in R/audit/qa_sections.R... |
| ? | hivolda | coursework_requirements | merged_sections | medium | widespread | hivolda_MGL1-7MA1A-1_2017_autumn_2; hivolda_MGL1-7NO3C-1_... | Add heading rules for 'Sensorordning', 'Evaluering og kvalitetssikring', 'Maksimumstal', and 'Emneansvarleg' to route them to a discard/b... |
| ? | hivolda | learning_outcomes | truncated | medium | widespread | hivolda_MGL1-7MA1A2-1_2021_autumn_1; hivolda_MGL1-7MA1B2-... | Remove or raise the character cap on sections_raw text storage. After fixing the merged_sections issue (teaching_methods split), the lear... |
| ? | hivolda | teaching_methods | missing_section | medium | widespread | hivolda_MGL1-7MA1A2-1_2019_autumn_2; hivolda_MGL1-7NO3C-1... | Add 'Praktisk organisering og arbeidsmåtar' to the heading map mapped to teaching_methods. This will simultaneously fix the learning_outc... |
| ? | hvl | assessment | boilerplate_only | medium | widespread | hvl_MGPRA10-1_2025_autumn_1; hvl_MGPRA15-1_2025_autumn_1;... | In the post_fn for assessment, strip trailing content matching the pattern /\n-\s*\nMer om hjelpemidler.*$/i (and Norwegian variants 'Mei... |
| ? | inn | reading_list | boilerplate_only | medium | widespread | inn_2MEN5101-7-1_2022_autumn_1; inn_2MPED-3-1_2022_autumn... | After splitting, apply a post-processing strip that removes the footer block (lines matching '^Fakultet$', '^Fagområde$', '^Studieprogram... |
| ? | inn | prerequisites | missing_section | medium | widespread | inn_2MEN5101-2-1_2022_autumn_1; inn_2MNK5101-3-1_2022_aut... | Add 'Forkunnskapskrav' and 'Anbefalte forkunnskaper' as heading patterns mapped to prerequisites in the inn section-heading map. |
| ? | inn | assessment | formatting_noise | medium | widespread | inn_2MNK5101-3-1_2022_autumn_1; inn_2NOL51-5-1_2022_autum... | Add a pre-processing regex that detects and removes the concatenated table string (matching 'VurderingsordningKarakterskala[A-Za-zÆØÅæøå/... |
| ✓ | mf | course_content | empty_placeholder | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_PRA1001-1_2025_autumn_1; ... | Remap the heading to coursework_requirements and drop heading-only rows in .clean_sections() in R/extract_sections.R (text remaining afte... |
| ✓ | mf | learning_outcomes | formatting_noise | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf_... | Add 'Overlappende emner' as a discard/boundary heading in R/section_heading_map.R; remove the 'Litteraturliste' stub line. |
| ✓ | mf | assessment | formatting_noise | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1014-1_2025_autumn_1; m... | Cut assessment at 'Eksamensdatoer' (or drop lines from 'Eksamensdato:'/'Oppgaven utleveres:' to 'Sensur kunngjøres innen:' plus the follo... |
| ✓ | nih | reading_list | boilerplate_only | medium | widespread | nih_LKI120-1_2021_autumn_2; nih_LKI110-1_2023_autumn_1; n... | In .clean_sections() (R/extract_sections.R), drop reading_list rows that match ^(se emnearkivet\|pensum(lista\|liste) for (høsten\|våren) \d... |
| ? | nla | prerequisites | boilerplate_only | medium | widespread | nla_4MGL1PE101-1_2025_autumn_1; nla_4MGL5PE101-1_2025_aut... | Post-process the prerequisites field: if the value matches patterns like /^Se (program\|studie\|emne)plan\.?$/i, replace with an empty list... |
| ? | ntnu | reading_list | missing_section | medium | widespread | ntnu_PPU4729-1_2016_autumn_1; ntnu_HIST3485-1_2019_spring... | Add 'Kursmateriell' to the heading-to-section map, mapped to reading_list. Also consider 'Faglig innhold: Kursmateriell' if the heading a... |
| ? | uia | course_content | merged_sections | medium | widespread | uia_EN-153-1_2020_autumn_1; uia_ERN129-1_2023_autumn_1; u... | Either (a) add 'Faget i praksis' to the heading map as a sub-heading that triggers a cut within the course_content block (discarding or r... |
| ? | uia | cross_section | missing_section | medium | widespread | uia_EN-153-1_2020_autumn_1; uia_ERN129-1_2023_autumn_1; u... | Map 'Vilkår for å gå opp til eksamen' to coursework_requirements in the heading map. Where this sub-heading is embedded within an 'Eksame... |
| ? | uib | prerequisites | merged_sections | medium | widespread | uib_HIS302L-0_2025_autumn_1; uib_HIS303L-0_2025_autumn_1;... | Map 'Krav til studierett' to no canonical section (drop it). Add a post_fn for prerequisites that removes lines consisting solely of 'Ing... |
| ✗ | uio | assessment | boilerplate_only | medium | widespread | uio_PROMO8-1_2025_autumn_4; uio_PROMO4-1_2025_spring_1; u... | In .clean_sections() (or .anon_uio()), cut everything from the line 'Mer om eksamen ved UiO' to the end, and optionally drop the standalo... |
| ✓ | uio | prerequisites | boilerplate_only | medium | widespread | uio_TYSK4091-1_2025_spring_1; uio_NOR1000-1_2025_spring_1... | Strip the two standard Studentweb/studieprogrammer sentences in .clean_sections() for uio; consider splitting 'Opptak til emnet' from 'Ob... |
| ? | uis | assessment | formatting_noise | medium | widespread | uis_LPRA40-1_2019_autumn_1; uis_LPRA50-1_2018_autumn_1; u... | Add 'Åpen for' and 'Emneevaluering' as stop-headings for the assessment section in PDF-sourced plans. Strip runs of whitespace from PDF t... |
| ? | uit | prerequisites | wrong_content | medium | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3901-1_2024_autumn_... | Post-process the prerequisites field to strip sentences matching the pattern 'Når eksamen i … er bestått, kan studenten framstille seg ti... |
| ? | usn | assessment | formatting_noise | medium | widespread | usn_MG1KR7-1_2024_autumn_1; usn_MG2NO3-1_2018_autumn_2; u... | Add a post-processing strip for patterns matching: 'Godkjent emneplan', 'Godkjent \d{2}\.\d{2}\.\d{4}', 'Godkjent \w+ \d{2}.', 'Endringsb... |
| ? | hiof | reading_list | truncated | medium | common | hiof_LMUMAT40117-1_2020_autumn_2; hiof_LMBENG10117-1_2018... | Increase or remove the character cap for reading_list rows, or ensure all sub-sections under 'Litteratur' (including 'Artikler og kapitte... |
| ? | hivolda | prerequisites | merged_sections | medium | common | hivolda_MGL1-7MA1B-1_2018_autumn_2; hivolda_MGL5-10EN1A-1... | Register 'Om emnet' as a heading that closes prerequisites and opens course_content. This single fix addresses both this finding and the ... |
| ? | hvl | prerequisites | merged_sections | medium | common | hvl_MGUNA501-1_2025_spring_1; hvl_MGBNO201-1_2025_spring_... | Map 'Anbefalte forkunnskaper' / 'Tilrådde forkunnskapar' to the same prerequisites section but prefix the captured text with a marker (e.... |
| ✓ | mf | teaching_methods | missing_section | medium | common | mf_RL1014-1_2025_autumn_1; mf_PRA1005-1_2025_autumn_1; mf... | Add '^Arbeidsform( og organisering)?:?$' mapped to teaching_methods in R/section_heading_map.R, ending the section at 'Om studiet'/'Oblig... |
| ? | nmbu | assessment | truncated | medium | common | nmbu_MATH100-1_2025_autumn_1; nmbu_PPXP100-1_2025_autumn_... | Verify that the full text of the section body (starting from the first content sentence immediately after the heading) is included in the... |
| ? | nmbu | coursework_requirements | merged_sections | medium | common | nmbu_MATH100-1_2025_autumn_1; nmbu_PPFD301-1_2025_autumn_... | Map 'Undervisningstider', 'Overlapp', 'Opptakskrav', 'Merknader', and 'Fortrinnsrett' as terminators for all content sections (or route t... |
| ? | nmbu | teaching_methods | merged_sections | medium | common | nmbu_FYS100-1_2025_autumn_1; nmbu_PPRA201-1_2025_autumn_1... | Map 'Læringsstøtte', 'Pensum', 'Forutsatte forkunnskaper', and 'Anbefalte forkunnskaper' as section boundaries. Route 'Læringsstøtte' to ... |
| ? | nmbu | reading_list | wrong_content | medium | common | nmbu_PPPE301-1_2025_autumn_1; nmbu_PPPE301-1_2025_spring_1 | Same as teaching_methods fix: cut at 'Pensum' heading and extract only the content under that heading into reading_list. |
| ? | nord | assessment | formatting_noise | medium | common | nord_SAM1001-1_2019_autumn_1; nord_KHV1010-1_2023_autumn_... | Trim assessment trailing lines matching: 'Å generere besvarelse ved hjelp av ChatGPT', 'Covid-19 epidemien', 'Midlertidig forskrift', 'jm... |
| ? | ntnu | prerequisites | boilerplate_only | medium | common | ntnu_PPU4621-1_2017_autumn_1; ntnu_PPU4623-1_2018_autumn_... | Add a post-processing filter: if the prerequisites text exactly matches the known NTNU boilerplate strings ('Beståtte eksamener i henhold... |
| ? | oslomet | cross_section | duplicate | medium | common | oslomet_M1GP4200-1_2021_autumn_1; oslomet_M5GP4100-1_2020... | Add the trailing OsloMet sub-headings (Hjelpemidler ved eksamen, Vurderingsuttrykk, Sensorordning, Pensum, Timeplan) as recognised bounda... |
| ? | oslomet | teaching_methods | empty_placeholder | medium | common | oslomet_M5GMT1200-1_2019_autumn_1; oslomet_MGEN4200-1_202... | Same placeholder-detection rule as above; drop 'Se fagplanen.' teaching_methods rather than emitting them. No selector change needed. |
| ? | steiner | learning_outcomes | truncated | medium | common | steiner_M-MAT1_1_2025_spring_1; steiner_M-NOR1_3_2025_spr... | Remove or raise the per-section character cap for learning_outcomes. If a cap is needed for downstream processing, split into multiple ro... |
| ? | uia | learning_outcomes | merged_sections | medium | common | uia_PRA2-302-1_2020_autumn_1; uia_PRA1-302-1_2020_autumn_... | Add 'Faget i praksis' as a cut-point within the learning_outcomes block (strip from that point onward). Also strip any leading repetition... |
| ? | uis | reading_list | empty_placeholder | medium | common | uis_MGL3122-1_2021_autumn_2; uis_LPRA20-1_2015_autumn_2; ... | Fix the Litteratur heading-boundary issue (see course_content merged_sections finding). Post-extraction: suppress reading_list rows whose... |
| ? | uis | prerequisites | merged_sections | medium | common | uis_MGL1110-1_2025_autumn_1; uis_LPRA40-1_2019_autumn_1 | For the HTML template issue: fix the nested section boundary detection so 'Vilkår for å gå opp til eksamen/vurdering' fires before the Fo... |
| ? | uis | teaching_methods | missing_section | medium | common | uis_MGL3122-1_2021_autumn_2; uis_MGL1051-1_2022_autumn_1;... | Add 'Arbeidsformer' and 'Arbeids- og undervisningsformer' as headings mapped to teaching_methods in the UiS heading map. Ensure the selec... |
| ? | usn | course_content | truncated | medium | common | usn_MG1PR1-1_2025_autumn_1; usn_MG1PR3-1_2025_autumn_1; u... | Increase or remove the character cap on course_content. If a cap must exist, raise it to at least 5,000 characters. Verify no deliberate ... |
| ? | usn | teaching_methods | merged_sections | medium | common | usn_MG1KR7-1_2022_autumn_1; usn_LH-PRA1000-1_2018_autumn_... | Filter leading and trailing heading-text artefacts from teaching_methods: strip any line that is an exact match to a known section headin... |
| ? | hiof | reading_list | empty_placeholder | medium | occasional | hiof_LMUENG40317-1_2024_autumn_1; hiof_LMBPED40517-1_2024... | Consider suppressing reading_list rows whose entire content matches patterns like 'Gjeldende litteraturliste.*(Leganto\|Canvas)' or whose ... |
| ✓ | mf | prerequisites | missing_section | medium | occasional | mf_PPU1015-1_2025_autumn_1; mf_PPU1020-1_2025_autumn_1 | Extend the prerequisites pattern to '^Forkunnskap(er\|skrav)' and allow 'Label: text' on one line. |
| ? | nla | assessment | empty_placeholder | medium | occasional | nla_MGL1NO102-1_2025_spring_1; nla_4MGL5FOU201-1_2025_spr... | Same fix as the main assessment finding. Additionally, add a sanity check: if the assessment field value is ≤ 20 chars or matches a langu... |
| ? | nord | prerequisites | missing_section | medium | occasional | nord_MAT5006-1_2022_autumn_1; nord_NOR1005-1_2022_autumn_1 | When building prerequisites, include adjacent non-boilerplate lines preceding the 'Forkunnskapskrav'/'forkunnskaper' field that name cour... |
| ? | nord | teaching_methods | wrong_content | medium | occasional | nord_REL1005-1_2021_autumn_1 | Within the teaching_methods block, split off paragraphs containing 'arbeidskrav', 'må vere/være godkjen(de/t) før ... eksamen' into cours... |
| ? | ntnu | assessment | missing_section | medium | occasional | ntnu_PPU4700-1_2012_autumn_1; ntnu_PPU4611-1_2018_autumn_1 | When no assessment row is extracted but the course metadata contains a non-empty 'Vurderingsordning' field, emit a diagnostic flag so the... |
| ? | oslomet | prerequisites | boilerplate_only | medium | occasional | oslomet_M5GNA3100-1_2020_autumn_1; oslomet_M1GEN2100-1_20... | Treat placeholder/pointer phrases as empty for prerequisites; optionally map the fagplan 'Opptakskrav' block to prerequisites after strip... |
| ? | steiner | teaching_methods | missing_section | medium | occasional | steiner_M-PEL2.2_2025_spring_1 | Add 'Arbeidsmåter' to the heading-to-section map pointing to teaching_methods. If the splitter processes headings in order, ensure 'Arbei... |
| ? | uib | assessment | wrong_content | medium | occasional | uib_SOS340-L-0_2025_spring_1 | Add a post_fn strip for lines matching 'Vi opplever problemer med å hente inn eksamensinformasjon' and 'Lukk'. Alternatively, identify th... |
| ? | uis | coursework_requirements | wrong_content | medium | occasional | uis_LPRA50-1_2015_autumn_1; uis_LPRA50-1_2016_autumn_1 | Ensure the coursework_requirements section captures all content from 'Vilkår for å gå opp til eksamen/vurdering' through the next major h... |
| ? | uis | learning_outcomes | wrong_content | medium | occasional | uis_MGL1322-1_2025_autumn_1; uis_MGL1110-1_2025_autumn_1 | Same fix as the HTML cross_section merged_sections finding: register 'Læringsutbytte' as a hard section boundary in the newer template. V... |
| ? | uit | learning_outcomes | truncated | medium | occasional | uit_LER-3903-1_2024_autumn_1; uit_LER-3913-1_2024_autumn_... | Investigate and raise (or remove) any field-length cap applied to sections_raw text. Once the merge is fixed the correct learning_outcome... |
| ? | usn | learning_outcomes | truncated | medium | occasional | usn_MG2NO3-1_2018_autumn_2; usn_MG2NO1-1_2018_autumn_1; u... | Remove 'Click to view interactive reading list in Leganto' and related Leganto widget strings as preprocessing before section splitting. ... |
| ? | usn | assessment | missing_section | medium | occasional | usn_LRMF510-1_2024_spring_1; usn_MG1SA3-1_2022_spring_1; ... | Ensure 'Vurderingsformer' is registered as a boundary heading for assessment, in addition to 'Eksamensformer'. Strip 'Annet', 'Endringsbe... |
| ? | oslomet | learning_outcomes | field_or_language_junk | medium | rare | oslomet_MGMT5100-1_2023_autumn_1 | Combine length + pattern heuristics to suppress these: drop learning_outcomes/teaching_methods rows under ~30 chars that match grading-sc... |
| ? | steiner | course_content | merged_sections | medium | rare | steiner_M-PEL2.2_2025_spring_1 | Same fix as teaching_methods finding: add 'Arbeidsmåter' to the heading map. Once correctly split, course_content will end at that heading. |
| ✓ | uio | reading_list | missing_section | medium | rare | uio_HIS4015L-1_2025_spring_1 | Add 'Pensum' to reading_list patterns in R/section_heading_map.R and allow the UiO splitter to cut at it when it occurs within Undervisni... |
| ? | hiof | prerequisites | other | low | widespread | hiof_LMUMAT40117-1_2020_autumn_2; hiof_LMBENG10117-1_2018... | Adjust the boilerplate heuristic to treat 'Bestått [specific academic milestone]' patterns under 'Absolutte forkunnskaper' or 'Forkunnska... |
| ? | hivolda | reading_list | missing_section | low | widespread | hivolda_MGL1-7MA1A-1_2017_autumn_2; hivolda_MGL1-7MA1A2-1... | After fixing the wrong_content issue, strip 'Pensumliste for emnet finn du her' as a known boilerplate placeholder (or return it as-is as... |
| ? | hvl | coursework_requirements | boilerplate_only | low | widespread | hvl_MGUMA101-1_2025_autumn_1; hvl_LUPEKI101-1_2025_autumn... | Add a post_fn for coursework_requirements that strips text matching the recurring boilerplate pattern starting with 'De obligatoriske lær... |
| ? | hvl | reading_list | missing_section | low | widespread | hvl_MGUMA201-1_2025_autumn_1; hvl_MGBNO201-1_2025_spring_... | Document in institution_config.R that reading_list is not available from HVL's emneplan pages. No code change needed, but consider adding... |
| ? | nla | coursework_requirements | boilerplate_only | low | widespread | nla_4MGL1PE101-1_2025_autumn_1; nla_MGL1PE102-1_2025_autu... | In the NLA JSON extraction function, exclude the value of the "Vurderingsuttrykk arbeidskrav" key from the coursework_requirements output... |
| ? | nla | course_content | truncated | low | widespread | nla_4MGL1PE101-1_2025_autumn_1; nla_4MGL5PE103-1_2025_aut... | Check whether the NLA JSON API returns the full text of the Innhold field. If so, remove any artificial character cap in the extractor. I... |
| ? | nmbu | cross_section | formatting_noise | low | widespread | nmbu_MATH100-1_2025_autumn_1; nmbu_FYS100-1_2025_autumn_1... | Map 'Om bruk av KI' and 'Sensorordning' to drop/admin bucket. They are institutional boilerplate, not canonical section content. |
| ? | nord | reading_list | missing_section | low | widespread | nord_PEL1001-1_2022_autumn_1; nord_NOR1005-1_2022_autumn_1 | No action needed for reading_list; do not map the 'Semesteravgift og pensumlitteratur' cost note to reading_list (it is admin boilerplate). |
| ? | uib | cross_section | duplicate | low | widespread | uib_RELV302L-0_2025_autumn_1; uib_ENG339L-0_2025_autumn_1... | No change needed to the extractor. Note this duplication in data documentation so downstream consumers are not confused by the repeated c... |
| ? | uit | course_content | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3903-1_2024_autumn_... | Strip the literal string 'Hva lærer du' (and its variants) from the tail of the course_content field during post-processing (post_fn), or... |
| ? | uit | coursework_requirements | formatting_noise | low | widespread | uit_LER-3901-1_2023_autumn_1; uit_LER-3902-1_2024_autumn_... | Add a post_fn (or pre_fn cleaning step) that strips the literal strings 'UiTs samleside om eksamen', 'Mer info om arbeidskrav', and 'Mer ... |
| ? | hiof | assessment | formatting_noise | low | common | hiof_LMBKHV10117-1_2019_autumn_1; hiof_LMBKHV10117-1_2020... | Consider splitting on 'Sensorordning' or 'Vilkår for ny/utsatt eksamen' sub-headings to trim exam-logistics boilerplate from the end of t... |
| ? | hvl | coursework_requirements | wrong_content | low | common | hvl_MGUMA101-1_2025_autumn_1; hvl_MGUMA102-1_2025_autumn_... | This is an authoring issue in the source; no clean programmatic fix is possible without risking dropping real arbeidskrav. The downstream... |
| ? | hvl | all | empty_placeholder | low | common | hvl_MGPRA10-1_2025_autumn_1; hvl_MGPRA5-1_2025_autumn_1; ... | In the post_fn (or a shared normaliser), convert extracted text that is exactly '-' (after trimming whitespace) to NA/NULL, so empty-plac... |
| ✓ | nih | prerequisites | empty_placeholder | low | common | nih_LKI110-1_2025_autumn_1; nih_LKI226-1_2025_autumn_1; n... | Map "Hvem kan ta dette emnet?" to an ignored/admin bucket in R/section_heading_map.R, and have .clean_sections() drop prerequisites rows ... |
| ? | ntnu | teaching_methods | boilerplate_only | low | common | ntnu_PPU4623-1_2021_autumn_1; ntnu_PPU4625-1_2021_autumn_... | Add a post-processing blocklist for the NTNU certification-responsibility paragraph: match on 'NTNU er tillagt et sertifiseringsansvar' a... |
| ? | oslomet | assessment | formatting_noise | low | common | oslomet_MGMT5100-1_2023_autumn_1; oslomet_M5GMT1200-1_201... | Optionally split or trim the 'Ny/utsatt eksamen' and 'Hjelpemidler ved eksamen' sub-blocks out of assessment; low priority since they are... |
| ? | steiner | all | formatting_noise | low | common | steiner_M-MAT1_1_2025_spring_1; steiner_M-NOR1_3_2025_spr... | Add a pre-processing step that strips lines matching /^\d{1,3}$/ (standalone 1-3 digit numbers) that appear at the start or end of a para... |
| ? | uia | prerequisites | empty_placeholder | low | common | uia_PRA044-1_2020_autumn_1; uia_PRA044-1_2023_autumn_1; u... | Add a post-filter: if the extracted text for prerequisites matches '^\s*[Ii]ngen\.?\s*$', suppress the row entirely (do not emit a prereq... |
| ? | uia | learning_outcomes | truncated | low | common | uia_PRA1-202-1_2020_autumn_1; uia_PRA2-202-1_2020_autumn_... | Once the 'Faget i praksis' trailing boilerplate is stripped (per the merged_sections fix above), these truncations will no longer appear.... |
| ? | uib | prerequisites | empty_placeholder | low | common | uib_RELV302L-0_2025_autumn_1; uib_HIS301L-0_2025_autumn_1... | Collapse multiple 'Ingen'/'\-' values in prerequisites to a single token, or to NULL/empty if all constituent fields are null placeholders. |
| ? | uib | assessment | wrong_content | low | common | uib_NOLI103-L-0_2025_autumn_1; uib_NORAN204-L-0_2025_autu... | Add this exact boilerplate sentence to the list of patterns stripped in the assessment post_fn (e.g. regex: /Klokkeslett for oppstart av ... |
| ? | uib | assessment | wrong_content | low | common | uib_HIDID112-0_2025_autumn_1; uib_HIS302L-0_2025_autumn_1... | Identify the 'Hjelpemiddel til eksamen' sub-section within the assessment panel and drop it, or add its null-value strings ('Ingen', '-')... |
| ? | uis | course_content | formatting_noise | low | common | uis_LPRA40-1_2019_autumn_1; uis_LPRA50-1_2018_autumn_1; u... | Add a pre-processing step for UiS PDF sources that removes lines matching 'side \d+' and 'EMNE [A-Z0-9_]+ BOKMÅL Versjon \d{2}\.\w+\.\d{4... |
| ? | usn | prerequisites | empty_placeholder | low | common | usn_MG1PR1-1_2025_autumn_1; usn_HIOLJ4000-1_2025_spring_1... | Map prerequisites values of exactly 'Ingen' or 'Ingen.' (case-insensitive, trimmed) to a null/empty row at post-processing, or emit a str... |
| ? | hvl | course_content | boilerplate_only | low | occasional | hvl_LUPEMP100-1_2025_autumn_1; hvl_LUPEMP200-1_2025_autumn_1 | Add a pre_fn or post_fn for course_content that strips lines matching 'Emnet vert ikkje tilbydd' (and Bokmål variant 'Emnet tilbys ikke'). |
| ? | inn | cross_section | other | low | occasional | inn_2MNK172S-1-1_2022_autumn_1; inn_2MNK172S-1-1_2023_spr... | Add 'Emnet overlapper med' as a heading that triggers a discard zone (content until the next recognised heading is dropped) in the post-p... |
| ? | nla | learning_outcomes | truncated | low | occasional | nla_4MGL5PE103-1_2025_autumn_1; nla_MGL5PE103-1_2025_autu... | Same as course_content truncation fix. Also add validation to detect mid-sentence truncation (no final period/punctuation at end of last ... |
| ? | nla | cross_section | other | low | occasional | nla_MGL5KH201A-1_2025_autumn_1; nla_MGL5SA301-1_2025_spri... | Investigate the NLA JSON key names across course types for reading-list fields. Standardize handling: either always emit null/empty when ... |
| ? | nmbu | prerequisites | boilerplate_only | low | occasional | nmbu_PPXP100-1_2025_autumn_1; nmbu_PPXP100-1_2025_spring_1 | Map 'Opptakskrav' to a drop/admin bucket, not to prerequisites. Only 'Forutsatte forkunnskaper' and 'Anbefalte forkunnskaper' should feed... |
| ? | nord | course_content | split_section | low | occasional | nord_MAT5006-1_2022_autumn_1 | Merge the intro/'Mål og innhold' content paragraph into course_content when it precedes the main innhold block and is content-describing ... |
| ? | ntnu | learning_outcomes | truncated | low | occasional | ntnu_PPU4623-1_2019_autumn_1; ntnu_PPU4625-1_2019_autumn_... | Increase or remove the character limit on the sections_raw text field. The truncation point is consistent across affected courses suggest... |
| ? | oslomet | learning_outcomes | formatting_noise | low | occasional | oslomet_MGNT5100-1_2022_autumn_2; oslomet_M1GEN2100-1_202... | In the pre_fn, normalise repeated ';' between words back to spaces/newlines, drop standalone punctuation-only lines, and fix the '¿' moji... |
| ? | oslomet | course_content | formatting_noise | low | occasional | oslomet_MGEN4200-1_2021_spring_2; oslomet_MGNT5100-1_2022... | Drop leading lines matching 'Fagplanen tilhoerende dette emnet er lagt paa ...' (and similar pointer phrases) from course_content during ... |
| ? | uib | course_content | wrong_content | low | occasional | uib_SPLA106-0_2025_autumn_1; uib_NOLISP300-L-0_2025_autum... | No change required; this is correctly handled. Document in data notes that SPLA106-type short courses may have thin course_content becaus... |
| ? | uib | learning_outcomes | formatting_noise | low | occasional | uib_RELV107-0_2025_autumn_2 | Add a learning_outcomes post_fn that collapses paragraph breaks that split a sentence (i.e., where a paragraph ends mid-word or without t... |
| ? | uis | learning_outcomes | wrong_content | low | occasional | uis_LFYBAC-1_2022_autumn_1; uis_LMAMAS-1_2016_autumn_2 | Add a pattern-based detection for headingless prerequisite sentences (e.g., text matching 'Studenten må ha fullført minst .* stp' or 'God... |
| ? | uit | prerequisites | wrong_content | low | occasional | uit_LER-2152-1_2025_spring_1 | Strip paragraphs matching 'Studiepoengreduksjon' (and the credit-reduction boilerplate that follows) in the prerequisites post_fn, or add... |
| ? | uit | teaching_methods | wrong_content | low | occasional | uit_LRU-3300-1_2016_autumn_1; uit_LRU-2642-1_2014_spring_... | Add patterns for 'For nærmere informasjon om praksis, se egen praksisplan' and 'Emnet evalueres muntlig eller skriftlig minimum en gang h... |
| ? | usn | cross_section | formatting_noise | low | occasional | usn_LRMF510-1_2024_spring_1; usn_MG2KH1-1_2022_autumn_1; ... | Add 'Endringsbeskrivelse', 'Utgifter i emnet', 'Evaluering av emnet', and 'Annet' to a strip-list of USN-specific administrative sections... |
| ? | hiof | teaching_methods | wrong_content | low | rare | hiof_LMBPED40517-1_2024_spring_1 | No change to the extractor needed; consider tuning the leak detector to require a citation/bibliographic pattern before flagging teaching... |
| ? | hvl | course_content | boilerplate_only | low | rare | hvl_LUPEKI101-1_2025_autumn_1 | Add a pattern to the post_fn for course_content that strips paragraphs starting with 'Retningsliner for godkjenning' / 'Retningslinjer fo... |
| ✓ | mf | learning_outcomes | wrong_content | low | rare | mf_RL1013-1_2025_autumn_1 | Do not map 'Delemne X:' headings outside the Læringsutbytte block to learning_outcomes; keep them as course_content and preserve document... |
| ✓ | nih | course_content | formatting_noise | low | rare | nih_LKI226-1_2025_autumn_1 | For nih, add a pre_fn in R/institution_config.R that inserts a newline for <br> and between adjacent block elements before text extraction. |
| ? | nmbu | prerequisites | boilerplate_only | low | rare | nmbu_M60-LUN-1_2025_autumn_1; nmbu_M60-LUN-1_2025_spring_1 | Drop 'Opptakskrav' content from prerequisites output; only extract 'Forutsatte forkunnskaper' and 'Anbefalte forkunnskaper'. |
| ? | nord | prerequisites | empty_placeholder | low | rare | nord_REL1005-1_2021_autumn_1 | Drop section rows whose trimmed content is a placeholder ('...', '-', 'Ingen.', empty) so they are treated as absent. |
| ? | uia | assessment | boilerplate_only | low | rare | uia_PRA044-1_2020_autumn_1 | Add a post-extraction strip matching the pattern 'Sist hentet fra FS \(Felles studentsystem\).*' to the UiA pre/post-fn or as a generic a... |
