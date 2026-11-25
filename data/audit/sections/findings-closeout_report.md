# sections audit — aggregated findings

38 findings across 5 institutions (from 5 agent reports in `data/audit/sections/findings-closeout`; model: gpt-6.2-sol).
Mechanical verification: 38 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 5 | fjellhaug, himolde, khio, nhh, nmh | medium |
| assessment | truncated | 4 | fjellhaug, himolde, nhh, nmh | high |
| assessment | wrong_content | 3 | fjellhaug, khio, nhh | high |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | low | 7 |
| wrong_content | high | 6 |
| formatting_noise | medium | 4 |
| merged_sections | high | 4 |
| truncated | medium | 4 |
| wrong_content | medium | 3 |
| missing_section | high | 2 |
| truncated | high | 2 |
| boilerplate_only | medium | 1 |
| duplicate | low | 1 |
| duplicate | medium | 1 |
| empty_placeholder | low | 1 |
| merged_sections | medium | 1 |
| wrong_content | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 14 |
| cross_section | 6 |
| course_content | 5 |
| coursework_requirements | 5 |
| prerequisites | 3 |
| reading_list | 3 |
| teaching_methods | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fjellhaug | gpt-6.2-sol | 17 | Course descriptions and learning outcomes were generally intact in the packet, but seven of the 17 courses placed graded assessment components in coursework requirements. All four coursework-to-assessment leak flags were confirmed, and the reverse flag on BTM2001 was triggered by prose about acce... |
| himolde | gpt-6.2-sol | 20 | Standard course sections and learning-outcome groups were generally captured correctly across the 20 offerings, and the repeated lecture and exam text flagged as boilerplate is substantive. The paired leak and empty flags identify one coursework boundary error in three SYD204 offerings, while the... |
| khio | gpt-6.2-sol | 20 | Section extraction is generally sound across the 14 suspects and six random controls, with learning outcomes intact and explicit coursework blocks and approval gates separated from assessment. All 28 boilerplate flags are false alarms for extraction quality: the repeated coursework text sets real... |
| nhh | gpt-6.2-sol | 20 | NHH generally extracts the accordion sections cleanly, preserving all displayed learning-outcome groups and keeping admission text out of prerequisites, but both coursework leak flags are real and the controls also show assessment noise and lost labels. All seven empty flags and the short flag ar... |
| nmh | gpt-6.2-sol | 20 | NMH generally captures the main sections and correctly routes explicit exam gates, while missed boundaries, embedded requirements and assessment logistics reduce accuracy in suspects and random controls. Two leakage flags identify merged sections and both .drop flags identify retained admissions ... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | fjellhaug | coursework_requirements | merged_sections | high | occasional | fjellhaug_PT2005-1_2026_autumn_1; fjellhaug_KRLE1001N-1_2... | For Fjellhaug, detect tables whose first column is Vurderingsform and whose other columns describe duration, scope or weighting inside th... |
| ✓ | fjellhaug | coursework_requirements | merged_sections | high | occasional | fjellhaug_BTM1002-1_2026_autumn_1; fjellhaug_BTM2010-1_20... | Add an exact Fjellhaug subheading mapping from Sluttvurdering to assessment. In R/blocks.R, recognize it both as a standalone line and at... |
| ✓ | fjellhaug | teaching_methods | wrong_content | high | occasional | fjellhaug_PT2005-1_2026_autumn_1; fjellhaug_BTM2006-1_202... | Add a Fjellhaug rule that moves full sentences beginning Som et arbeidskrav into coursework_requirements. When the existing requirements ... |
| ✓ | himolde | cross_section | merged_sections | high | occasional | himolde_SYD204-1_2023_autumn_1; himolde_SYD204-1_2022_spr... | For the HiMolde paragraph subheading that exactly matches Arbeidskrav, retain the coursework group through numbered items and their unmap... |
| ✓ | nhh | assessment | wrong_content | high | occasional | nhh_BEA512-1_2026_autumn_1; nhh_ECS530-1_2026_autumn_1 | Recognize the NHH validity sentence in .split_inline_coursework(), move it to coursework_requirements, and suppress a verbatim repeat alr... |
| ✓ | nmh | cross_section | merged_sections | high | occasional | nmh_KUNSTHO71B-1_2026_autumn_1; nmh_ALEX20-2_2025_autumn_1 | Promote standalone rich-text blocks with the exact labels Avsluttende vurdering, Pensumlitteratur or Anbefalt litteratur to section bound... |
| ✓ | nmh | cross_section | wrong_content | high | occasional | nmh_FOLKDANS20-1_2024_autumn_1; nmh_KPKUNST70-1_2021_autu... | Move standalone NMH group-instruction paragraphs beginning with Undervisningen vil foregå to teaching_methods. Route sentences about proj... |
| ✓ | fjellhaug | coursework_requirements | truncated | high | rare | fjellhaug_PT1501-1_2026_autumn_1 | Map the exact Fjellhaug heading Oppmøte og fravær to coursework_requirements so the attendance gate is retained alongside the other requi... |
| ✓ | himolde | coursework_requirements | wrong_content | high | rare | himolde_TRA715-1_2024_spring_1 | When moving a mandatory-attendance sentence, also move the immediately following sentence if it refers to attendance falling below that t... |
| ✓ | khio | course_content | wrong_content | high | rare | khio_IM210-1_2026_autumn_1 | Add a KHiO sentence move from course_content to assessment for the documented project process and result presented 'som grunnlag for vurd... |
| ✓ | khio | teaching_methods | wrong_content | high | rare | khio_MK522-1_2026_autumn_1 | Add a KHiO sentence move from teaching_methods to coursework_requirements for standalone statements of the form 'Deltakelse på ... er obl... |
| ✓ | nmh | reading_list | missing_section | high | rare | nmh_MPTEO73-1_2021_autumn_1 | Route standalone NMH body paragraphs beginning with the exact label Pensum: and a substantive value to reading_list. Remove the paragraph... |
| ✓ | nmh | prerequisites | missing_section | high | rare | nmh_ENSLED22-2_2025_autumn_1 | Send inline Opptakskrav: body labels through the prerequisite filter and retain sentences that specify prior subject study or a prior cou... |
| ✓ | nmh | assessment | truncated | high | rare | nmh_MUSPED55-1_2021_autumn_1 | Preserve the exact NMH and NTNU assessment headings when consolidating the chunks into one assessment row. Remove the redundant scroll di... |
| ✓ | nhh | assessment | truncated | medium | widespread | nhh_ECO422-1_2026_autumn_1; nhh_ENE423-1_2026_autumn_1; n... | Include Permitted Support Material, Grading Scale and Vurderingsuttrykk in the assessment label handling used by sectionize(). Preserve g... |
| ✓ | nmh | assessment | formatting_noise | medium | widespread | nmh_PEDUT52-1_2026_autumn_1; nmh_MPHO22-1_2023_autumn_1; ... | Add assessment-only cleanup for standalone logistics and bounded clauses such as parenthetical frist dates and calendar-semester phrases.... |
| ✓ | fjellhaug | assessment | truncated | medium | common | fjellhaug_BTM1002-1_2026_autumn_1; fjellhaug_BTM2501-1_20... | Add exact assessment mappings for Hjelpemiddeliste ved eksamen and Hjelpemiddelliste ved eksamen and register both as retained labels. Dr... |
| ✓ | nhh | assessment | formatting_noise | medium | common | nhh_ECS509-1_2026_autumn_1; nhh_ECS530-1_2026_autumn_1; n... | Add NHH-scoped cleanup rules for standalone release, deadline, presentation scheduling and resit-availability sentences. Match the submis... |
| ✓ | nhh | cross_section | wrong_content | medium | common | nhh_MAB4-1_2026_autumn_1; nhh_FIN539B-1_2026_autumn_1; nh... | Recognize a standalone Pedagogical Approach line in NHH Topics as an embedded teaching_methods heading, ending at the next labeled subsec... |
| ✓ | nmh | course_content | formatting_noise | medium | common | nmh_IMPRO27-1_2025_autumn_1; nmh_CBE-1_2024_autumn_1; nmh... | Strip standalone NMH sentences matching the observed ensemble-formation condition, application instructions, participation pointers and e... |
| ✓ | himolde | prerequisites | boilerplate_only | medium | occasional | himolde_TRA715-1_2024_spring_1; himolde_TRA725-1_2026_aut... | Extend prerequisite cleanup for the exact admission-pointer and absence formulations in the packet and strip the sentence beginning “Oppt... |
| ✓ | khio | course_content | wrong_content | medium | occasional | khio_IM209-1_2026_autumn_1; khio_IM210-1_2026_autumn_1; k... | Add KHiO sentence moves from course_content to teaching_methods for the standalone closing-exhibition, optional external-workshop, and pr... |
| ✓ | nhh | assessment | formatting_noise | medium | occasional | nhh_ECO422-1_2026_autumn_1; nhh_BED2-1_2026_autumn_1; nhh... | Strip the exact Norwegian and English NHH regulations-and-links paragraph variants following permitted-aids lists in assessment. Keep the... |
| ✓ | fjellhaug | course_content | merged_sections | medium | rare | fjellhaug_GRE1002-1_2026_autumn_1 | Route a Fjellhaug Tekster subheading under Emnets innhold to reading_list when it introduces itemized citations with page counts. Preserv... |
| ✓ | himolde | cross_section | duplicate | medium | rare | himolde_MHS704-2_2025_autumn_1 | Within HiMolde thesis plans, compare normalized paragraphs of the thesis instructions under contents and exam. Remove identical deliverab... |
| ✓ | himolde | assessment | truncated | medium | rare | himolde_MHS704-2_2025_autumn_1 | Under Eksamen in a HiMolde thesis plan, retain Introduksjon and Innholdsfortegnelse as internal assessment labels within the thesis-guide... |
| ✓ | himolde | assessment | truncated | medium | rare | himolde_TRA715-1_2024_spring_1 | For HiMolde, classify a Course evaluation block only when its contents explicitly name student exam components and grading scales. Append... |
| ✓ | khio | assessment | wrong_content | medium | rare | khio_PPU800-1_2026_autumn_1 | For KHiO, move the paragraph opening 'Vurderingen underveis i studiet skal være individbasert' and its following feedback sentence to tea... |
| ✓ | himolde | cross_section | formatting_noise | low | common | himolde_SYD204-1_2023_autumn_1; himolde_MAR200-1_2022_spr... | Drop HiMolde structured fields whose values are blank, a bare percent sign or a dash. Suppress the default attendance field when the rest... |
| ✓ | fjellhaug | assessment | wrong_content | low | occasional | fjellhaug_BTM2503-DK1_2026_autumn_1; fjellhaug_HEB1501-1_... | Extend the Fjellhaug gate move to take the following stock sentence about who sets the requirements and deadlines when it immediately fol... |
| ✓ | fjellhaug | assessment | formatting_noise | low | occasional | fjellhaug_BTM2001-1_2026_autumn_1; fjellhaug_BTM2006-1_20... | Add a Fjellhaug logistics rule for the exact sentence beginning Ved stryk får studentene anledning and ending i perioden for ny/utsatt ek... |
| ✓ | himolde | reading_list | empty_placeholder | low | occasional | himolde_MAR200-1_2021_spring_1; himolde_IDR208-1_2021_spr... | Add anchored reading-list-only placeholder patterns for the learning-platform announcement and “Pensum oppgis av instruktører ved oppstar... |
| ✓ | khio | assessment | formatting_noise | low | occasional | khio_MK522-1_2026_autumn_1; khio_RE103-1_2026_autumn_1 | Use scoped KHiO phrase removal for the committee composition after 'komite' and the trailing 'av fagansvarlig' after a pass/fail grade, p... |
| ✓ | fjellhaug | prerequisites | formatting_noise | low | rare | fjellhaug_PT1501-1_2026_autumn_1 | Filter standalone placeholder-only prerequisite blocks before concatenating the required and recommended prerequisite text. Preserve BTM1... |
| ✓ | fjellhaug | course_content | formatting_noise | low | rare | fjellhaug_GRE1002-1_2026_autumn_1 | Remove the exact standalone Les mer line for Fjellhaug at section cleanup, preserving the following expanded prose. |
| ✓ | himolde | assessment | formatting_noise | low | rare | himolde_SAF140-1_2021_autumn_1; himolde_MHS704-2_2025_aut... | Check the existing HiMolde exam-form suffix rule against SAF140, and strip the submission platform from the standalone “I pdf-format via ... |
| ✓ | himolde | reading_list | formatting_noise | low | rare | himolde_SYD204-1_2022_spring_1 | Check the archived widget markup for this offering and extend the selector to remove the Pensumoversikt widget and its linked list title ... |
| ✓ | nmh | coursework_requirements | duplicate | low | rare | nmh_MUSPED55-1_2021_autumn_1 | Deduplicate exactly matching moved gate sentences when appending them, keeping the wording and order of the first occurrence. |
