# sections audit — aggregated findings

35 findings across 5 institutions (from 5 agent reports in `data/audit/sections/findings-closeout`; model: gpt-6.2-sol).
Mechanical verification: 35 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 5 | fjellhaug, himolde, khio, nhh, nmh | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| fjellhaug | 9 | 7 | teaching_methods / wrong_content; course_content / merged_sections; assessment / wrong_content; assessment / formatting_noise; prerequisites / formatting_noise | cross_section / wrong_content; coursework_requirements / wrong_content | coursework_requirements / merged_sections; coursework_requirements / truncated; assessment / truncated; course_content / formatting_noise |
| nhh | 4 | 7 | assessment / formatting_noise | learning_outcomes / merged_sections; teaching_methods / missing_section; prerequisites / wrong_content; reading_list / wrong_content; course_content / empty_placeholder; learning_outcomes / empty_placeholder | assessment / wrong_content; assessment / truncated; cross_section / wrong_content |
| nmh | 8 | 6 | cross_section / wrong_content; prerequisites / missing_section; reading_list / missing_section; assessment / formatting_noise | course_content / wrong_content; cross_section / formatting_noise | cross_section / merged_sections; assessment / truncated; coursework_requirements / duplicate; course_content / formatting_noise |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| wrong_content | high | 9 |
| formatting_noise | low | 6 |
| empty_placeholder | low | 3 |
| formatting_noise | medium | 3 |
| missing_section | high | 3 |
| wrong_content | medium | 3 |
| merged_sections | high | 2 |
| truncated | medium | 2 |
| boilerplate_only | medium | 1 |
| duplicate | medium | 1 |
| merged_sections | medium | 1 |
| wrong_content | low | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 9 |
| cross_section | 6 |
| course_content | 5 |
| prerequisites | 4 |
| reading_list | 4 |
| teaching_methods | 3 |
| coursework_requirements | 2 |
| learning_outcomes | 2 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fjellhaug | gpt-6.2-sol | 16 | Learning outcomes and graded assessment descriptions, including the prose and table variants, were generally intact across all 16 courses. All 12 prerequisite empty flags came from genuine short values, with one stray placeholder in PT1501, and the BTM2001 leak flag came from a wrapped sentence a... |
| himolde | gpt-6.2-sol | 20 | Standard course sections and learning-outcome groups were generally captured correctly across the 20 offerings, and the repeated lecture and exam text flagged as boilerplate is substantive. The paired leak and empty flags identify one coursework boundary error in three SYD204 offerings, while the... |
| khio | gpt-6.2-sol | 20 | Section extraction is generally sound across the 14 suspects and six random controls, with learning outcomes intact and explicit coursework blocks and approval gates separated from assessment. All 28 boilerplate flags are false alarms for extraction quality: the repeated coursework text sets real... |
| nhh | gpt-6.2-sol | 20 | NHH generally preserves the main accordion sections, the displayed learning-outcome groups and labeled assessment values, though assessment logistics persist in both suspects and controls. All six empty flags are false alarms (Papers., Lectures., ECS508., Presentation., Attendance. and BED3), whi... |
| nmh | gpt-6.2-sol | 20 | NMH reliably captures learning outcomes and the main assessment and coursework blocks, while assessment logistics persist in suspects and random controls and some substantive text follows the wrong heading. The coursework leakage flags identify mandatory practice assigned to teaching and practice... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nmh | course_content | wrong_content | high | common | nmh_EXCOM20-1_2023_autumn_1; nmh_CBE-1_2024_autumn_1; nmh... | Add NMH sentence moves from course_content to teaching_methods for explicit delivery statements such as Det vil bli gitt forelesninger, D... |
| ✓ | fjellhaug | teaching_methods | wrong_content | high | occasional | fjellhaug_PT2005-1_2026_autumn_1; fjellhaug_BTM2006-1_202... | Within Fjellhaug's teaching-methods blocks, route sentences beginning Som et arbeidskrav skal to coursework requirements. For the recurri... |
| ✓ | himolde | cross_section | merged_sections | high | occasional | himolde_SYD204-1_2023_autumn_1; himolde_SYD204-1_2022_spr... | For the HiMolde paragraph subheading that exactly matches Arbeidskrav, retain the coursework group through numbered items and their unmap... |
| ✓ | nmh | cross_section | wrong_content | high | occasional | nmh_DISTUD78-1_2024_autumn_1; nmh_DISTUD78-1_2021_autumn_1 | When an NMH Praksis subheading is nested under Arbeidskrav and opens with an explicit mandatory-participation requirement, retain the blo... |
| ✓ | fjellhaug | cross_section | wrong_content | high | rare | fjellhaug_BTM2506-DK1_2026_autumn_1 | For Fjellhaug, keep the opening teaching sentence with the lecture and seminar details when it contains med obligatorisk oppmøte and the ... |
| ✓ | himolde | coursework_requirements | wrong_content | high | rare | himolde_TRA715-1_2024_spring_1 | When moving a mandatory-attendance sentence, also move the immediately following sentence if it refers to attendance falling below that t... |
| ✓ | khio | course_content | wrong_content | high | rare | khio_IM210-1_2026_autumn_1 | Add a KHiO sentence move from course_content to assessment for the documented project process and result presented 'som grunnlag for vurd... |
| ✓ | khio | teaching_methods | wrong_content | high | rare | khio_MK522-1_2026_autumn_1 | Add a KHiO sentence move from teaching_methods to coursework_requirements for standalone statements of the form 'Deltakelse på ... er obl... |
| ✓ | nhh | learning_outcomes | merged_sections | high | rare | nhh_RET1-1_2026_autumn_1 | In the NHH reader or .text_subheadings(), recognize the standalone Undervisningsopplegg line inside Læringsutbytte as a teaching_methods ... |
| ✓ | nhh | teaching_methods | missing_section | high | rare | nhh_MAB4-1_2026_autumn_1 | Recognize a standalone Pedagogical Approach label within NHH Topics as a teaching_methods opener, then return to course_content at Detail... |
| ✓ | nhh | prerequisites | wrong_content | high | rare | nhh_BUS444N-1_2026_autumn_1 | Add an NHH-scoped rule matching the explicit SKL402 approval-for-assessment sentence and its concurrent-enrolment qualification to course... |
| ✓ | nhh | reading_list | wrong_content | high | rare | nhh_RET1-1_2026_autumn_1 | In RET1, recognize the aids list starting at the explicit dictionary permission and ending with the Category 2 list. Compare those items ... |
| ✓ | nmh | prerequisites | missing_section | high | rare | nmh_ENSLED22-2_2025_autumn_1 | Send stand-alone NMH body lines starting with Opptakskrav: through the substantive-prerequisite filter. Preserve the full mixed sentence ... |
| ✓ | nmh | reading_list | missing_section | high | rare | nmh_MPTEO73-1_2021_autumn_1 | Route a separate NMH body paragraph anchored on Pensum: with a substantive value into reading_list, removing it from the enclosing sectio... |
| ✓ | nmh | assessment | formatting_noise | medium | widespread | nmh_PEDUT52-1_2026_autumn_1; nmh_MPTEO73-1_2021_autumn_1;... | Apply assessment-only cleanup to standalone submission deadlines, exam-system and venue instructions, examiner assignments and repeat-cou... |
| ✓ | nhh | assessment | formatting_noise | medium | common | nhh_ECS509-1_2026_autumn_1; nhh_ECN435-1_2026_autumn_1; n... | Extend NHH cleanup to the presentation-scheduling and standalone deadline and appeal variants observed here. For a relative deadline atta... |
| ✓ | nmh | cross_section | formatting_noise | medium | common | nmh_IMPRO27-1_2025_autumn_1; nmh_CBE-1_2024_autumn_1; nmh... | Extend NMH cleanup with anchored rules for ensemble-formation admission conditions, application deadlines and pointers, responsibility or... |
| ✓ | himolde | prerequisites | boilerplate_only | medium | occasional | himolde_TRA715-1_2024_spring_1; himolde_TRA725-1_2026_aut... | Extend prerequisite cleanup for the exact admission-pointer and absence formulations in the packet and strip the sentence beginning “Oppt... |
| ✓ | khio | course_content | wrong_content | medium | occasional | khio_IM209-1_2026_autumn_1; khio_IM210-1_2026_autumn_1; k... | Add KHiO sentence moves from course_content to teaching_methods for the standalone closing-exhibition, optional external-workshop, and pr... |
| ✓ | fjellhaug | coursework_requirements | wrong_content | medium | rare | fjellhaug_BTM2506-DK1_2026_autumn_1 | For Fjellhaug, route the clauses on the number and submission of mappeoppgaver to assessment when the following Sluttvurdering explicitly... |
| ✓ | fjellhaug | course_content | merged_sections | medium | rare | fjellhaug_GT606-1_2026_autumn_1 | In Fjellhaug's course-content blocks, recognize the lead-in Studenten skal velge ett av de følgende seks tekstområder when followed by na... |
| ✓ | himolde | cross_section | duplicate | medium | rare | himolde_MHS704-2_2025_autumn_1 | Within HiMolde thesis plans, compare normalized paragraphs of the thesis instructions under contents and exam. Remove identical deliverab... |
| ✓ | himolde | assessment | truncated | medium | rare | himolde_MHS704-2_2025_autumn_1 | Under Eksamen in a HiMolde thesis plan, retain Introduksjon and Innholdsfortegnelse as internal assessment labels within the thesis-guide... |
| ✓ | himolde | assessment | truncated | medium | rare | himolde_TRA715-1_2024_spring_1 | For HiMolde, classify a Course evaluation block only when its contents explicitly name student exam components and grading scales. Append... |
| ✓ | khio | assessment | wrong_content | medium | rare | khio_PPU800-1_2026_autumn_1 | For KHiO, move the paragraph opening 'Vurderingen underveis i studiet skal være individbasert' and its following feedback sentence to tea... |
| ✓ | fjellhaug | assessment | formatting_noise | low | common | fjellhaug_BTM2001-1_2026_autumn_1; fjellhaug_BTM2010-1_20... | Add Fjellhaug sentence patterns for the exact resit-period notice and the standalone sentence Frister for innleveringer blir kunngjort ve... |
| ✓ | himolde | cross_section | formatting_noise | low | common | himolde_SYD204-1_2023_autumn_1; himolde_MAR200-1_2022_spr... | Drop HiMolde structured fields whose values are blank, a bare percent sign or a dash. Suppress the default attendance field when the rest... |
| ✓ | himolde | reading_list | empty_placeholder | low | occasional | himolde_MAR200-1_2021_spring_1; himolde_IDR208-1_2021_spr... | Add anchored reading-list-only placeholder patterns for the learning-platform announcement and “Pensum oppgis av instruktører ved oppstar... |
| ✓ | khio | assessment | formatting_noise | low | occasional | khio_MK522-1_2026_autumn_1; khio_RE103-1_2026_autumn_1 | Use scoped KHiO phrase removal for the committee composition after 'komite' and the trailing 'av fagansvarlig' after a pass/fail grade, p... |
| ✓ | fjellhaug | assessment | wrong_content | low | rare | fjellhaug_BTM2503-DK1_2026_autumn_1 | When the Fjellhaug gate pass moves the approval sentences, include the immediately following stock sentence about the instructor setting ... |
| ✓ | fjellhaug | prerequisites | formatting_noise | low | rare | fjellhaug_PT1501-1_2026_autumn_1 | Discard placeholder-only required and recommended prerequisite blocks before joining them, then preserve the real course code. |
| ✓ | himolde | assessment | formatting_noise | low | rare | himolde_SAF140-1_2021_autumn_1; himolde_MHS704-2_2025_aut... | Check the existing HiMolde exam-form suffix rule against SAF140, and strip the submission platform from the standalone “I pdf-format via ... |
| ✓ | himolde | reading_list | formatting_noise | low | rare | himolde_SYD204-1_2022_spring_1 | Check the archived widget markup for this offering and extend the selector to remove the Pensumoversikt widget and its linked list title ... |
| ✓ | nhh | course_content | empty_placeholder | low | rare | nhh_SOL23-1_2026_autumn_1 | Drop a course_content row whose entire normalized text matches this English-page pointer, allowing optional terminal punctuation. |
| ✓ | nhh | learning_outcomes | empty_placeholder | low | rare | nhh_TEST-1_2026_autumn_1 | Suppress this known NHH test stub by matching the TEST Testemne title and the emitted placeholder body together. |
