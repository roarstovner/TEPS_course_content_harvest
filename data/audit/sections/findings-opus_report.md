# sections audit — aggregated findings

20 findings across 3 institutions (from 3 agent reports in `data/audit/sections/findings-opus`; model: opus).
Mechanical verification: 20 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| assessment | formatting_noise | 3 | mf, nih, uio | medium |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting_noise | medium | 3 |
| wrong_content | high | 3 |
| boilerplate_only | medium | 2 |
| missing_section | high | 2 |
| other | high | 2 |
| boilerplate_only | high | 1 |
| empty_placeholder | low | 1 |
| merged_sections | low | 1 |
| merged_sections | medium | 1 |
| missing_section | medium | 1 |
| other | low | 1 |
| truncated | high | 1 |
| wrong_content | medium | 1 |

## By target

| target | n |
| --- | --- |
| assessment | 4 |
| cross_section | 3 |
| prerequisites | 3 |
| reading_list | 3 |
| teaching_methods | 3 |
| learning_outcomes | 2 |
| course_content | 1 |
| coursework_requirements | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| mf | opus | 20 | Section extraction for MF is poor, and the cause is systematic. MF pages use WordPress accordions (details.wp-block-mf-accordion-item > summary). The only h2 elements are 'Om studiet', 'Emneansvarlig' and 'Studentlivet på MF', so the configured html_headings/h2 strategy finds fewer than 3 section... |
| nih | opus | 28 | Section extraction for NIH is highly accurate. The html_headings strategy splits on the FS/Vortex h2 headings (Kort om emnet, Læringsutbytte, Læringsformer og aktiviteter, Arbeidskrav, Vurdering/eksamen, Kjernelitteratur), and in all 28 courses content, outcomes, teaching, coursework and assessme... |
| uio | opus | 13 | course_content and learning_outcomes are correct and complete in all 13 courses (Kunnskap/Ferdigheter/Generell kompetanse groups are kept intact), but the h2-only split leaves the UiO exam navigation/logistics block in almost every assessment row and admission boilerplate from 'Opptak til emnet' ... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | mf | course_content | missing_section | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1016-1_2025_autumn_1; m... | Give mf a DOM strategy that fits its markup, modelled on extract_sections_nord(). (a) course_content is html_text2() of the intro block, ... |
| ✓ | mf | coursework_requirements | wrong_content | high | widespread | mf_RL1016-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1; mf... | Use the summary-based mf strategy from the course_content finding. The summary 'Obligatoriske aktiviteter' already maps to coursework_req... |
| ✓ | mf | learning_outcomes | truncated | high | widespread | mf_PED1010-1_2025_autumn_1; mf_PRA1001-1_2025_autumn_1; m... | Use the summary-based mf strategy, which takes the Læringsutbytte accordion body whole; the <p><b>Kunnskap</b></p> labels are then harmle... |
| ✓ | mf | cross_section | other | high | widespread | mf_SAM1080L-1_2025_spring_1; mf_PRA1005-1_2025_autumn_1; ... | Restrict the mf section container to div.template-study-subject__accordion plus the intro block, excluding #studentradgiver / .template-s... |
| ✓ | uio | prerequisites | boilerplate_only | high | widespread | uio_TYSK4091-1_2025_spring_1; uio_NOR1000-1_2025_spring_1... | Together with `section_heading_selector = "h2, h3"` for uio, add an institution-level heading override, e.g. config `section_heading_over... |
| ✓ | mf | prerequisites | missing_section | high | common | mf_SAM1050-1_2025_autumn_1; mf_PPU1015-1_2025_autumn_1; m... | In the mf DOM strategy, split the intro block at <p><b>…</b></p> labels and at paragraphs beginning 'Forkunnskap(er\|skrav):', and route e... |
| ✓ | uio | teaching_methods | wrong_content | high | common | uio_HIS1200L-1_2025_spring_1; uio_HIS4015L-1_2025_spring_... | Add an opt-in pseudo-heading rule for uio, e.g. config `section_pseudo_heading = "p"`. In extract_sections_html(), treat a <p> whose whol... |
| ✓ | mf | cross_section | wrong_content | high | occasional | mf_PRA1005-1_2025_autumn_1; mf_PPU1015-1_2025_autumn_1; m... | The same fixes apply as for the truncation finding: the mf DOM strategy, plus word-count, word-boundary and case guards on text_split hea... |
| ✓ | uio | cross_section | other | high | occasional | uio_MAT5930L-2_2025_spring_1; uio_PROF3025-1_2025_spring_1 | In R/run_extract_sections.R, after bind_rows(sections_list), add `sections_raw$raw_text <- anonymize_fulltext(sections_raw$institution, s... |
| ✓ | mf | assessment | formatting_noise | medium | widespread | mf_SAM1080L-1_2025_spring_1; mf_RL1010-1_2025_autumn_1; m... | In match_heading_to_section(), deny-list 'eksamensdatoer' and 'eksamensdato' (return NA), next to the existing 'eksamensspråk' denylist. ... |
| ✓ | mf | reading_list | boilerplate_only | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | In the mf strategy, drop #block-emnelegantoinfo from the Litteraturliste body. Emit reading_list as just the link text plus href (the Leg... |
| ✓ | mf | learning_outcomes | merged_sections | medium | widespread | mf_RL1014-1_2025_autumn_1; mf_RL1016-1_2025_autumn_1; mf_... | In the mf DOM strategy every <summary> is a boundary, regardless of blank lines. Add 'overlappende emner' (and 'studiepoengsreduksjon') a... |
| ✓ | uio | assessment | formatting_noise | medium | widespread | uio_PROMO4-1_2025_autumn_4; uio_NOR1000-1_2025_spring_1; ... | In R/institution_config.R, set `section_heading_selector = "h2, h3"` for uio. In match_heading_to_section() (R/section_heading_map.R), ex... |
| ✓ | mf | teaching_methods | missing_section | medium | common | mf_SAM1000L-1_2025_autumn_1; mf_PRA1005-1_2025_autumn_1; ... | Add 'arbeidsform og organisering' and 'arbeidsform' as teaching_methods patterns, placed after the coursework/assessment rows. In the mf ... |
| ✓ | nih | prerequisites | boilerplate_only | medium | common | nih_LKI110-1_2025_autumn_1; nih_LKI226-1_2025_autumn_1; n... | (1) Remove 'hvem kan ta dette emnet' from section_heading_patterns, or add it to the NA denylist in match_heading_to_section(). The h2 th... |
| ✓ | nih | assessment | formatting_noise | medium | common | nih_LKI110-1_2021_autumn_2; nih_LKI235-1_2023_autumn_2; n... | In .clean_section_text(), when section == 'assessment', remove lines matching '(?im)^.*oppmerksom på at oppgaver som leveres i .*(plagiat... |
| ✓ | uio | assessment | wrong_content | medium | common | uio_PROF3025-1_2025_spring_1; uio_PROF1005-1_2025_spring_... | With the pseudo-heading rule from the teaching_methods finding, add 'krav for å kunne avlegge eksamen' -> coursework_requirements in R/se... |
| ✓ | nih | reading_list | empty_placeholder | low | widespread | nih_LKI120-1_2021_autumn_2; nih_LKI236-1_2024_spring_2; n... | In .keep_section_row(), drop reading_list rows that match '^se emnearkivet\\.?$' (case-insensitive). Optionally add a reading_list_pointe... |
| ✓ | uio | teaching_methods | other | low | common | uio_RELDID4009-1_2025_spring_1; uio_PROF3025-1_2025_sprin... | If the pseudo-heading rule is added, map the pseudo-headings 'adgang til undervisning', 'undervisningssted' and 'eventuelle utgifter' to ... |
| ✓ | uio | reading_list | merged_sections | low | rare | uio_HIS4015L-1_2025_spring_1 | This is covered by the pseudo-heading rule proposed for coursework requirements. An exact-match <p>Pensum</p> would start reading_list, a... |
