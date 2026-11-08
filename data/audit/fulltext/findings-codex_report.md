# fulltext audit — aggregated findings

16 findings across 12 institutions (from 19 agent reports in `data/audit/fulltext/findings-codex`; model: gpt-6.1-sol).
Mechanical verification: 16 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| page_chrome | junk_included | 5 | hiof, mf, steiner, uis, usn | medium |
| whole_text | empty_or_failed | 4 | hiof, nhh, nih, uib | high |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting | low | 3 |
| junk_included | low | 3 |
| junk_included | medium | 3 |
| empty_or_failed | medium | 2 |
| wrong_year | high | 2 |
| empty_or_failed | high | 1 |
| empty_or_failed | low | 1 |
| wrong_page | high | 1 |

## By target

| target | n |
| --- | --- |
| whole_text | 7 |
| page_chrome | 5 |
| assessment | 1 |
| coursework_requirements | 1 |
| metadata | 1 |
| reading_list | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| fhs | gpt-6.1-sol | 28 | Fulltext extraction is faithful across all 28 reviewed courses (20 suspects and 8 random controls): the facts box, Hva lærer du introductions and other prose before the accordions are captured, the accordion sections are readable, and the uncaptured lines contain only update timestamps and versio... |
| himolde | gpt-6.1-sol | 28 | The 28 packet entries (one suspect and 27 random controls) show no confirmed fulltext extraction defect: retained plans preserve the visible course sections, facts boxes and readable structure, while omitted page text consists of navigation, timestamps, staff names and utility links. The short-te... |
| hiof | gpt-6.1-sol | 34 | Reviewed all 28 fetched-page entries (20 suspects and 8 controls), including all text supplied before explicit packet truncation markers, and all 6 additional failed-fetch entries. The 16 year flags concern later plans correctly excluded from the build; the earlier English control is also correct... |
| hvl | gpt-6.1-sol | 34 | Reviewed all 28 fetched course entries (20 suspects and eight controls), plus all six failed-fetch entries: full plans are generally readable and substantively captured, but 15 retained English-page pointers substitute notices for plans. Of the three year flags, SABV270 is a false alarm (spring 2... |
| inn | gpt-6.1-sol | 28 | No reportable fulltext defects were found in the 28-course packet. The short/long and duplicate-code flags identify six search-result fallback pages, while the year flags identify fourteen later-version pages; all twenty suspects are appropriately excluded from the build, as are five later-versio... |
| khio | gpt-6.1-sol | 31 | Reviewed all 31 packet entries: 28 complete extracted plans and three failed-fetch records. The 28 random controls show readable, course-specific plans with facts boxes, no uncaptured page text and no substantive extraction defects; the absence of pre-pass flags is consistent with manual review, ... |
| mf | gpt-6.1-sol | 11 | Read all eight random-control plans in full and reviewed all three failed-fetch entries; there were no suspects or pre-pass flags. The plans retain substantive sections, facts boxes and readable structure, but four of eight include exam-schedule boilerplate missed by the pre-pass. Current-plan da... |
| nhh | gpt-6.1-sol | 28 | All 28 courses were reviewed, including 11 suspects. Extraction generally preserves the available plan and readable structure: uncaptured flags reflect excluded staff/contact boxes, next-term links or alternative link serialization; the three short suspects are sparse or expired/pointer source pa... |
| nih | gpt-6.1-sol | 20 | Reviewed all 14 fetched courses and all six failed-fetch entries. The eight retained random controls contain complete, readable course plans with matching course codes and terms; all six short flags identify genuine page shells that the build correctly excludes, and uncaptured text is breadcrumbs... |
| nla | gpt-6.1-sol | 31 | Reviewed all 28 sampled fetched offerings and the three appended failed-fetch examples; the available plans are generally clean, correctly keyed to the academic year, and show no demonstrated omitted page content or chrome contamination. The two year flags are false alarms caused by incidental hi... |
| nmbu | gpt-6.1-sol | 11 | Reviewed all eight random-control plan excerpts, including the packet-capped M30-LUN excerpt, and all three failed-fetch entries. No pre-pass flags were raised; the displayed plans match their course codes, retain readable sections and facts, and have no reported uncaptured text or visible site c... |
| oslomet | gpt-6.1-sol | 28 | All 28 courses were reviewed in full: the 10 retained plans preserve the visible plan content and readable section structure for the correct course and academic year, while 17 page shells (including the practice page with x-only sections) and one error page are appropriately excluded from the bui... |
| phs | gpt-6.1-sol | 28 | All 28 course entries were read in full as presented in the packet; no supported fulltext defects were found in the 16 retained plans, whose visible content and metadata are cleanly structured, with only breadcrumbs and update information omitted. The 12 excluded pages match the stated earlier-ve... |
| steiner | gpt-6.1-sol | 11 | Reviewed all eight random-control plans in full and all three failed entries. The plans have readable sections and course metadata, but four of eight retain bare PDF page numbers despite zero pre-pass flags; no further defect is established from the packet, which lacks source-page comparisons. Th... |
| uib | gpt-6.1-sol | 40 | Reviewed all 28 fetched-page samples (20 suspects and 8 random controls) and all 12 additional failed-fetch entries. Sixteen suspects have empty extraction despite substantial plan content, but all are absent from the build, suggesting a coverage/join problem rather than demonstrated selector los... |
| uio | gpt-6.1-sol | 16 | Reviewed all 16 packet entries (13 fetched plans, including five suspects and eight random controls, plus three failed-fetch entries); the available plan text is readable and shows no confirmed extraction defect. All five year flags concern last-offered spring 2025 dates on current-plan pages and... |
| uis | gpt-6.1-sol | 31 | Reviewed all 28 sampled course entries (20 suspects and 8 controls), plus all 3 appended fetch failures; packet truncation markers were treated as display limits. Course content is generally readable, but eight older PDF extracts retain printing artefacts, including one random control, and one re... |
| usn | gpt-6.1-sol | 28 | All 28 packet courses were read in full as presented, including the packet-capped texts; course codes and start terms match, all have a plan in the build, and all show zero uncaptured page text. The junk flags are false alarms where their triggers are visible: ordinary “document.” and “function (... |
| vid | gpt-6.1-sol | 31 | Reviewed all 28 fetched-page examples and three failed-fetch examples: the eight retained control plans are complete and readable apart from one minor numeric artefact. The 19 empty suspects, one short shell and the uncaptured flag show no omitted plan body or unjustified exclusion; the three sof... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | nhh | whole_text | empty_or_failed | high | widespread | nhh_BAN427-1_2025_autumn_1; nhh_FIE420-1_2025_autumn_1; n... | For current-plan pilot audits, trace the same fetched page through its actual build academic-year assignment before reporting plan availa... |
| ✓ | hvl | whole_text | wrong_page | high | common | hvl_PHDH913-1_2024_spring_1; hvl_DAT355-1_2025_spring_1; ... | Extend the HVL English-pointer detector with explicit Norwegian, Nynorsk and English variants observed in this packet, matching normalize... |
| ✓ | hvl | whole_text | wrong_year | high | occasional | hvl_MAS502-1_2023_spring_1; hvl_MAS532-1_2023_spring_2 | Add an HVL page_term parser restricted to explicit first-time teaching declarations in the instruction-semester fact, and use the existin... |
| ✓ | uis | whole_text | wrong_year | high | rare | uis_MGL2071-1_2019_autumn_1 | Parse anchored PDF plan-year declarations such as this studieinformasjon notice and Dette er studietilbudet for studieår, compare them wi... |
| ✓ | mf | page_chrome | junk_included | medium | common | mf_PED1010-1_2025_autumn_1; mf_SAM2080L-1_2025_autumn_1; ... | Add an MF-specific structural exclusion in R/institution_config.R for the schedule container headed Eksamensdatoer, bounded by that conta... |
| ✓ | steiner | page_chrome | junk_included | medium | common | steiner_M-MAT1_1_2025_autumn_1; steiner_M-NAT1_2_2025_aut... | Remove numeric footer lines from each PDF page before concatenation in .steiner_extract_emner_from_pdf(), restricting removal to the fina... |
| ✓ | uib | whole_text | empty_or_failed | medium | common | uib_KJEM214-0_2015_autumn_1; uib_TEAT252-0_2017_spring_1;... | Rebuild the derived extraction/build artifacts from the same raw-store snapshot, then regenerate this packet. In harvested_rows()/prepare... |
| ✓ | uis | page_chrome | junk_included | medium | common | uis_LNOR165-1_2019_autumn_1; uis_LPRA40-1_2018_autumn_1; ... | Add UiS-specific PDF cleanup in the raw-to-fulltext path, including legacy stored-text rows: remove standalone side N lines and the TCPDF... |
| ✓ | hiof | whole_text | empty_or_failed | medium | occasional | hiof_LAÅKHX 000_2004_spring_0; hiof_SFB00204 000_2004_aut... | Prefer URLs discovered from HiOF listings. When a constructed fallback is needed, percent-encode its UTF-8 filename component, including ... |
| ✓ | hiof | page_chrome | junk_included | low | widespread | hiof_IRM23116-1_2019_autumn_1; hiof_SFB11615-1_2023_sprin... | Add a HiOF-specific post_fn that removes only complete lines exactly equal to Innholdsfortegnelse or Table of contents, retaining every p... |
| ✓ | usn | page_chrome | junk_included | low | widespread | usn_SL-PTM2000-1_2025_autumn_1; usn_PRO3000K-1_2024_autum... | Exclude the course table-of-contents component and Leganto action controls during shadow-DOM traversal. For existing stored text, remove ... |
| ✓ | usn | reading_list | formatting | low | common | usn_SL-PTM2000-1_2025_autumn_1; usn_CS4210-1_2025_autumn_... | Insert a space or newline at the reading-list type/title element boundary in read_usn_live_html(). Repair already stored text in .cleanup... |
| ✓ | nih | whole_text | empty_or_failed | low | occasional | nih_FAH310 000_2005_spring_0; nih_FLL231 000_2005_spring_... | Percent-encode the course slug as a single URL path component after lowercasing and transliteration in add_course_url_nih(), including sp... |
| ✓ | nmbu | metadata | formatting | low | occasional | nmbu_PPFD201-1_2025_autumn_2; nmbu_M30-LUN-1_2025_autumn_1 | Add a narrowly scoped NMBU normalization in R/institution_config.R post_fn (implemented in R/extract_fulltext.R) for the observed metadat... |
| ✓ | nla | coursework_requirements | formatting | low | rare | nla_IKF302-1_2018_spring_1 | Before converting NLA accordion HTML to text in .json_blocks(), normalize list elements to ul or ol according to listType and listItem el... |
| ✓ | vid | assessment | junk_included | low | rare | vid_BAVER2030-1_2024_autumn_1 | Confirm the number against the stored BAVER2030 article block, then remove the confirmed artefact with a narrowly scoped VID postprocesso... |
