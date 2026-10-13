# fulltext audit — aggregated findings

56 findings across 18 institutions (from 18 agent reports in `data/audit/fulltext/findings`; model: opus, sonnet).
Mechanical verification: 56 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| whole_text | empty_or_failed | 8 | hiof, nih, nla, nord, ntnu, uib, uio, uis | high |
| page_chrome | junk_included | 7 | hvl, inn, nmbu, steiner, uis, uit, usn | low |
| whole_text | wrong_year | 5 | hvl, inn, nord, ntnu, oslomet | high |
| reading_list | missing_content | 4 | hiof, hivolda, mf, nih | medium |
| whole_text | wrong_page | 3 | inn, oslomet, steiner | high |
| course_content | duplicate_content | 3 | nord, uia, uis | low |
| metadata | wrong_year | 3 | mf, nmbu, uia | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| nih | 3 | 2 | reading_list / missing_content; whole_text / empty_or_failed |  | metadata / missing_content |
| steiner | 3 | 3 | whole_text / wrong_page; page_chrome / junk_included | learning_outcomes / formatting | whole_text / formatting |
| uio | 2 | 3 |  | assessment / junk_included; metadata / formatting; whole_text / empty_or_failed | page_chrome / junk_included; metadata / missing_content |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| junk_included | low | 11 |
| duplicate_content | low | 8 |
| formatting | low | 6 |
| wrong_year | low | 4 |
| empty_or_failed | high | 3 |
| empty_or_failed | medium | 3 |
| missing_content | low | 3 |
| wrong_page | high | 3 |
| wrong_year | high | 3 |
| empty_or_failed | low | 2 |
| formatting | medium | 2 |
| missing_content | medium | 2 |
| wrong_year | medium | 2 |
| missing_content | high | 1 |
| other | low | 1 |
| other | medium | 1 |
| wrong_page | low | 1 |

## By target

| target | n |
| --- | --- |
| whole_text | 20 |
| reading_list | 9 |
| metadata | 8 |
| page_chrome | 8 |
| assessment | 4 |
| course_content | 4 |
| learning_outcomes | 3 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 8 | Extraction works well for HiOF: all 8 fetched pages contain the full plan (facts box, learning outcomes, teaching, assessment, requirements, literature) with clean structure and no navigation or junk; the only uncaptured text is the breadcrumb. Remaining issues are a high 404 rate (about half of ... |
| hivolda | sonnet | 8 | Extraction works well for hivolda: all 8 random controls contain the whole plan (facts box, description, learning outcomes, organisation, requirements, assessment table) with clean structure, correct course and year, and no chrome or junk. The only uncaptured page text is year pickers, the page h... |
| hvl | sonnet | 23 | Extraction works well for ordinary pages (random controls are complete, clean and well structured). Two real problems: spring offerings are mapped to the plan of the academic year that starts that autumn (wrong year), and about 11 of 15 suspects are English-only stub pages with no plan text (the ... |
| inn | sonnet | 28 | When the Edutorium page holds a real plan, extraction is clean and complete (random controls 22, 24-27 are good). Two systematic problems remain: the 'dup_code' suspects are the catalogue's course-search list, not a plan, and many offerings carry the 'not yet published, latest version' fallback, ... |
| mf | sonnet | 8 | Extraction at MF is clean and complete for the plan body: facts box, description, study requirements, assessment, learning outcomes and overlap sections are all captured, and only staff contact boxes are left out. Remaining issues are that the page is the current (2026) plan labelled as 2025 offe... |
| nih | sonnet | 10 | Fulltext extraction works well for NIH: all 10 fetched pages are clean, complete copies of the plan with the facts box, correct year and readable structure, and the uncaptured page text is only breadcrumb, responsible-person name and FS timestamp. The only weakness is that the reading list is emp... |
| nla | sonnet | 10 | The two non-empty plans (KRLE 2) are complete and well structured; the 'long' flag is a false alarm (genuinely long 30 ECTS plans). But 8 of the 8 random controls have a fetched page and empty extracted text, so the large majority of NLA offerings with a page get no text; this is the institution-... |
| nmbu | sonnet | 8 | Extraction with selector .layout is complete and clean for all 8 fetched courses: the packet shows no uncaptured page text, and the facts box, outcomes, teaching, assessment, literature and prerequisites are all present. Remaining issues are cosmetic: facts-box labels run into their values, one l... |
| nord | sonnet | 20 | Extraction is faithful for courses that have a plan on the page: all accordion sections, learning outcomes, exam text and the facts box are captured, with no site chrome. The 11 'short' suspects are pages that carry no plan at all (discontinued or not taught for 5 years), so the text is only titl... |
| ntnu | opus | 29 | The extraction itself is clean: every fetched plan is complete (facts box, all plan tabs, exam tab without dates or rooms), and all 20 'uncaptured' flags are false alarms. The lines they flag are the exam parts' dates, times and rooms, the banners, the year picker and the timetable, all excluded ... |
| oslomet | sonnet | 9 | Extraction from #main-content is largely complete and well structured for most courses. Main problems: some pages hold only a site error message (the pre-pass 'short'/'dup_code' flags are real), stray ';' characters corrupt words and add empty lines in some plans, and some older programme-level p... |
| steiner | sonnet | 8 | Extraction from the Steiner PDF is complete and readable: all seven course-plan parts are present in all eight controls. Defects are limited to PDF page-number lines left in the text, lost bullet glyphs/broken lines, and one offering (M-PEL2.1) whose text is a different course's plan. |
| uia | sonnet | 28 | Extraction faithfully copies what the #right-main selector sees; random controls (2020-2024) are complete, structured plans. The 20 'short' suspects are all 2023 autumn pages that carry only the facts box and no plan text on the page itself (nothing prose is uncaptured), so they are stub plans in... |
| uib | sonnet | 28 | When UiB has a plan, extraction is complete and clean: all eight random controls hold the full plan with every section and no navigation, and the year picker and contact lines are correctly left out. The weakness is offerings whose page has no plan: all 20 suspects are stubs that hold only the fa... |
| uio | sonnet | 8 | UiO fulltext extraction is good: all 8 fetched plans contain the full plan (intro, learning outcomes, admission, teaching, exam, facts box) and the uncaptured page text is only breadcrumbs, fetch timestamps and year pickers. The main defect is a recurring block of generic exam-information link la... |
| uis | sonnet | 28 | PDF-sourced plans (www6.uis.no/Stine) and the controls on current uis.no course pages are complete and mostly clean. All 20 suspects are empty: uis.no/nb/student/course pages with no extracted text and no stored page text, so nothing can be said about the page content. Minor noise: PDF page marke... |
| uit | sonnet | 28 | UiT extraction via #maincontents is generally faithful: whole plans, correct year per p_document_id, no site chrome, and the uncaptured text is only year pickers and staff boxes. The short-flagged suspects are genuinely thin source pages (2008-2012 master-thesis stubs), not extraction losses. Rem... |
| usn | sonnet | 8 | USN extraction is good: all 8 random controls hold the full plan for the right course, year and semester, with no uncaptured page text and no wrong-page or empty cases. Only low-severity noise remains: a table of contents repeating the headings, and Leganto reading-list artefacts with glued type ... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | whole_text | wrong_year | high | widespread | inn_2PRALU2-32-1_2022_spring_1; inn_2ML355-1_2023_autumn_... | Detect the 'Statusmelding / ikke publisert enda' banner in post_fn (or a flag column) and either blank the text for that offering or mark... |
| ✓ | nla | whole_text | empty_or_failed | high | widespread | nla_MGL1NO102-1_2024_autumn_1; nla_MGL5PE101-1_2023_autum... | Take 5 of the empty offerings, list the year keys actually present in the page's EmneplanPage JSON and compare them with .nla_academic_ye... |
| ✓ | ntnu | whole_text | wrong_year | high | widespread | ntnu_MGLU4213-1_2023_spring_1; ntnu_PPU4657-1_2021_spring... | Pass Semesternavn to add_course_url_ntnu() and use Årstall - 1 for 'Vår' and 'Sommer' (as nla_academic_year() does). For most codes the /... |
| ✓ | uis | whole_text | empty_or_failed | high | widespread | uis_MLI355-1_2022_spring_1; uis_MGL4037-1_2023_autumn_1; ... | Inspect the raw html for a few of these courses. If the page is a stub, make html_pdf_discovery try the Stine PDF for each year first. If... |
| ✓ | hvl | whole_text | wrong_year | high | common | hvl_MGBKØ401-1_2025_spring_1; hvl_MGUNA402-1_2024_spring_... | For hvl, build the URL with year - 1 for spring offerings (academic year = start year), or derive the year from the page's 'Emneplan for ... |
| ✓ | hvl | whole_text | missing_content | high | common | hvl_MGBPE201E-1_2024_spring_1; hvl_MGBEN503-1_2021_spring... | Detect the 'finnes kun på engelsk' notice (post_fn or harvest strategy) and re-fetch the English URL for those courses; otherwise mark th... |
| ✓ | inn | whole_text | wrong_page | high | common | inn_2PEDL51-3-1_2023_autumn_3; inn_2LUOPP2-1_2024_spring_... | Treat pages whose text starts with 'Emnesøket gjelder kun fra 2022' (or lacking 'Emnets innhold'/'Læringsutbytte'/'Emnekode') as not-a-pl... |
| ✓ | uib | whole_text | empty_or_failed | high | common | uib_TYS307L-0_2017_spring_2; uib_HIDID111-0_2010_spring_1... | In the UiB config or the extract_fulltext post step, treat a text with no 'Mål og innhold' or 'Læringsutbytte' section (text ends at 'Emn... |
| ✓ | oslomet | whole_text | wrong_page | high | occasional | oslomet_M1KP3000-1_2024_autumn_2 | Treat text matching 'Siden du leter etter finnes ikke' as a failed fetch (empty extracted_text or a fetch-status flag) in the OsloMet con... |
| ✓ | steiner | whole_text | wrong_page | high | rare | steiner_M-PEL2.1_2025_autumn_1 | Check the code-to-section matching in the pdf_split strategy (R/harvest_strategies.R / institution_config.R steiner entry); verify the ex... |
| ✓ | hiof | whole_text | empty_or_failed | medium | widespread | hiof_LMUKHV10421-1_2024_spring_1 | Cross-check the 404 offerings against the course list: for each 404 try the alternate path and confirm the course was offered that year; ... |
| ✓ | mf | metadata | wrong_year | medium | widespread | mf_PED1010-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf... | Document that MF is current-plan-only (plan_years = 'current') and do not treat offering year as the plan year; optionally parse the page... |
| ✓ | mf | reading_list | missing_content | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | Inspect the raw html for the literature link target; if it is a separate URL, harvest it and append to the text, otherwise record that MF... |
| ✓ | hiof | reading_list | missing_content | medium | common | hiof_LMBENG11321-1_2025_spring_1; hiof_LMBENG10125-1_2025... | Treat the Leganto pointer sentence as a placeholder in section extraction (no reading_list content) and note in data_notes.qmd that readi... |
| ✓ | nord | whole_text | empty_or_failed | medium | common | nord_HI225S-1_2016_autumn_2; nord_KH253L-1_2020_spring_3;... | Flag such offerings as no-plan (for example when extracted text has no Beskrivelse/Læringsutbytte/Eksamensbeskrivelse heading, or the pag... |
| ✓ | nord | whole_text | wrong_year | medium | common | nord_SP154L-1_2018_autumn_3; nord_MA131L-1_2018_autumn_1;... | Capture the 'Emnebeskrivelse for YYYY/YY' or 'sist oppdatert' label from the page, store it as plan year, and compare it with the offerin... |
| ✓ | oslomet | whole_text | formatting | medium | common | oslomet_M5GPE5100-1_2022_autumn_1; oslomet_M1GSF2110-1_20... | Find where ';' is introduced (compare html with extracted_text). Fix it in the block text collapsing in R/blocks.R, or add a post_fn for ... |
| ✓ | uia | whole_text | other | medium | common | uia_KH-503-1_2023_autumn_1; uia_IDR509-1_2023_autumn_1; u... | Mark offerings whose extracted text has no plan heading (Læringsutbytte/Innhold/Eksamen) as 'no plan' (e.g. in the gaps target) so they a... |
| ✓ | ntnu | whole_text | empty_or_failed | medium | occasional | ntnu_PPU4759-1_2019_spring_3 | The same change to add_course_url_ntnu() (Årstall - 1 for Vår/Sommer) fixes these. Afterwards, check that the 108 offerings get the /{Y-1... |
| ✓ | oslomet | whole_text | formatting | medium | occasional | oslomet_M5GMT3100-1_2020_autumn_1 | Add a block-splitting rule for OsloMet fagplan content (break at br, headings and nested divs), or insert newlines before known heading w... |
| ✓ | hivolda | reading_list | missing_content | low | widespread | hivolda_MGL5-10SA1B-1_2019_spring_1; hivolda_MGL5-10EN3C-... | If reading lists are wanted, resolve the 'Pensumliste' link in the url_discovery strategy and append the linked page text; otherwise docu... |
| ✓ | inn | page_chrome | junk_included | low | widespread | inn_2MNK5101-3-1_2025_autumn_1; inn_2MMU5101-4-1_2024_spr... | Remove the line 'Ingen pensumliste tilgjengelig for dette emnet' and the Startsemestre year list in post_fn. |
| ✓ | mf | reading_list | junk_included | low | widespread | mf_RL1016-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1 | Add an exclude/post_fn rule in the mf config removing the 'Tilgang til litteratur:' block through 'MERK: Sjekk at litteraturlisten…' (kee... |
| ✓ | nih | whole_text | empty_or_failed | low | widespread | nih_LKI152-1_2024_spring_2 | Spot check a few 404 URLs for courses known to be offered (e.g. LKI152 spring 2024) in a browser; if they exist elsewhere, adjust the URL... |
| ✓ | nmbu | metadata | formatting | low | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_M30-LUN-1_2025_autumn_1 | In the block text conversion, join child text nodes of the facts box with a space (or newline), or add a post_fn that inserts a space aft... |
| ✓ | oslomet | metadata | junk_included | low | widespread | oslomet_M5GMT3100-1_2025_autumn_1; oslomet_M5GPE5100-1_20... | Add an exclude or a post_fn that drops the lines Emnehistorikk, Pensum, Timeplan, Programplan and the semester label lines that follow th... |
| ✓ | uio | assessment | junk_included | low | widespread | uio_PROMO1003-1_2025_autumn_2; uio_BIOS5000-1_2025_autumn... | Add an exclude (or post_fn line filter) for the shared exam link list and the 'Andre veiledninger og ressurser...' sentence, and for the ... |
| ✓ | usn | page_chrome | junk_included | low | widespread | usn_MG1MA3-1_2019_spring_2; usn_MG2NA2-1_2024_spring_1; u... | In the usn post_fn, drop the block from the 'Innholdsfortegnelse' line up to the second occurrence of 'Faglig innhold i emnet', or exclud... |
| ✓ | usn | reading_list | formatting | low | widespread | usn_MG2NA2-1_2024_spring_1; usn_MG2NO1-1_2019_autumn_1; u... | Insert a newline after known Leganto type labels (Book, Book Chapter, Article, Electronic Article, Website, Government Document, Journal,... |
| ✓ | hvl | page_chrome | junk_included | low | common | hvl_MGPRA10-1_2025_spring_1; hvl_MGBPR201-1_2023_spring_1... | In the hvl post_fn, drop lines that are just '-' and the trailing 'Mer om hjelpemidler/Meir om hjelpemiddel' label; the section step shou... |
| ✓ | mf | assessment | duplicate_content | low | common | mf_PED5610-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1 | Optional: de-duplicate identical consecutive paragraphs in post_fn for mf; otherwise accept as source duplication. |
| ✓ | nih | reading_list | missing_content | low | common | nih_LKI221-1_2024_autumn_1; nih_LKI106-1_2025_spring_1; n... | Accept as a source limitation and let section extraction treat stub-only reading_list as empty; optionally check whether the pensum link ... |
| ✓ | nmbu | page_chrome | junk_included | low | common | nmbu_FYS100-1_2025_autumn_1; nmbu_PPPE301-1_2025_autumn_1 | Add a post_fn or exclude pattern in R/institution_config.R for the nmbu entry that drops the line 'Her finner du KI-kategoriene beskrevet.'. |
| ✓ | nord | course_content | duplicate_content | low | common | nord_PO118LS-1_2024_spring_1; nord_SAM1007-1_2023_autumn_... | Deduplicate: drop the ingress paragraph when it equals the start of the Beskrivelse section, and dedupe identical lines in the Overlappen... |
| ✓ | steiner | page_chrome | junk_included | low | common | steiner_M-MAT1_2_2025_autumn_1; steiner_M-MAT1_1_2025_aut... | Add a post_fn for steiner that drops lines matching ^\s*\d{1,3}\s*$ (and rejoins the split sentence/list item where a page break interrup... |
| ✓ | steiner | learning_outcomes | formatting | low | common | steiner_M-NAT1_2_2025_autumn_1; steiner_M-SAM1_2_2025_aut... | In post-processing join continuation lines (line not starting with bullet/heading and previous line not ending in sentence punctuation), ... |
| ✓ | uia | metadata | wrong_year | low | common | uia_KH-503-1_2023_autumn_1; uia_NAT200-1_2023_autumn_1; u... | Do not derive semester from the page's Undervisningssemester; use the offering's own semester field. |
| ✓ | uib | assessment | missing_content | low | common | uib_FRAN313L-0_2025_autumn_1; uib_NODI302-0_2024_spring_1 | No change needed if exam dates count as logistics. If the assessment form (for example 'Skriftleg skoleeksamen', 'Mappevurdering', 'Varig... |
| ✓ | uio | whole_text | empty_or_failed | low | common | uio_BIOS5000-1_2025_spring_1 | Check in harvest_strategies.R whether non-fetched UiO offerings are URL duplicates; if so, document it and have downstream steps reuse th... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_ENG112-1_2025_autumn_2; uis_PPU118-1_2025_autumn_3 | Add exclude patterns in R/institution_config.R for the footer line and the 'Opptak til enkeltemner'/'Utveksling til' blocks, or cut the '... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_MLH150-1_2013_spring_1; uis_MTLMAS-1_2012_spring_1; u... | Remove lines matching ^\s*side \d+\s*$ and the repeated '<CODE> - <title>' header in the PDF post_fn. |
| ✓ | uit | metadata | duplicate_content | low | common | uit_FRA-3903-1_2009_spring_1; uit_FRA-3903-1_2010_spring_... | In the uit post_fn, drop the header line matching '^<code> .* - \d+ stp$' and the split variant (consecutive lines code / blank / title /... |
| ✓ | usn | reading_list | duplicate_content | low | common | usn_MG2NO1-1_2019_autumn_1; usn_MG2NA2-1_2024_spring_1 | Drop lines before the Leganto 'Click to view' label in the Litteratur section, or collapse them to unique values. |
| ✓ | uis | course_content | duplicate_content | low | occasional | uis_MGL3126-1_2022_autumn_1; uis_MGL2060-1_2021_autumn_1;... | Optionally drop a paragraph in the PDF text that is identical to one already seen in the same text. Low priority. |
| ✓ | uit | learning_outcomes | formatting | low | occasional | uit_PFF-1001-1_2009_autumn_1 | Check the raw html of this page for <br> or <li> in these blocks. If present, make blocks.R treat them as line breaks. If the source is a... |
| ✓ | hiof | reading_list | wrong_year | low | rare | hiof_LMUSPA10124-1_2025_autumn_1 | No pipeline change; do not use the year in the Leganto sentence for year validation. |
| ✓ | nmbu | metadata | wrong_year | low | rare | nmbu_FYS100-1_2025_autumn_1 | Document in methods that NMBU text is the current plan; avoid joining the semester text to the offering semester, or dedupe offerings per... |
| ✓ | nord | reading_list | other | low | rare | nord_SPD2001-1_2019_autumn_2 | Check whether the raw html holds the character; if so, map '¿' between spaces to a dash in post_fn; otherwise ignore. |
| ✓ | oslomet | whole_text | wrong_year | low | rare | oslomet_M1GSF2110-1_2022_spring_2 | Check whether OsloMet plan pages are per academic year (start autumn) and map spring offerings to the academic year that starts the previ... |
| ✓ | uia | metadata | wrong_page | low | rare | uia_REL420-1_2021_autumn_2 | Flag offerings whose page title differs strongly from the metadata title for manual review; no extraction change needed. |
| ✓ | uia | course_content | formatting | low | rare | uia_PRA044-1_2021_autumn_1; uia_REL420-1_2021_autumn_2 | In the block-to-text conversion insert a newline for <br> and list items inside #right-main. |
| ✓ | uia | course_content | duplicate_content | low | rare | uia_NO-155-1_2023_autumn_1 | Check the HTML of NO-155; if the row is not duplicated in source, deduplicate adjacent identical table lines in blocks.R. |
| ✓ | uio | metadata | formatting | low | rare | uio_GEO5930L-2_2025_autumn_1 | Render the facts box as 'Label: value' pairs, with an explicit empty value, in the UiO post_fn or in a block formatter. |
| ✓ | uit | page_chrome | junk_included | low | rare | uit_LRU-1270-1_2011_spring_1; uit_MAT-3906-1_2012_spring_1 | Add a uit post_fn in R/institution_config.R that strips literal <...> tags (e.g. gsub('<[^>]+>', ' ', x)) and decodes HTML entities (&ari... |
| ✓ | uit | assessment | duplicate_content | low | rare | uit_LER-1060-1_2025_autumn_1 | No change needed. Optionally note in data notes that UiT pensum lists on new pages are external (Leganto) and not in the text. |
| ✓ | usn | learning_outcomes | duplicate_content | low | rare | usn_MG1KH2-1_2018_autumn_1 | Collapse consecutive identical heading lines. |
