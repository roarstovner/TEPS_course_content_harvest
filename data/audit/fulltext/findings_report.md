# fulltext audit — aggregated findings

66 findings across 18 institutions (from 18 agent reports in `data/audit/fulltext/findings`; model: sonnet).
Mechanical verification: 66 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| whole_text | empty_or_failed | 9 | hiof, nih, nla, nord, ntnu, oslomet, uib, uio, uis | high |
| page_chrome | junk_included | 8 | inn, nmbu, nord, oslomet, steiner, uis, uit, usn | low |
| reading_list | missing_content | 7 | hiof, hivolda, hvl, mf, nih, nla, oslomet | medium |
| course_content | duplicate_content | 4 | hvl, nord, uia, uis | low |
| metadata | formatting | 4 | inn, nmbu, ntnu, uio | low |
| whole_text | wrong_page | 3 | inn, oslomet, steiner | high |
| course_content | formatting | 3 | nla, oslomet, uia | low |
| learning_outcomes | formatting | 3 | ntnu, steiner, uit | low |
| metadata | wrong_year | 3 | mf, nmbu, uia | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hivolda | 1 | 1 | reading_list / missing_content |  |  |
| hvl | 3 | 4 | whole_text / missing_content | reading_list / missing_content; course_content / duplicate_content; whole_text / formatting | whole_text / wrong_year; page_chrome / junk_included |
| inn | 3 | 4 | whole_text / wrong_page; whole_text / wrong_year; page_chrome / junk_included | metadata / formatting |  |
| nla | 1 | 4 | whole_text / empty_or_failed | whole_text / formatting; course_content / formatting; reading_list / missing_content |  |
| nord | 4 | 4 | whole_text / empty_or_failed; whole_text / wrong_year; course_content / duplicate_content | page_chrome / junk_included | reading_list / other |
| ntnu | 2 | 6 | whole_text / empty_or_failed | assessment / missing_content; course_content / other; learning_outcomes / formatting; metadata / formatting; whole_text / junk_included | whole_text / wrong_year |
| oslomet | 4 | 5 | whole_text / wrong_page | whole_text / empty_or_failed; page_chrome / junk_included; reading_list / missing_content; course_content / formatting | whole_text / formatting; metadata / junk_included; whole_text / wrong_year |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| formatting | low | 13 |
| junk_included | low | 12 |
| duplicate_content | low | 9 |
| empty_or_failed | high | 5 |
| missing_content | low | 5 |
| empty_or_failed | low | 4 |
| missing_content | medium | 4 |
| wrong_page | high | 3 |
| wrong_year | low | 3 |
| other | medium | 2 |
| wrong_year | medium | 2 |
| empty_or_failed | medium | 1 |
| missing_content | high | 1 |
| wrong_page | low | 1 |
| wrong_year | high | 1 |

## By target

| target | n |
| --- | --- |
| whole_text | 20 |
| reading_list | 11 |
| metadata | 9 |
| page_chrome | 9 |
| course_content | 8 |
| assessment | 5 |
| learning_outcomes | 4 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 8 | Extraction works well for HiOF: all 8 fetched pages contain the full plan (facts box, learning outcomes, teaching, assessment, requirements, literature) with clean structure and no navigation or junk; the only uncaptured text is the breadcrumb. Remaining issues are a high 404 rate (about half of ... |
| hivolda | sonnet | 28 | Extraction works well for hivolda: all 28 random controls contain the whole plan (facts box, description, learning outcomes, organisation, requirements, assessment table) with clean line structure, correct course and year, and no chrome or junk. Uncaptured page text is only year pickers, the repe... |
| hvl | sonnet | 28 | Extraction works well for ordinary pages: the 22 random controls are complete, include facts box, outcomes, teaching, assessment and requirements, and are free of chrome; years are correct (spring offerings use the previous academic-year URL). The one real problem is English-only courses, whose N... |
| inn | sonnet | 28 | Where the page holds a real plan, extraction is complete and clean (all sections, facts box, no navigation). Two institution-wide problems remain: many offerings get the latest published plan under a 'not published yet' notice (wrong year), and some course codes return the course-search listing i... |
| mf | sonnet | 8 | Extraction at MF is clean and complete for the plan body: facts box, description, study requirements, assessment, learning outcomes and overlap sections are all captured, and only staff contact boxes are left out. Remaining issues are that the page is the current (2026) plan labelled as 2025 offe... |
| nih | sonnet | 10 | Fulltext extraction works well for NIH: all 10 fetched pages are clean, complete copies of the plan with the facts box, correct year and readable structure, and the uncaptured page text is only breadcrumb, responsible-person name and FS timestamp. The only weakness is that the reading list is emp... |
| nla | sonnet | 28 | Where text is extracted (15 of 28 reviewed courses) it is complete, well structured and for the right year; the three 'year' flags are false alarms (older years mentioned inside the plan text). The main weakness is that 13 of the 25 random controls have a fetched page but empty text, and the pre-... |
| nmbu | sonnet | 8 | Extraction with selector .layout is complete and clean for all 8 fetched courses: the packet shows no uncaptured page text, and the facts box, outcomes, teaching, assessment, literature and prerequisites are all present. Remaining issues are cosmetic: facts-box labels run into their values, one l... |
| nord | sonnet | 28 | For offerings that have a plan, div.node__content captures the whole plan cleanly with good structure, and the random controls show no loss of content. Problems are (a) many pages, especially pre-2023 and discontinued courses, have no plan text so only the facts box is extracted, (b) older years ... |
| ntnu | sonnet | 33 | Extraction is good: all 28 fetched pages hold the facts box, description, learning outcomes, teaching, assessment, requirements, prerequisites, reading list and exam overview, with the right academic year (spring offerings correctly read the previous year's page). The 20 'uncaptured' flags are fa... |
| oslomet | sonnet | 28 | Where the page has a plan, extraction is complete and clean (learning outcomes, teaching, assessment, exam sections all present; random controls are good). But a large share of offerings (15 of 16 suspects, 30 short texts of 189) contain only the facts box and programme list because the fetched p... |
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
| ✓ | inn | whole_text | wrong_year | high | widespread | inn_3JUS330-1_2023_spring_1; inn_2MEN5101-5-1_2023_spring... | Detect the 'Statusmelding' fallback text ('ikke publisert enda' / 'Kun tilgjengelig på engelsk' is a separate language notice) and mark t... |
| ✓ | nla | whole_text | empty_or_failed | high | widespread | nla_4POPM101-1_2025_autumn_1; nla_IKF305-1_2022_spring_1;... | For 5 of the empty offerings list the year keys present in the page JSON and compare them with the offering's academic year; fix the key ... |
| ✓ | uis | whole_text | empty_or_failed | high | widespread | uis_MLI355-1_2022_spring_1; uis_MGL4037-1_2023_autumn_1; ... | Inspect the raw html for a few of these courses. If the page is a stub, make html_pdf_discovery try the Stine PDF for each year first. If... |
| ✓ | hvl | whole_text | missing_content | high | common | hvl_MAS538-1_2025_spring_1; hvl_ADA512-1_2025_autumn_1; h... | In the hvl harvest or post_fn, detect the notice ('kun på engelsk' / 'berre skildra på engelsk' / 'engelske nettsider') and re-fetch the ... |
| ✓ | nord | whole_text | empty_or_failed | high | common | nord_FORGLU-1_2019_autumn_1; nord_SOS9001-1_2022_autumn_1... | Flag offerings whose extracted text lacks any plan heading (Beskrivelse av emnet, Læringsutbytte, Eksamensbeskrivelse) as 'no plan' (e.g.... |
| ✓ | oslomet | whole_text | empty_or_failed | high | common | oslomet_FK2200-1_2025_spring_3; oslomet_ARKROM6100-1_2025... | Add a content test in the OsloMet post-step or fetch validation: if the text has no 'Læringsutbytte'/'Innledning' heading (or is < ~400 c... |
| ✓ | uib | whole_text | empty_or_failed | high | common | uib_TYS307L-0_2017_spring_2; uib_HIDID111-0_2010_spring_1... | In the UiB config or the extract_fulltext post step, treat a text with no 'Mål og innhold' or 'Læringsutbytte' section (text ends at 'Emn... |
| ✓ | inn | whole_text | wrong_page | high | occasional | inn_1AKT01-1_2023_autumn_1; inn_2PRLU20-29-1_2025_spring_... | In R/institution_config.R inn post/validation step: treat a page whose text starts with 'Emnesøket gjelder kun fra 2022' (or lacks 'Emnet... |
| ✓ | oslomet | whole_text | wrong_page | high | rare | oslomet_FADUFA-1_2018_spring_1 | Treat text matching 'Siden du leter etter finnes ikke' as a failed fetch (empty extracted_text) via a post_fn or fetch check for oslomet. |
| ✓ | steiner | whole_text | wrong_page | high | rare | steiner_M-PEL2.1_2025_autumn_1 | Check the code-to-section matching in the pdf_split strategy (R/harvest_strategies.R / institution_config.R steiner entry); verify the ex... |
| ✓ | hiof | whole_text | empty_or_failed | medium | widespread | hiof_LMUKHV10421-1_2024_spring_1 | Cross-check the 404 offerings against the course list: for each 404 try the alternate path and confirm the course was offered that year; ... |
| ✓ | hvl | reading_list | missing_content | medium | widespread | hvl_MAS538-1_2025_autumn_1 | Inspect the raw html of one HVL page for the 'Pensumliste/Reading lists' element; if it is in the html, widen the selector or add it as a... |
| ✓ | mf | metadata | wrong_year | medium | widespread | mf_PED1010-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf... | Document that MF is current-plan-only (plan_years = 'current') and do not treat offering year as the plan year; optionally parse the page... |
| ✓ | mf | reading_list | missing_content | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | Inspect the raw html for the literature link target; if it is a separate URL, harvest it and append to the text, otherwise record that MF... |
| ✓ | hiof | reading_list | missing_content | medium | common | hiof_LMBENG11321-1_2025_spring_1; hiof_LMBENG10125-1_2025... | Treat the Leganto pointer sentence as a placeholder in section extraction (no reading_list content) and note in data_notes.qmd that readi... |
| ✓ | nord | whole_text | wrong_year | medium | common | nord_RET1002-1_2019_spring_2; nord_RE214E-1_2019_spring_1... | Keep the plan-year heading (outside div.node__content) in the harvest metadata and flag/exclude offerings where the heading says 'Gjelden... |
| ✓ | oslomet | reading_list | missing_content | medium | common | oslomet_DTV3301-1_2022_autumn_1; oslomet_PHUV9330-1_2022_... | If reading lists are wanted, add a second fetch of the Pensum URL for oslomet; otherwise document that reading_list is empty by design. |
| ✓ | uia | whole_text | other | medium | common | uia_KH-503-1_2023_autumn_1; uia_IDR509-1_2023_autumn_1; u... | Mark offerings whose extracted text has no plan heading (Læringsutbytte/Innhold/Eksamen) as 'no plan' (e.g. in the gaps target) so they a... |
| ✓ | ntnu | course_content | other | medium | occasional | ntnu_SMF1081-1_2022_autumn_3; ntnu_RAD6020-1_2022_spring_... | Keep the not-taught notice in extracted_text, or mark offerings whose page carries it, so the frame can drop them; drop a trailing empty ... |
| ✓ | inn | page_chrome | junk_included | low | widespread | inn_SYD2009-1_2023_autumn_2; inn_2M352-1_2023_spring_1 | Add exclude for the Statusmelding block (and its heading) in the inn entry of R/institution_config.R, after recording the fallback flag f... |
| ✓ | mf | reading_list | junk_included | low | widespread | mf_RL1016-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1 | Add an exclude/post_fn rule in the mf config removing the 'Tilgang til litteratur:' block through 'MERK: Sjekk at litteraturlisten…' (kee... |
| ✓ | nih | whole_text | empty_or_failed | low | widespread | nih_LKI152-1_2024_spring_2 | Spot check a few 404 URLs for courses known to be offered (e.g. LKI152 spring 2024) in a browser; if they exist elsewhere, adjust the URL... |
| ✓ | nmbu | metadata | formatting | low | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_M30-LUN-1_2025_autumn_1 | In the block text conversion, join child text nodes of the facts box with a space (or newline), or add a post_fn that inserts a space aft... |
| ✓ | ntnu | assessment | missing_content | low | widespread | ntnu_BEV2101-1_2019_spring_1; ntnu_TS200114-1_2022_spring... | Leave as is for dates and rooms; optionally keep the 'Øvrige kommentarer' paragraph. Add an NTNU exception to the 'uncaptured' pre-pass s... |
| ✓ | ntnu | whole_text | junk_included | low | widespread | ntnu_BEV2101-1_2019_spring_1; ntnu_BK3380-1_2019_autumn_1 | Add the literal line 'course-details-portlet' and the 'Emne - ... - CODE' title line to the NTNU exclude/post_fn. |
| ✓ | uio | assessment | junk_included | low | widespread | uio_PROMO1003-1_2025_autumn_2; uio_BIOS5000-1_2025_autumn... | Add an exclude (or post_fn line filter) for the shared exam link list and the 'Andre veiledninger og ressurser...' sentence, and for the ... |
| ✓ | usn | page_chrome | junk_included | low | widespread | usn_MG1MA3-1_2019_spring_2; usn_MG2NA2-1_2024_spring_1; u... | In the usn post_fn, drop the block from the 'Innholdsfortegnelse' line up to the second occurrence of 'Faglig innhold i emnet', or exclud... |
| ✓ | usn | reading_list | formatting | low | widespread | usn_MG2NA2-1_2024_spring_1; usn_MG2NO1-1_2019_autumn_1; u... | Insert a newline after known Leganto type labels (Book, Book Chapter, Article, Electronic Article, Website, Government Document, Journal,... |
| ✓ | hivolda | reading_list | missing_content | low | common | hivolda_HI100N-1_2025_autumn_1; hivolda_MEI301-1_2025_aut... | If reading lists are wanted, follow the 'Pensumliste' link in the url_discovery strategy and append the linked page text; otherwise docum... |
| ✓ | inn | metadata | formatting | low | common | inn_3JUS330-1_2023_spring_1; inn_2ENL51-9-1_2025_spring_1 | Low priority; optionally emit 'label: value' for the facts box in the inn post_fn. |
| ✓ | mf | assessment | duplicate_content | low | common | mf_PED5610-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1 | Optional: de-duplicate identical consecutive paragraphs in post_fn for mf; otherwise accept as source duplication. |
| ✓ | nih | reading_list | missing_content | low | common | nih_LKI221-1_2024_autumn_1; nih_LKI106-1_2025_spring_1; n... | Accept as a source limitation and let section extraction treat stub-only reading_list as empty; optionally check whether the pensum link ... |
| ✓ | nla | reading_list | missing_content | low | common | nla_P320-1_2025_autumn_1; nla_3GJ302-1_2025_autumn_1; nla... | Append the href of the link in the digital reading list block to the text, or mark the reading list as external in the section step. Chec... |
| ✓ | nmbu | page_chrome | junk_included | low | common | nmbu_FYS100-1_2025_autumn_1; nmbu_PPPE301-1_2025_autumn_1 | Add a post_fn or exclude pattern in R/institution_config.R for the nmbu entry that drops the line 'Her finner du KI-kategoriene beskrevet.'. |
| ✓ | nord | page_chrome | junk_included | low | common | nord_RE214E-1_2019_spring_1; nord_IDR1037-1_2025_spring_1... | Add the overlap boilerplate sentence and the link-label lines to the Nord `exclude`/post_fn patterns (boilerplate only, as per course_pla... |
| ✓ | ntnu | metadata | formatting | low | common | ntnu_KLMED8004-1_2019_autumn_1; ntnu_TKT4215-1_2019_spring_1 | Join table rows with newlines and cells with ' \| ' in the block step, or exclude the Studiepoengreduksjon block and its explanation text. |
| ✓ | steiner | page_chrome | junk_included | low | common | steiner_M-MAT1_2_2025_autumn_1; steiner_M-MAT1_1_2025_aut... | Add a post_fn for steiner that drops lines matching ^\s*\d{1,3}\s*$ (and rejoins the split sentence/list item where a page break interrup... |
| ✓ | steiner | learning_outcomes | formatting | low | common | steiner_M-NAT1_2_2025_autumn_1; steiner_M-SAM1_2_2025_aut... | In post-processing join continuation lines (line not starting with bullet/heading and previous line not ending in sentence punctuation), ... |
| ✓ | uia | metadata | wrong_year | low | common | uia_KH-503-1_2023_autumn_1; uia_NAT200-1_2023_autumn_1; u... | Do not derive semester from the page's Undervisningssemester; use the offering's own semester field. |
| ✓ | uib | assessment | missing_content | low | common | uib_FRAN313L-0_2025_autumn_1; uib_NODI302-0_2024_spring_1 | No change needed if exam dates count as logistics. If the assessment form (for example 'Skriftleg skoleeksamen', 'Mappevurdering', 'Varig... |
| ✓ | uio | whole_text | empty_or_failed | low | common | uio_BIOS5000-1_2025_spring_1 | Check in harvest_strategies.R whether non-fetched UiO offerings are URL duplicates; if so, document it and have downstream steps reuse th... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_ENG112-1_2025_autumn_2; uis_PPU118-1_2025_autumn_3 | Add exclude patterns in R/institution_config.R for the footer line and the 'Opptak til enkeltemner'/'Utveksling til' blocks, or cut the '... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_MLH150-1_2013_spring_1; uis_MTLMAS-1_2012_spring_1; u... | Remove lines matching ^\s*side \d+\s*$ and the repeated '<CODE> - <title>' header in the PDF post_fn. |
| ✓ | uit | metadata | duplicate_content | low | common | uit_FRA-3903-1_2009_spring_1; uit_FRA-3903-1_2010_spring_... | In the uit post_fn, drop the header line matching '^<code> .* - \d+ stp$' and the split variant (consecutive lines code / blank / title /... |
| ✓ | usn | reading_list | duplicate_content | low | common | usn_MG2NO1-1_2019_autumn_1; usn_MG2NA2-1_2024_spring_1 | Drop lines before the Leganto 'Click to view' label in the Litteratur section, or collapse them to unique values. |
| ✓ | hvl | course_content | duplicate_content | low | occasional | hvl_ØMF190-1_2022_autumn_1; hvl_EVL-MA115-1_2019_autumn_1 | Collapse consecutive identical heading lines in the hvl post_fn. |
| ✓ | hvl | whole_text | formatting | low | occasional | hvl_LU1-PRA3-1_2017_autumn_1; hvl_YV204-1_2017_autumn_1; ... | Check whether the raw html already contains '¿' (then it is a source defect, document it in data notes); otherwise verify the decoding. O... |
| ✓ | nla | whole_text | formatting | low | occasional | nla_TAM301-1_2022_autumn_1; nla_REL208-1_2019_autumn_2; n... | Check the raw JSON for the character; if it is in the source, add a post_fn for nla that maps ¿ between digits/letters to an en dash or h... |
| ✓ | nord | course_content | duplicate_content | low | occasional | nord_ORG2007-1_2025_autumn_1; nord_PRO9019-1_2025_autumn_... | In the Nord post_fn, drop the unlabelled block between the code line and 'Beskrivelse av emnet' when it equals (or is a prefix of) the fi... |
| ✓ | ntnu | learning_outcomes | formatting | low | occasional | ntnu_SKOLE6249-1_2022_autumn_1; ntnu_RAD6020-1_2022_spring_1 | Insert a newline for <br> and a space between adjacent inline text nodes in the block-to-text step (R/blocks.R); add known glued headings... |
| ✓ | oslomet | page_chrome | junk_included | low | occasional | oslomet_VESK6300-1_2025_spring_1; oslomet_DKLH6000-1_2019... | Drop the Pensum/Timeplan link block in the oslomet exclude selector or post_fn; consider following the Pensum link for reading_list. |
| ✓ | uis | course_content | duplicate_content | low | occasional | uis_MGL3126-1_2022_autumn_1; uis_MGL2060-1_2021_autumn_1;... | Optionally drop a paragraph in the PDF text that is identical to one already seen in the same text. Low priority. |
| ✓ | uit | learning_outcomes | formatting | low | occasional | uit_PFF-1001-1_2009_autumn_1 | Check the raw html of this page for <br> or <li> in these blocks. If present, make blocks.R treat them as line breaks. If the source is a... |
| ✓ | hiof | reading_list | wrong_year | low | rare | hiof_LMUSPA10124-1_2025_autumn_1 | No pipeline change; do not use the year in the Leganto sentence for year validation. |
| ✓ | nla | course_content | formatting | low | rare | nla_3GJ302-1_2025_autumn_1; nla_EX101-1_2017_autumn_1 | Treat inline headings (strong/b followed directly by text) as block breaks in the json block reader; low priority. |
| ✓ | nmbu | metadata | wrong_year | low | rare | nmbu_FYS100-1_2025_autumn_1 | Document in methods that NMBU text is the current plan; avoid joining the semester text to the offering semester, or dedupe offerings per... |
| ✓ | nord | whole_text | empty_or_failed | low | rare | nord_ØKO110FU-1_2019_spring_3 | Check whether https://www.nord.no/studier/emner/%C3%B8ko110fu loads; if so, URL-encode the code in the Nord URL builder. |
| ✓ | ntnu | whole_text | empty_or_failed | low | rare | ntnu_PHMT/AVHANDL-1_2019_autumn_4 | Check how ntnu.no links codes containing '/' (encoded slash or a different path) and encode it in the NTNU URL builder in R/add_course_ur... |
| ✓ | oslomet | course_content | formatting | low | rare | oslomet_DTV3301-1_2022_autumn_1; oslomet_MABIOD4300-1_202... | Optionally collapse runs of ';' and strip '¿' in the oslomet post_fn. |
| ✓ | uia | metadata | wrong_page | low | rare | uia_REL420-1_2021_autumn_2 | Flag offerings whose page title differs strongly from the metadata title for manual review; no extraction change needed. |
| ✓ | uia | course_content | formatting | low | rare | uia_PRA044-1_2021_autumn_1; uia_REL420-1_2021_autumn_2 | In the block-to-text conversion insert a newline for <br> and list items inside #right-main. |
| ✓ | uia | course_content | duplicate_content | low | rare | uia_NO-155-1_2023_autumn_1 | Check the HTML of NO-155; if the row is not duplicated in source, deduplicate adjacent identical table lines in blocks.R. |
| ✓ | uio | metadata | formatting | low | rare | uio_GEO5930L-2_2025_autumn_1 | Render the facts box as 'Label: value' pairs, with an explicit empty value, in the UiO post_fn or in a block formatter. |
| ✓ | uit | page_chrome | junk_included | low | rare | uit_LRU-1270-1_2011_spring_1; uit_MAT-3906-1_2012_spring_1 | Add a uit post_fn in R/institution_config.R that strips literal <...> tags (e.g. gsub('<[^>]+>', ' ', x)) and decodes HTML entities (&ari... |
| ✓ | uit | assessment | duplicate_content | low | rare | uit_LER-1060-1_2025_autumn_1 | No change needed. Optionally note in data notes that UiT pensum lists on new pages are external (Leganto) and not in the text. |
| ✓ | usn | learning_outcomes | duplicate_content | low | rare | usn_MG1KH2-1_2018_autumn_1 | Collapse consecutive identical heading lines. |
