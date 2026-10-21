# fulltext audit — aggregated findings

70 findings across 18 institutions (from 18 agent reports in `data/audit/fulltext/findings`; model: sonnet).
Mechanical verification: 70 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| page_chrome | junk_included | 9 | hiof, inn, nmbu, nord, oslomet, steiner, uia, uis, usn | low |
| whole_text | empty_or_failed | 7 | nih, nla, nord, ntnu, oslomet, uio, uis | high |
| reading_list | missing_content | 7 | hivolda, hvl, mf, nih, nla, oslomet, uit | medium |
| course_content | duplicate_content | 5 | hvl, nord, uib, uis, uit | low |
| metadata | formatting | 5 | inn, nmbu, ntnu, uib, uio | low |
| whole_text | wrong_page | 4 | inn, oslomet, steiner, usn | high |
| whole_text | wrong_year | 3 | hiof, inn, nord | high |
| whole_text | formatting | 3 | hvl, nla, uit | low |
| metadata | wrong_year | 3 | mf, nmbu, uia | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hiof | 3 | 2 |  | whole_text / wrong_year; page_chrome / junk_included | reading_list / missing_content; reading_list / wrong_year; whole_text / empty_or_failed |
| nih | 2 | 2 | whole_text / empty_or_failed; reading_list / missing_content |  |  |
| uia | 5 | 4 | whole_text / other; metadata / wrong_year | page_chrome / junk_included; metadata / duplicate_content | metadata / wrong_page; course_content / formatting; course_content / duplicate_content |
| uib | 2 | 3 | assessment / missing_content | course_content / duplicate_content; metadata / formatting | whole_text / empty_or_failed |
| uit | 4 | 5 |  | reading_list / missing_content; reading_list / junk_included; course_content / duplicate_content; teaching_methods / junk_included; whole_text / formatting | page_chrome / junk_included; metadata / duplicate_content; learning_outcomes / formatting; assessment / duplicate_content |
| usn | 4 | 6 | page_chrome / junk_included; reading_list / formatting; reading_list / duplicate_content | whole_text / other; whole_text / wrong_page; metadata / other | learning_outcomes / duplicate_content |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| junk_included | low | 15 |
| formatting | low | 13 |
| duplicate_content | low | 8 |
| missing_content | medium | 5 |
| empty_or_failed | high | 4 |
| empty_or_failed | low | 4 |
| missing_content | low | 4 |
| wrong_page | high | 3 |
| other | low | 2 |
| other | medium | 2 |
| wrong_year | high | 2 |
| wrong_year | low | 2 |
| wrong_year | medium | 2 |
| duplicate_content | medium | 1 |
| empty_or_failed | medium | 1 |
| missing_content | high | 1 |
| wrong_page | medium | 1 |

## By target

| target | n |
| --- | --- |
| whole_text | 23 |
| metadata | 11 |
| reading_list | 11 |
| page_chrome | 10 |
| course_content | 8 |
| assessment | 4 |
| learning_outcomes | 2 |
| teaching_methods | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | sonnet | 28 | Extraction works well for HiOF: the plan (facts box, content, learning outcomes, teaching, requirements, assessment, evaluation, literature) is captured completely and cleanly structured on all 28 pages; the only uncaptured text is the breadcrumb. The one real problem is that for some offerings t... |
| hivolda | sonnet | 28 | Extraction works well for hivolda: all 28 random controls contain the whole plan (facts box, description, learning outcomes, organisation, requirements, assessment table) with clean line structure, correct course and year, and no chrome or junk. Uncaptured page text is only year pickers, the repe... |
| hvl | sonnet | 28 | Extraction works well for ordinary pages: the 22 random controls are complete, include facts box, outcomes, teaching, assessment and requirements, and are free of chrome; years are correct (spring offerings use the previous academic-year URL). The one real problem is English-only courses, whose N... |
| inn | sonnet | 28 | Where the page holds a real plan, extraction is complete and clean (all sections, facts box, no navigation). Two institution-wide problems remain: many offerings get the latest published plan under a 'not published yet' notice (wrong year), and some course codes return the course-search listing i... |
| mf | sonnet | 8 | Extraction at MF is clean and complete for the plan body: facts box, description, study requirements, assessment, learning outcomes and overlap sections are all captured, and only staff contact boxes are left out. Remaining issues are that the page is the current (2026) plan labelled as 2025 offe... |
| nih | sonnet | 25 | Extraction works well for the 21 fetched pages: complete plan text with facts box, correct year, readable structure, and the uncaptured text is only breadcrumb, responsible person and FS timestamp. The weaknesses are coverage (166 of 190 offerings return 404, one URL is malformed) and reading lis... |
| nla | sonnet | 28 | Where text is extracted (15 of 28 reviewed courses) it is complete, well structured and for the right year; the three 'year' flags are false alarms (older years mentioned inside the plan text). The main weakness is that 13 of the 25 random controls have a fetched page but empty text, and the pre-... |
| nmbu | sonnet | 8 | Extraction with selector .layout is complete and clean for all 8 fetched courses: the packet shows no uncaptured page text, and the facts box, outcomes, teaching, assessment, literature and prerequisites are all present. Remaining issues are cosmetic: facts-box labels run into their values, one l... |
| nord | sonnet | 28 | For offerings that have a plan, div.node__content captures the whole plan cleanly with good structure, and the random controls show no loss of content. Problems are (a) many pages, especially pre-2023 and discontinued courses, have no plan text so only the facts box is extracted, (b) older years ... |
| ntnu | sonnet | 33 | Extraction is good: all 28 fetched pages hold the facts box, description, learning outcomes, teaching, assessment, requirements, prerequisites, reading list and exam overview, with the right academic year (spring offerings correctly read the previous year's page). The 20 'uncaptured' flags are fa... |
| oslomet | sonnet | 28 | Where the page has a plan, extraction is complete and clean (learning outcomes, teaching, assessment, exam sections all present; random controls are good). But a large share of offerings (15 of 16 suspects, 30 short texts of 189) contain only the facts box and programme list because the fetched p... |
| steiner | sonnet | 8 | Extraction from the Steiner PDF is complete and readable: all seven course-plan parts are present in all eight controls. Defects are limited to PDF page-number lines left in the text, lost bullet glyphs/broken lines, and one offering (M-PEL2.1) whose text is a different course's plan. |
| uia | sonnet | 28 | Extraction with #right-main works well at UiA: all 27 random controls are complete, well-structured plans (learning outcomes, content, teaching, requirements, exam), and the only uncaptured page text is the breadcrumb. The one 'short' suspect is a stub page with only the facts box; remaining issu... |
| uib | sonnet | 28 | Where UiB has a plan page, extraction is complete and clean: the 19 random controls hold all plan sections, with the year picker, error banners and contact lines correctly left out. The main weakness is pages with no plan, where the text is only the facts box and bare headings. Exam logistics (da... |
| uio | sonnet | 8 | UiO fulltext extraction is good: all 8 fetched plans contain the full plan (intro, learning outcomes, admission, teaching, exam, facts box) and the uncaptured page text is only breadcrumbs, fetch timestamps and year pickers. The main defect is a recurring block of generic exam-information link la... |
| uis | sonnet | 28 | PDF-sourced plans (www6.uis.no/Stine) and the controls on current uis.no course pages are complete and mostly clean. All 20 suspects are empty: uis.no/nb/student/course pages with no extracted text and no stored page text, so nothing can be said about the page content. Minor noise: PDF page marke... |
| uit | sonnet | 28 | UiT extraction via #maincontents is generally faithful: whole plans for the right course and year, no site chrome, and the uncaptured text is only year pickers, apply-period banners and staff boxes. The four suspects are false alarms (thin source page, year-picker lines) apart from minor junk on ... |
| usn | sonnet | 28 | USN extraction is good: all 28 plans hold the full plan text for the right course code and year, the 'Page text not in extracted text' is empty everywhere, and no wrong-page, wrong-year or empty cases appear. The 3 suspects are false alarms (genuinely sparse or long plans). Remaining problems are... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | inn | whole_text | wrong_year | high | widespread | inn_3JUS330-1_2023_spring_1; inn_2MEN5101-5-1_2023_spring... | Detect the 'Statusmelding' fallback text ('ikke publisert enda' / 'Kun tilgjengelig på engelsk' is a separate language notice) and mark t... |
| ✓ | nla | whole_text | empty_or_failed | high | widespread | nla_4POPM101-1_2025_autumn_1; nla_IKF305-1_2022_spring_1;... | For 5 of the empty offerings list the year keys present in the page JSON and compare them with the offering's academic year; fix the key ... |
| ✓ | uis | whole_text | empty_or_failed | high | widespread | uis_MLI355-1_2022_spring_1; uis_MGL4037-1_2023_autumn_1; ... | Inspect the raw html for a few of these courses. If the page is a stub, make html_pdf_discovery try the Stine PDF for each year first. If... |
| ✓ | hvl | whole_text | missing_content | high | common | hvl_MAS538-1_2025_spring_1; hvl_ADA512-1_2025_autumn_1; h... | In the hvl harvest or post_fn, detect the notice ('kun på engelsk' / 'berre skildra på engelsk' / 'engelske nettsider') and re-fetch the ... |
| ✓ | nord | whole_text | empty_or_failed | high | common | nord_FORGLU-1_2019_autumn_1; nord_SOS9001-1_2022_autumn_1... | Flag offerings whose extracted text lacks any plan heading (Beskrivelse av emnet, Læringsutbytte, Eksamensbeskrivelse) as 'no plan' (e.g.... |
| ✓ | oslomet | whole_text | empty_or_failed | high | common | oslomet_FK2200-1_2025_spring_3; oslomet_ARKROM6100-1_2025... | Add a content test in the OsloMet post-step or fetch validation: if the text has no 'Læringsutbytte'/'Innledning' heading (or is < ~400 c... |
| ✓ | hiof | whole_text | wrong_year | high | occasional | hiof_LSKNO22215-1_2019_spring_1; hiof_LSKAS10116-1_2022_a... | Add a year check in the HiOF fetch/post step: compare the year in the page title '(Vår/Høst YYYY)' with the offering's term and mark or d... |
| ✓ | inn | whole_text | wrong_page | high | occasional | inn_1AKT01-1_2023_autumn_1; inn_2PRLU20-29-1_2025_spring_... | In R/institution_config.R inn post/validation step: treat a page whose text starts with 'Emnesøket gjelder kun fra 2022' (or lacks 'Emnet... |
| ✓ | oslomet | whole_text | wrong_page | high | rare | oslomet_FADUFA-1_2018_spring_1 | Treat text matching 'Siden du leter etter finnes ikke' as a failed fetch (empty extracted_text) via a post_fn or fetch check for oslomet. |
| ✓ | steiner | whole_text | wrong_page | high | rare | steiner_M-PEL2.1_2025_autumn_1 | Check the code-to-section matching in the pdf_split strategy (R/harvest_strategies.R / institution_config.R steiner entry); verify the ex... |
| ✓ | hvl | reading_list | missing_content | medium | widespread | hvl_MAS538-1_2025_autumn_1 | Inspect the raw html of one HVL page for the 'Pensumliste/Reading lists' element; if it is in the html, widen the selector or add it as a... |
| ✓ | mf | metadata | wrong_year | medium | widespread | mf_PED1010-1_2025_autumn_1; mf_RL1013-1_2025_autumn_1; mf... | Document that MF is current-plan-only (plan_years = 'current') and do not treat offering year as the plan year; optionally parse the page... |
| ✓ | mf | reading_list | missing_content | medium | widespread | mf_RL1016-1_2025_autumn_1; mf_PED5610-1_2025_autumn_1; mf... | Inspect the raw html for the literature link target; if it is a separate URL, harvest it and append to the text, otherwise record that MF... |
| ✓ | nih | whole_text | empty_or_failed | medium | widespread | nih_PHD610-1_2021_autumn_1; nih_TI201-1_2021_autumn_1; ni... | Spot check some 404 URLs in a browser (e.g. thp311 2021, ti201 2021); if pages exist under another path or the archive (emnearkivet), adj... |
| ✓ | nord | whole_text | wrong_year | medium | common | nord_RET1002-1_2019_spring_2; nord_RE214E-1_2019_spring_1... | Keep the plan-year heading (outside div.node__content) in the harvest metadata and flag/exclude offerings where the heading says 'Gjelden... |
| ✓ | oslomet | reading_list | missing_content | medium | common | oslomet_DTV3301-1_2022_autumn_1; oslomet_PHUV9330-1_2022_... | If reading lists are wanted, add a second fetch of the Pensum URL for oslomet; otherwise document that reading_list is empty by design. |
| ✓ | uib | assessment | missing_content | medium | common | uib_ENG120-0_2025_spring_1; uib_GEOV113-0_2022_autumn_1; ... | If exam form and duration are wanted, add the exam-info container as a second selector and drop dates, rooms and the 'Klokkeslett for opp... |
| ✓ | uit | reading_list | missing_content | medium | common | uit_HIF-2160-1_2025_spring_1; uit_LER-1210-2_2025_spring_... | Confirm in the raw html whether a Leganto list id or URL is present and, if so, fetch the list via site_request(); otherwise record in da... |
| ✓ | ntnu | course_content | other | medium | occasional | ntnu_SMF1081-1_2022_autumn_3; ntnu_RAD6020-1_2022_spring_... | Keep the not-taught notice in extracted_text, or mark offerings whose page carries it, so the frame can drop them; drop a trailing empty ... |
| ✓ | usn | reading_list | duplicate_content | medium | occasional | usn_MET1020-1_2025_spring_2; usn_MG1SA1-1_2025_spring_1 | Drop the campus list-name lines that precede 'Click to view', and deduplicate repeated reference blocks within the reading_list section (... |
| ✓ | uia | whole_text | other | medium | rare | uia_HEL905-1_2022_autumn_1 | Treat offerings whose extracted text lacks any plan heading (Læringsutbytte, Innhold, Eksamen) as 'no plan' (e.g. report in the gaps targ... |
| ✓ | usn | whole_text | wrong_page | medium | rare | usn_BABLU-6_2018_autumn_1 | Check how R/add_course_url.R builds the code for ids ending in -N with N other than 1, and verify that BABLU_3 and BABLU_6 are not both m... |
| ✓ | hiof | page_chrome | junk_included | low | widespread | hiof_LPKR116-1_2019_autumn_1; hiof_LMBNOR40117-1_2025_aut... | Add an exclude or post_fn regex for '^Sist hentet fra FS.*$' (and optionally the lone 'Innholdsfortegnelse' heading) in the hiof entry of... |
| ✓ | inn | page_chrome | junk_included | low | widespread | inn_SYD2009-1_2023_autumn_2; inn_2M352-1_2023_spring_1 | Add exclude for the Statusmelding block (and its heading) in the inn entry of R/institution_config.R, after recording the fallback flag f... |
| ✓ | mf | reading_list | junk_included | low | widespread | mf_RL1016-1_2025_autumn_1; mf_RL2030-1_2025_autumn_1 | Add an exclude/post_fn rule in the mf config removing the 'Tilgang til litteratur:' block through 'MERK: Sjekk at litteraturlisten…' (kee... |
| ✓ | nih | reading_list | missing_content | low | widespread | nih_THP311-1_2025_autumn_1; nih_LKI105-1_2022_spring_1; n... | Treat stub-only reading_list sections as empty in section extraction; optionally check whether the pensum link target is harvestable. |
| ✓ | nmbu | metadata | formatting | low | widespread | nmbu_PPFD201-1_2025_autumn_2; nmbu_M30-LUN-1_2025_autumn_1 | In the block text conversion, join child text nodes of the facts box with a space (or newline), or add a post_fn that inserts a space aft... |
| ✓ | ntnu | assessment | missing_content | low | widespread | ntnu_BEV2101-1_2019_spring_1; ntnu_TS200114-1_2022_spring... | Leave as is for dates and rooms; optionally keep the 'Øvrige kommentarer' paragraph. Add an NTNU exception to the 'uncaptured' pre-pass s... |
| ✓ | ntnu | whole_text | junk_included | low | widespread | ntnu_BEV2101-1_2019_spring_1; ntnu_BK3380-1_2019_autumn_1 | Add the literal line 'course-details-portlet' and the 'Emne - ... - CODE' title line to the NTNU exclude/post_fn. |
| ✓ | uia | page_chrome | junk_included | low | widespread | uia_LIT606-1_2022_autumn_1; uia_FYS008-G_2020_autumn_1; u... | Add exclude/post_fn for the 'Innholdsfortegnelse' heading and a regex dropping '^Sist hent(et\|a) fra FS .*$' (also nynorsk 'Sist henta fr... |
| ✓ | uio | assessment | junk_included | low | widespread | uio_PROMO1003-1_2025_autumn_2; uio_BIOS5000-1_2025_autumn... | Add an exclude (or post_fn line filter) for the shared exam link list and the 'Andre veiledninger og ressurser...' sentence, and for the ... |
| ✓ | usn | page_chrome | junk_included | low | widespread | usn_SMERTE100-1_2022_autumn_1; usn_KH-EMNE2-1_2022_spring... | Drop the block from the 'Innholdsfortegnelse' / 'Table of contents' line up to the next heading that repeats its first entry, in the usn ... |
| ✓ | usn | reading_list | formatting | low | widespread | usn_HIS2010-1_2025_autumn_2; usn_REG3000D-1_2019_autumn_1... | Insert a newline after 'Leganto' and after the known type labels (Book, Book Chapter, Article, E-book, Journal, Website, Document, Note),... |
| ✓ | hivolda | reading_list | missing_content | low | common | hivolda_HI100N-1_2025_autumn_1; hivolda_MEI301-1_2025_aut... | If reading lists are wanted, follow the 'Pensumliste' link in the url_discovery strategy and append the linked page text; otherwise docum... |
| ✓ | inn | metadata | formatting | low | common | inn_3JUS330-1_2023_spring_1; inn_2ENL51-9-1_2025_spring_1 | Low priority; optionally emit 'label: value' for the facts box in the inn post_fn. |
| ✓ | mf | assessment | duplicate_content | low | common | mf_PED5610-1_2025_autumn_1; mf_PED1010-1_2025_autumn_1 | Optional: de-duplicate identical consecutive paragraphs in post_fn for mf; otherwise accept as source duplication. |
| ✓ | nla | reading_list | missing_content | low | common | nla_P320-1_2025_autumn_1; nla_3GJ302-1_2025_autumn_1; nla... | Append the href of the link in the digital reading list block to the text, or mark the reading list as external in the section step. Chec... |
| ✓ | nmbu | page_chrome | junk_included | low | common | nmbu_FYS100-1_2025_autumn_1; nmbu_PPPE301-1_2025_autumn_1 | Add a post_fn or exclude pattern in R/institution_config.R for the nmbu entry that drops the line 'Her finner du KI-kategoriene beskrevet.'. |
| ✓ | nord | page_chrome | junk_included | low | common | nord_RE214E-1_2019_spring_1; nord_IDR1037-1_2025_spring_1... | Add the overlap boilerplate sentence and the link-label lines to the Nord `exclude`/post_fn patterns (boilerplate only, as per course_pla... |
| ✓ | ntnu | metadata | formatting | low | common | ntnu_KLMED8004-1_2019_autumn_1; ntnu_TKT4215-1_2019_spring_1 | Join table rows with newlines and cells with ' \| ' in the block step, or exclude the Studiepoengreduksjon block and its explanation text. |
| ✓ | steiner | page_chrome | junk_included | low | common | steiner_M-MAT1_2_2025_autumn_1; steiner_M-MAT1_1_2025_aut... | Add a post_fn for steiner that drops lines matching ^\s*\d{1,3}\s*$ (and rejoins the split sentence/list item where a page break interrup... |
| ✓ | steiner | learning_outcomes | formatting | low | common | steiner_M-NAT1_2_2025_autumn_1; steiner_M-SAM1_2_2025_aut... | In post-processing join continuation lines (line not starting with bullet/heading and previous line not ending in sentence punctuation), ... |
| ✓ | uia | metadata | duplicate_content | low | common | uia_MAS416-G_2025_autumn_1; uia_MA-311-1_2025_autumn_2; u... | Drop the later 'Undervisningsspråk' heading and its value line via post_fn (the facts-box copy is kept). |
| ✓ | uia | metadata | wrong_year | low | common | uia_IDR210-1_2022_autumn_1; uia_MA-168-1_2022_autumn_1; u... | Never derive semester from the 'Undervisningssemester' line; rely on the offering's own semester field. |
| ✓ | uib | metadata | formatting | low | common | uib_EXFAC00SK-0_2025_spring_1; uib_MATDID350-0_2025_autumn_1 | Drop trailing empty headings and '-' placeholder values in a post_fn, or render 'Label: value' for the facts box. |
| ✓ | uio | whole_text | empty_or_failed | low | common | uio_BIOS5000-1_2025_spring_1 | Check in harvest_strategies.R whether non-fetched UiO offerings are URL duplicates; if so, document it and have downstream steps reuse th... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_ENG112-1_2025_autumn_2; uis_PPU118-1_2025_autumn_3 | Add exclude patterns in R/institution_config.R for the footer line and the 'Opptak til enkeltemner'/'Utveksling til' blocks, or cut the '... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_MLH150-1_2013_spring_1; uis_MTLMAS-1_2012_spring_1; u... | Remove lines matching ^\s*side \d+\s*$ and the repeated '<CODE> - <title>' header in the PDF post_fn. |
| ✓ | uit | reading_list | junk_included | low | common | uit_HIF-2160-1_2025_spring_1; uit_SPL-3004-1_2025_autumn_1 | Add the sentence 'Du kan se og få tilgang til deler av pensum via Leganto.' to the uit exclude/post_fn (boilerplate per course_plan_spec.... |
| ✓ | hvl | course_content | duplicate_content | low | occasional | hvl_ØMF190-1_2022_autumn_1; hvl_EVL-MA115-1_2019_autumn_1 | Collapse consecutive identical heading lines in the hvl post_fn. |
| ✓ | hvl | whole_text | formatting | low | occasional | hvl_LU1-PRA3-1_2017_autumn_1; hvl_YV204-1_2017_autumn_1; ... | Check whether the raw html already contains '¿' (then it is a source defect, document it in data notes); otherwise verify the decoding. O... |
| ✓ | nla | whole_text | formatting | low | occasional | nla_TAM301-1_2022_autumn_1; nla_REL208-1_2019_autumn_2; n... | Check the raw JSON for the character; if it is in the source, add a post_fn for nla that maps ¿ between digits/letters to an en dash or h... |
| ✓ | nord | course_content | duplicate_content | low | occasional | nord_ORG2007-1_2025_autumn_1; nord_PRO9019-1_2025_autumn_... | In the Nord post_fn, drop the unlabelled block between the code line and 'Beskrivelse av emnet' when it equals (or is a prefix of) the fi... |
| ✓ | ntnu | learning_outcomes | formatting | low | occasional | ntnu_SKOLE6249-1_2022_autumn_1; ntnu_RAD6020-1_2022_spring_1 | Insert a newline for <br> and a space between adjacent inline text nodes in the block-to-text step (R/blocks.R); add known glued headings... |
| ✓ | oslomet | page_chrome | junk_included | low | occasional | oslomet_VESK6300-1_2025_spring_1; oslomet_DKLH6000-1_2019... | Drop the Pensum/Timeplan link block in the oslomet exclude selector or post_fn; consider following the Pensum link for reading_list. |
| ✓ | uia | metadata | duplicate_content | low | occasional | uia_SYP212-1_2020_autumn_1; uia_SE-204-G_2020_autumn_4 | Deduplicate adjacent/identical table lines in post_fn; optionally drop the whole 'Reduksjon i studiepoeng' block as non-plan text. |
| ✓ | uis | course_content | duplicate_content | low | occasional | uis_MGL3126-1_2022_autumn_1; uis_MGL2060-1_2021_autumn_1;... | Optionally drop a paragraph in the PDF text that is identical to one already seen in the same text. Low priority. |
| ✓ | uit | teaching_methods | junk_included | low | occasional | uit_HIF-3022-1_2008_autumn_1; uit_STV-2010-1_2008_autumn_2 | In the uit post_fn drop lines starting with 'Lectures ' / 'Undervisning ' followed by a semester and the following 'Forelesning' table ro... |
| ✓ | uit | whole_text | formatting | low | occasional | uit_MUS-1510-1_2019_spring_1; uit_FRI-2580-1_2019_autumn_1 | Accept as a source limitation, or map '¿' at line start to a bullet and between digits to an en dash in the uit post_fn. |
| ✓ | usn | whole_text | other | low | occasional | usn_SMERTE100-1_2022_autumn_1; usn_MAR4020-1_2022_autumn_... | Add a post_fn replacement for '¿' used between spaces as an en-dash and as an apostrophe after a word (' ¿ ' to ' - ', 'x¿ ' to "x' "), o... |
| ✓ | nih | whole_text | empty_or_failed | low | rare | nih_MAS- ESSAY-1_2022_autumn_1 | Normalise the code in the NIH URL builder (remove spaces, check the real page slug, e.g. mas-essay) or skip codes that cannot form a vali... |
| ✓ | nla | course_content | formatting | low | rare | nla_3GJ302-1_2025_autumn_1; nla_EX101-1_2017_autumn_1 | Treat inline headings (strong/b followed directly by text) as block breaks in the json block reader; low priority. |
| ✓ | nmbu | metadata | wrong_year | low | rare | nmbu_FYS100-1_2025_autumn_1 | Document in methods that NMBU text is the current plan; avoid joining the semester text to the offering semester, or dedupe offerings per... |
| ✓ | nord | whole_text | empty_or_failed | low | rare | nord_ØKO110FU-1_2019_spring_3 | Check whether https://www.nord.no/studier/emner/%C3%B8ko110fu loads; if so, URL-encode the code in the Nord URL builder. |
| ✓ | ntnu | whole_text | empty_or_failed | low | rare | ntnu_PHMT/AVHANDL-1_2019_autumn_4 | Check how ntnu.no links codes containing '/' (encoded slash or a different path) and encode it in the NTNU URL builder in R/add_course_ur... |
| ✓ | oslomet | course_content | formatting | low | rare | oslomet_DTV3301-1_2022_autumn_1; oslomet_MABIOD4300-1_202... | Optionally collapse runs of ';' and strip '¿' in the oslomet post_fn. |
| ✓ | uib | course_content | duplicate_content | low | rare | uib_FSKLI901-0_2025_autumn_1; uib_FSKLI901-0_2025_spring_1 | Optional: drop exact-duplicate paragraphs of 80 or more characters within one text in a UiB post_fn. Low priority. |
| ✓ | uio | metadata | formatting | low | rare | uio_GEO5930L-2_2025_autumn_1 | Render the facts box as 'Label: value' pairs, with an explicit empty value, in the UiO post_fn or in a block formatter. |
| ✓ | uit | course_content | duplicate_content | low | rare | uit_JUR-3902-1_2022_autumn_1 | Check the raw html for the nested structure; emit separators between nested inline elements in R/blocks.R or deduplicate repeated list it... |
| ✓ | usn | metadata | other | low | rare | usn_EE505-1_2025_autumn_1 | No extraction change; consider flagging plans with no sections beyond the TOC and facts box downstream so they are not counted as full pl... |
