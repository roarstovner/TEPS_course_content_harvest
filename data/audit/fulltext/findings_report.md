# fulltext audit — aggregated findings

48 findings across 17 institutions (from 18 agent reports in `data/audit/fulltext/findings`; model: gpt-6.1-sol, sonnet).
Mechanical verification: 48 passed (✓), 0 failed (✗), 0 not checkable (?, stale report or no packet).
Sorted by severity then prevalence. Source: `R/audit/aggregate.R`.

## Failed verification — check by hand before acting

Unknown course ids or evidence that does not occur verbatim in the packet.
Usually a paraphrased quote; occasionally an invented finding.

_(none)_

## Patterns in 3+ institutions

| target | error_type | n_inst | institutions | worst |
| --- | --- | --- | --- | --- |
| page_chrome | junk_included | 12 | hiof, hvl, inn, mf, nmbu, ntnu, oslomet, steiner, uia, uio, uis, usn | medium |
| whole_text | wrong_page | 4 | nmbu, steiner, uio, usn | high |
| course_content | duplicate_content | 4 | nord, uib, uis, uit | low |
| whole_text | formatting | 4 | mf, nmbu, ntnu, uit | medium |
| reading_list | missing_content | 3 | hivolda, nih, uit | medium |

## Change since `HEAD` (keyed by target / error_type)

| institution | n_before | n_now | persisting | new | gone |
| --- | --- | --- | --- | --- | --- |
| hvl | 4 | 1 |  | page_chrome / junk_included | whole_text / missing_content; reading_list / missing_content; course_content / duplicate_content; whole_text / formatting |
| inn | 4 | 1 | page_chrome / junk_included |  | whole_text / wrong_page; whole_text / wrong_year; metadata / formatting |
| nla | 4 | 0 |  |  | whole_text / empty_or_failed; whole_text / formatting; course_content / formatting; reading_list / missing_content |
| nord | 4 | 1 | course_content / duplicate_content |  | whole_text / empty_or_failed; whole_text / wrong_year; page_chrome / junk_included |
| ntnu | 6 | 4 | assessment / missing_content; learning_outcomes / formatting | page_chrome / junk_included; whole_text / formatting | course_content / other; metadata / formatting; whole_text / junk_included; whole_text / empty_or_failed |
| oslomet | 5 | 1 | page_chrome / junk_included |  | whole_text / empty_or_failed; whole_text / wrong_page; reading_list / missing_content; course_content / formatting |

## By error type

| error_type | severity | n |
| --- | --- | --- |
| junk_included | low | 14 |
| formatting | low | 7 |
| duplicate_content | low | 6 |
| missing_content | medium | 4 |
| wrong_page | high | 3 |
| missing_content | low | 2 |
| other | low | 2 |
| other | medium | 2 |
| duplicate_content | medium | 1 |
| empty_or_failed | high | 1 |
| empty_or_failed | low | 1 |
| empty_or_failed | medium | 1 |
| formatting | medium | 1 |
| junk_included | medium | 1 |
| wrong_page | medium | 1 |
| wrong_year | low | 1 |

## By target

| target | n |
| --- | --- |
| page_chrome | 13 |
| whole_text | 13 |
| metadata | 7 |
| reading_list | 6 |
| course_content | 4 |
| assessment | 2 |
| learning_outcomes | 2 |
| teaching_methods | 1 |

## Per institution

| institution | model | n_courses_reviewed | overall_assessment |
| --- | --- | --- | --- |
| hiof | gpt-6.1-sol | 28 | All 28 courses were reviewed in full as presented in the packet: both year flags are genuine, whereas the short HOVVEIL120 page is faithfully captured and refers readers to the general study plan, so its short flag is not an extraction defect. Across the suspects and 25 controls, the only uncaptu... |
| hivolda | sonnet | 28 | Extraction works well for hivolda: all 28 random controls contain the whole plan (facts box, description, learning outcomes, organisation, requirements, assessment table) with clean line structure, correct course and year, and no chrome or junk. Uncaptured page text is only year pickers, the repe... |
| hvl | gpt-6.1-sol | 31 | Reviewed all 28 fetched plans and three failed-fetch entries: substantive plan sections and facts boxes are preserved, academic-year headings match the requested years, and DAT300's sole short flag reflects the source page referring readers to UiB rather than extraction loss. The three soft-404 U... |
| inn | gpt-6.1-sol | 28 | All 28 packet courses were reviewed: four contain course-search listings and 21 contain a later-year or later-semester plan, including five of eight random controls. The sampled long/dup_code flags identify real fallback listings and all 16 sampled year flags identify real mismatches; the year pr... |
| mf | gpt-6.1-sol | 34 | Reviewed all 34 entries: 28 fetched pages (19 suspects and nine controls) and six failed-fetch entries; substantive plans and facts are generally captured, with no demonstrated omission of on-page plan content, but recurring library-access boilerplate and concatenated headings/list items affect s... |
| nih | sonnet | 25 | Extraction works well for the 21 fetched pages: complete plan text with facts box, correct year, readable structure, and the uncaptured text is only breadcrumb, responsible person and FS timestamp. The weaknesses are coverage (166 of 190 offerings return 404, one URL is malformed) and reading lis... |
| nla | gpt-6.1-sol | 31 | Reviewed all 28 fetched offerings (20 suspects and eight random controls) plus the three failed-fetch examples: the ten nonempty plans preserve the correct academic-year JSON entry, metadata and all accordion sections with readable structure and no demonstrated extraction omissions, junk, truncat... |
| nmbu | gpt-6.1-sol | 31 | Reviewed all 31 packet records: 28 fetched pages (2 suspects and 26 controls) plus 3 failed-fetch examples. Both short suspects are Norwegian pointers to English descriptions rather than complete plans; the controls show no omitted non-chrome page text, with only minor formatting defects and a re... |
| nord | gpt-6.1-sol | 28 | All 28 fetched courses were read in full (7 suspects and 21 controls), and the additional failed-fetch entry was reviewed: visible substantive sections are captured with readable structure and no demonstrated navigation contamination. All seven short flags reflect source-page shells with no omitt... |
| ntnu | gpt-6.1-sol | 33 | Reviewed all 28 fetched courses (20 suspects and 8 random controls) and all 5 failed-fetch entries: core plan sections, facts and academic-year selection are generally faithful, with no demonstrated wrong-page or wrong-year extraction. The 18 uncaptured suspects mostly reflect intentionally omitt... |
| oslomet | gpt-6.1-sol | 28 | All 28 courses were reviewed: the 12 random controls preserve structured plan content, with no demonstrated omitted on-page prose or wrong academic-year selection. Of the 16 suspects, 15 are facts-only source pages whose uncaptured text contains only year-picker options, so their short flags do n... |
| steiner | sonnet | 8 | Extraction from the Steiner PDF is complete and readable: all seven course-plan parts are present in all eight controls. Defects are limited to PDF page-number lines left in the text, lost bullet glyphs/broken lines, and one offering (M-PEL2.1) whose text is a different course's plan. |
| uia | sonnet | 28 | Extraction with #right-main works well at UiA: all 27 random controls are complete, well-structured plans (learning outcomes, content, teaching, requirements, exam), and the only uncaptured page text is the breadcrumb. The one 'short' suspect is a stub page with only the facts box; remaining issu... |
| uib | sonnet | 28 | Where UiB has a plan page, extraction is complete and clean: the 19 random controls hold all plan sections, with the year picker, error banners and contact lines correctly left out. The main weakness is pages with no plan, where the text is only the facts box and bare headings. Exam logistics (da... |
| uio | gpt-6.1-sol | 31 | Reviewed all 31 packet records (20 suspects, eight controls and three failed fetches): substantive plans are generally complete and readable, but GEO9630 resolves to login pages, shared exam-help navigation remains, eight discontinuation/replacement notices are dropped, and four teaching-semester... |
| uis | sonnet | 28 | PDF-sourced plans (www6.uis.no/Stine) and the controls on current uis.no course pages are complete and mostly clean. All 20 suspects are empty: uis.no/nb/student/course pages with no extracted text and no stored page text, so nothing can be said about the page content. Minor noise: PDF page marke... |
| uit | sonnet | 28 | UiT extraction via #maincontents is generally faithful: whole plans for the right course and year, no site chrome, and the uncaptured text is only year pickers, apply-period banners and staff boxes. The four suspects are false alarms (thin source page, year-picker lines) apart from minor junk on ... |
| usn | sonnet | 28 | USN extraction is good: all 28 plans hold the full plan text for the right course code and year, the 'Page text not in extracted text' is empty everywhere, and no wrong-page, wrong-year or empty cases appear. The 3 suspects are false alarms (genuinely sparse or long plans). Remaining problems are... |

## All findings (ranked)

| ok | institution | target | error_type | severity | prevalence | example | suggested_fix |
| --- | --- | --- | --- | --- | --- | --- | --- |
| ✓ | uis | whole_text | empty_or_failed | high | widespread | uis_MLI355-1_2022_spring_1; uis_MGL4037-1_2023_autumn_1; ... | Inspect the raw html for a few of these courses. If the page is a stub, make html_pdf_discovery try the Stine PDF for each year first. If... |
| ✓ | nmbu | whole_text | wrong_page | high | occasional | nmbu_M60-IES-1_2025_autumn_1; nmbu_NOVA-416-1_2025_autumn_1 | Add an NMBU fallback that detects when the substantive section bodies consist of the exact pointer phrases "Se engelsk emnebeskrivelse" o... |
| ✓ | steiner | whole_text | wrong_page | high | rare | steiner_M-PEL2.1_2025_autumn_1 | Check the code-to-section matching in the pdf_split strategy (R/harvest_strategies.R / institution_config.R steiner entry); verify the ex... |
| ✓ | uio | whole_text | wrong_page | high | rare | uio_GEO9630-1_2025_autumn_1; uio_GEO9630-1_2025_spring_1 | Validate UiO responses before classifying them as fetched plans: detect Weblogin/login-form markers and require a course-plan heading mat... |
| ✓ | nih | whole_text | empty_or_failed | medium | widespread | nih_PHD610-1_2021_autumn_1; nih_TI201-1_2021_autumn_1; ni... | Spot check some 404 URLs in a browser (e.g. thp311 2021, ti201 2021); if pages exist under another path or the archive (emnearkivet), adj... |
| ✓ | uio | page_chrome | junk_included | medium | widespread | uio_PSY9170-1_2025_autumn_1; uio_HIS2011MV-1_2025_autumn_... | Exclude the shared exam-help module in the UiO DOM configuration, identified by the exact heading Mer om eksamen ved UiO / Meir om eksame... |
| ✓ | mf | whole_text | formatting | medium | common | mf_TEOL5430-1_2025_autumn_1; mf_TEOL8598-1_2025_autumn_1;... | Inspect the stored HTML for these three fields, then preserve its explicit heading, list and br boundaries as separate blocks/newlines in... |
| ✓ | ntnu | assessment | missing_content | medium | common | ntnu_MV2013-1_2019_autumn_1; ntnu_NEVR3002-3_2019_autumn_... | Replace the blanket #omEksamen ul.portlet-msg-info exclusion with filtering of individual logistics-only messages. Retain li.exam-comment... |
| ✓ | uib | assessment | missing_content | medium | common | uib_ENG120-0_2025_spring_1; uib_GEOV113-0_2022_autumn_1; ... | If exam form and duration are wanted, add the exam-info container as a second selector and drop dates, rooms and the 'Klokkeslett for opp... |
| ✓ | uio | metadata | missing_content | medium | common | uio_PSY9170-1_2025_autumn_1; uio_HELSEF4510-1_2025_autumn... | Refine the title-area exclusion to preserve elements containing notices beginning Dette emnet er nedlagt, This course is discontinued, or... |
| ✓ | uio | metadata | other | medium | common | uio_PSY9302B-1_2025_autumn_1; uio_ECON4150-1_2025_autumn_... | Validate UiO facts values for the exact token pattern BinaryValueReference{ref = digits}. Record the affected teaching-semester field as ... |
| ✓ | uit | reading_list | missing_content | medium | common | uit_HIF-2160-1_2025_spring_1; uit_LER-1210-2_2025_spring_... | Confirm in the raw html whether a Leganto list id or URL is present and, if so, fetch the list via site_request(); otherwise record in da... |
| ✓ | usn | reading_list | duplicate_content | medium | occasional | usn_MET1020-1_2025_spring_2; usn_MG1SA1-1_2025_spring_1 | Drop the campus list-name lines that precede 'Click to view', and deduplicate repeated reference blocks within the reading_list section (... |
| ✓ | uia | whole_text | other | medium | rare | uia_HEL905-1_2022_autumn_1 | Treat offerings whose extracted text lacks any plan heading (Læringsutbytte, Innhold, Eksamen) as 'no plan' (e.g. report in the gaps targ... |
| ✓ | usn | whole_text | wrong_page | medium | rare | usn_BABLU-6_2018_autumn_1 | Check how R/add_course_url.R builds the code for ids ending in -N with N other than 1, and verify that BABLU_3 and BABLU_6 are not both m... |
| ✓ | hiof | page_chrome | junk_included | low | widespread | hiof_HOVVEIL120-1_2022_autumn_1; hiof_LPKR116-1_2019_autu... | Add a hiof-specific post_fn that removes only complete lines matching ^[ \t]*Innholdsfortegnelse[ \t]*$ and ^[ \t]*Sist hentet fra FS \(F... |
| ✓ | hvl | page_chrome | junk_included | low | widespread | hvl_MAS538-1_2025_autumn_1; hvl_FHA912-1_2022_autumn_1; h... | Add an HVL exclude XPath in R/institution_config.R that removes only anchor elements whose normalize-space(.) equals 'Mer om hjelpemidler... |
| ✓ | inn | page_chrome | junk_included | low | widespread | inn_INN3024-1_2023_autumn_1; inn_4PM791-1_2025_spring_1; ... | Add INN exclusions for fieldset.semesters-top and the status-message block, retaining the course header and facts list including Undervis... |
| ✓ | mf | page_chrome | junk_included | low | widespread | mf_BAO2800-1_2025_autumn_1; mf_EX1020-1_2025_autumn_1; mf... | Add an MF-specific post_fn that removes only the exact Tilgang til litteratur heading and its four standard access paragraphs, ending at ... |
| ✓ | nih | reading_list | missing_content | low | widespread | nih_THP311-1_2025_autumn_1; nih_LKI105-1_2022_spring_1; n... | Treat stub-only reading_list sections as empty in section extraction; optionally check whether the pensum link target is harvestable. |
| ✓ | nmbu | page_chrome | junk_included | low | widespread | nmbu_AQX100-1_2025_autumn_1; nmbu_BOT200-1_2025_autumn_1;... | Add an NMBU exclude selector for the anchor whose normalized text is exactly "Her finner du KI-kategoriene beskrevet.", or remove only th... |
| ✓ | ntnu | page_chrome | junk_included | low | widespread | ntnu_BEV2101-1_2019_spring_1; ntnu_TMMT5003-A_2025_spring... | Add #content h2.portlet-title-text to the NTNU exclude configuration, leaving the actual course title, course code and facts box intact. |
| ✓ | oslomet | page_chrome | junk_included | low | widespread | oslomet_VESK6300-1_2025_spring_1; oslomet_DKLH6000-1_2019... | In an OsloMet-only post_fn, restrict cleanup to the header before the first substantive plan heading: remove lines exactly equal to Emneh... |
| ✓ | uia | page_chrome | junk_included | low | widespread | uia_LIT606-1_2022_autumn_1; uia_FYS008-G_2020_autumn_1; u... | Add exclude/post_fn for the 'Innholdsfortegnelse' heading and a regex dropping '^Sist hent(et\|a) fra FS .*$' (also nynorsk 'Sist henta fr... |
| ✓ | usn | page_chrome | junk_included | low | widespread | usn_SMERTE100-1_2022_autumn_1; usn_KH-EMNE2-1_2022_spring... | Drop the block from the 'Innholdsfortegnelse' / 'Table of contents' line up to the next heading that repeats its first entry, in the usn ... |
| ✓ | usn | reading_list | formatting | low | widespread | usn_HIS2010-1_2025_autumn_2; usn_REG3000D-1_2019_autumn_1... | Insert a newline after 'Leganto' and after the known type labels (Book, Book Chapter, Article, E-book, Journal, Website, Document, Note),... |
| ✓ | hivolda | reading_list | missing_content | low | common | hivolda_HI100N-1_2025_autumn_1; hivolda_MEI301-1_2025_aut... | If reading lists are wanted, follow the 'Pensumliste' link in the url_discovery strategy and append the linked page text; otherwise docum... |
| ✓ | steiner | page_chrome | junk_included | low | common | steiner_M-MAT1_2_2025_autumn_1; steiner_M-MAT1_1_2025_aut... | Add a post_fn for steiner that drops lines matching ^\s*\d{1,3}\s*$ (and rejoins the split sentence/list item where a page break interrup... |
| ✓ | steiner | learning_outcomes | formatting | low | common | steiner_M-NAT1_2_2025_autumn_1; steiner_M-SAM1_2_2025_aut... | In post-processing join continuation lines (line not starting with bullet/heading and previous line not ending in sentence punctuation), ... |
| ✓ | uia | metadata | duplicate_content | low | common | uia_MAS416-G_2025_autumn_1; uia_MA-311-1_2025_autumn_2; u... | Drop the later 'Undervisningsspråk' heading and its value line via post_fn (the facts-box copy is kept). |
| ✓ | uia | metadata | wrong_year | low | common | uia_IDR210-1_2022_autumn_1; uia_MA-168-1_2022_autumn_1; u... | Never derive semester from the 'Undervisningssemester' line; rely on the offering's own semester field. |
| ✓ | uib | metadata | formatting | low | common | uib_EXFAC00SK-0_2025_spring_1; uib_MATDID350-0_2025_autumn_1 | Drop trailing empty headings and '-' placeholder values in a post_fn, or render 'Label: value' for the facts box. |
| ✓ | uis | page_chrome | junk_included | low | common | uis_ENG112-1_2025_autumn_2; uis_PPU118-1_2025_autumn_3 | Add exclude patterns in R/institution_config.R for the footer line and the 'Opptak til enkeltemner'/'Utveksling til' blocks, or cut the '... |
| ✓ | uis | page_chrome | junk_included | low | common | uis_MLH150-1_2013_spring_1; uis_MTLMAS-1_2012_spring_1; u... | Remove lines matching ^\s*side \d+\s*$ and the repeated '<CODE> - <title>' header in the PDF post_fn. |
| ✓ | uit | reading_list | junk_included | low | common | uit_HIF-2160-1_2025_spring_1; uit_SPL-3004-1_2025_autumn_1 | Add the sentence 'Du kan se og få tilgang til deler av pensum via Leganto.' to the uit exclude/post_fn (boilerplate per course_plan_spec.... |
| ✓ | nmbu | whole_text | formatting | low | occasional | nmbu_VU-VET340-1_2025_autumn_1; nmbu_LAFT210-1_2025_autumn_1 | Inspect the stored NMBU rich-text nodes at these joins and preserve actual nested block and line-break boundaries as whitespace before te... |
| ✓ | nord | course_content | duplicate_content | low | occasional | nord_ORG2007-1_2025_autumn_1; nord_PRO9019-1_2025_autumn_1 | Add a Nord fulltext post_fn that compares the complete introduction after the course code with the initial paragraph under Beskrivelse av... |
| ✓ | ntnu | whole_text | formatting | low | occasional | ntnu_KLMED8004-1_2019_autumn_1; ntnu_NEVR3002-3_2019_autu... | Normalize the observed C1 punctuation in an NTNU post_fn: map U+0091/U+0092 to opening/closing single quotation marks, U+0093/U+0094 to o... |
| ✓ | uia | metadata | duplicate_content | low | occasional | uia_SYP212-1_2020_autumn_1; uia_SE-204-G_2020_autumn_4 | Deduplicate adjacent/identical table lines in post_fn; optionally drop the whole 'Reduksjon i studiepoeng' block as non-plan text. |
| ✓ | uis | course_content | duplicate_content | low | occasional | uis_MGL3126-1_2022_autumn_1; uis_MGL2060-1_2021_autumn_1;... | Optionally drop a paragraph in the PDF text that is identical to one already seen in the same text. Low priority. |
| ✓ | uit | teaching_methods | junk_included | low | occasional | uit_HIF-3022-1_2008_autumn_1; uit_STV-2010-1_2008_autumn_2 | In the uit post_fn drop lines starting with 'Lectures ' / 'Undervisning ' followed by a semester and the following 'Forelesning' table ro... |
| ✓ | uit | whole_text | formatting | low | occasional | uit_MUS-1510-1_2019_spring_1; uit_FRI-2580-1_2019_autumn_1 | Accept as a source limitation, or map '¿' at line start to a bullet and between digits to an en dash in the uit post_fn. |
| ✓ | usn | whole_text | other | low | occasional | usn_SMERTE100-1_2022_autumn_1; usn_MAR4020-1_2022_autumn_... | Add a post_fn replacement for '¿' used between spaces as an en-dash and as an apostrophe after a word (' ¿ ' to ' - ', 'x¿ ' to "x' "), o... |
| ✓ | nih | whole_text | empty_or_failed | low | rare | nih_MAS- ESSAY-1_2022_autumn_1 | Normalise the code in the NIH URL builder (remove spaces, check the real page slug, e.g. mas-essay) or skip codes that cannot form a vali... |
| ✓ | ntnu | learning_outcomes | formatting | low | rare | ntnu_SKOLE6249-1_2022_autumn_1 | Add a narrowly scoped NTNU text normalization for the observed glued heading sequences: insert paragraph breaks before Kunnskaper, Ferdig... |
| ✓ | uib | course_content | duplicate_content | low | rare | uib_FSKLI901-0_2025_autumn_1; uib_FSKLI901-0_2025_spring_1 | Optional: drop exact-duplicate paragraphs of 80 or more characters within one text in a UiB post_fn. Low priority. |
| ✓ | uit | course_content | duplicate_content | low | rare | uit_JUR-3902-1_2022_autumn_1 | Check the raw html for the nested structure; emit separators between nested inline elements in R/blocks.R or deduplicate repeated list it... |
| ✓ | usn | metadata | other | low | rare | usn_EE505-1_2025_autumn_1 | No extraction change; consider flagging plans with no sections beyond the TOC and facts box downstream so they are not counted as full pl... |
