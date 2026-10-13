# Data Quality Notes


Generated: 2026-10-13

## Overview

| Institution | Rows | Codes | Years | Extracted OK | Rate | Median chars | Unique plans | Dedup % |
|:---|---:|---:|:---|---:|:---|---:|---:|:---|
| hiof | 1712 | 240 | 2017-2025 | 846 | 49.4% | 7151 | 820 | 3.1% |
| hivolda | 1182 | 113 | 2017-2025 | 594 | 50.3% | 4622 | 547 | 7.9% |
| hvl | 3604 | 393 | 2017-2025 | 3335 | 92.5% | 4116 | 1647 | 49.3% |
| inn | 2861 | 286 | 2018-2025 | 1777 | 62.1% | 4164 | 553 | 16.8% |
| mf | 55 | 29 | 2025 | 26 | 47.3% | 4056 | 26 | 0.0% |
| nih | 184 | 34 | 2021-2025 | 81 | 44.0% | 2330 | 81 | 0.0% |
| nla | 2074 | 209 | 2017-2025 | 249 | 12.0% | 5885 | 181 | 27.3% |
| nmbu | 29 | 15 | 2025 | 12 | 41.4% | 5900 | 12 | 0.0% |
| nord | 3489 | 434 | 2016-2025 | 3489 | 100.0% | 4004 | 1365 | 60.2% |
| ntnu | 5735 | 409 | 2004-2025 | 3791 | 66.1% | 3985 | 2015 | 46.8% |
| oslomet | 1684 | 178 | 2018-2025 | 1684 | 100.0% | 12259 | 568 | 64.7% |
| samas | 130 | 68 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| steiner | 36 | 18 | 2025 | 15 | 41.7% | 5027 | 15 | 0.0% |
| uia | 2881 | 245 | 2013-2025 | 1123 | 39.0% | 4343 | 681 | 33.8% |
| uib | 2619 | 177 | 2004-2025 | 1473 | 56.2% | 3135 | 564 | 59.5% |
| uio | 100 | 54 | 2025 | 47 | 47.0% | 4467 | 47 | 0.0% |
| uis | 2921 | 316 | 2007-2025 | 2728 | 93.4% | 5622 | 1287 | 52.8% |
| uit | 6693 | 621 | 2004-2025 | 3154 | 47.1% | 4108 | 2300 | 27.0% |
| usn | 3036 | 401 | 2018-2025 | 1238 | 40.8% | 10800 | 1182 | 4.5% |

**Total**: 41025 rows, 25662 with extracted text (62.6%), 13994 unique
plans.

**Columns:**

- **Rows**: Total course-semester rows in harvested data (from DBH)
- **Codes**: Unique course codes (Emnekode_raw)
- **Extracted OK**: Rows with successfully extracted course plan text
- **Rate**: Extraction success rate (Extracted OK / Rows)
- **Median chars**: Median character count of extracted text
- **Unique plans**: Distinct course plans after normalization and
  deduplication
- **Dedup %**: Percentage of rows removed by deduplication (1 -
  unique/has_plan)

## Year coverage

Sites that show only the plan in force (`plan_years = "current"` in
`R/institution_config.R`) give, at each harvest, the plan of that
academic year only. Each harvest is kept, and an offering gets the plan
from a harvest made in its own academic year, or none (README “Sites
that show only the current plan”). In this data the harvests are of
2026-04-04 (2025/26: autumn 2025 offerings) and 2026-09-30 (2026/27: no
DBH offerings yet), so spring 2025 offerings (2024/25) have no plan at
these sites. They are mf, nmbu, samas, steiner, uio. The others have
historical plans as far back as the site still serves them. Share of
offerings with a plan, by period:

| Institution | –2014 | 2015–17 | 2018–19 | 2020–21 | 2022–23 | 2024– |
|:------------|:------|:--------|:--------|:--------|:--------|:------|
| hiof        |       | 100%    | 53%     | 49%     | 48%     | 49%   |
| hivolda     |       | 100%    | 54%     | 50%     | 50%     | 48%   |
| hvl         |       | 100%    | 100%    | 98%     | 92%     | 88%   |
| inn         |       |         | 0%      | 0%      | 100%    | 100%  |
| mf          |       |         |         |         |         | 47%   |
| nih         |       |         |         | 62%     | 46%     | 41%   |
| nla         |       | 30%     | 2%      | 3%      | 2%      | 30%   |
| nmbu        |       |         |         |         |         | 41%   |
| nord        |       | 100%    | 100%    | 100%    | 100%    | 100%  |
| ntnu        | 91%   | 93%     | 61%     | 47%     | 48%     | 51%   |
| oslomet     |       |         | 100%    | 100%    | 100%    | 100%  |
| samas       |       |         |         |         |         | 0%    |
| steiner     |       |         |         |         |         | 42%   |
| uia         | 0%    | 0%      | 0%      | 50%     | 47%     | 49%   |
| uib         | 31%   | 44%     | 63%     | 81%     | 79%     | 86%   |
| uio         |       |         |         |         |         | 47%   |
| uis         | 92%   | 96%     | 95%     | 92%     | 92%     | 94%   |
| uit         | 51%   | 43%     | 40%     | 46%     | 54%     | 52%   |
| usn         |       |         | 39%     | 45%     | 38%     | 41%   |

## Semester registration in DBH

DBH lists most courses under both spring and autumn of a year, also when
the course is taught, and its plan published, in one semester only. How
this shows up depends on the website:

- **Semester-specific pages** (HiOF, Hivolda, NIH, NLA, UiA, USN): the
  other semester has no page (a 404, or no USN version), so about half
  of the rows lack text by design.
- **One page per course or year**: both rows get the same page, and
  deduplication collapses them.

The row-level **Rate** in the overview therefore understates coverage
for the first group. To compare institutions, count course-years (code ×
year) that have a plan in at least one semester:

| Institution | Course-years | Both semesters in DBH | Both reg., plan in one | Both reg., plan in both | Both reg., no plan | Course-years with plan | Row rate |
|:---|---:|:---|---:|---:|---:|:---|:---|
| hiof | 936 | 83% | 715 | 8 | 53 | 90% | 49.4% |
| hivolda | 629 | 88% | 510 | 8 | 35 | 93% | 50.3% |
| hvl | 1931 | 87% | 0 | 1516 | 157 | 90% | 92.5% |
| inn | 1500 | 91% | 608 | 14 | 739 | 43% | 62.1% |
| mf | 29 | 90% | 25 | 0 | 1 | 90% | 47.3% |
| nih | 107 | 72% | 58 | 0 | 19 | 76% | 44.0% |
| nla | 1103 | 88% | 232 | 0 | 739 | 23% | 12.0% |
| nmbu | 15 | 93% | 11 | 0 | 3 | 80% | 41.4% |
| nord | 1886 | 85% | 2 | 1575 | 26 | 98% | 100.0% |
| ntnu | 2851 | 80% | 0 | 1427 | 867 | 63% | 66.1% |
| oslomet | 895 | 88% | 2 | 757 | 30 | 95% | 100.0% |
| samas | 68 | 91% | 0 | 0 | 62 | 0% | 0.0% |
| steiner | 18 | 100% | 15 | 0 | 3 | 83% | 41.7% |
| uia | 1508 | 91% | 968 | 3 | 402 | 68% | 39.0% |
| uib | 1328 | 85% | 14 | 653 | 458 | 54% | 56.2% |
| uio | 54 | 85% | 44 | 0 | 2 | 87% | 47.0% |
| uis | 1602 | 82% | 0 | 1235 | 84 | 93% | 93.4% |
| uit | 3605 | 86% | 1077 | 878 | 1133 | 63% | 47.1% |
| usn | 1657 | 83% | 1021 | 31 | 327 | 73% | 40.8% |

## Course-years without a plan

Why a course-year (code × year) has no plan in any semester, from
`data/interim/plan_gaps.RDS` (target `gaps`, `plan_gaps()` in
`R/pipeline.R`). **No URL**: the site has no page for that year, or URL
discovery found none (hivolda, uit, usn). **404**: the page was not
found. **Fetch error**: another error (ntnu’s “no information” pages,
uib). **Page, no plan**: a page shell, a page that is not the plan, or a
year missing from NLA’s JSON. **No page**: a current site without a
harvest in that year. **Between plans**: the course has a plan the year
before and the year after, so the gap is the likeliest to be a wrong URL
or a missed page; these are the ones to check by hand (`url_tried`, and
`url_next_door`, the page of an adjacent year that worked).

| Institution | Without plan | 404 | No URL | Fetch error | Page, no plan | No page | Between plans |
|:---|---:|---:|---:|---:|---:|---:|---:|
| hiof | 98 | 98 | 0 | 0 | 0 | 0 | 6 |
| hivolda | 40 | 0 | 40 | 0 | 0 | 0 | 10 |
| hvl | 198 | 152 | 0 | 0 | 46 | 0 | 5 |
| inn | 849 | 0 | 584 | 0 | 265 | 0 | 7 |
| mf | 3 | 0 | 0 | 0 | 0 | 3 | 0 |
| nih | 26 | 26 | 0 | 0 | 0 | 0 | 2 |
| nla | 854 | 146 | 0 | 0 | 708 | 0 | 0 |
| nmbu | 2 | 0 | 0 | 0 | 0 | 2 | 0 |
| nord | 30 | 0 | 0 | 0 | 30 | 0 | 0 |
| ntnu | 1043 | 0 | 0 | 1043 | 0 | 0 | 3 |
| oslomet | 45 | 0 | 0 | 0 | 45 | 0 | 0 |
| samas | 68 | 0 | 68 | 0 | 0 | 0 | 0 |
| steiner | 3 | 0 | 3 | 0 | 0 | 0 | 0 |
| uia | 483 | 483 | 0 | 0 | 95 | 0 | 92 |
| uib | 614 | 447 | 0 | 137 | 36 | 0 | 0 |
| uio | 7 | 0 | 0 | 0 | 0 | 7 | 0 |
| uis | 109 | 14 | 0 | 0 | 95 | 0 | 0 |
| uit | 1322 | 0 | 1319 | 0 | 8 | 0 | 52 |
| usn | 431 | 0 | 431 | 0 | 0 | 0 | 30 |

To list the gaps between plans:
`readRDS("data/interim/plan_gaps.RDS") |> dplyr::filter(between_plans)`.

Checked 2026-10-07 and 2026-10-08, all 208 gaps between plans: one was a
wrong URL, UiT’s ENG-2980 spring 2017 (an English twin, see UiT), so 207
remain. The 404s at HiOF (11 of 12 URLs; the twelfth is now an empty
page), HVL (5 of 5, soft 404) and NIH (4 of 4) are still 404. No URL
means the site has no page for that year: UiT’s semester picker has no
option for it (all 49; UiT’s other 3 are pages without a plan), USN
shows another year’s plan for every version number (all 30, both
semesters), and Hivolda’s course page lists no version for it (all 10,
mostly 2024). NTNU’s 3 (2010) still say “Det finnes ingen informasjon
for dette studieåret”; INN’s 7 show a later plan version. UiA’s 91 are
its autumn-2023 page shells (see UiA), and REL410 2024 is registered in
spring while UiA publishes it under autumn (the only such case among
UiA’s 74 course-years since 2020 that are 404).

## Interpreting the dedup ratio

High dedup (\>40%) is expected for current-year-only institutions where
DBH registers courses for both semesters but only one plan page exists
(see “Semester registration in DBH”).

Low dedup (\<10%) means most rows have genuinely different content,
typically because plans change between years/versions (HiOF, USN) or
each row is truly distinct (NIH, Steiner, UiO).

## Per-Institution Notes

Figures are in the tables above; the notes say why.

------------------------------------------------------------------------

### HiOF (Ostfold University College)

- Plans before autumn 2021 use another URL structure;
  `add_course_url_hiof()` switches by date, and the selector lists
  fallbacks for both page layouts
- Needs a browser-like user agent (`user_agent = "browser"`) to avoid
  403s
- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- Reading lists are part of the plan text; `.anon_hiof()` removes only
  the “Litteraturlista er sist oppdatert” line

------------------------------------------------------------------------

### Hivolda (Volda University College)

- Semester-specific pages have numeric ids
  (e.g. `/emne/MGL5-10NO2B/12177`) that metadata cannot give;
  `resolve_urls_hivolda_batch()` reads them from the course’s base page,
  matching “Haust” to “Høst”
- Rows without a plan are mostly semesters in which the course was not
  taught

------------------------------------------------------------------------

### HVL (Western Norway University of Applied Sciences)

- Year in the URL. Soft-404 “course not found” pages are detected at
  fetch time (`fetch_html_cols_single_hvl()`) and stored as no HTML

------------------------------------------------------------------------

### INN (Inland Norway University of Applied Sciences)

- No plans before 2022: INN’s site starts in 2022, so the URL builder
  returns NA for earlier years
- Some pages are the course search page (“Emnesøket gjelder kun fra …”)
  instead of a plan; the anonymizer turns them into NA
- Some courses are published in both Norwegian and English
- A page whose semester’s plan is not published yet says
  “Emnebeskrivelsen for valgt semester er ikke publisert enda. Her er
  den siste versjonen vi har tilgjengelig.” and shows a later version
  (“Undervisningssemestre” 2026 or later). It is not that offering’s
  plan, so the offering gets no plan: 1,077 offerings, 384 of the 407
  pages from 2022 and about half of 2023-2025. The April 2026 harvest
  shows the same notice on the same pages

------------------------------------------------------------------------

### MF (MF Norwegian School of Theology)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Selector `article .content-body`: the title and
  code, the facts box (credits, semester, language, programmes; without
  its buttons) and the plan; the title and facts box are in no section
- `.anon_mf()` cuts from “Emneansvarlig” to the end (names, contact
  details, marketing)
- The reading list is on a separate page; the plan holds only a link and
  a library notice, which `.anon_mf()` removes, so MF plans have no
  `reading_list` section

------------------------------------------------------------------------

### NIH (Norwegian School of Sport Sciences)

- Short year range. Most 404s are the other semester of a course
  registered in both (see “Semester registration in DBH”)

------------------------------------------------------------------------

### NLA (NLA University College)

- All DBH years (`plan_years = "page"`): each course page embeds the
  plans of several academic years as JSON, for most courses only recent
  ones; the json block reader (`R/blocks.R`) takes the year of the row
  (spring 2025 → 2024-2025, autumn 2025 → 2025-2026). A row with a page
  but no plan is an academic year missing from that JSON
- The `/studietilbud/emner/` URLs are rendered in the browser, so the
  harvest uses
  `/for-studenter/Studie-%20og%20emneplaner/emneplan/{CODE}`

------------------------------------------------------------------------

### NMBU (Norwegian University of Life Sciences)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”); few course codes. Rows without a plan are 404s for
  discontinued courses, or spring 2025 offerings

------------------------------------------------------------------------

### Nord (Nord University)

- Year and semester in the URL, with Norwegian characters
  (`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic page without
  year-specific content
- Accordion pages, read with a multi-element selector

------------------------------------------------------------------------

### NTNU (Norwegian University of Science and Technology)

- Year in the URL, no semester. Pages saying no information is available
  raise `ntnu_no_info_error`; robots.txt asks for a 10-second crawl
  delay
- The exam tab (FS’s exam registration: assessment form, grading scale,
  and per exam session the exam parts with weight, duration and
  permitted aids) is in `course_plan` without dates, times, rooms and
  comments; the assessment section and the plan id keep one session of
  it (`exam_sessions`). The timetable tab (from 2025) is not read.
  `.anon_ntnu()` removes the contact block (course coordinator,
  teachers); the responsible unit stays, and the dedup hash ignores it

------------------------------------------------------------------------

### OsloMet (Oslo Metropolitan University)

- The URL always uses `HØST`: spring URLs return 404 and the plan is the
  same. Pattern:
  `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`
- Re-harvested 2026-02-14 after the site moved to server-side rendering.
  Error pages (“Siden du leter etter finnes ikke”) become NA in the
  anonymizer
- **Practicum courses have no sections** (M1GP\*/M5GP\*, 135 offerings):
  every section of the course plan only says “Se fagplanen.” The page’s
  “Fagplan” block is the programme-wide practicum plan for all study
  years, with no markup separating the years, so it is not read into the
  course’s sections. Their `course_plan` keeps the full page text

------------------------------------------------------------------------

### Samas (Sámi University of Applied Sciences)

- `noop` strategy, no plans: the website’s course codes (SÁM-1005,
  DUO-1014, …) do not correspond to DBH codes (V1SAM-1100-1, …), and the
  V1/V5 teacher education courses appear only in programme PDFs without
  per-course descriptions

------------------------------------------------------------------------

### Steiner (Rudolf Steiner University College)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Plans come from five subject PDFs at
  `steinerhoyskolen.no`, split by course heading; the PDFs are not
  stored, so the split text is the raw data
- The practicum modules (M-LP1/2/3) have no PDF content
- M-PEL2.1/M-PEL2.2 get the PDF’s PEL1.4/PEL1.5 plans
  (`.steiner_remap_pel_emne()`); to be confirmed against the programme
  description

------------------------------------------------------------------------

### UiA (University of Agder)

- The site has plans from 2020 on; earlier years return 404
- DBH registers most courses in both semesters, but UiA publishes one
  page, so most 404s are the other semester (see “Semester registration
  in DBH”)
- Autumn 2023: 95 pages are shells (title and facts box, no plan), so 91
  course-years have no plan between two that do. They are courses UiA
  now teaches in spring (94 pages say “Undervisningssemester: Vår”).
  Every copy UiA has rebuilt from FS since (2026-09-30, 10-06 and 10-08,
  by the page’s “Sist hentet fra FS”) is a shell, the English pages too;
  the full pages seen at 13:40 on 2026-10-07 were old copies, last
  fetched from FS in July 2024. The site no longer has these plans, so
  the offerings stay without one
- URL pattern:
  `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`

------------------------------------------------------------------------

### UiB (University of Bergen)

- Year in the URL; accordion pages read with a multi-element selector;
  robots.txt asks for a 10-second crawl delay
- The page’s semester picker is dropped when the HTML is parsed
- About 280 pages say “Vi opplever problemer med å hente inn
  eksamensinformasjon”: UiB’s exam information service fails for these
  courses, in every year. Fetching the 294 affected pages again on
  2026-10-02 fixed 7, so it is not a harvest error. The banner is
  removed from the sections, and these courses have no assessment text

------------------------------------------------------------------------

### UiO (University of Oslo)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Semester URLs (`/h24/`, `/v25/`) hold logistics
  (teachers, timetable, exam dates), not the plan
- Needs faculty/department slugs derived from `Avdelingsnavn` (mapping
  in `add_course_url_uio()`). Pattern:
  `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`
- `.anon_uio()` cuts the generic “Mer om eksamen ved UiO” tail

------------------------------------------------------------------------

### UiS (University of Stavanger)

- Current plans come from the course web page, older ones from the PDF
  archive linked in its semester dropdown (`html_pdf_discovery`); the
  PDFs are not stored, so their text is the raw data
- Web pages are read from the blocks of `div.article`, without
  navigation and contact footer; the facts box (“Fakta”: code, credits,
  semesters, language) is in `course_plan` without its timetable and
  Leganto links, and in no section

------------------------------------------------------------------------

### UiT (The Arctic University of Norway)

- **Historical plans** via document ids from the course page’s semester
  picker
- The plan is read from `.mainContent` (since 2026-10; the old
  `.hovedfelt > main > div.col-md-12` matched nothing after a site
  change, so the 2026-10 harvest first had no text)
- `.pre_uit()` removes the “OBS! Dette emnet tilhører et tidligere
  semester / år” banner and cuts from “Andre år og semester” / “Kontakt
  oss” (staff names and e-mails) to the end
- Some pages say “Error rendering component” where the plan should be:
  the plan failed to render on UiT’s site, and these rows have no text
- The semester picker can list a semester twice: an English document
  (“Course code:”, in all 5 cases an empty page) beside the Norwegian
  one. The Norwegian one is used; the first option was used before
  2026-10-08, and a retry harvest
  (`harvest_all(institutions = "uit", retry = TRUE)`) gave these 5
  offerings their plan

------------------------------------------------------------------------

### USN (University of South-Eastern Norway)

- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- The URL holds a plan version (1-3 are tried; all URLs found are 1
  or 2) that metadata cannot give:
  `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`.
  Invalid version/year combinations silently show another page, so
  `resolve_urls_usn_batch()` renders each candidate in Chrome and checks
  the displayed year (README “Institution-Specific Notes”)
- Content is rendered in Shadow DOM and read with `read_usn_live_html()`
