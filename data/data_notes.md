# Data Quality Notes


Generated: 2026-10-02

## Overview

| Institution | Rows | Codes | Years | Extracted OK | Rate | Median chars | Unique plans | Dedup % |
|:---|---:|---:|:---|---:|:---|---:|---:|:---|
| hiof | 1712 | 240 | 2017-2025 | 846 | 49.4% | 7154 | 820 | 3.1% |
| hivolda | 1182 | 113 | 2017-2025 | 594 | 50.3% | 4702 | 548 | 7.7% |
| hvl | 3604 | 393 | 2017-2025 | 3335 | 92.5% | 4117 | 1654 | 50.4% |
| inn | 2368 | 262 | 2018-2024 | 1284 | 54.2% | 4169 | 542 | 56.8% |
| mf | 55 | 29 | 2025 | 51 | 92.7% | 4350 | 26 | 49.0% |
| nih | 184 | 34 | 2021-2025 | 81 | 44.0% | 2332 | 81 | 0.0% |
| nla | 365 | 189 | 2025 | 142 | 38.9% | 5502 | 139 | 2.1% |
| nmbu | 29 | 15 | 2025 | 21 | 72.4% | 4653 | 11 | 47.6% |
| nord | 3489 | 434 | 2016-2025 | 3479 | 99.7% | 4980 | 1398 | 59.8% |
| ntnu | 5735 | 409 | 2004-2025 | 3791 | 66.1% | 4066 | 2009 | 47.0% |
| oslomet | 1684 | 178 | 2018-2025 | 1684 | 100.0% | 12259 | 655 | 60.9% |
| samas | 130 | 68 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| steiner | 36 | 18 | 2025 | 30 | 83.3% | 5027 | 15 | 50.0% |
| uia | 2881 | 245 | 2013-2025 | 1123 | 39.0% | 4344 | 811 | 27.8% |
| uib | 2619 | 177 | 2004-2025 | 1473 | 56.2% | 6826 | 772 | 47.6% |
| uio | 100 | 54 | 2025 | 96 | 96.0% | 4342 | 52 | 45.8% |
| uis | 2921 | 316 | 2007-2025 | 2728 | 93.4% | 5600 | 1299 | 52.4% |
| uit | 6693 | 621 | 2004-2025 | 3150 | 47.1% | 4105 | 2341 | 25.7% |
| usn | 3036 | 401 | 2018-2025 | 1238 | 40.8% | 10800 | 1183 | 4.4% |

**Total**: 38823 rows, 25146 with extracted text (64.8%), 14499 unique
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

Where the URL carries no year (`year_in_url = FALSE` in
`R/institution_config.R`), every year gets the latest plan, so only the
latest DBH year is harvested (`apply_year_filter()`). In this data that
applies to mf, nla, nmbu, samas, steiner, uio. The others have
historical plans as far back as the site still serves them. Share of
offerings with a plan, by period:

| Institution | –2014 | 2015–17 | 2018–19 | 2020–21 | 2022–23 | 2024– |
|:------------|:------|:--------|:--------|:--------|:--------|:------|
| hiof        |       | 100%    | 53%     | 49%     | 48%     | 49%   |
| hivolda     |       | 100%    | 54%     | 50%     | 50%     | 48%   |
| hvl         |       | 100%    | 100%    | 98%     | 92%     | 88%   |
| inn         |       |         | 0%      | 0%      | 100%    | 100%  |
| mf          |       |         |         |         |         | 93%   |
| nih         |       |         |         | 62%     | 46%     | 41%   |
| nla         |       |         |         |         |         | 39%   |
| nmbu        |       |         |         |         |         | 72%   |
| nord        |       | 99%     | 99%     | 100%    | 100%    | 100%  |
| ntnu        | 91%   | 93%     | 61%     | 47%     | 48%     | 51%   |
| oslomet     |       |         | 100%    | 100%    | 100%    | 100%  |
| samas       |       |         |         |         |         | 0%    |
| steiner     |       |         |         |         |         | 83%   |
| uia         | 0%    | 0%      | 0%      | 50%     | 47%     | 49%   |
| uib         | 31%   | 44%     | 63%     | 81%     | 79%     | 86%   |
| uio         |       |         |         |         |         | 96%   |
| uis         | 92%   | 96%     | 95%     | 92%     | 92%     | 94%   |
| uit         | 51%   | 43%     | 39%     | 46%     | 54%     | 52%   |
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
| hvl | 1931 | 87% | 0 | 1556 | 117 | 92% | 92.5% |
| inn | 1244 | 90% | 0 | 611 | 513 | 52% | 54.2% |
| mf | 29 | 90% | 0 | 25 | 1 | 90% | 92.7% |
| nih | 107 | 72% | 58 | 0 | 19 | 76% | 44.0% |
| nla | 189 | 93% | 133 | 0 | 43 | 75% | 38.9% |
| nmbu | 15 | 93% | 0 | 10 | 4 | 73% | 72.4% |
| nord | 1886 | 85% | 0 | 1599 | 4 | 100% | 99.7% |
| ntnu | 2851 | 80% | 0 | 1427 | 867 | 63% | 66.1% |
| oslomet | 895 | 88% | 0 | 787 | 2 | 99% | 100.0% |
| samas | 68 | 91% | 0 | 0 | 62 | 0% | 0.0% |
| steiner | 18 | 100% | 0 | 15 | 3 | 83% | 83.3% |
| uia | 1508 | 91% | 1063 | 3 | 307 | 74% | 39.0% |
| uib | 1328 | 85% | 3 | 681 | 441 | 56% | 56.2% |
| uio | 54 | 85% | 0 | 44 | 2 | 96% | 96.0% |
| uis | 1602 | 82% | 0 | 1235 | 84 | 93% | 93.4% |
| uit | 3605 | 86% | 1078 | 877 | 1133 | 63% | 47.1% |
| usn | 1657 | 83% | 1021 | 31 | 327 | 73% | 40.8% |

## Interpreting the dedup ratio

High dedup (\>40%) is expected for current-year-only institutions where
DBH registers courses for both semesters but only one plan page exists
(see “Semester registration in DBH”).

Low dedup (\<10%) means most rows have genuinely different content,
typically because plans change between years/versions (HiOF, USN) or
each row is truly distinct (NIH, Steiner, UiO).

## Per-Institution Notes

Figures are in the tables above; the notes say why. Issue numbers refer
to chainlink.

------------------------------------------------------------------------

### HiOF (Ostfold University College)

- Plans before autumn 2021 use another URL structure;
  `add_course_url_hiof()` switches by date, and the selector lists
  fallbacks for both page layouts
- Needs a browser-like user agent (`user_agent = "browser"`) to avoid
  403s (#27)
- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- Reading lists are part of the plan text; `.anon_hiof()` removes only
  the “Litteraturlista er sist oppdatert” line

------------------------------------------------------------------------

### Hivolda (Volda University College)

- Semester-specific pages have numeric ids
  (e.g. `/emne/MGL5-10NO2B/12177`) that metadata cannot give;
  `resolve_urls_hivolda_batch()` reads them from the course’s base page,
  matching “Haust” to “Høst” (#63, \#79-#83)
- Rows without a plan are mostly semesters in which the course was not
  taught

------------------------------------------------------------------------

### HVL (Western Norway University of Applied Sciences)

- Year in the URL. Soft-404 “course not found” pages are detected at
  fetch time (`fetch_html_cols_single_hvl()`) and stored as no HTML
  (#66)

------------------------------------------------------------------------

### INN (Inland Norway University of Applied Sciences)

- No plans before 2022: INN’s site starts in 2022, so the URL builder
  returns NA for earlier years (#26)
- Some pages are the course search page (“Emnesøket gjelder kun fra …”)
  instead of a plan; the anonymizer turns them into NA (#53)
- Some courses are published in both Norwegian and English (#52)
- Some 2022 pages show “Undervisningssemestre” for another year, and a
  few carry no plan; this is upstream, with no client-side fix (#123)

------------------------------------------------------------------------

### MF (MF Norwegian School of Theology)

- Latest year only (no year in the URL). Selector `main` (#25)
- `.anon_mf()` cuts from “Emneansvarlig” to the end (names, contact
  details, marketing)

------------------------------------------------------------------------

### NIH (Norwegian School of Sport Sciences)

- Short year range. Most 404s are the other semester of a course
  registered in both (see “Semester registration in DBH”; \#220)

------------------------------------------------------------------------

### NLA (NLA University College)

- Latest year only. Each course page embeds every academic year as JSON;
  `extract_nla_json()` takes the year of the row (spring 2025 →
  2024-2025, autumn 2025 → 2025-2026). A row with a page but no plan is
  an academic year missing from that JSON
- The `/studietilbud/emner/` URLs are rendered in the browser, so the
  harvest uses
  `/for-studenter/Studie-%20og%20emneplaner/emneplan/{CODE}` (#55)

------------------------------------------------------------------------

### NMBU (Norwegian University of Life Sciences)

- Latest year only (no year in the URL); few course codes. Rows without
  a plan are 404s for discontinued courses

------------------------------------------------------------------------

### Nord (Nord University)

- Year and semester in the URL, with Norwegian characters
  (`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic page without
  year-specific content (#126, \#129)
- Accordion pages, read with a multi-element selector

------------------------------------------------------------------------

### NTNU (Norwegian University of Science and Technology)

- Year in the URL, no semester. Pages saying no information is available
  raise `ntnu_no_info_error`; robots.txt asks for a 10-second crawl
  delay
- `.anon_ntnu()` cuts from “Kontaktinformasjon” to the end (teachers,
  exam dates, timetable)

------------------------------------------------------------------------

### OsloMet (Oslo Metropolitan University)

- The URL always uses `HØST`: spring URLs return 404 and the plan is the
  same (#1). Pattern:
  `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`
- Re-harvested 2026-02-14 after the site moved to server-side rendering
  (#57, \#61). Error pages (“Siden du leter etter finnes ikke”) become
  NA in the anonymizer
- **Practicum courses have no sections** (M1GP\*/M5GP\*, 135 offerings):
  every section of the course plan only says “Se fagplanen.” The page’s
  “Fagplan” block is the programme-wide practicum plan for all study
  years, with no markup separating the years, so it is not read into the
  course’s sections. Their `course_plan` keeps the full page text (see
  \#242)

------------------------------------------------------------------------

### Samas (Sámi University of Applied Sciences)

- `noop` strategy, no plans: the website’s course codes (SÁM-1005,
  DUO-1014, …) do not correspond to DBH codes (V1SAM-1100-1, …), and the
  V1/V5 teacher education courses appear only in programme PDFs without
  per-course descriptions (#69-#72, \#85, \#89, \#90)

------------------------------------------------------------------------

### Steiner (Rudolf Steiner University College)

- Latest year only. Plans come from five subject PDFs at
  `steinerhoyskolen.no`, split by course heading (#68, \#73); the PDFs
  are not stored, so the split text is the raw data
- The practicum modules (M-LP1/2/3) have no PDF content
- M-PEL2.1/M-PEL2.2 get the PDF’s PEL1.4/PEL1.5 plans
  (`.steiner_remap_pel_emne()`); to be confirmed against the programme
  description (#225)

------------------------------------------------------------------------

### UiA (University of Agder)

- The site has plans from 2020 on; earlier years return 404
- DBH registers most courses in both semesters, but UiA publishes one
  page, so most 404s are the other semester (see “Semester registration
  in DBH”; \#220)
- URL pattern:
  `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`

------------------------------------------------------------------------

### UiB (University of Bergen)

- Year in the URL; accordion pages read with a multi-element selector;
  robots.txt asks for a 10-second crawl delay
- The page’s semester picker is dropped when the HTML is parsed (#219)

------------------------------------------------------------------------

### UiO (University of Oslo)

- Latest year only: UiO publishes only the current version of each plan.
  Semester URLs (`/h24/`, `/v25/`) hold logistics (teachers, timetable,
  exam dates), not the plan (#76)
- Needs faculty/department slugs derived from `Avdelingsnavn` (mapping
  in `add_course_url_uio()`). Pattern:
  `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`
- `.anon_uio()` cuts the generic “Mer om eksamen ved UiO” tail (#224)

------------------------------------------------------------------------

### UiS (University of Stavanger)

- Current plans come from the course web page, older ones from the PDF
  archive linked in its semester dropdown (`html_pdf_discovery`, \#127);
  the PDFs are not stored, so their text is the raw data
- Web pages are read from the blocks of `.article__section`, without
  navigation, facts box and contact footer (#251)

------------------------------------------------------------------------

### UiT (The Arctic University of Norway)

- **Historical plans** via document ids from the course page’s semester
  picker (see \#113)
- The plan is read from `.mainContent` (since \#218; the old
  `.hovedfelt > main > div.col-md-12` matched nothing after a site
  change, so the 2026-10 harvest first had no text)
- `.pre_uit()` removes the “OBS! Dette emnet tilhører et tidligere
  semester / år” banner and cuts from “Andre år og semester” / “Kontakt
  oss” (staff names and e-mails) to the end
- Some pages say “Error rendering component” where the plan should be:
  the plan failed to render on UiT’s site (see \#64), and these rows
  have no text

------------------------------------------------------------------------

### USN (University of South-Eastern Norway)

- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”; \#221)
- The URL holds a plan version (1-5) that metadata cannot give:
  `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`.
  Invalid version/year combinations silently show another page, so
  `resolve_urls_usn_batch()` renders each candidate in Chrome and checks
  the displayed year (README “Institution-Specific Notes”)
- Content is rendered in Shadow DOM and read with `read_usn_live_html()`
