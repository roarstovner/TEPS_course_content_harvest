# Data Quality Notes


Generated: 2026-03-19

## Overview

| Institution | Rows | Codes | Years | Fulltext OK | Rate | Median chars | Unique plans | Dedup % |
|:---|---:|---:|:---|---:|:---|---:|---:|:---|
| hiof | 1712 | 240 | 2017-2025 | 846 | 49.4% | 7190 | 821 | 3.0% |
| hivolda | 1182 | 113 | 2017-2025 | 594 | 50.3% | 4684 | 555 | 6.6% |
| hvl | 670 | 346 | 2025 | 591 | 88.2% | 4484 | 296 | 49.9% |
| inn | 2368 | 262 | 2018-2024 | 1282 | 54.1% | 4184 | 542 | 56.7% |
| mf | 55 | 29 | 2025 | 51 | 92.7% | 4458 | 26 | 49.0% |
| nih | 184 | 34 | 2021-2025 | 81 | 44.0% | 2341 | 81 | 0.0% |
| nla | 365 | 189 | 2025 | 365 | 100.0% | 5311 | 189 | 48.2% |
| nmbu | 29 | 15 | 2025 | 19 | 65.5% | 5787 | 10 | 47.4% |
| nord | 540 | 294 | 2025 | 540 | 100.0% | 5592 | 294 | 45.6% |
| oslomet | 1705 | 179 | 2018-2025 | 1705 | 100.0% | 12226 | 593 | 64.0% |
| samas | 130 | 68 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| steiner | 18 | 18 | 2025 | 15 | 83.3% | 5027 | 15 | 0.0% |
| uia | 2374 | 237 | 2020-2025 | 1130 | 47.6% | 4449 | 689 | 39.0% |
| uib | 187 | 99 | 2025 | 177 | 94.7% | 8110 | 94 | 46.9% |
| uio | 100 | 54 | 2025 | 52 | 52.0% | 3878 | 52 | 0.0% |
| uis | 353 | 197 | 2025 | 204 | 57.8% | 4109 | 109 | 46.6% |
| uit | 6693 | 621 | 2004-2025 | 3161 | 47.2% | 4106 | 2349 | 25.7% |
| usn | 3036 | 401 | 2018-2025 | 1189 | 39.2% | 10600 | 1145 | 3.7% |

**Total**: 21701 rows, 12002 with fulltext (55.3%), 7948 unique plans.

**Columns:**

- **Rows**: Total course-semester rows in harvested data (from DBH)
- **Codes**: Unique course codes (Emnekode_raw)
- **Fulltext OK**: Rows with successfully extracted course plan text
- **Rate**: Fulltext success rate (Fulltext OK / Rows)
- **Median chars**: Median character count of extracted text
- **Unique plans**: Distinct course plans after normalization and
  deduplication
- **Dedup %**: Percentage of rows removed by deduplication (1 -
  unique/has_plan)

## Year coverage

Institutions fall into two groups based on URL structure:

**Year-specific URLs** (historical data available): HiOF, INN, NIH,
NTNU, OsloMet, UiA, USN. These institutions include the year (and
sometimes semester) in their course page URLs, so we can harvest
distinct content for each year.

**No year in URL** (current year only): HVL, Hivolda, MF, NLA, NMBU,
Nord, Samas, Steiner, UiB, UiO, UiS, UiT. These institutions serve the
same (latest) page regardless of year. Data is filtered to 2025 only to
avoid duplicating current content across historical years. Hivolda is an
exception: it has semester-specific pages discoverable via URL
resolution, so it retains historical data.

## Interpreting the dedup ratio

High dedup (\>40%) is expected for current-year-only institutions where
DBH registers courses for both semesters but only one plan page exists.

Low dedup (\<10%) means most rows have genuinely different content,
typically because plans change between years/versions (HiOF, USN) or
each row is truly distinct (NIH, Steiner, UiO).

## Per-Institution Notes

------------------------------------------------------------------------

### HiOF (Ostfold University College)

- **49.4% success rate**: Many older URLs (pre-2021) return 404
- Historical courses (before autumn 2021) use a different URL structure;
  the URL builder has fallback logic
- Two CSS selectors used: old structure (`#vrtx-fs-emne-content`) and
  new (`.entry-content`)
- HiOF required a browser-like user agent to avoid 403 blocks (see \#27)
- `normalize_plan_text` strips “Sist hentet fra FS” timestamps and
  “Litteraturlista er sist oppdatert” lines

------------------------------------------------------------------------

### Hivolda (Volda University College)

- **Re-harvested 2026-02-16** with URL discovery (see \#63, \#79-#83)
- The base URL `/emne/{CODE}` returns only the latest version.
  Semester-specific pages have numeric IDs (e.g.,
  `/emne/MGL5-10NO2B/12177`) that cannot be derived from metadata
- `resolve_urls_hivolda_batch()` scrapes the base page to discover
  semester links, mapping “Haust” to “Høst” for matching
- Result: 594/1182 rows with fulltext (up from 1182 identical pages).
  556 unique plans (up from 99)
- 588 NA rows = course not offered in that semester (legitimate missing
  data)

------------------------------------------------------------------------

### HVL (Western Norway University of Applied Sciences)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **88.2% success rate**: 79 rows have no content
- 716 rows in the original harvest were soft-404 “course not found”
  pages; these are now detected during fetch and marked as `html=NA`
  (see \#66)
- Uses `.extract_one()` with selector `.l-2-col__main-content`

------------------------------------------------------------------------

### INN (Inland Norway University of Applied Sciences)

- **54.1% success rate**: 1086 rows have no HTML (URLs return 404 or
  errors)
- URL builder filters out pre-2022 courses (return NA), as INN’s website
  only covers 2022+ (see \#26)
- 30 pages are placeholder/error pages (“Emnesøket gjelder kun fra…”)
  filtered as NA by `normalize_plan_text` (see \#53)
- Year range 2018-2024 (no 2025 data in DBH extract)
- INN publishes some courses in both Norwegian and English, causing
  lower dedup than expected. `normalize_plan_text` strips 35 NO/EN field
  labels and normalizes language metadata (see \#52)

------------------------------------------------------------------------

### MF (MF Norwegian School of Theology)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **92.7% success rate**: 4 pages return 404 (discontinued courses)
- CSS selector was fixed from `#main` to `main` (see \#25)
- `normalize_plan_text` strips from “Emneansvarlig” to end (removes
  teacher names, contact info, marketing content)

------------------------------------------------------------------------

### NIH (Norwegian School of Sport Sciences)

- **44% success rate**: 103 of 184 pages return 404
- Small institution (34 codes), short year range (2021-2025)
- No deduplication (0%) — each fetched plan is distinct
- Shortest median content (2341 chars)

------------------------------------------------------------------------

### NLA (NLA University College)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **100% fulltext rate** (365/365)
- Original harvest failed (0% extraction) because NLA’s
  `/studietilbud/emner/` URLs are React-rendered. Fix: switched to
  `/for-studenter/Studie-%20og%20emneplaner/emneplan/{CODE}` with
  selector `.page-course-plan` (see \#55)
- `.pre_nla()` in `extract_fulltext.R` strips year dropdown, “Last ned
  PDF” link, and truncates at “Digital litteraturliste” to remove
  reading lists (reduced median from 34.9K to 5.3K chars; see \#67)

------------------------------------------------------------------------

### NMBU (Norwegian University of Life Sciences)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- Only 15 unique course codes in current year
- **65.5% success rate**: 10 pages return 404 (discontinued courses)

------------------------------------------------------------------------

### Nord (Nord University)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **100% success rate** (540/540)
- Uses `.extract_many()` for accordion-structured content

------------------------------------------------------------------------

### NTNU (Norwegian University of Science and Technology)

- **66.1% success rate**: 1944 rows with no HTML (older courses no
  longer online)
- URL includes year but not semester; special error detection for “no
  information available” pages (`ntnu_no_info_error`)
- `normalize_plan_text` strips from “Kontaktinformasjon” to end (teacher
  names, exam dates, JS artifacts, timetable)
- Wide year range (2004-2025) with genuine content evolution across
  years

------------------------------------------------------------------------

### OsloMet (Oslo Metropolitan University)

- **Re-harvested 2026-02-14** (see \#57, \#61)
- Original harvest returned only error pages because OsloMet was using
  client-side rendering (React/Liferay). The site later switched to
  server-side rendering, fixing the issue
- **100% fulltext rate** (1705/1705), but 60 rows contain error pages
  (“Siden du leter etter finnes ikke”) which are filtered as NA by
  `normalize_plan_text`, leaving 1645 rows with genuine content
- **URL semester issue**: OsloMet’s website only accepts “HØST” (autumn)
  in URLs. Spring semester URLs return 404. Workaround: always use
  “HØST” regardless of actual semester (see \#1)
- URL pattern:
  `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`

------------------------------------------------------------------------

### Samas (Sámi University of Applied Sciences)

- **New institution, added 2026-02-16** (see \#69-#72)
- **Filtered to 2025 only**
- **0% fulltext rate**: No reliable matching between DBH course codes
  and website course codes (see \#85, \#89, \#90)
- Samas publishes course plans as PDFs (“Oahppoplána”) at
  `samas.no/se/oahput`, but the website uses its own codes (SÁM-1005,
  DUO-1014, SER 103) that do NOT correspond to DBH codes (V1SAM-1100-1,
  V1DUO-1140-1, etc.)
- ~98% of DBH rows are V1/V5 teacher education courses. These only
  appear in program-level PDFs (“PROGRÁMMAPLÁNA”) that list course names
  and credits but contain no per-course descriptions
- Individual course plan PDFs exist on the website for standalone
  courses, but none match any DBH course codes in the data

------------------------------------------------------------------------

### Steiner (Rudolf Steiner University College)

- **New institution, added 2026-02-16** (see \#68, \#73)
- **Filtered to 2025 only** (18 rows)
- Course plans harvested from 5 subject-area PDFs at
  `steinerhoyskolen.no`, split by course heading pattern
- 15/18 rows with fulltext (83.3%). 3 missing courses are Praksis
  modules (M-LP1/2/3) with no PDF content
- No deduplication (0%) — each of the 15 plans is distinct

------------------------------------------------------------------------

### UiA (University of Agder)

- **47.6% success rate**: Website only has courses from **2020
  onwards**; 2013-2019 courses return 404
- **Whole-year courses**: DBH registers courses for both semesters, but
  UiA typically publishes only one page. Of 1150 courses in both
  semesters: 76.3% have autumn page only, 16.8% spring only, 0.3% both
- **URL vs content semester mismatch**: 37% of autumn URLs contain
  spring-semester content
- URL pattern:
  `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`

------------------------------------------------------------------------

### UiB (University of Bergen)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **94.7% success rate**: 10 rows with no content
- Uses `.extract_many()` for accordion-structured content

------------------------------------------------------------------------

### UiO (University of Oslo)

- **Filtered to 2025 only** (see \#74, \#76)
- **52% success rate**: UiO only publishes the **latest version** of
  each course plan
- Semester-specific URLs (`/h24/`, `/v25/`) contain **logistics only**
  (instructors, schedule, exam dates) — NOT course plan content. The
  base URL always returns the current plan (see \#76)
- Requires faculty/department slugs derived from `Avdelingsnavn`
  (hardcoded mapping in `add_course_url_uio()`)
- URL pattern:
  `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`

------------------------------------------------------------------------

### UiS (University of Stavanger)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **57.8% success rate**: 149 rows with no content
- Website completely redesigned (migrated to Drupal); old URL pattern
  `/nb/course/{CODE}` returned 100% 404s. New URL pattern:
  `/nb/student/course/{CODE}_1` where CODE uses `Emnekode_raw` with dash
  replaced by underscore (see \#19-#24)
- Uses `.extract_many()` for content sections

------------------------------------------------------------------------

### UiT (The Arctic University of Norway)

- **Filtered to 2025 only** (no year in URL; see \#74, \#77)
- **97.5% success rate** (545/559)
- All UiT pages contain “Access denied to page component” HTML comments
  and “Error rendering component” artifacts — this is a persistent issue
  on UiT’s website, not a scraping problem (see \#64)
- CSS selector changed from `.hovedfelt` (single) to
  `.hovedfelt > main > div.col-md-12` (many) to work around the broken
  component (see \#60)
- `.pre_uit()` strips remaining “Error rendering component” text and
  breadcrumb artifacts
- 14 rows have fulltext that normalizes to NA (rendering errors with no
  real content)

------------------------------------------------------------------------

### USN (University of South-Eastern Norway)

- **39.2% success rate (expected)**: USN requires URL version discovery
  via JavaScript-rendered pages
- Course plan URLs include a version number (1-5) that cannot be
  determined from metadata alone
- URL pattern:
  `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`
- **Silent redirects**: Invalid version+year combos redirect to other
  pages without changing the URL. Validation checks that displayed year
  matches requested year
- Uses Shadow DOM extraction via `rvest::read_html_live()` with session
  reuse for performance
- Breakdown: ~39% autumn-start only, ~11% spring-start only, ~25% both
  semesters, ~25% no page at all
- “New” courses (status=2) have ~5% success; “Active” (status=1) courses
  ~81%
- Low dedup (3.4%): Plans change frequently between versions/years
