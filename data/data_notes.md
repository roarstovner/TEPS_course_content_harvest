# Data Quality Notes

Generated: 2026-02-14

## Overview

| Institution | Rows | Codes | Years | Fulltext OK | Rate | Median chars | Unique plans | Dedup % |
|------------|-----:|------:|-------|------------:|-----:|-------------:|-------------:|--------:|
| hiof | 1712 | 240 | 2017-2025 | 846 | 49.4% | 7190 | 826 | 2.4% |
| hivolda | 1182 | 113 | 2017-2025 | 1182 | 100% | 4384 | 99 | 91.6% |
| hvl | 3599 | 392 | 2017-2025 | 2883 | 80.1% | 4484 | 296 | 89.7% |
| inn | 2368 | 262 | 2018-2024 | 1282 | 54.1% | 4184 | 542 | 56.7% |
| mf | 603 | 67 | 2011-2025 | 309 | 51.2% | 4298 | 27 | 91.3% |
| nih | 184 | 34 | 2021-2025 | 81 | 44.0% | 2341 | 81 | 0% |
| nla | 2074 | 209 | 2017-2025 | 2074 | 100% | 34864 | 209 | 89.9% |
| nmbu | 335 | 18 | 2014-2025 | 207 | 61.8% | 5787 | 10 | 95.2% |
| nord | 3664 | 453 | 2016-2025 | 3654 | 99.7% | 5269 | 440 | 88.0% |
| ntnu | 5735 | 409 | 2004-2025 | 3791 | 66.1% | 4936 | 1531 | 59.6% |
| oslomet | 1705 | 179 | 2018-2025 | 1705 | 100% | 12226 | 702 | 57.3% |
| uia | 2374 | 237 | 2020-2025 | 1130 | 47.6% | 4406 | 701 | 38.0% |
| uib | 2613 | 171 | 2004-2025 | 1616 | 61.8% | 7780 | 105 | 93.5% |
| uio | 1767 | 109 | 2003-2025 | 93 | 5.3% | 3489 | 92 | 1.1% |
| uis | 2920 | 316 | 2007-2025 | 1208 | 41.4% | 4064 | 109 | 91.0% |
| uit | 6693 | 621 | 2004-2025 | 6693 | 100% | 4080 | 554 | 91.5% |
| usn | 3036 | 401 | 2018-2025 | 1189 | 39.2% | 10600 | 1148 | 3.4% |
| **Total** | **42564** | | | **29943** | **70.3%** | | **7472** | |

**Columns:**
- **Rows**: Total course-semester rows in harvested data (from DBH)
- **Codes**: Unique course codes (Emnekode_raw)
- **Fulltext OK**: Rows with successfully extracted course plan text
- **Rate**: Fulltext success rate (Fulltext OK / Rows)
- **Median chars**: Median character count of extracted text
- **Unique plans**: Distinct course plans after normalization and deduplication
- **Dedup %**: Percentage of rows removed by deduplication (1 - unique/has_plan)

## Interpreting the dedup ratio

High dedup (>80%) is expected and normal for institutions where:
- The same course plan text is reused across multiple years (hivolda, hvl, nla, nord, uit, uib, uis, mf, nmbu)
- DBH registers courses for both semesters but only one plan page exists

Low dedup (<20%) means most rows have genuinely different content, typically because:
- Plans change frequently between years (usn, hiof)
- Only one year of data is available (uio, nih)

## Per-Institution Notes

---

### HiOF (Ostfold University College)

- **49.4% success rate**: Many older URLs (pre-2021) return 404
- Historical courses (before autumn 2021) use a different URL structure; the URL builder has fallback logic
- Two CSS selectors used: old structure (`#LiteralFullBeskrivelse`) and new (`.description-section`)

---

### Hivolda (Volda University College)

- **100% fulltext rate** but **91.6% dedup**: The URL builder ignores year/semester and always fetches `https://www.hivolda.no/emne/{CODE}`, which returns the latest version
- The website uses numeric IDs to distinguish versions (e.g., `/emne/MGL5-10NO2B/12177` vs `/emne/MGL5-10NO2B/6255`), but these IDs cannot be derived from metadata
- **Known issue (see #63)**: All years got the same content; only 99 unique plans from 1182 rows. Data needs re-harvesting once the ID-to-year mapping is understood
- Data should be treated as "latest version only" until resolved

---

### HVL (Western Norway University of Applied Sciences)

- **80.1% success rate**: 716 rows have HTML but extraction returned empty
- These are likely pages where the CSS selector doesn't match (website structure variation)
- High dedup (89.7%) suggests stable course plans across years

---

### INN (Inland Norway University of Applied Sciences)

- **54.1% success rate**: 1086 rows have no HTML (URLs return 404 or errors)
- 30 pages are placeholder/error pages ("Emnesøket gjelder kun fra...") filtered as NA by normalize_plan_text
- Year range 2018-2024 (no 2025 data in DBH extract)
- Moderate dedup (56.7%) indicates regular plan updates

---

### MF (MF Norwegian School of Theology)

- **51.2% success rate**: 294 pages return 404
- Small institution (67 unique course codes)
- Very high dedup (91.3%): only 27 unique plans from 309 successful fetches
- Normalize strips from "Emneansvarlig" to end (removes teacher names, marketing)

---

### NIH (Norwegian School of Sport Sciences)

- **44% success rate**: 103 of 184 pages return 404
- Small institution (34 codes), short year range (2021-2025)
- No deduplication (100% unique) - each plan row is distinct
- Shortest median content (2341 chars)

---

### NLA (NLA University College)

- **100% fulltext rate** with very high median content (34,864 chars)
- High character count suggests the CSS selector captures more than just the course plan (possibly entire page including navigation/boilerplate)
- High dedup (89.9%): 209 unique plans from 2074 rows

---

### NMBU (Norwegian University of Life Sciences)

- Only 18 unique course codes in harvest (small DBH subset)
- **61.8% success rate**: 128 pages return 404
- Extremely high dedup (95.2%): only 10 unique plans from 207 rows

---

### Nord (Nord University)

- **99.7% success rate**: Near-perfect fetch rate (only 10 failures)
- High dedup (88%): Course plans stable across years
- 453 unique codes spanning 2016-2025

---

### NTNU (Norwegian University of Science and Technology)

- **66.1% success rate**: 1944 rows with no HTML
- URL includes year but not semester; special error detection for "no information available" pages
- Normalize strips from "Kontaktinformasjon" to end (teacher names, exam dates, timetable)
- Moderate dedup (59.6%) with wide year range (2004-2025)

---

### OsloMet (Oslo Metropolitan University)

**Re-harvested 2026-02-14** (original harvest returned only error pages due to client-side rendering).

- **100% fulltext rate** (1705/1705 rows have content)
- 60 rows contain error pages ("Siden du leter etter finnes ikke") which are filtered as NA by normalize_plan_text, leaving 1645 rows with genuine content
- Median 12,226 chars per page (highest after NLA and USN)

**URL semester issue**: OsloMet's website only accepts "HØST" (autumn) in URLs. All semester variants for spring return 404. Workaround: always use "HØST" regardless of actual semester. Course content appears semester-agnostic.

- URL pattern: `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`
- The `semester` column in output reflects DBH registration, not the URL used

---

### UiA (University of Agder)

**Verified 2026-02-09**: Pipeline working correctly with updated CSS selector.

- **47.6% success rate**: Website only has courses from **2020 onwards**; 2013-2019 courses return 404
- Filter applied in `R/run_harvest_uia.R` to only process 2020+ courses

**Whole-year courses**: DBH registers courses for both semesters, but UiA typically publishes only one page. Of 1150 courses in both semesters: 76.3% have autumn page only, 16.8% spring only, 0.3% both.

**URL vs content semester mismatch**: 37% of autumn URLs contain spring-semester content. UiA publishes some spring courses under autumn URLs only.

- URL pattern: `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`
- CSS selector: `#right-main`

---

### UiB (University of Bergen)

- **61.8% success rate**: 997 rows with no HTML
- Wide year range (2004-2025); older pages likely no longer available
- Very high dedup (93.5%): only 105 unique plans from 1616 rows
- Uses `.extract_many()` for multiple content sections (accordion structure)

---

### UiO (University of Oslo)

- **5.3% success rate (by design)**: UiO only publishes the latest version of each course plan, not historical versions
- Only 93 rows have content (out of 1767 total rows spanning 2003-2025)
- Content was harvested only for 2025 or the last year a discontinued course was offered
- Requires faculty/department slugs derived from `Avdelingsnavn` (hardcoded mapping)
- URL pattern: `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`

---

### UiS (University of Stavanger)

- **41.4% success rate**: 1576 rows with no HTML
- Wide year range (2007-2025); older pages no longer available
- Very high dedup (91%): only 109 unique plans from 1208 rows
- Uses `.extract_many()` for accordion-style content

---

### UiT (The Arctic University of Norway)

- **100% fulltext rate** (6693/6693 rows)
- However, 2817 rows contain "Error rendering component" artifacts in the text
- These are stripped by normalize_plan_text (.pre_uit) but may indicate partially rendered pages
- Very high dedup (91.5%): 554 unique plans from 6519 rows with valid plan IDs
- 174 rows have fulltext but normalize to NA (likely rendering errors with no real content)

---

### USN (University of South-Eastern Norway)

- **39.2% success rate (expected)**: Requires URL version discovery via JavaScript-rendered pages
- Course plan URLs include a version number that can't be determined from metadata alone
- Breakdown: ~39% autumn-start only, ~11% spring-start only, ~25% both semesters, ~25% no page at all
- "New" courses (status=2) have ~5% success; "Active" (status=1) courses ~81%
- Uses Shadow DOM extraction via `rvest::read_html_live()`
- Low dedup (3.4%): Plans change frequently between versions/years
- High median chars (10,600) - comprehensive course plan content
