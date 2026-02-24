# R/generate_data_notes.R
#
# Regenerates data/data_notes.md from current data files.
# Run: Rscript R/generate_data_notes.R

library(dplyr)

# --- Load data ---------------------------------------------------------------

df <- readRDS("data/courses_with_plan_id.RDS")
plans <- readRDS("data/plan_lookup.RDS")

# --- Summary table -----------------------------------------------------------

summary_tbl <- df |>
  group_by(institution_short) |>
  summarise(
    rows       = n(),
    codes      = n_distinct(Emnekode_raw),
    year_min   = min(Årstall),
    year_max   = max(Årstall),
    has_fulltext = sum(!is.na(fulltext) & nchar(fulltext) > 0),
    has_plan_id  = sum(!is.na(plan_content_id)),
    median_chars = round(median(nchar(fulltext[!is.na(fulltext) & nchar(fulltext) > 0]), na.rm = TRUE)),
    unique_plans = n_distinct(plan_content_id, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    years     = ifelse(year_min == year_max, as.character(year_min), paste0(year_min, "-", year_max)),
    rate      = sprintf("%.1f%%", has_fulltext / rows * 100),
    dedup_pct = sprintf("%.1f%%", (1 - unique_plans / has_plan_id) * 100)
  ) |>
  select(institution_short, rows, codes, years, has_fulltext, rate, median_chars, unique_plans, dedup_pct)

total_rows     <- nrow(df)
total_fulltext <- sum(!is.na(df$fulltext) & nchar(df$fulltext) > 0)
total_rate     <- sprintf("%.1f%%", total_fulltext / total_rows * 100)
total_plans    <- nrow(plans)

# --- Write markdown ----------------------------------------------------------

out <- file("data/data_notes.md", open = "wt", encoding = "UTF-8")

w <- function(...) writeLines(paste0(...), out)

w("# Data Quality Notes")
w("")
w("Generated: ", Sys.Date())
w("")
w("## Overview")
w("")
w("| Institution | Rows | Codes | Years | Fulltext OK | Rate | Median chars | Unique plans | Dedup % |")
w("|------------|-----:|------:|-------|------------:|-----:|-------------:|-------------:|--------:|")

for (i in seq_len(nrow(summary_tbl))) {
  s <- summary_tbl[i, ]
  w(sprintf("| %s | %d | %d | %s | %d | %s | %d | %d | %s |",
            s$institution_short, s$rows, s$codes, s$years,
            s$has_fulltext, s$rate, s$median_chars, s$unique_plans, s$dedup_pct))
}

w(sprintf("| **Total** | **%d** | | | **%d** | **%s** | | **%d** | |",
          total_rows, total_fulltext, total_rate, total_plans))

w("")
w("**Columns:**")
w("- **Rows**: Total course-semester rows in harvested data (from DBH)")
w("- **Codes**: Unique course codes (Emnekode_raw)")
w("- **Fulltext OK**: Rows with successfully extracted course plan text")
w("- **Rate**: Fulltext success rate (Fulltext OK / Rows)")
w("- **Median chars**: Median character count of extracted text")
w("- **Unique plans**: Distinct course plans after normalization and deduplication")
w("- **Dedup %**: Percentage of rows removed by deduplication (1 - unique/has_plan)")
w("")
w("## Year coverage")
w("")
w("Institutions fall into two groups based on URL structure:")
w("")
w("**Year-specific URLs** (historical data available): HiOF, INN, NIH, NTNU, OsloMet, UiA, USN.")
w("These institutions include the year (and sometimes semester) in their course page URLs, so we can harvest distinct content for each year.")
w("")
w("**No year in URL** (current year only): HVL, Hivolda, MF, NLA, NMBU, Nord, Samas, Steiner, UiB, UiO, UiS, UiT.")
w("These institutions serve the same (latest) page regardless of year. Data is filtered to 2025 only to avoid duplicating current content across historical years. Hivolda is an exception: it has semester-specific pages discoverable via URL resolution, so it retains historical data.")
w("")
w("## Interpreting the dedup ratio")
w("")
w("High dedup (>40%) is expected for current-year-only institutions where DBH registers courses for both semesters but only one plan page exists.")
w("")
w("Low dedup (<10%) means most rows have genuinely different content, typically because plans change between years/versions (HiOF, USN) or each row is truly distinct (NIH, Steiner, UiO).")
w("")
w("## Per-Institution Notes")

# --- Per-institution sections (qualitative, from chainlink issue history) ---

w("")
w("---")
w("")
w("### HiOF (Ostfold University College)")
w("")
w("- **49.4% success rate**: Many older URLs (pre-2021) return 404")
w("- Historical courses (before autumn 2021) use a different URL structure; the URL builder has fallback logic")
w("- Two CSS selectors used: old structure (`#vrtx-fs-emne-content`) and new (`.entry-content`)")
w("- HiOF required a browser-like user agent to avoid 403 blocks (see #27)")
w("- `normalize_plan_text` strips \"Sist hentet fra FS\" timestamps and \"Litteraturlista er sist oppdatert\" lines")

w("")
w("---")
w("")
w("### Hivolda (Volda University College)")
w("")
w("- **Re-harvested 2026-02-16** with URL discovery (see #63, #79-#83)")
w("- The base URL `/emne/{CODE}` returns only the latest version. Semester-specific pages have numeric IDs (e.g., `/emne/MGL5-10NO2B/12177`) that cannot be derived from metadata")
w("- `resolve_urls_hivolda_batch()` scrapes the base page to discover semester links, mapping \"Haust\" to \"Høst\" for matching")
w("- Result: 594/1182 rows with fulltext (up from 1182 identical pages). 556 unique plans (up from 99)")
w("- 588 NA rows = course not offered in that semester (legitimate missing data)")

w("")
w("---")
w("")
w("### HVL (Western Norway University of Applied Sciences)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **88.2% success rate**: 79 rows have no content")
w("- 716 rows in the original harvest were soft-404 \"course not found\" pages; these are now detected during fetch and marked as `html=NA` (see #66)")
w("- Uses `.extract_one()` with selector `.l-2-col__main-content`")

w("")
w("---")
w("")
w("### INN (Inland Norway University of Applied Sciences)")
w("")
w("- **54.1% success rate**: 1086 rows have no HTML (URLs return 404 or errors)")
w("- URL builder filters out pre-2022 courses (return NA), as INN's website only covers 2022+ (see #26)")
w("- 30 pages are placeholder/error pages (\"Emnesøket gjelder kun fra...\") filtered as NA by `normalize_plan_text` (see #53)")
w("- Year range 2018-2024 (no 2025 data in DBH extract)")
w("- INN publishes some courses in both Norwegian and English, causing lower dedup than expected. `normalize_plan_text` strips 35 NO/EN field labels and normalizes language metadata (see #52)")

w("")
w("---")
w("")
w("### MF (MF Norwegian School of Theology)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **92.7% success rate**: 4 pages return 404 (discontinued courses)")
w("- CSS selector was fixed from `#main` to `main` (see #25)")
w("- `normalize_plan_text` strips from \"Emneansvarlig\" to end (removes teacher names, contact info, marketing content)")

w("")
w("---")
w("")
w("### NIH (Norwegian School of Sport Sciences)")
w("")
w("- **44% success rate**: 103 of 184 pages return 404")
w("- Small institution (34 codes), short year range (2021-2025)")
w("- No deduplication (0%) — each fetched plan is distinct")
w("- Shortest median content (2341 chars)")

w("")
w("---")
w("")
w("### NLA (NLA University College)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **100% fulltext rate** (365/365)")
w("- Original harvest failed (0% extraction) because NLA's `/studietilbud/emner/` URLs are React-rendered. Fix: switched to `/for-studenter/Studie-%20og%20emneplaner/emneplan/{CODE}` with selector `.page-course-plan` (see #55)")
w("- `.pre_nla()` in `extract_fulltext.R` strips year dropdown, \"Last ned PDF\" link, and truncates at \"Digital litteraturliste\" to remove reading lists (reduced median from 34.9K to 5.3K chars; see #67)")

w("")
w("---")
w("")
w("### NMBU (Norwegian University of Life Sciences)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- Only 15 unique course codes in current year")
w("- **65.5% success rate**: 10 pages return 404 (discontinued courses)")

w("")
w("---")
w("")
w("### Nord (Nord University)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **100% success rate** (540/540)")
w("- Uses `.extract_many()` for accordion-structured content")

w("")
w("---")
w("")
w("### NTNU (Norwegian University of Science and Technology)")
w("")
w("- **66.1% success rate**: 1944 rows with no HTML (older courses no longer online)")
w("- URL includes year but not semester; special error detection for \"no information available\" pages (`ntnu_no_info_error`)")
w("- `normalize_plan_text` strips from \"Kontaktinformasjon\" to end (teacher names, exam dates, JS artifacts, timetable)")
w("- Wide year range (2004-2025) with genuine content evolution across years")

w("")
w("---")
w("")
w("### OsloMet (Oslo Metropolitan University)")
w("")
w("- **Re-harvested 2026-02-14** (see #57, #61)")
w("- Original harvest returned only error pages because OsloMet was using client-side rendering (React/Liferay). The site later switched to server-side rendering, fixing the issue")
w("- **100% fulltext rate** (1705/1705), but 60 rows contain error pages (\"Siden du leter etter finnes ikke\") which are filtered as NA by `normalize_plan_text`, leaving 1645 rows with genuine content")
w("- **URL semester issue**: OsloMet's website only accepts \"HØST\" (autumn) in URLs. Spring semester URLs return 404. Workaround: always use \"HØST\" regardless of actual semester (see #1)")
w("- URL pattern: `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`")

w("")
w("---")
w("")
w("### Samas (Sámi University of Applied Sciences)")
w("")
w("- **New institution, added 2026-02-16** (see #69-#72)")
w("- **Filtered to 2025 only**")
w("- **0% fulltext rate**: No reliable matching between DBH course codes and website course codes (see #85, #89, #90)")
w("- Samas publishes course plans as PDFs (\"Oahppoplána\") at `samas.no/se/oahput`, but the website uses its own codes (SÁM-1005, DUO-1014, SER 103) that do NOT correspond to DBH codes (V1SAM-1100-1, V1DUO-1140-1, etc.)")
w("- ~98% of DBH rows are V1/V5 teacher education courses. These only appear in program-level PDFs (\"PROGRÁMMAPLÁNA\") that list course names and credits but contain no per-course descriptions")
w("- Individual course plan PDFs exist on the website for standalone courses, but none match any DBH course codes in the data")

w("")
w("---")
w("")
w("### Steiner (Rudolf Steiner University College)")
w("")
w("- **New institution, added 2026-02-16** (see #68, #73)")
w("- **Filtered to 2025 only** (18 rows)")
w("- Course plans harvested from 5 subject-area PDFs at `steinerhoyskolen.no`, split by course heading pattern")
w("- 15/18 rows with fulltext (83.3%). 3 missing courses are Praksis modules (M-LP1/2/3) with no PDF content")
w("- No deduplication (0%) — each of the 15 plans is distinct")

w("")
w("---")
w("")
w("### UiA (University of Agder)")
w("")
w("- **47.6% success rate**: Website only has courses from **2020 onwards**; 2013-2019 courses return 404")
w("- **Whole-year courses**: DBH registers courses for both semesters, but UiA typically publishes only one page. Of 1150 courses in both semesters: 76.3% have autumn page only, 16.8% spring only, 0.3% both")
w("- **URL vs content semester mismatch**: 37% of autumn URLs contain spring-semester content")
w("- URL pattern: `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`")

w("")
w("---")
w("")
w("### UiB (University of Bergen)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **94.7% success rate**: 10 rows with no content")
w("- Uses `.extract_many()` for accordion-structured content")

w("")
w("---")
w("")
w("### UiO (University of Oslo)")
w("")
w("- **Filtered to 2025 only** (see #74, #76)")
w("- **52% success rate**: UiO only publishes the **latest version** of each course plan")
w("- Semester-specific URLs (`/h24/`, `/v25/`) contain **logistics only** (instructors, schedule, exam dates) — NOT course plan content. The base URL always returns the current plan (see #76)")
w("- Requires faculty/department slugs derived from `Avdelingsnavn` (hardcoded mapping in `add_course_url_uio()`)")
w("- URL pattern: `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`")

w("")
w("---")
w("")
w("### UiS (University of Stavanger)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **57.8% success rate**: 149 rows with no content")
w("- Website completely redesigned (migrated to Drupal); old URL pattern `/nb/course/{CODE}` returned 100% 404s. New URL pattern: `/nb/student/course/{CODE}_1` where CODE uses `Emnekode_raw` with dash replaced by underscore (see #19-#24)")
w("- Uses `.extract_many()` for content sections")

w("")
w("---")
w("")
w("### UiT (The Arctic University of Norway)")
w("")
w("- **Filtered to 2025 only** (no year in URL; see #74, #77)")
w("- **97.5% success rate** (545/559)")
w("- All UiT pages contain \"Access denied to page component\" HTML comments and \"Error rendering component\" artifacts — this is a persistent issue on UiT's website, not a scraping problem (see #64)")
w("- CSS selector changed from `.hovedfelt` (single) to `.hovedfelt > main > div.col-md-12` (many) to work around the broken component (see #60)")
w("- `.pre_uit()` strips remaining \"Error rendering component\" text and breadcrumb artifacts")
w("- 14 rows have fulltext that normalizes to NA (rendering errors with no real content)")

w("")
w("---")
w("")
w("### USN (University of South-Eastern Norway)")
w("")
w("- **39.2% success rate (expected)**: USN requires URL version discovery via JavaScript-rendered pages")
w("- Course plan URLs include a version number (1-5) that cannot be determined from metadata alone")
w("- URL pattern: `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`")
w("- **Silent redirects**: Invalid version+year combos redirect to other pages without changing the URL. Validation checks that displayed year matches requested year")
w("- Uses Shadow DOM extraction via `rvest::read_html_live()` with session reuse for performance")
w("- Breakdown: ~39% autumn-start only, ~11% spring-start only, ~25% both semesters, ~25% no page at all")
w("- \"New\" courses (status=2) have ~5% success; \"Active\" (status=1) courses ~81%")
w("- Low dedup (3.4%): Plans change frequently between versions/years")

close(out)

cat("data/data_notes.md written successfully.\n")
cat(sprintf("  %d institutions, %d total rows, %d unique plans\n",
            nrow(summary_tbl), total_rows, total_plans))
