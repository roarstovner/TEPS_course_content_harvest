# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

`cl` is alias for `chainlink`

## Project Overview

This is an R-based web scraping pipeline that harvests course descriptions from Norwegian higher education institutions. It takes course metadata (institution, course code, year, semester) and produces structured data with URLs, HTML, and extracted text from institutional course webpages.

**Input:** Course metadata from `data/courses.RDS` (typically sourced from DBH database via rdbhapi)
**Output:** Same data with added `url`, `html`, and `extracted_text` columns

## Core Pipeline Architecture

### Sequential Processing Steps

1. **Course ID Generation** (`R/utils.R:add_course_id`)
   - Creates unique identifiers: `{institution}_{code}_{year}_{semester}_{status}`
   - Uses `Emnekode_raw` to preserve granularity
   - Status codes: 1=Active, 2=New, 3=Discontinued, 4=Discontinued but exam offered

2. **URL Generation** (`R/add_course_url.R`)
   - Institution-specific URL builders (e.g., `add_course_url_oslomet()`)
   - Each institution has custom URL patterns and rules
   - Some institutions use year/semester in URLs, others don't
   - Historical data may use different URL structures (see `add_course_url_hiof`)
   - Returns NA for institutions that require URL discovery (e.g., USN)

3. **URL Resolution** (`R/resolve_course_urls.R`) *(optional, for institutions needing discovery)*
   - Discovers URLs that can't be generated from metadata alone
   - USN requires trial-and-error with version numbers (1, 2, 3, ...)
   - Uses Chrome + Shadow DOM extraction to validate URLs point to real course pages
   - For USN: Also captures HTML content during resolution (skips separate fetch step)
   - Checkpoints discovered URLs (and HTML for USN) in `data/checkpoint/usn_urls.RDS`
   - Skipped for institutions where URLs are deterministic

4. **HTML Fetching** (`R/fetch_html_cols.R` + `R/checkpoint.R`)
   - Downloads HTML from URLs with automatic retry via checkpointing
   - Institution-specific overrides for special cases (NTNU detects "no info" pages)
   - User agent identifies the TEPS research project
   - Checkpoints stored in `data/checkpoint/html_{institution}.RDS`

5. **Text Extraction** (`R/extract_fulltext.R`)
   - Config-driven: `extract_fulltext_css()` uses selector and mode from institution config
   - Pre/post processing functions referenced directly in config (e.g., `pre_fn = .add_table_cell_breaks`)
   - Wrapped in `purrr::possibly()` for safe execution (returns NA on failure)

### Main Entry Points

- **`R/harvest.R`**: `harvest_institution()` harvests a single institution; `harvest_all()` loops all
- Both dispatch to strategy functions in `R/harvest_strategies.R` via config from `R/institution_config.R`

### Institution Configuration (`R/institution_config.R`)

Single source of truth for all institution config — a plain R list, no YAML files.
Each institution declares: strategy, CSS selectors, selector mode, `year_in_url`, pre/post functions, fetch overrides.

**Important:** When institutions change their website structure, update selectors in `R/institution_config.R`, then test extraction.

## Key Functions

### `add_course_id(dbh_df)` - R/utils.R:16
Creates unique course identifiers combining institution, raw course code, year, semester, and status.

### `add_course_url(df)` - R/add_course_url.R:1
Dispatches to institution-specific URL builders using `case_match()`. Returns tibble with `url` column added. Returns NA for institutions requiring URL discovery.

### `resolve_course_urls(df, checkpoint_path, .progress)` - R/resolve_course_urls.R:11
Discovers URLs for courses where URL is NA (institutions like USN requiring trial-and-error). Uses checkpointing to avoid re-discovering. Dispatches to institution-specific batch resolvers via `group_modify()` + `case_match()`. For USN, returns df with both `url` and `html` columns filled in (no separate fetch step needed).

### `resolve_urls_usn_batch(df, max_version, .progress)` - R/resolve_course_urls.R:101
Batch URL resolver for USN that reuses a single Chrome session. Navigates via hash changes instead of creating new sessions per URL. Uses `read_usn_live_html()` to extract Shadow DOM content and validate that displayed year/semester matches the requested one. Returns both `url` and `html` columns since the HTML is already fetched during validation.

### `read_usn_live_html(session)` - R/resolve_course_urls.R:219
Extracts rendered text content from a USN LiveHTML session by traversing Shadow DOM via JavaScript. The caller is responsible for: (1) creating the session with `rvest::read_html_live()`, (2) waiting for content to render, and (3) closing the session when done. Returns content string or NULL on error.

### `fetch_html_with_checkpoint(courses, checkpoint_path, .progress)` - R/checkpoint.R:28
Downloads HTML for courses not already in checkpoint. Automatically skips completed courses and handles failures gracefully.

### `harvest_institution(institution, courses, year, refetch)` - R/harvest.R:14
Main entry point for harvesting a single institution. Reads config, filters courses, dispatches to the appropriate strategy function, and ensures uniform output columns.

### `harvest_all(courses, year, refetch, institutions)` - R/harvest.R:56
Loops through all (or specified) institutions, calls `harvest_institution()` for each, saves results to `data/html_{inst}.RDS`.

### `extract_fulltext_css(html, selector, mode, pre_fn, post_fn)` - R/extract_fulltext.R:11
Config-driven CSS extraction. Replaces the old institution-dispatching `extract_fulltext()`. Takes selector and mode from institution config.

### `get_institution_config(inst)` - R/institution_config.R:173
Returns the config list for an institution. Config includes strategy, selector, selector_mode, year_in_url, pre/post functions, fetch overrides.

### `validate_courses(df, stage)` - R/utils.R:55
Validates required columns exist at pipeline stages: "initial", "with_url", "with_html".

### `anonymize_text(institution, text)` - R/anonymize.R:13
Removes PII (names, emails, phone numbers), dates, seasons, administrative year references (e.g., "Opprettet 2020", "2023/2024"), and institution-specific boilerplate from raw text. Applied to `extracted_text` (→ `course_plan`, in `R/run_dedup.R`) and to each section's `raw_text` (in `R/run_extract_sections.R`). Content years (e.g., "etter 1945", "NOU 2015:2") are preserved. Returns readable anonymized text preserving case and paragraph structure. Uses institution-specific handlers (`.anon_*()`) followed by generic cleanup (`.anon_generic()`).

### `normalize_plan_text(course_plan)` - R/normalize_plan_text.R:14
Applies lossy dedup-specific transforms on already-anonymized `course_plan`: `tolower()`, heading synonym normalization ("eksamensformer" → "vurderingsformer"), blanket 4-digit year removal, and `str_squish()`. No longer takes `institution` parameter.

### `deduplicate_plans(df)` - R/deduplicate_plans.R:14
Takes combined data with `course_plan` column, normalizes text, builds content hashes, and produces a plan lookup table. Returns list with `plans` (unique plans) and `courses` (original data with `plan_content_id`).

## Running the Pipeline

### Harvest a single institution:

```r
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/institution_config.R")
source("R/checkpoint.R")
source("R/harvest_strategies.R")
source("R/harvest.R")

courses <- readRDS("data/courses.RDS")
result <- harvest_institution("hivolda", courses)
saveRDS(result, "data/html_hivolda.RDS")
```

### Harvest a specific year:

```r
result <- harvest_institution("oslomet", courses, year = 2025)
```

### Re-harvest (ignore checkpoints):

```r
result <- harvest_institution("ntnu", courses, refetch = TRUE)
```

### Harvest all institutions:

```r
harvest_all()  # Reads courses.RDS, loops all institutions, saves results
```

### Anonymize and deduplicate:

```r
source("R/run_dedup.R")  # Loads html_*.RDS, anonymizes extracted_text → course_plan, deduplicates
```

Pipeline: `extracted_text` → `course_plan` (anonymized, readable) → `course_plan_normalized` (lossy, for hashing)

### Extract sections:

```bash
Rscript R/run_extract_sections.R  # html_*.RDS → data/sections_raw.RDS + coverage/unmapped-heading diagnostics
```

Splits each course into the seven canonical sections, anonymizes each section's
`raw_text` with `anonymize_text()`, and writes `data/sections_raw.RDS`
(`course_id`, `institution`, `section`, `raw_text`). Re-run after a harvest or
after changing `R/section_heading_map.R`, `R/extract_sections.R`,
`R/anonymize.R` or a `section_*` field in `R/institution_config.R`, then
rebuild the browser data. See "Section Extraction" below.

### Regenerate data quality notes:
```bash
quarto render data/data_notes.qmd  # Reads course_offerings.RDS + course_plans.RDS
```
Run this after re-harvesting any institution or re-running the dedup pipeline (`R/run_dedup.R`).
Edit `data/data_notes.qmd` directly to update per-institution prose notes.

### Run the course browser:
```bash
Rscript R/build_browser_data.R                      # after run_dedup.R or extract_sections
Rscript -e 'shiny::runApp("app/course_browser")'
```
`build_browser_data.R` must be re-run whenever `course_plans.RDS`,
`course_offerings_full.RDS`, or `sections_raw.RDS` change — the app reads only
the prebuilt `data/browser_data.RDS`.

## Section Extraction

`extract_sections(institution, html, extracted_text, course_id)`
(`R/extract_sections.R`) dispatches on `section_strategy` in the institution
config:

| Strategy | Institutions | How it splits |
|---|---|---|
| `html_headings` | oslomet, uia, ntnu, inn, hiof, hvl, nih, uio, uis, uit, nmbu | DOM walk; a heading (`section_heading_level`, or `section_heading_selector`) starts a section. Falls back to `text_split` when it finds < 3 sections |
| `html_fields` | hivolda | `div.field-<name>` containers mapped to sections by `section_fields` |
| `details_mf` | mf | `<details><summary>` accordions + the untitled intro block (`section_intro_selector`) as course_content |
| `details_uib` | uib | `<details><summary>` accordions + top-level `h2` |
| `accordion_nord` | nord | `div.ac` accordion trigger/panel pairs |
| `json_nla` | nla | Titles in the embedded EmneplanPage JSON |
| `text_split` | usn, steiner (+ fallback) | Heading-shaped lines in `extracted_text` |
| `noop` | samas | — |

Other `section_*` config fields:
- `section_selector`: container for section extraction when the fulltext
  `selector` is a multi-element selector (`html_element()` would only take its
  first match; uis needs `#block-page-content`).
- `section_subheading_selector`: elements inside a section that switch to
  another section (uio `"h3, h4, p"`; `"p"` for uia, oslomet, hiof, hvl, nmbu,
  mf). A `<p>` counts only if its whole text (minus a trailing colon) equals a
  heading pattern; an unmapped `h3` hands its text back to the parent section.
- `section_inline_coursework`: move "Arbeidskrav (AK): …" / "Obligatorisk
  deltakelse …" lines from assessment to coursework_requirements (nord).

**Heading map** (`R/section_heading_map.R`): `match_heading_to_section()`
checks exact equality against every pattern first, then substring patterns in
table order (first hit wins; rows with `exact = TRUE` only match whole
headings). `text_split` passes `word_start = TRUE` so "elevkunnskap" does not
match "kunnskap", and only accepts heading-shaped lines (capitalised, ≤ 8 words,
no digits, no "Label: value", no closing full stop). Patterns mapped to
`".drop"` end the current section and their text is discarded: admission
headings ("Opptak til emnet", "Opptakskrav", "Hvem kan ta dette emnet?") and
exam logistics ("Mer om eksamen ved UiO", "Hjelpemidler", "Sensorordning").

**Cleanup** (`.clean_sections()`): removes `.drop` rows, strips notices and
page widgets from assessment/coursework (`.section_noise`: plagiarism and
ChatGPT/COVID notices, uib banner and footer, …), removes admission sentences
from prerequisites (`.strip_admission_lines()`), drops placeholder-only rows
(`.placeholder_phrases`: "Ingen", "Se fagplanen.", "-", Leganto pointers).

**Privacy:** sections are cut from raw `html`/`extracted_text`, so
`run_extract_sections.R` must keep anonymizing `raw_text` before saving.

## Auditing a Pipeline Step Across Institutions

Use the `/audit-institutions <sections|fulltext|anonymization> [inst ...]` skill
to check a pipeline step for every institution with one review agent per
institution. R scripts build a review packet per institution
(`R/audit/prepare_{check}.R`), the agents write findings to
`data/audit/{check}/findings/{inst}.json`, and `R/audit/aggregate.R` verifies
them (course ids and verbatim quotes must be in the packet) and writes a ranked
cross-institution report. See `.claude/skills/audit-institutions/SKILL.md`.

## Adding a New Institution

Use the `/add-institution` skill for step-by-step guidance on adding support for a new institution. The procedure involves:

1. Add config entry to `R/institution_config.R` (strategy, selector, year_in_url, etc.)
2. Add URL builder function to `R/add_course_url.R`
3. Test with `harvest_institution("newuni", courses)`

For standard institutions, no changes to `extract_fulltext.R` or `harvest_strategies.R` are needed — the config-driven pipeline handles it automatically. Complex strategies (shadow DOM, PDF split, etc.) need custom strategy functions in `R/harvest_strategies.R`.

See `.claude/skills/add-institution/SKILL.md` for detailed instructions and code templates.

## Important Data Patterns

### Course Status Codes (from DBH)
- 1: Aktivt (Active)
- 2: Nytt (New)
- 3: Avviklet (Discontinued)
- 4: Avviklet, men tas eksamen (Discontinued but exam offered)

### Semester Normalization
`canon_semester_name()` converts:
- "Vår" → "spring"
- "Høst" → "autumn"
- "Sommer" → "summer"

### Course Code Normalization
`canon_remove_trailing_num()` removes trailing version numbers (e.g., "ABC123-1" → "ABC123").
Used in older code but current pipeline uses `Emnekode_raw` to preserve granularity.

### Checkpoint Behavior
- Checkpoints store course_id + html + html_success + html_error
- Only fetches courses not in checkpoint (anti_join by course_id)
- If script crashes, just re-run - it resumes from checkpoint
- One checkpoint file per institution: `data/checkpoint/html_{inst}.RDS`

## Institution-Specific Quirks

### NORD
- URL requires Norwegian characters in semester param: `HØST` or `VÅR` (not ASCII `HOEST`)
- ASCII `HOEST` falls back to a generic "Gjeldende emnebeskrivelse" page without year-specific content
- The site maps each semester to its academic year automatically (e.g. `VÅR&year=2023` → 2022/23 plan)
- URL pattern: `https://www.nord.no/studier/emner/{code}?year={year}&semester={HØST|VÅR}`

### NTNU
- URL includes year but not semester
- Special error detection: checks for "no information available" message and raises ntnu_no_info_error

### UiO
- **No historical course plans**: UiO only publishes the **latest version** of each course plan. The base URL always returns the current plan; there is no way to access older versions. Semester-specific URLs (`/h24/`, `/v25/`) contain **logistics only** (instructors, schedule, exam dates) — NOT course plan content. Do NOT switch to semester-specific URLs.
- Data should be filtered to current year only (same as other no-year institutions)
- Requires faculty/department slugs derived from `Avdelingsnavn`
- Uses hardcoded mapping in `add_course_url_uio()` (R/add_course_url.R:77)
- URL pattern: `https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`

### HiOF
- Historical courses (before autumn 2021) use different URL structure
- Switch logic in `add_course_url_hiof()` (R/add_course_url.R:62)
- Multiple fallback selectors in `extract_fulltext_hiof()`

### USN
- **Requires URL version discovery** - course plan URLs include a version number that can't be determined from metadata
- URL pattern: `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`
- Version numbers (1, 2, 3, ...) indicate revisions to the course plan, each valid for specific year ranges
- **Silent redirects**: Invalid version+year combos redirect to other pages without changing the URL
- `resolve_urls_usn_batch()` verifies displayed year matches requested year by extracting "Undervisningsstart høst YYYY" from rendered page
- Tries versions 1-5, with early exit optimizations:
  - If course code not in page content → version doesn't exist, stop checking (versions are sequential)
  - If version 1's teaching start > requested year → course didn't exist yet, stop checking
- Returns NA when no version exists for that year/semester (course wasn't offered)
- Uses `rvest::read_html_live()` for JavaScript rendering (hash fragment routing)
- Uses `shadow_dom` strategy in `R/harvest_strategies.R`

**Shadow DOM**: USN renders course content inside Shadow DOM (web components). Standard `html_text()` cannot access this content. Use `read_usn_live_html(session)` to extract content:
```r
session <- rvest::read_html_live(url)
Sys.sleep(5)  # Wait for JS rendering
content <- read_usn_live_html(session)
session$session$close()
```
The function uses JavaScript to recursively traverse shadow roots and extract text content.

**Session reuse optimization**: `resolve_urls_usn_batch()` reuses a single Chrome session for all URL validations by navigating via hash changes (`window.location.hash = '#/emne/...'`) instead of creating new sessions per URL. This is much faster since Chrome startup (~2-3s) is avoided for each URL.

### UiT
- **Requires document ID discovery** - historical course plans use `p_document_id` parameter
- Active page (`/utdanning/aktivt/emne/{CODE}`) has a `<select>` dropdown listing all semesters with document IDs
- Option text format: `{CODE}: H 2020` or `{CODE}: V 2021` (H=Høst, V=Vår)
- Historical URL pattern: `https://uit.no/utdanning/emner/emne?p_document_id={ID}`
- Discontinued courses (status 3/4) still have working active pages with dropdowns
- Same CSS selector works for both active and historical pages
- Uses `url_discovery` strategy (with `resolve_course_urls()` for URL discovery)

### Samas (Sámi University)
- Uses `noop` strategy — extracted_text is set to NA (course plans are in Sami, not Norwegian)

### Institutions with Multiple CSS Selectors
Some institutions (nord, uib, uis, uit) use `selector_mode = "multi"` to capture content from multiple elements because course info is spread across accordions or sections. Section extraction cannot use such a selector as its container; give the institution a `section_selector` (see "Section Extraction").

## File Structure

```
R/
├── harvest.R              # Entry points: harvest_institution(), harvest_all()
├── harvest_strategies.R   # Strategy implementations: harvest_standard(), harvest_shadow_dom(), etc.
├── institution_config.R   # Institution registry: strategy, selectors, pre/post fns, fetch overrides
├── utils.R                # add_course_id, validate_courses, normalization
├── add_course_url.R       # Institution-specific URL builders
├── resolve_course_urls.R  # URL discovery for institutions needing trial-and-error
├── fetch_html_cols.R      # HTML downloading with httr2
├── extract_fulltext.R     # extract_fulltext_css() (config-driven), extract_nla_json(), helpers
├── checkpoint.R           # Checkpoint read/write/resume logic
├── anonymize.R            # PII removal: anonymize_text() for course_plan and sections_raw
├── normalize_plan_text.R  # Lossy normalization for dedup hashing (tolower + synonyms + year removal + squish)
├── deduplicate_plans.R    # Groups identical plans by content hash
├── run_dedup.R            # Entry point: anonymize + normalize + dedup pipeline
├── section_heading_map.R  # Heading → section patterns (incl. ".drop") + match_heading_to_section()
├── extract_sections.R     # extract_sections(): section strategies + cleanup
├── run_extract_sections.R # Entry point: extract + anonymize sections → sections_raw.RDS
├── build_browser_data.R   # Builds data/browser_data.RDS for the course browser
└── audit/                 # Audit harness for /audit-institutions
    ├── utils.R            # Shared helpers + allowed finding enums per check
    ├── qa_sections.R      # Deterministic pre-pass over sections_raw.RDS (feeds prepare_sections.R)
    ├── prepare_{check}.R  # Pre-pass + review packets (sections, fulltext, anonymization)
    └── aggregate.R        # Verify + merge agent findings into a report

data/
├── courses.RDS            # Input: course metadata
├── html_{inst}.RDS        # Output: processed data per institution
├── course_offerings.RDS       # Published dataset (slim): DBH metadata + plan_content_id FK, no url/text/html
├── course_offerings_full.RDS  # Internal: same rows as offerings but with url + extracted_text + course_plan + course_plan_normalized (used by course_browser)
├── course_plans.RDS           # Published dataset: deduplicated course plan texts
├── sections_raw.RDS           # Extracted, anonymized sections, one row per (course_id, section)
├── browser_data.RDS           # Slim payload for course_browser (build with R/build_browser_data.R)
├── data_notes.qmd         # Data quality notes (Quarto source; render to regenerate data_notes.md)
├── data_notes.md          # Data quality documentation (rendered from data_notes.qmd)
├── audit/{check}/         # Audit harness output: packets/ (gitignored), findings/, findings_report.md, synthesis.md
└── checkpoint/
    ├── html_{inst}.RDS    # HTML fetch checkpoints
    └── urls_{inst}.RDS    # URL discovery checkpoints

app/
└── course_browser/        # Plan-centric concordance browser (Shiny)
    ├── app.R              # UI + server: Concordance, Trend, Coverage, Diff tabs
    ├── R/search.R         # Search engine, KWIC snippets, match highlighting
    ├── R/data.R           # Payload loading, labels, optional term set, diff renderer
    └── www/styles.css

data-raw/
└── courses.R              # Script to create courses.RDS
```

## Course Browser

`app/course_browser` is **plan-centric**: the unit is the unique course plan
(~11k), not the offering (~33k). 66% of offerings share their plan text, so an
offering-level browser makes you read the same paragraph repeatedly. Offering
coverage (years, semesters, count) is rolled onto each plan instead.

- **Concordance** — ad-hoc literal or regex search over plan text, or scoped to
  one extracted section. All matches highlighted, shown as numbered
  keyword-in-context snippets, with full plan / sections / source HTML for
  provenance.
- **Trend** — occurrence of the current search by year, optionally by
  institution or Fagnavn. Plans count in `year_from`, so this measures uptake in
  new or revised plans, not the share of plans in force.
- **Coverage** — share of offerings with extracted text per institution-year.
  41% of offerings have no text, so denominators are not comparable across
  institutions without checking this first.
- **Diff** — consecutive plan versions for one course code.

**Query syntax.** In literal mode, whitespace separates terms and every term
must be present — `livsmestring dybdelæring` is AND, not a phrase. Wrap a
phrase in double quotes (`"folkehelse og livsmestring"`) to search it as one
term. Each term gets its own highlight colour (cycling after four) and its own
count in the legend. Regex mode takes the whole box as a single pattern.

AND is implemented in `parse_query()` rather than as a regex, deliberately.
Regex has no AND operator, and the lookahead that emulates one is a trap on
this data: `(?=.*a)(?=.*b)` silently matches nothing without `(?s)` because
plan text contains newlines, costs ~80 min over 11k plans unless anchored with
`^`, and even anchored matches zero-width — so it can filter but never
highlight, and reports a hit count of 1 per plan. Two `stri_count_fixed` passes
give the same 120 plans in 0.019s with real per-term counts and highlighting.

**Search performance.** `build_search_index()` keeps a pre-lowercased copy of
every searchable text. A literal case-insensitive search then reduces to
`stri_count_fixed` on already-folded text (~0.01s over 11k plans); regex
queries cannot use that path — lowercasing a pattern would corrupt it (`\S`
becomes `\s`) — so they run case-insensitively against the original text
(~0.5s). Match *positions* are computed only for the selected plan, never for
the whole result set, and overlapping ranges from different terms are merged by
`merge_locs()` so the markup never nests.

**Deep links.** `?q=livsmestring&scope=learning_outcomes&regex=1&inst=ntnu,uit`
restores a search, so a query can be bookmarked or cited. Percent-encode spaces
in a multi-term query (`?q=livsmestring%20dybdel%C3%A6ring`).

**Optional term set.** Point `TEPS_BROWSER_TERMS` at a terms YAML (same shape as
`emneplan_LK20/LK20_terms.yaml`: a `terms:` list of `id`/`label`/`regex`), or
drop a `browser_terms.yaml` in the repo root, to get a dropdown of predefined
terms. Absent, the control hides itself and ad-hoc search is unaffected.

## Common Issues

### URLs look wrong?
- Check the institution-specific URL builder in R/add_course_url.R
- Print sample: `courses |> filter(institution == "inst") |> add_course_url() |> select(url) |> head()`

### Empty or wrong extracted text?
- Verify CSS selector using browser dev tools on actual course page
- Update selector in `R/institution_config.R` if website structure changed
- Check if institution needs `selector_mode = "multi"` instead of `"single"`
- Some institutions have different structures for different years

### HTML download fails?
- Check `df$html_error` for error messages
- Some institutions may rate-limit or block automated requests
- Verify URL is accessible in browser
- For NTNU, check if page shows "no information available"

### Checkpoint file is huge?
- Normal - HTML is large
- Checkpoint files are in .gitignore
- Can delete and restart if needed: checkpoint will rebuild

### Content not captured from JavaScript-rendered pages?
- Some sites use Shadow DOM (web components) which `html_text()` cannot access
- For USN, use `read_usn_live_html(session)` to extract Shadow DOM content
- For other sites, use `session$session$Runtime$evaluate("javascript code")` to execute JS
- Use `session$view()` to open browser and visually debug what's rendered
