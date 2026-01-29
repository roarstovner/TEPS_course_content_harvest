# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based web scraping pipeline that harvests course descriptions from Norwegian higher education institutions. It takes course metadata (institution, course code, year, semester) and produces structured data with URLs, HTML, and extracted text from institutional course webpages.

**Input:** Course metadata from `data/courses.RDS` (typically sourced from DBH database via rdbhapi)
**Output:** Same data with added `url`, `html`, and `fulltext` columns

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
   - Uses institution-specific CSS selectors from `config/selectors.yaml`
   - Two helper functions: `.extract_one()` for single elements, `.extract_many()` for multiple
   - Wrapped in `purrr::possibly()` for safe execution (returns NA on failure)

### Main Entry Points

- **`R/run_harvest.R`**: Loop-based processing of standard institutions (skips those needing special handling)
- **`R/run_harvest_usn.R`**: USN-specific workflow with URL version discovery
- **`R/run_harvest_uio.R`**: UiO-specific workflow (or other institutions needing tailored processing)

## Configuration Files

### `config/institutions.yaml`
- Maps institution codes (e.g., "1175") to short names (e.g., "oslomet")
- Semester style conventions per institution

### `config/selectors.yaml`
- CSS selectors for text extraction per institution
- `fulltext`: Main content selector
- `course_name_no`: Course name/title selector

**Important:** When institutions change their website structure, update selectors here first, then test extraction.

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

### `extract_fulltext(institution_short, raw_html)` - R/extract_fulltext.R:3
Extracts clean text from HTML using institution-specific CSS selectors. Returns character vector with extracted text.

### `validate_courses(df, stage)` - R/utils.R:55
Validates required columns exist at pipeline stages: "initial", "with_url", "with_html".

## Running the Pipeline

### Basic workflow for a single institution:

```r
source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "hivolda") |>
  add_course_id() |>
  validate_courses("initial") |>
  add_course_url() |>
  validate_courses("with_url")

df <- fetch_html_with_checkpoint(
  df,
  checkpoint_path = "data/checkpoint/html_hivolda.RDS"
)

df$fulltext <- extract_fulltext(df$institution_short, df$html)
saveRDS(df, "data/html_hivolda.RDS")
```

### USN workflow (with URL discovery):
```r
source("R/run_harvest_usn.R")  # Handles URL version discovery
```

Or manually:
```r
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/extract_fulltext.R")

courses <- readRDS("data/courses.RDS")

df <- courses |>
  filter(institution_short == "usn") |>
  add_course_id() |>
  add_course_url() |>  # Returns NA for USN
  resolve_course_urls(checkpoint_path = "data/checkpoint/usn_urls.RDS")  # Discovers URLs AND fetches HTML

# No fetch_html_with_checkpoint needed - HTML already captured during URL resolution
df$fulltext <- extract_fulltext(df$institution_short, df$html)
saveRDS(df, "data/html_usn.RDS")
```

### Run all standard institutions:
```r
source("R/run_harvest.R")  # Loops through standard institutions (skips USN, UiO)
```

## Adding a New Institution

Use the `/add-institution` skill for step-by-step guidance on adding support for a new institution. The procedure involves:

1. Add institution mapping to `config/institutions.yaml`
2. Add URL builder function to `R/add_course_url.R`
3. Add CSS selector to `config/selectors.yaml`
4. Add extraction function to `R/extract_fulltext.R`
5. Test with small sample, then expand gradually

See `.claude/skills/add-institution.md` for detailed instructions and code templates.

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

### NTNU
- URL includes year but not semester
- Special error detection: checks for "no information available" message and raises ntnu_no_info_error

### UiO
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
- Uses its own harvest script: `R/run_harvest_usn.R`

**Shadow DOM**: USN renders course content inside Shadow DOM (web components). Standard `html_text()` cannot access this content. Use `read_usn_live_html(session)` to extract content:
```r
session <- rvest::read_html_live(url)
Sys.sleep(5)  # Wait for JS rendering
content <- read_usn_live_html(session)
session$session$close()
```
The function uses JavaScript to recursively traverse shadow roots and extract text content.

**Session reuse optimization**: `resolve_urls_usn_batch()` reuses a single Chrome session for all URL validations by navigating via hash changes (`window.location.hash = '#/emne/...'`) instead of creating new sessions per URL. This is much faster since Chrome startup (~2-3s) is avoided for each URL.

### Institutions with Multiple CSS Selectors
Some institutions (nord, uib, uis, usn) need `.extract_many()` to capture content from multiple elements because course info is spread across accordions or sections.

## File Structure

```
R/
├── run_harvest.R          # Main loop: process standard institutions
├── run_harvest_usn.R      # USN-specific workflow with URL discovery
├── run_harvest_uio.R      # UiO-specific workflow
├── utils.R                # add_course_id, validate_courses, normalization
├── add_course_url.R       # Institution-specific URL builders
├── resolve_course_urls.R  # URL discovery for institutions needing trial-and-error
├── fetch_html_cols.R      # HTML downloading with httr2
├── extract_fulltext.R     # CSS selector-based text extraction
└── checkpoint.R           # Checkpoint read/write/resume logic

config/
├── institutions.yaml      # Institution metadata and URL patterns
└── selectors.yaml         # CSS selectors per institution

data/
├── courses.RDS            # Input: course metadata
├── html_{inst}.RDS        # Output: processed data per institution
└── checkpoint/
    ├── html_{inst}.RDS    # HTML fetch checkpoints
    └── usn_urls.RDS       # USN URL discovery checkpoint

data-raw/
└── courses.R              # Script to create courses.RDS
```

## Common Issues

### URLs look wrong?
- Check the institution-specific URL builder in R/add_course_url.R
- Print sample: `courses |> filter(institution_short == "inst") |> add_course_url() |> select(url) |> head()`

### Empty or wrong extracted text?
- Verify CSS selector using browser dev tools on actual course page
- Update config/selectors.yaml if website structure changed
- Check if institution needs `.extract_many()` instead of `.extract_one()`
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
