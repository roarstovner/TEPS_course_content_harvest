# TEPS Course Content Harvest - User Guide


## Overview

This project harvests course descriptions from Norwegian teacher
education institutions. It takes course codes, years, and semesters as
input and produces structured data containing URLs, HTML, and extracted
text from course webpages.

**Input:** A tibble as output by `rdbhapi` with course metadata
(institution, course code, year, semester)

**Output:** The same tibble with added course URL, raw HTML, and
extracted full text

## Quick Start: Running the Pipeline

The main entry points are in `R/harvest.R`:

``` r
# Load all pipeline functions
source("R/institution_config.R")
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/checkpoint.R")
source("R/harvest_strategies.R")
source("R/harvest.R")

# Harvest a single institution
courses <- readRDS("data/courses.RDS")
result <- harvest_institution("hivolda", courses)
saveRDS(result, "data/html_hivolda.RDS")

# Or harvest all institutions at once
harvest_all()
```

### What Happens Under the Hood

1.  **`get_institution_config()`**: Looks up strategy, CSS selectors,
    and overrides
2.  **`apply_year_filter()`**: Filters to relevant years based on config
3.  **`add_course_id()`**: Creates unique identifiers for each course
4.  **`add_course_url()`**: Generates the correct URL for each course
5.  **Strategy dispatch**: Routes to the right harvesting strategy
    (standard, URL discovery, etc.)
6.  **`ensure_output_columns()`**: Guarantees uniform output shape

### About Checkpointing

The pipeline uses **checkpointing** to avoid re-downloading data. If the
script stops or crashes:

- Already-downloaded HTML is saved in checkpoint_path, which should be
  updated to `data/checkpoint/html_{institution}.RDS`
- Re-running the script will skip courses already in the checkpoint
- This saves time and is polite to institutional servers

## Adding a New Institution

To add support for a new institution, you need to modify **two files**:

### 1. Add config entry to `R/institution_config.R`

``` r
# Add to the institution_configs list:
newuni = list(
  code = "1234",
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # CSS selector for course plan content
  selector_mode = "single",       # "single" or "multi"
  year_in_url = TRUE
)
```

**How to find CSS selectors:**

1.  Open a course page in your browser
2.  Right-click on the course description text → “Inspect”
3.  Look for a `class` or `id` attribute that wraps the content
4.  Use the SelectorGadget tool:
    https://rvest.tidyverse.org/articles/selectorgadget.html
5.  Test your selector to make sure it captures all course text

### 2. Add URL builder to `R/add_course_url.R`

Add a case to `case_match()` and create a URL-building function:

``` r
# In add_course_url(), add:
"newuni" ~ add_course_url_newuni(Emnekode, Årstall, Semesternavn),

# Create a helper function for your institution
add_course_url_newuni <- function(course_code, year, semester) {
  semester <- case_match(semester, "Vår" ~ "spring", "Høst" ~ "autumn")
  glue::glue("https://www.newuni.no/courses/{year}/{semester}/{tolower(course_code)}")
}
```

### 3. Test Your Changes

``` r
# Load pipeline and test
courses <- readRDS("data/courses.RDS")
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> select(Emnekode, url, html_success, fulltext) |> head()
result$fulltext[1]  # Inspect first result
```

Expand a little bit at a time, for example by year. Add more years as
the pipeline works.

## Post-Harvest: Anonymization and Deduplication

After harvesting, run `R/run_dedup.R` to anonymize and deduplicate
course plans:

``` r
source("R/run_dedup.R")
```

This runs a three-stage pipeline:

1.  **Anonymize** (`anonymize_fulltext()`): Removes PII (teacher names,
    emails, phone numbers), dates, seasons, and administrative year
    references from `fulltext`, producing a readable `course_plan`
    column. Content years (e.g., “etter 1945”) are preserved.
2.  **Normalize** (`normalize_plan_text()`): Applies lossy transforms
    (lowercasing, heading synonyms, blanket year removal, whitespace
    squishing) on `course_plan` for content hashing
3.  **Deduplicate** (`deduplicate_plans()`): Groups identical normalized
    texts under a shared `plan_content_id`

**Output columns:**

<table>
<colgroup>
<col style="width: 38%" />
<col style="width: 61%" />
</colgroup>
<thead>
<tr>
<th>Column</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>course_plan</code></td>
<td>Anonymized, readable text (primary column for consumers)</td>
</tr>
<tr>
<td><code>fulltext</code></td>
<td>Raw extracted text (kept for debugging)</td>
</tr>
<tr>
<td><code>fulltext_normalized</code></td>
<td>Lossy normalized text (internal, for hashing)</td>
</tr>
<tr>
<td><code>plan_content_id</code></td>
<td>SHA-256 hash of <code>fulltext_normalized</code></td>
</tr>
</tbody>
</table>

**Output files:**

- `data/courses_with_plan_id.RDS` — all course rows with `course_plan` +
  `plan_content_id`
- `data/plan_lookup.RDS` — one row per unique plan per course code per
  institution

## Data Structure

### Input Data (`data/courses.RDS`)

The courses dataset contains:

``` r
courses <- readRDS("data/courses.RDS")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution_short Institusjonskode Institusjonsnavn       Avdelingskode
      <chr>             <chr>            <chr>                  <chr>        
    1 uib               1120             Universitetet i Bergen 220440       
    2 uib               1120             Universitetet i Bergen 220440       
    # ℹ 21 more variables: Avdelingsnavn <chr>, Avdelingskode_SSB <chr>,
    #   Årstall <int>, Semester <int>, Semesternavn <chr>, Studieprogramkode <chr>,
    #   Studieprogramnavn <chr>, Emnekode_raw <chr>, Emnekode <chr>,
    #   Emnenavn <chr>, Nivåkode <chr>, Nivånavn <chr>, Studiepoeng <dbl>,
    #   `NUS-kode` <chr>, Status <int>, Statusnavn <chr>, Underv.språk <chr>,
    #   Navn <chr>, Fagkode <chr>, Fagnavn <chr>, `Oppgave (ny fra h2012)` <int>

Key columns: - `institution_short`: Short code (e.g., “oslomet”,
“uia”) - `Emnekode_raw`: Original course code from DBH - `Emnekode`:
Normalized course code (trailing numbers removed) - `Årstall`: Year -
`Semesternavn`: Semester name (“Vår” or “Høst”) - `Status`: Course
status (1=Active, 2=New, 3=Discontinued, 4=Discontinued but exam
offered)

### Output Data Structure

After processing, you’ll have:

    # A tibble: 2 × 6
      course_id                    url       html  html_success fulltext course_plan
      <chr>                        <chr>     <chr> <lgl>        <chr>    <chr>      
    1 oslomet_ABC123_2024_autumn_1 https://… <htm… TRUE         ABC123 … ABC123 Cou…
    2 oslomet_XYZ456_2024_spring_1 https://… <htm… TRUE         XYZ456 … XYZ456 Ano…

- `course_plan` is the anonymized version of `fulltext` (no names,
  emails, dates, or administrative years). Content years like historical
  references are preserved. Use this column for analysis.

## Key Functions Reference

### `add_course_id(dbh_df)`

Creates unique course identifiers combining institution, code, year,
semester, and status.

### `add_course_url(df)`

Generates the correct URL for each course based on institution-specific
patterns.

### `fetch_html_with_checkpoint(courses, checkpoint_path, .progress = TRUE)`

Downloads HTML from URLs with automatic checkpointing. Skips
already-downloaded courses.

### `harvest_institution(institution_short, courses, year, refetch)`

Main entry point: harvests a single institution using its configured
strategy.

### `harvest_all(courses, year, refetch, institutions)`

Loops through all (or specified) institutions, harvests each, saves
results.

### `validate_courses(df, stage)`

Validates that required columns exist at each pipeline stage: -
`"initial"`: institution_short, Emnekode, Årstall - `"with_url"`:
institution_short, course_id, url - `"with_html"`: institution_short,
course_id, url, html, html_success

## Troubleshooting

**URLs look wrong?** - Check your `url_pattern` - Print a few URLs to
verify: `courses |> select(url) |> head()`

**HTML download fails?** - Check if the website is accessible - Look at
`courses$html_error` for error messages - Some institutions may block
automated requests

**Extracted text is empty or wrong?** - Verify your CSS selector using
browser dev tools - Update selector in `R/institution_config.R` if
website structure changed - Some pages may have different structures for
different years

**Checkpoint file is huge?** - This is normal - HTML is large -
Checkpoint files are in `.gitignore` and won’t be committed

## File Organization

    ├── R/
    │   ├── harvest.R              # Entry points: harvest_institution(), harvest_all()
    │   ├── harvest_strategies.R   # Strategy implementations
    │   ├── institution_config.R   # Institution registry (strategy, selectors, overrides)
    │   ├── utils.R                # Helper functions (add_course_id, validation)
    │   ├── add_course_url.R       # URL generation logic
    │   ├── resolve_course_urls.R  # URL discovery for USN, UiT, etc.
    │   ├── fetch_html_cols.R      # HTML downloading
    │   ├── extract_fulltext.R     # Config-driven text extraction
    │   ├── checkpoint.R           # Checkpoint management
    │   ├── anonymize.R            # PII removal: fulltext → course_plan
    │   ├── normalize_plan_text.R  # Lossy normalization for dedup hashing
    │   ├── deduplicate_plans.R    # Groups identical plans by content hash
    │   └── run_dedup.R            # Entry point for anonymize + dedup pipeline
    ├── data/
    │   ├── courses.RDS            # Input course data
    │   └── checkpoint/            # Checkpoint files (not in git)
    └── data-raw/
        └── courses.R              # script that creates courses.RDS

## Need Help?

Look at the existing institution examples, but be quick to contact me,
also! 🤩
