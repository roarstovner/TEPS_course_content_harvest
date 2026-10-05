# TEPS Course Content Harvest - User Guide


## Overview

This project harvests course descriptions from Norwegian teacher
education institutions. It takes course codes, years, and semesters as
input and produces structured data containing URLs, HTML, and extracted
text from course webpages.

**Input:** A tibble as output by `rdbhapi` with course metadata
(institution, course code, year, semester)

**Output:** The same tibble with added course URL, raw HTML, and
extracted text

### Pipeline Stages

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Stage</th>
<th>Code</th>
<th>Output</th>
</tr>
</thead>
<tbody>
<tr>
<td>1. Course id
<code>{institution}_{Emnekode_raw}_{year}_{semester}_{status}</code></td>
<td><code>add_course_id()</code> (<code>R/utils.R</code>)</td>
<td><code>course_id</code></td>
</tr>
<tr>
<td>2. URL from metadata (institution-specific builders; NA where the
URL must be discovered)</td>
<td><code>add_course_url()</code> (<code>R/add_course_url.R</code>)</td>
<td><code>url</code></td>
</tr>
<tr>
<td>3. URL discovery (USN, UiT, hivolda; optional)</td>
<td><code>resolve_course_urls()</code>
(<code>R/resolve_course_urls.R</code>)</td>
<td><code>url</code>, checkpoint
<code>data/raw/checkpoint/urls_{inst}.RDS</code></td>
</tr>
<tr>
<td>4. Fetch HTML with checkpointing</td>
<td><code>fetch_html_with_checkpoint()</code>
(<code>R/checkpoint.R</code>, <code>R/fetch_html_cols.R</code>)</td>
<td><code>html</code>, <code>html_success</code>,
<code>html_error</code></td>
</tr>
<tr>
<td>5. Extract text (the page’s blocks as text, USN cleanup, PDF
text)</td>
<td><code>extract_fulltext_from_raw()</code>
(<code>R/extract_fulltext.R</code>)</td>
<td><code>data/raw/html_{inst}.RDS</code> (raw harvest)</td>
</tr>
<tr>
<td>6. Read each stored page into blocks; the text is the blocks</td>
<td><code>institution_blocks()</code>,
<code>institution_fulltext()</code> (<code>R/pipeline.R</code>)</td>
<td><code>extracted_text.RDS</code></td>
</tr>
<tr>
<td>7. Anonymize and deduplicate</td>
<td><code>institution_plans()</code></td>
<td><code>course_plans.RDS</code>,
<code>course_offerings*.RDS</code></td>
</tr>
<tr>
<td>8. Split each plan into sections</td>
<td><code>institution_sections()</code></td>
<td><code>plan_sections.RDS</code></td>
</tr>
</tbody>
</table>

Stages 1-5 are run per institution by `harvest_institution()`, which
dispatches to a strategy in `R/harvest_strategies.R` (`standard`,
`url_discovery`, `shadow_dom`, `html_pdf_discovery`, `pdf_split`,
`json_extract`, `noop`). `R/institution_config.R` is the single source
of truth for each institution: strategy, the part of the page that holds
the plan (`selector`, minus `exclude`), `year_in_url`, pre/post
functions, fetch overrides and the `section_*` fields. Stages 6-8 and
everything after them are a {targets} pipeline (see “Rebuilding Derived
Data”).

`data/raw/html_{inst}.RDS` is the raw harvest and only harvesting writes
it. The steps after it rebuild everything from it with the current code,
so a changed selector or post function needs `targets::tar_make()`, not
a new harvest. A page is read once into blocks (see “Blocks and
Sections”), and both the `extracted_text` and the sections are made from
them. The `extracted_text` saved with the harvest is not used
downstream, except for plans that came from a PDF (uis archive plans,
steiner): the PDF is not kept, so that text is the raw data.

## Quick Start: Running the Pipeline

The main entry points are in `R/harvest.R`:

``` r
# Load all pipeline functions
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/section_heading_map.R")
source("R/blocks.R")
source("R/institution_config.R")
source("R/checkpoint.R")
source("R/harvest_strategies.R")
source("R/harvest.R")

# Harvest a single institution
courses <- readRDS("data/input/courses.RDS")
result <- harvest_institution("hivolda", courses)
saveRDS(result, "data/raw/html_hivolda.RDS")

# Only one year, or ignore the checkpoints and fetch again
result <- harvest_institution("oslomet", courses, year = 2025)
result <- harvest_institution("ntnu", courses, refetch = TRUE)

# Or harvest all institutions at once (saves data/raw/html_{inst}.RDS)
harvest_all()
```

The source order matters: `R/institution_config.R` refers to functions
defined in `R/fetch_html_cols.R` and `R/extract_fulltext.R`.

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

- Already-downloaded HTML is saved in
  `data/raw/checkpoint/html_{institution}.RDS` (`course_id`, `html`,
  `html_success`, `html_error`)
- Re-running the script fetches only courses not in the checkpoint
  (anti-join by `course_id`)
- This saves time and is polite to institutional servers

### Rebuilding Derived Data

Everything after the harvest is a {targets} pipeline: `_targets.R` lists
the steps, `R/pipeline.R` holds their functions. Run it after a harvest
or after changing code or config:

``` r
targets::tar_make()               # rebuild whatever is outdated
targets::tar_outdated()           # what would be rebuilt, without building
targets::tar_read(metrics_check)  # changes against the metrics snapshot
targets::tar_read(unmapped)       # headings the section extractor could not map
targets::tar_read(privacy_check)  # personal data found in data/processed/
```

{targets} keeps each step’s result in `_targets/` (gitignored) together
with a hash of its inputs and of the code it calls, and reruns a step
only when one of those changed. The steps per institution (fulltext,
plans, sections, unmapped headings) run once per institution: a new
`data/raw/html_uib.RDS`, or a change to uib’s entry in
`R/institution_config.R`, rebuilds only uib, while a change to shared
code (`R/anonymize.R`, `R/section_heading_map.R`,
`R/extract_sections.R`) reruns that step for every institution. The
pipeline writes the data files in `data/interim/` and `data/processed/`
(see “Data Files: Published and Internal”),
`app/course_browser/data/browser_data.RDS`, the OJS Parquet files and
`data/data_notes.md`. The harvest is not part of it.

The steps run on 4 local worker processes ({crew}, set in
`tar_option_set()` in `_targets.R`), several institutions at a time. A
full build takes about 19 minutes; the floor is the slowest single
institution’s sections (about 8 minutes). To debug a step with
`browser()`, run in the current session instead:
`targets::tar_make(callr_function = NULL, use_crew = FALSE)`.

The pipeline ends by comparing the built data with a snapshot of
per-institution and per-section counts and median text lengths
(`R/pipeline_metrics.R`, `tests/snapshots/pipeline_metrics.csv`).
Changes beyond tolerance show as a warning in `tar_make()` and make
`tests/testthat/test-pipeline-metrics.R` fail. When a change is
intended, update the snapshot and commit it together with the change:

``` r
source("R/pipeline_metrics.R")
check_pipeline_metrics(update = TRUE)
```

Per-institution prose notes are edited directly in
`data/data_notes.qmd`.

### Packages and Tests

Package versions are pinned with renv: `renv.lock` records R 4.6.1 and
every package the code uses, and R started in this folder loads them
from the project library in `renv/` (activated by `.Rprofile`). On a new
machine, `renv::restore()` installs them. After adding a package, record
it with `renv::snapshot()` and commit `renv.lock`.

The tests run with `testthat::test_dir("tests/testthat")`, and on GitHub
Actions for every push and pull request to main
(`.github/workflows/tests.yml`, packages from `renv.lock`). Tests that
need the harvested or built data (only `data/input/` is in git), a
browser or external URLs skip themselves there.

## Adding a New Institution

To add support for a new institution, you need to modify **two files**.
In Claude Code, the `/add-institution` skill walks through the same
steps.

### 1. Add config entry to `R/institution_config.R`

``` r
# Add to the institution_configs list:
newuni = list(
  code = "1234",
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # the element that holds the course plan
  exclude = ".contact",           # optional: parts of it that are not the plan
  year_in_url = TRUE,
  section_strategy = "html"       # how the plan is split; see "Blocks and Sections"
)
```

**How to find CSS selectors:**

1.  Open a course page in your browser
2.  Right-click on the course description text → “Inspect”
3.  Look for a `class` or `id` attribute that wraps the content
4.  Use the SelectorGadget tool:
    https://rvest.tidyverse.org/articles/selectorgadget.html
5.  Test your selector to make sure it captures all course text

`selector` names one element (the first match is used); choose the
narrowest element that holds the whole plan, and leave out navigation,
contact boxes (staff names) and widgets inside it with `exclude` (uis,
mf, hivolda do this).

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
courses <- readRDS("data/input/courses.RDS")
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> select(Emnekode, url, html_success, extracted_text) |> head()
result$extracted_text[1]  # Inspect first result
```

Expand a little bit at a time, for example by year. Add more years as
the pipeline works.

For standard institutions no changes to `R/extract_fulltext.R` or
`R/harvest_strategies.R` are needed. Sites that need a browser (Shadow
DOM), PDF splitting or URL discovery need a strategy function in
`R/harvest_strategies.R`.

## Post-Harvest: Anonymization and Deduplication

The pipeline anonymizes and deduplicates the course plans in three
stages:

1.  **Anonymize** (`anonymize_text()`): Removes PII (teacher names,
    emails, phone numbers, staff lists “Name (Role)”, signature lines
    “Name, dekan”), dates, seasons, and administrative year references
    (“Opprettet 2020”, “2023/2024”) from `extracted_text`, producing a
    readable `course_plan` column. Content years (e.g., “etter 1945”,
    “NOU 2015:2”) are preserved. Institution-specific handlers
    (`.anon_*()`) run first, then the generic cleanup
    (`.anon_generic()`).
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
<td><code>extracted_text</code></td>
<td>Raw extracted text (kept for debugging)</td>
</tr>
<tr>
<td><code>course_plan_normalized</code></td>
<td>Lossy normalized text (internal, for hashing)</td>
</tr>
<tr>
<td><code>plan_content_id</code></td>
<td>SHA-256 hash of <code>course_plan_normalized</code></td>
</tr>
</tbody>
</table>

**Output files:**

- `data/processed/course_offerings.RDS` — published slim dataset: course
  rows with DBH metadata + `plan_content_id` FK (no url, no text)
- `data/interim/course_offerings_full.RDS` — internal working file: same
  rows plus `url`, `extracted_text`, `course_plan`,
  `course_plan_normalized`; used by the course_browser app
- `data/processed/course_plans.RDS` — one row per unique plan per course
  code per institution, with the offering its text comes from
  (`source_course_id`)

## Post-Harvest: Section Extraction

The pipeline splits each course plan into its parts and writes
`data/processed/plan_sections.RDS` with one row per plan and section
(`plan_content_id`, `institution`, `Emnekode`, `source_course_id`,
`section`, `text`), keyed like `course_plans.RDS`. A plan’s sections are
cut from the page of `source_course_id`, the offering whose text the
plan keeps, so they match its `course_plan`. The seven sections are
`course_content`, `learning_outcomes`, `teaching_methods`, `assessment`,
`coursework_requirements`, `prerequisites` and `reading_list`. Section
text is anonymized with the same `anonymize_text()` as `course_plan`:
sections are cut from the raw `html`/`extracted_text`, so this step must
keep anonymizing. Admission text, exam logistics and placeholder rows
such as “Se fagplanen.” are left out.

`tar_read(metrics)` has the number of plans with each section per
institution, and `tar_read(unmapped)` the headings the extractor could
not map, with the number of pages each is on. To map a new heading, add
a pattern to `R/section_heading_map.R`; how each institution is split is
set by the `section_*` fields in `R/institution_config.R`.

### Blocks and Sections

Each page is read once into a **block table** (`R/blocks.R`): one row
per heading, sub-heading or piece of text, in document order, with the
section a heading maps to. `sectionize()` (`R/extract_sections.R`) then
walks the blocks with the same rules for every institution: a heading
opens its section (an unmapped one closes it), a sub-heading switches
section only inside a mapped one, and text goes to the open section.
What differs between institutions is how their pages are read, set by
`section_strategy` and the other `section_*` fields in
`R/institution_config.R`:

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th><code>section_strategy</code></th>
<th>Institutions</th>
<th>Reads</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>html</code></td>
<td>oslomet, uia, ntnu, inn, hiof, hivolda, hvl, mf, nord, nih, uib,
uio, uis, uit, nmbu</td>
<td>The DOM under <code>selector</code>. Falls back to <code>text</code>
when it finds &lt; 3 sections</td>
</tr>
<tr>
<td><code>json</code></td>
<td>nla</td>
<td>Titles and contents in the embedded EmneplanPage JSON</td>
</tr>
<tr>
<td><code>text</code></td>
<td>usn, steiner (+ the fallback, e.g. uis PDF plans)</td>
<td>Heading-shaped lines in <code>extracted_text</code>; skips a table
of contents (“Innholdsfortegnelse”, usn); inside a reading list opened
by a whole heading only a whole heading switches section (book
titles)</td>
</tr>
<tr>
<td><code>noop</code></td>
<td>samas</td>
<td>—</td>
</tr>
</tbody>
</table>

Fields for the `html` reader:

- `selector` and `exclude` (shared with the fulltext): the container,
  and elements in it that are not read (mf’s facts box, contact card and
  banner; nord’s “Kopier lenke” label).
- `section_heading_selector`: section headings (default `h2`; ntnu
  `"h2, h3"` for its “Eksamen” block, inn’s `div.label`, uib’s and mf’s
  `summary`, nord’s `button.ac-trigger`).
- `section_scope`: elements that hold a section of their own (`details`,
  nord’s `div.ac`); the section open around them continues after them.
- `section_initial`: section for text before the first heading (mf’s
  untitled intro is course_content).
- `section_fields`: hivolda’s Drupal fields (`div.field-<name>`) are the
  headings, each mapped to a section by its class name; text outside the
  fields (title, programmes) is in the fulltext but in no section.
- `section_subheading_selector`: elements inside a section that switch
  to another section (uio `"h3, h4, p"`; `"p"` for uia, oslomet, hiof,
  hvl, nmbu; mf’s intro paragraphs). A `<p>` counts if its whole text
  (minus a trailing colon) equals a heading pattern, or if its leading
  `<em>`/`<strong>` run or its first or last `<br>`-separated line does
  (uia `<p><em>Faget i praksis</em>I løpet …</p>`). A whole-bold `<p>`
  that names no section is a group label: it ends a sub-section and
  returns to the parent. Sub-headings act only under a mapped heading
  (oslomet’s programme “Fagplan” block stays out); one naming the
  section already open (“Kunnskap”) stays as content. A `<p>` in a list
  item is skipped unless that `<li>` holds a section heading (oslomet’s
  accordion). An unmapped `h3` (“Karakterskala”) hands its text back to
  the parent section, keeping the heading as its first line. A
  colon-ended lead-in naming a coursework gate (“… følgende
  obligatoriske aktiviteter:”) starts coursework_requirements.

Other fields:

- `section_text_header`: regex for a title + metadata block at the top
  of `extracted_text` (uis PDFs: “Emnekode:”, “Tilbys av:”); the `text`
  reader skips it and files the untitled paragraph after it as
  course_content.
- `section_inline_coursework`: move “Arbeidskrav (AK): …” /
  “Obligatorisk deltakelse …” lines from assessment to
  coursework_requirements (nord).

### Heading Map and Cleanup

`match_heading_to_section()` (`R/section_heading_map.R`) checks exact
equality against every pattern first, then substring patterns in table
order (first hit wins; rows with `exact = TRUE` only match whole
headings). The `text` reader passes `word_start = TRUE` so
“elevkunnskap” does not match “kunnskap”, and only accepts
heading-shaped lines (capitalised, ≤ 8 words, no digits, no “Label:
value”, no closing full stop). Patterns mapped to `".drop"` end the
current section and their text is discarded: admission headings (“Opptak
til emnet”, “Opptakskrav”, “Hvem kan ta dette emnet?”) and exam
logistics (“Mer om eksamen ved UiO”, “Hjelpemidler”, “Sensorordning”,
resit headings such as “Ny/utsatt eksamen”). Exam language and grading
scale stay in assessment.

HTML is parsed by `.read_doc()`, which drops `script`, `style`, `select`
and `label` (ntnu script text, uib semester picker); the `json` reader
parses the page itself because its data is in a `<script>`.

`.clean_sections()` removes `.drop` rows, strips notices and page
widgets from assessment/coursework (`.section_noise`: plagiarism and
ChatGPT/COVID notices, uib banner and footer, flattened exam-table
headers, …), removes admission sentences from prerequisites
(`.strip_admission_lines()`), drops placeholder-only rows
(`.placeholder_phrases`: “Ingen”, “Se fagplanen.”, “-”, Leganto
pointers, “Oppgis senere.”, “… ikke publisert ennå”), and drops a first
line that only repeats the section heading (but keeps learning-outcome
group labels such as “Kunnskap”).

## Auditing a Pipeline Step

The `/audit-institutions <sections|fulltext|anonymization> [inst ...]`
skill (Claude Code) checks a pipeline step for every institution with
one review agent per institution. R scripts build a review packet per
institution (`R/audit/prepare_{check}.R`; for sections after the
deterministic pre-pass `R/audit/qa_sections.R`), the agents write
findings to `data/audit/{check}/findings/{inst}.json`, and
`R/audit/aggregate.R` verifies them (course ids and verbatim quotes must
be in the packet) and writes a ranked cross-institution report. Details:
`.claude/skills/audit-institutions/SKILL.md`.

## Data Files: Published and Internal

Files that hold raw page text (`html`, `extracted_text`) contain staff
names, e-mail addresses and phone numbers and are never published.
Everything that is shared is built from the anonymized text
(`anonymize_text()`). The data are kept in one folder per stage, so the
folder says what may be shared:

<table>
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th>Folder</th>
<th>Holds</th>
<th>Written by</th>
<th>In git</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>data/input/</code></td>
<td>the DBH course list, the pipeline input</td>
<td><code>data-raw/courses.R</code></td>
<td>yes</td>
</tr>
<tr>
<td><code>data/raw/</code></td>
<td>the harvest, with personal data</td>
<td>harvesting only</td>
<td>no</td>
</tr>
<tr>
<td><code>data/interim/</code></td>
<td>text rebuilt from the harvest, with personal data; never shared</td>
<td><code>targets::tar_make()</code></td>
<td>no</td>
</tr>
<tr>
<td><code>data/processed/</code></td>
<td>anonymized data or data without text; can be shared</td>
<td><code>targets::tar_make()</code></td>
<td>no</td>
</tr>
</tbody>
</table>

`data/processed/` may only hold text that went through
`anonymize_text()`. The pipeline checks this on every build: target
`privacy_check` scans the files for e-mail addresses, phone numbers,
“Name (Role)” lines and “Name, dekan” signatures and warns, and
`tests/testthat/test-anonymize.R` fails, on any hit. The repository is
public.

<table>
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th>File</th>
<th>Contents</th>
<th>Personal data</th>
<th>Status</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>data/input/courses.RDS</code></td>
<td>DBH course metadata (pipeline input)</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/raw/html_{inst}.RDS</code>,
<code>data/raw/checkpoint/</code></td>
<td>raw harvest: <code>html</code> (+ <code>extracted_text</code> from
harvest time)</td>
<td>yes (raw)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/interim/extracted_text.RDS</code></td>
<td><code>extracted_text</code> rebuilt from the raw harvest</td>
<td>yes (raw)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/interim/course_offerings_full.RDS</code></td>
<td>offerings + <code>url</code>, <code>extracted_text</code>,
<code>course_plan</code></td>
<td>yes (<code>extracted_text</code>)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/processed/course_offerings.RDS</code></td>
<td>DBH metadata + <code>plan_content_id</code>, no text</td>
<td>none</td>
<td>published</td>
</tr>
<tr>
<td><code>data/processed/course_plans.RDS</code></td>
<td>anonymized <code>course_plan</code> (+
<code>course_plan_normalized</code>)</td>
<td>anonymized</td>
<td>published</td>
</tr>
<tr>
<td><code>data/processed/plan_sections.RDS</code></td>
<td>anonymized section <code>text</code> per plan</td>
<td>anonymized</td>
<td>publishable</td>
</tr>
<tr>
<td><code>app/course_browser/data/browser_data.RDS</code></td>
<td>course_browser payload: plans, offering metadata, sections</td>
<td>anonymized (no <code>extracted_text</code>)</td>
<td>internal (app)</td>
</tr>
<tr>
<td><code>app/course_browser_ojs/data/*.parquet</code></td>
<td>anonymized <code>course_plan</code> + slim offering metadata</td>
<td>anonymized</td>
<td>published with the OJS page (build artifact)</td>
</tr>
<tr>
<td><code>data/audit/</code></td>
<td>audit findings (quotes from anonymized text)</td>
<td>staff names redacted</td>
<td>in git</td>
</tr>
</tbody>
</table>

The scan catches text that skipped the anonymizer, not names the
anonymizer misses; the anonymization audit
(`/audit-institutions anonymization`) checks that per institution.

## Data Structure

### Input Data (`data/input/courses.RDS`)

The courses dataset contains:

``` r
courses <- readRDS("data/input/courses.RDS")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution Institusjonskode Institusjonsnavn Avdelingskode Avdelingsnavn     
      <chr>       <chr>            <chr>            <chr>         <chr>             
    1 samas       0217             Samisk høgskole  480000        Avdeling for duod…
    2 samas       0217             Samisk høgskole  480000        Avdeling for duod…
    # ℹ 20 more variables: Avdelingskode_SSB <chr>, Årstall <int>, Semester <int>,
    #   Semesternavn <chr>, Studieprogramkode <chr>, Studieprogramnavn <chr>,
    #   Emnekode_raw <chr>, Emnekode <chr>, Emnenavn <chr>, Nivåkode <chr>,
    #   Nivånavn <chr>, Studiepoeng <dbl>, `NUS-kode` <chr>, Status <int>,
    #   Statusnavn <chr>, Underv.språk <chr>, Navn <chr>, Fagkode <chr>,
    #   Fagnavn <chr>, `Oppgave (ny fra h2012)` <int>

Key columns: - `institution`: Short code (e.g., “oslomet”, “uia”) -
`Emnekode_raw`: Original course code from DBH (used in `course_id`) -
`Emnekode`: Course code with the trailing version number removed
(`canon_remove_trailing_num()`: “ABC123-1” → “ABC123”) - `Årstall`:
Year - `Semesternavn`: Semester name (“Vår” or “Høst”;
`canon_semester_name()` maps Vår/Høst/Sommer to spring/autumn/summer) -
`Status`: Course status (1=Active, 2=New, 3=Discontinued, 4=Discontinued
but exam offered)

DBH lists most courses under both semesters of a year, also when they
are taught in one. Where pages are semester-specific, about half the
rows therefore have no plan by design; see “Semester registration in
DBH” in `data/data_notes.qmd` before reading row-level success rates.

### Output Data Structure

After processing, you’ll have:

    # A tibble: 2 × 6
      course_id                  url   html  html_success extracted_text course_plan
      <chr>                      <chr> <chr> <lgl>        <chr>          <chr>      
    1 oslomet_ABC123_2024_autum… http… <htm… TRUE         ABC123 Course… ABC123 Cou…
    2 oslomet_XYZ456_2024_sprin… http… <htm… TRUE         XYZ456 Anothe… XYZ456 Ano…

- `course_plan` is the anonymized version of `extracted_text` (no names,
  emails, dates, or administrative years). Content years like historical
  references are preserved. Use this column for analysis.

## Institution-Specific Notes

**NORD.** The URL needs Norwegian characters in the semester parameter
(`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic “Gjeldende
emnebeskrivelse” page without year-specific content. The site maps each
semester to its academic year (`VÅR&year=2023` gives the 2022/23 plan).
Pattern:
`https://www.nord.no/studier/emner/{code}?year={year}&semester={HØST|VÅR}`.

**NTNU.** The URL has the year but not the semester. A page saying no
information is available raises `ntnu_no_info_error`. robots.txt asks
for a 10-second crawl delay (`request_delay` in the config).

**UiO.** UiO publishes only the latest version of each course plan: the
base URL always returns the current plan and older versions cannot be
reached. Semester URLs (`/h24/`, `/v25/`) hold logistics only (teachers,
timetable, exam dates), not the plan, so they are not used. Data should
be filtered to the current year. The URL needs faculty/department slugs
derived from `Avdelingsnavn`, via a hard-coded mapping in
`add_course_url_uio()`. Pattern:
`https://www.uio.no/studier/emner/{faculty}/{inst}/{CODE}/`.

**HiOF.** Courses before autumn 2021 use a different URL structure
(switch in `add_course_url_hiof()`); the config selector lists fallbacks
for the old and new page layouts.

**USN.** The URL holds a version number that metadata cannot give:
`https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`.
Each version (1, 2, 3, …) is a revision valid for a range of years, and
an invalid version/year combination silently shows another page without
changing the URL. `resolve_urls_usn_batch()` therefore renders each
candidate in Chrome (`rvest::read_html_live()`, hash routing) and checks
that the displayed “Undervisningsstart” year matches. It tries versions
1-5 and stops early when the course code is missing from the page (no
such version) or version 1 starts after the requested year (course did
not exist yet); NA means no version exists for that semester. The
content is rendered inside Shadow DOM, which `html_text()` cannot read;
`read_usn_live_html(session)` walks the shadow roots with JavaScript:

``` r
session <- rvest::read_html_live(url)
Sys.sleep(5)  # wait for JS rendering
content <- read_usn_live_html(session)
session$session$close()
```

The batch resolver reuses one Chrome session and navigates by changing
`window.location.hash`, which avoids a 2-3 s browser start per URL. HTML
is captured during resolution, so USN has no separate fetch step.

**UiT.** Historical plans need a document id:
`https://uit.no/utdanning/emner/emne?p_document_id={ID}`. The active
page (`/utdanning/aktivt/emne/{CODE}`) has a `<select>` listing every
semester with its document id (option text `{CODE}: H 2020` /
`{CODE}: V 2021`), also for discontinued courses. The same CSS selector
works for active and historical pages.

**Samas.** `noop` strategy: `extracted_text` is NA, since the plans are
in Sámi.

## Key Functions Reference

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Function</th>
<th>File</th>
<th>Purpose</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>harvest_institution(institution, courses, year, refetch)</code></td>
<td><code>R/harvest.R</code></td>
<td>Harvest one institution with its configured strategy</td>
</tr>
<tr>
<td><code>harvest_all(courses, year, refetch, institutions)</code></td>
<td><code>R/harvest.R</code></td>
<td>Harvest all (or the given) institutions, save
<code>data/raw/html_{inst}.RDS</code></td>
</tr>
<tr>
<td><code>get_institution_config(inst)</code></td>
<td><code>R/institution_config.R</code></td>
<td>Config list for one institution</td>
</tr>
<tr>
<td><code>add_course_id(dbh_df)</code></td>
<td><code>R/utils.R</code></td>
<td>Unique id from institution, raw code, year, semester, status</td>
</tr>
<tr>
<td><code>add_course_url(df)</code></td>
<td><code>R/add_course_url.R</code></td>
<td>Institution-specific URL builders; NA where discovery is needed</td>
</tr>
<tr>
<td><code>resolve_course_urls(df, checkpoint_path)</code></td>
<td><code>R/resolve_course_urls.R</code></td>
<td>URL discovery (USN, UiT, hivolda) with checkpointing</td>
</tr>
<tr>
<td><code>fetch_html_with_checkpoint(courses, checkpoint_path)</code></td>
<td><code>R/checkpoint.R</code></td>
<td>Download HTML, skipping courses already in the checkpoint</td>
</tr>
<tr>
<td><code>extract_fulltext_from_raw(df, config)</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td><code>extracted_text</code> from the raw harvest, per strategy</td>
</tr>
<tr>
<td><code>page_blocks(html, text, cfg)</code></td>
<td><code>R/blocks.R</code></td>
<td>Read one page into a block table</td>
</tr>
<tr>
<td><code>page_fulltext(blocks, config)</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td>A page’s <code>extracted_text</code> from its blocks</td>
</tr>
<tr>
<td><code>read_harvest(institutions)</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td>Raw harvest joined with
<code>data/interim/extracted_text.RDS</code></td>
</tr>
<tr>
<td><code>validate_courses(df, stage)</code></td>
<td><code>R/utils.R</code></td>
<td>Required columns at <code>"initial"</code>, <code>"with_url"</code>,
<code>"with_html"</code></td>
</tr>
<tr>
<td><code>anonymize_text(institution, text)</code></td>
<td><code>R/anonymize.R</code></td>
<td>PII and admin-date removal for <code>course_plan</code> and
sections</td>
</tr>
<tr>
<td><code>normalize_plan_text(course_plan)</code></td>
<td><code>R/normalize_plan_text.R</code></td>
<td>Lossy normalization for dedup hashing</td>
</tr>
<tr>
<td><code>deduplicate_plans(df)</code></td>
<td><code>R/deduplicate_plans.R</code></td>
<td>Content hashes → <code>plans</code> and <code>courses</code> with
<code>plan_content_id</code></td>
</tr>
<tr>
<td><code>page_sections(blocks, text, cfg)</code></td>
<td><code>R/extract_sections.R</code></td>
<td>A page’s sections from its blocks</td>
</tr>
<tr>
<td><code>extract_sections(config, html, extracted_text, course_id)</code></td>
<td><code>R/extract_sections.R</code></td>
<td><code>page_sections()</code> for many pages</td>
</tr>
</tbody>
</table>

## Troubleshooting

**URLs look wrong?** - Check the institution’s builder in
`R/add_course_url.R` - Print a few:
`courses |> filter(institution == "inst") |> add_course_url() |> select(url) |> head()`

**HTML download fails?** - Check if the website is accessible in a
browser - Look at `html_error` for error messages - Some institutions
rate-limit or block automated requests - For NTNU, check whether the
page says no information is available

**Extracted text is empty or wrong?** - Verify the CSS selector with
browser dev tools on a real course page - Update the selector in
`R/institution_config.R` if the website changed, then
`targets::tar_make()` (no new harvest needed) - Read one page into
blocks and look at them:
`page_blocks(html, NA, .block_cfg(get_institution_config("inst")))` -
Some pages have different structures for different years

**Content missing from JavaScript-rendered pages?** - Shadow DOM content
is invisible to `html_text()`; see USN above -
`session$session$Runtime$evaluate("...")` runs JavaScript in a live
session - `session$view()` opens the browser to see what is rendered

**Checkpoint file is huge?** - This is normal - HTML is large -
Checkpoint files are in `.gitignore`; deleting one makes the next run
fetch again

## File Organization

    ├── _targets.R                 # Post-harvest pipeline: targets::tar_make()
    ├── renv.lock                  # Pinned package versions (renv::restore())
    ├── .github/workflows/tests.yml # Tests on GitHub Actions
    ├── R/
    │   ├── harvest.R              # Entry points: harvest_institution(), harvest_all()
    │   ├── harvest_strategies.R   # Strategy implementations
    │   ├── institution_config.R   # Institution registry (strategy, selectors, overrides, section_*)
    │   ├── utils.R                # Helpers (data paths, add_course_id, validation, normalization)
    │   ├── add_course_url.R       # URL generation logic
    │   ├── resolve_course_urls.R  # URL discovery for USN, UiT, hivolda
    │   ├── fetch_html_cols.R      # HTML downloading (httr2)
    │   ├── extract_fulltext.R     # Config-driven text extraction
    │   ├── checkpoint.R           # Checkpoint management
    │   ├── anonymize.R            # PII removal: anonymize_text() for course_plan and sections
    │   ├── normalize_plan_text.R  # Lossy normalization for dedup hashing
    │   ├── deduplicate_plans.R    # Groups identical plans by content hash
    │   ├── section_heading_map.R  # Heading → section patterns (incl. ".drop")
    │   ├── extract_sections.R     # Section extraction strategies + cleanup
    │   ├── pipeline.R             # Steps of the {targets} pipeline (_targets.R)
    │   ├── pipeline_metrics.R     # Regression snapshot: check_pipeline_metrics()
    │   └── audit/                # Audit harness for /audit-institutions
    ├── app/
    │   ├── course_browser/        # Shiny concordance browser; build_data.R → data/ (see its README)
    │   └── course_browser_ojs/    # Static Observable JS browser; build_data.R → data/ (see its README)
    ├── data/                      # README "Data Files: Published and Internal"
    │   ├── input/courses.RDS      # DBH course list, the pipeline input (in git)
    │   ├── raw/                   # Harvest: html_{inst}.RDS, checkpoint/ (not in git)
    │   ├── interim/               # Rebuilt text with personal data (not in git)
    │   ├── processed/             # Anonymized data files, can be shared (not in git)
    │   ├── audit/                 # Audit findings and reports
    │   └── data_notes.qmd         # Data quality notes per institution
    ├── tests/
    │   ├── testthat/              # Rscript -e 'testthat::test_dir("tests/testthat")'
    │   └── snapshots/             # pipeline_metrics.csv (regression snapshot)
    └── data-raw/
        └── courses.R              # script that creates data/input/courses.RDS

## Need Help?

Look at the existing institution examples, but be quick to contact me,
also! 🤩
