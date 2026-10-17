# TEPS Course Content Harvest - User Guide


## Overview

This project harvests the course descriptions of Norwegian higher
education institutions: every course DBH lists at the institutions with
a config, with teacher education as one frame of them (see “Frames”). It
takes course codes, years, and semesters as input and produces
structured data containing URLs, HTML, and extracted text from course
webpages.

**Input:** A dated snapshot of DBH’s course list (`dbh_courses()`, see
“Input Data”) with course metadata (institution, course code, year,
semester)

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
<code>{institution}_{course_code_raw}_{year}_{semester}_{status}</code></td>
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
<td>3. URL discovery (USN, UiT, hivolda, UiO; optional)</td>
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
<td><code>data/raw/{inst}/{year}/{run}.parquet</code> (raw harvest)</td>
</tr>
<tr>
<td>6. Read each stored page into blocks, per raw partition; the text is
the blocks</td>
<td><code>partition_blocks()</code>, <code>partition_text()</code>
(<code>R/pipeline.R</code>)</td>
<td><code>extracted_text.RDS</code></td>
</tr>
<tr>
<td>7. Give each offering its page; anonymize, and deduplicate within
each frame</td>
<td><code>read_harvest()</code>, <code>plan_offerings()</code>,
<code>frame_plans()</code></td>
<td><code>course_plans.RDS</code>,
<code>course_offerings*.RDS</code></td>
</tr>
<tr>
<td>8. Split each plan into sections</td>
<td><code>partition_sections()</code>,
<code>frame_sections()</code></td>
<td><code>plan_sections.RDS</code></td>
</tr>
</tbody>
</table>

Stages 1-5 are run per institution by `harvest_institution()`, which
dispatches to a strategy in `R/harvest_strategies.R` (`standard`,
`url_discovery`, `shadow_dom`, `html_pdf_discovery`, `pdf_split`,
`json_extract`, `noop`). `R/institution_config.R` is the single source
of truth for each institution: strategy, the part of the page that holds
the plan (`selector`, minus `exclude`), how a plan’s year is known
(`plan_years`: `"url"`, `"page"` or `"current"`), pre/post functions,
fetch overrides and the `section_*` fields. Stages 6-8 and everything
after them are a {targets} pipeline (see “Rebuilding Derived Data”).

The raw harvest is append-only Parquet: every harvest adds one file per
institution and DBH year, `data/raw/{inst}/{year}/{run}.parquet`, with
what it fetched, and no raw file is written twice (see “Raw Store and
Finalized Releases”). The steps after it rebuild everything from it with
the current code, so a changed selector or post function needs
`targets::tar_make()`, not a new harvest. A page is read once into
blocks (see “Blocks and Sections”), and both the `extracted_text` and
the sections are made from them. The raw store keeps `extracted_text`
only for plans that came from a PDF (uis archive plans, steiner): that
text is the raw data there (uis keeps the PDF too since 2026-10-16).

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

# Harvest a single institution: the rows, not saved (a test)
courses <- dbh_courses("teacher_ed")   # the teacher-ed frame; dbh_courses(): all courses
result <- harvest_institution("hivolda", courses)

# Only one year, or ignore the checkpoints and fetch again
result <- harvest_institution("oslomet", courses, year = 2025)
result <- harvest_institution("ntnu", courses, refetch = TRUE)

# Harvest into the raw store (data/raw/{inst}/{year}/{date}.parquet): all
# institutions, or the given ones; without `courses`, every course of the
# latest DBH snapshot (weeks at the slowest sites)
harvest_all(courses, institutions = "hivolda")
harvest_all()

# Fetch again the offerings without a plan, except pages not found (README
# "Fetch Ledger and Retries"); run targets::tar_make() first
harvest_all(institutions = "uia", year = 2023, retry = TRUE)
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
script stops or crashes, or a site refuses the harvest (HTTP 403 or 429;
see “Fetch Ledger and Retries”):

- Already-downloaded HTML is saved in
  `data/raw/checkpoint/html_{institution}.RDS` (`course_id`, `html`,
  `html_success`, `html_error`)
- Re-running the script fetches only courses not in the checkpoint
  (anti-join by `course_id`)
- This saves time and is polite to institutional servers

The checkpoint only resumes an interrupted harvest; it is a cache, not
the record. Offerings already in a raw file are skipped by
`harvest_all()` itself (see “Raw Store and Finalized Releases”). For
“page” and “current” sites (`plan_years`) the checkpoint is removed
before every harvest, because their pages change over time and an old
copy must not stand in for a new one.

### Rebuilding Derived Data

Everything after the harvest is a {targets} pipeline: `_targets.R` lists
the steps, `R/pipeline.R` holds their functions. Run it after a harvest
or after changing code or config:

``` r
targets::tar_make()               # rebuild whatever is outdated
targets::tar_outdated()           # what would be rebuilt, without building
targets::tar_read(metrics_check)  # changes against the metrics snapshot
targets::tar_read(unmapped)       # headings the section extractor could not map
targets::tar_read(heading_use)    # pages per heading-map pattern (0 = unused)
targets::tar_read(privacy_check)  # personal data found in data/processed/
targets::tar_read(frozen_check)   # changes to the finalized release (empty before one)
targets::tar_read(gaps)           # course-years without a plan, and why
targets::tar_read(coverage)       # every offering of each frame: fetch outcome, plan, retry
targets::tar_read(changed_pages)  # pages whose text changed between harvest runs
```

{targets} keeps each step’s result in `_targets/` (gitignored) together
with a hash of its inputs and of the code it calls, and reruns a step
only when one of those changed. The steps that read pages run once per
raw partition, an institution’s DBH year (`data/raw/{inst}/{year}/`,
`raw_partitions()`), so no step holds more than one partition’s HTML:
`blocks` (every stored page read once), `page_text` (its text,
anonymized plan and plan id), `page_headings` (for `unmapped` and
`heading_use`) and `source_sections` (the sections of the pages plans
are cut from). A new raw file of uib 2025, or a change to uib’s entry in
`R/institution_config.R`, rebuilds only uib’s partitions, while a change
to shared code (`R/anonymize.R`, `R/section_heading_map.R`,
`R/extract_sections.R`) reruns that step for every partition. Which page
each offering takes can cross years (a spring offering reads the
academic year’s page, a current site’s snapshot serves every year), so
that is decided per institution without reading the pages (`pages`:
`read_harvest(content = FALSE)`, which puts the page id,
`{inst}/{year}/{run}/{row}`, in place of the page), and the steps over
all offerings (`courses`, `plans`, `sections`, `offerings`) join the
partitions’ results by page id. The pipeline writes the data files of
each frame (see “Frames”) in `data/interim/` and `data/processed/` (see
“Data Files: Published and Internal”),
`app/course_browser/data/browser_data.RDS`, the OJS Parquet files,
`data/data_notes.md` and `methods.md`. The harvest is not part of it.

`methods.md` (from `methods.qmd`) holds the choices behind the data that
an article’s methods section must report, by topic, with numbers
computed from the data; a decision that changes what the data mean gets
an entry there. `data/interim/plan_gaps.RDS` (target `gaps`) lists every
course-year without a plan with the reason (no URL, 404, fetch error,
page but no plan) and flags the gaps between two years with plans, the
ones worth checking by hand; the data notes summarize it (“Course-years
without a plan”).

The steps run on 4 local worker processes ({crew}, set in
`tar_option_set()` in `_targets.R`), several partitions at a time. A
full build takes about 21 minutes, a build with nothing to do about 10
seconds (2026-10-16). To debug a step with `browser()`, run in the
current session instead:
`targets::tar_make(callr_function = NULL, use_crew = FALSE)`.

The pipeline ends by comparing the built data of each frame with a
snapshot of per-institution and per-section counts and median text
lengths (`R/pipeline_metrics.R`,
`tests/snapshots/pipeline_metrics_{frame}.csv`). Changes beyond
tolerance show as a warning in `tar_make()` (target `metrics_check`,
with the frame) and make `tests/testthat/test-pipeline-metrics.R` fail.
The teacher-ed snapshot stays put; the all-courses one changes with each
harvest. When a change is intended, update the snapshot of the frames it
changed and commit it together with the change:

``` r
source("R/utils.R"); source("R/pipeline_metrics.R")
check_pipeline_metrics(update = TRUE, frames = "all")
```

### Frames

The pipeline builds every offering in the raw store, and writes the data
files of two frames (`FRAMES`): `all`, every harvested offering, in
`data/interim/` and `data/processed/`; and `teacher_ed`, the offerings
in the teacher-ed frame of the latest DBH snapshot
(`dbh_courses("teacher_ed")`, matched on the offering without its
status, `frame_members()`), in the `teacher_ed/` folders of both
(`frame_dir()`). The text, plan id and sections of a page are the same
in both; the plans are deduplicated within each frame (`frame_plans()`),
so a teacher-ed plan has the years, text and sections of its own
earliest teacher-ed offering, whatever else the raw store holds, and a
harvest of other courses leaves the teacher-ed files as they were. The
teacher-ed files are the releases’ (`finalize_release()`,
`release_data()`, target `frozen_check`), the browsers’ and
`methods.md`’s; the data notes, the gaps and the audits read those of
all courses. Each frame has its metrics snapshot, and target `coverage`
its DBH offerings.

Per-institution prose notes are edited directly in
`data/data_notes.qmd`.

### Packages and Tests

Package versions are pinned with renv: `renv.lock` records R 4.6.1 and
every package the code uses, and R started in this folder loads them
from the project library in `renv/` (activated by `.Rprofile`). On a new
machine, `renv::restore()` installs them. Harvesting USN also needs
Chrome or Chromium on the machine, which chromote drives. After adding a
package, record it with `renv::snapshot()` and commit `renv.lock`.

The tests run with `testthat::test_dir("tests/testthat")`, and on GitHub
Actions for every push and pull request to main
(`.github/workflows/tests.yml`, packages from `renv.lock`). Tests that
need the harvested or built data (only `data/input/` is in git), a
browser or external URLs skip themselves there.

## The Yearly Harvest

A new year is harvested with the same code as the first harvest:
`harvest_all()` fetches only the offerings no raw file holds, and
`tar_make()` rebuilds everything from the raw store. Each year, in this
order:

1.  **September-November: the sites that show only the current plan.**
    UiO, MF, NMBU and Steiner replace a plan when the next academic year
    begins, so a year without a harvest has no plans for them (see
    “Sites that show only the current plan”). Harvest them in every
    academic year, whether or not DBH has a new year:

    ``` r
    harvest_all(institutions = c("uio", "mf", "nmbu", "steiner"))
    ```

    This fetches again the pages of every course of the latest DBH year
    in the snapshot. Samas has `plan_years = "current"` too, but fetches
    nothing (`noop`).

2.  **When DBH has the new year: a new DBH snapshot.** DBH has the year
    when this query returns rows (Semester 1 is spring, 3 autumn); until
    then it fails with DBH’s empty answer:

    ``` r
    rdbhapi::dbh_data(208, filters = list("Årstall" = "2026"), variables = "Semester") |>
      dplyr::count(Semester)
    ```

    Then, from the repository root, `Rscript data-raw/courses.R` writes
    `data/input/dbh_courses_{date}.parquet` and
    `dbh_programmes_{date}.parquet` (DBH’s whole tables 208 and 347; the
    download of table 208 is about 700 MB). Commit both. From then on
    `dbh_courses()` and the pipeline read the new snapshot: the
    teacher-ed frame and target `coverage` follow it, and an offering
    already harvested keeps its `course_id` (see “Input Data”).

3.  **Harvest.** `harvest_all()` fetches every offering of the snapshot
    that no raw file holds: the new year, and rows DBH added to earlier
    years since the last snapshot. `harvest_all(year = 2026)` fetches
    the new year only. A site that shows only the current plan gets
    another snapshot, now with the new year’s courses. At the slowest
    sites this takes days, so run institutions side by side (below). Raw
    files exist only on this machine: back up `data/raw/` once the
    harvest is done.

4.  **Rebuild and check.** Run `targets::tar_make()` and read the
    targets of “Rebuilding Derived Data”:

    - `metrics_check`: the all-courses frame changes with every harvest,
      the teacher-ed frame with a new year. Explain the rows in the
      commit and update the snapshots of those frames
      (`check_pipeline_metrics(update = TRUE, frames = ...)`).
    - `frozen_check`: the plans of a new year are not listed. A row (an
      offering that left the teacher-ed frame in the new snapshot, a
      plan a site reworded) is a change to the last release; explain it
      in the commit.
    - `privacy_check`: must have no rows.
    - `unmapped` and `heading_use`: headings of new or changed page
      layouts.
    - `coverage` of the new year by institution, and `changed_pages`.

    Then fetch again the pages that gave no plan (a 404 is not tried
    again; see “Fetch Ledger and Retries”) and rebuild:

    ``` r
    harvest_all(year = 2026, retry = TRUE)
    targets::tar_make()
    ```

    Update `methods.qmd` where it names the snapshot, the years or the
    harvest dates (“Population and sampling frame”, “Harvest”), and
    `data/data_notes.qmd` where a site changed.

5.  **Audit.** In Claude Code, `/audit-institutions fulltext`, then
    `sections` and `anonymization` (see “Auditing a Pipeline Step”), for
    all institutions or those whose site changed. The skill has no year
    option: its packets sample each institution’s courses of all years.
    A finding is fixed in the config or the heading map and rebuilt with
    `tar_make()`, without a new harvest.

6.  **Release.** Tag the commit and finalize the release (see “Raw Store
    and Finalized Releases”):

    ``` r
    # after git tag data-YYYY-MM-DD
    source("R/utils.R"); source("R/release.R")
    finalize_release("data-YYYY-MM-DD")   # then commit tests/snapshots/raw_manifest.csv
    ```

    The hand-over to the coding project,
    `release_data("data-YYYY-MM-DD")` (into
    `../TEPS_course_content_coding/data`), is the project lead’s
    decision and step, because the coders work from that repository.

### Several Institutions at Once

`harvest_all()` takes the institutions one after another. At the slowest
sites a harvest of every course takes days (NTNU and UiB at 10 seconds
between requests, the Crawl-delay of NTNU’s robots.txt), so run several
institutions side by side, each in its own R process, with the
institution or institutions it harvests in `institutions`. From the
repository root, with a log per process:

``` sh
mkdir -p tmp/harvest_logs
for inst in ntnu uib uio; do
  nohup Rscript -e "for (f in c('R/utils.R', 'R/add_course_url.R', 'R/resolve_course_urls.R',
    'R/fetch_html_cols.R', 'R/extract_fulltext.R', 'R/section_heading_map.R', 'R/blocks.R',
    'R/institution_config.R', 'R/checkpoint.R', 'R/harvest_strategies.R', 'R/harvest.R')) source(f)
    harvest_all(institutions = '$inst')" > tmp/harvest_logs/${inst}_$(date +%F).log 2>&1 &
done
pgrep -af harvest_all   # the harvests still running
```

- **One process per institution.** Each institution has its own
  checkpoint (`data/raw/checkpoint/*_{inst}.RDS`), raw folder, run names
  and ledger, so processes for different institutions do not collide.
  Never harvest one institution in two processes at once: they would
  share its checkpoint, and the build stops at an offering in two runs.
- **One process per host.** A process pauses between its own requests
  (3-5 seconds, or the institution’s `request_delay`), so two processes
  on one host double the load on it. Institutions on a shared host (a
  common course catalogue) go in one process, `institutions = c(...)`,
  which harvests them one after another. Each configured institution has
  a host of its own today. Read the robots.txt of a new site before its
  first harvest: a `Crawl-delay` goes in `request_delay`, and the date,
  delay and disallowed paths in `data/robots_txt.csv` (see “Piloting an
  Institution”, step 1).
- **At night.** Start long harvests in the evening, so that most
  requests come outside the sites’ busy hours. A site with the year in
  the URL can be stopped (end the process) and started again: the
  checkpoint, saved every 50 pages, resumes it. A “page” or “current”
  site fetches all its pages again at every start, so let it run to the
  end. A site that answers 403 or 429 stops its own harvest; start it
  again later.
- **Memory and the build.** A process keeps its institution’s pages in
  memory until it writes the run, so check free memory before starting
  more of them. `tar_make()` (4 workers) builds the runs written so far;
  run it when the harvests are done, or audit only the institutions
  whose harvest has ended.

## Piloting an Institution

Before the full harvest of an institution, a new one or a configured one
going from teacher education to all its courses, a pilot harvests a
sample of its courses and checks the config, the URLs and the headings
on it: about 200 pages instead of tens of thousands. The full harvest
starts when the pilot audits find the institution ready (step 4). Load
the functions as in “Quick Start”, then:

1.  **Draw the sample.** First read the robots.txt of each host the
    harvest requests and update its row in `data/robots_txt.csv`: the
    date, the `Crawl-delay` asked of all user agents (`User-agent: *`)
    and the `Disallow` rules that touch course plans. A `Crawl-delay`
    goes in `request_delay` (`tests/testthat/test-robots.R` checks it);
    a path it disallows is not harvested and is noted in
    `data/data_notes.qmd`. Then find the oldest year the site has plans
    for.

    ``` r
    s <- pilot_sample("ntnu", oldest_year = 2004)
    ```

    `pilot_sample()` (`R/harvest.R`) draws 100 course-years (a course in
    a year, with every semester DBH lists it in: about 180-200
    offerings) from the latest DBH snapshot:

    - in 2019, 2022 and 2025 (`PILOT_YEARS`) and `oldest_year`, leaving
      out the pilot years before it; at a site that shows only the
      current plan, in the latest DBH year;
    - at least one course-year of each department (DBH’s
      `department_name`: a faculty at a college, an institute at a
      university), five of each level group (`pilot_level()`: lower
      degree, higher degree, PhD, continuing education, other) and ten
      English-taught ones (`PILOT_COVER`), and the rest at random, in
      equal numbers per year.

    The same snapshot, arguments and seed (`PILOT_SEED`) give the same
    sample. It is written to `data/pilot/{inst}.csv` (course id,
    course-year, strata, snapshot and seed), which the audits and
    `pilot_gate()` read: commit it.

2.  **Harvest the sample** into the raw store:
    `harvest_all(courses = s, institutions = "ntnu")`. At an institution
    already harvested, only the sample’s offerings that no raw file
    holds are fetched. At 10 seconds a page, 200 pages take about 35
    minutes.

3.  **Build and read the gate numbers.** Run `targets::tar_make()`,
    then, with the pipeline’s functions loaded:

    ``` r
    targets::tar_load_globals()
    g <- pilot_gate("ntnu")
    g[c("metric", "value", "n", "threshold", "review")]
    g$examples   # per metric: a course id of each course-year that counts against it
    ```

    `pilot_gate()` (`gate_numbers()` in `R/pipeline_metrics.R`) reads
    the sample’s course-years in the current build:

    <table>
    <colgroup>
    <col style="width: 33%" />
    <col style="width: 33%" />
    <col style="width: 33%" />
    </colgroup>
    <thead>
    <tr>
    <th>Metric</th>
    <th>Share or count of the sample’s course-years</th>
    <th>Review if</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td><code>fetched</code></td>
    <td>with a page in some semester</td>
    <td>&lt; 0.80</td>
    </tr>
    <tr>
    <td><code>plan</code></td>
    <td>with a plan</td>
    <td>&lt; 0.80</td>
    </tr>
    <tr>
    <td><code>assessment</code></td>
    <td>of those with a plan: an assessment section</td>
    <td>&lt; 0.90</td>
    </tr>
    <tr>
    <td><code>coursework</code></td>
    <td>of those with a plan: a coursework-requirements section</td>
    <td>&lt; 0.70</td>
    </tr>
    <tr>
    <td><code>no_sections</code></td>
    <td>count: a plan with no section (a layout the extractor does not
    read)</td>
    <td>counted</td>
    </tr>
    <tr>
    <td><code>unmapped_headings</code></td>
    <td>the share of the headings on the sample’s pages that map to no
    section</td>
    <td>&gt; 0.65</td>
    </tr>
    <tr>
    <td><code>privacy_hits</code></td>
    <td>count: personal data left in the plan or its sections
    (<code>privacy_check</code>’s patterns)</td>
    <td>&gt; 0</td>
    </tr>
    </tbody>
    </table>

    The values are `PILOT_GATE`; methods.qmd (“Pilot protocol and gate”)
    gives their reasons. They are flags, not a pass mark: `review` is
    TRUE for a number beyond its value, and the audits (step 4) find out
    why. Some institutions are lower for reasons on the site, such as
    course-years DBH lists that have no page. With 100 course-years a
    share is uncertain by about ±0.07.

4.  **Audit and verdict.** In Claude Code, run
    `/audit-institutions fulltext ntnu --pilot`, then `sections` and
    `anonymization` (see “Auditing a Pipeline Step”): with `--pilot`,
    the packets show the sample’s courses only, and each synthesis reads
    the gate numbers and ends with a verdict on the institution: **ready
    for full harvest**, **ready with known limits** (each flagged number
    named with its reason) or **not ready** (what must be fixed first: a
    confirmed serious error in the text, the sections or the
    anonymization of the sample, personal data left, or a flagged number
    with no reason found). The skill gives the criteria
    (`.claude/skills/audit-institutions/SKILL.md`, “Verdict”). Record
    the verdicts in the institution’s issue with the gate table, the
    course ids of `no_sections` and the ten most frequent unmapped
    headings, each either mapped or not plan text (facts box, contact,
    overlap). A plan that the section extractor cannot cut because of an
    unusual layout is accepted, but counted in `no_sections`; a layout
    left so is noted in the institution’s notes in
    `data/data_notes.qmd`.

5.  **Fix and rebuild.** Fix what the audits show in the config, the URL
    builder or the heading map, rebuild with `tar_make()`, and audit the
    institution again. Only new URLs need a new harvest:
    `harvest_all(courses = s, institutions = "ntnu", retry = TRUE)`
    fetches the sample’s offerings without a plan whose URL nobody has
    requested (see “Fetch Ledger and Retries”).

6.  **Full harvest** once all three audits find the institution ready,
    with or without known limits: `harvest_all(institutions = "ntnu")`
    fetches every other offering; the sample is in the raw store
    already. Start the slowest first (NTNU and UiB, 10 seconds a page,
    five to six days each), in the background, several institutions at
    once (see “Several Institutions at Once”).

A site that shows only the current plan gives the plans of the academic
year its pages were fetched in (see “Sites that show only the current
plan”), so its pilot’s pages join DBH’s offerings once the DBH snapshot
has that year. Until then only `fetched` can be read; the other gate
numbers and the audits wait for the new snapshot (“The Yearly Harvest”,
step 2). Samas fetches nothing (`noop`), so it has no pilot.

## Adding a New Institution

To add support for a new institution, you need to modify **two files**.
In Claude Code, the `/add-institution` skill walks through the same
steps.

### 1. Add config entry to `R/institution_config.R`

``` r
# Add to the institution_configs list:
newuni = list(
  code = "1234",                  # DBH code; several if it changed: c("0264", "1177")
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # the element that holds the course plan
  exclude = ".contact",           # optional: parts of it that are not the plan
  plan_years = "url",             # year in the URL; "page" or "current": see R/institution_config.R
  # url_year = "academic",        # if the URL's year is the academic year (ntnu, hvl, oslomet, uis)
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
narrowest element that holds the whole plan with its title and facts box
(`course_plan_spec.md`), and leave out navigation, contact boxes (staff
names) and widgets inside it with `exclude` (hvl, nord, uib, uio, uis,
mf, hivolda do this). Name the title and facts box in `facts`, so that
they stay out of the sections and the plan id.

### 2. Add URL builder to `R/add_course_url.R`

Add a case to `case_match()` and create a URL-building function:

``` r
# In add_course_url(), add:
"newuni" ~ add_course_url_newuni(course_code, year, semester_name),

# Create a helper function for your institution
add_course_url_newuni <- function(course_code, year, semester) {
  semester <- case_match(semester, "Vår" ~ "spring", "Høst" ~ "autumn")
  glue::glue("https://www.newuni.no/courses/{year}/{semester}/{tolower(course_code)}")
}
```

### 3. Test Your Changes

``` r
# Load pipeline and test
courses <- dbh_courses()   # every DBH course; the institution's code must be in its config
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> select(course_code, url, html_success, extracted_text) |> head()
result$extracted_text[1]  # Inspect first result
```

Then pilot it on a sample of its courses before the full harvest (see
“Piloting an Institution”).

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
3.  **Deduplicate** (`plan_ids()`, `plan_table()`): Groups identical
    normalized texts under a shared `plan_content_id`, within each frame
    (see “Frames”)

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
<tr>
<td><code>plan_year_basis</code></td>
<td>How the plan’s year is known: <code>url</code>, <code>page</code> or
<code>current</code> (<code>plan_years</code> in the config)</td>
</tr>
<tr>
<td><code>harvested_at</code></td>
<td>For <code>current</code> sites: the date of the harvest the page
comes from</td>
</tr>
</tbody>
</table>

A page with no section to read and under 1,500 characters is a page
shell, not a plan (`SHELL_MAX_NCHAR`): its offerings keep their text but
get no `plan_content_id`.

### Sites that show only the current plan

UiO, MF, NMBU and Steiner (`plan_years = "current"`) publish only the
plan in force, without a year in the URL. A harvest therefore holds the
plans of the academic year (August-July) it was made in, and a later
harvest cannot recover them. Their raw files are snapshots:

- Each harvest of such a site fetches the pages of its latest DBH year
  again and adds them as a dated raw file, stamped `harvested_at`.
  Checkpoints from an earlier academic year are removed first (they
  would return last year’s pages), and a harvest in June-August warns
  (the site may already show next year’s plan).
- `read_harvest()` gives each offering the page of its course code from
  the latest harvest in the offering’s academic year (autumn Y: Y/Y+1,
  spring Y: Y-1/Y), or no page. A page counts for its course and year
  whichever DBH row it was fetched for, so DBH rows published years
  later still find it.
- So harvest these sites once in every academic year (best
  September-November or February-April), whether or not DBH has a new
  year; a year without a harvest has no plans for them.

Their earlier plans survive only where the Internet Archive’s Wayback
Machine captured the page. `R/wayback.R` lists the captures of a URL or
of every URL under a prefix (`wayback_cdx()`, the CDX API) and gives the
address of a capture as archived (`wayback_capture_url()`). It is not
part of the harvest: no capture is in the raw store or the data. Tested
2026-10-16 and rejected as a plan source 2026-10-17: the captures cover
far fewer courses than the live harvest (methods.qmd, “Sites that
publish only the current plan”). It remains for spot checks of single
pages.

Their runs are `2026-04-04` (the user’s earlier harvest, 2025/26) and
`2026-09-30` (2026/27), both of DBH 2025 rows (`data/raw/{inst}/2025/`).
A harvest date comes from the `harvested_at` column or the run’s name,
never from a file’s modification time, which a copy changes. The rest of
the April backup, for sites with the year in the URL, is in
`data/backup/2026-04-04/` and is not read. Raw files exist only locally:
back them up.

A “page” site (NLA) and a “current” site change their pages over time,
so their checkpoints are removed before every harvest and all pages are
fetched again; for “url” sites the checkpoint only resumes an
interrupted harvest.

### Raw Store and Finalized Releases

The raw store is Parquet (zstd), one file per institution, DBH year
(`year`) and harvest run: `data/raw/{inst}/{year}/{run}.parquet`. A run
is named by its date, a second run of an institution the same day by
date and time (`{date}_{HHMMSS}`, which sorts after it), so a retry need
not wait for the next day. No raw file is written twice. A harvest
fetches only the offerings no raw file holds yet (a current site: a new
snapshot) and `harvest_all()` saves them as a new run
(`write_raw_run()`), with the run’s fetch ledger (see “Fetch Ledger and
Retries”). `harvest_all(year = 2026)` and `harvest_all()` therefore both
just add what is new. An offering is matched on institution, code, year
and semester: when a new DBH snapshot changes its status, it keeps the
`course_id` it was first harvested under. `read_harvest()` combines the
runs; an offering in two runs stops the build.

A raw file holds the rows as the harvest returned them: the offering’s
DBH columns as they were at the harvest (under DBH’s names until
2026-10-16, which `read_raw()` renames: `dbh_rename()`, see
“Variables”), `url`, `html`, `html_success`, `html_error` (the error
message), `harvested_at`, `retry` (a retry run) and `harvest_row`, the
row’s place in its run, so `read_raw()` gives the rows in the order they
were harvested (which offering a plan’s text comes from depends on it).
`extracted_text` is kept only for a row without `html`, text read from a
PDF, the raw data there; uis keeps the PDF’s bytes too (`pdf`, since
2026-10-16). Elsewhere `tar_make()` rebuilds the text from `html`.

The format changed once, on 2026-10-16; the content did not. The RDS raw
files of the harvests before (`data/raw/html_{inst}.RDS`, the first
harvests, and `data/raw/harvests/{date}/html_{inst}.RDS`) were written
one to one to the Parquet store by `data-raw/raw_to_parquet.R`, which
compares every run with its RDS file column by column and row by row,
and `tar_make()` then rebuilt release data-2026-10-16 exactly. The first
harvests did not stamp `harvested_at` and were not dated: their run is
named by the day their RDS file was written (2026-09-30 to 2026-10-06),
and their `harvested_at` stays empty. The RDS files are no longer read;
they stay in `data/raw/` until they are removed by hand, after a backup.

A page that gave no plan can be fetched again with
`harvest_all(retry = TRUE)` (see “Fetch Ledger and Retries”). Retry
before finalizing a release; afterwards it changes the release.

To finalize a release, after `tar_make()` and tagging the commit:

``` r
source("R/utils.R"); source("R/release.R")
finalize_release("data-YYYY-MM-DD")   # then commit tests/snapshots/raw_manifest.csv
release_data("data-YYYY-MM-DD")       # hand-over to the coding project
```

`finalize_release()` makes every raw file and fetch ledger read-only (an
overwrite then fails), lists them with md5 sums in
`tests/snapshots/raw_manifest.csv`, and keeps the release’s offerings,
plans and sections (the teacher-ed frame’s, see “Frames”) in
`data/releases/{tag}/`. On every build `tar_read(frozen_check)` lists
any change to the release: a raw file changed or missing, an offering
gone or linked to another plan, a plan gone or with other text, a
section changed. Plans added later (a new DBH year) are not changes. A
planned change to the release (a privacy fix, a section fix after the
coders’ check) means a new release: finalize it, and `release_data()`
writes the old -\> new `plan_content_id` crosswalk. Until then
`frozen_check` lists the planned changes on every build, and a change
nobody intended would be listed among them, so each commit explains the
rows it adds, and the next release follows soon after the fixes. A
section fix keeps the plan’s id, so the crosswalk does not show it;
`release_data()` lists such sections (plan, section, changed, added or
removed) in `sections_changed.csv`.

**Output files** (of all courses; the teacher-ed frame’s in the
`teacher_ed/` folders, see “Frames”):

- `data/processed/course_offerings.RDS` — published slim dataset: course
  rows with DBH metadata + `plan_content_id` FK (no url, no text)
- `data/interim/course_offerings_full.RDS` — internal working file: same
  rows plus `url`, `extracted_text`, `course_plan`,
  `course_plan_normalized`; the teacher-ed one is used by the
  course_browser app
- `data/processed/course_plans.RDS` — one row per unique plan per course
  code per institution, with the offering its text comes from
  (`source_course_id`)

### Fetch Ledger and Retries

Every harvest run also writes its fetch ledger,
`data/raw/{inst}/ledger/{run}.parquet`: one row per offering of the run,
in the run’s order, with how its page was fetched (`fetch_ledger()`):

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Column</th>
<th>Holds</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>run</code>, <code>institution</code>, <code>course_id</code>,
<code>url</code></td>
<td>the run, the offering and the URL requested</td>
</tr>
<tr>
<td><code>requested_at</code></td>
<td>when the request was sent</td>
</tr>
<tr>
<td><code>http_status</code></td>
<td>the answer’s HTTP status, also of a site’s own not-found page</td>
</tr>
<tr>
<td><code>outcome</code></td>
<td>the outcome class, below</td>
</tr>
<tr>
<td><code>bytes</code>, <code>sha256</code></td>
<td>size and SHA-256 of the page as stored (<code>html</code>, else the
text read from a PDF)</td>
</tr>
<tr>
<td><code>raw_file</code></td>
<td>the raw file that holds the row</td>
</tr>
<tr>
<td><code>dbh_snapshot</code></td>
<td>the date of the DBH snapshot the harvest’s offerings came from
(<code>dbh_courses()</code>)</td>
</tr>
</tbody>
</table>

The outcome classes (`fetch_outcome()`): `ok` (a page or a PDF’s text
was stored); `not_found` (HTTP 404 or 410; HTTP 400, UiB’s answer for a
semester before its first; or the site’s own not-found page, HVL’s
“Course not found”); `shell` (the site answers that it has nothing for
that year, NTNU’s “No course information available for this academic
year”); `server_error` (HTTP 5xx); `timeout` (no answer: a timeout or a
failed connection); `blocked` (HTTP 403 or 429); `error` (anything
else); `no_url` (no URL to request: the URL builder or the URL discovery
found none).

The ledger is raw data like its run: append-only, written only by
`harvest_all()`, locked by `finalize_release()`. It holds URLs and
hashes, no page text. It records the page requests (`fetch_html_cols()`,
UiS’s PDFs); USN’s pages rendered in Chrome, Steiner’s subject PDFs and
the requests of URL discovery have no status or time in it. The runs
before 2026-10-16 have no ledger: `raw_ledger()` reconstructs theirs
from the raw rows, without `requested_at` and `dbh_snapshot`, and with
`http_status` only where the error message gives it (“HTTP 404 Not
Found.”); their `recorded` is `FALSE`. Target `ledger` is the ledger of
all runs, recorded or reconstructed.

**Retries within a harvest** (`site_request()`): a request without an
answer (a timeout, a failed connection) or with HTTP 500, 502, 503 or
504 is tried again, 3 times in all, after 30 and then 60 seconds; any
other answer is final. A site that answers 403 or 429 refuses the
harvest: its harvest stops, the pages fetched so far stay in the
checkpoint (the next harvest resumes there), and `harvest_all()` goes on
with the next institution.

**Retries between harvests:** `harvest_all(retry = TRUE)` fetches again
the offerings that have no plan in the current build
(`data/interim/course_offerings_full.RDS`, so run `tar_make()` first),
past the checkpoint, by how the URL the build gives them answered last,
for whichever offering it was requested (`offering_outcome()`): every
outcome but `not_found`, so a page not found is not asked for again, and
always a URL nobody has requested (a spring offering’s academic-year
page). `retry` can name the classes instead:
`retry = c("timeout", "server_error")` only failed fetches,
`retry = "not_found"` pages that may have been published since. The
retried offerings are saved as a new run, marked `retry`, and
`read_harvest()` gives a retried offering its latest row with a page. A
site that shows only the current plan is not retried; it takes a new
snapshot in each academic year.

``` r
harvest_all(institutions = "uia", year = 2023, retry = TRUE)
harvest_all(institutions = "uib", retry = "server_error")
```

**Coverage and changed pages.** Target `coverage` lists every offering
of each frame of the latest DBH snapshot (`frame`: `all`, every course
at the institutions with a config, or `teacher_ed`) with `outcome` (how
its URL answered last; `not requested`, or `not harvested`: in no raw
run), `has_plan` (in that frame’s files), and `retry` (no plan, and
`harvest_all(retry = TRUE)` would fetch it again): the coverage report
of a release. Target `changed_pages` lists the pages whose plan text
changed between two runs that fetched the same URL (a site that reworded
an old page; a current site’s plan of a new year). The body’s `sha256`
alone cannot tell, since some sites change the body on every fetch
(Liferay’s `authToken` at NTNU and OsloMet), so where the bodies differ
the text the pipeline reads from them is compared.

``` r
targets::tar_read(coverage) |> dplyr::filter(frame == "teacher_ed") |>
  dplyr::count(institution, outcome, has_plan, retry)
targets::tar_read(changed_pages)
```

## Post-Harvest: Section Extraction

The pipeline splits each course plan into its parts and writes
`data/processed/plan_sections.RDS` with one row per plan and section
(`plan_content_id`, `institution`, `course_code`, `source_course_id`,
`section`, `text`), keyed like `course_plans.RDS`. A plan’s sections are
cut from the page of `source_course_id`, the offering whose text the
plan keeps, so they match its `course_plan`. The seven sections are
`course_content`, `learning_outcomes`, `teaching_methods`, `assessment`,
`coursework_requirements`, `prerequisites` and `reading_list`. Section
text is anonymized with the same `anonymize_text()` as `course_plan`:
sections are cut from the raw `html`/`extracted_text`, so this step must
keep anonymizing. Admission text, exam logistics and placeholder rows
such as “Se fagplanen.” are left out. What a section holds and what it
leaves out is set by `course_plan_spec.md`.

`tar_read(metrics)` has the number of plans with each section per
institution and `pct_text_in_sections`, the median share of a plan’s
text that its sections hold (what is left is metadata, admission text,
exam logistics and headings the map does not know). `tar_read(unmapped)`
lists the headings the extractor could not map, with the number of pages
each is on, and `tar_read(heading_use)` every pattern of the heading map
with the pages it opens a section on: a pattern on no page is a
candidate for removal (the html reader’s text fallback, used for uis PDF
plans, is not counted). To map a new heading, add a pattern to
`R/section_heading_map.R`; how each institution is split is set by the
`section_*` fields in `R/institution_config.R`.

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
when it finds &lt; 3 sections, if the text gives more sections and one
that no heading on the page names</td>
</tr>
<tr>
<td><code>json</code></td>
<td>nla</td>
<td>Titles and contents in the embedded EmneplanPage JSON</td>
</tr>
<tr>
<td><code>text</code></td>
<td>usn, steiner (+ the fallback, e.g. uis PDF plans)</td>
<td>Heading-shaped lines in <code>extracted_text</code>; skips the title
and facts box and a table of contents (“Innholdsfortegnelse”, usn), and
a line repeating one of its entries is a heading, so an unmapped one
(“Annet”) ends the section; inside a reading list opened by a whole
heading only a whole heading switches section (book titles)</td>
</tr>
<tr>
<td><code>noop</code></td>
<td>samas</td>
<td>—</td>
</tr>
</tbody>
</table>

The `html` reader writes a table as one line per row, its cells
separated by tabs, so an exam table has one line per exam.

Fields for the `html` reader:

- `selector` and `exclude` (shared with the fulltext): the container,
  and elements in it that are not read (mf’s contact card, banner and
  the facts box’s buttons; uis’s facts-box links; nord’s “Kopier lenke”
  label; ntnu’s timetable tab and the exam tab’s link, rooms and
  comments). `exclude` holds CSS selectors; an entry that starts with
  “/” is XPath, for an element picked by its text (ntnu’s “Dato”, “Tid”
  and “Sted og rom” labels with the value after them).
- `facts` (shared with the fulltext): the elements that hold the title,
  the course code and the facts box (every `html` institution; for nla’s
  JSON the parts `"title"` and `"table"`), and plan text in no section
  that changes from year to year while the plan stays the same (ntnu’s
  admission programme list, credit-reduction box and subject areas;
  uia’s student evaluation and admission flags; hivolda’s evaluation and
  examiner sentences and programme list; nord’s “Type emne”). Their
  blocks are flagged `facts`: in `course_plan`, but in no section (a
  heading inside them opens none), and not in the plan text (`plan_text`
  in the fulltext table) that the plan id, the shell rule and the
  sections’ text fallback read, so a campus or programme list that
  changes from year to year does not split a plan. In pages read as text
  (usn, steiner, uis PDF plans) the title and facts box are the lines
  before the plan: a metadata block matching `section_text_header`, or
  the lines before a table of contents (usn).
- `section_heading_selector`: section headings (default `h2`; ntnu
  `"h2, h3:not(.exam-assessment-form)"`, inn’s `div.label`, uib’s and
  mf’s `summary`, nord’s `button.ac-trigger`).
- `section_scope`: elements that hold a section of their own (`details`,
  nord’s `div.ac`); the section open around them continues after them.
- `section_initial`: section for text before the first heading (mf’s
  untitled intro is course_content).
- `section_untitled`: elements that hold a section without a heading, as
  `c(selector = section)` (nord’s short description
  `div.placeholder-text` is course_content; the title and course code
  before it stay out).
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
  of `extracted_text` (uis PDFs: “Emnekode:”, “Tilbys av:”; steiner’s
  PDFs: “Studiepoeng”, “Undervisningsspråk”); the `text` reader skips it
  and files the untitled paragraph after it as course_content, and the
  plan id leaves it out.
- `section_inline_coursework`: move “Arbeidskrav (AK): …” /
  “Obligatorisk deltakelse …” lines from assessment to
  coursework_requirements (nord).
- `exam_sessions`: the page lists the exam parts once per exam session
  (ntnu’s exam tab: “Ordinær eksamen - Høst 2022”, “Utsatt eksamen -
  …”). The assessment section and the plan id keep the parts of one
  session, the first ordinary one, without the session headings and the
  exam system (`.exam_tab_design()`); `course_plan` keeps them all.
- `section_pointer`: a section whose text is only a pointer to another
  block of the page (“Se fagplanen.”) takes the text under a sub-heading
  of that block, after a lead naming the source. oslomet: teaching
  methods from the subject’s “Fagplan” block (“Fagets arbeids- og
  undervisningsformer”), as “Se fagplanen. Fagplanen sier: …”. The parts
  under the `append` sub-headings join their section when the page has
  it with text of its own (oslomet: the Fagplan’s “Arbeidskrav” and
  attendance parts join the coursework requirements), and a line
  starting with a `stop` label ends a part (labels the page did not mark
  as sub-headings: “Skikkethetsvurdering”, “Vurderingsuttrykk”).

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
logistics (“Mer om eksamen ved UiO”, “Sensorordning”, resit headings
such as “Ny/utsatt eksamen”). Exam aids, exam language and grading scale
stay in assessment. A `label` heading (“Hjelpemidler til eksamen”,
“Eksamensspråk”, “Fravær fra obligatorisk aktivitet”) stays as a line of
its section, since the text under it (“Ingen”, “Norsk.”) says nothing
alone; with nothing but a dash or pointer under it, it goes.

HTML is parsed by `.read_doc()`, which drops `script`, `style`, `select`
and `label` (ntnu script text, uib semester picker); the `json` reader
parses the page itself because its data is in a `<script>`.

A prerequisites label inside course_content (“Forkunnskapskrav”, uit’s
“Om emnet”) moves the paragraph it names to prerequisites
(`.split_inline_prerequisites()`), and a paragraph of 40 or more
characters that its section repeats is kept once.

`.clean_sections()` removes `.drop` rows, strips page widgets, pointers
and stock logistics sentences from assessment/coursework
(`.section_noise`: uib banner and footer, hvl’s “Mer om hjelpemidler”
link, oslomet’s resit sentence, …; notices about the assessment such as
AI, plagiarism and COVID rules stay, and so do exam tables with their
header row), removes admission and study-right sentences, pointers to
the admission rules and “Ingen utover opptakskravet.” from
prerequisites, keeping a sentence that also names a prerequisite
(`.strip_admission_lines()`), drops placeholder-only rows
(`.placeholder_phrases`: “Ingen”, “Se fagplanen.”, “-”, Leganto
pointers, “Oppgis senere.”, “… ikke publisert ennå”), and drops a first
line that only repeats the section heading (but keeps learning-outcome
group labels such as “Kunnskap”).

## What Each Mechanism May Remove

`course_plan_spec.md` says what the published text holds: personal data
and boilerplate are out of `course_plan` and the sections, other plan
text stays in `course_plan`, and a section holds only the plan text
about its part. The mechanisms that remove text run on different texts,
so each may remove only some of these:

<table>
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th>Mechanism</th>
<th>Code</th>
<th>Runs on</th>
<th>May remove</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>selector</code> minus <code>exclude</code>;
<code>.read_doc()</code> drops <code>script</code>, <code>style</code>,
<code>noscript</code>, <code>select</code>, <code>label</code></td>
<td><code>R/institution_config.R</code>, <code>R/blocks.R</code>,
<code>R/extract_fulltext.R</code></td>
<td><code>course_plan</code> and sections</td>
<td>personal data, boilerplate</td>
</tr>
<tr>
<td><code>post_fn</code> (uit banner, year picker and contact block,
which keeps the unit); <code>.cleanup_usn_text()</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td><code>course_plan</code> (and usn’s sections, cut from the
text)</td>
<td>personal data, boilerplate</td>
</tr>
<tr>
<td><code>anonymize_text()</code>: <code>.anon_*</code> handlers, then
<code>.anon_generic()</code></td>
<td><code>R/anonymize.R</code></td>
<td><code>course_plan</code> and sections</td>
<td>personal data, boilerplate; a page that is not a plan (NA)</td>
</tr>
<tr>
<td><code>facts</code> (title and facts box, and lists and stock text in
no section, out of the id text); <code>normalize_plan_text()</code></td>
<td><code>R/institution_config.R</code>, <code>R/blocks.R</code>,
<code>R/normalize_plan_text.R</code></td>
<td>the plan id only</td>
<td>plan text that changes with the year, not with the plan</td>
</tr>
<tr>
<td><code>.drop</code> headings, unmapped headings, text before the
first heading</td>
<td><code>R/section_heading_map.R</code>,
<code>R/extract_sections.R</code></td>
<td>sections only</td>
<td>plan text about no section</td>
</tr>
<tr>
<td><code>.clean_sections()</code>: <code>.section_noise</code>,
placeholders, admission lines</td>
<td><code>R/extract_sections.R</code></td>
<td>sections only</td>
<td>plan text about no section, placeholders</td>
</tr>
<tr>
<td>shell rule (<code>SHELL_MAX_NCHAR</code>)</td>
<td><code>R/pipeline.R</code></td>
<td>whether a page has a plan</td>
<td>pages that are not a plan</td>
</tr>
</tbody>
</table>

So a rule that keeps plan text out of the sections belongs on the
section side, not in `exclude`, `post_fn` or `anonymize_text()`, which
would take it out of `course_plan` too. The spec’s “Known deviations”
lists the rules that do not fit yet.

## Auditing a Pipeline Step

The `/audit-institutions <sections|fulltext|anonymization> [inst ...]`
skill (Claude Code) checks a pipeline step for every institution with
one review agent per institution. R scripts build a review packet per
institution (`R/audit/prepare_{check}.R`; for sections after the
deterministic pre-pass `R/audit/qa_sections.R`), the agents write
findings to `data/audit/{check}/findings/{inst}.json`, and
`R/audit/aggregate.R` verifies them (course ids and verbatim quotes must
be in the packet) and writes a ranked cross-institution report. With
`--pilot`, each institution’s packet is drawn from its pilot sample only
(`data/pilot/{inst}.csv`), and the synthesis ends with a verdict on
whether the institution is ready for its full harvest (see “Piloting an
Institution”). Details: `.claude/skills/audit-institutions/SKILL.md`.

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
<td>the DBH snapshots, the pipeline input</td>
<td><code>data-raw/courses.R</code></td>
<td>yes</td>
</tr>
<tr>
<td><code>data/pilot/</code></td>
<td>the pilot samples: offerings and their strata</td>
<td><code>pilot_sample()</code></td>
<td>yes</td>
</tr>
<tr>
<td><code>data/raw/</code></td>
<td>the harvest, with personal data; append-only</td>
<td>harvesting only</td>
<td>no</td>
</tr>
<tr>
<td><code>data/releases/{tag}/</code></td>
<td>offerings, plans and sections of a finalized release</td>
<td><code>finalize_release()</code></td>
<td>no</td>
</tr>
<tr>
<td><code>data/backup/</code></td>
<td>earlier harvests kept but not read (personal data)</td>
<td>by hand</td>
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
<td><code>data/input/dbh_courses_{date}.parquet</code>,
<code>dbh_programmes_{date}.parquet</code></td>
<td>DBH tables 208 (courses) and 347 (programmes), the harvest
input</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/raw/{inst}/{year}/{run}.parquet</code>,
<code>data/raw/checkpoint/</code></td>
<td>raw harvest: <code>html</code> (+ text and bytes of PDFs)</td>
<td>yes (raw)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/raw/{inst}/ledger/{run}.parquet</code></td>
<td>fetch ledger: URL, HTTP status, outcome and page hash per offering
and run</td>
<td>none</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/pilot/{inst}.csv</code></td>
<td>an institution’s pilot sample (“Piloting an Institution”)</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/variables.csv</code></td>
<td>every variable of the published files, with its old name
(“Variables”)</td>
<td>none</td>
<td>in git; released with the data</td>
</tr>
<tr>
<td><code>data/robots_txt.csv</code></td>
<td>each host’s robots.txt: date read, Crawl-delay, disallowed
course-plan paths (“Piloting an Institution”)</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/interim/plan_gaps.RDS</code></td>
<td>course-years without a plan, reason, URLs tried</td>
<td>none</td>
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
<td><code>data/interim/teacher_ed/</code>,
<code>data/processed/teacher_ed/</code></td>
<td>the same files for the teacher-ed frame (“Frames”)</td>
<td>as above</td>
<td>as above</td>
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

### Input Data (`data/input/dbh_*_{date}.parquet`)

The harvest’s input is a dated snapshot of DBH, written by
`data-raw/courses.R` from DBH’s bulk files: `dbh_courses_{date}.parquet`
is table 208 (Emner), every course of every institution, year and
semester, all levels and statuses, one row per course and semester;
`dbh_programmes_{date}.parquet` is table 347 (Studieprogram), with the
programmes’ study type (`Studiumkode`). A new snapshot gets a new date
and the old ones stay. The snapshots keep DBH’s column names.
`dbh_courses()` reads the latest (or the one of `date`), gives DBH’s
columns the pipeline’s names (`DBH_COLUMNS`; see “Variables”) and adds
`institution` and `course_code`:

``` r
courses <- dbh_courses("teacher_ed")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution institution_code institution_name department_code department_name 
      <chr>       <chr>            <chr>            <chr>           <chr>           
    1 samas       0217             Samisk høgskole  480000          Avdeling for du…
    2 samas       0217             Samisk høgskole  480000          Avdeling for du…
    # ℹ 20 more variables: department_code_ssb <chr>, year <int>, semester <int>,
    #   semester_name <chr>, programme_code <chr>, programme_name <chr>,
    #   course_code_raw <chr>, course_code <chr>, course_name <chr>,
    #   level_code <chr>, level_name <chr>, credits <dbl>, nus_code <chr>,
    #   status <int>, status_name <chr>, language_code <chr>, language_name <chr>,
    #   subject_code <chr>, subject_name <chr>, thesis <int>

`dbh_courses()` is every course; `dbh_courses("teacher_ed")` keeps the
courses of the teacher education programmes (study types in
`TEACHER_ED_STUDIUM`, matched on institution and programme code: a
programme code is unique only within an institution). The teacher-ed
frame is a filter of the same snapshot, so its offerings have the same
`course_id` in both. `harvest_all()` without `courses` harvests every
course; `tar_make()` builds everything in `data/raw/` and writes the
teacher-ed frame’s files apart (see “Frames”).

Key columns: - `institution`: Short code (e.g., “oslomet”, “uia”); NA
for an institution code no config has - `course_code_raw`: DBH’s course
code (used in `course_id`) - `course_code`: the code with the trailing
version number removed (`canon_remove_trailing_num()`: “ABC123-1” →
“ABC123”) - `year`, `semester_name` (“Vår”, “Høst”, “Sommer”, “Vinter”;
`canon_semester_name()` gives `course_id` spring, autumn, summer) -
`programme_code`, `programme_name`: the programme DBH registers the
course under (one per row) - `status`: course status (1 active, 2 new, 3
discontinued, 4 discontinued with active students). It is the last part
of `course_id`, and a later snapshot can change it: an offering already
in a raw file keeps the id it was first harvested under
(`first_course_id()`)

DBH lists most courses under both semesters of a year, also when they
are taught in one. Where pages are semester-specific, about half the
rows therefore have no plan by design; see “Semester registration in
DBH” in `data/data_notes.qmd` before reading row-level success rates.

`harvest_institution()` returns the offerings with `course_id`, `url`,
`html`, `html_error`, `html_success` and `extracted_text` added, the
rows of the raw store (“Raw Store and Finalized Releases”).

### Variables (`data/variables.csv`)

`data/variables.csv` documents every variable of the published files
(`course_offerings.RDS`, `course_plans.RDS`, `plan_sections.RDS`, of
both frames): the file, its name, type, source (the column of DBH table
208, or how the pipeline gives it), meaning and codes, and `old_name`,
its name in the releases up to data-2026-10-16. `release_data()` hands
it over with the data, and `tests/testthat/test-dbh-input.R` checks it
against the built files and `DBH_COLUMNS`. `course_offerings_full.RDS`
(internal) adds `url`, `extracted_text`, `course_plan`,
`course_plan_normalized` and `plan_nchar`.

Since 2026-10-16 the DBH columns have English snake_case names
(methods.qmd, “Variable names”); their values are DBH’s (Norwegian
names, DBH’s codes), and `year_from` and `year_to` are integers (double
before). DBH’s snapshots, the raw files of the harvests until 2026-10-16
and the releases up to data-2026-10-16 have DBH’s names: the pipeline
renames them where it reads such data (`dbh_rename()`, in
`dbh_courses()`, `read_raw()`, `read_checkpoint()`, `frozen_changes()`
and `release_data()`). Code that reads an old release can rename it by
the same pairs:

``` r
v <- read.csv("data/variables.csv")
pairs <- unique(v[v$old_name != v$variable, c("variable", "old_name")])
old <- readRDS("data/releases/data-2026-10-16/course_offerings.RDS") |>
  dplyr::rename(dplyr::any_of(setNames(pairs$old_name, pairs$variable)))
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 2%" />
<col style="width: 10%" />
<col style="width: 69%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">file</th>
<th style="text-align: left;">variable</th>
<th style="text-align: left;">old_name</th>
<th style="text-align: left;">type</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_id</td>
<td style="text-align: left;">course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Offering id:
{institution}<em>{course_code_raw}</em>{year}<em>{semester}</em>{status},
with the semester in English (spring, autumn, summer). An offering keeps
the id it was first harvested under when a later DBH snapshot changes
its status.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from institution_code</td>
<td style="text-align: left;">Short name of the institution in the
harvest (“oslomet”, “uia”); several DBH codes can map to one (INN: 0264
and 1177).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution_code</td>
<td style="text-align: left;">Institusjonskode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonskode</td>
<td style="text-align: left;">DBH’s institution code (“0256”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution_name</td>
<td style="text-align: left;">Institusjonsnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonsnavn</td>
<td style="text-align: left;">DBH’s institution name.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_code</td>
<td style="text-align: left;">Avdelingskode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode</td>
<td style="text-align: left;">DBH’s code of the unit (faculty or
department) that offers the course.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_name</td>
<td style="text-align: left;">Avdelingsnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingsnavn</td>
<td style="text-align: left;">Name of the unit.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_code_ssb</td>
<td style="text-align: left;">Avdelingskode_SSB</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode_SSB</td>
<td style="text-align: left;">Statistics Norway’s code of the unit; NA
in some years.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">year</td>
<td style="text-align: left;">Årstall</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Årstall</td>
<td style="text-align: left;">Calendar year of the semester.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">semester</td>
<td style="text-align: left;">Semester</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Semester</td>
<td style="text-align: left;">Semester code: 0 winter, 1 spring, 2
summer, 3 autumn.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">semester_name</td>
<td style="text-align: left;">Semesternavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Semesternavn</td>
<td style="text-align: left;">Semester in Norwegian: “Vinter”, “Vår”,
“Sommer”, “Høst”.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">programme_code</td>
<td style="text-align: left;">Studieprogramkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramkode</td>
<td style="text-align: left;">The study programme DBH registers the
course under in this semester (one per row); unique only within an
institution.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">programme_name</td>
<td style="text-align: left;">Studieprogramnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramnavn</td>
<td style="text-align: left;">Name of the programme.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_code_raw</td>
<td style="text-align: left;">Emnekode_raw</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnekode</td>
<td style="text-align: left;">DBH’s course code, with its version number
(“LMUKHV10421-1”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from course_code_raw</td>
<td style="text-align: left;">Course code without a trailing version
number (“LMUKHV10421”): the code plans are kept by.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_name</td>
<td style="text-align: left;">Emnenavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnenavn</td>
<td style="text-align: left;">Course name as DBH registers it.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">level_code</td>
<td style="text-align: left;">Nivåkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivåkode</td>
<td style="text-align: left;">Level of the course: LN lower degree
level, HN higher degree level, AR one-year programme, VS upper secondary
level, 50 an older code without a name, and others in DBH (FU, B3, M2,
…).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">level_name</td>
<td style="text-align: left;">Nivånavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivånavn</td>
<td style="text-align: left;">Name of the level; NA for code 50.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">credits</td>
<td style="text-align: left;">Studiepoeng</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">DBH table 208 Studiepoeng</td>
<td style="text-align: left;">ECTS credits.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">nus_code</td>
<td style="text-align: left;">NUS-kode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 NUS-kode</td>
<td style="text-align: left;">Statistics Norway’s standard
classification of education (NUS), six digits; “999999” unknown.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">status</td>
<td style="text-align: left;">Status</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Status</td>
<td style="text-align: left;">Course status: 0 no data, 1 active, 2 new,
3 discontinued, 4 discontinued with active students (exams still
held).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">status_name</td>
<td style="text-align: left;">Statusnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Statusnavn</td>
<td style="text-align: left;">Status in Norwegian (“Aktivt”, “Nytt”,
“Avviklet”, “Avviklet med aktive studenter”, “Ikke data”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">language_code</td>
<td style="text-align: left;">Underv.språk</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Underv.språk</td>
<td style="text-align: left;">Language of instruction as DBH registers
it: “NOR”, “ENG”, “SME”, …; “999” unknown; a few in lower case
(“nob”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">language_name</td>
<td style="text-align: left;">Navn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Navn</td>
<td style="text-align: left;">Name of the language of instruction
(“Norsk”, “Engelsk”, “Ukjent”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">subject_code</td>
<td style="text-align: left;">Fagkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagkode</td>
<td style="text-align: left;">Subject (fag) code of the course
(“KUNST”); NA for some courses.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">subject_name</td>
<td style="text-align: left;">Fagnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagnavn</td>
<td style="text-align: left;">Name of the subject (“Kunst og
Håndverk”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">thesis</td>
<td style="text-align: left;">Oppgave (ny fra h2012)</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Oppgave (ny fra h2012)</td>
<td style="text-align: left;">Whether the course is a thesis: 0 no, 1 a
bachelor’s thesis or project, 2 a master’s (or hovedfag) thesis.
Registered from autumn 2012, and for some earlier courses.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_year_basis</td>
<td style="text-align: left;">plan_year_basis</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">How the year of the offering’s plan is
known: “url” the page’s URL holds the year; “page” one page holds the
plans of several years (NLA); “current” the site shows only the plan in
force, so a page is the plan of the academic year it was fetched
in.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The offering’s plan in course_plans (with
institution and course_code); NA when the offering has no plan. A
SHA-256 hash of course_plan_normalized, so offerings with the same plan
text share it.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">harvested_at</td>
<td style="text-align: left;">harvested_at</td>
<td style="text-align: left;">Date</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">Date of the harvest of the offering’s row
(recorded since 2026-10-06, else NA); at a site that shows only the
current plan, the date of the harvest whose page the offering
takes.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">academic_year</td>
<td style="text-align: left;">academic_year</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">NLA only: the academic year (“2023-2024”)
whose part of the course page holds the offering’s plan; NA
elsewhere.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">has_extracted_text</td>
<td style="text-align: left;">has_extracted_text</td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Whether text was read from the offering’s
page.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">extracted_text_nchar</td>
<td style="text-align: left;">extracted_text_nchar</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Characters of the text read from the page,
before anonymization; NA without text.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Plan id: a SHA-256 hash of
course_plan_normalized. A plan is a plan_content_id, institution and
course_code.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">year_from</td>
<td style="text-align: left;">year_from</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">First year of the offerings with this plan
(double up to data-2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Last year of the offerings with this plan
(double up to data-2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The plan’s earliest offering: course_plan
and the sections are read from its page.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_plan</td>
<td style="text-align: left;">course_plan</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The plan’s text, anonymized
(course_plan_spec.md).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_plan_normalized</td>
<td style="text-align: left;">course_plan_normalized</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">course_plan lower-cased, without
structural headings and labels, whitespace squished
(normalize_plan_text()): the text the id is made from. Not for
reading.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">section</td>
<td style="text-align: left;">section</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">One of seven sections
(section_codebook.yml): course_content, learning_outcomes,
prerequisites, teaching_methods, coursework_requirements, assessment,
reading_list.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">text</td>
<td style="text-align: left;">text</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The section’s text, anonymized.</td>
</tr>
</tbody>
</table>

## Institution-Specific Notes

**NORD.** The URL needs Norwegian characters in the semester parameter
(`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic “Gjeldende
emnebeskrivelse” page without year-specific content. The site maps each
semester to its academic year (`VÅR&year=2023` gives the 2022/23 plan).
For a year it has no plan for, it shows the current plan, headed
“Gjeldende emnebeskrivelse (sist oppdatert 2026/27)”; `page_year` in the
config reads that year, and an offering whose page shows a later year
gets no plan. Pattern:
`https://www.nord.no/studier/emner/{code}?year={year}&semester={HØST|VÅR}`.

**NTNU.** The URL has the year but not the semester. The year is the
academic year the page is for (`/2025` is “Studieår 2025/2026”), so a
spring or summer offering reads the year before
(`url_year = "academic"`; HVL and OsloMet likewise), and
`read_harvest()` gives an offering the page at its URL whichever
offering it was fetched for. DBH’s “-A” codes (MGLU1101-A) are the
site’s code without the suffix. A page saying no information is
available raises `ntnu_no_info_error`. robots.txt asks for a 10-second
crawl delay (`request_delay` in the config). The page’s exam tab
(`#omEksamen`: assessment form, grading scale, exam sessions and their
parts) is read without its link, rooms, comments, dates and times; the
assessment section gets one session of it (`exam_sessions`). The
timetable tab (`#timeplan`, from 2025) is not read.

**UiO.** UiO publishes only the latest version of each course plan: the
base URL always returns the current plan and older versions cannot be
reached. Semester URLs (`/h24/`, `/v25/`) hold logistics only (teachers,
timetable, exam dates), not the plan, so they are not used. Data should
be filtered to the current year. The URL holds a faculty and department
folder that DBH does not give:
`https://www.uio.no/studier/emner/{faculty}/{department}/{CODE}/`.
`resolve_urls_uio_batch()` reads the folders from UiO’s sitemap
(`uio_course_folders()`), which lists every course taught now (the same
courses as the listing `/studier/emner/alle/`). A course not taught now
keeps its page but is not listed: it gets the folder that most listed
courses with its code prefix (`ENG`) have, else the one of its DBH
department (`department_code`); a wrong guess is a 404. A few folders
are named in lower case (`sv/iss/sos9009`), and the URL must be too; Ø
is O (`MØNA2504` is in `hf/ikos/MONA2504/`).

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
1-3 and stops early when the course code is missing from the page (no
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
`window.location.hash`, which avoids a 2-3 s browser start per URL. It
waits the config’s `request_delay` after each page (5 seconds, the
Crawl-delay of USN’s robots.txt) before it reads it and asks for the
next. A semester without a plan of its own shows another semester’s plan
under the banner “Emneplanen for dette semesteret er ikke klar ennå”.
When the next candidate shows the same plan, the banner stays until the
page is reloaded, so a page with the requested year and the banner is
reloaded. HTML is captured during resolution, so USN has no separate
fetch step.

**UiT.** Historical plans need a document id:
`https://uit.no/utdanning/emner/emne?p_document_id={ID}`. The active
page (`/utdanning/aktivt/emne/{CODE}`) has a `<select>` listing every
semester with its document id (option text `{CODE}: H 2020` /
`{CODE}: V 2021`), also for discontinued courses. A semester listed
twice can be an English document (“Course code:”) beside the Norwegian
one; `uit_one_per_semester()` fetches those and keeps the first
Norwegian one. The same CSS selector works for active and historical
pages.

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
<td><code>harvest_all(courses, year, refetch, institutions, retry)</code></td>
<td><code>R/harvest.R</code></td>
<td>Harvest all (or the given) institutions; each harvest adds a run,
<code>data/raw/{inst}/{year}/{date}.parquet</code></td>
</tr>
<tr>
<td><code>write_raw_run(rows, institution, run)</code>,
<code>read_raw(files)</code></td>
<td><code>R/utils.R</code></td>
<td>Write a harvest run to the raw store; read raw files in harvest
order</td>
</tr>
<tr>
<td><code>fetch_ledger(rows, institution, run)</code>,
<code>raw_ledger(files)</code></td>
<td><code>R/utils.R</code></td>
<td>A run’s fetch ledger; the ledger of raw files, recorded or
reconstructed (target <code>ledger</code>)</td>
</tr>
<tr>
<td><code>fetch_outcome(url, page, status, error)</code>,
<code>offering_outcome(course_id, url, ledger)</code></td>
<td><code>R/utils.R</code></td>
<td>Outcome class of a fetch; how an offering’s URL answered last
(retries)</td>
</tr>
<tr>
<td><code>site_request(url)</code></td>
<td><code>R/fetch_html_cols.R</code></td>
<td>A request to a course site: user agent, timeout, retries of timeouts
and server errors</td>
</tr>
<tr>
<td><code>pilot_sample(institution, oldest_year)</code>,
<code>pilot_gate(institution)</code></td>
<td><code>R/harvest.R</code>, <code>R/pipeline_metrics.R</code></td>
<td>The pilot of an institution: its stratified sample of DBH
course-years; the gate numbers of the sample in the build
(<code>gate_numbers()</code>)</td>
</tr>
<tr>
<td><code>fetch_coverage(frame, ledger, offerings)</code>,
<code>page_changes(ledger, files, config)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Fetch outcome, plan and retry of every offering of a frame; pages
changed between runs (targets <code>coverage</code>,
<code>changed_pages</code>)</td>
</tr>
<tr>
<td><code>raw_partitions()</code>,
<code>read_raw(files, content = FALSE)</code>,
<code>raw_page_id(file, row)</code></td>
<td><code>R/utils.R</code></td>
<td>The raw store’s institution x year partitions; raw rows with the
page id in place of the page</td>
</tr>
<tr>
<td><code>partition_blocks()</code>, <code>partition_text()</code>,
<code>partition_sections()</code></td>
<td><code>R/pipeline.R</code></td>
<td>Per raw partition: every page’s blocks, text and plan
(<code>page_plans()</code>), and the sections of the pages plans are cut
from</td>
</tr>
<tr>
<td><code>plan_offerings(pages, page_text, config)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Every offering with the text, <code>course_plan</code> and plan id
of its page (target <code>courses</code>)</td>
</tr>
<tr>
<td><code>frame_members()</code>, <code>frame_plans()</code>,
<code>frame_sections()</code>, <code>frame_offerings()</code>,
<code>frame_dir()</code></td>
<td><code>R/pipeline.R</code>, <code>R/utils.R</code></td>
<td>The frames: their offerings, plans (<code>plan_table()</code>),
sections and data folders</td>
</tr>
<tr>
<td><code>harvest_files(institution)</code>,
<code>harvested_institutions()</code></td>
<td><code>R/utils.R</code></td>
<td>An institution’s raw files, run by run; the institutions with raw
files</td>
</tr>
<tr>
<td><code>dbh_courses(frame, date)</code></td>
<td><code>R/utils.R</code></td>
<td>DBH offerings of a snapshot: all courses, or the teacher-ed frame
(<code>"teacher_ed"</code>)</td>
</tr>
<tr>
<td><code>dbh_rename(df)</code></td>
<td><code>R/utils.R</code></td>
<td>DBH’s column names (snapshots, raw files and releases until
2026-10-16) as the pipeline’s (<code>DBH_COLUMNS</code>)</td>
</tr>
<tr>
<td><code>institution_from_code(code)</code></td>
<td><code>R/institution_config.R</code></td>
<td>Institution short name for DBH institution codes</td>
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
<td>URL discovery (USN, UiT, hivolda, UiO) with checkpointing</td>
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
<td><code>read_harvest(files, config, content)</code></td>
<td><code>R/pipeline.R</code></td>
<td>One institution’s raw files combined (<code>harvest_files()</code>):
the page each offering takes; current sites matched by course code and
academic year</td>
</tr>
<tr>
<td><code>harvested_rows(institutions)</code></td>
<td><code>R/pipeline.R</code></td>
<td><code>read_harvest()</code> joined with
<code>data/interim/extracted_text.RDS</code> (audit scripts)</td>
</tr>
<tr>
<td><code>finalize_release(tag)</code>,
<code>frozen_changes()</code></td>
<td><code>R/release.R</code></td>
<td>Lock a release’s raw files; list changes to it (target
<code>frozen_check</code>)</td>
</tr>
<tr>
<td><code>release_data(tag, dest, frame)</code></td>
<td><code>R/release.R</code></td>
<td>Copy the published files of a frame (the teacher-ed one),
<code>methods.md</code>, <code>data/variables.csv</code>, a manifest and
section coverage to the coding project</td>
</tr>
<tr>
<td><code>plan_gaps(offerings, fetch_status)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Course-years without a plan, with the reason (target
<code>gaps</code>)</td>
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
<td><code>plan_ids(id_text)</code>, <code>plan_table(courses)</code>,
<code>deduplicate_plans(df)</code></td>
<td><code>R/deduplicate_plans.R</code></td>
<td>Content hashes (<code>plan_content_id</code>); the unique plans of a
set of offerings</td>
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
<tr>
<td><code>wayback_cdx(url, match_type, from, to, filter, collapse)</code>,
<code>wayback_capture_url(timestamp, original)</code></td>
<td><code>R/wayback.R</code></td>
<td>Captures of a URL or URL prefix in the Wayback Machine; a capture’s
address as archived. Not used by the harvest</td>
</tr>
</tbody>
</table>

## Troubleshooting

**URLs look wrong?** - Check the institution’s builder in
`R/add_course_url.R` - Print a few:
`courses |> filter(institution == "inst") |> add_course_url() |> select(url) |> head()`

**HTML download fails?** - Check if the website is accessible in a
browser - Count the outcomes:
`raw_ledger(harvest_files("inst")) |> dplyr::count(outcome, http_status)` -
Look at `html_error` for error messages - Some institutions rate-limit
or block automated requests - For NTNU, check whether the page says no
information is available

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

**“the site refuses the harvest (HTTP 429 …)”?** - The site answered 403
or 429, so its harvest stopped; the pages fetched so far are in the
checkpoint. Harvest it again later (the checkpoint resumes), at night,
or with a longer `request_delay` in `R/institution_config.R`

**Checkpoint file is huge?** - This is normal - HTML is large -
Checkpoint files are in `.gitignore`; deleting one makes the next run
fetch again the offerings that are not yet in a raw file (never the ones
that are)

**“raw files are not written twice”?** - A run of that name exists: two
harvests of the same institution in the same second, or a run written by
hand. The raw store is append-only; run the harvest again

**“offerings in more than one raw file”?** - Two runs of an institution
hold the same `course_id` (not a retry); a file was copied into
`data/raw/` by hand. Remove the copy

## File Organization

    ├── _targets.R                 # Post-harvest pipeline: targets::tar_make()
    ├── methods.qmd                # Methods choices for articles (rendered to methods.md)
    ├── course_plan_spec.md        # What course_plan and the sections hold, and what stays out
    ├── renv.lock                  # Pinned package versions (renv::restore())
    ├── .github/workflows/tests.yml # Tests on GitHub Actions
    ├── R/
    │   ├── harvest.R              # Entry points: harvest_institution(), harvest_all(); pilot_sample()
    │   ├── harvest_strategies.R   # Strategy implementations
    │   ├── institution_config.R   # Institution registry (strategy, selectors, overrides, section_*)
    │   ├── utils.R                # Helpers (data paths, add_course_id, validation, normalization)
    │   ├── add_course_url.R       # URL generation logic
    │   ├── resolve_course_urls.R  # URL discovery for USN, UiT, hivolda, UiO
    │   ├── fetch_html_cols.R      # HTML downloading (httr2)
    │   ├── extract_fulltext.R     # Config-driven text extraction
    │   ├── checkpoint.R           # Checkpoint management
    │   ├── anonymize.R            # PII removal: anonymize_text() for course_plan and sections
    │   ├── normalize_plan_text.R  # Lossy normalization for dedup hashing
    │   ├── deduplicate_plans.R    # Groups identical plans by content hash
    │   ├── section_heading_map.R  # Heading → section patterns (incl. ".drop")
    │   ├── extract_sections.R     # Section extraction strategies + cleanup
    │   ├── pipeline.R             # Steps of the {targets} pipeline (_targets.R)
    │   ├── pipeline_metrics.R     # Regression snapshot: check_pipeline_metrics(); the pilot gate numbers
    │   ├── release.R              # release_data(), finalize_release(), frozen_changes()
    │   ├── wayback.R              # Wayback Machine captures (CDX API); not used by the harvest
    │   └── audit/                # Audit harness for /audit-institutions
    ├── app/
    │   ├── course_browser/        # Shiny concordance browser; build_data.R → data/ (see its README)
    │   └── course_browser_ojs/    # Static Observable JS browser; build_data.R → data/ (see its README)
    ├── data/                      # README "Data Files: Published and Internal"
    │   ├── input/                 # DBH snapshots dbh_{courses,programmes}_{date}.parquet (in git)
    │   ├── pilot/                 # Pilot samples {inst}.csv (in git)
    │   ├── raw/                   # Harvest: {inst}/{year}/{run}.parquet, {inst}/ledger/, checkpoint/ (not in git)
    │   ├── interim/               # Rebuilt text with personal data (not in git); teacher_ed/: that frame's
    │   ├── processed/             # Anonymized data files, can be shared (not in git); teacher_ed/: that frame's
    │   ├── releases/              # Finalized releases' data (not in git)
    │   ├── backup/                # Earlier harvests, not read (not in git)
    │   ├── audit/                 # Audit findings and reports
    │   ├── variables.csv          # Every variable of the data files, old and new names
    │   ├── robots_txt.csv         # Each host's robots.txt: date read, crawl delay, disallowed paths
    │   └── data_notes.qmd         # Data quality notes per institution
    ├── tests/
    │   ├── testthat/              # Rscript -e 'testthat::test_dir("tests/testthat")'
    │   └── snapshots/             # pipeline_metrics_{frame}.csv; raw_manifest.csv once a release is finalized
    └── data-raw/
        ├── courses.R              # script that writes a DBH snapshot to data/input/
        └── raw_to_parquet.R       # the one-time conversion of the RDS raw files (2026-10-16)

## Need Help?

Look at the existing institution examples, but be quick to contact me,
also! 🤩
