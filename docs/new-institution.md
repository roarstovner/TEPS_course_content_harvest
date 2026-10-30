# Adding and Piloting an Institution


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
  plan_years = "url",             # year in the URL; or "current": see R/institution_config.R
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

## Piloting an Institution

Before the full harvest of an institution, a new one or a configured one
going from teacher education to all its courses, a pilot harvests a
sample of its courses and checks the config, the URLs and the headings
on it: about 200 pages instead of tens of thousands. The full harvest
starts when the pilot audits find the institution ready (step 4). Load
the functions as in [“Quick
Start”](../README.md#quick-start-running-the-pipeline), then:

1.  **Draw the sample.** First read the robots.txt of each host the
    harvest requests and update its row in `data/robots_txt.csv`: the
    date, the `Crawl-delay` asked of all user agents (`User-agent: *`)
    and the `Disallow` and `Allow` rules that touch course plans (see
    [“robots.txt”](harvesting.md#robotstxt); the harvest stops at a host
    not in the table). A `Crawl-delay` goes in `request_delay`
    (`tests/testthat/test-robots.R` checks it); a path it disallows is
    not requested and is noted in `data/data_notes.qmd`. Then find the
    oldest year the site has plans for.

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
    `anonymization` (see [“Auditing a Pipeline
    Step”](rebuilding.md#auditing-a-pipeline-step)): with `--pilot`, the
    packets show the sample’s courses only, and each synthesis reads the
    gate numbers and ends with a verdict on the institution: **ready for
    full harvest**, **ready with known limits** (each flagged number
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
    requested (see [“Fetch Ledger and
    Retries”](harvesting.md#fetch-ledger-and-retries)).

6.  **Full harvest** once all three audits find the institution ready,
    with or without known limits: `harvest_all(institutions = "ntnu")`
    fetches every other offering; the sample is in the raw store
    already. Start the slowest first, in the background, several
    institutions at once (see [“Several Institutions at
    Once”](harvesting.md#several-institutions-at-once)): NTNU and UiB,
    at 10 seconds a page (the Crawl-delay of NTNU’s robots.txt), take
    about 16 days each for all courses (NTNU 137,474 URLs for 255,055
    offerings, about 29 days if each offering were fetched; UiB 141,206
    URLs for 147,115).

A site that shows only the current plan gives the plans of the academic
year its pages were fetched in (see [“Sites that show only the current
plan”](harvesting.md#sites-that-show-only-the-current-plan)), so its
pilot’s pages join DBH’s offerings once the DBH snapshot has that year.
Until then only `fetched` can be read; the other gate numbers and the
audits wait for the new snapshot ([“The Yearly
Harvest”](harvesting.md#the-yearly-harvest), step 2). Samas fetches
nothing (`noop`), so it has no pilot.
