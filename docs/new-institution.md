# Adding and Piloting an Institution


## Adding a New Institution

An institution is added in steps: read its robots.txt, give it a config
entry and a way to find its URLs, test that on a few courses, then pilot
it on a sample ([“Piloting an Institution”](#piloting-an-institution))
and harvest all its courses. In Claude Code, `/add-institution` follows
this page.

### Before You Start

Find out:

1.  **The short name**, lower case (`newuni`). The ten institutions
    surveyed for the all-courses frame (kristiania, bi, nmh, nhh, vid,
    himolde, khio, fjellhaug, fhs, phs) have one in
    `data/robots_txt.csv`: use it.

2.  **The DBH institution code.** With the pipeline’s functions loaded
    (README [“Quick
    Start”](../README.md#quick-start-running-the-pipeline)), search the
    institution names in the latest snapshot; an institution that merged
    or was renamed may have several codes, each with its own years:

    ``` r
    courses <- dbh_courses()   # every DBH course of the latest snapshot
    courses |>
      dplyr::filter(grepl("Handelshøyskole", institution_name)) |>
      dplyr::summarise(first = min(year), last = max(year), offerings = dplyr::n(),
                       .by = c(institution_code, institution_name))
    ```

3.  **Whether the plans are public.** The harvest reads what a site
    shows without a login. If the full plans are behind a login, or the
    public pages hold only an excerpt of them, ask the project lead
    before going on: what is harvested decides what the data hold
    (`course_plan_spec.md`).

4.  **How a course’s page is found**, from a few course pages of
    different years, levels and faculties:

    - from the course code, year and semester (a URL builder, step 3);
    - from a list on the site, such as a course list, a sitemap or a
      menu of a course’s semester pages, when the URL holds something
      DBH does not give (a slug made from the course name, a document
      id, a faculty folder): URL discovery ([“Special
      Cases”](#special-cases));
    - whether the URL holds the year. A site that shows only the plan in
      force, at a URL without a year, gives the plans of the academic
      year it is harvested in and no earlier ones ([“Sites that show
      only the current
      plan”](harvesting.md#sites-that-show-only-the-current-plan));
    - the oldest year the site has plans for.

5.  **The page’s parts**: the element that holds the course plan with
    its title and facts box, the parts inside it that are not the plan
    (navigation, contact boxes with staff names, widgets), and the
    elements of the title and facts box (step 2).

### 1. Read robots.txt First

`data/robots_txt.csv` holds the robots.txt of every host the harvest
requests (see [“robots.txt”](harvesting.md#robotstxt)). Find the hosts
the harvest will request: the course pages, plan PDFs and discovery
pages (a course list can be on another host). For a host with no row, or
a row more than a month old, read its robots.txt once with the harvest’s
user agent, before any other request to the host:

``` r
source("R/fetch_html_cols.R")
site_request("https://www.newuni.no/robots.txt") |>
  httr2::req_error(is_error = \(resp) FALSE) |>
  httr2::req_perform() |>
  httr2::resp_body_string() |>
  cat()
```

Then add or update the host’s row:

- `institution`: the short name; `tests/testthat/test-robots.R` fails if
  a configured institution has no row.
- `read_on` and `status`: the date and the HTTP status. A 404 means
  nothing is disallowed; a 5xx means the host may not be crawled at all.
- The rules for `User-agent: *` apply: no robots.txt names the harvest’s
  user agent, and a group for a named bot (Scrapy, GPTBot) does not.
- `crawl_delay`: the `Crawl-delay` asked of all user agents. A delay
  above 3 seconds goes in the config as `request_delay` (step 2); the
  test fails if `request_delay` is shorter.
- `disallow` and `allow`: the `Disallow` rules that touch course-plan
  paths (course pages, plan PDFs, discovery pages) and the `Allow` rules
  that override them, as robots.txt path patterns separated by spaces
  (`/dokumenter/`, `/*.pdf$ /*.doc$`).
- `note`: the rest of the robots.txt, in words.

The harvest applies the rules: a URL they disallow is not requested
(outcome `robots`), and a host with no row stops the harvest before its
first request to it. If the plans are on paths robots.txt disallows,
note it in the institution’s notes in `data/data_notes.qmd` and ask the
project lead. Such paths are requested only with the institution’s
permission, recorded in its config as `robots_permission`
([“robots.txt”](harvesting.md#robotstxt)).

### 2. Add a Config Entry to `R/institution_config.R`

``` r
# Add to the institution_configs list:
newuni = list(
  code = "1234",                  # DBH code; several if it changed: c("0264", "1177")
  strategy = "standard",          # "url_discovery" if the URL must be looked up
  selector = ".main-content",     # the element that holds the course plan (first match)
  exclude = ".contact",           # optional: parts of it that are not the plan
  facts = c("h1", ".facts"),      # the title and facts box
  plan_years = "url",             # year in the URL; "current" if the site shows only the plan in force
  # url_year = "academic",        # if the URL's year is the academic year (ntnu, hvl, oslomet, uis)
  # request_delay = 10,           # if robots.txt asks a Crawl-delay above 3 seconds
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

`selector` names one element (only the first match is read; a list
separated by commas takes the first that matches, as hiof’s). Choose the
narrowest element that holds the whole plan with its title and facts box
(`course_plan_spec.md`), and leave out navigation, contact boxes (staff
names) and widgets inside it with `exclude` (hvl, nord, uib, uio, uis,
mf, hivolda do this). Name the title and facts box in `facts`: they stay
in the plan’s text but out of the sections and the plan id. `selector`,
`exclude` and `facts` take CSS, or XPath when they start with `/`. A
site read as text (`section_strategy = "text"`) has no `facts`:
`section_text_header` matches its metadata lines instead.

The page is read once into blocks (`R/blocks.R`); the plan’s text and
its sections are both made from them, so `selector` and `exclude` decide
what text the plan has, and the `section_*` fields only how it is split.
Pages built from accordions take `section_heading_selector = "summary"`
(or the accordion’s trigger) and `section_scope = "details"` (or the
accordion item); paragraph sub-headings take
`section_subheading_selector = "p"`. [“Blocks and
Sections”](sections.md#blocks-and-sections) lists every `section_*`
field with the institutions that use it.

The other fields are described at the top of `R/institution_config.R`,
each with the institutions that use it:

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Field</th>
<th>When</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>request_delay</code></td>
<td>robots.txt asks a <code>Crawl-delay</code> above the default 3-5
seconds</td>
</tr>
<tr>
<td><code>user_agent</code></td>
<td><code>"browser"</code> if the site refuses the harvest’s user agent
(hiof: 403)</td>
</tr>
<tr>
<td><code>url_year</code></td>
<td><code>"academic"</code> if the URL’s year is the academic year, so a
spring offering reads the year before</td>
</tr>
<tr>
<td><code>page_term</code>, <code>page_end_term</code></td>
<td>the page says which semesters its plan version is for, and that can
differ from the URL’s (inn, nord, uis, hiof)</td>
</tr>
<tr>
<td><code>english_page</code></td>
<td>the Norwegian page of a course taught in English only says the plan
is on the English page (hvl, nmbu, nord, ntnu)</td>
</tr>
<tr>
<td><code>fetch_fn</code></td>
<td>the site answers a missing plan with a normal page (<a
href="#special-cases">“Special Cases”</a>)</td>
</tr>
<tr>
<td><code>post_fn</code></td>
<td>text to cut after the page is read (uit); like <code>exclude</code>
and the anonymizer it may remove only personal data and boilerplate</td>
</tr>
<tr>
<td><code>exam_sessions</code></td>
<td>the exam tab lists a session per semester (ntnu)</td>
</tr>
<tr>
<td><code>robots_permission</code></td>
<td>the institution has let the project harvest paths its robots.txt
disallows (step 1)</td>
</tr>
</tbody>
</table>

### 3. Add a URL Builder to `R/add_course_url.R`

Add a case to the `dplyr::case_match()` in `add_course_url()` and a
function that builds the URL from the offering’s DBH columns
(`course_code`, the code without DBH’s trailing version number;
`course_code_raw`; `year`; `semester_name`, “Vår” or “Høst”):

``` r
# In add_course_url(), add:
"newuni"  ~ add_course_url_newuni(course_code, year, semester_name),

# and at the end of the file:
add_course_url_newuni <- function(course_code, year, semester) {
  glue::glue("https://www.newuni.no/courses/{year}/{semester_to_url(semester)}/{tolower(course_code)}")
}
```

Check on the site:

- the semester’s form in the URL: `semester_to_url()` (`R/utils.R`)
  gives `var` and `host`; others need their own
  (`dplyr::case_match(semester, "Vår" ~ "spring", "Høst" ~ "autumn")`);
- whether the year is the academic year:
  `academic_start_year(year, semester)` gives it, and the config gets
  `url_year = "academic"`;
- upper or lower case codes, and Norwegian letters: nih and nmbu drop
  them as the CMS does (KRØ100 → kro100);
- DBH codes that differ from the site’s (uia: variant suffixes such as
  `-G`), or codes with a space (uib: percent-encoded);
- years without plans: return `NA_character_` for them (inn before
  2022);
- older years at another address (hiof’s `historiske-emner/`).

An institution whose URL is looked up instead returns `NA` here
(`"newuni" ~ NA_character_`, as uio, uit, usn) and gets a resolver
([“Special Cases”](#special-cases)).

### 4. Test on a Few Courses

Load the pipeline’s functions as in README [“Quick
Start”](../README.md#quick-start-running-the-pipeline), and harvest a
few offerings with `harvest_institution()`, which only returns the rows:
only `harvest_all()` writes the raw store.

``` r
courses <- dbh_courses()   # the institution's code must be in its config
few <- courses |>
  dplyr::filter(institution == "newuni", year == 2025) |>
  dplyr::slice_sample(n = 5)
result <- harvest_institution("newuni", few)

result |> dplyr::select(course_code, url, html_success, html_error) |> print()
cat(result$extracted_text[1])   # the plan's text

# How the page is read and split: the headings with the section each maps to
# (NA: unmapped; add a pattern to R/section_heading_map.R), then the sections
source("R/extract_sections.R")
cfg <- .block_cfg(get_institution_config("newuni"))
b <- page_blocks(result$html[1], result$extracted_text[1], cfg)
b[b$role != "text", c("role", "text", "section")]
page_sections(b, result$extracted_text[1], cfg)
```

The test writes checkpoints (`data/raw/checkpoint/newuni/`). Pages are
kept by URL, so a changed URL builder fetches the new URLs; after
changing a resolver, test with `refetch = TRUE`, which removes the
institution’s checkpoints.

For a standard site no change to `R/extract_fulltext.R` or
`R/harvest_strategies.R` is needed.

### 5. Pilot, Record, Full Harvest

Pilot the institution on a sample of its courses ([“Piloting an
Institution”](#piloting-an-institution)); its full harvest starts when
the pilot audits find it ready. Along the way, record it where people
look for it:

- `data/data_notes.qmd`: a section under “Per-Institution Notes”, with
  what the harvest cannot get (years without plans, paths robots.txt
  disallows, layouts left without sections);
- `docs/institutions.qmd`: the URL pattern and quirks, if any;
- a site that shows only the current plan: add it to the list of such
  sites in `docs/harvesting.qmd` ([“The Yearly
  Harvest”](harvesting.md#the-yearly-harvest), step 1, and [“Sites that
  show only the current
  plan”](harvesting.md#sites-that-show-only-the-current-plan)), since it
  must be harvested every academic year;
- `institution_labels` in `app/course_browser/R/data.R`: its label in
  the browser;
- `methods.qmd`: the institutions with a config are part of the frame
  (“Population and sampling frame”), so a new one gets an entry there.

## Special Cases

**The URL must be looked up (URL discovery).** When the URL holds
something DBH does not give, such as a slug made from the course name or
a document id:

1.  Set `strategy = "url_discovery"` in the config and return
    `NA_character_` in `add_course_url()` (step 3).
2.  Write a resolver in `R/resolve_course_urls.R`: a function of the
    offerings (`course_code`, `year`, `semester_name`, …) that returns
    them with `url` filled where a page was found, and `NA` where none
    was. Add it to the dispatch in `resolve_batch()` inside
    `resolve_course_urls()`.
3.  Send every request with `site_html()` or `site_request()`
    (`R/fetch_html_cols.R`), which apply the user agent, the retries and
    robots.txt, and wait `polite_pause(config$request_delay)` between
    requests.

The resolvers there are models. A list that holds every course: UiO’s
sitemap (`resolve_urls_uio_batch()`) and HiOF’s folder listings
(`vortex_course_index()`) are read once per harvest and matched by
course code, as a course list whose links end in the code would be. A
page per course code that lists its semester pages: UiT and Volda
(`resolve_urls_uit_batch()`, `resolve_urls_hivolda_batch()`), which read
each code once through `discover_codes()`, so a harvest of many years
does not ask twice. `resolve_course_urls()` keeps the URLs found in the
institution’s `urls` checkpoint, so a harvest that stops resumes there.

**The site shows only the current plan.** Set `plan_years = "current"`
(mf; uio with URL discovery). A harvest then fetches the latest DBH year
only, and its pages serve the offerings of the academic year it is made
in; the pilot’s gate numbers and audits wait for a DBH snapshot with
that year ([“Piloting an Institution”](#piloting-an-institution), end).
Such a site must be harvested once in every academic year (step 5).

**The page is rendered by JavaScript** (USN, Shadow DOM). The HTML the
server sends has no plan in it. USN’s pages are rendered in Chrome by
its resolver (`resolve_urls_usn_batch()`), which stores their text; the
`shadow_dom` strategy reads that text (`section_strategy = "text"`). See
USN in [Institution-Specific Notes](institutions.md). A site like it
needs a resolver of its own.

**The plans are in PDFs or in JSON.** The strategies other than
`standard` and `url_discovery` are written for one institution each:
`html_pdf_discovery` (UiS: the semester menu, then an HTML page or a
PDF), `pdf_split` (Steiner: a PDF per subject, split into courses),
`json_extract` (NLA: the plan in the page’s JSON), `shadow_dom` (USN)
and `noop` (Samas: nothing to fetch). A site like one of them needs a
strategy function of its own in `R/harvest_strategies.R`, modelled on
it, and a case in the `switch()` of `harvest_institution()`
(`R/harvest.R`). Years published only as PDFs, next to HTML pages for
later years, are left out until such a function exists: set the oldest
year to the first HTML year and note the rest in `data/data_notes.qmd`.

**A missing plan comes as a normal page.** A site that answers a year
without a plan with a page that says so (NTNU: “Det finnes ingen
informasjon for dette studieåret”), or with an error page under HTTP 200
(HVL), needs a `fetch_fn`: a function of the URL that requests it with
`site_request()` (with the browser user agent if the site needs it:
`user_agent` does not apply to a `fetch_fn`), raises an error with the
response’s `http_status` when the page says the plan is missing, and
returns the response otherwise (`fetch_html_cols_single_ntnu()`,
`fetch_html_cols_single_hvl()` in `R/fetch_html_cols.R`). The offering
then counts as a fetch error, not as a plan.

**The plan is spread over several parts of the page.** Use the narrowest
element that holds all of them, the title and facts box included
(`course_plan_spec.md`), as `selector`, and leave out what lies between
them with `exclude` (hvl: `div.l-body` minus the year stamp, the year
links and, in the facts aside, the course coordinators and the exam
schedule).

## Piloting an Institution

Before the full harvest of an institution, a new one or a configured one
going from teacher education to all its courses, a pilot harvests a
sample of its courses and checks the config, the URLs and the headings
on it: about 200 pages instead of tens of thousands. The full harvest
starts when the pilot audits find the institution ready (step 4). Load
the functions as in [“Quick
Start”](../README.md#quick-start-running-the-pipeline), then:

1.  **Draw the sample.** First read the robots.txt of each host the
    harvest requests, if its row in `data/robots_txt.csv` is more than a
    month old (“Adding a New Institution”, step 1; the harvest stops at
    a host not in the table). Then find the oldest year the site has
    plans for.

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
    builder, the heading map (`R/section_heading_map.R`) or, for
    personal data the generic rules miss, the institution’s rules in
    `R/anonymize.R`; rebuild with `tar_make()`, and audit the
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
