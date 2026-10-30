# Harvesting


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
    nothing (`noop`), and so does Steiner as long as its robots.txt
    disallows its PDFs and it has given no permission (see
    “robots.txt”).

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
    already harvested keeps its `course_id` (see [“Input
    Data”](data-files.md#input-data-datainputdbh__dateparquet)).

3.  **Harvest.** `harvest_all()` fetches every offering of the snapshot
    that no raw file holds: the new year, and rows DBH added to earlier
    years since the last snapshot. `harvest_all(years = 2026)` fetches
    the new year only. A site that shows only the current plan gets
    another snapshot, now with the new year’s courses. At the slowest
    sites this takes days, so run institutions side by side (below). Raw
    files exist only on this machine: back up `data/raw/` once the
    harvest is done.

4.  **Rebuild and check.** Run `targets::tar_make()` and read the
    targets of [“Rebuilding Derived
    Data”](rebuilding.md#rebuilding-derived-data):

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
    harvest_all(years = 2026, retry = TRUE)
    targets::tar_make()
    ```

    Update `methods.qmd` where it names the snapshot, the years or the
    harvest dates (“Population and sampling frame”, “Harvest”), and
    `data/data_notes.qmd` where a site changed.

5.  **Audit.** In Claude Code, `/audit-institutions fulltext`, then
    `sections` and `anonymization` (see [“Auditing a Pipeline
    Step”](rebuilding.md#auditing-a-pipeline-step)), for all
    institutions or those whose site changed. The skill has no year
    option: its packets sample each institution’s courses of all years.
    A finding is fixed in the config or the heading map and rebuilt with
    `tar_make()`, without a new harvest.

6.  **Release.** Tag the commit and finalize the release (see [“Raw
    Store and Finalized
    Releases”](rebuilding.md#raw-store-and-finalized-releases)):

    ``` r
    # after git tag data-YYYY-MM-DD
    source("R/utils.R"); source("R/release.R")
    finalize_release("data-YYYY-MM-DD")   # then commit tests/snapshots/raw_manifest.csv
    ```

    The hand-over to the coding project,
    `release_data("data-YYYY-MM-DD")` (into
    `../TEPS_course_content_coding/data`), is the project lead’s
    decision and step, because the coders work from that repository.

## Several Institutions at Once

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
  checkpoints (`data/raw/checkpoint/{inst}/`), raw folder, run names and
  ledger, so processes for different institutions do not collide. Never
  harvest one institution in two processes at once: they would share its
  checkpoint, and the build stops at an offering in two runs.
- **One process per host.** A process pauses between its own requests
  (3-5 seconds, or the institution’s `request_delay`), so two processes
  on one host double the load on it. Institutions on a shared host (a
  common course catalogue) go in one process, `institutions = c(...)`,
  which harvests them one after another. Each configured institution has
  a host of its own today. Read the robots.txt of a new site before its
  first harvest: a `Crawl-delay` goes in `request_delay`, and the date,
  delay and disallowed paths in `data/robots_txt.csv`; the harvest of a
  host not in it stops (see “robots.txt”).
- **At night.** Start long harvests in the evening, so that most
  requests come outside the sites’ busy hours. A site with the year in
  the URL can be stopped (end the process) and started again: the years
  written are skipped, and the checkpoint, saved every 50 pages, resumes
  the year it stopped in. So does a “current” site within the academic
  year it started in (from 1 August); a later start fetches all its
  pages again. A site that refuses the harvest (HTTP 429, or 403 to 5
  requests in a row and to a page known to be public) stops its own
  harvest; start it again later.
- **Memory and the build.** A process keeps a year’s pages in memory
  until it writes that year’s run (NTNU: up to 13,016 offerings a year),
  so check free memory before starting more of them. `tar_make()` (4
  workers) builds the runs written so far; run it when the harvests are
  done, or audit only the institutions whose harvest has ended.

## What Happens Under the Hood

1.  **`get_institution_config()`**: Looks up strategy, CSS selectors,
    and overrides
2.  **`apply_year_filter()`**: Filters to relevant years based on config
3.  **`add_course_id()`**: Creates unique identifiers for each course
4.  **`add_course_url()`**: Generates the correct URL for each course
5.  **Strategy dispatch**: Routes to the right harvesting strategy
    (standard, URL discovery, etc.)
6.  **`ensure_output_columns()`**: Guarantees uniform output shape

## About Checkpointing

The pipeline uses **checkpointing** to avoid re-downloading data. If the
script stops or crashes, or a site refuses the harvest (HTTP 429, or 403
to 5 requests in a row and to a page known to be public; see “Fetch
Ledger and Retries”), the next harvest resumes where it stopped
(`R/checkpoint.R`):

- `harvest_all()` harvests an institution one DBH year at a time, newest
  first, and writes each year as its own run, so memory holds one year’s
  pages.
- Each URL is fetched once, and its page joined to every offering with
  that URL: an academic-year page serves an autumn offering and the next
  spring’s (NTNU’s 255,055 offerings in the DBH snapshot of 2026-10-16
  have 137,474 URLs). The raw run and the fetch ledger keep one row per
  offering.
- The pages fetched are saved every 50 in
  `data/raw/checkpoint/{inst}/{year}/html/`, keyed by URL, as
  append-only Parquet parts (`part-000001.parquet`, …; `url`, `html`,
  `html_error` as text, `html_success`, `http_status`, `requested_at`).
  A part is written as `.tmp` and renamed, so a crash loses fewer than
  50 pages and leaves at most a `.tmp`, which the next harvest deletes.
  A part is written without its trailing 403s (they wait for the next
  part), so the 403s of a refusal never reach it and the next harvest
  asks for those pages again.
- A harvest fetches only the URLs no checkpoint of the institution
  holds, also the year before’s: the spring offerings of year Y fetch
  the pages the autumn offerings of year Y-1 read.
- The URL discovery (`{inst}/urls/`), the course pages UiT and hivolda
  list their semester pages on (`{inst}/discovery/`, read once per
  course code: UiT’s 13,000 take 16 hours) and UiS’s PDFs
  (`{inst}/pdf/`) are kept for the institution, across its years.

The checkpoint only resumes an interrupted harvest; it is a cache, not
the record. Offerings already in a raw file are skipped by
`harvest_all()` itself (see [“Raw Store and Finalized
Releases”](rebuilding.md#raw-store-and-finalized-releases)). A year’s
html checkpoint is removed once its run and the next year’s run are
written, and the URL, discovery and PDF checkpoints once every year of
the harvest is: a later harvest asks for the course pages again, which
may list semesters published since. An error keeps them all. For
“current” sites (`plan_years`) and with `refetch = TRUE` or `retry`, the
institution’s checkpoints are removed before the harvest, because pages
change over time and an old copy must not stand in for a new one. The
RDS checkpoints of the harvests until 2026-10-28
(`data/raw/checkpoint/{type}_{inst}.RDS`) are not read; they held no
offering missing from the raw store.

## Fetch Ledger and Retries

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
“Course not found”); `forbidden` (HTTP 403 to this page among other
answers: a page closed to the public, such as some of MF’s course
pages); `shell` (the site answers that it has nothing for that year,
NTNU’s “No course information available for this academic year”);
`server_error` (HTTP 5xx); `timeout` (no answer: a timeout or a failed
connection); `blocked` (HTTP 429); `robots` (not requested: robots.txt
disallows it, see “robots.txt”); `error` (anything else); `no_url` (no
URL to request: the URL builder or the URL discovery found none). A row
with outcome `probe` and no `course_id` is a request of a page known to
be public, made to tell a site that refuses the harvest from pages
closed to the public (below); its `http_status` is the answer.

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
other answer is final. A site refuses the harvest when it answers 429
(too many requests), or 403 to 5 requests in a row (`FORBIDDEN_RUN`) and
then also to a page known to be public: its harvest stops, the pages
fetched before stay in the checkpoint (the next harvest resumes there
and asks again for the pages that answered 403 in the refusal), and
`harvest_all()` goes on with the next institution (`refusal_watch()`).
The page known to be public is one of the same host that answered 200
earlier in the harvest, else the host’s latest page in the raw store;
with none, 5 answers 403 in a row stop the harvest. The request waits
its pause and goes in the fetch ledger (outcome `probe`). A 403 among
other answers is the page’s own: it gets outcome `forbidden` and the
harvest goes on; so do 403s in a row when the public page answers 200
(MF’s PHI course pages, next to each other in a sample ordered by course
code).

**Retries between harvests:** `harvest_all(retry = TRUE)` fetches again
the offerings that have no plan in the current build
(`data/interim/course_offerings_full.RDS`, so run `tar_make()` first),
past the checkpoint, by how the URL the build gives them answered last,
for whichever offering it was requested (`offering_outcome()`): every
outcome but `not_found`, `forbidden` and `robots`, so a page not found,
closed to the public or disallowed by robots.txt is not asked for again,
and always a URL nobody has requested (a spring offering’s academic-year
page). `retry` can name the classes instead:
`retry = c("timeout", "server_error")` only failed fetches,
`retry = "not_found"` pages that may have been published since,
`retry = "forbidden"` pages that may have been opened since,
`retry = "robots"` the URLs an institution has since given permission
for. The retried offerings are saved as a new run, marked `retry`, and
`read_harvest()` gives a retried offering its latest row with a page. A
site that shows only the current plan is not retried; it takes a new
snapshot in each academic year.

``` r
harvest_all(institutions = "uia", years = 2023, retry = TRUE)
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

## robots.txt

The harvest requests nothing a site’s robots.txt disallows for all user
agents (`User-agent: *`; no robots.txt names the harvest’s user agent),
and waits at least the `Crawl-delay` it asks (`request_delay`).
`data/robots_txt.csv` holds the robots.txt of every host the harvest
requests, read once before the first request to the host
(add-institution skill, Step 0; [“Piloting an
Institution”](new-institution.md#piloting-an-institution), step 1):

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
<td><code>institution</code>, <code>host</code></td>
<td>the host and the institution whose pages are on it</td>
</tr>
<tr>
<td><code>read_on</code>, <code>status</code></td>
<td>the date robots.txt was read, and its HTTP status (404: no
robots.txt, nothing disallowed)</td>
</tr>
<tr>
<td><code>crawl_delay</code></td>
<td>the <code>Crawl-delay</code> asked of all user agents
(<code>tests/testthat/test-robots.R</code> checks
<code>request_delay</code> against it)</td>
</tr>
<tr>
<td><code>disallow</code>, <code>allow</code></td>
<td>the <code>Disallow</code> and <code>Allow</code> rules for all user
agents that touch course plans (course pages, plan PDFs, discovery
pages), as robots.txt path patterns separated by spaces:
<code>/dokumenter/</code>, <code>/*.pdf$ /*.doc$</code></td>
</tr>
<tr>
<td><code>note</code></td>
<td>the rest of the robots.txt, in words</td>
</tr>
</tbody>
</table>

`robots_allows()` (`R/fetch_html_cols.R`) matches a URL against its
host’s rules as RFC 9309 does: a pattern matches the path and query from
the start, `*` stands for any characters and a final `$` for the end;
the longest matching rule wins, `allow` on a tie. Since 2026-10-17:

- **A URL robots.txt disallows is not requested.** `site_request()`,
  which every request of the harvest goes through (course pages, URL
  discovery, UiS’s semester menu and PDF archive, Steiner’s subject
  PDFs), refuses it, and no pause is waited for it. The row gets the
  error “robots.txt disallows {url} (data/robots_txt.csv): not
  requested” and the outcome `robots` in the fetch ledger, with no
  `requested_at` or `http_status`. `harvest_institution()` says how many
  of the URLs it built robots.txt disallows, and UiS’s PDF step how many
  of its PDFs. USN’s pages, rendered in Chrome, are checked by their
  start page. This leaves out of new harvests UiS’s PDF archive
  (`www6.uis.no`, which allows only `/ansatt/`), so a UiS harvest gets
  plans only from the course pages on `www.uis.no`. Steiner’s five
  subject PDFs (`/dokumenter/`) are disallowed too, but fetched by the
  project lead’s decision (below). The plans harvested from both up to
  2026-10 stay in the raw store.
- **A permission.** When an institution lets the project harvest paths
  its robots.txt disallows, record it in its config:
  `robots_permission = "granted YYYY-MM-DD by <who> (<what>)"`
  (`R/institution_config.R`). Its URLs are then requested; those refused
  before are fetched with
  `harvest_all(institutions = ..., retry = "robots")`. A decision of the
  project’s own, without asking the institution, is recorded the same
  way but says so: Steiner’s reads “decided 2026-10-19 by the project
  lead, not granted by Steiner (not asked)” (the PDFs are open data and
  five files make negligible traffic). No institution has granted one
  yet.
- **A host not in the table stops the harvest** of its institution
  before any request to it (`harvest_all()` goes on with the next
  institution): read its robots.txt and add its row first. Only
  `/robots.txt` itself may be requested from a host not in the table.

## Sites that show only the current plan

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
`2026-09-30` (2026/27), both of DBH 2025 rows (`data/raw/{inst}/2025/`);
Steiner’s all-courses runs are `2026-10-19` and `2026-10-19_121636`
(2026/27). A harvest date comes from the `harvested_at` column or the
run’s name, never from a file’s modification time, which a copy changes.
The rest of the April backup, for sites with the year in the URL, is in
`data/backup/2026-04-04/` and is not read. Raw files exist only locally:
back them up.

A “current” site changes its pages over time, so a harvest started in a
later academic year (from 1 August) removes its checkpoints and fetches
all pages again; within the academic year recorded in
`data/raw/checkpoint/{inst}/academic_year` it resumes, as a “url” site’s
harvest does.
