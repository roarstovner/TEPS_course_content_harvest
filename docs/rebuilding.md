# Rebuilding Derived Data and Releases


## Rebuilding Derived Data

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
each frame (see [“Frames”](data-files.md#frames)) in `data/interim/` and
`data/processed/` (see [“Data Files: Published and
Internal”](data-files.md#data-files-published-and-internal)),
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

To try a change to shared code without a full build, `preview_change()`
(`R/pipeline_metrics.R`) runs the per-partition steps on a sample of the
raw store (5 pages of each institution’s DBH years, or the pages of the
offerings named) with the code of a git ref and with the working tree,
and returns the plans and sections whose text differs, in seconds to a
minute:

``` r
targets::tar_load_globals()
preview_change(c("uib", "usn"))                            # against HEAD
preview_change("uib", n = 20, seed = 7)                    # another, larger sample
preview_change("uib", course_ids = "uib_HTEK101-0_2022_autumn_1")
```

It does not replace the full build: fixes tried this way are rebuilt
together with `tar_make()` (with no harvest running: the steps need the
memory), and each change `metrics_check` flags is explained in the
commit that caused it.

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

## Post-Harvest: Anonymization and Deduplication

The pipeline anonymizes and deduplicates the course plans in three
stages:

1.  **Anonymize** (`anonymize_text()`): Removes PII (teacher names,
    emails, phone numbers, staff lists “Name (Role)”, role labels with
    names, signature lines “Name, dekan”), dates, semester labels (a
    season with its year, “Høst 2024”, “H23”), and administrative year
    references (“Opprettet 2020”, “2023/2024”) from `extracted_text`,
    producing a readable `course_plan` column. Content years (e.g.,
    “etter 1945”, “NOU 2015:2”), a year that sets a condition (“avlagt
    studieåret 2013-2014 eller senere”) and a season alone after a
    semester label (“Undervisningssemester: Høst”) are preserved; the
    plan id and the sections leave the season out
    (`.drop_semester_values()`). Institution-specific handlers
    (`.anon_*()`) run first, then the generic cleanup
    (`.anon_generic()`).
2.  **Normalize** (`normalize_plan_text()`): Applies lossy transforms
    (lowercasing, heading synonyms, blanket year removal, whitespace
    squishing) on `course_plan` for content hashing
3.  **Deduplicate** (`plan_ids()`, `plan_table()`): Groups identical
    normalized texts under a shared `plan_content_id`, within each frame
    (see [“Frames”](data-files.md#frames))

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
<td>How the plan’s year is known: <code>url</code> or
<code>current</code> (<code>plan_years</code> in the config;
<code>page</code> for NLA in release data-2026-10-16)</td>
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

## Raw Store and Finalized Releases

The raw store is Parquet (zstd), one file per institution, DBH year
(`year`) and harvest run: `data/raw/{inst}/{year}/{run}.parquet`. A run
is named by its date, a later run of an institution the same day by date
and time (`{date}_{HHMMSS}`, which sorts after it; a second later when
that name is taken), so a retry need not wait for the next day. No raw
file is written twice. A harvest fetches only the offerings no raw file
holds yet (a current site: a new snapshot) and `harvest_all()` saves
each DBH year’s as a new run (`write_raw_run()`), the newest year first,
with the run’s fetch ledger (see [“Fetch Ledger and
Retries”](harvesting.md#fetch-ledger-and-retries)).
`harvest_all(years = 2026)` and `harvest_all()` therefore both just add
what is new. An offering is matched on institution, code, year and
semester: when a new DBH snapshot changes its status, it keeps the
`course_id` it was first harvested under. `read_harvest()` combines the
runs; an offering in two runs stops the build.

A raw file holds the rows as the harvest returned them: the offering’s
DBH columns as they were at the harvest (under DBH’s names until
2026-10-16, which `read_raw()` renames: `dbh_rename()`, see
[“Variables”](data-files.md#variables-datavariablescsv)), `url`, `html`,
`html_success`, `html_error` (the error message), `harvested_at`,
`retry` (a retry run) and `harvest_row`, the row’s place in its run, so
`read_raw()` gives the rows in the order they were harvested (which
offering a plan’s text comes from depends on it). `extracted_text` is
kept only for a row without `html`, text read from a PDF, the raw data
there; uis keeps the PDF’s bytes too (`pdf`, since 2026-10-16).
Elsewhere `tar_make()` rebuilds the text from `html`.

The format changed once, on 2026-10-16; the content did not. The RDS raw
files of the harvests before (`data/raw/html_{inst}.RDS`, the first
harvests, and `data/raw/harvests/{date}/html_{inst}.RDS`) were written
one to one to the Parquet store by `data-raw/raw_to_parquet.R`, which
compares every run with its RDS file column by column and row by row,
and `tar_make()` then rebuilt release data-2026-10-16 exactly. The first
harvests did not stamp `harvested_at` and were not dated: their run is
named by the day their RDS file was written (2026-09-30 to 2026-10-06),
and their `harvested_at` stays empty. The RDS files were removed from
`data/raw/` on 2026-10-19; they are kept in the restic snapshot tagged
`pre-parquet` (2026-10-16).

A page that gave no plan can be fetched again with
`harvest_all(retry = TRUE)` (see [“Fetch Ledger and
Retries”](harvesting.md#fetch-ledger-and-retries)). Retry before
finalizing a release; afterwards it changes the release.

To finalize a release, after `tar_make()` and tagging the commit:

``` r
source("R/utils.R"); source("R/release.R")
finalize_release("data-YYYY-MM-DD")   # then commit tests/snapshots/raw_manifest.csv
release_data("data-YYYY-MM-DD")       # hand-over to the coding project
```

`finalize_release()` makes every raw file and fetch ledger read-only (an
overwrite then fails), lists them with md5 sums in
`tests/snapshots/raw_manifest.csv`, and keeps the release’s offerings,
plans and sections (the teacher-ed frame’s, see
[“Frames”](data-files.md#frames)) in `data/releases/{tag}/`. On every
build `tar_read(frozen_check)` lists any change to the release: a raw
file changed or missing, an offering gone or linked to another plan, a
plan gone or with other text, a section changed. Plans added later (a
new DBH year) are not changes. A planned change to the release (a
privacy fix, a section fix after the coders’ check) means a new release:
finalize it, and `release_data()` writes the old -\> new
`plan_content_id` crosswalk. Until then `frozen_check` lists the planned
changes on every build, and a change nobody intended would be listed
among them, so each commit explains the rows it adds, and the next
release follows soon after the fixes. A section fix keeps the plan’s id,
so the crosswalk does not show it; `release_data()` lists such sections
(plan, section, changed, added or removed) in `sections_changed.csv`.

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
whether the institution is ready for its full harvest (see [“Piloting an
Institution”](new-institution.md#piloting-an-institution)). Details:
`.claude/skills/audit-institutions/SKILL.md`.
