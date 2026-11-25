# Course Plan Browser

A static web app for reading, searching and downloading the harvested course
plans. It runs in the browser without a server, so it can be published on
GitHub Pages; the data are Parquet files next to the page. It replaced a
Shiny app and an Observable page (retired 2026-11-20).

Status: under construction. The first version covers the teacher-education
frame; the full harvest (all courses, filtered by study type) follows. The
page is not published until the terms of use of the plan text are settled.

## Build and run

```sh
# data: targets::tar_make() rebuilds it (target browser_data_files); by hand,
# from the repository root (R, reads data/processed/, data/interim/ and data/input/):
Rscript -e 'for (f in c("utils", "anonymize", "deduplicate_plans", "browser_data")) source(file.path("R", paste0(f, ".R"))); build_browser_data("teacher_ed")'
# app:
cd app/browser
npm ci
npm run dev      # development server
npm test         # vitest
npm run build    # static site in dist/
```

Node 22.12 or later (vitest needs it; `.nvmrc` names the version CI uses).
`public/data/` and `dist/` are build output (gitignored).

## CI and Publishing

- `.github/workflows/browser.yml` runs `npm ci`, `npm test` and
  `npm run build` in `app/browser` (Node from `.nvmrc`, npm cache) on a push
  or pull request to `main` that touches `app/browser/` or
  `data/variables.csv` (built into the downloads). The build has no data:
  it checks that the app compiles.
- `.github/workflows/browser-pages.yml` publishes the page on GitHub Pages
  with the data of a finalized release, downloaded from Zenodo when the site
  is built (record id and file name as inputs, the file checked against
  Zenodo's MD5), so that no plan text enters the git history. It is
  **disabled** (`if: false` on its build job, run only by hand) until
  publishing is decided: the terms of use of the plan text, and a first
  finalized release that holds the browser data as a ZIP of `public/data/`.
  The top of the file says how to enable it.

## Decisions

- **One row per course.** A course (institution + course code) is the unit
  of the results list; its plan versions are grouped under it, with the years
  each was in force. A course matches when one of its versions in the
  selected years matches.
- **Count course-years, never DBH rows.** DBH lists a course once per
  semester and programme (46,882 rows for 24,634 course-years in the
  teacher-ed frame). Trends are the share of course-years in force that have a
  matching plan, never the number of new plans per year (that mostly measures
  which sites keep old plans). Every number is shown with its coverage (the
  share of course-years with a plan) and can be clicked to list its plans.
  "No plan" and "no match" are shown apart.
- **Study types as checkboxes.** The programme filter is a tree of DBH study
  types (table 347 `Studiumkode`: GLU 1-7, lektor, nursing, law, ...),
  grouped, with counts. A course-year belongs to a study type when DBH
  registers the course under a programme of that type in that year. The
  preset "Teacher education (TE frame)" ticks the study types of the
  teacher-ed frame.
- **Search is loose by default and exact on request** (below), and every
  term shows the word forms it matched, so a query can be checked and
  reported.
- **Only anonymized text.** The page holds `course_plan` and the sections from
  `data/processed/`, never `html` or `extracted_text`.

## Data Contract

`build_browser_data(frame)` (`R/browser_data.R`) writes `public/data/`. All
Parquet files are zstd-compressed (level 19). Text files are sorted by
institution, course code and first year, so that consecutive versions of a
course sit next to each other and compress to a fraction (the teacher-ed
plans: 5.5 MB sorted, 18 MB unsorted). It stops before writing when a text
column holds personal data (the patterns of target `privacy_check`).

A plan version is `plan_content_id` + `institution` + `course_code`: the id
is a hash of the normalized text, which two courses of an institution can
share while their `course_plan` differs (16,574 ids for the 17,005 teacher-ed plans,
2026-11-19). `plan_id` is that key in one column
(`{institution}_{course_code}_{first 10 characters of plan_content_id}`): join
plans, sections and course-years on it. The data built before 2026-11-19 lack
it, and `browser_tables()` makes it from the three columns.

| File | One row per | Columns |
|---|---|---|
| `meta.json` | | `version` (the data version, used as cache key: frame and time of `course_plans.RDS`), `built_at`, `frame`, counts, institutions (code, latest name, `plan_year_basis`), study types (every row of `data/study_types.csv`: `code`, `name_no`, `name_en`, `group`, `group_label_en`, `group_label_no`, `te_frame`, plus `courses`, the courses with a course-year of the type, 0 when not in this data; a type of the data missing from the CSV is added with DBH's name (`study_type_name`) in group `other`), sections (`code` as in `sections.parquet`, `query` its name in the query syntax, `label_no`, `label_en`) |
| `courses.parquet` | course | `institution`, `course_code`, `course_name`, `level_code`, `credits`, `language_code`, `subject_code`, `subject_name`, `nus_code`, `department_name` (from the latest offering), `year_first`, `year_last` |
| `course_years.parquet` | course-year x programme x plan | `institution`, `course_code`, `year`, `programme_code`, `study_type`, `plan_id`, `plan_content_id` (both NA without a plan), `had_students` (any offering of the course-year), `no_plan_reason` (`reason` of `data/interim/plan_gaps.RDS`; NA with a plan) |
| `programmes.parquet` | programme | `institution`, `programme_code`, `programme_name`, `study_type` |
| `plans.parquet` | plan version | `plan_id`, `plan_content_id`, `institution`, `course_code`, `year_from`, `year_to`, `source_url`, `harvested_at` (NA where the harvest did not record it), `course_plan` |
| `sections.parquet` | plan x section | `plan_id`, `plan_content_id`, `institution`, `course_code`, `section`, `text` |

A course-year belongs to every programme DBH lists it under that year and has
every plan of its offerings that year (a new plan in the autumn gives two), so
a course-year with two programmes and two plans has four rows. Its study type
is the programme's study type that year, `study_type_code` of
`course_offerings` (DBH table 347 `Studiumkode`, registered per semester;
where a programme changes type within the year, the type of most of the
course's offerings).

The page loads `meta.json`, `courses`, `course_years` and `programmes` first
(well under 1 MB for the teacher-ed frame), shows the list, and loads the
plan text in a Web Worker in the background. Of `sections.parquet` the
worker reads at first only which sections each plan has (the sections
filter); their text is read on first need: a query with a section prefix,
the section scope of AND, the Sections tab or margin of an opened plan, a
section download ("loading sections…" meanwhile). The files are cached in the
browser under `meta.version`, so a second visit starts from the cache.

## Query Syntax

| Query | Matches |
|---|---|
| `didaktikk` | the letters anywhere, also inside words: *matematikkdidaktikk* |
| `fremmedspråk* morsmål*` | both terms (AND is the default) |
| `utforsk*` | words that start with *utforsk*; `*didaktikk` words that end with it; `ut*ing` both |
| `"ikt"` | the whole word only (not *konflikt*) |
| `"folkehelse og livsmestring"` | the exact phrase, whole words at both ends |
| `dybdelæring OR "deep learning"` | either |
| `-praksis`, `NOT praksis` | plans without it |
| `(lesing OR skriving) vurdering` | grouping |
| `outcomes:dybdelæring` | in one section only (`content`, `outcomes`, `prerequisites`, `teaching`, `coursework`, `assessment`, `reading`, `practice`; or `innhold`, `læringsutbytte`, `forkunnskaper`, `undervisning`, `arbeidskrav`, `vurdering`, `pensum`, `praksis`) |
| `/utforsk(e\|ende\|ing)/` | a regular expression (JavaScript, case-insensitive) |

Precisely (`src/search/`):

- `OR`, `NOT` and an optional `AND` are operators only in upper case. `NOT`
  and `-` bind tightest, then `OR`, then the implicit AND: `a b OR c` is
  *a* and (*b* or *c*), `-a OR b` is (not *a*) or *b*. `-` must be attached
  to its term (`-praksis`); a hyphen inside a term is a letter (`1-7`).
- `*` stands for any letters and digits (not hyphens) and may appear anywhere
  in a term; the match is the whole word (for marking and word forms).
  Quoted terms take no wildcards.
- A quoted term or phrase needs a non-letter, non-digit before and after it;
  a hyphen counts as one, so `"ikt"` also matches *IKT-basert* (shown as the
  word form *ikt-basert*, which can be unticked).
- Regular expressions run with the flags `i`, `u` and `m` (`^` and `$` match
  at line breaks). In JavaScript `\b` knows only ASCII letters and fails next
  to æ, ø and å: use `(?<!\p{L})økonomi(?!\p{L})`.
- A section prefix can apply to a group, `outcomes:(lesing OR skriving)`; a
  prefix inside the group wins. `-outcomes:praksis` finds plans whose
  outcomes section lacks *praksis* (plans without that section included).
- Hit counts are all matches in the plan (or the term's section), also
  outside the unit that satisfied the scope of AND.

Matching ignores case (æ, ø and å included). A word is a run of letters and
digits, with inner hyphens (*IKT-basert*); a hyphen before a space is kept,
so split compounds show up as a form of their own (*fremmedspråks-* in
"fremmedspråks- og morsmålsdidaktikk").

**Scope of AND.** A setting chooses where all terms must occur: in the plan
(default), the same section, the same paragraph, the same sentence, or within
N words. A paragraph is a line (the extraction puts each paragraph, heading
and list item on a line of its own). A sentence ends at a line break or at
`.`, `!` or `?` followed by whitespace and a capital letter or digit (so
"bl.a. lesing" does not end one). Within N words: the matched words are at
most N words apart (`lese x x skrive` is 3 apart). OR is evaluated inside the
unit; NOT always on the whole plan, or on its section with a prefix. The
section scope, and the paragraph, sentence and word scopes when a term has a
section prefix, use the sections' text: a term without a prefix then matches
in any section.

**Word forms.** Under the search box each term lists the distinct words it
matched in the plans in the filters, with the number of plans. Unticking a
form drops the matches inside that word: `fremmedspråk*` matches
*fremmedspråksdidaktikk* but also *Fremmedspråksenteret*, an institution's
name. Unticked forms are part of the
query, in the URL and in the download manifest. Forms are lower-cased and
belong to a term (as written, lower-cased, with its section prefix); a match
that spans words (a phrase, a regular expression) has those words as its
form.

Why it matters, measured on the teacher-ed plans (2026-11-19):
`fremmedspråksdidaktikk AND morsmålsdidaktikk` finds no plan, while
`fremmedspråk* morsmål*` finds 48. The words vary: *fremmedspråksdidaktikk*
occurs 122 times, *fremmedspråkdidaktikk* 14 times, *utforsking* is in 1,421
plans and *utforskning* in 850, and 91% of the plans have a split compound
such as "lese- og skrivevansker".

## Layout

- **Header:** search box, scope of AND, search in full plan or one section,
  the word forms of each term, and a line of counts ("312 courses, 1,204
  plan versions; 8.2% of course-years in force in 2024, coverage 71%"; the
  share is of the course-years with a plan in the last year of the filter).
  The years run by default from the first year with a plan in the data (2007
  in the teacher-ed data) to the last; earlier DBH years can be selected.
- **Left:** filters, each a collapsible section: years, study type tree,
  institution, subject (DBH `subject_name`), level, credits (in ranges),
  language, sections the plan has (a version qualifies when it has every
  ticked section, a course when a matching version does). Each value shows
  the courses matching the query and every other filter (its own ignored).
  The study type tree follows the groups of `data/study_types.csv`, with a
  tri-state checkbox per group, English names (Norwegian on hover), per type
  the courses and the coverage of its course-years, and the presets "Teacher
  education (TE frame)" and "Clear"; types without courses in this data are
  greyed and folded in their group (the all-courses data adds them).
- **Middle:** results, one row per course: institution, code, name, a strip
  of the years with the matching versions marked, the number of hits.
- **Right:** the course: full plan with the matches marked and a coloured
  margin showing which section each line went to (a dotted mark for lines in
  no section; a legend, which can switch it off); tabs for sections (those
  the plan lacks greyed), versions (word-level comparison of any two, the
  shown one and the previous by default, matches kept) and source (URL,
  harvest date where recorded, programmes and years using the plan, other
  courses of the institution with the same text). Sections are cut from the
  plan and cleaned, so a line goes to the section that holds the same line
  (case and whitespace ignored); on the teacher-ed plans 99% of the section
  lines are found in their plan, and 64% of the plan's lines are in a section
  (headings and course facts are not; 2026-11-19).
- **Panes:** the course pane takes room only while a plan is open in the
  list view; in Trend and Coverage, or with no course open, the middle pane
  takes the width. In the plan tab the section legend sits above the text,
  which scrolls on its own, and the margin switch is remembered in the
  browser (localStorage; without storage it starts on). Below 1250 px the
  filters narrow; below 900 px an open course replaces the list; below 640 px
  the panes stack in one column and the page scrolls.
- **Views** beside the list (List | Trend | Coverage | Screen | Compare, in
  the URL as `view`): trend: by year, the share of the course-years in force in the
  filter whose plan matches the query, out of those with a plan (a
  course-year matches when any of its plans does); one line overall, per
  institution or per study type (`by`; a course-year counts in each of its
  study types), at most 8 lines (the largest 7 by course-years with a plan,
  the rest under "others", each course-year once); under it the coverage per
  year as bars, so missing plans are not read as absence; without a query the
  lines are the coverage. A tooltip gives the counts (matching / with a
  plan / all course-years), a table under the chart the numbers, and
  clicking a point, bar or cell sets the year (and institution or study type)
  and shows the list. Coverage: per institution the course-years, those with
  a plan, the share, how plan years were found (`plan_year_basis`; sites that
  show only the current plan are flagged) and the most common no-plan
  reasons; a cell without a plan in the year strip gives its reason on hover.
  Screen (needs a query): a random sample of N hits of the selection (the
  hits of the download unit *hit*: each match in the matching plans, 80
  characters of context on either side), drawn with a seed; size and seed
  are in the URL (`sample`, default 50; `seed`, six digits drawn on entering
  the view, or any text), so a precision check can be cited and redrawn.
  Each hit is equally likely and drawn at most once (a partial Fisher-Yates
  shuffle on a seeded generator); the sample is listed in the order of the
  results, so the hits of a plan stay together, and N at or above the number
  of hits takes them all. Step through it with `j`/`k` (or the arrow keys),
  tag a hit `i` include, `e` exclude or `m` maybe (the same key again clears
  it; tagging moves on), `n` writes a note, `o` opens the plan. A line counts
  the tags and the precision (include of include + exclude). Tags are kept in
  the browser (localStorage; every access guarded, and a warning when the
  browser does not keep them) under the data frame and the query (with its
  scope and unticked word forms), per hit (plan version, term, section,
  position): another seed, sample size or filter keeps the tags of the hits
  it shares. Export CSV writes the sample, one row per hit (`sample_rank`,
  institution, course code and name, `plan_id`, years, section, term,
  position, the context before, match and after, tag, note, query, seed).
  Compare: two plan versions side by side (any two, or two versions of one
  course), each picked by institution, code or name, or with *compare* in a
  course's plan pane (side A, or B when A holds another plan); B is by
  default the version before A. With *mark differences* (default) the words
  only in A are struck through on the left and those only in B underlined on
  the right (the word-level comparison of the versions tab); without it each
  side shows the plan with its section margin. Or one year across
  institutions: the plans in force that year of the courses in the list
  (filters and query, matching versions only; set the subject or study type
  to line up one subject, as *Matematikk 1* in GLU 1-7), a column per
  institution, at most 40 plans. Either mode shows the whole plan or one
  section; the matches of the query stay marked. In the URL: `cm` (`pair` or
  `across`), `ca`, `cb` (`institution:course_code@version`), `diff=0`,
  `cyear` and `csec`. Planned: term lists (a heatmap of saved terms x
  institution x year).
- **Download:** the button in the header opens a dialog for the current
  selection, exactly what the list shows (filters, query, unticked word
  forms), as one row per course, plan version, course-year, section or hit
  (keyword in context, 80 characters on either side), with a preview of the
  row count. Formats: CSV (UTF-8 with BOM for Excel, RFC 4180, line breaks
  kept inside quoted cells), XLSX (text over Excel's 32,767 characters per cell
  is cut and the row flagged in `truncated`; 41 teacher-ed plans exceed it),
  Parquet, or QDA: one UTF-8 text file per plan version (named
  `{institution}_{course code}_{years}`, the plan under a short header) with a
  classification sheet (CSV and XLSX, one row per document; the import format
  of NVivo, MAXQDA and ATLAS.ti, with a README on importing). Every download is
  one ZIP named after the data version, unit and time
  (`course-plans_{version}_{unit}_{UTC time}.zip`), so that a table never
  travels without: `manifest.json` (query, scope, excluded forms, filters, data
  version, time, counts, the link to the view), `variables.csv` (the
  exported columns, described from `data/variables.csv` or, for the
  browser's own columns, in `src/download/bundle.js`), `keys.csv` (the plan
  versions: `plan_id` and its parts), `course_year_keys.csv` (for the course
  and course-year units) and `rebuild.R`/`rebuild.py`, which read the release
  files (`course_offerings`, `course_plans`, `plan_sections`; Parquet or RDS)
  and keep the rows by those keys; the query is not re-run, so hits are
  rebuilt as the plans they are in. The worker builds the file (it holds the
  text) and the page saves it.
- **State in the URL**, so every view can be bookmarked and cited.
- **Keyboard:** `/` search, `j`/`k` next and previous course, `Enter` open,
  `Esc` back, `?` help; in Screen the keys above.

## Performance Budgets

| Step | Budget |
|---|---|
| list and filters usable | < 2 s on a 50 Mbit/s line |
| plan text loaded | < 5 s |
| literal search over the teacher-ed plans | < 100 ms (a plain scan takes 30-50 ms) |
| regular expression | < 500 ms |
| a new query while a slow one runs | as if alone, plus at most one chunk of the old (1,000 plans) |
| open a plan | < 50 ms |
| second visit | from the browser cache |

## Code

- `src/search/`: query parser, matcher, word forms, keyword-in-context; pure
  functions with tests (`*.test.js`). `text.js` also holds `FoldedText`, a
  text kept once: its folded (lower-cased) copy, which the matchers read,
  plus the offsets and characters where the original differs (3% of the
  characters), from which the original or a piece of it is rebuilt for
  display. A regular expression runs on the folded copy, which is exact
  with the flags `i` and `u` unless a character folds otherwise than it
  lower-cases (as *İ*); such a text is searched in the original. `chunks.js`:
  a scan in chunks that yields between them and stops when a newer request
  makes it stale.
- `src/data/`: everything but the view, with tests (`*.test.js`).
  - `keys.js`: the identity of a course and of a plan version (`planKey()`;
    the only place that knows how a version is identified).
  - `load.js`: fetching the files through the Cache API (keyed on
    `meta.version`, the app works without it) and reading Parquet.
  - `model.js`: courses with their course-years and versions; the filtered,
    matched view (one row per course), its counts and the counts of each
    filter value (ignoring its own filter); the year strip.
  - `trend.js`: course-years by year and institution or study type, matching
    and with a plan (the trend view), and coverage per institution.
  - `filters.js`: the course filters (institution, subject, level, credits,
    language), their values and readable names.
  - `url.js`: the state in the URL (query, scope, excluded forms, filters,
    course, version, tab, compared version, view, trend lines, screening
    sample size and seed, the plans or year compared).
  - `overlay.js`: the section of each line of a plan.
  - `related.js`: the programmes and years that use a version, other courses
    with the same text.
  - `compare.js`: the plans of the compare view (a plan as
    `institution|course_code@version`, the course picker, the plans of a year
    a column per institution).
  - `screening.js`: the seeded sample, the identity and storage of a hit's
    tags, and the screening CSV.
  - `client.js`: the page's side of the worker (requests as promises).
- `src/worker.js`: fetches and holds the plan and section text and runs the
  search engine; the page gets the plans' metadata, the matches, the word
  forms, keyword-in-context lines and the text of the plan it shows. The text
  stays in the worker, each text once as a `FoldedText`, folded while the
  Parquet is decoded: the teacher-ed plans take a JavaScript heap of 150 MB
  there, 270 MB with the sections (2026-11-19, Chromium; before, the text plus
  its folded copy took 500 MB in node, 250 MB of heap plus the originals
  outside it in Chromium; the tab's memory fell from 575 to 260 MB). Searches
  and word forms run in chunks of 1,000 plans: a new search stops a running
  one (and its word forms) at the next chunk, so a slow regular expression
  no longer holds up the next query (in node, a literal search sent during a
  1 s regular expression answered after 120 ms instead of 1,060), and reading
  the sections gives way to them. The word forms are counted over the plans
  in the filters (the page sends their keys).
- `src/App.svelte`: loading, state, URL, keyboard, the panes.
- `src/lib/`: components: `Header` (search box, scope, counts), `WordForms`,
  `Filters` (with `StudyTypes`, the study type tree, and `CheckList`, a
  collapsible checkbox filter with counts),
  `Results`, `CourseView` (with `PlanText`, the plan with the section
  margin, `Versions`, the comparison, and `Source`); `format.js` (numbers,
  marked text); `diff.js` (word-level comparison, and its two sides for the
  compare view; jsdiff is loaded on first use, a separate chunk of 22 kB); `Download` (the download dialog);
  `Trend` (plain SVG, colours from CSS custom properties), `Coverage` and
  `Screen` (the screening view; the worker draws the sample from the hits),
  `Compare` (the compare view; the worker sends the texts with their
  sections).
- `src/download/`: downloads, with tests (`download.test.js`). `table.js`: the
  selection as tables of each unit; `encode.js`: CSV, XLSX (a minimal writer on
  fflate) and Parquet (hyparquet-writer); `rebuild.js`: the R and Python code;
  `bundle.js`: the column descriptions, manifest, QDA files and the ZIP;
  `dictionary.js`: `data/variables.csv`, built in. The libraries and the
  dictionary are loaded in the worker on the first download (about 100 kB).
