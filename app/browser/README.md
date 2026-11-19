# Course Plan Browser

A static web app for reading, searching and downloading the harvested course
plans. It runs in the browser without a server, so it can be published on
GitHub Pages; the data are Parquet files next to the page. It replaces the
Shiny `course_browser` and the Observable proof of concept.

Status: under construction. The first version covers the teacher-education
frame; the full harvest (all courses, filtered by study type) follows. The
page is not published until the terms of use of the plan text are settled.

## Build and run

```sh
# data (from the repository root; R, reads data/processed/ and data/input/):
Rscript -e 'source("R/utils.R"); source("R/anonymize.R"); source("R/browser_data.R"); build_browser_data("teacher_ed")'
# app:
cd app/browser
npm ci
npm run dev      # development server
npm test         # vitest
npm run build    # static site in dist/
```

Node 20 or later. `public/data/` and `dist/` are build output (gitignored).

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
| `meta.json` | | `version` (the data version, used as cache key: frame and time of `course_plans.RDS`), `built_at`, `frame`, counts, institutions (code, latest name, `plan_year_basis`), study types (code, DBH's `Studnavn`), sections (`code` as in `sections.parquet`, `query` its name in the query syntax, `label_no`, `label_en`) |
| `courses.parquet` | course | `institution`, `course_code`, `course_name`, `level_code`, `credits`, `language_code`, `subject_code`, `subject_name`, `nus_code`, `department_name` (from the latest offering), `year_first`, `year_last` |
| `course_years.parquet` | course-year x programme x plan | `institution`, `course_code`, `year`, `programme_code`, `study_type`, `plan_id`, `plan_content_id` (both NA without a plan), `had_students` (any offering of the course-year), `no_plan_reason` (`reason` of `data/interim/plan_gaps.RDS`; NA with a plan) |
| `programmes.parquet` | programme | `institution`, `programme_code`, `programme_name`, `study_type` |
| `plans.parquet` | plan version | `plan_id`, `plan_content_id`, `institution`, `course_code`, `year_from`, `year_to`, `source_url`, `harvested_at` (NA where the harvest did not record it), `course_plan` |
| `sections.parquet` | plan x section | `plan_id`, `plan_content_id`, `institution`, `course_code`, `section`, `text` |

A course-year belongs to every programme DBH lists it under that year and has
every plan of its offerings that year (a new plan in the autumn gives two), so
a course-year with two programmes and two plans has four rows. Its study type
is the programme's `Studiumkode` that year in the latest DBH programmes
snapshot.

The page loads `meta.json`, `courses`, `course_years` and `programmes` first
(well under 1 MB for the teacher-ed frame), shows the list, and loads the
plan text in a Web Worker in the background. The files are cached in the
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
matched, with the number of plans. Unticking a form drops the matches inside
that word: `fremmedspråk*` matches *fremmedspråksdidaktikk* but also
*Fremmedspråksenteret*, an institution's name. Unticked forms are part of the
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
  plan versions; 8.2% of course-years in force in 2024, coverage 71%").
- **Left:** filters: study type tree, institution, years, subject, level,
  credits, language, sections the plan has.
- **Middle:** results, one row per course: institution, code, name, a strip
  of the years with the matching versions marked, the number of hits.
- **Right:** the course: full plan with the matches marked and a coloured
  margin showing which section each paragraph went to; tabs for sections,
  versions (word-level comparison of any two) and source (URL, harvest date
  where recorded, programmes and years using the plan).
- **Views** beside the list: trend (share of course-years by year,
  institution or study type, with coverage), compare (plans side by side),
  term lists (a heatmap of saved terms x institution x year), screening
  (tag hits include, exclude or maybe, with a note), random sample (seeded).
- **Download:** the current selection, as one row per course, plan version,
  course-year, section or hit; CSV (UTF-8 with BOM for Excel), XLSX (plans
  over Excel's 32,767 characters per cell are flagged), Parquet, a
  keyword-in-context table, or a ZIP of one text file per plan with a sheet of
  their metadata (the import format of NVivo, MAXQDA and ATLAS.ti). Each
  download carries a manifest (query, filters, data version, date, counts),
  `data/variables.csv` and R and Python code that rebuilds the subset from the
  release.
- **State in the URL**, so every view can be bookmarked and cited.
- **Keyboard:** `/` search, `j`/`k` next and previous course, `Enter` open,
  `Esc` back, `?` help.

## Performance Budgets

| Step | Budget |
|---|---|
| list and filters usable | < 2 s on a 50 Mbit/s line |
| plan text loaded | < 5 s |
| literal search over the teacher-ed plans | < 100 ms (a plain scan takes 30-50 ms) |
| regular expression | < 500 ms |
| open a plan | < 50 ms |
| second visit | from the browser cache |

## Code

- `src/search/`: query parser, matcher, word forms, keyword-in-context; pure
  functions with tests (`*.test.js`).
- `src/data/`: everything but the view, with tests (`*.test.js`).
  - `keys.js`: the identity of a course and of a plan version (`planKey()`;
    the only place that knows how a version is identified).
  - `load.js`: fetching the files through the Cache API (keyed on
    `meta.version`, the app works without it) and reading Parquet.
  - `model.js`: courses with their course-years and versions; the filtered,
    matched view (one row per course) and its counts; the year strip.
  - `url.js`: the state in the URL (query, scope, excluded forms, filters,
    course, version, tab).
  - `client.js`: the page's side of the worker (requests as promises).
- `src/worker.js`: fetches and holds the plan and section text and runs the
  search engine; the page gets the plans' metadata, the matches, the word
  forms, keyword-in-context lines and the text of the plan it shows. The text
  stays in the worker (the teacher-ed plans and sections take about 500 MB
  there, the text plus its folded copy).
- `src/App.svelte`: loading, state, URL, keyboard, the panes.
- `src/lib/`: components: `Header` (search box, scope, counts), `WordForms`,
  `Filters`, `StudyTypes` (a flat list, to be replaced by the grouped tree),
  `Results`, `CourseView`; `format.js` (numbers, marked text).
