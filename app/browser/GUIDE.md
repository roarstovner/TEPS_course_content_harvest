# User Guide

A quick guide to the course plan browser: how to search, what the numbers
mean, and what each view does. *Help* in the header opens it, `Esc` closes
it. The browser speaks English or Norwegian: the button at the right of the
header switches (the plans and this guide are not translated). The examples are links: in the browser they run the query
and keep your other filters; the line under the search box then gives the
counts in the data the browser holds. The precise rules are in the
[developer README](README.md#query-syntax).

## The Data

- **Course plans** (emneplaner) harvested from the institutions' web pages,
  one plan per course and year, matched to the courses DBH lists. A plan
  that did not change between years is one *plan version*.
- **Anonymized.** Staff names, e-mail addresses and phone numbers are
  removed; the plan text otherwise stays as published.
- **Sections** (learning outcomes, content, assessment, ...) are cut from
  the plan by rules per institution. They are not perfect: a heading the
  rules do not know leaves its text out of every section, and an
  institution's pages may have no such section at all (see
  [Searching Within a Section](#searching-within-a-section)).
- **Only what the sites hold.** Some sites keep older plans, others show
  only the current one; years without a plan are counted, not hidden
  (see [What the Numbers Mean](#what-the-numbers-mean)).
- **Only the institutions harvested.** Some institutions with courses in
  DBH are not harvested. With study types ticked, a note under the study
  type tree names, per type, those with courses in the current year (for
  example Høyskolen Kristiania for law) and, folded, former institutions
  whose courses DBH lists under a code of their own.
- **Two data sets.** *Teacher education* (the default) is the
  teacher-education frame, loaded at once. *All courses* (the links at the
  top of the filters, where the site has both) holds every harvested
  course, too much to load at once: tick a study type and its courses and
  plans are loaded (the size is beside each type until then). Searches,
  views and downloads cover the study types loaded.
- More: [what the data files hold](../../docs/data-files.md),
  [per-institution notes](../../data/data_notes.md), and the
  [methods notes](../../methods.md).

## Searching

Type in the search box (`/` jumps there). Case never matters, æ, ø and å
included. Terms separated by spaces must all match (AND).

| Query | Finds |
|---|---|
| [`samisk`](?q=samisk) | the letters anywhere, also inside words: *nordsamisk*, *samiskfaget* |
| [`"samisk"`](?q=%22samisk%22) | the whole word only (and *samisk-norsk*, a hyphen counts as a word end) |
| [`samisk*`](?q=samisk*) | words that start with *samisk*: *samiske*, *samiskspråklige* |
| [`*samisk`](?q=*samisk) | words that end with *samisk*: *sørsamisk*, *lulesamisk* |
| [`"samisk kultur"`](?q=%22samisk%20kultur%22) | the exact phrase |
| [`samisk urfolk`](?q=samisk%20urfolk) | plans with both words |
| [`samisk OR urfolk`](?q=samisk%20OR%20urfolk) | plans with either |
| [`urfolk -samisk`](?q=urfolk%20-samisk) | *urfolk* but not *samisk* (same as `urfolk NOT samisk`) |
| [`(samisk OR urfolk) lesing`](?q=(samisk%20OR%20urfolk)%20lesing) | grouping: either of the first two, and *lesing* |
| [`/urfolk(et\|ene)?/`](?q=/urfolk(et%7Cene)?/) | a regular expression (JavaScript) |

- `OR`, `NOT` and `AND` are operators only in capitals; `or` is a word.
- `-` must touch its term (`-praksis`); a hyphen inside a word is a letter.
- `a b OR c` means *a* and (*b* or *c*): OR binds tighter than the space.
- In a regular expression `\b` fails next to æ, ø and å: write
  `(?<!\p{L})økonomi(?!\p{L})` instead.

**Check the word forms.** Under the search box each term lists the words it
matched, with the number of plans. Untick a form to drop it: `samisk`
also matches *samiskfaget* and *samisk-norsk*; if you meant only the
adjective, untick the rest (or search `"samisk"`). Unticked forms are kept in
the link and in downloads.

**Where must all terms occur?** The setting *all terms in the same ...*
beside the search box (the scope of AND):

| Scope | [`samisk urfolk`](?q=samisk%20urfolk) in the same ... |
|---|---|
| plan (default) | [plan](?q=samisk%20urfolk&scope=plan) |
| section | [section](?q=samisk%20urfolk&scope=section) |
| line (a paragraph, heading or list item) | [line](?q=samisk%20urfolk&scope=paragraph) |
| sentence | [sentence](?q=samisk%20urfolk&scope=sentence) |
| within N words (here 5) | [within 5 words](?q=samisk%20urfolk&scope=words&n=5) |

A narrower scope usually finds fewer courses (run them in turn and watch the
counts): the words used together rather than somewhere in the same plan. The section scope reads only the sections' text, so text outside
every section cannot match there. NOT is always tested on the whole plan.

## Searching Within a Section

Two ways, which give the same result:

- **The *in* control** beside the search box: *full plan* (default) or one
  section. The whole query then searches that section. It stays in the link
  (`in=outcomes`) and applies in every view: the counts, Trend, Screen,
  Compare, Term lists and downloads.
- **A section prefix**: a section name and a colon before a term, for one
  term only.

| Query | Searches |
|---|---|
| [`samisk` in learning outcomes](?q=samisk&in=outcomes) | the learning outcomes (the control) |
| [`outcomes:samisk`](?q=outcomes:samisk), the same as [`læringsutbytte:samisk`](?q=l%C3%A6ringsutbytte:samisk) | the same, with the prefix |
| [`outcomes:samisk content:urfolk`](?q=outcomes:samisk%20content:urfolk) | *samisk* in the outcomes and *urfolk* in the content |
| [`samisk content:urfolk` in learning outcomes](?q=samisk%20content:urfolk&in=outcomes) | the same: a term's own prefix wins over the control |
| [`outcomes:(samisk OR urfolk)`](?q=outcomes:(samisk%20OR%20urfolk)) | a whole group in one section |
| [`reading:samisk`](?q=reading:samisk) | the reading list |

**How they combine.** The control puts its section's prefix on every term
that has none: with the control on *learning outcomes*, `samisk
content:urfolk` is `outcomes:samisk content:urfolk`. The word forms show it:
each term is listed with the section it searched (`outcomes:samisk`).
Without the control, a term without a prefix searches the whole plan.

Section names, English or Norwegian:

| English | Norwegian | Section |
|---|---|---|
| `content` | `innhold` | course content |
| `outcomes` | `læringsutbytte` | learning outcomes |
| `prerequisites` | `forkunnskaper` | prerequisites |
| `teaching` | `undervisning` | teaching and learning methods |
| `coursework` | `arbeidskrav` | coursework requirements |
| `assessment` | `vurdering` | assessment |
| `reading` | `pensum` | reading list |
| `practice` | `praksis` | practice (placement) |

`-outcomes:samisk` (or `-samisk` with the control on learning outcomes)
finds plans whose outcomes do not mention *samisk*, including plans without
an outcomes section.

**The limits.** A section search can only find plans where that section was
extracted, and that share differs by section and by institution: almost
every plan has learning outcomes, content and assessment, but far fewer
have a reading list or a practice section, and some institutions have none.
[`reading:samisk`](?q=reading:samisk) therefore misses most of the courses
of [`samisk`](?q=samisk), and says nothing about an institution whose plans
have no reading list. Before you compare institutions on a section search:

- Open [section coverage](?view=coverage&cov=sections) (Coverage, *sections
  in the plans*): per institution, the share of the plan versions in the
  filter that have each section. A low share means the section search sees
  few of that institution's plans.
- Run the plain query too, and compare. The difference is what the section
  search leaves out: text in other sections, text outside every section, and
  plans without the section.
- Open a plan and its *Sections* tab (sections the plan lacks are greyed).
  In the plan tab the coloured margin shows which section each line went
  to; lines with a dotted mark are in no section.

**The *Sections* filter is not a search scope.** Ticking *Learning outcomes*
there keeps the plans that *have* such a section; the query still searches
the whole plan. Compare [`samisk` with that filter](?q=samisk&sec=learning_outcomes)
with [`samisk` in learning outcomes](?q=samisk&in=outcomes).

## What the Numbers Mean

The line under the search box reads, with made-up numbers, *600 courses,
2,200 plan versions · 2,500 of 19,500 course-years with a plan match
(12.8%) · 11.8% of course-years in force in 2026 (coverage 90.0%) ·
coverage 79.5% of 24,500 course-years*:

- **Courses**: an institution and a course code. A course matches when one
  of its plan versions in force in the selected years matches.
- **Plan versions**: distinct plan texts of those courses.
- **Course-year**: a course in a year in which DBH lists it (counted once,
  however many programmes or semesters DBH lists it under). This is the
  unit of the shares, so a course offered for ten years weighs ten times as
  much as a course offered once.
- **Match share**: course-years whose plan matches, out of the course-years
  *with a plan*. A year without a plan is neither a match nor a miss.
- **Coverage**: the share of course-years that have a plan. Read every
  share with its coverage: a low coverage means the share rests on few
  plans.
- **In force in YEAR**: the same share for the last year of the filter.

**Which plan is in force in a year** depends on the site (*Plan years* in
the Coverage view):

- *URL year*: the site keeps a page per academic year. An autumn course
  reads the plan of its academic year, a spring or summer course the
  academic year that began the autumn before.
- *Current only*: the site shows only the plan in force (flagged in the
  Coverage view). A harvest counts only for the academic year
  it was made in, so such a site has plans only for the years it was
  harvested, and earlier years have no plan.

## The Views

The tabs above the results. Each keeps the query and the filters.

- **List**: one row per course, with a strip of years (matching versions
  marked; hover a cell without a plan for the reason). Open a course to read
  its plan with the matches marked and the section margin. Tabs: *Sections*,
  *Versions* (word-level differences between any two versions), *Source*
  (the page, when it was harvested, the programmes and years that use the
  plan, other courses with the same text). Example: search
  [`outcomes:samisk`](?q=outcomes:samisk), open a course, and see where the
  matches sit in its outcomes.
- **Trend**: by year, the share of course-years with a plan whose plan
  matches; overall, per institution or per study type; coverage as bars
  underneath. Without a query the lines show coverage. Click a point to
  list its courses. Example: [`samisk` per institution](?q=samisk&view=trend&by=inst).
- **Coverage**: per institution, course-years, those with a plan, the
  share, how plan years are found, and the most common reasons for a
  missing plan; or, with *sections in the plans*, the share of the plan
  versions that have each section. Example: [open it](?view=coverage)
  before comparing institutions, and [section coverage](?view=coverage&cov=sections)
  before a section search.
- **Screen**: a random sample of hits (each match with 80 characters of
  context), drawn with a seed so it can be redrawn and cited. Tag each hit
  include (`i`), exclude (`e`) or maybe (`m`), add a note (`n`), and read the
  precision. *Export CSV* writes the sample with tags and notes. Tags are
  kept in this browser only. Example:
  [50 hits of `samisk`](?q=samisk&view=screen&seed=123456) to check how
  often it means something else.
- **Compare**: two plan versions side by side, differences marked (choose
  them here or with *compare* in a plan); or one year across institutions,
  the plans of the courses in the list in a column per institution. Either
  can show one section only. Example: in a course with several versions,
  click *compare*, then pick an older version as B.
- **Term lists**: named lists of terms, each a query, run as a heatmap of
  terms (or terms per institution) by year. Import and export them as YAML
  (the format of `LK20_terms.yaml`; see the README's
  [Term Lists](README.md#term-lists)). Lists are kept in this browser only:
  send the YAML file along with a link. Example: *new list*, then *add the
  current query* after searching [`samisk OR urfolk`](?q=samisk%20OR%20urfolk).

Every view is in the address bar: copy the link to cite or share exactly
what you see.

## Downloads

*Download* in the header saves exactly what the list shows (filters, query,
unticked word forms), one row per course, plan version, course-year,
section or hit (keyword in context), as CSV, XLSX, Parquet, or text files
for NVivo, MAXQDA and ATLAS.ti. The ZIP also holds:

- `variables.csv`: the data dictionary, a description of every column;
- `manifest.json`: the query, scope, filters, data version and the link;
- `keys.csv`: the plan versions, to join with the published data files;
- `rebuild.R` and `rebuild.py`: code that rebuilds the table from a data
  release.

XLSX cuts text over Excel's limit of 32,767 characters per cell and flags
the row in `truncated`.

## Keyboard

| Key | Does |
|---|---|
| `/` | go to the search box |
| `j` / `k` | next / previous course in the list |
| `↓` / `↑` | the same, when a course in the list has the focus |
| `Enter` | open the course |
| `Esc` | leave the search box; close a dialog, the course or this guide |
| `?` | list the keyboard shortcuts (also *Keys* in the header) |
| Screen: `j` / `k`, `i` / `e` / `m`, `n`, `o` | next / previous hit; include / exclude / maybe; note; open the plan |

The keys do nothing while you type in a field; `Esc` leaves it.
