# Course Browser (Shiny)

A plan-centric concordance browser for the harvested course plans.

## Build & run

```sh
Rscript -e 'targets::tar_make()'                    # rebuilds app/course_browser/data/browser_data.RDS when needed
Rscript -e 'shiny::runApp("app/course_browser")'
```

The app reads only the prebuilt `data/browser_data.RDS` in this folder, which
`build_data.R` makes from `course_plans.RDS`, `course_offerings_full.RDS` and
`plan_sections.RDS`; `targets::tar_make()` reruns it when they change.

## Design

The unit is the unique course plan (~11k), not the offering (~33k). 66% of
offerings share their plan text, so an offering-level browser makes you read the
same paragraph repeatedly. Offering coverage (years, semesters, count) is rolled
onto each plan instead.

- **Concordance**: ad-hoc literal or regex search over plan text, or scoped to
  one extracted section. All matches highlighted, shown as numbered
  keyword-in-context snippets, with full plan / sections / source HTML for
  provenance.
- **Trend**: occurrence of the current search by year, optionally by
  institution or Fagnavn. Plans count in `year_from`, so this measures uptake in
  new or revised plans, not the share of plans in force.
- **Coverage**: share of offerings with extracted text per institution-year.
  41% of offerings have no text, so denominators are not comparable across
  institutions without checking this first.
- **Diff**: consecutive plan versions for one course code.

Code: `app.R` (UI + server), `R/search.R` (search engine, KWIC snippets, match
highlighting), `R/data.R` (payload loading, labels, optional term set, diff
renderer), `www/styles.css`.

## Query syntax

In literal mode, whitespace separates terms and every term must be present:
`livsmestring dybdelæring` is AND, not a phrase. Wrap a phrase in double quotes
(`"folkehelse og livsmestring"`) to search it as one term. Each term gets its
own highlight colour (cycling after four) and its own count in the legend.
Regex mode takes the whole box as a single pattern.

AND is implemented in `parse_query()` rather than as a regex, deliberately.
Regex has no AND operator, and the lookahead that emulates one is a trap on this
data: `(?=.*a)(?=.*b)` silently matches nothing without `(?s)` because plan text
contains newlines, costs ~80 min over 11k plans unless anchored with `^`, and
even anchored matches zero-width, so it can filter but never highlight, and
reports a hit count of 1 per plan. Two `stri_count_fixed` passes give the same
120 plans in 0.019s with real per-term counts and highlighting.

## Search performance

`build_search_index()` keeps a pre-lowercased copy of every searchable text. A
literal case-insensitive search then reduces to `stri_count_fixed` on
already-folded text (~0.01s over 11k plans). Regex queries cannot use that path
(lowercasing a pattern would corrupt it: `\S` becomes `\s`), so they run
case-insensitively against the original text (~0.5s). Match positions are
computed only for the selected plan, never for the whole result set, and
overlapping ranges from different terms are merged by `merge_locs()` so the
markup never nests.

## Deep links

`?q=livsmestring&scope=learning_outcomes&regex=1&inst=ntnu,uit` restores a
search, so a query can be bookmarked or cited. Percent-encode spaces in a
multi-term query (`?q=livsmestring%20dybdel%C3%A6ring`).

## Optional term set

Point `TEPS_BROWSER_TERMS` at a terms YAML (same shape as
`emneplan_LK20/LK20_terms.yaml`: a `terms:` list of `id`/`label`/`regex`), or
drop a `browser_terms.yaml` in the repo root, to get a dropdown of predefined
terms. Absent, the control hides itself and ad-hoc search is unaffected.

The static Observable JS alternative is in `app/course_browser_ojs/` (see its
README).
