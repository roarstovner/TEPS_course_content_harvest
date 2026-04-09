# Plan: Phase 1 — Structured Section Extraction from Course HTML

## Context

The existing pipeline extracts full-page text (`extracted_text`) from course plan HTML. The user wants to also extract individual sections (assessment, prerequisites, learning outcomes, etc.) as raw text. This is Phase 1 — a later Phase 2 will use LLM structured output to parse sections into typed fields (exam_type, duration, weight, etc.) stored in separate tables per section type.

Phase 1 produces an intermediate long table: `data/sections_raw.RDS` with columns `(course_id, section, raw_text)`.

## Target sections

| Canonical name | Norwegian heading patterns |
|---|---|
| `assessment` | eksamen, vurderingsformer, vurderingsordning, eksamensformer, vurdering og eksamen |
| `prerequisites` | forkunnskapskrav, anbefalte forkunnskaper, krav til forkunnskaper |
| `learning_outcomes` | læringsutbytte, læringsmål (includes sub-headings: kunnskap, ferdigheter, generell kompetanse) |
| `teaching_methods` | arbeids- og undervisningsformer, undervisningsformer |
| `coursework_requirements` | arbeidskrav, obligatoriske aktiviteter |
| `reading_list` | pensum, lesestoff, læremidler, litteratur |
| `course_content` | emnets innhold, faginnhold, innhold, mål og innhold |

Plus English equivalents (examination, assessment methods, learning outcomes, etc.).

## Architecture

Three new files, following existing project patterns:

```
R/section_heading_map.R     — Heading-to-section mapping table + matcher function
R/extract_sections.R        — Core extraction logic (dispatcher + strategies)
R/run_extract_sections.R    — Entry point script (like run_dedup.R)
```

Minor addition to `R/institution_config.R`: optional `section_heading_level` field per institution.

## Extraction strategies

The dispatcher selects a strategy per institution:

### 1. `html_headings` (default for most institutions)
Parse HTML, find heading tags (`<h2>` or `<h3>` per config), match heading text to canonical sections, extract text between headings.

**Institutions**: oslomet (h2), ntnu (h3), uio (h2), hiof (h2), hvl (h3), uia (h2), nih (h2), mf (h2), nmbu (h3), uit (h2), uis (heading-based for HTML rows)

**Algorithm**:
1. Parse HTML with `rvest::read_html()`
2. Select content container (reuse `selector` from institution config)
3. Get all child nodes of container
4. Walk nodes sequentially — when a heading node matches a section, start accumulating content for that section
5. Use `rvest::html_text2()` on accumulated nodes per section
6. If fewer than 3 sections found, fall back to `text_split`

### 2. `text_split` (plain-text heading splitter)
Split `extracted_text` by lines matching heading patterns. A line is a heading if it's short (<80 chars), matches a pattern, and is surrounded by blank lines.

**Institutions**: usn, steiner, hivolda, inn (fallback)

### 3. `accordion_nord` (Nord-specific)
Parse `button.ac-trigger` elements as headings, adjacent `.ac-panel--inner` divs as content.

### 4. `details_uib` (UiB-specific)
Parse `details` elements with `summary` as heading + remaining children as content. Also scan `h2` headings for top-level sections outside accordions.

### 5. `json_nla` (NLA-specific)
Reuse existing JSON parsing from `extract_nla_json()`. Map accordion/table titles to canonical sections.

### 6. `noop` — samas (no content)

## Strategy assignment per institution

| Institution | Strategy | Heading level | Container selector |
|---|---|---|---|
| oslomet | html_headings | h2 | `#main-content` |
| ntnu | html_headings | h3 | `#content` |
| uio | html_headings | h2 | `#vrtx-course-content` |
| hiof | html_headings | h2 | `#vrtx-fs-emne-content, main .entry-content, .entry-content` |
| hvl | html_headings | h3 | `.l-2-col__main-content` |
| uia | html_headings | h2 | `#right-main` |
| nih | html_headings | h2 | `.fs-body` |
| mf | html_headings | h2 | `main` |
| nmbu | html_headings | h3 | `.layout` |
| uit | html_headings | h2 | `.hovedfelt > main > div.col-md-12` |
| inn | html_headings + text_split fallback | h2 | `.content-inner` |
| uis | html_headings | TBD | `#block-page-content` |
| hivolda | text_split | — | — |
| nord | accordion_nord | — | — |
| uib | details_uib + html_headings | h2 | — |
| nla | json_nla | — | — |
| usn | text_split | — | — |
| steiner | text_split | — | — |
| samas | noop | — | — |

## Key design decisions

- **Learning outcome sub-sections** (Kunnskap, Ferdigheter, Generell kompetanse) are kept together in a single `learning_outcomes` row. Phase 2 can split them.
- **Heading matching** is case-insensitive substring matching, ordered by specificity (longer patterns first).
- **Missing sections** simply produce no row — no placeholder NAs.
- **Error handling** uses `purrr::possibly()` per row, returning empty tibble on failure.
- **Pre-processing functions** (e.g., `.add_table_cell_breaks` for INN/Hivolda) are reused from existing config.

## Implementation sequence

1. **`R/section_heading_map.R`** — Pattern table + `match_heading_to_section()` function
2. **`R/extract_sections.R`** — Dispatcher + `extract_sections_html()` for h2/h3-based institutions
3. **Add `extract_sections_text()`** — Text-based splitter for USN, Steiner, Hivolda, fallback
4. **Add `extract_sections_nord()`** — Nord accordion parsing
5. **Add `extract_sections_uib()`** — UiB details/summary hybrid
6. **Add `extract_sections_nla()`** — NLA JSON section extraction
7. **`R/run_extract_sections.R`** — Entry point + diagnostic output (coverage rates, unmapped headings)
8. **Test and iterate** — Run across all institutions, review unmapped headings, expand pattern table

## Output format

```r
# data/sections_raw.RDS
# A tibble with columns:
#   course_id     <chr>  — links to main data
#   institution_short <chr> — for filtering/grouping
#   section       <chr>  — canonical section name
#   raw_text      <chr>  — extracted text for that section
```

## Verification

1. Run `R/run_extract_sections.R` on all institutions
2. Check per-institution coverage: what % of courses with `extracted_text` also have each section extracted
3. Review unmapped headings (the script should log heading texts that didn't match any pattern)
4. Spot-check a few courses per institution: compare `raw_text` against the original HTML in browser
5. Verify `sections_raw.RDS` can be joined to `courses_with_plan_id.RDS` on `course_id`

## Critical files to modify/create

- **Create**: `R/section_heading_map.R`, `R/extract_sections.R`, `R/run_extract_sections.R`
- **Modify**: `R/institution_config.R` (add optional `section_heading_level` per institution)
- **Reuse**: `R/extract_fulltext.R` (pattern for `purrr::possibly()`), `R/institution_config.R` (container selectors), `R/normalize_plan_text.R` (heading word list for reference)

## Known challenges

- **INN** uses h2 for some sections but plain text labels for others — needs text fallback
- **UiS** has both HTML and PDF rows — HTML rows use heading-based extraction, PDF rows need text splitting
- **Heading text variation** — will need iterative expansion of pattern table based on unmapped headings
- **DOM structure variation** — content "between headings" extraction requires walking sibling nodes; some institutions may nest headings differently
