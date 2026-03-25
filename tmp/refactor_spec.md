# Harvest Pipeline Refactor — Spec & Plan

## Problem Statement

The pipeline has grown organically to support 20 institutions with 6 distinct harvesting
strategies. The result is:

1. **~10 nearly-identical `run_harvest_*.R` scripts** differing only in institution name
   and minor config (year filters, etc.)
2. **No single entry point** for "harvest everything for year X" — the annual re-run
   use case requires manually running the right scripts in the right order.
3. **Config files are decorative** — `config/selectors.yaml` and `config/institutions.yaml`
   exist but are not read by the code. Selectors are hardcoded in `extract_fulltext.R`.
4. **Inconsistent output shape** — USN produces text (from Shadow DOM) while others produce
   HTML. The `html` column means different things depending on institution.

## What Should NOT Change

- **The six harvesting strategies stay.** The complexity is inherent: USN needs Shadow DOM,
  Steiner needs PDF splitting, NLA needs JSON extraction. We don't try to unify these into
  one abstraction.
- **The checkpoint system stays.** It works, it's crash-safe, and it's well-understood.
- **The normalization and dedup pipeline stays** (`normalize_plan_text.R`,
  `deduplicate_plans.R`, `run_dedup.R`). It's downstream and already well-structured.
- **Institution-specific helper functions stay** (URL builders, pre/post-processors, etc.).
  These encode real domain knowledge.

## Design Goals

1. **One command to harvest all institutions for a given year** (or all years).
2. **One command to re-harvest a specific institution** (for debugging/fixing).
3. **Each institution's strategy is declared, not coded** — a config object says "UiS uses
   HTML+PDF discovery", not a 200-line script.
4. **New institutions are easy to add** — add config, add URL builder, add selector, done
   (for standard institutions). Complex strategies still need custom functions.
5. **Output is uniform** — every institution produces the same columns.

## Proposed Architecture

### Strategy Classification

Every institution gets classified into one of these strategies:

| Strategy | Institutions | Description |
|----------|-------------|-------------|
| `standard` | oslomet, uia, ntnu, inn, hiof, hvl, mf, nih, nmbu, uib, uio | URL → HTTP fetch → CSS extract |
| `url_discovery` | hivolda, uit | URL → discover real URL → HTTP fetch → CSS extract |
| `shadow_dom` | usn | URL → Chrome + Shadow DOM extraction (URL discovery + fetch combined) |
| `html_pdf_discovery` | uis | URL → discover dropdown → HTML or PDF fetch → extract |
| `pdf_split` | steiner | Fetch wrapper → download PDF → split by section |
| `json_extract` | nla | URL → HTTP fetch → JSON extraction from `<script>` tag |
| `noop` | samas | Set fulltext = NA |

### Institution Registry (`R/institution_config.R` — R list, no YAML)

Config is a plain R list in `R/institution_config.R`. No YAML files. Both
`config/institutions.yaml` and `config/selectors.yaml` are deleted.

Benefits over YAML:
- Native R types (no `"true"` vs `TRUE` ambiguity)
- Function references directly in config (`pre_fn = add_table_cell_breaks`)
- No parsing layer, no extra dependency
- One file to check, not two

```r
institution_configs <- list(
  oslomet = list(
    code = "0959",
    strategy = "standard",
    selector = "div#course-plan-box",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = NULL,
    post_fn = NULL
  ),
  nord = list(
    code = "1154",
    strategy = "standard",
    selector = ".accordion-body, .emneinfo-accordion-innhold",
    selector_mode = "multi",
    year_in_url = TRUE
  ),
  inn = list(
    code = "1110",
    strategy = "standard",
    selector = "div#LearningOutcome",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = add_table_cell_breaks  # inserts \n before </td>, </th>
  ),
  ntnu = list(
    code = "1150",
    strategy = "standard",
    selector = "div.emne-detaljer",
    selector_mode = "single",
    year_in_url = TRUE,
    post_fn = post_ntnu,  # strips "Vis detaljert timeplan" JS artifact
    fetch_fn = fetch_ntnu # detects "no information available" → error
  ),
  usn = list(
    code = "1163",
    strategy = "shadow_dom",
    year_in_url = TRUE
  ),
  uis = list(
    code = "1160",
    strategy = "html_pdf_discovery",
    selector = "#fs-emneinfo-container, .fs-field-content",
    selector_mode = "multi",
    year_in_url = TRUE
  ),
  steiner = list(
    code = "8101",
    strategy = "pdf_split",
    year_in_url = FALSE
  ),
  nla = list(
    code = "1171",
    strategy = "json_extract",
    year_in_url = FALSE
  ),
  hvl = list(
    code = "1181",
    strategy = "standard",
    selector = ".l-2-col__main-content",
    selector_mode = "single",
    year_in_url = TRUE,
    fetch_fn = fetch_hvl  # detects "course not found" soft 404 → error
  ),
  hiof = list(
    code = "1141",
    strategy = "standard",
    selector = "#vrtx-fs-emne-content, main .entry-content, .entry-content",
    selector_mode = "single",
    year_in_url = TRUE,
    user_agent = "browser"  # uses browser UA to avoid 403 blocks
  ),
  samas = list(
    code = "8102",
    strategy = "noop",
    year_in_url = FALSE
  )
  # ... remaining institutions follow same pattern
)

get_institution_config <- function(inst) {
  config <- institution_configs[[inst]]
  if (is.null(config)) stop("Unknown institution: ", inst)
  config
}

load_all_configs <- function() institution_configs
```

### Fetch-Time Overrides Move to Config

Currently `fetch_html_cols.R` has its own `switch()` dispatching by institution name for
three overrides: NTNU (no-info detection), HVL (not-found detection), HiOF (custom UA).
This is the same problem as the hardcoded selectors — institution-specific logic hidden
inside a "generic" function.

These move to config as:
- `fetch_fn`: A custom single-URL fetch function (replaces the switch in
  `fetch_html_cols_single`). When present, `fetch_html_cols_single` calls it instead of
  the default fetch. NTNU and HVL use this.
- `user_agent`: `"browser"` or a custom string. HiOF uses this.

`fetch_html_cols_single()` becomes truly generic — it reads `config$fetch_fn` and
`config$user_agent` from the config passed to it, with no institution name checking.

### Core Module: `R/harvest.R` (new — replaces all `run_harvest_*.R` scripts)

```r
#' Harvest one institution
#'
#' @param institution_short Character, e.g. "oslomet"
#' @param courses Data frame from courses.RDS (pre-filtered or not)
#' @param year Optional integer — if given, only harvest this year
#' @param refetch Logical — if TRUE, ignore checkpoints and re-download everything
harvest_institution <- function(institution_short, courses, year = NULL, refetch = FALSE) {
  config <- get_institution_config(institution_short)

  df <- courses |>
    filter(institution_short == !!institution_short) |>
    # If year_in_url is FALSE, auto-filter to max(Årstall) — unless caller specified year
    apply_year_filter(config, year) |>
    add_course_id() |>
    validate_courses("initial") |>
    add_course_url() |>
    validate_courses("with_url")

  # Dispatch to strategy
  result <- switch(config$strategy,
    standard          = harvest_standard(df, config, refetch),
    url_discovery     = harvest_url_discovery(df, config, refetch),
    shadow_dom        = harvest_shadow_dom(df, config, refetch),
    html_pdf_discovery = harvest_html_pdf_discovery(df, config, refetch),
    pdf_split         = harvest_pdf_split(df, config, refetch),
    json_extract      = harvest_json_extract(df, config, refetch),
    noop              = harvest_noop(df, config),
    stop("Unknown strategy: ", config$strategy)
  )

  # Ensure uniform output columns
  result |> ensure_output_columns()
}

#' Harvest all institutions
harvest_all <- function(courses = NULL, year = NULL, refetch = FALSE) {
  if (is.null(courses)) courses <- readRDS("data/courses.RDS")
  configs <- load_all_configs()

  for (inst in names(configs)) {
    message("=== ", inst, " ===")
    result <- harvest_institution(inst, courses, year, refetch)
    saveRDS(result, file.path("data", paste0("html_", inst, ".RDS")))
    log_summary(inst, result)
  }
}
```

### Strategy Implementations

Each strategy gets its own function in `R/harvest_strategies.R`:

```r
harvest_standard <- function(df, config, refetch) {
  # 1. Fetch HTML (with checkpoint)
  df <- fetch_html_with_checkpoint(df, checkpoint_path(config, "html"), refetch)
  # 2. Extract fulltext using config-driven selector
  df$fulltext <- extract_fulltext_css(df$html, config$selector, config$selector_mode)
  df
}

harvest_url_discovery <- function(df, config, refetch) {
  # 1. Discover URLs
  df <- resolve_course_urls(df, checkpoint_path(config, "urls"), refetch)
  # 2. Fetch HTML
  df <- fetch_html_with_checkpoint(df, checkpoint_path(config, "html"), refetch)
  # 3. Extract fulltext
  df$fulltext <- extract_fulltext_css(df$html, config$selector, config$selector_mode)
  df
}

harvest_shadow_dom <- function(df, config, refetch) {
  # USN: resolve also fetches HTML (combined step)
  df <- resolve_course_urls(df, checkpoint_path(config, "urls"), refetch)
  df <- df |> mutate(
    html_success = !is.na(html) & html != "",
    html_error = vector("list", n())
  )
  df$fulltext <- extract_fulltext_usn(df$html)
  df
}

# ... etc for each strategy
```

### What extract_fulltext becomes

Currently `extract_fulltext()` is a big router that dispatches by institution name. Replace
with two clean functions:

```r
# Config-driven CSS extraction (replaces institution-specific dispatching)
extract_fulltext_css <- function(html, selector, mode = "single",
                                 pre_fn = NULL, post_fn = NULL) {
  purrr::map_chr(html, \(h) {
    if (is.na(h)) return(NA_character_)
    if (!is.null(pre_fn)) h <- pre_fn(h)
    doc <- rvest::read_html(h)
    text <- if (mode == "single") {
      rvest::html_element(doc, selector) |> rvest::html_text2()
    } else {
      rvest::html_elements(doc, selector) |>
        rvest::html_text2() |>
        paste(collapse = "\n")
    }
    if (!is.null(post_fn)) text <- post_fn(text)
    if (is.na(text) || text == "") NA_character_ else text
  })
}
```

The institution-specific pre/post processors (`.add_table_cell_breaks`, `.post_ntnu`, etc.)
become named functions referenced in the config, not wired through `case_match()`.

### File Structure After Refactor

```
R/
├── harvest.R               # NEW: harvest_institution(), harvest_all() — main entry points
├── harvest_strategies.R    # NEW: harvest_standard(), harvest_shadow_dom(), etc.
├── institution_config.R    # NEW: get_institution_config(), load_all_configs()
├── utils.R                 # KEPT: add_course_id, validate_courses, etc.
├── add_course_url.R        # KEPT: institution-specific URL builders
├── resolve_course_urls.R   # KEPT: URL discovery logic
├── fetch_html_cols.R       # KEPT: HTTP fetching
├── extract_fulltext.R      # REFACTORED: extract_fulltext_css(), extract_fulltext_pdf(), etc.
├── checkpoint.R            # KEPT: checkpoint logic
├── normalize_plan_text.R   # KEPT: dedup preprocessing
├── deduplicate_plans.R     # KEPT: dedup pipeline
├── run_dedup.R             # KEPT: dedup entry point
└── run_harvest.R           # DELETED (replaced by harvest.R)
    run_harvest_oslomet.R   # DELETED (absorbed into harvest_standard)
    run_harvest_uib.R       # DELETED
    run_harvest_nmbu.R      # DELETED
    run_harvest_uia.R       # DELETED
    run_harvest_uio.R       # DELETED
    run_harvest_usn.R       # DELETED
    run_harvest_uis.R       # DELETED (absorbed into harvest_html_pdf_discovery)
    run_harvest_nla.R       # DELETED (absorbed into harvest_json_extract)
    run_harvest_steiner.R   # DELETED (absorbed into harvest_pdf_split)
    run_harvest_samas.R     # DELETED
    translate_samas.R       # DELETED (samas is noop; translation never ran successfully)
```

### Config Becomes Authoritative

`R/institution_config.R` is the single source of truth for:
- Institution code → short name mapping
- Harvesting strategy
- CSS selectors (moved from hardcoded `.selectors` list in `extract_fulltext.R`)
- Selector mode (single/multi)
- Whether URLs contain year (`year_in_url`)
- Pre/post processing functions (actual R function references, not strings)
- Any institution-specific flags (e.g., NTNU's "no info" detection)

Both `config/institutions.yaml` and `config/selectors.yaml` are **deleted**.
The `config/` directory is removed entirely.

## Data Contract Between Pipeline Stages

### Stage 1: Input — `courses.RDS` (from DBH)

These columns come from the DBH database and pass through unchanged:

```
institution_short   chr   "oslomet", "ntnu", ...
Institusjonskode    chr   "0959", "1150", ...
Institusjonsnavn    chr   full institution name
Avdelingskode       chr
Avdelingsnavn       chr
Avdelingskode_SSB   chr
Årstall             int   2020, 2021, ...
Semester            int   1 (spring), 3 (autumn)
Semesternavn        chr   "Vår", "Høst"
Studieprogramkode   chr
Studieprogramnavn   chr
Emnekode_raw        chr   original course code (may have version suffix)
Emnekode            chr   normalized course code
Emnenavn            chr   course name in Norwegian
Nivåkode            chr
Nivånavn            chr
Studiepoeng         num   ECTS credits
NUS-kode            chr
Status              int   1=Active, 2=New, 3=Discontinued, 4=Discontinued+exam
Statusnavn          chr
Underv.språk        chr   "Norsk", "Engelsk", ...
Navn                chr
Fagkode             chr
Fagnavn             chr
Oppgave (ny fra h2012) chr
```

### Stage 2: Output — `html_{inst}.RDS` (from `harvest_institution()`)

All DBH columns above, plus these columns added by the pipeline:

```
course_id           chr   "{inst}_{code}_{year}_{semester}_{status}" — UNIQUE KEY
url                 chr   URL fetched (or NA if not available)
html                chr   raw HTML body (or extracted text for USN; or NA for PDF/noop)
html_error          list  error object per row (NULL on success, character on failure)
html_success        lgl   TRUE if fetch succeeded
fulltext            chr   extracted course plan text (or NA)
```

**`ensure_output_columns()` guarantees all six columns exist** with correct types,
regardless of strategy. This fixes the current bug where Steiner is missing
`html`, `html_error`, `html_success`.

Column order: DBH columns first (in original order), then `course_id`, `url`, `html`,
`html_error`, `html_success`, `fulltext`.

### Stage 3: Output — `courses_with_plan_id.RDS` (from `run_dedup.R`)

All Stage 2 columns (from all institutions bound together), plus:

```
fulltext_normalized chr   text after institution-specific + generic normalization
plan_content_id     chr   SHA256 hash of fulltext_normalized (or NA)
```

### Stage 3b: Output — `plan_lookup.RDS` (from `run_dedup.R`)

One row per unique (plan_content_id, institution_short, Emnekode):

```
plan_content_id     chr   SHA256 hash — joins to courses_with_plan_id
institution_short   chr
Emnekode            chr
year_from           int   earliest year this plan appeared
year_to             int   latest year this plan appeared
fulltext            chr   original text (first occurrence)
fulltext_normalized chr   normalized text (first occurrence)
```

### Consumers

| Consumer | Reads | Required columns |
|----------|-------|-----------------|
| `run_dedup.R` | `html_*.RDS` | institution_short, Emnekode, Årstall, fulltext |
| Shiny app (browse) | `courses_with_plan_id.RDS` | course_id, institution_short, Emnekode_raw, Emnenavn, Årstall, Semesternavn, Status, url, fulltext, plan_content_id |
| Shiny app (diff) | `courses_with_plan_id.RDS` | plan_content_id, fulltext, fulltext_normalized |
| Shiny app (HTML view) | `html_*.RDS` | course_id, html |

## Migration Plan

### Phase 1: Infrastructure (no behavior change)
1. Create `R/institution_config.R` — R list with all institution configs
2. Populate with all 20 institutions: strategy, selectors, year_in_url, pre/post functions

### Phase 2: Extract strategy functions
4. Create `R/harvest_strategies.R` with `harvest_standard()`, `harvest_url_discovery()`, etc.
5. Refactor `extract_fulltext.R`: replace `case_match()` routing with config-driven extraction
6. Keep institution-specific pre/post processors as named functions

### Phase 3: Wire up entry points
7. Create `R/harvest.R` with `harvest_institution()` and `harvest_all()`
8. Add `year_in_url` auto-filtering logic in `apply_year_filter()`

### Phase 4: Cleanup
9. Delete all `run_harvest_*.R` scripts
10. Delete `R/translate_samas.R`
11. Delete `config/` directory entirely (both YAML files)
12. Update CLAUDE.md to reflect new architecture

### Phase 5: Verification
12. Compare new output against existing `html_*.RDS` files
13. Run dedup pipeline, compare against existing `courses_with_plan_id.RDS`
14. If outputs match → refactor is correct

## Risks

1. **Regression risk**: The biggest risk is that the refactored pipeline produces slightly
   different output for some institution. Mitigation: compare outputs column-by-column.
2. **Over-abstraction**: Making the config too clever could make it harder to debug
   institution-specific issues. Keep config simple — just strings and flags, not DSL.
3. **Steiner/UiS complexity**: These strategies are genuinely complex. The functions
   will be large but live inside `harvest_strategies.R` — accept longer functions
   over separate files.

## Resolved Design Decisions

1. **No standalone scripts.** All harvesting goes through `harvest_institution()` or
   `harvest_all()`. No `run_harvest_*.R` scripts to source directly. Complex strategy logic
   (Steiner PDF splitting, UiS discovery, NLA JSON) lives in functions inside
   `harvest_strategies.R`, not in standalone scripts.

2. **`refetch` parameter** (not `force`). Named to clearly communicate intent: "ignore
   checkpoints and re-download everything." This is the mechanism for re-harvesting with
   updated code.

3. **`year_in_url` config flag auto-filters.** If `year_in_url: false`, the pipeline
   automatically filters to `max(Årstall)` before harvesting. Rationale: fetching the same
   page N times for N years is pure waste, and the dedup pipeline would collapse them anyway.
   UiO fits this pattern (only publishes latest plan despite having year in URL structure).
   The caller can still pass `year = 2025` explicitly to override.

4. **Delete `translate_samas.R`.** Samas is noop strategy (fulltext = NA). The translation
   infrastructure never produced results. Remove it entirely.
