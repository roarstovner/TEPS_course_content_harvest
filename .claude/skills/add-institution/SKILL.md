---
name: add-institution
description: Step-by-step procedure for adding harvesting support for a new Norwegian higher education institution — config entry in R/institution_config.R, URL builder in R/add_course_url.R, and a test harvest. Use when asked to add, onboard or support a new institution.
argument-hint: <institution>
---

# Add Institution

Add support for harvesting course descriptions from a new Norwegian higher education institution.

## Usage

```
/add-institution <institution>
```

## Prerequisites

Before starting, gather:
1. The institution's short name (lowercase, e.g., "newuni")
2. The institution code from DBH (e.g., "1234")
3. A sample course URL from the institution's website
4. The CSS selector of the element that holds the course plan, and the parts
   of it that are not the plan (navigation, contact box, widgets)

## Procedure

### Step 1: Add config entry to R/institution_config.R

Add a new entry to the `institution_configs` list:

```r
newuni = list(
  code = "1234",
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # the element that holds the course plan (first match)
  exclude = ".contact-box",       # optional: parts of it that are not the plan
  year_in_url = TRUE,             # FALSE if institution doesn't use year in URLs
  section_strategy = "html",      # read the page as html; see README "Blocks and Sections"
  section_heading_selector = "h2" # the section headings (default h2)
)
```

The page is read once into blocks (`R/blocks.R`); the `extracted_text` and the
sections are both made from them, so `selector`/`exclude` decide what text the
plan has and the `section_*` fields only how it is split. For pages built
from accordions use `section_heading_selector = "summary"` (or the accordion's
trigger) and `section_scope = "details"` (or the accordion item); for
paragraph sub-headings use `section_subheading_selector = "p"`. README "Blocks
and Sections" lists every field with the institution that uses it.

Optional config fields:
- `pre_fn`: Function applied to HTML before parsing (e.g., `.add_table_cell_breaks`)
- `post_fn`: Function applied to extracted text after parsing (e.g., `.post_ntnu`)
- `fetch_fn`: Custom fetch function (e.g., `fetch_html_cols_single_ntnu`)
- `user_agent`: `"browser"` to use browser User-Agent (e.g., HiOF needs this to avoid 403)

### Step 2: Add URL builder to R/add_course_url.R

1. Add a case to the `case_match()` in `add_course_url()`:

```r
"newuni" ~ add_course_url_newuni(Emnekode, Årstall, Semesternavn),
```

2. Create the URL builder function at the bottom of the file:

```r
add_course_url_newuni <- function(course_code, year, semester) {
 sem <- case_match(semester, "Vår" ~ "var", "Høst" ~ "host")
 glue::glue("https://www.newuni.no/courses/{year}/{sem}/{tolower(course_code)}")
}
```

Adapt the URL pattern based on actual institution URLs. Check if the institution:
- Uses year in URL
- Uses semester in URL (and what format: "var/host", "spring/autumn", "v/h", etc.)
- Requires lowercase/uppercase course codes
- Has different URL patterns for historical courses

### Step 3: Test with harvest_institution()

```r
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/section_heading_map.R")
source("R/blocks.R")
source("R/extract_sections.R")
source("R/institution_config.R")
source("R/checkpoint.R")
source("R/harvest_strategies.R")
source("R/harvest.R")

courses <- readRDS("data/input/courses.RDS")

# Test with a specific year first
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> dplyr::select(Emnekode, url, html_success, extracted_text) |> head()
result$extracted_text[1]  # Inspect extracted text

# How a page is read and split: headings with the section they map to (NA =
# unmapped; add a pattern to R/section_heading_map.R), then the sections
cfg <- .block_cfg(get_institution_config("newuni"))
b <- page_blocks(result$html[1], NA, cfg)
b[b$role != "text", c("role", "text", "section")]
page_sections(b, result$extracted_text[1], cfg)
```

### Step 4: Run full harvest

```r
# Harvest all years
result <- harvest_institution("newuni", courses)
saveRDS(result, "data/raw/html_newuni.RDS")
```

Or include in `harvest_all()` — it will automatically pick up the new config entry.

## Special Cases

### Institution requires URL discovery (like UiT, USN)

If URLs can't be determined from metadata alone:
1. Set `strategy = "url_discovery"` in config
2. Have `add_course_url_newuni()` return `NA_character_`
3. Add resolver function to `R/resolve_course_urls.R`
4. Add case to `resolve_course_urls()` dispatch

### Institution uses JavaScript rendering (like USN)

If content is loaded via JavaScript/Shadow DOM:
1. Set `strategy = "shadow_dom"` in config
2. Add custom strategy function in `R/harvest_strategies.R`

### Institution has "no content" detection (like NTNU)

Add a custom `fetch_fn` to the config that detects empty/error pages and raises an error.

### The plan is spread over several parts of the page

Use the narrowest element that holds all of them as `selector` and leave out
what lies between them with `exclude` (uis: `"#block-page-content"` minus
navigation, contact footer and facts box). A selector that matches several
elements is not supported: only the first match is read.
