# Add Institution

Add support for harvesting course descriptions from a new Norwegian higher education institution.

## Usage

```
/add-institution <institution_short_name>
```

## Prerequisites

Before starting, gather:
1. The institution's short name (lowercase, e.g., "newuni")
2. The institution code from DBH (e.g., "1234")
3. A sample course URL from the institution's website
4. The CSS selector for the main course content

## Procedure

### Step 1: Add config entry to R/institution_config.R

Add a new entry to the `institution_configs` list:

```r
newuni = list(
  code = "1234",
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # CSS selector for course plan content
  selector_mode = "single",       # "single" (html_element) or "multi" (html_elements)
  year_in_url = TRUE              # FALSE if institution doesn't use year in URLs
)
```

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
source("R/institution_config.R")
source("R/utils.R")
source("R/add_course_url.R")
source("R/resolve_course_urls.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/checkpoint.R")
source("R/harvest_strategies.R")
source("R/harvest.R")

courses <- readRDS("data/courses.RDS")

# Test with a specific year first
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> dplyr::select(Emnekode, url, html_success, fulltext) |> head()
result$fulltext[1]  # Inspect extracted text
```

### Step 4: Run full harvest

```r
# Harvest all years
result <- harvest_institution("newuni", courses)
saveRDS(result, "data/html_newuni.RDS")
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

### Institution needs multiple CSS selectors

Set `selector_mode = "multi"` in config and use a comma-separated CSS selector string:

```r
selector = ".section-1, .section-2, .accordion-body",
selector_mode = "multi"
```
