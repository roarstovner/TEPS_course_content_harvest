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

### Step 1: Add institution mapping to config/institutions.yaml

Add the institution code to short name mapping:

```yaml
institutions:
  # ... existing entries ...
  "1234": "newuni"
```

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

### Step 3: Add CSS selector to config/selectors.yaml

```yaml
selectors:
  # ... existing entries ...
  newuni:
    fulltext: ".main-content"  # Adjust to actual selector
    course_name_no: "h1"       # Selector for course title
```

**Finding the right selector:**
- Use browser DevTools (F12) to inspect the course page
- Use SelectorGadget browser extension
- Test selector returns complete course info without extraneous content
- Some institutions need multiple selectors (use comma-separated list)

### Step 4: Add extraction function to R/extract_fulltext.R

1. Add safe wrapper near the top with other safe_extract functions:

```r
safe_extract_newuni <- purrr::possibly(extract_fulltext_newuni, otherwise = NA_character_)
```

2. Add case to the switch statement in `extract_fulltext()`:

```r
"newuni" = safe_extract_newuni(html),
```

3. Add extraction function at the bottom:

```r
extract_fulltext_newuni <- function(raw_html) {
  .extract_one(raw_html, ".main-content")  # Use selector from Step 3
}
```

Use `.extract_many()` instead if content is spread across multiple elements (accordions, tabs, etc.):

```r
extract_fulltext_newuni <- function(raw_html) {
  .extract_many(raw_html, c(".section-1", ".section-2", ".section-3"))
}
```

### Step 5: Test with small sample

```r
source("R/utils.R")
source("R/add_course_url.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/checkpoint.R")

# Load and filter test data
test <- readRDS("data/courses.RDS") |>
  filter(institution_short == "newuni") |>
  slice(1:3) |>
  add_course_id() |>
  add_course_url()

# Verify URLs look correct
test |> select(Emnekode, Årstall, Semesternavn, url)

# Test fetching
test <- fetch_html_with_checkpoint(
  test,
  checkpoint_path = "data/checkpoint/test_newuni.RDS"
)
test |> count(html_success)

# Test extraction
test$fulltext <- extract_fulltext(test$institution_short, test$html)
test$fulltext[1]  # Inspect result
```

### Step 6: Expand testing gradually

Once small sample works, test by year:

```r
test_2020 <- readRDS("data/courses.RDS") |>
  filter(institution_short == "newuni", Årstall == 2020) |>
  add_course_id() |>
  add_course_url()

# Continue with fetch and extraction...
```

Add more years progressively as validation succeeds.

### Step 7: Run full harvest

Add to `R/run_harvest.R` or create institution-specific script if needed:

```r
# In run_harvest.R, add to institutions vector:
institutions <- c(..., "newuni")
```

## Special Cases

### Institution requires URL discovery (like USN)

If URLs can't be determined from metadata alone:
1. Have `add_course_url_newuni()` return `NA_character_`
2. Add resolver function to `R/resolve_course_urls.R`
3. Add case to `resolve_course_urls()` dispatch
4. Create institution-specific harvest script

### Institution uses JavaScript rendering

If content is loaded via JavaScript:
1. Use `rvest::read_html_live()` instead of `httr2`
2. Add custom fetch logic in `R/fetch_html_cols.R`
3. May need to handle Shadow DOM (see USN implementation)

### Institution has "no content" detection (like NTNU)

Add error detection in fetch function to identify pages that exist but have no course info.
