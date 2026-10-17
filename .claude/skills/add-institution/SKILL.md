---
name: add-institution
description: Step-by-step procedure for adding harvesting support for a new Norwegian higher education institution — its robots.txt first (data/robots_txt.csv), then the config entry in R/institution_config.R, URL builder in R/add_course_url.R, and a test harvest. Use when asked to add, onboard or support a new institution.
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
1. The institution's short name (lowercase, e.g., "newuni"; the new ones have
   one in `data/robots_txt.csv`)
2. The institution code from DBH (e.g., "1234")
3. A sample course URL from the institution's website
4. The CSS selector of the element that holds the course plan with its title
   and facts box, the parts of it that are not the plan (navigation, contact
   box, widgets), and the elements that hold the title and facts box

## Procedure

### Step 0: Read robots.txt before the first request to the site

`data/robots_txt.csv` holds the robots.txt of every host harvested or planned
(read 2026-10-16 for the configured institutions and the ten new ones:
kristiania, bi, nmh, nhh, vid, himolde, khio, fjellhaug, fhs, phs). Find the
hosts the harvest will request (course pages, PDFs, discovery pages); for a
host with no row, or a row more than a month old, read its robots.txt once
with the harvest's user agent, before any other request to the host (the
pilot, Step 4, reads it again):

```r
source("R/fetch_html_cols.R")
site_request("https://www.newuni.no/robots.txt") |>
  httr2::req_error(is_error = \(resp) FALSE) |> httr2::req_perform() |>
  httr2::resp_body_string() |> cat()
```

and add or update its row (`read_on`, HTTP `status`, `note`):

- The rules for `User-agent: *` apply: no robots.txt names the harvest's
  user agent, and a group for a named bot (Scrapy, GPTBot) does not. A 404
  means nothing is disallowed.
- `crawl_delay`: the `Crawl-delay` asked of all user agents. It goes in the
  config as `request_delay` (Step 1); `tests/testthat/test-robots.R` fails if
  `request_delay` is shorter, or if a configured institution has no row (use
  the table's short name, or rename the row).
- `disallow`: the `Disallow` rules that touch course-plan paths (the course
  pages, plan PDFs or the discovery pages). A path robots.txt disallows is not
  harvested: say so in the institution's notes in `data/data_notes.qmd`, and
  if the plans are only there, stop and ask the user.

### Step 1: Add config entry to R/institution_config.R

Add a new entry to the `institution_configs` list:

```r
newuni = list(
  code = "1234",
  strategy = "standard",          # or url_discovery, shadow_dom, etc.
  selector = ".main-content",     # the element that holds the course plan (first match)
  exclude = ".contact-box",       # optional: parts of it that are not the plan
  facts = c("h1", ".facts"),      # the title and facts box: in the text, not in a section or the plan id
                                  # (a site read as text, section_strategy = "text": section_text_header
                                  # matches the metadata lines instead; see README "Blocks and Sections")
  plan_years = "url",             # "current" if the site shows only the plan in force; "page" if one page holds all years
  section_strategy = "html",      # read the page as html; see README "Blocks and Sections"
  section_heading_selector = "h2", # the section headings (default h2)
  request_delay = 10              # only if robots.txt asks a Crawl-delay above 3 s (Step 0)
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
- `post_fn`: Function applied to extracted text after parsing (e.g., `.post_ntnu`)
- `fetch_fn`: Custom fetch function (e.g., `fetch_html_cols_single_ntnu`)
- `user_agent`: `"browser"` to use browser User-Agent (e.g., HiOF needs this to avoid 403)

### Step 2: Add URL builder to R/add_course_url.R

1. Add a case to the `case_match()` in `add_course_url()`:

```r
"newuni" ~ add_course_url_newuni(course_code, year, semester_name),
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

courses <- dbh_courses()   # every DBH course (R/utils.R); the institution's code must be in its config

# Test with a specific year first
result <- harvest_institution("newuni", courses, year = 2025)

# Inspect results
result |> dplyr::select(course_code, url, html_success, extracted_text) |> head()
result$extracted_text[1]  # Inspect extracted text

# How a page is read and split: headings with the section they map to (NA =
# unmapped; add a pattern to R/section_heading_map.R), then the sections
cfg <- .block_cfg(get_institution_config("newuni"))
b <- page_blocks(result$html[1], NA, cfg)
b[b$role != "text", c("role", "text", "section")]
page_sections(b, result$extracted_text[1], cfg)
```

### Step 4: Pilot, then the full harvest

Follow README "Piloting an Institution": a stratified sample of the
institution's DBH course-years, harvested into the raw store, built, audited
and fixed until the pilot audits find it ready; only then the full harvest.
The gate numbers (`pilot_gate()`) flag what the audits review; they do not
block on their own.

```r
s <- pilot_sample("newuni", oldest_year = 2015)    # writes data/pilot/newuni.csv: commit it
harvest_all(courses = s, institutions = "newuni")  # into data/raw/newuni/{year}/{date}.parquet
targets::tar_make()
targets::tar_load_globals(); pilot_gate("newuni")  # review = TRUE: a number the audits explain or fix
# /audit-institutions fulltext newuni --pilot, then sections and anonymization:
# each ends with a verdict; record it with the gate table in the institution's issue

# once all three verdicts are "ready for full harvest" or "ready with known limits"
harvest_all(institutions = "newuni")
```

Only `harvest_all()` writes `data/raw/`: test with `harvest_institution()`
(Step 3), which only returns the rows.

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

Use the narrowest element that holds all of them, the title and facts box
included (`course_plan_spec.md`), as `selector` and leave out what lies
between them with `exclude` (hvl: `"div.l-body"` minus the year stamp, the
year links and, in the facts aside, the course coordinators and the exam
schedule). A selector that matches several elements is not supported: only
the first match is read.
