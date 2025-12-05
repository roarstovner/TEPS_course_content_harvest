# TEPS Course Content Harvest - User Guide


## Overview

This project harvests course descriptions from Norwegian teacher
education institutions. It takes course codes, years, and semesters as
input and produces structured data containing URLs, HTML, and extracted
text from course webpages.

**Input:** A tibble as output by `rdbhapi` with course metadata
(institution, course code, year, semester)

**Output:** The same tibble with added course URL, raw HTML, and
extracted full text

## Quick Start: Running the Pipeline

The main pipeline is in `R/run_harvest.R`. Here’s the basic workflow:

``` r
# 1. Load required functions
source("R/utils.R")
source("R/fetch_html_cols.R")
source("R/extract_fulltext.R")
source("R/add_course_url.R")
source("R/checkpoint.R")

# 2. Load course data
courses <- readRDS("data/courses.RDS")

# 3. Filter to the institution you want to process
courses <- courses |> 
  filter(institution_short == "hivolda") |>  # Change this to your institution
  add_course_id() |> 
  validate_courses("initial") |>
  add_course_url() |> 
  validate_courses("with_url")

# 4. Fetch HTML (with automatic checkpointing). If this fails, just run it again, and it will start where it failed.
courses <- fetch_html_with_checkpoint(
  courses,
  checkpoint_path = "data/checkpoint/html_hivolda.RDS"  # One file per institution
)

# 5. Extract full text from HTML
fulltext <- extract_fulltext(courses$institution_short, courses$html)
```

### What Each Step Does

1.  **`add_course_id()`**: Creates unique identifiers for each course
2.  **`validate_courses()`**: Checks that required columns exist
3.  **`add_course_url()`**: Generates the correct URL for each course
4.  **`fetch_html_with_checkpoint()`**: Downloads HTML (skips
    already-downloaded courses)
5.  **`extract_fulltext()`**: Extracts clean text from HTML using CSS
    selectors

### About Checkpointing

The pipeline uses **checkpointing** to avoid re-downloading data. If the
script stops or crashes:

-   Already-downloaded HTML is saved in checkpoint_path, which should be
    updated to `data/checkpoint/html_{institution}.RDS`
-   Re-running the script will skip courses already in the checkpoint
-   This saves time and is polite to institutional servers

## Adding a New Institution

To add support for a new institution, you need to modify **two key
files**:

**How to find CSS selectors:**

1.  Open a course page in your browser
2.  Right-click on the course description text → “Inspect”
3.  Look for a `class` or `id` attribute that wraps the content
4.  Use the SelectorGadget tool:
    https://rvest.tidyverse.org/articles/selectorgadget.html
5.  Test your selector to make sure it captures all course text

For one particularly gnarly selector, asking an LLM with thinking
enabled to return “the full info about Emneplan, but without extraneous
elements” got the right selector.

### 1. Update `R/add_course_url.R`

Add a case for your institution and create a URL-building function.

``` r
add_course_url <- function(df) {
  df |>
    mutate(
      url = case_match(
        institution_short,
        "hiof" ~ add_course_url_hiof(Emnekode, Årstall, Semesternavn),
        "hivolda" ~ add_course_url_hivolda(Emnekode),
        "newuni" ~ add_course_url_newuni(Emnekode, Årstall, Semesternavn),  # Add this
        .default = NA_character_
      )
    )
}

# Create a helper function for your institution
add_course_url_newuni <- function(course_code, year, semester) {
  semester <- case_match(semester, "Vår" ~ "spring", "Høst" ~ "autumn")
  glue::glue("https://www.newuni.no/courses/{year}/{semester}/{tolower(course_code)}")
}
```

Add the institution’s URL pattern:

**URL pattern tokens** you can use: - `{year}` - Year (e.g., “2024”) -
`{semester}` - Semester label - `{course_code}` - Original course code -
`{tolower(code_lower)}` - Course code in lowercase

### 2. Update `R/extract_fulltext.R`

Add a case for extracting text from your institution’s HTML:

``` r
extract_fulltext <- function(institution_short, raw_html) {
  purrr::map2_chr(institution_short, raw_html, \(inst, html) {
    if(is.na(html)) {
      return(NA_character_)
    }

    switch(
      inst,
      "hivolda" = extract_fulltext_hivolda(html),
      "newuni" = extract_fulltext_newuni(html),  # Add this
      "unsupported institution"
    )
  })
}

# Create extraction function using the correct CSS selector
extract_fulltext_newuni <- function(raw_html) {
  raw_html |> 
    rvest::read_html() |> 
    rvest::html_elements(".main-content") |> 
    rvest::html_text2()
}
```

### 3. Test Your Changes

Run the pipeline with a small sample from your new institution:

``` r
# Test with a few courses first
test_courses <- readRDS("data/courses.RDS") |>
  filter(institution_short == "newuni") |>
  slice(1:3) |>  # Just 3 courses for testing
  add_course_id() |>
  add_course_url()

# Check URLs look correct
test_courses |> select(Emnekode, Årstall, Semesternavn, url)

# Try fetching HTML
test_courses <- fetch_html_with_checkpoint(
  test_courses,
  checkpoint_path = "data/checkpoint/test_newuni.RDS"
)

# Check if HTML was retrieved
test_courses |> count(html_success)

# Try extracting text
fulltext <- extract_fulltext(test_courses$institution_short, test_courses$html)
fulltext[1]  # Inspect first result
```

Expand a little bit at a time, for example by year:
`filter(Årstall %in% c(2017, 2018))` Add more years as the pipeline
works.

## Data Structure

### Input Data (`data/courses.RDS`)

The courses dataset contains:

``` r
courses <- readRDS("data/courses.RDS")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution_short Institusjonskode Institusjonsnavn       Avdelingskode
      <chr>             <chr>            <chr>                  <chr>        
    1 uib               1120             Universitetet i Bergen 220440       
    2 uib               1120             Universitetet i Bergen 220440       
    # ℹ 21 more variables: Avdelingsnavn <chr>, Avdelingskode_SSB <chr>,
    #   Årstall <int>, Semester <int>, Semesternavn <chr>, Studieprogramkode <chr>,
    #   Studieprogramnavn <chr>, Emnekode_raw <chr>, Emnekode <chr>,
    #   Emnenavn <chr>, Nivåkode <chr>, Nivånavn <chr>, Studiepoeng <dbl>,
    #   `NUS-kode` <chr>, Status <int>, Statusnavn <chr>, Underv.språk <chr>,
    #   Navn <chr>, Fagkode <chr>, Fagnavn <chr>, `Oppgave (ny fra h2012)` <int>

Key columns: - `institution_short`: Short code (e.g., “oslomet”,
“uia”) - `Emnekode_raw`: Original course code from DBH - `Emnekode`:
Normalized course code (trailing numbers removed) - `Årstall`: Year -
`Semesternavn`: Semester name (“Vår” or “Høst”) - `Status`: Course
status (1=Active, 2=New, 3=Discontinued, 4=Discontinued but exam
offered)

### Output Data Structure

After processing, you’ll have:

    # A tibble: 2 × 5
      course_id                    url                   html  html_success fulltext
      <chr>                        <chr>                 <chr> <lgl>        <chr>   
    1 oslomet_ABC123_2024_autumn_1 https://student.oslo… <htm… TRUE         ABC123 …
    2 oslomet_XYZ456_2024_spring_1 https://student.oslo… <htm… TRUE         XYZ456 …

## Key Functions Reference

### `add_course_id(dbh_df)`

Creates unique course identifiers combining institution, code, year,
semester, and status.

### `add_course_url(df)`

Generates the correct URL for each course based on institution-specific
patterns.

### `fetch_html_with_checkpoint(courses, checkpoint_path, .progress = TRUE)`

Downloads HTML from URLs with automatic checkpointing. Skips
already-downloaded courses.

### `extract_fulltext(institution_short, raw_html)`

Extracts clean text from raw HTML using institution-specific CSS
selectors.

### `validate_courses(df, stage)`

Validates that required columns exist at each pipeline stage: -
`"initial"`: institution_short, Emnekode, Årstall - `"with_url"`:
institution_short, course_id, url - `"with_html"`: institution_short,
course_id, url, html, html_success

## Troubleshooting

**URLs look wrong?** - Check your `url_pattern` - Print a few URLs to
verify: `courses |> select(url) |> head()`

**HTML download fails?** - Check if the website is accessible - Look at
`courses$html_error` for error messages - Some institutions may block
automated requests

**Extracted text is empty or wrong?** - Verify your CSS selector using
browser dev tools - Check `config/selectors.yaml` matches the HTML
structure - Some pages may have different structures for different years

**Checkpoint file is huge?** - This is normal - HTML is large -
Checkpoint files are in `.gitignore` and won’t be committed

## File Organization

    ├── R/
    │   ├── run_harvest.R           # Main pipeline script
    │   ├── utils.R                 # Helper functions (add_course_id, validation)
    │   ├── add_course_url.R        # URL generation logic
    │   ├── fetch_html_cols.R       # HTML downloading
    │   ├── extract_fulltext.R      # Text extraction
    │   └── checkpoint.R            # Checkpoint management
    ├── config/
    │   ├── institutions.yaml       # URL patterns and aliases
    │   └── selectors.yaml          # CSS selectors per institution
    ├── data/
    │   ├── courses.RDS             # Input course data
    │   └── checkpoint/             # Checkpoint files (not in git)
    └── data-raw/
        └── courses.R               # script that creates courses.RDS

## Need Help?

Look at the existing institution examples, but be quick to contact me,
also! 🤩
