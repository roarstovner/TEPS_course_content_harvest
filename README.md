# README


# Specificiation of data harvest for TEPS Course Content

The input are the institution, course code, year and semester:

``` r
courses <- read_xlsx("data-raw/courses.xlsx")
courses |> slice(1:2)
```

      institution_short course_code year semester
    1           oslomet    MGVM4100 2024     høst
    2               uia      NO-155 2024     Vår 

This file should have at least one line for each teacher education
institution with a five year integrated “Lektorprogram”. These are
oslomet, uia, NTNU, INN, HiVolda, HiOF, HVL, MF, NLA, Nord, NIH, Samas,
Steiner, UiB, UiO, UiS, USN, UiT.

The output is a tibble with a course url, html, and full text from
website, like this

    # A tibble: 2 × 3
      url                                                            html  fulltekst
      <chr>                                                          <chr> <chr>    
    1 https://student.oslomet.no/studier/-/studieinfo/emne/MGVM4100… <hea… M5GEN100…
    2 https://[...]                                                  <hea… teksten …

We want the entire pipeline to be computational, possibly consisting of
these functions:

-   `course_url(institution_short, course_code, year, semester)`, which
    ouputs the course_url.
-   `parse_html(institution_short, url)`, which outputs the full text of
    course information with ok formatting.

Ideally, the work is added to this repository.

Below are some suggested steps. When the work proceeds, you will have
more knowledge of this than me, so feel free to suggest changes.

## 1. Create a file with one or more courses from each institution

See `data-raw/courses.xlsx` for a start.

## 2. Create `course_url` function

We would create using the `dplyr` function “case_match” and simple
string pasting.

``` r
courses |> 
  slice(1:3) |> 
  mutate(
    url = case_match(
      institution_short,
      "oslomet" ~ paste("www.oslomet.no/courses", course_code, year, semester, "index.html", sep = "/"),
      "uia" ~ paste("www.uia.no/course/info/", paste0(year, semester), course_code, "index.html", sep = "/")
    )
  )
```

      institution_short course_code year semester
    1           oslomet    MGVM4100 2024     høst
    2               uia      NO-155 2024     Vår 
    3              NTNU        <NA>   NA     <NA>
                                                       url
    1 www.oslomet.no/courses/MGVM4100/2024/høst/index.html
    2   www.uia.no/course/info//2024Vår /NO-155/index.html
    3                                                 <NA>

(Koden over lager ikke riktige URLer. Jeg bare improviserte. 😸)

## 3. Retrieve all htmls

There should be something in the rvest-package that does this
automatically? I imagine a function that adds an `html` variable to the
above tibble.

## 4. Identify the css-selector corresponding to the full course text and collect them in a .xlsx

These are the steps I’m most unsure about. I don’t know much about
webscraping. I’m basing myself on the documentation of the R-package
`rvest` and the relevant chapter of Rohan Alexander’s open textbook,
“Telling Stories with Data”.

I envision having a function `scrape_course(institution_short, html)`

1.  Get the selector gadget:
    https://rvest.tidyverse.org/articles/selectorgadget.html
2.  Use it to find the css-selector corresponding to the full text of
    the course webpage.
3.  Add code that parses this css-selector, for example to the
    `scrape_course` function.

That last step might be something like this. Only code for “OsloMet” is
shown.:

``` r
scrape_course <- function(institution_short, html){
  if(institution_short == "oslomet"){
    fulltekst <- html |> 
      read_html() |> # the rest is from the 'rvest' package
      html_elements(".oslomet-margin-wrapper-top") |> # this is the css-selector on oslomet pages
      html_text2() |> #strip html code from html. Below is just to get nice formatting:
      str_replace_all("  ", " ") |> # remove excessive whitespace
      str_replace_all("[\r]", "") |> # remove carriage returns. Don't remember why I put it in brackets
      str_replace_all("^[:space:]+", "") |> # remove spaces at the beginning of lines
      str_replace_all("[\n]+", "\n\n") # replace any number of newlines with two newlines
    return(fulltekst)
  } 
}
```
