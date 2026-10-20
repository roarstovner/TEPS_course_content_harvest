# Data Quality Notes


Generated: 2026-10-20

## Overview

| Institution | Rows | Codes | Years | Extracted OK | Rate | Median chars | Unique plans | Dedup % |
|:---|---:|---:|:---|---:|:---|---:|---:|:---|
| hiof | 1887 | 333 | 2007-2025 | 852 | 45.2% | 7151 | 826 | 3.1% |
| hivolda | 22740 | 2843 | 2000-2025 | 5181 | 22.8% | 4220 | 4331 | 16.4% |
| hvl | 3777 | 486 | 2017-2025 | 3488 | 92.3% | 4398 | 1910 | 45.2% |
| inn | 3040 | 377 | 2018-2025 | 1956 | 64.3% | 4150 | 613 | 18.7% |
| mf | 237 | 122 | 2025 | 26 | 11.0% | 4056 | 26 | 0.0% |
| nih | 363 | 122 | 2021-2025 | 95 | 26.2% | 2534 | 95 | 0.0% |
| nla | 2243 | 292 | 2017-2025 | 1831 | 81.6% | 6050 | 929 | 49.3% |
| nmbu | 218 | 114 | 2025 | 12 | 5.5% | 5900 | 12 | 0.0% |
| nord | 3645 | 520 | 2016-2025 | 3644 | 100.0% | 4066 | 1319 | 52.6% |
| ntnu | 5946 | 508 | 2004-2025 | 5538 | 93.1% | 4368 | 2100 | 62.1% |
| oslomet | 1869 | 276 | 2018-2025 | 1869 | 100.0% | 10359 | 719 | 59.4% |
| samas | 130 | 68 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| steiner | 244 | 125 | 2025 | 15 | 6.1% | 5027 | 15 | 0.0% |
| uia | 3067 | 341 | 2013-2025 | 1182 | 38.5% | 4160 | 616 | 43.3% |
| uib | 2827 | 274 | 2004-2025 | 1513 | 53.5% | 3324 | 582 | 59.4% |
| uio | 286 | 151 | 2025 | 47 | 16.4% | 4623 | 47 | 0.0% |
| uis | 2921 | 316 | 2007-2025 | 2719 | 93.1% | 5760 | 1307 | 50.2% |
| uit | 6868 | 714 | 2004-2025 | 3232 | 47.1% | 4259 | 2324 | 27.6% |
| usn | 3219 | 493 | 2018-2025 | 1282 | 39.8% | 10538 | 1224 | 4.4% |

**Total**: 65527 rows, 34482 with extracted text (52.6%), 19410 unique
plans.

**Columns:**

- **Rows**: Total course-semester rows in harvested data (from DBH)
- **Codes**: Unique course codes (course_code_raw)
- **Extracted OK**: Rows with successfully extracted course plan text
- **Rate**: Extraction success rate (Extracted OK / Rows)
- **Median chars**: Median character count of extracted text
- **Unique plans**: Distinct course plans after normalization and
  deduplication
- **Dedup %**: Percentage of rows removed by deduplication (1 -
  unique/has_plan)

## Year coverage

Sites that show only the plan in force (`plan_years = "current"` in
`R/institution_config.R`) give, at each harvest, the plan of that
academic year only. Each harvest is kept, and an offering gets the plan
from a harvest made in its own academic year, or none (README “Sites
that show only the current plan”). In this data the harvests are of
2026-04-04 (2025/26: autumn 2025 offerings) and 2026-09-30 (2026/27: no
DBH offerings yet), so spring 2025 offerings (2024/25) have no plan at
these sites. They are mf, nmbu, samas, steiner, uio. The others have
historical plans as far back as the site still serves them. Share of
offerings with a plan, by period:

| Institution | –2014 | 2015–17 | 2018–19 | 2020–21 | 2022–23 | 2024– |
|:------------|:------|:--------|:--------|:--------|:--------|:------|
| hiof        | 4%    | 100%    | 43%     | 49%     | 45%     | 46%   |
| hivolda     | 0%    | 30%     | 45%     | 45%     | 48%     | 48%   |
| hvl         |       | 61%     | 97%     | 94%     | 93%     | 91%   |
| inn         |       |         | 0%      | 0%      | 100%    | 100%  |
| mf          |       |         |         |         |         | 11%   |
| nih         |       |         |         | 7%      | 25%     | 35%   |
| nla         |       | 36%     | 81%     | 84%     | 85%     | 81%   |
| nmbu        |       |         |         |         |         | 6%    |
| nord        |       | 100%    | 100%    | 100%    | 100%    | 100%  |
| ntnu        | 81%   | 97%     | 98%     | 98%     | 98%     | 98%   |
| oslomet     |       |         | 100%    | 100%    | 100%    | 100%  |
| samas       |       |         |         |         |         | 0%    |
| steiner     |       |         |         |         |         | 6%    |
| uia         | 0%    | 0%      | 0%      | 48%     | 46%     | 48%   |
| uib         | 30%   | 44%     | 58%     | 81%     | 71%     | 81%   |
| uio         |       |         |         |         |         | 16%   |
| uis         | 92%   | 96%     | 95%     | 91%     | 92%     | 94%   |
| uit         | 50%   | 44%     | 40%     | 46%     | 53%     | 51%   |
| usn         |       |         | 35%     | 45%     | 37%     | 41%   |

## Semester registration in DBH

DBH lists most courses under both spring and autumn of a year, also when
the course is taught, and its plan published, in one semester only. How
this shows up depends on the website:

- **Semester-specific pages** (HiOF, Hivolda, NIH, NLA, UiA, USN): the
  other semester has no page (a 404, or no USN version), so about half
  of the rows lack text by design.
- **One page per course or year**: both rows get the same page, and
  deduplication collapses them.

The row-level **Rate** in the overview therefore understates coverage
for the first group. To compare institutions, count course-years (code ×
year) that have a plan in at least one semester:

| Institution | Course-years | Both semesters in DBH | Both reg., plan in one | Both reg., plan in both | Both reg., no plan | Course-years with plan | Row rate |
|:---|---:|:---|---:|---:|---:|:---|:---|
| hiof | 1029 | 83% | 720 | 8 | 130 | 82% | 45.2% |
| hivolda | 12913 | 76% | 4412 | 193 | 5222 | 39% | 22.8% |
| hvl | 2025 | 87% | 165 | 1542 | 45 | 96% | 92.3% |
| inn | 1593 | 91% | 635 | 42 | 770 | 45% | 64.3% |
| mf | 122 | 94% | 25 | 0 | 90 | 21% | 11.0% |
| nih | 200 | 82% | 72 | 0 | 91 | 48% | 26.2% |
| nla | 1191 | 88% | 68 | 826 | 158 | 84% | 81.6% |
| nmbu | 114 | 91% | 11 | 0 | 93 | 11% | 5.5% |
| nord | 1973 | 85% | 105 | 1228 | 339 | 79% | 100.0% |
| ntnu | 2950 | 80% | 100 | 2242 | 18 | 97% | 93.1% |
| oslomet | 993 | 88% | 44 | 812 | 20 | 97% | 100.0% |
| samas | 68 | 91% | 0 | 0 | 62 | 0% | 0.0% |
| steiner | 125 | 95% | 15 | 0 | 104 | 12% | 6.1% |
| uia | 1604 | 91% | 1024 | 3 | 436 | 68% | 38.5% |
| uib | 1425 | 84% | 14 | 669 | 507 | 52% | 53.5% |
| uio | 151 | 89% | 44 | 0 | 91 | 31% | 16.4% |
| uis | 1602 | 82% | 41 | 1165 | 113 | 91% | 93.1% |
| uit | 3699 | 86% | 1113 | 887 | 1169 | 63% | 47.1% |
| usn | 1752 | 84% | 1058 | 33 | 376 | 71% | 39.8% |

## Course-years without a plan

Why a course-year (code × year) has no plan in any semester, from
`data/interim/plan_gaps.RDS` (target `gaps`, `plan_gaps()` in
`R/pipeline.R`). **No URL**: the site has no page for that year, or URL
discovery found none (hivolda, uit, usn). **404**: the page was not
found. **Fetch error**: another error (ntnu’s “no information” pages,
uib). **Page, no plan**: a page shell, a page that is not the plan, or a
year missing from NLA’s JSON. **No page**: a current site without a
harvest in that year. **Between plans**: the course has a plan the year
before and the year after, so the gap is the likeliest to be a wrong URL
or a missed page; these are the ones to check by hand (`url_tried`, and
`url_next_door`, the page of an adjacent year that worked).

| Institution | Without plan | 404 | No URL | Fetch error | Page, no plan | No page | Between plans |
|:---|---:|---:|---:|---:|---:|---:|---:|
| hiof | 185 | 185 | 0 | 0 | 0 | 0 | 6 |
| hivolda | 7804 | 0 | 7804 | 0 | 0 | 0 | 133 |
| hvl | 79 | 79 | 0 | 0 | 0 | 0 | 1 |
| inn | 881 | 0 | 584 | 0 | 297 | 0 | 7 |
| mf | 96 | 0 | 0 | 0 | 0 | 96 | 0 |
| nih | 104 | 103 | 0 | 1 | 0 | 0 | 2 |
| nla | 186 | 165 | 0 | 0 | 21 | 0 | 2 |
| nmbu | 98 | 0 | 0 | 0 | 0 | 98 | 0 |
| nord | 413 | 1 | 0 | 0 | 412 | 0 | 0 |
| ntnu | 81 | 1 | 0 | 74 | 0 | 6 | 0 |
| oslomet | 34 | 0 | 0 | 0 | 34 | 0 | 0 |
| samas | 68 | 0 | 68 | 0 | 0 | 0 | 0 |
| steiner | 100 | 0 | 100 | 0 | 0 | 0 | 0 |
| uia | 521 | 521 | 0 | 0 | 96 | 0 | 92 |
| uib | 691 | 521 | 0 | 140 | 36 | 0 | 0 |
| uio | 104 | 0 | 7 | 0 | 0 | 97 | 0 |
| uis | 145 | 12 | 0 | 0 | 133 | 0 | 0 |
| uit | 1365 | 0 | 1362 | 0 | 13 | 0 | 50 |
| usn | 485 | 0 | 485 | 0 | 1 | 0 | 30 |

To list the gaps between plans:
`readRDS("data/interim/plan_gaps.RDS") |> dplyr::filter(between_plans)`.

Checked 2026-10-07 and 2026-10-08, all 208 gaps between plans: one was a
wrong URL, UiT’s ENG-2980 spring 2017 (an English twin, see UiT), so 207
remain. The 404s at HiOF (11 of 12 URLs; the twelfth is now an empty
page), HVL (5 of 5, soft 404) and NIH (4 of 4) are still 404. No URL
means the site has no page for that year: UiT’s semester picker has no
option for it (all 49; UiT’s other 3 are pages without a plan), USN
shows another year’s plan for every version number (all 30, both
semesters), and Hivolda’s course page lists no version for it (all 10,
mostly 2024). NTNU’s 3 (2010) still say “Det finnes ingen informasjon
for dette studieåret”; INN’s 7 show a later plan version. UiA’s 91 are
its autumn-2023 page shells (see UiA), and REL410 2024 is registered in
spring while UiA publishes it under autumn (the only such case among
UiA’s 74 course-years since 2020 that are 404).

## Interpreting the dedup ratio

High dedup (\>40%) is expected for current-year-only institutions where
DBH registers courses for both semesters but only one plan page exists
(see “Semester registration in DBH”).

Low dedup (\<10%) means most rows have genuinely different content,
typically because plans change between years/versions (HiOF, USN) or
each row is truly distinct (NIH, Steiner, UiO).

## robots.txt

The robots.txt of every host the harvests request, and of the new
institutions’ sites, was read on 2026-10-16; `data/robots_txt.csv` has
the date, the crawl delay and the disallowed paths that touch course
plans per host. The rules for all user agents apply: no robots.txt names
the harvest’s user agent.

- **Crawl delay.** NTNU asks for 10 seconds between requests and USN for
  5, which the harvest waits (`request_delay`; USN’s resolver waited 2
  seconds before 2026-10-16). UiB is harvested at 10 seconds since
  2026-09-30, although its robots.txt asks this only of one crawler
  (Scrapy) on 2026-10-16. BI’s course information site
  (programmeinfo.bi.no) asks for 10 seconds
- **Disallowed paths the harvests used.** Two sources of plans in the
  harvests up to 2026-10 are paths robots.txt disallows for all user
  agents: UiS’s PDF archive on `www6.uis.no` (see UiS) and Steiner’s
  subject PDFs under `/dokumenter/` (see Steiner). No other path the
  harvests request is disallowed. Since 2026-10-17 the harvest does not
  request a disallowed path unless a permission is recorded in the
  config: no institution has granted one (the project asks UiS);
  Steiner’s PDFs are fetched by the project lead’s decision of
  2026-10-19 (open data, five files), not by Steiner’s permission, and a
  host whose robots.txt has not been read stops the harvest. At the new
  institutions’ sites robots.txt disallows only the CMS’s own paths,
  search and staff pages, except that BI’s main site disallows PDF and
  Word files: a BI plan published as such a file would not be harvested

## Per-Institution Notes

Figures are in the tables above; the notes say why.

------------------------------------------------------------------------

### HiOF (Ostfold University College)

- Plans before autumn 2021 use another URL structure;
  `add_course_url_hiof()` switches by date, and the selector lists
  fallbacks for both page layouts
- Needs a browser-like user agent (`user_agent = "browser"`) to avoid
  403s
- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- Reading lists are part of the plan text; `.anon_hiof()` removes only
  the “Litteraturlista er sist oppdatert” line

------------------------------------------------------------------------

### Hivolda (Volda University College)

- Semester-specific pages have numeric ids
  (e.g. `/emne/MGL5-10NO2B/12177`) that metadata cannot give;
  `resolve_urls_hivolda_batch()` reads them from the course’s base page,
  matching “Haust” to “Høst”
- Rows without a plan are mostly semesters in which the course was not
  taught
- All DBH courses since 2000 were harvested 2026-10-20. The course pages
  list versions from autumn 2015 on, so none of the 6,326 course-years
  before 2015 has a plan (3,981 of them are codes with no page on the
  site), nor has spring 2015. In 2016–2025, 4,769 of 5,830 course-years
  have a plan (82%); of the rest, 162 are codes with no page and 899 are
  pages that list no version for that year

------------------------------------------------------------------------

### HVL (Western Norway University of Applied Sciences)

- A page that says the plan is only in English (a shell) is fetched from
  the English site instead (`english_page` in the config):
  “Emnebeskrivelsen finnes kun på engelsk” since 2026-10-14; “Dette
  emnet er kun beskrevet på engelsk”, “Dette emnet er berre skildra på
  engelsk” and “Se engelske nettsider for mer informasjon” since
  2026-10-17 (English-taught master’s courses of 2025 in the pilot
  sample: ADA512, BI4-301, MAS538, MMO5010; their 7 offerings, whose
  pointer had counted as a plan, were fetched again that day and got the
  English plan). The retry harvest of 2026-10-15 gave all 86 offerings
  with such a page and no plan an English plan (46 plans, mostly the
  English subject modules MGBEN and MGUEN); their English headings map
  like the Norwegian ones. The 5 MGBPE201E offerings of 2023-2025 were
  not retried, since their pointer had counted as a plan; a second retry
  harvest the same day gave them their English plan (3 plans)
- Year in the URL: the academic year the page is for (`/2025/` is
  “Emneplan for studieåret 2025/2026”), so a spring offering reads the
  year before (since 2026-10-14; until then it read the next study
  year’s plan). 42 of the 162 spring 2020 offerings have no plan: HVL
  has no 2019/20 page for them (“Vi kunne ikkje finne emnet du ser
  etter” in every harvest up to 2026-10-15); 39 of them had read
  2020/21’s plan before. Soft-404 “course not found” pages are detected
  at fetch time (`fetch_html_cols_single_hvl()`) and stored as no HTML
- Read from `div.l-body`: the title, the facts aside (credits, campus,
  semesters, language, programmes) and the plan; not the year stamp and
  links, and from the aside not the course coordinators (names), the
  semester’s exam schedule and the reading-list button

------------------------------------------------------------------------

### INN (Inland Norway University of Applied Sciences)

- No plans before 2022: INN’s site starts in 2022, so the URL builder
  returns NA for earlier years
- Some pages are the course search page (“Emnesøket gjelder kun fra …”)
  instead of a plan; the anonymizer turns them into NA
- Some courses are published in both Norwegian and English
- Where the requested semester has no version, the page shows the latest
  one, mostly with the notice “Emnebeskrivelsen for valgt semester er
  ikke publisert enda. Her er den siste versjonen vi har tilgjengelig.”,
  sometimes without it (2MNK172-2 and 2MNK172S-2 spring 2022 to spring
  2025 show the version from autumn 2025; INN3024-1 in 2023 “Teaching
  semester 2027 Spring”). The version’s start semester (the selected
  “Startsemestre”, `page_year`) gives its academic year; where that is
  after the offering’s, the offering gets no plan: 1,136 offerings, 388
  of the 407 from 2022 (since 2026-10-20; before, the notice alone
  decided). The April 2026 harvest shows the same notice on the same
  pages

------------------------------------------------------------------------

### MF (MF Norwegian School of Theology)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Selector `article .content-body`: the title and
  code, the facts box (credits, semester, language, programmes; without
  its buttons) and the plan; the title and facts box are in no section
- `.anon_mf()` cuts from “Emneansvarlig” to the end (names, contact
  details, marketing)
- The reading list is on a separate page; the plan holds only a link and
  a library notice, which `.anon_mf()` removes, so MF plans have no
  `reading_list` section
- Some course pages are closed to the public and answer HTTP 403 to
  every user agent, a browser’s too (access-restricted Drupal nodes: the
  PHI courses, such as PHI2030 and PHI2115, on 2026-10-17): outcome
  `forbidden`, no plan, not fetched again by `retry = TRUE`

------------------------------------------------------------------------

### NIH (Norwegian School of Sport Sciences)

- Short year range. Most 404s are the other semester of a course
  registered in both (see “Semester registration in DBH”)
- Read from `#vrtx-fs-emne-content`: the title, the facts list (credits,
  department, semester, duration, language) and the plan; not the course
  coordinator, the table of contents’ heading and the FS fetch stamp

------------------------------------------------------------------------

### NLA (NLA University College)

- All DBH years (`plan_years = "url"`): a page per course and academic
  year, `/emneplan/{CODE}/{YYYY-YYYY}` (autumn 2024 and spring 2025 →
  2024-2025), whose embedded JSON holds the plan of that year only; the
  json block reader (`R/blocks.R`) takes the year of the row
- Harvests until 2026-10-17 fetched one page per course code, the URL of
  its first offering, and gave it to every year of the course. In
  release data-2026-10-16, 1,554 of NLA’s 2,074 teacher-ed offerings
  have another year’s page and so no plan (a plan for 2-8 % of the
  course-years 2018-2023), and 271 carry the HTTP 404 of the one URL
  requested for their course. Their own pages are fetched by a retry
  (`harvest_all(retry = c(RETRY_OUTCOMES, "not_found"))`: the 404 was
  recorded at URLs never requested); the pilot sample’s 125 were fetched
  on 2026-10-17 (61 plans, 42 not found, 22 pages of another year), and
  the other 1,811 teacher-ed offerings the same day (998 URLs: 1,459
  plans, 271 not found, 81 pages of another year; teacher-ed offerings
  with a plan 263 → 1,722)
- For a year it has no plan for, the site answers the year’s URL with
  another year’s plan (the JSON names the year), which gives the
  offering no plan: 22 of the 125 pages fetched on 2026-10-17, 14 of
  them spring 2017 (2016-2017: no NLA page in the raw store holds that
  year). So a row with a page but no plan has another year’s page: the
  site’s answer, or a harvest until 2026-10-17 not yet fetched again
- The `/studietilbud/emner/` URLs are rendered in the browser, so the
  harvest uses
  `/for-studenter/Studie-%20og%20emneplaner/emneplan/{CODE}/{YYYY-YYYY}`

------------------------------------------------------------------------

### NMBU (Norwegian University of Life Sciences)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”); few course codes. Rows without a plan are 404s for
  discontinued courses, or spring 2025 offerings

------------------------------------------------------------------------

### Nord (Nord University)

- Year and semester in the URL, with Norwegian characters
  (`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic page without
  year-specific content
- For a year it has no plan for, the page shows the current plan, headed
  “Gjeldende emnebeskrivelse (sist oppdatert 2026/27)” instead of
  “Emnebeskrivelse for 2023/24”. Where the year it names is after the
  offering’s academic year, the offering gets no plan (since
  2026-10-14): all offerings of 2016-2018 and 205 later ones, 761 in
  all. Where it is not, the page is the version in force that year
- A course taught in English (2025 in the pilot sample: BIO5023,
  BIO5031, BIO5036) has a Norwegian page with only the title, facts and
  “Emnet foregår på engelsk. Se engelske nettsider ved å velge engelsk
  språk.” (a shell); since 2026-10-17 the English page of the same year
  and semester is fetched instead
  (`/en/studies/courses/{code}?year=…&semester=…`, `english_page` in the
  config). It has the same accordions with English headings, and its
  year stamp (“Course description for 2025/26”, “Current active course
  description (last updated 2026/27)”) is read like the Norwegian one.
  The 3 pilot offerings were fetched again that day and got the English
  plan
- Accordion pages, read with a multi-element selector
- Read from `div.node__content`: the plan and the sidebar’s facts
  (language, credits, faculty, campus) after it, in no section; not the
  sidebar’s semester picker, staff and Leganto link

------------------------------------------------------------------------

### NTNU (Norwegian University of Science and Technology)

- Year in the URL, no semester: the academic year the page is for
  (`/2025` is “Studieår 2025/2026”), so a spring or summer offering
  reads the year before (since 2026-10-14; until then it read the next
  study year’s plan). The site has no plans before 2007/08 (“Det finnes
  ingen informasjon for dette studieåret”), so the 150 spring and summer
  2007 offerings, which read 2006/07, have none; 147 of them had read
  2007/08’s plan before. DBH’s “-A” codes (MGLU1101-A, the primary
  teacher courses) are fetched without the suffix; with it the site has
  no page. Pages saying no information is available raise
  `ntnu_no_info_error`; robots.txt asks for a 10-second crawl delay
- The exam tab (FS’s exam registration: assessment form, grading scale,
  and per exam session the exam parts with weight, duration and
  permitted aids) is in `course_plan` without dates, times, rooms and
  comments; the assessment section and the plan id keep one session of
  it (`exam_sessions`). The timetable tab (from 2025) is not read.
  `.anon_ntnu()` removes the contact block (course coordinator,
  teachers); the responsible unit stays, and the dedup hash ignores it
- A course whose plan is only in English (master’s courses at Gjøvik in
  the pilot sample: IMT4127 2022, IMT4312 2025, MACS490 2025, SMF1081
  spring 2022) has a Norwegian page whose course content, and mostly
  every other part, says only “See English version.”, “Se engelsk
  beskrivelse.” or “Se engelsk tekst.”; since 2026-10-19 the English
  page on www.ntnu.edu is fetched instead (`english_page` in the
  config), with the same layout and English headings. Such a page
  fetched before gives no plan, though its exam tab would make it one.
  The 7 pilot offerings were fetched again on 2026-10-19 and got the
  English plan, as did GEOG2500 spring 2022, autumn 2022 and spring 2023
  (3 teacher-education offerings), whose course content says only “Se
  engelsk tekst.”
- The spring and autumn offerings of a year read the same page, fetched
  at different times. Between the fetches NTNU reworded the stock
  credit-overlap note (“Dette emne har …” / “Dette emnet har …”) and
  changed programme lists, so 555 pages gave two plans until 2026-10-13.
  The admission programme list, the credit-reduction box and the subject
  areas are now outside the plan id, and the banners (“Nytt fra
  studieåret …”, “Undervises ikke studieåret …”) are not read

------------------------------------------------------------------------

### OsloMet (Oslo Metropolitan University)

- The URL always uses `HØST`: spring URLs return 404. `/{YEAR}/HØST` is
  the plan of study year YEAR/YEAR+1, so a spring offering reads YEAR-1
  (since 2026-10-14). Pattern:
  `https://student.oslomet.no/studier/-/studieinfo/emne/{CODE}/{YEAR}/HØST`
- Re-harvested 2026-02-14 after the site moved to server-side rendering.
  Error pages (“Siden du leter etter finnes ikke”) become NA in the
  anonymizer
- `.anon_oslomet()` removes the “Emneansvarlig” block whole, up to
  “Emneoverlapp” or the end of the page: it holds one or more names,
  with a title, a part (“Delemne …: <name>”) or a role (“… coordinator:
  professor <name>”) (since 2026-10-17)
- **Practicum courses whose sections only say “Se fagplanen.” have no
  sections** (M1GP\*/M5GP\*, 132 of 248 offerings with a plan on
  2026-10-15; the others have text of their own and get its sections).
  The page’s “Fagplan” block is the programme-wide practicum plan for
  all study years, with no markup separating the years, so it is not
  read into the course’s sections. Their `course_plan` keeps the full
  page text

------------------------------------------------------------------------

### Samas (Sámi University of Applied Sciences)

- `noop` strategy, no plans: the website’s course codes (SÁM-1005,
  DUO-1014, …) do not correspond to DBH codes (V1SAM-1100-1, …), and the
  V1/V5 teacher education courses appear only in programme PDFs without
  per-course descriptions

------------------------------------------------------------------------

### Steiner (Rudolf Steiner University College)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Plans come from five subject PDFs at
  `steinerhoyskolen.no`, split by course heading; the PDFs are not
  stored, so the split text is the raw data
- The site’s robots.txt (read 2026-10-16) disallows `/dokumenter/`,
  where the five PDFs are, for all user agents. Every Steiner plan comes
  from that path. Since 2026-10-19 they are fetched by the project
  lead’s decision (open data, five files, negligible traffic), not by a
  permission from Steiner, which was not asked (`robots_permission` in
  the config)
- The five PDFs are those of the bachelor Steinerpedagogisk
  lærerutdanning: they cover 15 emner of five subjects, which the course
  names of that programme and of the 1-7 teacher programme (M- codes)
  match. Of DBH 2025’s 125 Steiner course codes (106 without the emne
  suffix, 244 offerings), the all-courses harvest of 2026-10-19
  (2026/27, which joins DBH 2026) found a plan for 30 (60 offerings).
  The others have no source: the kindergarten teacher programmes, the
  master in Waldorf education, social pedagogy, further education, the
  practicum modules (M-LP1/2/3, BDP, BLP, DLP) and the bachelor theses.
  Until DBH has 2026, Steiner’s plans are the 15 offerings of the
  2026-04-04 harvest (2025/26)
- M-PEL2.1/M-PEL2.2 get the PDF’s PEL1.4/PEL1.5 plans
  (`.steiner_remap_pel_emne()`), and since 2026-10-19 the bachelor’s
  PEL1.4/PEL1.5 too; the M-PEL2 match is to be confirmed against the
  programme description

------------------------------------------------------------------------

### UiA (University of Agder)

- The site has plans from 2020 on; earlier years return 404
- DBH registers most courses in both semesters, but UiA publishes one
  page, so most 404s are the other semester (see “Semester registration
  in DBH”)
- Autumn 2023: 95 pages are shells (title and facts box, no plan), so 91
  course-years have no plan between two that do. They are courses UiA
  now teaches in spring (94 pages say “Undervisningssemester: Vår”).
  Every copy UiA has rebuilt from FS since (2026-09-30, 10-06 and 10-08,
  by the page’s “Sist hentet fra FS”) is a shell, the English pages too;
  the full pages seen at 13:40 on 2026-10-07 were old copies, last
  fetched from FS in July 2024. The site no longer has these plans, so
  the offerings stay without one
- URL pattern:
  `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`

------------------------------------------------------------------------

### UiB (University of Bergen)

- Year in the URL; accordion pages read with a multi-element selector;
  harvested at 10 seconds a page (`request_delay`, set 2026-09-30 for
  the crawl delay robots.txt was read to ask; on 2026-10-16 it asks it
  only of Scrapy, and the pace is kept)
- The page’s semester picker is dropped when the HTML is parsed
- Read from `div.region-inner.grid`: the title, level and facts list
  (credits, semester, code, number of semesters, language) and the main
  column; not the sidebar (contact e-mail, the semester’s exam schedule,
  a link to the department, so the responsible unit is not in
  `course_plan`)
- About 280 pages say “Vi opplever problemer med å hente inn
  eksamensinformasjon”: UiB’s exam information service fails for these
  courses, in every year. Fetching the 294 affected pages again on
  2026-10-02 fixed 7, so it is not a harvest error. The banner is
  removed from the sections, and these courses have no assessment text
- 72 pages say “Inga emnebeskriving tilgjengeleg” (no plan for that
  semester) and give no plan; 30 of them also carry “Feil i visning av
  emnebeskrivelse”. Fetched again on 2026-10-14, 29 still carry both and
  none gives a plan, so the plan is not on the site

------------------------------------------------------------------------

### UiO (University of Oslo)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Semester URLs (`/h24/`, `/v25/`) hold logistics
  (teachers, timetable, exam dates), not the plan
- The URL holds a faculty and department folder
  (`https://www.uio.no/studier/emner/{faculty}/{department}/{CODE}/`),
  read from UiO’s sitemap, which lists only the courses taught now; a
  course not listed gets the folder of the listed courses with its code
  prefix, else of its DBH department (README “Institution-Specific
  Notes”). On 2026-10-16, 3,689 of the 5,163 course codes active in DBH
  2025 were listed. Of the 1,474 not listed (822 PhD level, 107 UNIS
  courses), a sample of 40 guessed URLs gave 11 pages; the rest were
  404s: UNIS courses (on unis.no), PhD reading-list codes (`SP`) and
  odontology specialist codes have no page at UiO
- Read from `#vrtx-content`: the title, the plan and the facts box after
  it (“Fakta om emnet”: level, credits, semesters, language,
  department), in no section
- `.anon_uio()` cuts the generic “Mer om eksamen ved UiO” link list, up
  to the facts box

------------------------------------------------------------------------

### UiS (University of Stavanger)

- Current plans come from the course web page, older ones from the PDF
  archive linked in its semester dropdown (`html_pdf_discovery`); the
  PDFs are not stored, so their text is the raw data
- The PDF archive is on `www6.uis.no`, whose robots.txt (read
  2026-10-16) disallows every path but `/ansatt/` for all user agents.
  The harvests up to 2026-10 fetched it: 2,201 of the 2,719 UiS
  offerings with a plan text (2009-2025) have it from a PDF, the other
  518 from the course page on `www.uis.no`, which robots.txt allows.
  Since 2026-10-17 a UiS harvest does not request the PDFs (outcome
  `robots`) until UiS gives permission
- A dropdown entry is an academic year (“2024 - 2025”; its PDF says
  “studietilbudet for studieår 2024-2025”), so a spring or summer
  offering reads the year before (since 2026-10-14; until then it read
  the next academic year’s plan). For a year the dropdown lacks, the
  course page itself is fetched. It shows the latest version, the one
  selected in the dropdown; where that is after the offering’s academic
  year, the offering gets no plan (since 2026-10-14): 97 offerings, most
  of 2007-2018. A course page without a dropdown names no year and stays
  (96 offerings)
- Web pages are read from the blocks of `div.article`, without
  navigation and contact footer; the facts box (“Fakta”: code, credits,
  semesters, language) is in `course_plan` without its timetable and
  Leganto links, and in no section

------------------------------------------------------------------------

### UiT (The Arctic University of Norway)

- **Historical plans** via document ids from the course page’s semester
  picker
- The plan is read from `#maincontents`: the course code and title, the
  facts (campus, semester, credits, level) and `.mainContent` (since
  2026-10; the old `.hovedfelt > main > div.col-md-12` matched nothing
  after a site change, so the 2026-10 harvest first had no text); not
  the breadcrumbs and the apply button with the application period
- `.pre_uit()` removes the “OBS! Dette emnet tilhører et tidligere
  semester / år” banner and cuts from “Andre år og semester” / “Kontakt
  oss” (staff names and e-mails) to the end; the responsible unit from
  the contact block (“Ansvarlig enhet”, without its e-mail) stays, at
  the end of the text and in no section, and the plan id leaves it out
- Some pages say “Error rendering component” where the plan should be:
  the plan failed to render on UiT’s site, and these rows have only the
  title and facts, a page shell without a plan
- The semester picker can list a semester twice: an English document
  (“Course code:”, in all 5 cases an empty page) beside the Norwegian
  one. The Norwegian one is used; the first option was used before
  2026-10-08, and a retry harvest
  (`harvest_all(institutions = "uit", retry = TRUE)`) gave these 5
  offerings their plan

------------------------------------------------------------------------

### USN (University of South-Eastern Norway)

- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- The URL holds a plan version (1-3 are tried; all URLs found are 1
  or 2) that metadata cannot give:
  `https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`.
  Invalid version/year combinations silently show another page, so
  `resolve_urls_usn_batch()` renders each candidate in Chrome and checks
  the displayed year (README “Institution-Specific Notes”)
- Content is rendered in Shadow DOM and read with `read_usn_live_html()`
- A semester with no plan of its own shows another semester’s plan under
  the banner “MERK: Emneplanen for dette semesteret er ikke klar ennå. …
  viser vi derfor emneplanen fra et tidligere semester.” Its displayed
  year is that other semester’s, so no URL is kept for it. Six pages of
  the 2026-09 harvest (HI-MOD4000 spring 2019, HI-MIG4000 and LH-HID1200
  spring 2022, LR-PRA3000 spring 2023, LRMF510 and MG1PP2 spring 2024)
  still carry the banner although they show the requested semester’s
  plan. The page read before them had shown the same plan, and the site
  keeps the banner until the page is reloaded. Loaded again on
  2026-10-13, they show the same plan text without the banner.
  `.cleanup_usn_text()` removes the banner, and the resolver now reloads
  a page that has the requested year and the banner
