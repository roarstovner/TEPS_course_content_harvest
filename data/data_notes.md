# Data Quality Notes


Generated: 2026-11-11

## Overview

| Institution | Rows | Codes | Years | Extracted OK | Rate | Median chars | Unique plans | Dedup % |
|:---|---:|---:|:---|---:|:---|---:|---:|:---|
| fhs | 6209 | 997 | 2017-2025 | 1903 | 30.6% | 4589 | 838 | 56.0% |
| fjellhaug | 532 | 277 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| himolde | 8291 | 935 | 2017-2025 | 3608 | 43.5% | 2824 | 2028 | 18.2% |
| hiof | 41168 | 6753 | 1994-2025 | 18136 | 44.1% | 4215 | 10415 | 20.5% |
| hivolda | 22740 | 2843 | 2000-2025 | 5181 | 22.8% | 4220 | 4331 | 16.4% |
| hvl | 41057 | 4799 | 2017-2025 | 29026 | 70.7% | 3758 | 13649 | 52.9% |
| inn | 28402 | 3272 | 2018-2025 | 14199 | 50.0% | 3625 | 3065 | 32.9% |
| khio | 896 | 356 | 2025-2026 | 347 | 38.7% | 1992 | 328 | 5.5% |
| mf | 605 | 311 | 2025 | 26 | 4.3% | 3304 | 26 | 0.0% |
| nhh | 1511 | 698 | 2025-2026 | 455 | 30.1% | 4111 | 425 | 1.6% |
| nih | 12285 | 1430 | 1990-2025 | 484 | 3.9% | 2376 | 442 | 7.5% |
| nla | 17435 | 2034 | 2001-2025 | 7414 | 42.5% | 5167 | 3539 | 52.3% |
| nmbu | 2734 | 1438 | 2025 | 12 | 0.4% | 5858 | 12 | 0.0% |
| nmh | 191 | 95 | 2021-2025 | 106 | 55.5% | 3173 | 72 | 32.1% |
| nord | 35900 | 5147 | 2016-2025 | 32913 | 91.7% | 3233 | 7864 | 58.3% |
| ntnu | 146312 | 12266 | 2004-2026 | 126844 | 86.7% | 3232 | 51291 | 59.3% |
| oslomet | 33318 | 3937 | 2018-2025 | 33318 | 100.0% | 3874 | 11286 | 59.5% |
| phs | 2180 | 307 | 2021-2025 | 271 | 12.4% | 3895 | 142 | 27.2% |
| samas | 130 | 68 | 2025 | 0 | 0.0% | NA | 0 | NaN% |
| steiner | 244 | 125 | 2025 | 15 | 6.1% | 5027 | 15 | 0.0% |
| uia | 55961 | 5047 | 2012-2025 | 10291 | 18.4% | 2944 | 5364 | 44.4% |
| uib | 84912 | 6163 | 2004-2026 | 44944 | 52.9% | 3295 | 17375 | 57.3% |
| uio | 10488 | 5380 | 2025 | 47 | 0.4% | 4331 | 47 | 0.0% |
| uis | 2921 | 316 | 2007-2025 | 2719 | 93.1% | 5760 | 1307 | 50.2% |
| uit | 133066 | 10772 | 2004-2025 | 48389 | 36.4% | 3286 | 35388 | 21.8% |
| usn | 38706 | 5286 | 2018-2025 | 13132 | 33.9% | 5842 | 12361 | 5.7% |
| vid | 4082 | 917 | 2023-2025 | 810 | 19.8% | 3640 | 588 | 23.5% |

**Total**: 732276 rows, 394590 with extracted text (53.9%), 186879
unique plans.

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
from a harvest made in its own academic year, or none
(`docs/harvesting.md` “Sites that show only the current plan”). In this
data the harvests are of 2026-04-04 (2025/26: autumn 2025 offerings) and
2026-09-30 (2026/27: no DBH offerings yet), so spring 2025 offerings
(2024/25) have no plan at these sites. They are fjellhaug, mf, nmbu,
samas, steiner, uio. The others have historical plans as far back as the
site still serves them. Share of offerings with a plan, by period:

| Institution | –2014 | 2015–17 | 2018–19 | 2020–21 | 2022–23 | 2024– |
|:------------|:------|:--------|:--------|:--------|:--------|:------|
| fhs         |       | 0%      | 5%      | 34%     | 79%     | 83%   |
| fjellhaug   |       |         |         |         |         | 0%    |
| himolde     |       | 0%      | 38%     | 49%     | 53%     | 49%   |
| hiof        | 34%   | 47%     | 51%     | 58%     | 59%     | 61%   |
| hivolda     | 0%    | 30%     | 45%     | 45%     | 48%     | 48%   |
| hvl         |       | 34%     | 73%     | 72%     | 74%     | 82%   |
| inn         |       |         | 0%      | 0%      | 100%    | 100%  |
| khio        |       |         |         |         |         | 39%   |
| mf          |       |         |         |         |         | 4%    |
| nhh         |       |         |         |         |         | 30%   |
| nih         | 0%    | 0%      | 0%      | 0%      | 13%     | 33%   |
| nla         | 0%    | 10%     | 65%     | 71%     | 82%     | 82%   |
| nmbu        |       |         |         |         |         | 0%    |
| nmh         |       |         |         | 25%     | 66%     | 73%   |
| nord        |       | 77%     | 92%     | 97%     | 99%     | 99%   |
| ntnu        | 81%   | 70%     | 88%     | 92%     | 93%     | 93%   |
| oslomet     |       |         | 100%    | 100%    | 100%    | 100%  |
| phs         |       |         |         | 0%      | 4%      | 28%   |
| samas       |       |         |         |         |         | 0%    |
| steiner     |       |         |         |         |         | 6%    |
| uia         | 0%    | 0%      | 0%      | 40%     | 38%     | 41%   |
| uib         | 34%   | 39%     | 48%     | 54%     | 57%     | 70%   |
| uio         |       |         |         |         |         | 0%    |
| uis         | 92%   | 96%     | 95%     | 91%     | 92%     | 94%   |
| uit         | 37%   | 31%     | 36%     | 40%     | 40%     | 38%   |
| usn         |       |         | 28%     | 33%     | 36%     | 40%   |
| vid         |       |         |         |         | 0%      | 31%   |

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
| fhs | 3323 | 87% | 15 | 850 | 2021 | 32% | 30.6% |
| fjellhaug | 277 | 92% | 0 | 0 | 255 | 0% | 0.0% |
| himolde | 4373 | 90% | 2001 | 134 | 1783 | 54% | 43.5% |
| hiof | 24063 | 71% | 8772 | 1244 | 7089 | 49% | 44.1% |
| hivolda | 12913 | 76% | 4412 | 193 | 5222 | 39% | 22.8% |
| hvl | 21818 | 88% | 4036 | 11705 | 3498 | 79% | 70.7% |
| inn | 14923 | 90% | 2867 | 659 | 9953 | 26% | 50.0% |
| khio | 452 | 98% | 341 | 0 | 103 | 77% | 38.7% |
| mf | 311 | 95% | 25 | 0 | 269 | 8% | 4.3% |
| nhh | 775 | 95% | 432 | 0 | 304 | 56% | 30.1% |
| nih | 7000 | 76% | 371 | 20 | 4894 | 7% | 3.9% |
| nla | 9535 | 83% | 876 | 3082 | 3942 | 45% | 42.5% |
| nmbu | 1438 | 90% | 11 | 0 | 1285 | 1% | 0.4% |
| nmh | 100 | 91% | 14 | 45 | 32 | 61% | 55.5% |
| nord | 19459 | 84% | 1053 | 8177 | 7211 | 55% | 91.7% |
| ntnu | 75873 | 92% | 6463 | 56868 | 6472 | 91% | 86.7% |
| oslomet | 17639 | 89% | 2040 | 12246 | 1393 | 89% | 100.0% |
| phs | 1144 | 91% | 131 | 25 | 880 | 15% | 12.4% |
| samas | 68 | 91% | 0 | 0 | 62 | 0% | 0.0% |
| steiner | 125 | 95% | 15 | 0 | 104 | 12% | 6.1% |
| uia | 29493 | 90% | 8716 | 152 | 17600 | 32% | 18.4% |
| uib | 43767 | 94% | 230 | 19796 | 20907 | 48% | 52.9% |
| uio | 5380 | 95% | 44 | 0 | 5064 | 1% | 0.4% |
| uis | 1602 | 82% | 41 | 1165 | 113 | 91% | 93.1% |
| uit | 69992 | 90% | 25285 | 8571 | 29218 | 52% | 36.4% |
| usn | 20631 | 88% | 11145 | 422 | 6508 | 61% | 33.9% |
| vid | 2113 | 93% | 569 | 73 | 1327 | 33% | 19.8% |

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
| fhs | 2263 | 8 | 2255 | 0 | 0 | 0 | 2 |
| fjellhaug | 277 | 0 | 76 | 0 | 0 | 201 | 0 |
| himolde | 1990 | 0 | 1928 | 0 | 279 | 0 | 110 |
| hiof | 12166 | 4419 | 7034 | 105 | 1760 | 0 | 359 |
| hivolda | 7804 | 0 | 7804 | 0 | 0 | 0 | 133 |
| hvl | 4564 | 4542 | 0 | 0 | 39 | 0 | 24 |
| inn | 10940 | 0 | 7408 | 0 | 3532 | 0 | 44 |
| khio | 105 | 0 | 0 | 0 | 0 | 105 | 0 |
| mf | 285 | 0 | 0 | 0 | 0 | 285 | 0 |
| nhh | 343 | 0 | 262 | 0 | 23 | 58 | 0 |
| nih | 6473 | 6426 | 0 | 44 | 6 | 0 | 5 |
| nla | 5145 | 3524 | 0 | 0 | 1637 | 0 | 7 |
| nmbu | 1331 | 0 | 0 | 0 | 0 | 1331 | 0 |
| nmh | 39 | 0 | 39 | 0 | 0 | 0 | 0 |
| nord | 8688 | 1568 | 0 | 146 | 6974 | 0 | 0 |
| ntnu | 7182 | 625 | 0 | 6441 | 556 | 0 | 10 |
| oslomet | 1996 | 0 | 0 | 0 | 1996 | 0 | 13 |
| phs | 967 | 21 | 962 | 0 | 64 | 0 | 0 |
| samas | 68 | 0 | 68 | 0 | 0 | 0 | 0 |
| steiner | 100 | 0 | 96 | 0 | 0 | 4 | 0 |
| uia | 19928 | 19920 | 0 | 0 | 635 | 0 | 498 |
| uib | 22901 | 20665 | 0 | 157 | 2104 | 0 | 8 |
| uio | 5320 | 0 | 7 | 0 | 0 | 5313 | 0 |
| uis | 145 | 12 | 0 | 0 | 133 | 0 | 0 |
| uit | 31183 | 0 | 30826 | 0 | 1915 | 0 | 1775 |
| usn | 7665 | 0 | 7665 | 0 | 21 | 0 | 458 |
| vid | 1417 | 204 | 966 | 0 | 247 | 0 | 0 |

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

## Coverage over course-years that had students

DBH lists courses that did not run in a year. Coverage is therefore also
given over two smaller sets of course-years (code with version × year),
from DBH’s latest student counts, a snapshot dated apart from the course
list (`docs/data-files.md`, “Input Data”; methods.qmd, “Population and
sampling frame”): **With students**, course-years with exam
registrations (table 905) or credits earned (table 900) in some semester
of the year (`had_students`); **With exam registrations and credits**,
both. **All** is every course-year in the data. A share is the
course-years in the set with a plan in some semester over the
course-years in the set: a plan of a course-year without students counts
only under All, and no plan is dropped. The counts sit in the exam
semester and DBH publishes them about a semester late: years marked
**provisional** (`counts_provisional`) lack the latest autumn, and their
sets with students and with both can still grow (the set with both holds
only complete years).

All years, by institution:

| Institution | Some year provisional | All | With students | With exam registrations and credits |
|:---|:---|:---|:---|:---|
| fhs | FALSE | 1053 / 3323 (32%) | 759 / 1374 (55%) | 733 / 1223 (60%) |
| fjellhaug | FALSE | 0 / 277 (0%) | 0 / 142 (0%) | 0 / 108 (0%) |
| himolde | FALSE | 2346 / 4373 (54%) | 2143 / 2864 (75%) | 2087 / 2692 (78%) |
| hiof | FALSE | 11864 / 24063 (49%) | 10499 / 17087 (61%) | 9609 / 14296 (67%) |
| hivolda | FALSE | 4988 / 12913 (39%) | 4513 / 10386 (43%) | 3955 / 9006 (44%) |
| hvl | FALSE | 17252 / 21818 (79%) | 14080 / 15344 (92%) | 12845 / 13585 (95%) |
| inn | FALSE | 3908 / 14923 (26%) | 3216 / 11169 (29%) | 2972 / 10132 (29%) |
| khio | TRUE | 347 / 452 (77%) | 226 / 316 (72%) | 0 / 79 (0%) |
| mf | FALSE | 26 / 311 (8%) | 25 / 199 (13%) | 22 / 167 (13%) |
| nhh | TRUE | 432 / 775 (56%) | 169 / 261 (65%) | 0 / 49 (0%) |
| nih | FALSE | 458 / 7000 (7%) | 410 / 4852 (8%) | 389 / 4291 (9%) |
| nla | FALSE | 4332 / 9535 (45%) | 3513 / 6271 (56%) | 3149 / 5106 (62%) |
| nmbu | FALSE | 12 / 1438 (1%) | 10 / 998 (1%) | 8 / 879 (1%) |
| nmh | FALSE | 61 / 100 (61%) | 46 / 57 (81%) | 37 / 42 (88%) |
| nord | FALSE | 10679 / 19459 (55%) | 8182 / 13181 (62%) | 7359 / 11484 (64%) |
| ntnu | TRUE | 68689 / 75873 (91%) | 51314 / 54610 (94%) | 44163 / 46614 (95%) |
| oslomet | FALSE | 15643 / 17639 (89%) | 13201 / 13720 (96%) | 12103 / 12335 (98%) |
| phs | FALSE | 170 / 1144 (15%) | 123 / 523 (24%) | 116 / 456 (25%) |
| samas | FALSE | 0 / 68 (0%) | 0 / 37 (0%) | 0 / 8 (0%) |
| steiner | FALSE | 15 / 125 (12%) | 13 / 70 (19%) | 5 / 45 (11%) |
| uia | FALSE | 9497 / 29493 (32%) | 7387 / 20245 (36%) | 6708 / 17811 (38%) |
| uib | TRUE | 20855 / 43767 (48%) | 16932 / 27981 (61%) | 14178 / 22475 (63%) |
| uio | FALSE | 47 / 5380 (1%) | 41 / 3139 (1%) | 32 / 2655 (1%) |
| uis | FALSE | 1457 / 1602 (91%) | 1219 / 1329 (92%) | 957 / 1037 (92%) |
| uit | FALSE | 36703 / 69992 (52%) | 29963 / 38036 (79%) | 26353 / 31788 (83%) |
| usn | FALSE | 12686 / 20631 (61%) | 11707 / 14865 (79%) | 11072 / 13301 (83%) |
| vid | FALSE | 696 / 2113 (33%) | 648 / 1408 (46%) | 619 / 1261 (49%) |

By year since 2010 (a panel per institution; empty panels are sites that
publish only the current plan, whose 2026/27 offerings are not yet in
DBH):

![](data_notes_files/figure-commonmark/coverage-by-year-1.png)

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

### FHS (Norwegian Defence University College)

- Added 2026-11-04 (all-courses frame). Course pages on Forsvaret’s site
  from autumn 2018, a page per version of the plan; an offering gets the
  latest version that starts in or before its semester, the one in force
  (`resolve_urls_fhs_batch()`). DBH’s offerings before the
  reorganisation of FHS’s education (2016-2019; codes such as M5-023,
  SEF1001MAD, VBU1-LED) have no page: in the pilot of 2026-11-04 none of
  47 course-years of 2018 and 3 of 47 of 2019 had one, against 36 of 39
  in 2022 and 40 of 45 in 2025
- A page is `/utdanning/emner/{CODE}/{year}-{HØST|VÅR}`; the code’s
  page, `/utdanning/emner/{CODE}`, lists the versions and answers 404
  for a code without pages. Read from `#main-container article`: the
  title, the facts box (credits, academic year, programmes, English
  title) and the plan; not the “Oppdatert” stamp and the versions menu.
  The title and facts box are in no section
- Forsvaret’s site has no robots.txt (404, read 2026-10-16); its sitemap
  answers 500

------------------------------------------------------------------------

### Fjellhaug (Fjellhaug International University College)

- Added 2026-11-05 (all-courses frame). Only the plan in force is
  published (`plan_years = "current"`; see “Year coverage”): no plan for
  offerings before the academic year of the first harvest (2026/27).
  Earlier plans are only in PDFs (the programmes’ study plans per
  semester from 2007, a course catalogue for autumn 2026;
  `/om-fih/arkiv-studieplaner`), which the harvest does not read
- A page per course code, `/emne/{code}` or, for most courses taught in
  English, `/en/subjects/{code}`, a few with the title after the code;
  found in the subject sitemaps (`resolve_urls_fjellhaug_batch()`,
  `docs/institutions.md`). DBH’s courses at the Danish campuses (`-DK1`
  København, `-DK2` Aarhus; 206 of the 532 offerings of 2025) and the
  variants `-IS` and `-EVU` take the page of the code without the
  suffix, which names the campuses in its facts box
- Read from `main#main`: the title, the facts box (code, credits,
  campus, language, start, duration) and the plan; not the breadcrumb,
  the menu of the plan’s parts and the plan’s second copy in accordions
  for phones. The title and facts box are in no section
- A private college; the site states no terms of use, only a privacy
  statement for visitors (checked 2026-11-05)
- robots.txt (fih.fjellhaug.no, read 2026-11-05) disallows only Craft
  CMS’s `/cpresources/`, `/vendor/`, `/.env` and `/cache/`

------------------------------------------------------------------------

### HiOF (Ostfold University College)

- A plan sits in a faculty or department folder that DBH does not give,
  and a course no longer taught moves to `historiske-emner/`
  (`docs/institutions.md`); since 2026-10-21 `resolve_urls_hiof_batch()`
  reads the folder listings of the harvested years, where before only
  the teacher-education folders were built (11 of the 2026-10-17 pilot’s
  100 course-years had a page; 50 after the re-harvest of 2026-10-21).
  The selector lists fallbacks for the old and new page layouts
- Course-years without a page in the pilot sample (2026-10-21): the site
  lists no page of that code in that year’s folders. Most are DBH codes
  the site never used: 18 of the 25 in 2007 (the site’s 2007 codes are
  others, e.g. `hsaku10107` where DBH has AKU200), the PhD courses (no
  PhD folder under `/studier/emner/`), the administration’s courses
  (FEDV316, FEHP1509; `fellesemner/` starts in 2022) and the Academy of
  Performing Arts (`nta/` lists 6 pages in 2019 and none after 2021, so
  0 of its 10 course-years). A few have their page in the year before
  only: LMUKRLE11323 and LMUSPA11219, registered in 2025, are in
  `lusp/lusp/2024/`
- Courses taught in English may have their page only in the English tree
  (`/english/studies/courses/`), which the URL discovery reads since
  2026-10-28: the PhD courses (PHDDS, `iio/iio/`), the Academy of
  Performing Arts (`nta/`, 2024 and 2025) and the English master’s
  programmes (IRMGR, ITI). In the full harvest of 2026-10-28, 128 of the
  1833 offerings of 2024 and 2025 without a URL have a page there (ITI
  28, IRMGR 17, PHDDS 16, the Academy 35). Of the rest, 853 are the
  other semester of a course with a page in one, 259 are Academy and PhD
  offerings with no page in either tree, and some 590 are codes the site
  lists no page for in that year (an old code version DBH still
  registers, 198 of them with status “Avviklet”, or a course not taught
  that year; direct URLs checked 2026-10-28 answer 404)
- The offerings of 2024 and 2025 still without a URL after the retry of
  2026-11-02/03 (1042) are not a gap in the URL discovery. Over
  course-years: 560 of them are the other semester of a course-year with
  a plan, and only 77 course-years with students (`had_students`; 43
  with both exam registrations and credits) have no plan and an offering
  without a URL. Read again on 2026-11-04 (the listings of 2023–2025,
  both trees), the listings give none of the 1042 a page, and direct
  URLs in the likely folders answer 404 (IRF14121, HOVPAM10320, LUVTM113
  in 2025; FEUHPDIG20, HSSHV40517 in 2024). Of the 77, 58 have their
  code’s page only in another year, mostly the autumn before (IRF14121
  “Høst 2024”, none in 2025): the site no longer publishes the course,
  while DBH registers students in it (35 of the 77 have status
  “Avviklet” or “Avviklet med aktive studenter”), and the spring
  offering takes that autumn page but gets no plan, since the page is
  for that autumn only; 17 have no page in 2023–2025 at all (the
  Academy’s AACT and ASC, FEUHPDIG20, KUDRKUNST); 6 are an old version
  or variant of a code whose new one has a page that year (ITF20119 and
  ITF20123, IRM25016 and IRM25021, LSNOR10221FN and LSNOR10221), which
  is a different plan and is not matched. Of all 1369 course-years with
  students in 2024 and 2025, 111 have no plan: the 77, 29 whose page was
  fetched but is a page shell, the title and facts box only (ITI41222,
  HOVVEIL123, DIGLED10122: 270–740 characters of text) or shows a later
  semester (`videre/2025/host/lskas10116`, “Høst 2026”), and 5
  teacher-education course-years whose URL in `lusp/lusp/` answered 404
  on 2026-09-30 (LMAGENT21, LMBPED10522)
- Needs a browser-like user agent (`user_agent = "browser"`) to avoid
  403s
- Most rows without a plan are the other semester of a course registered
  in both (see “Semester registration in DBH”)
- Reading lists are part of the plan text; `.anon_hiof()` removes only
  the “Litteraturlista er sist oppdatert” line
- Two `videre/` pages show a later semester’s plan than their URL
  (`videre/2019/var/lskno22215` is titled “Vår 2020”,
  `videre/2022/host/lskas10116` “Høst 2023”): `page_term` reads the
  semester in the page title, so the two offerings get no plan
  (2026-10-22). No offering of the teacher-education frame and no page
  in another folder is affected (their title semester equals the URL’s,
  or the title has none).

------------------------------------------------------------------------

### HiMolde (Molde University College)

- Added 2026-10-31 (all-courses frame). Pages from autumn 2018 (2017:
  two pages, MAR300 and MAR301): DBH’s offerings of 1994-2017 (some
  8 700) have no plan on the site. Some DBH courses have no page in the
  year’s listings (checked for 2025: most PhD courses, DRL and DRHS, and
  several health master’s courses, MAK7xx; direct URLs answer 404). In
  the pilot (2026-11-06), 56 of 100 sampled course-years have a page
  (2018 10 of 25, 2019 16, 2022 16, 2025 14); the listings of 2022 and
  2025 hold no page for the others (JUR216, PMP300, SCM410; SND102,
  SNH102 and VIVO-2 only in 2022, ADM499 only in 2025)
- A spring offering whose only page is the autumn before’s, for that
  autumn alone, gets no plan (25 of the pilot’s 189 offerings; the
  page’s plan ends the term before, `page_end_term`). One page, VPL023
  spring 2019, is a page shell
- A plan sits in a department folder DBH does not give, Norwegian or
  English tree; `resolve_urls_himolde_batch()` reads the folder listings
  (`docs/institutions.md`). An offering whose code the listings of its
  year do not hold gets no URL
- robots.txt disallows only `/vrtx`, which the harvest does not request
- Read from `#vrtx-fs-emne-content`: the title, the facts list (credits,
  department, campus, language, level, duration) and the plan; not the
  course coordinator, a guest lecturer’s line or biography in the plan’s
  text (“Lecturer:”, seminar courses), the timetable and reading-list
  links, the reading-list widget (“Pensumoversikt” and the titles of the
  course’s Leganto lists), the table of contents’ heading and the FS
  fetch stamp. The programme list is in the facts. The reading list
  itself is in Leganto: most plans’ “Pensum” holds only a link or a
  notice, which is no `reading_list` section

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
  semester 2027 Spring”). Where the version’s start semester (the
  selected “Startsemestre”, `page_term`) is after the offering’s
  semester, the offering gets no plan: 1,151 offerings, 388 of the 407
  from 2022 (since 2026-10-20; before, the notice alone decided, and
  briefly the academic year, which kept a version starting the spring
  after an autumn offering: 15 offerings, 2023-2025). The banner
  “Statusmelding / Kun tilgjengelig på engelsk” (or “Kun tilgjengelig
  på” with the language left out, 26 pages) over a plan published in
  English only is cut as site boilerplate (since 2026-10-20). The April
  2026 harvest shows the same notice on the same pages

------------------------------------------------------------------------

### KHiO (Oslo National Academy of the Arts)

- Added 2026-11-01 (all-courses frame). Only the plan in force is
  published (`plan_years = "current"`; see “Year coverage”): no plan for
  offerings before the academic year of the first harvest (2026/27)
- A page per course version, `/emner/{code}-{version}` (DBH’s code with
  its version number, lower case: KK202-4 is `/emner/kk202-4`); the code
  without a version has no page. A version no longer taught keeps a page
  with its title, facts list and the plan’s headings and no text under
  them (KK202-3, 2026-11-01): outcome `not_found`, no plan
- Read from `main section.content`: the title, the facts list (credits,
  code, the programmes the course belongs to) and the plan; the title
  and facts list are in no section. The page has no year: it shows the
  version’s plan as it is now
- Courses taught in English (61 of the 703 offerings of 2025) have a
  full Norwegian page, which is used, and an English one at
  `/en/courses/{code}`
- robots.txt (read 2026-10-16) has only comments: nothing disallowed
- Three doctoral courses in DBH (DG900-1, DG930-1, DG950-1) have no
  page: the site answers 404 (pilot harvest, 2026-11-01)

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

### NHH (Norwegian School of Economics)

- Added 2026-10-31 (all-courses frame). Only the plan in force is
  published (`plan_years = "current"`; see “Year coverage”): no plan for
  offerings before the academic year of the first harvest (2026/27)
- A page per course in its teaching language, `/emner/{slug}/`, named
  from the title; `resolve_urls_nhh_batch()` finds it with the site
  search by course code (`docs/institutions.md`). A code the search does
  not find gets no URL (outcome `no_url`, no plan). In the pilot sample
  (2025, checked 2026-11-02) these are 81 of 195 offerings, in three
  groups:
  - The translator exams (programme “Translatøreksamen”, AUT1*, AUT2*,
    TRA1*, TRA2*, 0 credits; 11 codes, 20 offerings): the state
    authorisation exam for translators, an exam without teaching, so no
    course plan by nature
  - The NHH Executive courses (department “Avdeling for eksternt rettet
    virksomhet”, 261 of the 837 codes of 2025; 28 codes, 55 offerings):
    continuing education under `/executive/`, where a programme or a
    module open to all has a page for applicants (credits, start, target
    group, fee, contacts), without a course code; a few also hold
    learning outcomes and assessment (the module “Innovasjon og
    omstillingskraft (NHH)”, EXECUTIVE685). Most codes are parts of a
    programme with no page of their own: project papers and assignments
    (AFA211O, MØS412O, SEA416), thesis parts (FINTECH3O, SEAF5D, MØM5),
    company-internal modules (INT102D for DNB, INT161D), and Executive
    MBA modules that the programme page describes in a few lines (MAS403
    “Kilder til konkurransefortrinn”). A match by name would be unsafe
    (MODUL4 “Corporate Finance”, 9 credits, matches the 30-credit
    “Corporate Finance” programme), so they get no plan
  - Courses no longer taught (BAN436 “(expired)”, ELE428 and ENE433
    “(not offered)”; 3 codes, 6 offerings): the master’s course list
    (`/for-studenter/emner2/valgfrie-emner_master/`) names them without
    a link, and the site search has no page for them
- The page shows the current semester’s version (“Høst 2026”); a course
  taught in spring links to the next semester’s version (“Vår 2027”),
  which is not fetched, so its spring offering gets the autumn version
- Read from `main#mainContent`: the code and title, the semester
  heading, the plan’s accordion and the facts aside (credits, language,
  teaching semester, programmes); not the course responsible box (names)
  and the link to the next semester’s version. A page fetched at
  `/emner/` has the Norwegian labels in the facts box, also for a course
  taught in English (the plan’s headings are the course’s own)
- robots.txt disallows only `/paraplyen` (the intranet)

------------------------------------------------------------------------

### NIH (Norwegian School of Sport Sciences)

- Short year range. Most 404s are the other semester of a course
  registered in both (see “Semester registration in DBH”)
- Pages exist by year folder,
  `/studier/emner/{year}/{host|var}/{code}.html`, and the site publishes
  them for few courses before 2025. The year indexes
  (`/studier/emner/{year}/{host|var}/`, read 2026-10-21) list 5 pages
  for 2021 (teacher education, LKI), 22 for 2022 (LKI, and FRI100,
  FRI101, IDR107, IDR111, IDR113, SPM100, TI100) and 163 for 2025 (both
  semesters). Every 404 of the pilot sample’s 2021 and 2022 course-years
  is a course the index of that year does not list (2021: 33 of 33
  course-years, 2022: 31 of 33), so NIH has no page for them; the code
  form is not the cause (`/studier/emner/faf314.html` is 404 too). The
  pilot sample’s 2025 404s are the other semester of a course registered
  in both, and the codes with Norwegian letters: the page of KRØ100 is
  `kro100.html`, which the harvest requested as `krø100.html` until
  2026-10-21 (`add_course_url_nih()` now writes ø as o)
- Codes with a space or hyphen that are not course codes (MAS- ESSAY,
  UTV-ESSAY, essays on the master’s programmes) have no page in any year
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
  its first offering, and gave it to every year of the course. In the
  data built on 2026-10-16, 1,554 of NLA’s 2,074 teacher-ed offerings
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
- Courses taught in English (the Global Journalism master’s, GJ and IKF
  codes) have a plan with English headings, the same JSON fields as the
  Norwegian ones. Since 2026-10-21 they map like the Norwegian ones
  (“Introduction” course content, “Teaching and learning methods” and
  “Practice” teaching methods, “Final assessment” assessment, “Syllabus”
  the reading list, as “Pensum”); before, their 14 plans (none in the
  teacher-education frame) had no teaching methods and lost the
  introduction, the final assessment and the syllabus. “Scope”, “Course
  evaluation”, “Relevance within study programme”, “Credit reductions”,
  “Incompatible courses” and “Other rules” map to nothing, as their
  Norwegian headings do

------------------------------------------------------------------------

### NMBU (Norwegian University of Life Sciences)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”); few course codes. Rows without a plan are 404s for
  discontinued courses, or spring 2025 offerings
- A course whose plan is only in English (M60-IES and NOVA-416 in the
  pilot sample, 4 offerings) has a Norwegian page with “Se engelsk
  emnebeskrivelse” or “See English text” under every heading; since
  2026-10-22 the English page (`/en/course/{code}`) is fetched instead
  (`english_page` in the config). A page fetched before gives no plan
- A course code with a Norwegian letter is in the site’s URL without it
  (MILJØ300 is `MILJO300`); until 2026-10-22 the harvest requested the
  letter and got 404

------------------------------------------------------------------------

### NMH (Norwegian Academy of Music)

- Added 2026-10-31 (all-courses frame). A study handbook per cohort,
  `/studiehandboker/startkull-{year}/`, from 2021: offerings before
  autumn 2021 get no URL. An offering takes the page of its code in the
  handbook of the cohort that starts in its academic year, else in the
  latest earlier handbook that lists the code
  (`resolve_urls_nmh_batch()`, `docs/institutions.md`). A handbook holds
  one cohort’s plans for all its years of study, so a second-year course
  gets the plan published for that year’s new cohort, not necessarily
  the version its students follow
- About half of DBH’s codes are in no handbook and get no URL:
  ensembles, choirs and orchestra projects registered per number of
  projects (BLENS21-BLENS27, KAKOR20-KAKOR27, OSINF60-OSINF72,
  BFO20-BFO24; the handbook has one page, BLENS20 or KAKOR, or none),
  the exchange students’ courses (UTV\*) and courses not taught any more
- Read from `main#main`: the title, the facts box (code, level,
  category, credits, cohort) and the plan; not the breadcrumb and the
  publication stamp. The plan begins with a list of facts (“Emnetype”,
  “Gjennomføres”, “Avsluttende vurdering”, “Forhåndskrav”,
  “Undervisningsspråk”), in no section. Courses taught in English have
  English pages in the Norwegian handbook (“Course description”,
  “Learning objectives”)
- Some plans name the teacher in the text, “Faglærer i emnet: <name>.”
  (electives and ensembles: 10 of the 106 pages of the pilot sample,
  2026-10-31), which the generic anonymizer rules do not catch
  (“Emneansvarlig: <name>” and “Kontaktperson: <name>” they do); a rule
  waits for the anonymization audit
- robots.txt disallows only Craft CMS’s `/cpresources/`, `/vendor/` and
  `/cache/`

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
- The arbeidskrav and compulsory attendance are mostly lines inside the
  assessment text (“Arbeidskrav (AK): …”, “OD: Obligatorisk deltakelse
  …”, “To arbeidskrav.”, “Coursework requirements:”); they move to
  coursework requirements (`section_inline_coursework`; abbreviations,
  number words and English since 2026-10-21). “Arbeidskrav og
  hjemmeeksamen (3 dager)” stays in assessment and is copied. Gate lines
  under Læringsaktiviteter stay in teaching methods (MET2000 spring
  2022’s “Arbeidskrav:” block, SYK5026 autumn 2025’s OD and AK
  paragraphs)

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
- The arbeidskrav are listed under “Mer om vurdering” (“OBLIGATORISKE
  ARBEIDSKRAV”, “Obligatoriske aktiviteter:”), often with only
  “Obligatoriske arbeidskrav iht. emnebeskrivelsen” under the coursework
  heading; since 2026-10-20 the block moves from the assessment section
  to coursework requirements (`section_inline_coursework`). Arbeidskrav
  and “Vurderingsform:” lines under Læringsformer stay in teaching
  methods (52 and 19 sections)
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

### PHS (Norwegian Police University College)

- Added 2026-11-02 (all-courses frame). Course pages from autumn 2022:
  DBH’s offerings of 2012-2021 (some 5 600) have no plan on the site.
  The year folders’ listings hold every page (offerings they do not list
  answer 404, checked 2026-11-02), and few of DBH’s offerings have one:
  in the listings of 2026-11-02, 7 of the 463 offerings of 2022, 29 of
  460 in 2023, 110 of 434 in 2024 and 112 of 392 in 2025. Most
  continuing-education courses (Etter- og videreutdanning) and every
  Master i politivitenskap course of 2024-2025 have no course page; many
  continuing-education programmes publish a study plan (a PDF, or an FS
  programme page under
  `/studier/master-etter-og-videreutdanninger/{programme}/studieplaner/`)
  instead, which the harvest does not read
- A page is `/studier/emner/{year}/{var|host}/{code}.html` (code in
  lower case, Norwegian letters as ASCII: AVHØRUTD is `avhorutd.html`),
  or in the English tree
  `/english/studies/courses/{year}/{spring|autumn}/` (the NCFI861
  courses of the Nordic Computer Forensic Investigators programme);
  `resolve_urls_phs_batch()` reads the listings. The English pages hold
  the title (“NCFI861-1 null (Spring 2025)”), the facts and the
  programme list, and no plan: `fetch_html_cols_single_phs()` counts a
  page without plan text as not found (outcome `not_found`, since
  2026-11-02; the pilot harvest of 2026-11-02 stored the 7 English pages
  of its sample as fetched, before that check) A course over two
  semesters has one page, under its autumn (“PATRULJE01 Politipatruljen
  B1 (Høst 2025–Vår 2026)”); a spring offering whose autumn page is for
  the autumn alone gets no plan (`page_end_term`)
- robots.txt (read 2026-10-16) disallows only `/vrtx`, which the harvest
  does not request
- Read from `#vrtx-content`: the title, the facts list (credits,
  department, semester, duration), the programme list and the plan; not
  the reading-list link (Leganto), the table of contents’ heading and
  the FS fetch stamp. The title, facts and programme list are in no
  section

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
  `steinerhoyskolen.no`, split by course heading; the split text is the
  raw data. Runs until 2026-10 do not store the PDFs; since 2026-11-06 a
  run keeps each PDF’s bytes on the first row its text joined to
  (`pdf`), and every harvest downloads the PDFs anew (until then a PDF
  kept in `temp/steiner/` from an earlier harvest was split again, so a
  harvest in a later academic year would have read last year’s PDFs).
  The runs until 2026-11 have no `url`: the PDF’s wrapper page is in
  `url.y`, which `read_harvest()` reads as `url` (since 2026-11-06;
  until then the pipeline left it out and their offerings had no URL);
  later runs give it in `url` (and the fetch ledger). Their fetch ledger
  has no URL, so an offering of theirs takes its own outcome (`ok`)
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
  (`.steiner_remap_pel_emne()`), and since 2026-10-19 (run
  `2026-10-19_121636`) the bachelor’s PEL1.4/PEL1.5 too: before, the
  remap replaced their key, so the run `2026-10-19` gave those 4
  offerings no plan (56 offerings with a plan, then 60). The M-PEL2
  match is to be confirmed against the programme description

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
  `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`;
  the semester folders are `host` and `var`
- DBH’s variant suffixes `-G`, `-G2` and `-D` (MA-015-G, TFL011-G2,
  PRA090-D) are not in the site’s code: ma-015, tfl011, pra090 answer,
  the codes with the suffix never do (checked 2026-10-21). Until then
  the builder kept the suffix and the 41 such offerings in the raw
  store, and 37 of the pilot’s, were 404. The builder now drops the
  suffix. A few such courses have no page under either form (IKT623,
  IKT454, MA-015 and others in the 2022 and 2025 pilot years, 404 in
  both semesters): the site does not publish them, so they stay without
  a plan

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
- All courses (pilot sample of 100 course-years, 2010/2019/2022/2025):
  on 2026-10-17, 74 course-years answered 404 and 3 failed. Fetched
  again on 2026-10-21, 29 gave a page (HIS101, STAT260, PED322 among
  them): UiB’s site sometimes answers 404 for a page it has. The other
  48 have no page on UiB’s site, in any semester and without
  `start_semester` (12 of them checked so): 25 courses DBH last lists
  before 2025 or as discontinued (UiB keeps no page for a course it no
  longer offers, even for the semesters it ran), 14 PhD courses, 4
  master’s theses and other milestones (JUS399, MABARN351, MEDFLMIDT,
  GEO310), 2 UNIS courses (AB206, AG335, on unis.no), and 3 design
  semester blocks whose DBH code has a space (`BAR 3`, `UTV VISK 5H`;
  also not as `bar3` or `bar-3`). Every course-year of the sample with a
  page is of a course DBH still lists in 2025

------------------------------------------------------------------------

### UiO (University of Oslo)

- Only the plan in force is published (`plan_years = "current"`; see
  “Year coverage”). Semester URLs (`/h24/`, `/v25/`) hold logistics
  (teachers, timetable, exam dates), not the plan
- The URL holds a faculty and department folder
  (`https://www.uio.no/studier/emner/{faculty}/{department}/{CODE}/`),
  read from UiO’s sitemap, which lists only the courses taught now; a
  course not listed gets the folder of the listed courses with its code
  prefix, else of its DBH department (`docs/institutions.md`). On
  2026-10-16, 3,689 of the 5,163 course codes active in DBH 2025 were
  listed. Of the 1,474 not listed (822 PhD level, 107 UNIS courses), a
  sample of 40 guessed URLs gave 11 pages; the rest were 404s: UNIS
  courses (on unis.no), PhD reading-list codes (`SP`) and odontology
  specialist codes have no page at UiO
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
  picker, plus the document the page itself shows (the latest; the
  picker leaves it out, so before 2026-10-21 the latest document of
  every course was missed, e.g. autumn 2025 for courses whose plan for
  spring 2026 was not yet published)
- Course-years without a document (pilot sample of all courses,
  2026-10-21, checked on the course pages: 33 of 100 course-years, after
  the shown document gave 7 more their URL). UiT’s documents follow the
  semesters a course is taught (LER-3101: every spring 2021-2026), while
  DBH also lists courses in years without a document:
  - The page exists but has no document for the DBH year (21): the
    documents stop years before (the PhD special-curriculum courses
    KJE-8810, MED-8815, SVF-8044: last document spring 2012, DBH 2025;
    PSY-3027 last spring 2021; HIF-3113 last spring 2023), start after
    it (2008: the earliest documents are autumn 2008, so spring 2008 has
    none; KJE-8897 and MED-3881 start in 2009), or skip it (SAM-1013:
    autumn 2024, then autumn 2026)
  - No course page at all (12): the page says “Error rendering
    component” for the code, also with DBH’s version suffix and in lower
    case. These are old doctoral programme codes of 2008 (FARMDG, FILDG,
    SOSANTDRSP2, SOSIODRSP1), codes such as XXX-1000 and BB351, and,
    likely, courses discontinued before the documents begin (REL-1012,
    SAM-0100 in 2008)
  - AG- and AGF- codes (DBH unit “UiT (uspesifisert underenhet)”,
    e.g. AG-323 “Sequence stratigraphy”) are by their codes and titles
    courses of UNIS on Svalbard, which publishes its plans on its own
    site; UiT’s pages have at most a 2012 document or none. They are not
    harvested
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
  the displayed year (`docs/institutions.md`)
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
- A course taught in English has an English page (“Applies to: Course
  start autumn 2022”, banner “Note: The course plan for this semester is
  not available yet.”). Until 2026-10-21 the resolver read only
  “Undervisningsstart”, so such a course got no URL in any semester; it
  now reads both. In the pilot sample (all courses, 100 course-years)
  this gave 12 course-years a page (46 → 58), all of them English
  (MAR4020 autumn 2022, MET9000 spring 2025, FEMLI20 autumn 2018, …)
- The pilot’s other 42 course-years without a page (2026-10-21, version
  1 rendered in Chrome, versions 2-3 by the resolver): 21 show another
  semester’s plan under the banner (e.g. EK5420 autumn 2022 shows autumn
  2021, 4013 autumn 2025 shows spring 2024), 10 show a cohort plan from
  before USN (“Planen gjelder for: Kull 2017 HØST”, “Class of 2014
  Autumn”: 3489, 9210, RET2000D and four more in 2018, SAM 501, VVL1,
  PHDMOD500 in 2019; no plan of USN’s own), and 11 show an empty page
  with only the banner (no plan in any version: e.g. ALDSU122 and 9228
  in 2025, STREKON1 in 2022, HRM100 and SAMH in 2018). The old numeric
  codes (3489, 4013, 4505, 9210, 9228) are USN’s codes too; they have no
  plan of their own for the year
- English pages are read like the Norwegian ones (since 2026-10-21): the
  facts box ends at “Table of contents” as at “Innholdsfortegnelse”, and
  the English headings map like their Norwegian relatives
  (“Participation/Compulsory work”, “Compulsory Activity” coursework
  requirements, “Forms of assessment” assessment, “Supervised
  professional training” teaching methods, as “Praksis”; “Course is part
  of these studies”, “Approved course plan” and “Change description” are
  dropped, as their Norwegian headings are). “Miscellaneous” maps to
  nothing, as “Annet” does. Before, the 12 English pilot plans had no
  plan text (so their plan id held the facts box), the programme list
  went into their reading list (all 12), the approval stamp and change
  log into their assessment (7), and only 1 of them had a coursework
  section (now 11; assessment 11 → 12)

------------------------------------------------------------------------

### VID (VID Specialized University)

- Added 2026-11-01 (all-courses frame). The course page holds the plans
  of every year and semester the course is taught in, from spring 2024
  (many spring 2024 entries have no text) to the next academic year:
  offerings before 2024 have no plan (DBH has VID from 2016)
- A page per course, `/studier/emner/{slug}-{code}` (slug from the
  title), found in the sitemap; a few codes have a second page with a
  numeric id after the code, for another programme or semester, and the
  offering takes the one with a plan for its semester. DBH codes without
  a course page (pilot, 2026-11-01: BASP1010, BATEOL1020, DIA5230,
  MAPS5010, MVBL1010, SOS-203 UTV, VULEVE8010; the site has, e.g.,
  BASP1010-O and MVBL1021) have no plan
- The plan is in the page’s JSON, not its HTML; `vid_plan_html()` reads
  the offering’s year and semester from it. A page whose JSON has no
  plan with text (MATRS5120) is outcome `not_found`. A page with plans
  for other semesters only gives the offering no plan: in the pilot, 25
  of the 70 course-years with a page had no plan for their year (most a
  course taught in other years, such as BAERG1050: spring 2025-2027
  only, DBH 2024)
- Read as HTML built from the JSON: the plan’s title, a facts list
  (code, credits, language, campus, duration, the programmes the course
  belongs to; in no section) and an h2 per paragraph and accordion. The
  reading list is a link (Leganto) and is not in the plan
- robots.txt (read 2026-10-16) allows everything for all user agents; it
  blocks AI crawlers by name
