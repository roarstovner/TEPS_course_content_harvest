# Fulltext audit — synthesis (pilot: 7 institutions)

Run 2026-10-17 with `--pilot` on the build of commit 7510144 (the 16 pilot
harvests): ntnu, oslomet, inn, nord, hvl, hivolda, nla. Each packet holds
only offerings from the institution's pilot sample (`data/pilot/{inst}.csv`,
all courses, not only teacher education): 28 pages plus up to 5 failed
fetches. One Sonnet review agent per institution; packets 77–137 KB; every
agent read all its courses. 29 findings for these 7, all pass mechanical
verification (0 ✗). The other 11 institutions' findings in
`findings_report.md` are from the full run of 2026-10-13 (its synthesis is
in git history, 07bec1c).

## Summary

- Where a page holds the plan, extraction is faithful at all seven: whole
  plan, facts box, readable structure, right year. The pages' own faults
  (a `¿` in the source HTML at NLA, OsloMet and HVL, words run together in
  NTNU's markup, a lead paragraph Nord's page shows twice) are not
  extraction faults.
- **NLA reads one page per course code for every year.**
  `harvest_json_extract()` fetches the URL of a code's first offering
  (`distinct(course_code)`) and joins that page to all its offerings, but a
  page's JSON holds only its own academic year (`/VUNA1AG17/2017-2018` holds
  `2017-2018` only; on the site `/4POPM101/2025-2026` holds `2025-2026`,
  while the raw store gives the 2025 offerings the `2019-2020` page). In the
  sample, 83 of 192 offerings read another year's page, and each of the 20
  codes without a page was requested for only one of the academic years
  it is offered in. This explains NLA's `plan` (.67) and part of `fetched` (.79), and
  the low plan coverage of NLA's 2018–2023 teacher-education build.
- **English-only courses at HVL and Nord are saved as Norwegian stubs.**
  The Norwegian page holds a title, credits, programmes and a pointer to
  the English page ("Dette emnet er kun beskrevet på engelsk. Klikk på
  "English" …", "Se engelske nettsider for mer informasjon"; Nord: "Emnet
  foregår på engelsk. Se engelske nettsider ved å velge engelsk språk.").
  `fetch_html_cols_single_hvl()` follows only the wording "Emnebeskrivelsen
  finnes kun på engelsk"; Nord has no such rule. The pointer becomes a
  `course_content` section, so the shell rule keeps the stub as a plan
  (HVL ADA512, BI4-301, MMO5010, MAS538 spring 2025; Nord BIO5023, BIO5031,
  BIO5036 2025). The English pages hold the full plan (checked on the site
  2026-10-17: HVL ADA512 2025, Nord BIO5031 2025).
- The other high findings are pages the pipeline already gives no plan:
  facts-box-only pages (shell rule: OsloMet, Nord), INN's "not published
  yet" pages and catalogue-search pages, OsloMet's error page (anonymizer
  returns NA) and Nord's current-plan fallback (`page_year`).

## Cross-institution patterns

1. **A pointer to the English page instead of the plan** (HVL, Nord): one
   fix per site in the fetch function — on the notice, fetch the English
   URL (HVL: `/en/studies-at-hvl/study-programmes/courses/`, as the
   existing rule does; Nord: the page's `hreflang="en"` link,
   `/en/studies/courses/{code}?semester=…&year=…`). Both need new fetches,
   so the full harvest would save a stub for every English-only course
   without it.
2. **Reading lists only as a link** (hivolda, HVL, NLA from 2022, OsloMet):
   the list is on another page (Leganto or the site's own Pensum page). By
   the spec the pointer is not a section; a source limit, as at MF.
3. **Link labels and banners left in `course_plan`** (OsloMet "Pensum" /
   "Timeplan", Nord "… finner du her.", INN "Statusmelding"): page chrome
   by the spec; each one `exclude` or `post_fn` rule.

## Prioritised fix list

Must be fixed before the full harvest (new fetches):

1. **NLA, `harvest_json_extract()` (`R/harvest_strategies.R`)**: fetch one
   page per distinct URL (course code × academic year), not per course
   code, and join each offering to the page of its own year. Examples:
   `nla_VUNA1AG17-1_2022_autumn_3`, `nla_4POPM101-1_2025_autumn_1`,
   `nla_IKF305-1_2022_spring_1`. Expected: `plan` and `fetched` up by the
   12 course-years that read another year's page, and the 404 codes
   requested for their other year.
2. **HVL, `fetch_html_cols_single_hvl()` (`R/fetch_html_cols.R`)**: follow
   the English page on every wording of the notice ("kun beskrevet på
   engelsk", "Dette emner er kun beskrevet", "berre skildra på engelsk",
   "Se engelske nettsider"). Examples: `hvl_ADA512-1_2025_autumn_1`,
   `hvl_MMO5010-1_2025_autumn_1`. 8 short plans with the notice in the
   current build.
3. **Nord, fetch**: the same for "Emnet foregår på engelsk. Se engelske
   nettsider", via the page's English link. Examples:
   `nord_BIO5031-1_2025_autumn_2`, `nord_BIO5023-1_2025_autumn_1`.

Rebuild only (low):

4. **OsloMet `exclude`**: the "Pensum" / "Timeplan" link labels with their
   semester (1,575 of 1,841 plans in the build). `oslomet_VESK6300-1_2025_spring_1`.
5. **Nord `exclude`**: the site-wide link labels "Praktisk informasjon og
   retningslinjer for eksamen.", "Generell informasjon om hjelpemidler til
   eksamen finner du her.", "Informasjon om ChatGPT/kunstig intelligens mm.
   finner du her." (101 plans). `nord_IDR1037-1_2025_spring_1`.
6. **INN `exclude` or `.anon_inn()`**: the status banner ("Statusmelding",
   "Kun tilgjengelig på engelsk"; 26 plans). `inn_SYD2009-1_2023_autumn_2`.

## Rejected findings

- oslomet `whole_text/empty_or_failed` (high): the facts-box-only pages
  already get no plan (shell rule; FK2200, ARKROM6100, IFHH3400 have no
  `plan_content_id`). Its "spring 2025 → /2024/HØST" is right: `/2024/HØST`
  is study year 2024/25.
- oslomet `whole_text/wrong_page` (high): FADUFA's error page gets no plan
  (`.anon_oslomet()` returns NA).
- inn `whole_text/wrong_page` and `whole_text/wrong_year` (high): both get
  no plan (`.anon_inn()` returns NA). The catalogue redirects codes it does
  not know to its search (`title_search=HELS3001`, `1AKT01`; checked on the
  site): a site limit.
- nord `whole_text/empty_or_failed` (high): facts-box-only pages get no plan
  (shell rule), except the English stubs, which are fix 3.
- nord `whole_text/wrong_year` (medium): `page_year` gives RET1002, RE214E
  and MFL240 2019 no plan.
- nord `course_content/duplicate_content`: the page itself holds the
  paragraph twice (short introduction and description field).
- nord `page_chrome/junk_included`, the overlap sentence: the spec counts
  the credit-overlap stock text as plan text (kept, left out of the plan
  id). The link labels in the same finding are fix 5.
- nord ØKO110FU 404: one discontinued 2019 offering, the only non-ASCII code
  in the build.
- ntnu `course_content/other` (medium): "Det tilbys ikke lenger
  undervisning" is out by the spec ("Emnet tilbys ikke 2025/2026"); SMF1081
  gets no plan, RAD6020 and SYT6020 keep full plans.
- ntnu `assessment/missing_content`: exam dates, times and rooms are out by
  the spec.
- ntnu `whole_text/junk_included`: "course-details-portlet" is in no
  `course_plan` (0 of 5,538).
- ntnu `learning_outcomes/formatting`, `metadata/formatting`: the page's own
  markup ("KunnskaperStudenten har-" is in the HTML); the overlap table is
  not plan text.
- ntnu `PHMT/AVHANDL`: a PhD thesis code; both URL forms 404. NTNU's 543
  DBH codes with a slash are pre-2007 codes and thesis registrations.
- hvl `course_content/duplicate_content`, `whole_text/formatting`; nla
  `whole_text/formatting`, `course_content/formatting`; oslomet
  `course_content/formatting`: the `¿` and repeated headings are in the
  source HTML (139 NLA, 174 OsloMet, 23 HVL pages).
- Reading-list findings (hivolda, hvl, nla, oslomet): a source limit
  (pattern 2), no fix.

## Verdict (`--pilot`, fulltext: `fetched`, `plan`)

| inst | fetched | plan | verdict |
|---|---|---|---|
| ntnu | .79 (n 100) ⚑ | .79 ⚑ | ready with known limits |
| oslomet | 1.00 | .93 | ready |
| inn | 1.00 | .59 ⚑ | ready with known limits |
| nord | .99 | .82 | not ready |
| hvl | .79 ⚑ | .79 ⚑ | not ready |
| hivolda | .77 ⚑ | .77 ⚑ | ready with known limits |
| nla | .79 ⚑ | .67 ⚑ | not ready |

- **ntnu — ready with known limits.** `fetched`/`plan` .79 < .80 (21
  course-years): 15 spring/summer 2007 offerings read 2006/07, which the
  site does not have; 5 shells of courses not taught that year; 1 PhD
  thesis code without a page. No confirmed high finding.
- **oslomet — ready.** No flag; no confirmed high finding.
- **inn — ready with known limits.** `plan` .59 < .80 (41 course-years):
  25 show only a later version ("Emnebeskrivelsen for valgt semester er
  ikke publisert enda"), 16 are codes the catalogue does not know (search
  page). Both already get no plan.
- **nord — not ready.** English-only courses are saved as Norwegian stubs
  and kept as plans (fix 3, a fetch fix).
- **hvl — not ready.** The English-page rule misses most wordings of the
  notice (fix 2, a fetch fix). `fetched`/`plan` .79: 20 soft 404s ("Vi
  kunne ikkje finne emnet") for PhD, test, practicum and continuing
  education codes the site has no page for — a known limit once fix 2 is in.
- **hivolda — ready with known limits.** `fetched`/`plan` .77 < .80 (23
  course-years without a URL): 12 in 2016 (the course pages list no 2016
  version), 5 codes without a page, 6 with no version for that semester.
- **nla — not ready.** One page per course code for all years (fix 1, a
  fetch fix); it causes most of `plan` .67 and part of `fetched` .79.
