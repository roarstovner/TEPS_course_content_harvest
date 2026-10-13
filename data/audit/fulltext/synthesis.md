# Fulltext audit — synthesis (all institutions)

Run 2026-10-13 on the build of that day (after the plan-id noise fixes).
18 institutions (Samas has no text by design), one review agent each: Sonnet,
except NTNU (Opus; its packet is 166 KB). 56 findings, all pass mechanical
verification. Report: `findings_report.md`. Pilot (uio, nih, steiner,
2026-09-30): the earlier version of this file in git history.

## Summary

- Where a page holds the plan, extraction is faithful at every institution:
  the whole plan, the facts box, readable structure, and no site chrome. The
  uncaptured page text is what the configuration leaves out on purpose (year
  pickers, staff boxes, exam dates and rooms, banners).
- **Spring offerings get the next academic year's plan at NTNU, HVL and
  OsloMet** (verified on the whole data, not only the packets). Their URL
  holds the start year of an academic year (`/{CODE}/2025` is 2025/26), and
  the URL builders put DBH's `Årstall` in it for every semester. So spring
  2025 (academic year 2024/25) reads the 2025/26 plan. NTNU: no spring page
  shows its own term as "Undervisningsstart"; 534 show "Vår Y+1". HVL: all
  1,556 spring pages say "Emneplan for studieåret Y/Y+1". OsloMet: all 796
  spring pages show study year Y/Y+1. NLA, Nord and the current-only sites
  already give an offering its own academic year's plan.
- **1,681 NTNU offerings (150 codes, the MGLU primary teacher courses with a
  "-A" code, 2017-2025) have no page:** the URL keeps the "-A" suffix, which
  the site does not know ("No course ..."). Without the suffix the page
  exists (agent's check of `MGLU1101/2022`). That is 29 % of NTNU offerings.
- **Nord shows the current plan for years it has no plan for:** 2,041 pages
  say "Gjeldende emnebeskrivelse (sist oppdatert YYYY/YY)" instead of
  "Emnebeskrivelse for YYYY/YY". Where that year is not after the offering's
  academic year, it is the version in force then. In 814 pages it is 2026/27
  for an earlier offering (all of 2015-2018, some of 2019-2024): a later
  version, which the spec says is not the offering's plan (as INN's).
- The rest is source limits already documented (NLA's JSON holds one academic
  year, UiS course versions that are no longer online, reading lists only as
  links) and cosmetic noise.

## Cross-institution patterns

1. **URL year is an academic year, not a calendar year** (NTNU, HVL,
   OsloMet). One fix: the URL year is the offering's academic year
   (`Årstall - 1` for spring and summer). Most corrected URLs were already
   fetched for another offering of the same course (autumn Y-1 or spring Y-1
   fetched `/Y-1`), so `read_harvest()` can give each offering the page of
   its academic year; the rest need a retry harvest.
2. **A fallback page stands in for the requested one** (Nord "Gjeldende …
   2026/27", INN "ikke publisert enda", UiS "no longer available", UiB "Inga
   emnebeskriving tilgjengeleg"). INN, UiS and UiB already give no plan (INN
   by its notice; UiS and UiB pages are empty or shells). Nord's needs the
   same rule, with the offering's year.
3. **Plan text on another page** (HVL English-only plans, reading lists
   behind links at HiOF, Volda, MF, NIH, UiT). Only HVL's is plan text the
   harvest could reach: 92 pages say "Emnebeskrivelsen finnes kun på engelsk"
   and link `/en/studies-at-hvl/study-programmes/courses/{year}/{CODE}`.
4. **Site boilerplate in `extracted_text` that the anonymizer already
   removes** (UiO exam links, MF library notice, USN Leganto labels, OsloMet
   ";" artifacts, INN's search page): not in `course_plan`; no action.

## Prioritised fix list

1. **Academic-year URLs (NTNU, HVL, OsloMet)** — `add_course_url_ntnu()`,
   `add_course_url_hvl()`, `add_course_url_oslomet()` in
   `R/add_course_url.R`; `read_harvest()` in `R/pipeline.R` matches pages to
   offerings by course and academic year (the year in the fetched URL). About
   4,000 spring and summer offerings change plan; spring Y and autumn Y-1
   share one page. Examples: `ntnu_MGLU4213-1_2023_spring_1`,
   `hvl_MGBKØ401-1_2025_spring_1`, `oslomet_M1GSF2110-1_2022_spring_2`.
   Needs a `methods.qmd` entry.
2. **NTNU "-A" codes** — `add_course_url_ntnu()` drops the suffix; then
   `harvest_all(institutions = "ntnu", retry = TRUE)` (robots.txt: 10 s per
   request, about 1,200 pages). Example: `ntnu_MGLU4106-A_2022_spring_1`.
3. **Nord later versions** — an offering whose page says "Gjeldende
   emnebeskrivelse (sist oppdatert YYYY/YY)" with YYYY after its academic
   year gets no plan (814 pages; `nord_SP154L-1_2018_autumn_3`). Needs a
   `methods.qmd` entry like INN's.
4. **HVL English-only plans** — fetch the English URL for pages with the
   "finnes kun på engelsk" notice (92 pages, about 86 offerings without a
   plan; `hvl_MGBPE201E-1_2024_spring_1`).
5. **UiB "Feil i visning av emnebeskrivelse"** — 30 pages with the site's
   error banner and no plan; a retry harvest may recover them
   (`uib_TYS307L-0_2017_spring_2`).
6. Low: NMBU facts labels glued to values ("Studiepoeng:10") and the KI link
   label "Her finner du KI-kategoriene beskrevet."; UiT literal tags and
   entities in two old pages (`uit_LRU-1270-1_2011_spring_1`); OsloMet
   M5GMT3100 2020 wall of text; Steiner page-number lines (known, 2026-09).

## Changes since the previous run (pilot: uio, nih, steiner)

- Fixed: UiO's exam-link block no longer reaches `course_plan`
  (`.anon_uio()`); it is still in `extracted_text`, which is not published.
- Persisting: NIH's 404s (data notes: semester pages), Steiner's
  PEL2 → PEL1.4/1.5 mapping (deliberate) and page numbers.
- New: everything above for the 15 institutions not piloted.

## Rejected findings

- **NLA "86 % empty, year lookup may fail"**: each page's JSON holds one
  academic year (880 pages 2025-2026, 656 2024-2025, …); the requested year is
  present for exactly the 249 offerings that have text. Documented in the data
  notes.
- **INN wrong page and wrong year**: the course-search page and the
  "ikke publisert enda" later version already give no plan (`.anon_inn()`).
- **UiS empty texts**: the pages say "This course version is no longer
  available as a current course description"; empty is right.
- **MF, NMBU wrong year**: current-only sites; the offering gets the harvest
  of its academic year by design.
- **UiA 2023 autumn and UiB "Inga emnebeskriving" stubs, Nord's "Dette faget
  har ikke hatt undervisning" pages, HVL's English stubs**: the shell rule
  already gives them no plan (UiB 72 pages, 0 plans; HVL 6 of 92 keep a plan).
- **HiOF and NIH 404s**: semester-specific pages; the other semester has no
  page (data notes, "Semester registration in DBH").
- **Steiner M-PEL2.1 wrong page**: deliberate mapping
  (`.steiner_remap_pel_emne()`).
- **Reading lists behind links, UiB exam logistics**: source limits or out of
  scope by the spec.
