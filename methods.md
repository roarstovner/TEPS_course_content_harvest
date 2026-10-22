# Methods notes: collecting and preparing the course plans


The choices behind the published data that a methods section has to
report, by topic. Each entry gives the choice, the reason, alternatives
that were rejected, the consequence in numbers (computed from the
current data when this file is rendered) and since when it applies
(date). What the published text holds and what stays out, as rules, is
in `course_plan_spec.md`; how to run the pipeline is in `README.md`;
per-institution data quality in `data/data_notes.md`. When a decision
changes what the data mean, add or update its entry here; each release
tag freezes this file with the data.

Current data, the teacher-ed frame (below): 41 025 offerings (DBH course
× year × semester) at 19 institutions, 26 505 of them with a plan, and
14 740 unique plans from 18 institutions (none from Samas, below).

## Population and sampling frame

**The frame is every course in DBH; teacher education is a filter of
it.** The input is a dated snapshot of DBH’s bulk files, written by
`data-raw/courses.R`: `data/input/dbh_courses_{date}.parquet` is table
208 (every course of every institution, year and semester, all levels
and statuses) and `dbh_programmes_{date}.parquet` table 347
(programmes). `dbh_courses()` reads the latest;
`dbh_courses("teacher_ed")` is the teacher-ed frame below. *Why:* the
plans of all higher education are to be collected, and teacher education
(or the AI question) is one use of them, not the frame (decided
2026-10-16). One snapshot for both frames gives a teacher-ed offering
the same `course_id` in both. *Rejected:* a separate teacher-ed query
(the earlier `data/input/courses.RDS`, table 208 queried by programme
code): two queries on different days can disagree on an offering’s
status, which is part of its id; querying each institution through the
API, where the bulk files give the whole table at once. *Consequence:*
the snapshot of 2026-10-16 has 2 164 079 rows (168 institution codes,
1986-2025), 1 257 158 of them at the 19 institutions with a config. Its
teacher-ed filter is the list of 2026-10-06 unchanged: the same 45 280
offerings, ids and values (DBH’s bulk file ends two course names with a
newline, which is trimmed). Nothing harvested or published changes: a
raw file holds the DBH columns of its harvest. *Since:* 2026-10-16.

**Teacher education: courses in the five-year teacher education
programmes, as registered in DBH.** Programmes are DBH table 347 rows
with study type INTMASTER, IMALU1-7, IMALU5-10 or LUPE (in any year);
courses are the table 208 rows of those programmes, matched on
institution and programme code (a programme code is unique only within
an institution). Table 208 has one row per course and semester, under
one programme (two exceptions among the 2 164 079 rows of 2026-10-16,
both in 2007; `dbh_courses()` keeps the first), so the frame holds the
courses DBH registers under a teacher-ed programme, not courses of other
programmes that its students also take. *Why:* DBH is the official
register and gives course metadata (credits, level, language, subject)
that need not be coded from the plans. *Rejected:* the four-year GLU
programmes (not in TEPS). *Since:* 2026-09-30 (institution + code match,
c04fbc3); a filter of the all-course snapshot since 2026-10-16.

**One build of every harvested course; teacher education is a filter of
its outputs.** The pipeline builds every offering in the raw store and
writes the data files of two frames: all courses (`data/processed/`) and
teacher education (`data/processed/teacher_ed/`), the offerings whose
institution, code, year and semester are in the teacher-ed frame of the
latest DBH snapshot. A page’s text, plan id and sections are the same in
both. Plans are deduplicated within each frame, so a teacher-ed plan has
the years (`year_from`, `year_to`), text and sections of its own
earliest teacher-ed offering. *Why:* a course can be registered under a
teacher-ed programme in some years and under another in others, and a
teacher-ed release must stay as it is when courses of other programmes
are harvested; one build keeps every fix in one place. *Rejected:* a
second build for all courses on the same raw store (every step and fix
twice); keeping the plans of all courses that have a teacher-ed offering
(their text, years and sections would come from offerings outside the
frame and change with every harvest of other courses). *Consequence:* on
2026-10-16 the raw store held only teacher-ed offerings (41 025), so
both frames held the same data, and the teacher-ed files were byte for
byte those of release data-2026-10-16. The releases, the browsers and
these notes use the teacher-ed frame; the data notes all courses.
*Since:* 2026-10-16.

**An offering keeps its first `course_id`.** The id is
`{institution}_{code}_{year}_{semester}_{status}`, and a later DBH
snapshot can change the status. A harvest matches offerings on the rest
of the id (institution, code, year, semester: one row in table 208), and
an offering already in a raw file keeps the id it was first harvested
under (`first_course_id()`); a frame filter of the raw store matches the
same way. *Why:* with the whole id, a changed status would make the same
course and semester a second offering, fetched again. *Rejected:*
dropping the status from the id, which would change every published id.
*Consequence:* none so far: the 2026-10-16 snapshot gives all 45 280
teacher-ed offerings of 2026-10-06 their old id, and holds every one of
the 41 025 ids in the raw store. *Since:* 2026-10-16.

**An institution can have several DBH codes.** INN reports 2025 under
its university code 1177 (Universitetet i Innlandet) instead of 0264;
both map to INN, which adds 493 INN offerings for 2025. Codes of
institutions that merged before 2018 (Høgskolen i Hedmark, HiOA, UiN,
HBV/HSN, UMB, HiA) are not mapped: their sites do not serve plans for
those years. *Since:* 2026-10-06.

**DBH registers most courses in both semesters**, also when a course is
taught and has a plan in one only. Coverage is therefore counted in
course-years (course × year with a plan in at least one semester), not
offerings (`data/data_notes.md`, “Semester registration in DBH”).

**Years:** DBH had no 2026 rows on 2026-10-06 or in the snapshot of
2026-10-16, so the frame ends with 2025.

**Samas is excluded:** its website’s course codes do not match DBH, and
the teacher education courses are described only in programme PDFs
(decided 2026-02).

**Variable names: DBH’s columns have English snake_case names, and every
variable is documented in `data/variables.csv`.** `Årstall` is `year`,
`Emnekode` `course_code`, `Underv.språk` and `Navn` `language_code` and
`language_name`, `Oppgave (ny fra h2012)` `thesis`, and so on; the
values stay DBH’s (Norwegian names, DBH’s codes). `data/variables.csv`
gives each variable of the published files its type, source (the column
of DBH table 208, or how the pipeline gives it), meaning and codes, and
its name in the releases up to data-2026-10-16; it is released with the
data. *Why:* DBH’s names mix Norwegian words, abbreviations and
punctuation (`NUS-kode`, `Underv.språk`), and two do not say what they
hold (`Navn` is the language of instruction, `Oppgave (ny fra h2012)`
whether the course is a thesis); the release of all higher education is
to be documented once, before users outside the project read it.
*Rejected:* keeping DBH’s names with a codebook (the names would still
need backticks in code and explaining in every analysis); translating
the values too (they are DBH’s codes and names, which users look up in
DBH). *Consequence:* the release schema changes once: the same files,
rows and values under the new names, with `year_from` and `year_to`
integer (double before). DBH’s snapshots and the raw files of the
harvests until 2026-10-16 keep DBH’s names, which the pipeline renames
when it reads them; data of release data-2026-10-16 or earlier are
renamed by the pairs in `data/variables.csv` (README “Variables”).
*Since:* 2026-10-16.

## Harvest

**Sources:** each institution’s public course pages, except UiS before
its current web plans (PDF archive), Steiner (five subject PDFs) and NLA
(the JSON embedded in the course page). Harvests: 2026-09-30 to
2026-10-02 (all institutions), 2026-10-06 (INN 2025, NLA’s earlier
years), and 2026-04-04 (an earlier harvest of all institutions, used for
the sites below).

**Each site’s robots.txt sets the pace of the harvest:** the harvest
names itself and the project (“TEPS research project”, with the
project’s web page and e-mail; HiØ answers that user agent with HTTP
403, so its pages are fetched with a browser’s) and waits 3-5 seconds
between requests to a site, or longer where its robots.txt asks all user
agents for a `Crawl-delay`: NTNU 10 seconds, USN 5 (since 2026-10-16; 2
seconds before). UiB is harvested at 10 seconds since 2026-09-30,
although its robots.txt asks this only of one crawler (Scrapy) on
2026-10-16. A URL whose path robots.txt disallows for all user agents is
not requested, unless the institution has given the project permission
(`robots_permission` in the config). Since 2026-10-17 the harvest
enforces this before every request (`robots_allows()`, README
“robots.txt”): such a URL gets the outcome `robots` in the fetch ledger
and is not retried, and a host whose robots.txt has not been read stops
the harvest. *Why:* the harvest asks a site for no more than it allows
crawlers. *Rejected:* one pace for every site, which is faster than NTNU
and USN ask; fetching the disallowed document URLs anyway, since
robots.txt is aimed at crawlers and the harvest asks for known URLs one
at a time (2026-10-17: the project asks for permission instead).
*Consequence:* the robots.txt of every host, read on 2026-10-16
(`data/robots_txt.csv`), disallows two sources that the harvests up to
2026-10 used: UiS’s PDF archive (`www6.uis.no` allows only `/ansatt/`;
2,201 of UiS’s 2,719 offerings with a plan text, 2009-2025) and
Steiner’s five subject PDFs (`/dokumenter/`; all 15 Steiner offerings
with a plan). Later harvests do not request UiS’s archive: a UiS harvest
reads only its course pages. The project asks UiS for permission
(decided 2026-10-17); until it answers, the plans harvested from the
archive up to 2026-10 stay in the data. Steiner’s PDFs are fetched by
the project lead’s decision of 2026-10-19, not by a permission from
Steiner, which was not asked: they are open data, and five files make
negligible traffic. Its config records this as such
(`robots_permission`). *Rejected:* waiting for Steiner’s answer, which
would leave Steiner without plans from 2026/27 on. *Since:* 2026-10-16;
enforced since 2026-10-17; Steiner’s exception since 2026-10-19.

**How a plan’s year is known** (`plan_years` in
`R/institution_config.R`): from the URL (most institutions; at NLA the
URL holds the academic year and the page’s JSON the plan of that year
only), or only from when the page was fetched (UiO, MF, NMBU, Steiner
publish only the plan in force). Until 2026-10-17 NLA’s page was taken
to hold the plans of several academic years (`plan_year_basis` “page” in
release data-2026-10-16; see below). *Since:* 2026-10-06.

**An offering reads the plan of its own academic year at NTNU, HVL and
OsloMet too.** Their URL’s year is the academic year the page is for:
NTNU’s `/2025` is “Studieår 2025/2026”, HVL’s “Emneplan for studieåret
2025/2026”, OsloMet’s `/2025/HØST` study year 2025/2026. The URLs had
DBH’s year for every semester, so a spring or summer offering read the
next academic year’s plan: no NTNU spring page showed its own term as
“Undervisningsstart” (534 showed “Vår” of the next year), and all HVL
and OsloMet spring pages named the next study year (fulltext audit,
2026-10-13). Now autumn Y reads `/Y` and spring and summer Y read
`/Y-1`, as NLA, Nord and the current-only sites already did; an offering
takes the page at that URL whichever offering it was fetched for, its
own first. NTNU’s “-A” codes (MGLU1101-A) are the site’s code without
the suffix. *Rejected:* keeping the calendar year, which codes a spring
offering with the plan of the year after. *Consequence:* autumn
offerings keep their plan. Of the spring and summer offerings, 2,851 get
another plan (HVL 1,284, NTNU 1,173, OsloMet 394), 225 get one where
they had none and 484 lose theirs until their page is fetched (HVL 167,
NTNU 233, OsloMet 84: no offering had fetched `/Y-1`). The number of
plans does not change (13,021). A retry harvest on 2026-10-14 fetched
those pages and NTNU’s “-A” pages: HVL 117, OsloMet 79 and NTNU 77
spring offerings get a plan, and so do 1,649 of NTNU’s 1,681 “-A”
offerings (the MGLU primary teacher courses, 2017-2025, which had none).
995 of the 999 NTNU pages fetched carry DBH’s course name, two a shorter
form of it. More plans: HVL 110, OsloMet 68, NTNU 864. The rest have no
page for that year (“Course not found”, OsloMet’s error page, NTNU’s “no
information”). *Since:* 2026-10-14.

**So does an offering at UiS.** UiS’s course page has a menu of plans by
academic year (“2024 - 2025”: the course page for recent years, a PDF
saying “studietilbudet for studieår 2024-2025” for older ones). The
harvest matched the menu on DBH’s year for every semester, so a spring
or summer offering read the next academic year’s plan: 831 spring
offerings’ PDFs named the next study year (fulltext audit pre-pass,
2026-10-14). Now spring and summer Y read the entry for Y-1 - Y, and an
offering takes the page of its course and academic year whichever
offering it was fetched for, its own first. *Rejected:* keeping the
calendar year, as for NTNU, HVL and OsloMet. *Consequence:* autumn
offerings keep their plan. Of the spring offerings, 978 get another
plan, 1 gets one where it had none and 96 lose theirs until their page
is fetched (no offering had fetched it). Plans 13,973 → 13,946 (UiS
1,276 → 1,249: a spring offering now shares the plan of the autumn
before it). A retry harvest on 2026-10-14 fetched those pages: 86 spring
offerings get a plan (63 from the PDF of their academic year, 23 from
the course page, where the menu has no entry for that year; it shows a
later version, and they lose it again under “Page shells are not
plans”), and UiS has 1,313 plans (14,010 in all). Compared with before
the change, 1,063 spring offerings have another plan, 2 have one where
they had none and 11 have none, their PDF being missing from UiS’s
archive. *Since:* 2026-10-14.

**And at NLA, each offering reads the page of its own academic year.**
NLA has a page per course and academic year
(`/emneplan/{CODE}/2024-2025`), and the JSON in it holds the plan of
that year only: so did every one of the 250 NLA pages in the raw store
on 2026-10-17. The harvest took a page to hold all years: it fetched one
page per course code, at the URL of the course’s first offering, gave it
to every offering of the course, and recorded that URL’s answer (a page
or a 404) for all of them. An offering of another year got no plan,
since the json reader takes only the offering’s own year. Now each URL
is fetched once and read by the offerings of its academic year (autumn Y
and spring Y+1), and NLA’s `plan_year_basis` is “url”. Where the site
has no plan for the year asked for, it answers with another year’s plan
(the JSON names that year), which gives no plan. *Rejected:* one page
per course code, which leaves most offerings without a plan.
*Consequence:* no plan changes. In release data-2026-10-16 (teacher-ed),
249 of NLA’s 2,074 offerings have a plan, all from their own year’s
page; 1,554 had another year’s page and 271 a 404 recorded at a URL that
was mostly never requested, hence a plan for only 2-8 % of the
course-years 2018-2023. The pilot sample’s 125 offerings without a plan
were fetched again on 2026-10-17
(`retry = c(RETRY_OUTCOMES, "not_found")`): 61 get a plan, 22 got
another year’s plan from the site and 42 a 404. Offerings with a plan 67
→ 128 of the sample’s 192, course-years 67 → 76 of 100; 14 of them are
teacher-ed offerings (NLA 249 → 263). A full harvest skips offerings in
the raw store, so the other 1,811 teacher-ed offerings without a plan
(998 URLs) get their own page only from such a retry. That retry, on
2026-10-17, gave 1,459 of them a plan; 81 got another year’s plan from
the site and 271 a 404. Teacher-ed NLA offerings with a plan 263 →
1,722, plans 190 → 824, course-years with a plan 254 → 939. *Since:*
2026-10-17.

**Sites that publish only the current plan:** a harvest counts only for
the academic year (August-July) it was made in, and an offering gets the
plan of a harvest from its own academic year, or none. *Why:* these
sites overwrite the plan every year, so a page fetched in 2026 says
nothing about 2025. Example: UiO SPA4192 in April 2026: “Muntlig eksamen
er obligatorisk for deg som fikk opptak fra og med høsten 2021. Muntlig
eksamen er valgfritt for deg som fikk opptak … høsten 2020 eller
tidligere”; in September 2026: “Før endelig karakter settes, skal du ha
muntlig eksamen.” *Rejected:* the latest page for the latest DBH year
(used until 2026-10), which put 2026/27 text on 2025 offerings. Also
rejected (2026-10-17): earlier plans from the Internet Archive’s Wayback
Machine, which would be used only if its captures covered nearly as many
courses as the live harvest. They do not: the share of DBH courses with
any capture of their page is 50-75% at UiO in 2015-17 and 2019-21 but
10% in 2018 and 21% in 2022, 59-76% at NMBU in 2014-22, under 10% at MF
before 2021, and none at INN, Samas and Steiner; NLA’s old site has
362-504 course pages a year in 2017-22 for 576-695 DBH courses. A
capture is also not always the plan of the right year (UiO and MF pages
carry no year). The live harvest finds a plan for 80-90% of these sites’
teacher-ed course-years. *Consequence:* at these sites 100 autumn 2025
offerings have the April 2026 plan and 0 spring 2025 offerings have a
plan (no harvest from 2024/25); the September 2026 plans wait for DBH
2026 offerings. These sites must be harvested once in every academic
year. *Since:* 2026-10-06.

**The raw harvest is append-only and a finalized release is frozen:** a
later harvest (a new DBH year) only adds offerings no earlier harvest
holds, as new files; the raw files of a finalized release are read-only,
and every build checks that each of its plans keeps its id, text and
sections. *Why:* coded plans must not change under the coders when the
data are extended. *Rejected:* merging new rows into the finalized
files, which rewrites frozen data. *Since:* 2026-10-06. The store is
Parquet since 2026-10-16 (one file per institution, DBH year and harvest
run), converted one to one from the RDS files: a format change only,
which leaves every offering, plan and section of release data-2026-10-16
as it was.

**Pages that gave no plan can be fetched again before a release, except
pages not found:** `harvest_all(retry = TRUE)` fetches the offerings
without a plan again and adds them as a new raw file; a retried offering
takes its latest page. Since 2026-10-16 it goes by how the URL the
offering reads answered last (the fetch ledger, below): a page not found
(HTTP 404, 410 or 400, or the site’s own not-found page) is final and
not fetched again, and so is a page that answered HTTP 403 (forbidden)
among other answers; any other outcome is (a server error, no answer, a
block, a page without the plan, NTNU’s “no information for this academic
year”, no URL), and so is a URL nobody has requested. Within a harvest,
a request without an answer or with HTTP 500, 502, 503 or 504 is tried 3
times in all, after 30 and 60 seconds, and a site that refuses the
harvest stops it: an answer 429 (too many requests), or 403 to 5
requests in a row and then to a page known to be public. *Why:* a page
can fail for a while; UiA’s 95 autumn-2023 pages were shells (title and
facts box) when harvested but full plans once on 2026-10-07. A 404 is
the site saying it has no such page, and asking again costs requests (10
seconds each at NTNU and UiB) for nothing. *Rejected:* overwriting the
raw file (the store is append-only); fetching every offering without a
plan again (the rule until 2026-10-16), which asks again for the pages
not found. *Consequence:* retries are done before a release is
finalized. UiA’s shells did not come back: the full pages were copies
last fetched from FS in July 2024, and every copy rebuilt since
2026-09-30 lacks the plan, so these offerings have no plan (2026-10-08).
Of the 15,997 offerings without a plan in release data-2026-10-16, 4,401
read a URL that was not found (UiA 1,758, UiB 1,134, HiØ 866, NLA 271,
HVL 232, NIH 103, other 37) and are no longer fetched again;
`harvest_all(retry = "not_found")` fetches them on request (a plan
published late). A single 403 is the page’s answer, not the site’s
(2026-10-17): MF answered the first page of its pilot, an
access-restricted course node, with 403, while another of its course
pages answered 404 to the same user agent, and the rule until then (any
403 stops the site) ended MF’s harvest after one page. A site that
blocks the harvest answers every request with 403, so after 5 in a row
the harvest asks for a page known to be public (one of the same host
that answered 200 earlier in the harvest, else the latest in the raw
store; the request is in the fetch ledger): a 200 means the 403s were
the pages’ own, and the harvest goes on; any other answer, or no such
page, stops it, and the 5 pages are asked for again at the next harvest.
MF’s PHI course pages answer 403 to every user agent, a browser’s too,
and lie next to each other in a sample ordered by course code, so 5 in a
row alone stopped MF’s pilot again (2026-10-17). *Rejected:* stopping at
the first 403 (one closed page ends a site’s harvest); stopping at 5 in
a row without the check (a run of closed pages ends it); retrying a 403
with `retry = TRUE` (a closed page stays closed; `retry = "forbidden"`
asks again on request). No harvest before 2026-10-17 stored a 403, so no
outcome changed. *Since:* 2026-10-07; by outcome since 2026-10-16; a
single 403 a page outcome since 2026-10-17.

**Each harvest records how every page was fetched (fetch ledger):** one
row per offering and harvest run with the URL, the time of the request,
the HTTP status, the outcome class (ok, not found, forbidden, shell,
server error, timeout, blocked, robots, error, no URL), the size and
SHA-256 hash of the page as stored, the raw file and the DBH snapshot
the offerings came from, and a row for each request of a page known to
be public that tells a site’s refusal from closed pages (outcome probe,
since 2026-10-17). *Why:* the coverage of a release (for each offering
of the frame: plan, not found, retry pending, never harvested) and the
retry rule need the outcome of each fetch, and the hash shows when a
site changed a page between harvests. *Rejected:* deriving the outcome
from the raw rows at each build only (as before), which has no time or
status of a fetch that succeeded and no record of the DBH snapshot used.
*Consequence:* the harvests before 2026-10-16 have no ledger; theirs is
reconstructed from the raw rows (no time and no DBH snapshot, the status
only from an error message). Of their 47,449 rows: 29,388 ok, 5,219 not
found, 2,036 NTNU “no information” shells, 12 server errors (UiB, HTTP
502) and 10,794 without a URL. Of the 44,492 offerings of the teacher-ed
frame (DBH snapshot 2026-10-16) at institutions with a config, 25,028
have a plan, 3,467 were never harvested (the years before the latest at
UiO, Samas, MF, NMBU and Steiner, which show only the current plan) and
a retry would fetch 11,361 again (6,996 of them without a URL). 235
pages read another plan text in a later harvest of the same URL: 95 UiA
shells that were full plans a week later, 51 HVL pages fetched again
from the English site, and 89 plans of a new academic year at UiO, MF
and NMBU. The page body alone does not show a change: at NTNU and
OsloMet it carries a new token on every fetch, and at UiB and UiS 70
pages had another body with the same text, so the plan text of two
bodies that differ is compared. *Since:* 2026-10-16.

**Pilot protocol and gate: an institution’s full harvest waits for a
pilot whose audits find it ready.** Before the full harvest of an
institution (a new one, or a configured one going from teacher education
to all its courses), a sample of its DBH courses is harvested, built and
audited; the audits decide, and the sample’s gate numbers flag what they
review (README “Piloting an Institution”). *The sample:* 100
course-years (a course in a year, with every semester DBH lists it in;
about 180-200 offerings) in 2019, 2022, 2025 and the oldest year the
site has plans for, leaving out the pilot years before it (at a site
that shows only the current plan, the latest DBH year), with at least
one course-year of each DBH department, five of each level group (lower
degree, higher degree, PhD, continuing education: a programme so named
or DBH’s experience-based master, other) and ten English-taught ones,
the rest at random in equal numbers per year. It is drawn with a fixed
seed from the dated DBH snapshot and kept in `data/pilot/`. The
course-year is the unit because DBH lists most courses in both
semesters, and a course taught in one has no page in the other. *The
gate numbers*, flagged for review below these values, as shares of the
sample’s course-years: a page in some semester 80 %, a plan 80 %, and of
those with a plan an assessment section 90 % and a
coursework-requirements section 70 %; flagged above: 65 % of the
headings on the sample’s pages unmapped, and any personal data left.
Plans that an unusual layout leaves without sections are accepted
(decided 2026-10-16) but counted. *The verdict* (decided 2026-10-17):
each of the three pilot audits (text, sections, anonymization) reads the
gate numbers and ends with a verdict on the institution: ready for full
harvest; ready with known limits, each flagged number named with its
cause on the site (such as course-years DBH lists that have no page); or
not ready, when the sample shows a confirmed serious error in the text,
the sections or the anonymization, personal data left, or a flagged
number with no cause found. The full harvest starts when all three find
the institution ready. *Why:* a full harvest takes days at the slowest
sites (10 seconds a page at NTNU and UiB), while a config, URL builder
or heading map that fails on part of a site (a faculty, an old layout,
the English pages) shows on a sample that crosses them, at a hundredth
of the requests. The audits decide rather than the numbers because some
institutions are below a value for causes on the site that no fix
removes, while a sample above every value can still hold text cut
wrongly, which only reading it shows. *Values:* from what the configured
institutions reach in the teacher-ed frame (build of 2026-10-16; years
2019, 2022, 2025 and the oldest year in which at least half the
course-years have a plan; 18 institutions, Samas having no plans by
design). A page and a plan: 15 of 18 reach 80 % (median plan 90 %);
below are UiT (64 %), UiB (72 %) and USN (72 %), where URL discovery or
the site has no page for a quarter to a third of the course-years DBH
lists. Assessment: all reach 92 % (OsloMet, practicum plans without
sections). Coursework: 17 reach 74 % (Nord); UiO has 32 %. Unmapped
headings: from 0 % (UiS, USN, read as text) to 64 % (Volda, whose facts
box has field labels for headings, “Emnekode:”); the unmapped headings
today are facts boxes, contact and course-overlap headings, so this flag
only catches a page read wrongly or a site whose headings the map does
not know, and the most frequent unmapped headings are checked by hand as
well. Personal data: none anywhere. A pilot sample drawn the same way
among the teacher-ed offerings gives the same numbers within its error
(about ±0.07 at 100 course-years), except where an old layout weighs
more in a sample with equal numbers per year: at UiT, 8 of the frame’s
10 course-years without an assessment section and 18 of 42 without
coursework are 2008 pages, so the sample has assessment 86 % and
coursework 60 % (98 % and 93 % in the frame); at UiS, 7 of the sample’s
17 without coursework are 2009 plans (81 %, 90 % in the frame).
*Rejected:* a simple random sample of offerings (it misses small
departments, PhD and continuing education); every department crossed
with every level and year (over a thousand cells at NTNU for 100
course-years); thresholds per institution (they would accept today’s
gaps as the standard); thresholds that block the full harvest on their
own (rejected 2026-10-17: an institution below a value for a cause on
the site, such as course-years with no page, would be held back for
good, or the value lowered for all); a full harvest without a pilot (a
fault found afterwards costs another harvest of the pages it affects).
*Consequence:* an institution can be harvested in full with a gate
number below its value; the cause is recorded with its verdict, and
where it limits the data (course-years without a page, layouts without
sections) in `data/data_notes.qmd`. *Since:* 2026-10-16 (the protocol;
the values proposed then are kept as review flags); the audits’ verdict
decides since 2026-10-17.

**UiO semester pages are not used:** URLs such as `/h24/` hold logistics
(teachers, timetable, exam dates), not the plan (decided 2026-02).

**UiO course URLs come from its sitemap:** the URL of a UiO course page
holds a faculty and department folder that DBH does not give. The
folders are read from UiO’s sitemap at each harvest; it lists the
courses taught now. A course not listed gets the folder of the listed
courses with its code prefix, else of its DBH department. *Why:* the
hand-kept map of 18 departments covered teacher education only, and
pages of courses not taught now remain (ENG4429, last taught spring
2023, has its plan). Tested on the listed courses, the prefix gives the
right folder for 97.8%, the department for 95.7%. *Rejected:* the
hand-kept map for all departments (a course that moves breaks it:
NAT5000 is now in `matnat/natfag`); only listed courses (they miss pages
that exist); the listing `/studier/emner/alle/` (the same courses as the
sitemap). *Consequence:* the teacher-education URLs are unchanged except
NAT5000 (its old URL redirects). Of the 5,163 course codes active in DBH
2025, 3,689 are listed; of 40 guessed URLs sampled, 11 gave a page, and
the rest are courses without a UiO page (UNIS courses, PhD reading
lists, odontology specialist codes). *Since:* 2026-10-16.

**UiT: the Norwegian document of a semester listed twice:** UiT’s
semester picker can list a semester twice, an English document beside
the Norwegian one; the Norwegian one is used. A course with only an
English document keeps it (ENG-1040, ENG-3193, ENG-3194, ENG-3993).
*Why:* the English twin was an empty page in all 5 cases, while the
Norwegian one has the plan. *Rejected:* the first option (the rule
before, which took the English twin). *Consequence:* 5 offerings get a
plan (ENG-2980 spring 2017, ENG-3980 and ENG-3981 spring 2016, LRU-1300
autumn 2010, LER-1150 autumn 2018), 2 of them a plan not in the data
before. *Since:* 2026-10-08.

**HVL, Nord and NTNU: a plan published only in English is read from the
English page:** an HVL page that says “Emnebeskrivelsen finnes kun på
engelsk” (or, since 2026-10-17, “Dette emnet er kun beskrevet på
engelsk”, “Dette emnet er berre skildra på engelsk”, “Se engelske
nettsider for mer informasjon”), a Nord page that says “Emnet foregår på
engelsk. Se engelske nettsider ved å velge engelsk språk.” (since
2026-10-17), and an NTNU page whose course content says only “See
English version.”, “Se engelsk beskrivelse.” or “Se engelsk tekst.”
(since 2026-10-19), holds no plan, only a pointer to the English site,
so the plan is fetched from the English page of the same course and year
(`english_page` in the configuration; NTNU’s is on www.ntnu.edu). Its
headings map like the Norwegian ones (“Entry requirements” is
prerequisites, “Compulsory learning activities” coursework requirements;
Nord’s “Course description”, “Learning activities”, “Exam description”;
NTNU’s “Learning methods and activities”, “Compulsory assignments”,
“Further on evaluation”, “Required previous knowledge”, “Course
materials”), Nord’s English year stamp (“Current active course
description (last updated 2026/27)”) is read like the Norwegian one,
NTNU’s exam tab and contact block are read like the Norwegian ones, and
a page with only the pointer is a shell. Since 2026-10-19 a page with
the pointer gives no plan also when it is not a shell
(`plan_offerings()`): NTNU’s pointer pages keep the exam tab, which is
an assessment section, so the shell rule would keep them as plans with
the pointer in place of the content. *Why:* these are mostly the English
subject modules of the teacher education programmes (MGBEN, MGUEN);
without the English page, HVL’s English subject would have no plans. The
pilot samples of 2026-10-17 found the other wordings in English-taught
master’s courses (HVL ADA512, BI4-301, MAS538, MMO5010; Nord BIO5023,
BIO5031, BIO5036, all 2025), whose pointer had counted as a plan.
*Rejected:* leaving these offerings without a plan; at Nord, the page’s
`hreflang="en"` link (the page has two, one of them to the current plan
without year and semester); at NTNU, keeping the pointer page as a plan
of its exam tab, and taking every page with the template’s “English
version” link (all pages have it). *Consequence:* 86 offerings (10
course codes, 2020-2025) get a plan, 46 plans more, in English. 5
MGBPE201E offerings whose pointer had counted as a plan (one plan of 140
characters) were not retried with them; a second retry harvest the same
day gave them their English plan (3 plans). On 2026-10-17 the pointers
of the pilot samples’ 10 such offerings (HVL 7, Nord 3) stopped counting
as plans, and their English pages, fetched again, gave them 10 English
plans (HVL 7, Nord 3) in place of 8 pointer plans. No teacher-education
offering has one of the new wordings. At NTNU the pilot sample’s 7 such
offerings (IMT4127 2022, IMT4312 2025, MACS490 2025, SMF1081 spring
2022) were fetched again on 2026-10-19 and got 7 English plans in place
of 5 pointer plans (NTNU plans 2,098 → 2,100). Three teacher-education
offerings (GEOG2500 spring 2022, autumn 2022, spring 2023) say “Se
engelsk tekst.” in the course content only, with Norwegian compulsory
activities; fetched again the same day, they got one English plan (4,201
characters, 7 sections) in place of one pointer plan (434 characters, 3
sections, its course content only the pointer). *Since:* 2026-10-15; the
other wordings and Nord from 2026-10-17; NTNU, and no plan for a pointer
page that is not a shell, from 2026-10-19.

## Unit of analysis: the unique plan

**A plan is a unique anonymized text per institution and course code**
(`plan_content_id`, a hash of the normalized text). Offerings with the
same text share a plan: 26 505 offerings have 14 740 plans. *Why:*
consecutive years and both semesters mostly repeat the same text.
*Consequence:* the id changes whenever cleaning changes the text, so
coding is tied to a frozen release and carried to later releases with a
crosswalk.

**Page shells are not plans:** a page with no plan section to read and
under 1,500 characters of plan text (without the title and facts box,
since 2026-10-13; only the facts box, a credit-overlap box,
“Emnebeskrivelse”, a pointer to the English page, every section “Se
fagplanen.”, a list of headings with no text under them) gives its
offering no plan. So does an INN page that shows a later version: where
the requested semester has no version, INN shows the latest one, mostly
with the notice “Emnebeskrivelsen for valgt semester er ikke publisert
enda. Her er den siste versjonen vi har tilgjengelig”, sometimes without
it (under “Kun tilgjengelig på engelsk”, or none). Where the version’s
start semester, selected under “Startsemestre” (`page_term`), is after
the offering’s semester, the offering gets no plan. The banner
“Statusmelding / Kun tilgjengelig på engelsk” (sometimes with the
language left out) stands over pages that hold the whole plan in
English; it is site boilerplate and is cut (since 2026-10-20). Nord does
the same without a notice: for a year it has no plan for, its page shows
the current plan, headed “Gjeldende emnebeskrivelse (sist oppdatert
2026/27)” instead of “Emnebeskrivelse for 2023/24”. Where the year it
names is after the offering’s academic year, the offering gets no plan;
where it is not, the page is the version in force that year and stays
(`page_term` in the configuration; a page that names only an academic
year counts as a version starting in its autumn, so the comparison is
the academic year’s). UiS does the same: for an academic year its menu
of plans lacks, the harvest fetches the course page, which shows the
latest version, the one selected in the menu (“2026 - 2027”); a course
page without a menu names no year and stays. Longer pages without
sections stay (OsloMet practicum courses, below). *Consequence:* 174
plans left the data on 2026-10-06 (UiA 95, UiB 27, OsloMet 26, Nord 11,
HVL 10, UiT 4, NTNU 1). With pages that are not the plan at all (INN’s
course search page, OsloMet’s error page) and later versions (HiOF, INN,
Nord, UiS), 2 286 offerings have page text but no plan (inn 1108, nord
819, oslomet 67, uia 95, uib 80, uis 97, uit 20). *Since:* 2026-10-06;
heading lists (UiB NOLI216 and NOSP216, autumn 2018) from 2026-10-07;
NTNU pages whose only section was the exam tab from 2026-10-12 to
2026-10-13, when the tab came back (20 plans, 70 offerings). INN later
versions from 2026-10-07: 1,077 INN offerings (384 of 407 pages from
2022, about half of 2023-2025) lose their plan; the April 2026 harvest
has the same notice on the same pages. From 2026-10-20 (anonymization
audit, 2026-10-17) INN decides by the version’s start semester, not by
the notice: the notice alone also removed versions in force, and pages
without it kept later ones. 15 offerings lose a plan (2022 4, 2023 6,
2024 2, 2025 3), among them 2MNK172-2 and 2MNK172S-2 from spring 2022 to
spring 2025, which showed the version taught from autumn 2025 (those two
plans leave the data), and INN3024-1 in 2023, which showed “Teaching
semester 2027 Spring”. 40 offerings with the notice get a plan back: 27
show a version that started before their semester, 13 one that starts
the spring after their autumn semester. INN offerings with a plan go
from 729 to 754, plans from 609 to 613; 1,136 offerings show a later
version (388 of 407 from 2022). The same day the rule moved from
academic years to semesters: an autumn offering whose page shows the
version from the following spring had kept it, since both are in one
academic year. 15 autumn offerings lose their plan (2023 2, 2024 10,
2025 3; among them the 2024 master’s modules 2MEN5101, 2MKRLE5101,
2MKRO5101 and 2MNF5101, which showed the version from spring 2025), and
1,151 offerings now show a later version. Cutting the English-only
banner changes the plan id of 9 offerings (6 plans) and joins
23DPRO101’s spring and autumn 2025 into one plan. INN offerings with a
plan go from 754 to 739, plans from 613 to 608. *Rejected:* keeping the
notice as a second rule (it removes the 27 versions in force); comparing
academic years for INN too (it keeps a version that starts after the
offering’s semester). Nord’s later versions from 2026-10-14 (fulltext
audit, 2026-10-13): 761 offerings lose their plan (all of 2016-2018 and
205 later ones), 90 plans leave the data, and Nord’s course-years with a
plan go from 1,843 to 1,478. UiS’s later versions from 2026-10-14 (found
while matching its menu on the academic year, above): 97 offerings lose
their plan (56 spring, 41 autumn, most of 2007-2018; the version shown
is 1 to 10 years later, median 4), among them the 23 that the retry
harvest of that day gave a course page. HiOF’s later versions from
2026-10-22 (pilot fulltext audit, 2026-10-21): the title of a page names
its semester, and two `videre/` pages name a later one than their URL
(spring 2020, autumn 2023); 2 offerings lose their plan, none in the
teacher-education frame. 6 plans leave the data (UiS 1,313 → 1,307,
14,004 in all), and UiS’s course-years with a plan go from 1,495 to
1,457. 306 course pages show an earlier version and stay, as do 96
without a menu. *Rejected:* keeping the later plan with a flag (decided
2026-10-07: an offering is coded with its own year’s plan or none). For
UiS, the menu’s latest entry before the offering’s year (32 of the 97
have one, e.g. “2014 - 2015” for 2016): the site shows no plan for that
year, and a year missing from the menu does not tell whether the earlier
plan was still in force, and the data do not guess a plan (confirmed
2026-10-15). A rule on `course_plan` alone (2026-10-13): the characters
in lines that read as text (four or more words; not a heading, a
placeholder or a short “Label: value” fact) do not separate shells from
plans at any threshold. A credit-overlap box reads as text, and so the
391 shell offerings have a median of 88 such characters and up to 738
(UiA, Nord); some plans have little but the assessment design (NTNU
master’s thesis pages, “Vurderingsordning: Avhandling”, UiS MGLINT’s
exam box). Below 150 characters, 128 plan offerings would become shells
and 127 shell offerings stay plans; below 500, 326 and 4. Whether a
section can be cut is what tells a page with text about the course from
one without.

**Tables are read one row per line:** each row of a table on the page is
one line, its cells separated by tabs and each cell on one line, so an
exam table (INN, Volda, UiT from 2022) has one line per exam. *Why:*
splitting the assessment text by exam needs the rows. The reader had run
a table with `<thead>`/`<tbody>` into one line (INN, Volda) and joined
the lines of a cell without a space (Volda “Fagartikkel2. Utvikling …”,
UiT “Karakterregel:A–E”, “Spring 2009Veiledning”). *Rejected:* keeping a
cell’s own line breaks, which split one exam over several lines.
*Consequence:* the text changes only in whitespace; 725 of 2,314 UiT
plans and 77 of 547 Volda plans get new ids, because words a cell had
joined are now apart; no plans merge or split. *Since:* 2026-10-08. A
line break inside a `<span>` or link elsewhere on the page is now a
space as well, not glue (HiOF “minutter.Det tas”, UiB
“KunnskaparStudenten”): 64 UiT, 37 HiOF, 8 UiB and 3 UiA plans get new
ids, and two pairs of UiT plans that differed only by such glue merge
(SOA-3905 spring and autumn 2011; LER-1201 spring 2018). *Since:*
2026-10-08.

## What the text holds

**The published text follows a written spec, `course_plan_spec.md`.**
Personal data are out of everything published. Boilerplate is out of
`course_plan` and the sections: text that belongs to the website, not
the plan (navigation, widgets, timetables, site-wide notices), or that
describes the page, not the course (fetch and version stamps, approval
stamps, administrative dates). All other plan text stays in
`course_plan`, also stock text repeated in every plan and text that
belongs to no section. A section holds the plan text about its part and
nothing else. Assessment holds the assessment design with exam aids and
exam language, but not logistics (dates, rooms, registration, resits).
Notices that say something about the assessment (AI use, plagiarism,
COVID changes of the exam form) are assessment text, stock or not.
*Why:* `course_plan` is the source text for readers and coders and
should lose only what is of no use to someone studying teacher
education; the sections are a convenience for coding. Before the spec
the rules were decided one at a time in seven places (README, “What Each
Mechanism May Remove”), and some went against each other: the MF and UiS
facts boxes were left out and NTNU’s kept, and the NTNU exam tab left
`course_plan` (2026-10-12). *Rejected:* removing every AI, plagiarism
and COVID line from assessment (the rule since 2026-10-01), which also
removed rules for the use of AI and the exam forms that replaced written
exams during COVID; keeping the NTNU exam tab out of the sections, which
leaves 743 NTNU plans without an assessment section. *Consequence:* the
sections hold a median 83 % of a plan’s text, and 45 plans have no
section. The data do not yet follow the spec where its “Known
deviations” says; each fix gets its own entry here, with numbers.
*Since:* 2026-10-13.

## Anonymization

**All published text is anonymized** (`anonymize_text()`): staff names
(role labels with names, “Name (Role)” lists, signature and approval
lines), e-mail addresses, phone numbers, IP addresses in library links,
and administrative dates and years (“Opprettet 2020”, “2023/2024”);
content years (“etter 1945”) are kept. A check on every build
(`privacy_check`) looks for e-mails, phone numbers, IP addresses and
staff-name layouts in the published files. *Why:* the plans are public,
but the published data should not carry personal data. *Since:* 2026-09
to 2026-10; UiA lists of several course coordinators and USN approval
lines from 2026-10-06; names after a staff role anywhere in a line (“…
avklares med faglærer <name>”, “Ta kontakt med <name> eller <name>”),
HVL campus contact lines and UiT’s 2008-2011 timetable block (teachers,
rooms, first lectures) from 2026-10-07, after the anonymization audit of
all institutions found them in 16 plans.

**Coordinator blocks and role headings go with their names.** OsloMet’s
“Emneansvarlig” block, the last part of the page before the
credit-overlap box (“Emneoverlapp”), goes whole: the heading and every
line under it, which hold only names, with a title (“Professor <name>”),
a part (“Delemne <part>: <name>”) or a role (“Writing workshop
coordinator: professor <name>”), or a placeholder (“x”). At every
institution a role heading over a line that names someone goes with that
line, also across blank lines and without a colon (UiB’s “Emneansvarlig”
over “<name>, <research school>”, UiA’s “Contact persons:” over “<name>
and <name>”); a role in a sentence loses the names also after “er:” and
with a lowercase role in brackets after each name (“Emneansvarlige er:
<name> (sosionom) og <name> (pedagog).” → “Emneansvarlige er:.”); and
“coordinator” counts as a role. `privacy_check` looks for the same
layouts, so it now finds a role label followed by a name in these forms.
*Why:* the pilot anonymization audit of 2026-10-17 found staff names in
4 OsloMet offerings of the all-courses data, in forms both the
anonymizer and `privacy_check` missed: the old rule took only one plain
name line under “Emneansvarlig”. The widened check found 3 more
offerings (UiB 2, UiA 1). *Rejected:* one rule per line form in the
block, which missed each new form; taking any bracket after a name,
which also took an affiliation (“faglærer <name> (UiA)” in a USN
teacher-education plan, which stays). *Consequence:* 8 offerings of the
all-courses data change text and plan id, each its own plan (OsloMet 5,
NATA6110 2018 only losing the placeholder; UiB 2; UiA 1); 2 OsloMet
`course_content` sections lose the names; 14,908 plans before and after.
The teacher-education frame does not change. *Since:* 2026-10-17.

**Dates of government documents are kept.** A date or year rule does not
fire when a government document is named up to 80 characters before it
on the same line: laws and regulations, white papers and propositions
(“Meld. St. 21 (2016–2017)”, “Prop. 57 L (2022–2023)”), NOUs, curricula
and framework plans, national guidelines, conventions, and what the
Storting, ministries and directorates publish (“Ny opplæringslov fra
01.08.24”, “fastsatt av Kunnskapsdepartementet 7. juni 2016”). *Why:*
these dates say which document is cited; they are content, not
administration (decided 2026-10-08). *Rejected:* removing session years
with the other academic-year ranges, which turned “Meld. St. 21
(2016–2017)” into “Meld. St. 21” (2026-09). *Consequence:* 185 plans
keep such dates; an access date next to a government link also stays
(“\[Lesedato: …\]” after a udir.no address). At the same time
administrative dates in more forms go: em-dash year ranges (“Studieår:
2019 — 2020”, NLA), UiS version stamps, approval and update dates in
words (“godkjent i studieutvalget 14. november 2016”) or with a
two-digit year (“Godkjent 22.03.23”), and semester codes (“H23”,
“H2025-V2026”): 1,683 plans lose one; 21 plans that differed only by
such a date merge with another. *Since:* 2026-10-08. The 80 characters
are counted from the start of the date, not its end, so “nasjonale
retningslinjer … av 1. september 2016, revidert 17. oktober 2018” keeps
its revision date and “høsten” over a line break stays; faculty
regulations count as such documents (“utfyllende regler … av
01.08.2015”). Dates in references stay too: a dd.mm.yyyy date after a
comma, which in these pages is always a reading-list entry (“Razsviet,
4.11.1923”, “SSB, 07.06.2023,”), and ISO dates (“2001-02-01”, read as
the academic year “2001-02” before). *Why:* a reference’s date
identifies the work cited (audit rubric, 2026-10-11). *Consequence:* 138
plans change text (usn 54, oslomet 39, uis 31, uit 9, ntnu 5); none
merge or split. *Since:* 2026-10-12.

**OsloMet’s “;” glue is a space.** In 2021–2022 OsloMet’s pages put a
“;” where a space or line break belongs (“En;viktig del;av”, “1;000
ord”); a “;” not followed by a space becomes a space (7,254 on 231
pages, none in other years), and a real semicolon, followed by a space,
stays. *Why:* the glue is page rendering, not text; it made the 2022
pages separate plans. *Consequence:* 57 OsloMet plans merge with the
same plan of a neighbouring year (625 → 568). At the same time HVL’s
Nynorsk “Emnet vert ikkje tilbydd 2025/2026.” goes like the Bokmål
notice (4 plans merge), USN’s Leganto type labels go before quoted and
lowercase titles too, and a second approval date on a stamp line goes
(“… og på fullmakt av leder i utdanningsutvalget 4. mars 2019”). 13,979
plans. *Since:* 2026-10-12.

**A removal takes the space and separators that joined it.** A removed
date, season or e-mail takes the space before it, a season with year
also the dash, comma or colon before it and a list after it (“15 stp,
høst 2019 og vår 2020” → “15 stp”), and a date or clock-time range goes
as one unit (“Dato: 27.05.2024 –31.05.2024”). *Why:* “Faget går .”,
“(Kull )” and “Dato: –” are artifacts of the removal, not text.
*Consequence:* no content changes, but the dedup hash keeps punctuation
and spacing, so 2,480 plans get a new id and 12 that differed only by
such leftovers merge (13,967 plans). *Since:* 2026-10-12.

**Approval and change-log dates with only month and year go; cohort
years stay.** USN’s approval stamps with a month and year lose the date
like those with a full date (“Godkjent desember 2017.” → “Godkjent”,
“Emneplan godkjent mars 2022.”), and a change-log entry at the start of
a line loses its date with the colon or dash after it (“November 2020:
Endret karakterskala.” → “Endret karakterskala.”; “27.08.2020: Endring i
…” lost its date before but kept the “:”). Cohort and plan-version years
stay: “(kull 2010-2012)”, “fra og med kull 2013”, “studieplan som
gjelder fra og med 2013” (210 plans, 339 offerings; UiT 140 plans, USN
28, HVL 20, HiØF 14, UiS 6, Nord 2). *Why:* the stamp and change-log
dates say when the record was edited, not what the course is; a cohort
year says which version of the programme the course belongs to, which is
plan text, and the offering’s own year is in the metadata (decided
2026-10-14 on the anonymization audit of 2026-10-11). *Rejected:*
removing cohort years with the administrative years, which would lose
which cohorts a course is for; removing every month and year, which
would cut publication months from references (“June 2011”, “utkommer
januar 2022”). A line-initial month and year goes only for years from
2000, so a timeline entry (“April 1940: …”) stays. *Consequence:* 45
plans change text (USN 37, HiØF 7, UiT 1), all in these lines; none
merge or split (13,973 plans). In the sections a dated change-log entry
goes as a whole line, since by the spec the change log is plan text but
not section text: 7 HiØF assessment and coursework sections lose a line
such as “Eksamensform er endret etter vurdering av programansvarlig og
studiedirektøren.”; an undated notice of a changed exam form stays
(“MERK: Skriftlig skoleeksamen endres til hjemmeeksamen.”). *Since:*
2026-10-14.

**NTNU: the exam tab is plan text; the assessment section holds one exam
session of it.** The page’s exam tab is FS’s exam registration:
“Vurderingsordning: …”, “Karakter: …”, then per exam session (“Ordinær
eksamen - Høst 2022”, “Utsatt eksamen - Vår 2023”) the exam parts with
weight, grading, duration, permitted aids, exam system, dates, times and
rooms. `course_plan` keeps the tab without the dates, times, rooms,
comments (“Individuelt tidspunkt kommer senere”) and the link. The
assessment section and the plan id keep the parts of one session, the
first ordinary one, without the session headings, the exam system and an
exam part’s “(1)” that points to a comment: the sessions repeat the
parts, and which sessions a semester lists (a resit or not) does not
change the plan. `course_plan` keeps “Ansvarlig enhet” with the
department; the contact names go, the timetable tab (from 2025) is not
read, and the plan id leaves the unit out. *Why:* by the spec the tab is
plan text, and its form, parts, weights, duration and aids are the
assessment design; for 1,068 NTNU plans it is the only assessment text.
A change in the design (a five-day home exam that became three days) is
a new plan. *Rejected:* leaving the tab out of `course_plan` and the
sections (2026-10-12: it looked like coded exam data, but it is page
text); keeping only “Vurderingsordning” and “Karakter” (2026-10-01,
2026-10-08: the parts and weights are in the sessions); leaving the
whole tab out of the plan id (an exam form changed only in the tab, as
in the COVID years, would not make a new plan); hashing the unit, which
split plans only because a department was renamed. *Consequence:* 2,009
of 2,015 NTNU plans have an assessment section (941 of 1,988 before):
941 add the tab to their own “Mer om vurdering” (median 449 → 597
characters) and 1,068 have only the tab (median 100). The 20 pages that
were shells for want of a section (master’s thesis courses `*3950`,
discontinued courses) are plans again (70 offerings). 7 plans split
where the design changed between years (duration, aids, an exam part):
13,967 → 13,994 plans. 1,908 of 2,015 NTNU plans name their unit. Other
institutions’ plans and sections do not change. *Since:* 2026-10-08
(unit); exam tab 2026-10-13, after it was left out from 2026-10-12.

**The rules that cut `course_plan` remove only personal data and
boilerplate.** On 2026-10-13 each such rule was checked against the spec
on the text it removes: `exclude`, `post_fn`, the `.anon_*` cuts to the
end of the page or of a block, and what `.read_doc()` drops (`<label>`,
`<select>`, scripts). They remove contact blocks, cards and staff lists
(names, titles, e-mails), approval signatures, year and semester
pickers, version stamps (“Gjeldende emnebeskrivelse (sist oppdatert
…)”), link labels (“Kopier lenke”), UiO’s site-wide list of exam links,
MF’s library notice, the semester timetable at the end of UiT’s
2008-2011 plans (first lectures, rooms, weeks, teachers, now and then a
line on the semester’s teaching such as “Det gis kun individuell
veiledning”) and MF’s exam schedule (“Eksamensdatoer”: dates, deadlines
and the exam forms the plan’s assessment text already gives). One rule
took plan text: UiT’s cut from the contact block to the end of the page
also took the responsible unit, which now stays (“Ansvarlig enhet” and
the unit, at the end of the text and in no section); the plan id leaves
it out, as for NTNU. Rules that no longer fire (UiS’s “Kontakt” cut,
UiB’s “Kontakt” and “Studierettleiar” lines) stay as a guard.
*Consequence:* 2,281 UiT plans gain their unit; no plan id or section
changes. Text outside each page’s `selector` is a known deviation of the
spec (title and facts sidebars at several institutions). *Since:*
2026-10-13.

**The title and facts box are in `course_plan`, but not in the plan
id.** At HVL, NIH, UiB, UiO and UiT the page’s title, course code and
facts box (credits, campus, semesters, language, level, programmes, the
responsible unit) sat outside the part of the page that was read; at
Nord its facts sidebar did. They are read now, without the names, exam
schedules, links and pickers next to them (HVL’s course coordinators and
exam dates, NIH’s course coordinator, UiB’s sidebar with contact e-mail
and exam schedule, UiO’s links to the semester pages, UiT’s apply button
with the application period, Nord’s staff and semester picker). Where
the configuration names the title and facts box (`facts`: HVL, MF, NIH,
Nord, UiB, UiO, UiS, UiT) they are in no section, and the plan id, the
shell rule and the sections’ text fallback read the plan text without
them. “Ansvarlig avdeling” (NIH, UiA, HiOF) and “Ansvarlig fakultet”
(Nord) with the unit are left out of the plan id too. *Why:* by the spec
the title and facts box are plan text. Hashed, they split plans whose
text is the same from year to year: with them in the id, 158 plans split
(HVL 41, Nord 29, UiT 87, UiB 1; 126 more plans in all), on a campus
that is added or dropped, a programme list, a course-code variant in the
title (MGBKR101 and MGUKR101, LER-1234 and LER-1234F) or a “Nivå” line
shown in some years. In the text fallback a title read as a heading
(“Tysk språk og litteraturvitskap …” as a reading list). *Rejected:*
hashing the facts box; rules for each field that varies (campus,
programmes, code), which would have to be found institution by
institution. *Consequence:* 4,742 plans at HVL, NIH, UiB, UiO and UiT
gain their title and facts with the same id; Nord’s 1,365 plans gain the
sidebar’s facts and, with the title out of the id, a new id; MF’s 26 and
180 UiS plans get a new id as their facts box leaves it, and HiOF’s 820
and UiA’s 681 as the unit does. No plans merge or split (13,994), and no
section changes. UiB’s unit, an unlabelled link in the sidebar, is not
read; every UiB offering has its unit from DBH (`department_name`). The
other institutions followed the same day (next entry). *Since:*
2026-10-13.

**The title and facts box leave the plan id at every institution.** The
configuration names them at HiOF, INN, NMBU, NTNU, OsloMet, UiA and
Volda (Volda’s one-line fields, its student numbers at the foot of the
page included) and in NLA’s JSON (the title and the table of code,
semester, place, year, language and credits). In pages read as text they
are the lines before the plan: USN’s lines before the table of contents,
the metadata block of UiS’s PDF plans (which the section reader already
skipped) and that of Steiner’s PDF plans. As before they stay in
`course_plan` and are in no section. UiA’s breadcrumbs, which repeated
the title, and its facts box’s links (“Timeplan”, “Pensumliste
(Leganto)”) and OsloMet’s portlet name (“EPN-V2”) leave `course_plan`,
as page chrome. The sections’ text fallback, tried when the html reader
finds fewer than three sections, now stands only if it finds a section
that no heading on the page names. Without the title in front, it read
the “Om emnet” tab of five terse NTNU plans as course content (their
admission, subject areas and unit), and it had read a credit-overlap
note as course content at three UiT pages. *Why:* the user’s aim
(2026-10-13) is that plans with the same text group together across
years, whatever their ids; a title, level, campus or credits line that
changes alone should not make a new plan. *Rejected:* keeping the facts
box in the id where a field such as credits or language changes; of the
158 plans the facts box split at the first eight institutions, none
changed credits. *Consequence:* 130 plans merge into others and none
split (13,994 to 13,861 with the three UiT shells): UiA 62 (a “Varighet:
½ år” line shown from 2024), NTNU 41 (level, language, place), UiS 11
(PDF headers), Volda 7 (level, student numbers), OsloMet 7 (programme
names, link labels), INN 1 (a renamed title) and USN 1. None changed
credits; two changed the language of instruction (NTNU POL3006 from
“Engelsk og norsk” to “Norsk”, TEGN1001 adds English). 12,128 offerings
get a new plan id. 25 more plan ids are now shared by two course codes
with the same text (OsloMet 20, NLA 4, NTNU 1; HVL 20 and UiT 83
before); plans are keyed by id, institution and course code. Sections
change only at USN, where the header (code, credits, language) had
opened 10 learning outcomes sections, and at the three UiT pages, which
become shells (4 offerings). *Since:* 2026-10-13.

**Stock text, lists and leftover labels no longer split plans.**
Comparing consecutive plans of one course (10,369 changes, 923 of them
by three distinct words or fewer) showed text that changes with the year
or the fetch while the plan stays the same. Some of it is boilerplate
and leaves `course_plan` and the sections: NTNU’s banners (“Nytt fra
studieåret 2025/2026”, “Undervises ikke studieåret …”, “Det tilbys ikke
lenger undervisning i emnet.”), UiT’s exam-table labels “Innlevering:”,
“Utlevering:” and “Dato:” (their dates were already cut, and a semester
without a date had no label), UiB’s notice of when the reading list is
published (“Litteraturlista vil vere klar innan 01.06. for
haustsemesteret …”, a publication date), and OsloMet’s runs of
semicolons before a space (the 2021-2022 rendering artifact, as single
ones before). The rest is plan text in no section; it stays in
`course_plan` and joins the facts box, out of the plan id and the shell
rule: NTNU’s admission programme list (“Spesielle vilkår”),
credit-reduction box and subject areas (“Fagområder”); UiA’s student
evaluation text and its admission flags (“Tilbys som enkeltemne”,
“Tilgang for privatister”); Volda’s evaluation and examiner-arrangement
sentences and its programme list; Nord’s “Type emne” (Valgbart,
Obligatorisk), shown in some years. *Why:* the user’s aim (2026-10-13)
is that plans with the same text group together. NTNU’s spring and
autumn offerings of a year read the same page (`/{CODE}/{YEAR}`),
fetched at different times, and between the fetches NTNU reworded the
stock credit-overlap note (“Dette emne har faglig overlapp …” to “Dette
emnet har …”), changed programme lists and added banners: 555 of its
1,801 pages gave two plans, now none. UiA’s evaluation text has four
wordings of one stock paragraph, and Volda’s examiner sentence cites a
renumbered regulation (§ 5-11, then § 9-4, then chapter 9). *Rejected:*
grouping near-identical plans by a threshold, or hashing only the
sections (the user chose exact plan ids with noise removed, 2026-10-13);
removing only the credit-overlap sentence and keeping the table (it
lists other courses, not this one’s plan). *Consequence:* 840 plans
merge into others and none split (13,861 to 13,021): NTNU 643, UiA 61,
Nord 55, UiT 33, Volda 28, OsloMet 10, UiB 10. No offering gains or
loses a plan. 48 UiB plans lose a reading list section that held only
the publication notice; UiT’s assessment sections lose the labels.
*Since:* 2026-10-13.

**UiT’s link to the university’s exam page leaves `course_plan`:** the
paragraph “Alt du trenger å vite om før, under og etter eksamen,
oppmelding, fravær klage og vitnemål: UiTs samleside om eksamen” and, on
courses being discontinued, “Her finner du mer informasjon om eksamen i
nedlagte emner”, both a link to uit.no/eksamen. A paragraph with plan
text and such a link keeps both. *Why:* by the spec link labels are
boilerplate. The first stood in no section but put “fravær” into the
`course_plan` of 683 plans; the second was in 16 assessment sections.
*Rejected:* removing the line only from the sections, as before (it
stayed in `course_plan`). *Consequence:* 693 UiT plans get a new id (880
offerings), one for one: none merge or split. Their sections keep their
text, except the label in the 16 assessment sections and a blank line
where the label stood in 652 others. *Since:* 2026-10-15.

**MF and UiS: the title and the facts box are plan text.** `course_plan`
keeps MF’s title and course code and its facts box (“Emneinfo”: code,
credits, language, programmes) and UiS’s facts box (“Fakta”: code,
credits, semesters, language), without their buttons and links (“Vis
flere”, “Timeplan”, “Kontakt studieveileder”, “Søk etter
pensumlitteratur i Leganto”); they are in no section. *Why:* by the spec
the facts box is plan text; INN’s, NTNU’s and UiA’s facts were already
in `course_plan`. MF’s pages of April 2026 name their parts differently
(`template-study-program__*`, later `template-study-subject__*`), so the
facts box and its buttons of those pages were already in `course_plan`
and opened the course content section of every MF plan, while the later
pages had no facts box. *Consequence:* all 26 MF plans and the 180 UiS
plans read from a web page get a new id (644 UiS offerings); none merge
or split, since the semester fields hold only season words, which the
plan id leaves out. MF’s course content sections lose the facts box
(median 967 → 819 characters). *Since:* 2026-10-13.

**A season that is a fact of the course stays in `course_plan`.** The
semester a course is taught or assessed in is plan text: the facts box’s
“Undervisningssemester: Høst”, “Vurderingssemester: Vår”, “Semester:
Høst og vår” (MF), UiO’s “Undervisning” and “Eksamen” fields, NLA’s
“Oppstart høst, undervisning over to semestere”, UiB’s “Haustsemesteret,
7. semester i lektorutdanningsprogrammet”, and the season of an exam
part (Volda’s “Nasjonal deleksamen haustsemesteret”, NTNU’s exam tab).
Until now a season right after a semester label left `course_plan` and
the sections as if it were a semester label (“Høst 2024”); a season with
its year, a semester code (“H23”) and an administrative academic year
still go. The plan id and the sections still leave the season out
(`.drop_semester_values()`): by the spec the semester fields of a facts
box or an exam tab change with the year while the plan stays the same,
and in a section the season says when, which is logistics. HVL’s notice
that a course is not offered goes whole also in English (“The module is
not offered in the academic year 2024/2025.”) and in the semester field
(“Emnet har ikke undervisning våren 2025.”), as the Norwegian notice
did: with only its year removed it said that the course is never taught.
*Why:* the anonymization audit of 2026-10-17 found that HVL’s semester
field was emptied in Norwegian (“Undervisningssemester: Høst” →
“Undervisningssemester:”, 1,658 plans) but kept in English (“Semester of
instruction: Autumn”, 55), and the same at NLA (“Oppstart høst,” against
“Start in the autumn,”), since the rule knew only Norwegian labels; the
user decided (2026-10-19) that a season or year that is a fact of the
course is plan text, as the spec says, and that only administrative
labels go. *Rejected:* adding the English labels to the rule, which
would have emptied the field in every language and hidden the teaching
semester from the text; keeping the season in the sections too (the
assessment semester is logistics by the spec). *Consequence:* 4,248 of
15,647 plans keep their seasons (HVL 1,688, NLA 956, UiA 712, UiB 609,
NIH 95, UiS 67, NTNU 45, UiO 45, MF 25, Volda 2, Nord 2, OsloMet 1, UiT
1); in the teacher-education frame 3,963 of 14,744. No plan id changes,
none merge or split, and no section changes. The two HVL notices are in
one plan each (PHD911 and BØA3100, 2025, outside the teacher-education
frame); PHD911 gets a new id and its course content loses the notice.
*Since:* 2026-10-19.

**An academic year that sets a condition stays.** An academic year goes
as an administrative date (“Studieår: 2019 — 2020”, “Emneplan for
studieåret 2024/2025”), but not when it is a condition of the course:
the year a course was taken in (“IKF132 …, gjennomført studieåret
2019-2020”, NLA’s credit overlap; “ikke fullførte emnet i løpet av
studieåret 2013/2014”, NTNU), a year and the ones after it (“når eksamen
i P301 er avlagt studieåret 2013-2014 eller senere”) and an exception
for one year (“Exception for academic year 2020-2021: Spring.”). *Why:*
without the year the sentence said something else (“gjennomført
studieåret.”, “Exception for academic year: Spring.”, a permanent
exception); found by the anonymization audit of 2026-10-17, and the user
decided (2026-10-19) that a year that sets a condition is plan text.
*Rejected:* removing the clause or sentence whole, which would lose the
credit-overlap rule; guarding conditions written with a season and a
year too (“studenter tatt opp på studieplanen som gjelder fra og med
høsten 2013”, UiT): they have the form of administrative dates
(“Gjeldende fra høstsemesteret 2013”, “Fra høst 2019: Endring i
hjelpemidler”) and would need a guard per wording, so they are a known
deviation of the spec (24 plans, UiT 22). *Consequence:* 5 plans keep a
year (NLA TAM301 twice, IKS104 and P320; NTNU PPU4000), and their 6
offerings get a new plan id, one for one: none merge or split. In the
teacher-education frame only PPU4000 (2014 and 2015), whose teaching
methods section keeps the year too. *Since:* 2026-10-19.

## Sections

**Seven canonical sections** are cut from each plan by its headings,
mapped through one heading table (`R/section_heading_map.R`): course
content (99 % of plans), learning outcomes (98 %), assessment (98 %),
teaching methods (94 %), coursework requirements (93 %), prerequisites
(53 %) and reading list (36 %). Where a section is missing, the coders
use the whole plan.

**Coursework requirements are kept apart from assessment:** arbeidskrav
and obligatory-activity paragraphs inside an assessment block are moved
to coursework requirements (since 2026-10-01).

**NTNU’s coursework block under the assessment goes to coursework
requirements.** NTNU lists the arbeidskrav in “Mer om vurdering”, under
a line of their own (“OBLIGATORISKE ARBEIDSKRAV”, “Obligatoriske
aktiviteter:”), while its coursework section often says only
“Obligatoriske arbeidskrav iht. emnebeskrivelsen”. The split that Nord
and UiT use is on for NTNU, and a gate heading now takes the lines under
it, and the paragraphs after it that are list items or still speak of a
gate, up to the next heading or the first line about the exam
(“Eksamen:”, “Vurderingsordning …:”, “Muntlig eksamen.”, “Evaluering”).
Any lead-in or label in the split also stops at such an exam label.
*Why:* by the spec arbeidskrav are coursework requirements also when the
page lists them under assessment; the sections audit of 2026-10-17 found
them in 876 of 2,299 NTNU assessment sections. *Rejected:* an NTNU-only
rule; taking the block to the end of the section or to the next blank
line (NTNU’s paragraphs often have none, and the exam description
followed); moving every sentence that mentions arbeidskrav (“Arbeidskrav
må være godkjent før eksamen” follows the rule as at Nord: a line that
starts with the gate moves). *Consequence:* 891 NTNU plans change (875
in the teacher-education frame): their requirements move from assessment
to coursework requirements, 7 assessment sections that held only
requirements go, and one plan gets a coursework section. NTNU assessment
sections that mention arbeidskrav fall from 876 to 146, those with a
bare “Obligatoriske arbeidskrav” line from 555 to 0; the median NTNU
assessment section falls from 403 to 267 characters and the coursework
section grows from 53 to 103. At Nord (10 plans) and UiT (16) a gate
heading now also takes the lines under it that the old rule left in
assessment (the pass/fail rule of an attendance requirement, “Studenten
er forpliktet til å … levere … vurderingsskjema”, a requirement after a
lead-in). A few NTNU plans with no paragraph breaks move one assessment
sentence along (4 found: “Masteroppgaven skrives normalt i monografiform
…”). Arbeidskrav and “Vurderingsform:” lines under NTNU’s teaching
methods stay (52 and 19 sections); the split reads only assessment.
`course_plan` and the plan ids do not change. *Since:* 2026-10-20.

**Nord’s abbreviated, counted and English gate lines go to coursework
requirements; a line naming a gate and an exam part stays in assessment
as well.** Nord also writes the gates by their abbreviation alone (“AK:
Inntil tre arbeidskrav”, “OD: Obligatorisk deltakelse, min. 80%
oppmøte”, “• AK Arbeidskrav, godkjent / ikke godkjent”), with a number
word (“To arbeidskrav. Godkjent/Ikke godkjent.”) and in English
(“Coursework requirements:”); the split now takes these, and English
gate headings and exam labels (“Compulsory assignments”, “Exam:”,
“Further on evaluation”) at NTNU and UiT. A lead-in also stops at a part
that counts towards the grade (“represents 100/100 of the grade”,
“Vektes 40 %”). A gate line that also names an exam part (“Arbeidskrav
og hjemmeeksamen (3 dager)”) or counts towards the grade stays in
assessment and is copied to coursework requirements. *Why:* by the spec
arbeidskrav are coursework requirements; the pilot sections audit of
2026-10-17 found such lines left in 14 of the 37 Nord sample plans
without a coursework section, and one plan whose only assessment line
had gone to coursework. *Rejected:* moving the line that names both
wholly (the plan loses its assessment) or leaving it wholly (coursework
loses the requirement); reading teaching methods for gate lines (2 Nord
sections: MET2000 spring 2022’s “Arbeidskrav:” block and SYK5026 autumn
2025’s OD and AK paragraphs; as at NTNU, the split reads only
assessment). *Consequence:* 248 Nord plans change (233 in the
teacher-education frame), 150 get a coursework section (138), and one
regains its assessment section. Nord assessment sections that mention
arbeidskrav or obligatory participation fall from 298 to 130, the median
Nord assessment section from 407 to 382 characters. 21 NTNU plans and 17
UiT plans change (15 UiT plans get a coursework section). `course_plan`
and the plan ids do not change. *Since:* 2026-10-21.

**Assessment holds the exam aids, the exam language and the notices
about the assessment.** A heading for the permitted aids (“Hjelpemidler
til eksamen”, “Hjelpemiddel ved eksamen”, “Tillatte hjelpemidler”) or
the exam language (“Eksamensspråk”) opens assessment, and “Fravær fra
obligatorisk aktivitet” coursework requirements; the heading stays as a
line, since the text under it is often a bare value (“Ingen”, “Alle”,
“Norsk.”). With nothing under it, or only a dash or a pointer (“-”, “Se
fagplanen.”), it goes. Rules for the use of AI (“Å generere besvarelse
ved hjelp av ChatGPT … er å regne som fusk”), plagiarism checks and
COVID changes of the exam form (“MERK: Skriftlig skoleeksamen endres til
hjemmeeksamen.”, Nord’s and UiB’s notices under the temporary
regulations) stay, and so does an exam table’s header row (Volda, INN,
UiS) and NMBU’s exam-table line. Still removed: pointers to an AI page
with no rule of their own (“Informasjon om ChatGPT/kunstig intelligens
mm. finner du her.”, HVL’s “For informasjon om bruk av kunstig
intelligens (KI) se …”), link labels (“Mer om hjelpemidler”), OsloMet’s
stock resit sentence. In USN’s text, a line that repeats an entry of the
page’s table of contents is a heading, so an unmapped one (“Annet”:
evaluation, organisation) ends the section instead of running into the
one before. *Why:* by the spec (`course_plan_spec.md`) aids and language
are assessment design, and a notice that says something about the
assessment is assessment text, stock or not; an aids value without its
heading, or an exam table’s values without the header, does not say what
it is. *Rejected:* removing every AI, plagiarism and COVID line from
assessment and coursework (2026-10-01 to 2026-10-13), which also removed
exam tasks that use ChatGPT (“Studentene skal lage et måltid ved hjelp
av ChatGPT”) and the exam forms that replaced written exams; dropping
aids and language as exam logistics; dropping “Ingen” under an aids
heading as a placeholder (it says that no aids are permitted); mapping
“Annet” to a dropped heading everywhere (it is also a type label in
Leganto reading lists). *Consequence:* the assessment or coursework text
of 6,401 plans changes, `course_plan` and the plan ids do not. 4,386
assessment sections have an aids or language heading (4 before); 556
assessment or coursework sections hold AI text (19), 171 COVID text
(none; Nord 124, UiB 47), 97 plagiarism text (none), 1,824 an exam-table
header (9). 18 plans get an assessment section, 14 of them at Nord,
where a COVID change of the exam form was the only assessment text. The
median assessment section grows from 261 to 325 characters. *Since:*
2026-10-13.

**UiT: the language label and 2008 coursework gates.** The sub-heading
“Undervisnings- og eksamensspråk” and its value (“Norsk”) on UiT’s 2018+
pages go (`.drop`); they were appended to teaching_methods on every such
page. It states a language fact, not how the course is taught (the bare
“Eksamensspråk” still maps to assessment, as above). In 2008 plans,
“Beskriving av arbeidskrav: … Beskriving av eksamen: …” and a numbered
point on “godkjente oppgaver” now move from assessment to
coursework_requirements like other inline gates. *Rejected:* mapping the
heading to teaching_methods or assessment (it is neither).
*Consequence:* 2,190 UiT teaching_methods sections change (median 532 to
460 characters, all frames), 4 plans get a coursework section from the
“Beskriving” rule and 1 from the numbered point; Nord and NTNU do not
change. *Since:* 2026-10-22.

**HVL: exam logistics and costs leave the sections.** A sentence goes
from any HVL section when it points to Studentweb or says where the
exam’s time, place or deadline will be announced (“Tid og sted for
eksamen blir opplyst på Studentweb.”, “Innleveringstidspunkt blir
oppgitt på Studentweb og digitalt eksamenssystem.”, “Tid og stad for
eksamen blir gitt på Canvas.”), when the exam is held (“Eksamen blir
avholdt i starten av januar.”) or what the student pays (“Kr 400 i
materialpenger betales inn ved studiestart”, “Materialkostnader (kr 500)
og ekskursjoner må dekkes av studentene.”, an excursion’s “egenandel” or
price); the rest of its line stays, and so does a sentence that also
names a coursework requirement. `course_plan` keeps them all. *Why:* by
the spec exam logistics and costs are plan text in no section; the
sections audit of 2026-10-17 found them in HVL’s assessment and teaching
methods, under the section’s own heading, where the `.drop` headings
cannot catch them. *Rejected:* removing every sentence on costs
(“forventet kostnad, se semesterplan”, “Ekskursjoner (studentene må
dekke egne kostnader)”), which would take coursework requirements and
teaching methods with them, and economics content (“kostnadsanalyse”);
removing the sentences in `course_plan` too. *Consequence:* 557 sections
of 541 HVL plans lose 700 sentences: 249 on Studentweb or announcements
(247 in assessment, 2 in course content on closed registration), 448
cost sentences (course content 90, coursework requirements 99, teaching
methods 259) and 3 exam dates; no section is left empty and
`course_plan` and the plan ids do not change. In the teacher-education
frame 527 sections of 515 plans. *Since:* 2026-10-19.

**Practicum stays in teaching methods** rather than a section of its
own: only HiOF and Nord give practicum its own heading (since
2026-10-01).

**Admission text is not prerequisites** (opptakskrav, study-right
sentences; since 2026-10-01), and credit-overlap boxes are dropped.
Since 2026-10-15 the rule works by sentence, also under a prerequisites
heading: a sentence goes if it only gives admission or study right
(“Forutsetter opptak til femårig lektorutdanning …”, “Emnet er
forbeholdt studenter på …”, “Kan tas som enkeltemne.”), points to the
admission rules (“Som opptakskrav til studieprogrammet, se studiets
studieplan.”, “Se fagplanen under Opptakskrav.”) or says there is
nothing beyond admission (“Ingen utover opptakskravet.”). So does a
programme list such a sentence introduces, and a label left with nothing
under it (“Forkunnskapskrav:”). A sentence that also names a
prerequisite (a course code, “bestått”, credits, “bør”, “anbefales”)
stays whole (“Opptak til femårig lektorutdanning og beståtte eksamener i
henhold til forskrift.”). *Why:* by the spec prerequisites hold prior
knowledge, not admission. The rule of 2026-10-01 dropped admission
headings and whole lines with an admission phrase from a short list, so
most admission under a prerequisites heading stayed, and a line with
both lost both (“Opptak til studieprogrammet. Fullført Pedagogikk og
elevkunnskap 1A.”): in `data-2026-10-15`, 1,098 of 7,863 prerequisites
sections mentioned “opptak” (NTNU 695, Nord 264), most of them only
admission. *Rejected:* removing every sentence that mentions admission
(the mixed ones carry a progression rule); the line as the unit.
*Consequence:* against `data-2026-10-15`, 442 prerequisites sections go
(NTNU 201, Nord 200, OsloMet 13, USN 13, NLA 11, UiS 4) and 327 change;
11 regain text the old rule removed along with an admission sentence on
the same line (“Studiets fire første år må være gjennomført”). Plans
with a prerequisites section: Nord 915 → 718, NTNU 1,815 → 1,622, NLA
122 → 111, OsloMet 83 → 70. 456 sections still mention “opptak”, 334 of
them NTNU’s mixed sentence. `course_plan` and the plan ids do not
change. *Since:* 2026-10-15.

**Placeholders are dropped** (“Ingen”, “-”, “Se fagplanen.”,
reading-list pointers; since 2026-10-01).

**Prerequisites written inside the course description are
prerequisites:** a “Forkunnskapskrav” label in course content (UiT’s “Om
emnet”, two MF courses) takes the paragraph it names to prerequisites.
At UiT that paragraph is almost always “Jf. opptakskrav og
progresjonskrav i studieplanen …”, a pointer to the programme’s
admission rules, which is dropped like other admission text, so most UiT
plans lose it from course content without gaining a prerequisites
section. *Rejected:* treating the label as a sub-heading (the course
description continues after the pointer and would be filed as
prerequisites). *Since:* 2026-10-07.

**Nord’s short description is course content:** the lead paragraph above
the accordions (pages from 2019 on) opens course content, without the
title and course code above it; “Se kursinnhold.” there is a
placeholder. A paragraph of 40 or more characters that a section repeats
is kept once (Nord’s lead is often the start of “Beskrivelse av emnet”;
UiS PDF plans repeat their lead sentence). *Rejected:* filing all text
before the first heading as course content (would take in the title and
code). *Since:* 2026-10-07.

**OsloMet teaching methods that only say “Se fagplanen.”** take the text
of the subject’s Fagplan on the same page (“Fagets arbeids- og
undervisningsformer”), marked “Se fagplanen. Fagplanen sier: …”: 114
plans. *Why:* the emneplan delegates the teaching methods to the subject
plan, which applies to the course. *Since:* 2026-10-06.

**OsloMet coursework requirements take the Fagplan’s rules for
coursework and attendance:** when the page holds the subject’s Fagplan,
its parts on coursework and attendance (“Arbeidskrav”, “Obligatorisk
deltakelse”, “Faglig aktivitet med krav om deltakelse” and the like)
join the course’s own coursework section after “Fagplanen sier:”, up to
the label of the next part (exam forms, suitability, grading): 248
plans. *Why:* the Fagplan sets these rules for all its courses
(deadlines, valid absence, resubmission, usually 80 % attendance with
compensatory work), and the course’s section often points to it
(“beskrevet i den innledende delen av fagplanen”). Before, they were
only in `course_plan`: in 167 plans, the absence rules were in no
section. *Rejected:* keeping them out of the sections as text not
written for the course (the coders read the sections); giving a course
without coursework of its own a section from the Fagplan alone
(practicum courses whose sections only point to the Fagplan keep none,
below). *Since:* 2026-10-15 (user decision).

**OsloMet practicum courses (M1GP/M5GP) whose sections only say “Se
fagplanen.” have no sections** (45 of 85 practicum plans; the others
have text of their own and get its sections): the page’s Fagplan is the
programme’s practicum plan for all study years, with no markup
separating the years. Their plan text keeps the whole page, which the
coders read where a section is missing. *Rejected:* reading the whole
practicum plan into each course, or slicing it by year (fragile).
*Since:* 2026-10-02.
