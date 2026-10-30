# Institution-Specific Notes


**NIH.** The page is `/studier/emner/{year}/{host|var}/{code}.html` with
the code in lower case and the Norwegian letters written as ASCII
(KRØ100 → `kro100.html`; `add_course_url_nih()`). The year indexes
(`/studier/emner/{year}/{host|var}/`, paged with `?page=n&u-page=n`)
list only the courses that have a page: 5 for 2021, 22 for 2022, 163 for
2025. A 404 for a course the index of its year does not list is final:
NIH has no page for it (`data/data_notes.qmd`, NIH).

**NMBU.** The page is `https://www.nmbu.no/emne/{CODE}`, with the code
without Norwegian letters (MILJØ300 is `MILJO300`; with Ø, raw or
percent-encoded, the site answers 404; since 2026-10-22). Only the plan
in force is published (`plan_years = "current"`). A course whose plan is
only in English has a Norwegian page whose “Om dette emnet”, and every
accordion, says only “Se engelsk emnebeskrivelse” or “See English text”;
the harvest then fetches the English page,
`https://www.nmbu.no/en/course/{CODE}` (`english_page` in the config, as
at HVL, Nord and NTNU), which has the same layout with English headings,
and the row keeps the Norwegian URL. A pointer page in the raw store
gives no plan, and NMBU’s snapshot is once a year, so fetch it again
with `harvest_all(courses, institutions = "nmbu")` for those offerings
alone.

**NORD.** The URL needs Norwegian characters in the semester parameter
(`HØST`/`VÅR`); ASCII `HOEST` falls back to a generic “Gjeldende
emnebeskrivelse” page without year-specific content. The site maps each
semester to its academic year (`VÅR&year=2023` gives the 2022/23 plan).
For a year it has no plan for, it shows the current plan, headed
“Gjeldende emnebeskrivelse (sist oppdatert 2026/27)”; `page_term` in the
config reads that year (as its autumn), and an offering whose page shows
a later year gets no plan. Pattern:
`https://www.nord.no/studier/emner/{code}?year={year}&semester={HØST|VÅR}`.
A course taught in English has a Norwegian page that only says so
(“Emnet foregår på engelsk. Se engelske nettsider …”); the harvest then
fetches the English page, `/en/studies/courses/{code}` with the same
query, and the row keeps the Norwegian URL (`english_page` in the
config; HVL likewise, from
`/en/studies-at-hvl/study-programmes/courses/`).

**NTNU.** The URL has the year but not the semester. The year is the
academic year the page is for (`/2025` is “Studieår 2025/2026”), so a
spring or summer offering reads the year before
(`url_year = "academic"`; HVL and OsloMet likewise), and
`read_harvest()` gives an offering the page at its URL whichever
offering it was fetched for. DBH’s “-A” codes (MGLU1101-A) are the
site’s code without the suffix. A page saying no information is
available raises `ntnu_no_info_error`. robots.txt asks for a 10-second
crawl delay (`request_delay` in the config). The page’s exam tab
(`#omEksamen`: assessment form, grading scale, exam sessions and their
parts) is read without its link, rooms, comments, dates and times; the
assessment section gets one session of it (`exam_sessions`). The
timetable tab (`#timeplan`, from 2025) is not read. A course whose plan
is only in English has a Norwegian page whose course content says only
“See English version.”, “Se engelsk beskrivelse.” or “Se engelsk
tekst.”; the harvest then fetches the English page,
`https://www.ntnu.edu/studies/courses/{code}/{year}` (`english_page` in
the config, as at HVL and Nord), and the row keeps the Norwegian URL.
Every page has a link titled “English version” in its template, so the
notice is the course content paragraph, not the link. A pointer page in
the raw store gives no plan, so `harvest_all(retry = "ok")` fetches it
again.

**UiO.** UiO publishes only the latest version of each course plan: the
base URL always returns the current plan and older versions cannot be
reached. Semester URLs (`/h24/`, `/v25/`) hold logistics only (teachers,
timetable, exam dates), not the plan, so they are not used. Data should
be filtered to the current year. The URL holds a faculty and department
folder that DBH does not give:
`https://www.uio.no/studier/emner/{faculty}/{department}/{CODE}/`.
`resolve_urls_uio_batch()` reads the folders from UiO’s sitemap
(`uio_course_folders()`), which lists every course taught now (the same
courses as the listing `/studier/emner/alle/`). A course not taught now
keeps its page but is not listed: it gets the folder that most listed
courses with its code prefix (`ENG`) have, else the one of its DBH
department (`department_code`); a wrong guess is a 404. A few folders
are named in lower case (`sv/iss/sos9009`), and the URL must be too; Ø
is O (`MØNA2504` is in `hf/ikos/MONA2504/`).

**UiB.** A page is
`https://www4.uib.no/studier/emner/{code}?start_semester={year}{V|H}`
(the code in lower case, a space as `%20`; summer is H). A course has
one page with a semester picker back to 2004; a course UiB no longer
offers, a master’s thesis, a UNIS course and most PhD courses have no
page in any semester (404). UiB’s site also answers 404 now and then for
pages it has: in the pilot of 2026-10-17, 29 of the 74 course-years that
answered 404 gave a page on 2026-10-21. After a harvest and
`tar_make()`, run
`harvest_all(institutions = "uib", retry = "not_found")` once.

**HiOF.** A page is
`/studier/emner/{folder}/{year}/{var|host}/{code}.html` (one page per
semester, the code in lower case), in a faculty or department folder DBH
does not give: `lusp/{lusp,pil,res,slik}/`, `hvo/{shb,vlo}/`,
`iio/{itk,ing,ois}/` (from 2021), `nta/` (2009–2021), `videre/` (from
2008), `fellesemner/` (from 2022), and
`historiske-emner/{hv,ir,it,lu,oss}/` for courses no longer taught
(2005–2024, the faculties before 2021). A course taught in English may
have its page only in the English tree,
`/english/studies/courses/{folder}/{year}/{spring|autumn}/{code}.html`
(the same folders, `historical-courses/` for `historiske-emner/`): the
Master in Green Energy Technology (IRMGR), Applied Computer Science
(ITI), the PhD courses (PHDDS, `iio/iio/`) and the Academy of Performing
Arts (`nta/`, with pages for 2024 and 2025); the English pages have the
Norwegian layout. The folder follows the site, not DBH’s department
(hsb1209, a health code, is in `ir/`), so `resolve_urls_hiof_batch()`
(url_discovery) lists the year folders of the harvest’s years in both
trees (`hiof_course_index()`: about 20 pages a listing page, some 1000
requests for all years, 70 more for two years of the English tree) and
gives an offering the page of its code, year and semester; of several,
the teacher-education URL that `add_course_url_hiof()` builds
(`historiske-emner/lu/` before autumn 2021, `lusp/lusp/` after), then a
Norwegian page, then one outside the historical folders. A course over
two semesters has one page, under its autumn folder, titled “(Høst
2013–Vår 2014)”: a spring offering without a page of its own takes its
course’s page of the autumn before, and the title’s semesters
(`page_term`, `page_end_term`) give it the plan only when the page is
for its spring too. Most offerings still without a page are the other
semester of a course DBH registers in both, or a code the site has no
page for in that year (an old code version DBH still registers, a course
not taught that year). An offering with no page gets no URL, unless the
listing of its teacher-education year folder failed: then it gets that
URL. The config selector lists fallbacks for the old and new page
layouts.

**USN.** The URL holds a version number that metadata cannot give:
`https://www.usn.no/studier/studie-og-emneplaner/#/emne/{CODE}_{VERSION}_{YEAR}_{SEMESTER}`.
Each version (1, 2, 3, …) is a revision valid for a range of years, and
an invalid version/year combination silently shows another page without
changing the URL. `resolve_urls_usn_batch()` therefore renders each
candidate in Chrome (`rvest::read_html_live()`, hash routing) and checks
that the displayed “Undervisningsstart” semester matches (“Course start
autumn 2022” on the English page of a course taught in English,
`usn_page_term()`). It tries versions 1-3 and stops early when the
course code is missing from the page (no such version) or version 1
starts after the requested year (course did not exist yet); NA means no
version exists for that semester. The content is rendered inside Shadow
DOM, which `html_text()` cannot read; `read_usn_live_html(session)`
walks the shadow roots with JavaScript:

``` r
session <- rvest::read_html_live(url)
Sys.sleep(5)  # wait for JS rendering
content <- read_usn_live_html(session)
session$session$close()
```

The batch resolver reuses one Chrome session and navigates by changing
`window.location.hash`, which avoids a 2-3 s browser start per URL. It
waits the config’s `request_delay` after each page (5 seconds, the
Crawl-delay of USN’s robots.txt) before it reads it and asks for the
next. A semester without a plan of its own shows another semester’s plan
under the banner “Emneplanen for dette semesteret er ikke klar ennå”.
When the next candidate shows the same plan, the banner stays until the
page is reloaded, so a page with the requested year and the banner is
reloaded. HTML is captured during resolution, so USN has no separate
fetch step.

**UiT.** Historical plans need a document id:
`https://uit.no/utdanning/emner/emne?p_document_id={ID}`. The active
page (`/utdanning/aktivt/emne/{CODE}`) shows the course’s latest
document and has a `<select>` (“Andre år og semester”) listing the other
semesters with their document ids (option text `{CODE}: H 2020` /
`{CODE}: V 2021`), also for discontinued courses. The shown document is
not in the list: its semester is under “Semester / år” and its id only
in the login link (`securerefresh?p_url=...?p_document_id=`);
`uit_semester_documents()` adds it (before 2026-10-21 it was missed,
e.g. every course whose latest document is autumn 2025). A semester
listed twice can be an English document (“Course code:”) beside the
Norwegian one; `uit_one_per_semester()` fetches those and keeps the
first Norwegian one. The same CSS selector works for active and
historical pages.

**Samas.** `noop` strategy: `extracted_text` is NA, since the plans are
in Sámi.
