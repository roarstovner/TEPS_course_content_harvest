# Institution-Specific Notes


**NIH.** The page is `/studier/emner/{year}/{host|var}/{code}.html` with
the code in lower case and the Norwegian letters written as ASCII
(KRØ100 → `kro100.html`; `add_course_url_nih()`). The year indexes
(`/studier/emner/{year}/{host|var}/`, paged with `?page=n&u-page=n`)
list only the courses that have a page: 5 for 2021, 22 for 2022, 163 for
2025. A 404 for a course the index of its year does not list is final:
NIH has no page for it (`data/data_notes.qmd`, NIH). A course taught in
English may have its page only in the English tree,
`/english/studies/courses/{year}/{spring|autumn}/{code}.html` (THP111,
LKI152, SPM123; from 2023), found as UiA’s below.

**UiA.** The page is `/studier/emner/{year}/{host|var}/{code}.html`, the
code in lower case without DBH’s variant suffixes `-G`, `-G2` and `-D`
(`add_course_url_uia()`; `data/data_notes.qmd`, UiA). A course taught in
English may have its page only in the English tree,
`/english/studies/courses/{year}/{spring|autumn}/{code}.html` (MF-102,
ME-112, ORG208, IS-100: 404 on the Norwegian path; from 2020). The
English tree also holds a translation of most courses with a Norwegian
page: some 1,600 English pages a year at UiA (2020-2026; 50 at NIH), and
of UiA’s 11,694 English pages of 2020-2027, 7,779 have a Norwegian page
that answered 200 (read 2026-11-23). So
`resolve_urls_english_tree_batch()` (url_discovery, UiA and NIH, since
2026-11-23) gives every offering the Norwegian URL, and the English page
only where the Norwegian URL answered 404 when last requested
(`raw_not_found_urls()`, from the raw store) and the English tree’s
listing of the offering’s year (`vortex_course_index()`, read only then)
has a page of its code in its semester. A course-year new to the harvest
thus gets the Norwegian URL, and its English page with the next retry of
pages not found (`retry = "not_found"`). The Norwegian listings are not
read: UiA’s stop at 500 pages, so they cannot show that a page is
missing. The English pages have the Norwegian layout with English labels
(“Course Leader:”, “Teaching and learning methods”, “Examinations”) and
read as the Norwegian ones (since 2026-11-23): UiA’s “Student
evaluation”, “Offered as a free-standing course” and “Admission for
external candidates” are facts, as “Studentevaluering”, “Tilbys som
enkeltemne” and “Tilgang for privatister” are; “Course Leader” is not
read, as “Emneansvarlig” is not (`exclude`; at UiA both since then: the
anonymizer had left names with an apostrophe or a lower-case particle on
English pages), nor UiA’s FS stamp “Last updated from FS …” (the
anonymizer removes only the Norwegian one); and the headings map as
their Norwegian counterparts: UiA’s “Examination requirements” (“Vilkår
for å gå opp til eksamen”) to coursework requirements, not assessment,
and “Admission Requirement if given as a free-standing course”
(“Opptakskrav hvis tilbudt som enkeltemne”) out of the sections; NIH’s
“Assessment/exam”, “Prerequisite” and “Core literature”
(“Vurdering/eksamen”, “Forkrav”, “Kjernelitteratur”) to assessment,
prerequisites and reading list. robots.txt disallows only `/vrtx` at
both.

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
offering it was fetched for. DBH’s codes with a one-letter variant
suffix (MGLU1101-A, SOL420-B, BØA2020-C, LVUT8005-K; before 2026-11-22
only -A) are the site’s code without the suffix. A page saying no
information is available raises `ntnu_no_info_error`. robots.txt asks
for a 10-second crawl delay (`request_delay` in the config). The page’s
exam tab (`#omEksamen`: assessment form, grading scale, exam sessions
and their parts) is read without its link, rooms, comments, dates and
times; the assessment section gets one session of it (`exam_sessions`).
The timetable tab (`#timeplan`, from 2025) is not read. A course whose
plan is only in English has a Norwegian page whose course content is
only a pointer to the English page, in some 60 wordings (“Se engelsk
versjon”, “Emnet undervises på engelsk. Se engelsk versjon for
emnebeskrivelse.”, “Se emnets engelske nettside – International MSc”,
“Cf. English description”; some 1,900 course-years of 2019-2025,
2026-11-05); the harvest then fetches the English page,
`https://www.ntnu.edu/studies/courses/{code}/{year}` (`english_page` in
the config, as at HVL and Nord), and the row keeps the Norwegian URL.
Every page has a link titled “English version” in its template, so the
notice is the course content paragraph, not the link. A pointer page in
the raw store gives no plan, so after `tar_make()`
`harvest_all(retry = "ok")` fetches it again (with
`institutions = "ntnu"` and the years, and not while an NTNU harvest
runs: a retry removes the institution’s checkpoints).

**HVL.** A course whose plan is only in English has a Norwegian page
whose content is only a pointer to the English page, its first
paragraph, in some 30 wordings (“Emnebeskrivelsen finnes kun på engelsk,
gå til …”, “Dette emnet er kun skildra på engelsk. Klikk på”English”
…“,”Dette emnet kan du lese om på dei engelske nettsidene våre.”, “Se
engelske nettsider for mer informasjon”, “You can read about this course
on our English website.”; some 250 course-years of 2017-2025,
2026-11-08); the harvest then fetches the English page,
`/en/studies-at-hvl/study-programmes/courses/{year}/{code}`
(`english_page` in the config), which has the same layout, and the row
keeps the Norwegian URL. A pointer followed by a plan in the same
content (a Norwegian plan after “For English course description, choose
…”) is not the notice. A pointer page in the raw store gives no plan, so
after `tar_make()` `harvest_all(institutions = "hvl", retry = "ok")`
fetches it again (not while an HVL harvest runs). The facts’ teaching
semester may say when a new course is first taught
(“Undervisningssemester: Høst. Undervises første gang høsten 2023.”,
“Semester of instruction: Autumn Semester - first time autumn 2023”), on
the pages of the academic years before it too: an offering before that
semester gets no plan (`page_term`; 21 offerings of 2021-2025, checked
2026-11-09).

**UiS.** The archive PDFs (`/Stine/EMNE/{year}/{HØST|VÅR}/`) often say
which study year they are for (“Dette er studietilbudet for studieår
2020-2021.”, “Vi gjør oppmerksom på at dette er studieinformasjon for
studieår 2018-2019.”). The pipeline does not read it: where it disagrees
with the offering’s academic year (spring offerings take their academic
year’s page, `.menu_year_pages()`), it is a line left from an earlier
year (MGL2071’s plans of 2019-2022 keep the 2018-2019 line in the
introduction, beside the right year from 2020; LENG116’s and LNOR176’s
PDFs of autumn 2015, headed “2015 HØST” and different from 2014’s, keep
2014-2015; checked 2026-11-09).

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
(the code in lower case, a space as `%20`, the Norwegian letters as
ASCII: ITØK264 is `itok264`, LÆFR101 `laefr101`, since 2026-11-22;
summer is H). www4 redirects to `www.uib.no` (seen 2026-11-22). A course
has one page with a semester picker back to 2004; a course UiB no longer
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
trees (`vortex_course_index()`: about 20 pages a listing page, some 1000
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

**HiMolde.** Høgskolen i Molde’s site is laid out as HiOF’s (FS pages in
Vortex): a page is
`/studier/emner/{folder}/{year}/{var|host}/{code}.html`, in a department
folder DBH does not give (`os/`, `log/`, `hs/`, `euv/` for continuing
education; `hogskolen-i-molde/` from 2026), from autumn 2018 (checked
2026-10-31; for 2017 only two pages, `log/2017/host/mar300.html` and
`mar301.html`). A year folder may list only its semester folders
(`os/2025/`), which `vortex_course_index()` then reads. A page is named
by the code without Norwegian letters (BØK705 at `bok705.html`). A
course taught in English may have its page only in the English tree,
`/english/studies/courses/{folder}/{year}/{spring|autumn}/{code}.html`
(ADM120). `resolve_urls_himolde_batch()` (url_discovery) reads the year
folders of the harvest’s years, and of the year before a spring
offering’s, in both trees with `vortex_course_index()`, and matches them
as HiOF’s (`vortex_match_urls()`, without a built URL): a course over
two semesters has one page under its autumn (“LOG954 Masteroppgave (Høst
2022–Vår 2023)”), and the title’s semesters (`page_term`,
`page_end_term`) decide whether it serves a spring offering. robots.txt
disallows only `/vrtx`.

**NHH.** Norges Handelshøyskole shows only the plan in force
(`plan_years = "current"`): one page per course, `/emner/{slug}/`, named
from the course’s title (`/emner/finansregnskap-i/`,
`/emner/forecasting/`), and for a course taught in English the same page
at `/en/courses/{slug}/` (checked 2026-10-31). The slug changes with the
title, so the sitemap’s URLs answer 404
(`/en/courses/financial-accounting-i/` is now
`financial-accounting-i-e/`), and the course lists
(`/en/for-students/courses2/`) cover only the courses offered now.
`resolve_urls_nhh_batch()` (url_discovery) searches each code once with
the Norwegian site search, `/sok/?q={code}`, which lists the course
pages of both languages with their code (“MRR451 (7,5 Stp)”), and takes
the page of the exact code (not “MRR451-OSL”), one not marked
“(expired)” or “(not offered)” first. The page shows the version of the
current semester (“Høst 2026”); a course taught in spring links to the
next semester’s version (`?displayNextTerm=true`, “Vår 2027”), which the
harvest does not fetch. The NHH Executive courses (AFA, MAB, SOL …) have
no course page. robots.txt disallows only `/paraplyen`.

**NMH.** Norges musikkhøgskole publishes a study handbook per cohort,
`https://nmh.no/studiehandboker/startkull-{year}/`, from 2021 (checked
2026-10-31; no earlier handbook is on the site), each with a page per
course named from its title: `/emner/{slug}` (`/emner/akkompagnement`,
`/emner/masterarbeid-2`) and, from 2022, the electives’ catalogue,
`/valgemner*/{slug}` (`/valgemner-2026-27/alexanderteknikk`). The page
gives the code in its facts box, and the handbook’s course lists give
each page’s code (“AKKOMP10 - Akkompagnement”, “BARENS20/21 -
Barokkensemble I og II”). `resolve_urls_nmh_batch()` (url_discovery)
finds the lists in the study-plan sitemaps
(`sitemaps-1-section-studyplans_content-1-sitemap-p*.xml`) and reads
them (about 25 requests a harvest year). An offering takes the page of
its code in the handbook of the cohort that starts in its academic year
(spring 2026: `startkull-2025`), else in the latest earlier handbook
that lists the code. Of two pages of a code in one handbook (56 such
codes, mostly in 2021 and 2022), the one listed for the code alone
(“OPERA21 - Operaseminar II”) goes before one listed for several
(“OPERA20/21”), else the first listed (the second is the same text or a
later cohort’s copy; checked 2026-11-16). The handbooks are copies of
one set of plans, each cohort’s for all its years of study: a course of
the second year in `startkull-2026` is taught in 2027/28. Ensembles and
orchestra projects registered per number of projects (BLENS21-BLENS27,
KAKOR20-KAKOR27, OSINF60-OSINF72, BFO20-BFO24, CBE20-CBE32,
JAENS20-JAENS29) take their series’ page by code stem (BLENS20, KAKOR,
BFO, CBE, JAENS; OSINF has none), only these series (`NMH_SERIES`, user
decisions 2026-11-14 and 2026-11-20). Many DBH codes have no page:
exchange students’ courses (UTV\*) and others, about half the codes of
2025. robots.txt (nmh.no, www.nmh.no redirects there) disallows only
Craft CMS’s `/cpresources/`, `/vendor/` and `/cache/`.

**KHiO.** Kunsthøgskolen i Oslo shows only the plan in force
(`plan_years = "current"`): a page per course version,
`https://khio.no/emner/{code}-{version}`, DBH’s code with its version
number in lower case (`/emner/kk202-4`, `/emner/cdpr528-1`; checked
2026-11-01). The code without a version, or one that does not exist,
answers 404. The page has no year and shows the version’s plan as it is
now. A version no longer taught keeps its page with the title, the facts
list and the plan’s headings, and no text under them (`/emner/kk202-3`,
`/emner/kk202-1`): `fetch_html_cols_single_khio()` treats it as a
missing page (“Course version without a plan (soft 404)”, outcome
`not_found`). The study programme pages,
`/studieprogrammer/{programme}`, list the course pages. A course taught
in English has a full Norwegian page and an English one at
`/en/courses/{code}`; the harvest reads the Norwegian one. The plan’s
headings are h2s (“Kort beskrivelse av emnet”, “Læringsutbytte for
emnet”, “Undervisning og læringsformer”, “Vurdering”), the coursework
requirements under a paragraph sub-heading (“Arbeidskrav for emnet
er:”). robots.txt has only comments.

**Fjellhaug.** Fjellhaug Internasjonale Høgskole shows only the plan in
force (`plan_years = "current"`), one page per course code on
`fih.fjellhaug.no` (Craft CMS): `/emne/{slug}`, or for most courses
taught in English only `/en/subjects/{slug}` (BMM, TMM). The slug is the
code in lower case (`/emne/krle1001n`), for a few courses followed by
the title (`/emne/btm2003n-markusevangeliet-10-ects-stp`); the bare code
of such a course answers 404 (checked 2026-11-05).
`resolve_urls_fjellhaug_batch()` (url_discovery) reads the subject
sitemaps, Norwegian and English
(`sitemaps-1-section-subjects-1-sitemap.xml`,
`/en/sitemaps-1-section-subjects-2-sitemap.xml`; about 170 pages), and
matches the code at the start of the slug; of a Norwegian and an English
page of one code, the Norwegian. DBH gives the courses taught at the
Danish campuses (Afdeling København, Aarhus) the suffix `-DK1` or
`-DK2`, and some variants `-IS` or `-EVU` (`BTM2008-DK1`, `BMM1001-IS`):
they take the page of the code without it, whose facts box names the
campuses (“Aarhus, København”). The page holds the plan twice, for wide
screens (`h4` headings next to a menu of the parts) and in accordions
for phones; the harvest reads the first and leaves out the menu and the
accordions. Earlier plans are only in PDFs: the programmes’ study plans
per semester from 2007 and a course catalogue for autumn 2026, listed on
`/om-fih/arkiv-studieplaner`, which the harvest does not read.
robots.txt disallows only Craft CMS’s `/cpresources/`, `/vendor/`,
`/.env` and `/cache/`.

**VID.** VID vitenskapelige høgskole has one page per course,
`https://www.vid.no/studier/emner/{slug}-{code}`, the slug made from the
title and the code in lower case, a space as a hyphen
(`/studier/emner/team-og-prosjektledelse-maverd-543`, `...-phd-dvp-902`;
checked 2026-11-01). A few codes have a second page, with a numeric id
after the code (`...-basos1020-1795878807970940938`), for another
programme or semester. The sitemap, `https://www.vid.no/sitemap.xml`,
lists every course page (717), and `resolve_urls_vid_batch()` matches
the offerings’ codes to the URLs’ ends; of a code’s several pages it
reads each once and takes the one with a plan for the offering’s
semester. A code that ends no URL takes the pages that end in it once a
suffix of one or two letters is stripped from both (BASOS3030-H:
`...-basos3030`, BAVER2040: `...-baver2040-d`; since 2026-11-22), which
may be another campus’s. The page is rendered in the browser (Enonic XP,
react4xp) from JSON in a script tag (`data-react4xp-ref="root"`): the
course’s title, code, credits, language, duration and programmes, and
`plans`, one per year and semester the course is taught in, from spring
2024 to the next academic year (many spring 2024 entries have no text).
The HTML the server sends has the facts box and an empty semester
picker, no plan. The harvest stores the page; the config’s `html_fn`,
`vid_plan_html()`, gives the block reader the plan of the offering’s
year and semester as HTML (title, facts list, an h2 per paragraph and
accordion: “Emnets innhold”, “Hovedtemaer”, “Læringsutbytte”, “Arbeids
og undervisningsformer”, “Obligatoriske aktiviteter”,
“Vurderingsordning”, …). The plan’s facts-box items (“Emnetype”,
“Obligatorisk undervisning”, “Totalt antall studentarbeidstimer”,
“Tilbys som enkeltemne”, “Opptak til enkeltemne”; `vid_fact_items`) join
the facts list instead, so they are in `course_plan` but in no section
and not in the plan id. A page with no plan with text counts as not
found (`fetch_html_cols_single_vid()`). robots.txt allows everything for
all user agents and blocks AI crawlers by name.

**PHS.** Politihøgskolen’s course pages are FS pages in Vortex, laid out
as HiMolde’s but without department folders:
`https://www.politihogskolen.no/studier/emner/{year}/{var|host}/{code}.html`,
the code in lower case with the Norwegian letters as ASCII
(`avhorutd.html` for AVHØRUTD) and a dash kept (`ma-910.html`,
`forebygg01-2.html`), though a few drop the dash or write æ as e
(`elkrise.html` for EL-KRISE, `ledlerutv.html` for LEDLÆRUTV; checked
2026-11-03), and an English tree,
`/english/studies/courses/{year}/{spring|autumn}/`, for the courses
taught in English (NCFI861-\*), whose pages hold the title (“NCFI861-1
null (Spring 2025)”), the facts and the programmes and no plan:
`fetch_html_cols_single_phs()` counts a page without plan text as not
found. The year folders run from 2022 to 2028 (checked 2026-11-02). The
folders’ listings hold every page, and an offering they do not list
answers 404 (checked again 2026-11-03: the sitemap, the site search, the
programme pages and 40 unresolved codes probed in every year folder
found no course page outside the listings), so
`resolve_urls_phs_batch()` reads them (`vortex_course_index()`,
`vortex_match_urls()`). A course over two semesters is filed under its
autumn, with both semesters in its title (“(Høst 2025–Vår 2026)”,
`page_term` and `page_end_term`). The plan sits beside the title and
facts list in `#vrtx-content`, not inside `#vrtx-fs-emne-content` as at
HiMolde. The headings are h2s: “Innledning”, “Læringsutbytte”,
“Kunnskap”, “Ferdigheter”, “Generell kompetanse”, “Organisering og
læringsaktiviteter”, “Obligatorisk aktivitet”, “Vurdering”. Programme
study plans
(`/studier/master-etter-og-videreutdanninger/{programme}/studieplaner/`,
HTML or PDF) are not read. robots.txt disallows `/vrtx`.

**FHS.** Forsvarets høgskole’s course pages are on Forsvaret’s site,
`https://www.forsvaret.no/utdanning/emner/{CODE}/{year}-{HØST|VÅR}` (the
code without DBH’s version suffix, the semester in capitals and
percent-encoded), one page per version of the plan, for the semester the
version starts in, from autumn 2018 (checked 2026-11-04). A course
taught every year may have versions for some years only (MILM1701:
autumn 2020, 2021, 2023, 2025). The code’s page,
`/utdanning/emner/{CODE}`, redirects to the latest version, which lists
them all in its “Emnehistorikk” menu; a code without pages answers 404.
`resolve_urls_fhs_batch()` reads each code’s page once and gives an
offering the latest version that starts in or before its semester, the
one in force. The page is server-rendered HTML: the title, a facts box
(credits, academic year, programmes, English title), “Hva lærer du”
(paragraphs with bold sub-headings: “Innledning”, “Emnets temaer”) and
accordions with h3 headings (“Læringsutbytte”, “Læringsaktiviteter”,
“Pensum”, “Arbeidskrav og vurdering”, “Utfyllende informasjon om
emnet”). The programme pages (`/utdanning/utdanninger/{slug}`) link the
course pages of their current versions; their study plans are not read.
Forsvaret’s site has no robots.txt (404) and its sitemap answers 500.

**DMMH.** Dronning Mauds Minne Høgskole’s course catalogue runs on INN’s
catalogue software (Edutorium, Drupal 7): a page per course and
semester, `https://studier.dmmh.no/nb/emne/{CODE}/{year}-{host|var}`,
DBH’s code as it is, built by `add_course_url_dmmh()` from autumn 2014
(the first semester of the catalogue’s menu). www.dmmh.no’s study-plan
links redirect there; the URLs go to studier.dmmh.no directly. A
semester without a version of its own shows the latest version, with or
without the notice “The course description for the semester you wanted
is not published yet”; `page_term` reads the version’s first teaching
semester in the facts list (“Undervisningssemester 2025 høst, 2026 vår”;
“Teaching semester” on an English page), as at INN. A code without any
page redirects to the course search
(`/nb/emne?sem=2017-autumn&title_search=...`), which
`fetch_html_cols_single_dmmh()` counts as not found. The page is server
rendered: the title, the facts list, a table of contents, h2 sections
(“Emneinnhold”, “Læringsutbytte”, “Pensumliste”, “Arbeids- og
læringsaktiviteter”, “Karakterskala”, “Informasjon om eksamen/vurdering
i emnet”) and two tables without an h2, coursework and assessment, each
twice: a wide copy and a narrow one under a `div.label` (“Obligatoriske
arbeidskrav:”, “Vurderinger:”), of which the narrow one is read. Courses
taught in English have English pages under `/nb/` (“Readinglist”,
“Working and learning activities”, “Evaluation”; `section_headings` in
the config). robots.txt asks a Crawl-delay of 10 seconds.

**AHO.** Arkitektur- og designhøgskolen i Oslo’s course pages are FS
pages in Vortex, laid out as PHS’s (title with its semester, facts list,
programme list, h2 sections, in `#vrtx-content`):
`https://www.aho.no/studier/emner/{year}/{var|host}/{code}.html`, or
`/english/studies/courses/{year}/{spring|autumn}/{code}.html` for a
course taught in English (one code can have both). The code is DBH’s in
lower case with a dash for its space (“40 333” at `40-333.html`, “MURB9”
at `murb9.html`). Pages start with 2025 (2024: three pages; 2023 answers
404) and the folders run to 2029 (checked 2026-11-20). The sitemap,
`https://www.aho.no/sitemap.xml`, lists every course page, the pages of
the year folders’ listings too, so `resolve_urls_aho_batch()` reads it
once (`aho_course_index()`) and matches the codes as at PHS
(`vortex_match_urls()`): a spring offering without a page takes the
autumn before’s, which serves it only if its title names both semesters.
aho.no redirects to www.aho.no, which the URLs name directly. robots.txt
disallows only `/vrtx`.

**ONH.** Oslo Nye Høyskole shows only the plan in force
(`plan_years = "current"`), one page per course code (Drupal), at a URL
named from the title, the code and the credits, spelled in several ways:
`https://oslonyehoyskole.no/Emner/{Title}-{CODE}-{credits}-sp` mostly
(7.5 credits as `75`), also `-15sp`, `-15-ECT`, the code first
(`/emner/okad3110-Etikk-...`), the code with Ø (`%C3%98KAD1150`), and a
few pages under a programme
(`/digital-markedsforing/emner/okad1110-bedriftsokonomi`) (checked
2026-11-20). The sitemap lists them; `resolve_urls_onh_batch()` takes
the page whose path has the offering’s code as a part between dashes or
slashes, upper case with Ø as O. The page: title, a summary, a facts row
(study form, credits, price), a facts table (admission, place, start,
exam form), “Om emnet:” and accordions with h2 headings, in the last
(“Arbeidskrav og eksamen”) h3 sub-headings “Arbeidskrav” and “Eksamen”;
English pages have “About the Course:”, “Learning outcomes”,
“Assessment” with “Requirement” and “Exam”. robots.txt disallows only
Drupal’s defaults.

**Barratt Due.** Barratt Due musikkinstitutt shows only the plan in
force (`plan_years = "current"`), one page per course under its
programme (WordPress):
`https://barrattdue.no/studier/hoyere-utdanning/{programme}/{code}/`,
the code in lower case with Ø as “oe” (`hoer1100/` for HØR1100), some
with the title after the code
(`valgemner/nokv5100-norske-kvinnelige-utovere-og-komponister/`), and
courses taught in English also under `/en/studier/` (checked
2026-11-20). The studies sitemap, `studier-sitemap.xml`, lists them;
`resolve_urls_barrattdue_batch()` takes the page whose path has the code
as a part (`code_in_path()`, shared with ONH), Norwegian before English,
the page named by the code alone before one with a suffix
(`verk2100-2025/` is the version for students up to 2025/26). The page:
the code as h1 and the name under it, three columns with h4 headings
(“Kort om emnet”, the facts; “Læringsutbytte”; “Undervisning og
læringsformer”), then “Mer om emnet” (h2) over accordions with h3
headings. The master’s programme has a study-plan page and no course
pages. robots.txt disallows nothing.

**LDH.** Lovisenberg diakonale høgskole publishes a plan per programme
cohort (Enonic XP; checked 2026-11-20). The bachelor in nursing has a
page per course and cohort for the cohorts BIS24-BIS26,
`https://ldh.no/studietilbud/bachelor-i-sykepleie/emnerBIS/emnerBIS25/bsy-321`
(16 a cohort, in the sitemap), with a facts box that names the course’s
semester and term (“3. semester: høst 2026”) and h2 sections (“Hensikt”,
“Innhold”, “Læringsutbytte”, “Obligatorisk aktivitet”,
“Vurderings-/eksamensform og vurderingsuttrykk”). Every other plan is a
programme study plan as a PDF, one per programme and cohort (“Kull
H2024”, “Kull V2025”; the bachelor’s “BIS21”), linked from the programme
pages (`/studietilbud/masterstudier/...`,
`/studietilbud/videreutdanninger/...`) and the page of earlier plans,
which writes each course out under a heading line that starts with its
code, followed by “Engelsk emnenavn”, the credits, “Semester: 2” and the
plan’s parts. `harvest_cohort_plans()` reads the index of both
(`ldh_plan_index()`: the sitemap, the programme pages and every study
plan, each harvest anew), splits the PDFs (`ldh_pdf_courses()`: a course
runs to the next course heading or a heading of the plan, such as
“Studiets overordnede læringsutbytter”, “Referanser” or “Vedlegg”,
without the page header, the footers and the page numbers) and gives
each offering the cohort that has the course in its term
(`cohort_plan_match()`; the rule is in `data/data_notes.md`, LDH). The
course pages are fetched as pages; a PDF’s course is stored as its text,
with the PDF’s bytes on its first row.

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
