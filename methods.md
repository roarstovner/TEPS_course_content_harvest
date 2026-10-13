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

Current data: 41 025 offerings (DBH course × year × semester) at 19
institutions, 23 322 of them with a plan, and 13 109 unique plans from
18 institutions (none from Samas, below).

## Population and sampling frame

**Courses in the five-year teacher education programmes, as registered
in DBH.** Programmes are DBH table 347 rows with study type INTMASTER,
IMALU1-7, IMALU5-10 or LUPE; courses are the table 208 rows of those
programmes, matched on institution and programme code (a programme code
is unique only within an institution). *Why:* DBH is the official
register and gives course metadata (credits, level, language, subject)
that need not be coded from the plans. *Rejected:* the four-year GLU
programmes (not in TEPS). *Since:* 2026-09-30 (institution + code match,
c04fbc3).

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

**Years:** DBH had no 2026 rows on 2026-10-06, so the frame ends with
2025.

**Samas is excluded:** its website’s course codes do not match DBH, and
the teacher education courses are described only in programme PDFs
(decided 2026-02).

## Harvest

**Sources:** each institution’s public course pages, except UiS before
its current web plans (PDF archive), Steiner (five subject PDFs) and NLA
(the JSON embedded in the course page). Harvests: 2026-09-30 to
2026-10-02 (all institutions), 2026-10-06 (INN 2025, NLA’s earlier
years), and 2026-04-04 (an earlier harvest of all institutions, used for
the sites below).

**How a plan’s year is known** (`plan_years` in
`R/institution_config.R`): from the URL (most institutions), from the
page, which holds the plans of several academic years (NLA; for most
courses only recent ones), or only from when the page was fetched (UiO,
MF, NMBU, Steiner publish only the plan in force). *Since:* 2026-10-06.

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
those pages: HVL 117 and OsloMet 79 offerings get a plan (HVL 110 and
OsloMet 68 more plans); the rest have no page for that year (“Course not
found”, OsloMet’s error page). *Since:* 2026-10-14.

**Sites that publish only the current plan:** a harvest counts only for
the academic year (August-July) it was made in, and an offering gets the
plan of a harvest from its own academic year, or none. *Why:* these
sites overwrite the plan every year, so a page fetched in 2026 says
nothing about 2025. Example: UiO SPA4192 in April 2026: “Muntlig eksamen
er obligatorisk for deg som fikk opptak fra og med høsten 2021. Muntlig
eksamen er valgfritt for deg som fikk opptak … høsten 2020 eller
tidligere”; in September 2026: “Før endelig karakter settes, skal du ha
muntlig eksamen.” *Rejected:* the latest page for the latest DBH year
(used until 2026-10), which put 2026/27 text on 2025 offerings.
*Consequence:* at these sites 100 autumn 2025 offerings have the April
2026 plan and 0 spring 2025 offerings have a plan (no harvest from
2024/25); the September 2026 plans wait for DBH 2026 offerings. These
sites must be harvested once in every academic year. *Since:*
2026-10-06.

**The raw harvest is append-only and a finalized release is frozen:** a
later harvest (a new DBH year) only adds offerings no earlier harvest
holds, as new files; the raw files of a finalized release are read-only,
and every build checks that each of its plans keeps its id, text and
sections. *Why:* coded plans must not change under the coders when the
data are extended. *Rejected:* merging new rows into the finalized
files, which rewrites frozen data. *Since:* 2026-10-06.

**Pages that gave no plan can be fetched again before a release:**
`harvest_all(retry = TRUE)` fetches the offerings without a plan (a 404,
a page shell) again and adds them as a new raw file; a retried offering
takes its latest page. *Why:* a page can fail for a while; UiA’s 95
autumn-2023 pages were shells (title and facts box) when harvested but
full plans once on 2026-10-07. *Rejected:* overwriting the raw file (the
store is append-only). *Consequence:* retries are done before a release
is finalized. UiA’s shells did not come back: the full pages were copies
last fetched from FS in July 2024, and every copy rebuilt since
2026-09-30 lacks the plan, so these offerings have no plan (2026-10-08).
*Since:* 2026-10-07.

**UiO semester pages are not used:** URLs such as `/h24/` hold logistics
(teachers, timetable, exam dates), not the plan (decided 2026-02).

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

## Unit of analysis: the unique plan

**A plan is a unique anonymized text per institution and course code**
(`plan_content_id`, a hash of the normalized text). Offerings with the
same text share a plan: 23 322 offerings have 13 109 plans. *Why:*
consecutive years and both semesters mostly repeat the same text.
*Consequence:* the id changes whenever cleaning changes the text, so
coding is tied to a frozen release and carried to later releases with a
crosswalk.

**Page shells are not plans:** a page with no plan section to read and
under 1,500 characters of plan text (without the title and facts box,
since 2026-10-13; only the facts box, a credit-overlap box,
“Emnebeskrivelse”, a pointer to the English page, every section “Se
fagplanen.”, a list of headings with no text under them) gives its
offering no plan. So does an INN page that says the requested semester’s
plan is not published yet and shows the latest version instead (“Her er
den siste versjonen vi har tilgjengelig”): that is a later plan (2026 or
later), not the offering’s. Nord does the same without a notice: for a
year it has no plan for, its page shows the current plan, headed
“Gjeldende emnebeskrivelse (sist oppdatert 2026/27)” instead of
“Emnebeskrivelse for 2023/24”. Where the year it names is after the
offering’s academic year, the offering gets no plan; where it is not,
the page is the version in force that year and stays (`page_year` in the
configuration). Longer pages without sections stay (OsloMet practicum
courses, below). *Consequence:* 174 plans left the data on 2026-10-06
(UiA 95, UiB 27, OsloMet 26, Nord 11, HVL 10, UiT 4, NTNU 1). With pages
that are not the plan at all (INN’s course search page, OsloMet’s error
page) and later versions (INN, Nord), 2 279 offerings have page text but
no plan (hvl 86, inn 1112, nord 819, oslomet 67, uia 95, uib 80, uit
20). *Since:* 2026-10-06; heading lists (UiB NOLI216 and NOSP216, autumn
2018) from 2026-10-07; NTNU pages whose only section was the exam tab
from 2026-10-12 to 2026-10-13, when the tab came back (20 plans, 70
offerings). INN later versions from 2026-10-07: 1,077 INN offerings (384
of 407 pages from 2022, about half of 2023-2025) lose their plan; the
April 2026 harvest has the same notice on the same pages. Nord’s later
versions from 2026-10-14 (fulltext audit, 2026-10-13): 761 offerings
lose their plan (all of 2016-2018 and 205 later ones), 90 plans leave
the data, and Nord’s course-years with a plan go from 1,843 to 1,478.
*Rejected:* keeping the later plan with a flag (decided 2026-10-07: an
offering is coded with its own year’s plan or none). A rule on
`course_plan` alone (2026-10-13): the characters in lines that read as
text (four or more words; not a heading, a placeholder or a short
“Label: value” fact) do not separate shells from plans at any threshold.
A credit-overlap box reads as text, and so the 391 shell offerings have
a median of 88 such characters and up to 738 (UiA, Nord); some plans
have little but the assessment design (NTNU master’s thesis pages,
“Vurderingsordning: Avhandling”, UiS MGLINT’s exam box). Below 150
characters, 128 plan offerings would become shells and 127 shell
offerings stay plans; below 500, 326 and 4. Whether a section can be cut
is what tells a page with text about the course from one without.

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
read; DBH’s `Avdelingsnavn` gives every UiB offering its unit. The other
institutions followed the same day (next entry). *Since:* 2026-10-13.

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

## Sections

**Seven canonical sections** are cut from each plan by its headings,
mapped through one heading table (`R/section_heading_map.R`): course
content (98 % of plans), learning outcomes (98 %), assessment (98 %),
teaching methods (94 %), coursework requirements (91 %), prerequisites
(54 %) and reading list (33 %). Where a section is missing, the coders
use the whole plan.

**Coursework requirements are kept apart from assessment:** arbeidskrav
and obligatory-activity paragraphs inside an assessment block are moved
to coursework requirements (since 2026-10-01).

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

**Practicum stays in teaching methods** rather than a section of its
own: only HiOF and Nord give practicum its own heading (since
2026-10-01).

**Admission text is not prerequisites** (opptakskrav, study-right
sentences; since 2026-10-01), and credit-overlap boxes are dropped.

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

**OsloMet practicum courses (M1GP/M5GP) have no sections:** every
section of the emneplan says “Se fagplanen.”, and the page’s Fagplan is
the programme’s practicum plan for all study years, with no markup
separating the years. Their plan text keeps the whole page. *Rejected:*
reading the whole practicum plan into each course, or slicing it by year
(fragile). *Since:* 2026-10-02.
