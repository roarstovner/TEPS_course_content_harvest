# Methods notes: collecting and preparing the course plans


The choices behind the published data that a methods section has to
report, by topic. Each entry gives the choice, the reason, alternatives
that were rejected, the consequence in numbers (computed from the
current data when this file is rendered) and since when it applies
(date, chainlink issue). How to run the pipeline is in `README.md`;
per-institution data quality in `data/data_notes.md`. When a decision
changes what the data mean, add or update its entry here; each release
tag freezes this file with the data.

Current data: 41 025 offerings (DBH course × year × semester) at 19
institutions, 24 080 of them with a plan, and 13 967 unique plans from
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
those years. *Since:* 2026-10-06 (#291).

**DBH registers most courses in both semesters**, also when a course is
taught and has a plan in one only. Coverage is therefore counted in
course-years (course × year with a plan in at least one semester), not
offerings (`data/data_notes.md`, “Semester registration in DBH”; \#253).

**Years:** DBH had no 2026 rows on 2026-10-06, so the frame ends with
2025.

**Samas is excluded:** its website’s course codes do not match DBH, and
the teacher education courses are described only in programme PDFs
(#69-#90).

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
MF, NMBU, Steiner publish only the plan in force). *Since:* 2026-10-06
(#292, \#293).

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
sites must be harvested once in every academic year. *Since:* 2026-10-06
(#293).

**The raw harvest is append-only and a finalized release is frozen:** a
later harvest (a new DBH year) only adds offerings no earlier harvest
holds, as new files; the raw files of a finalized release are read-only,
and every build checks that each of its plans keeps its id, text and
sections. *Why:* coded plans must not change under the coders when the
data are extended. *Rejected:* merging new rows into the finalized
files, which rewrites frozen data. *Since:* 2026-10-06 (#296).

**Pages that gave no plan can be fetched again before a release:**
`harvest_all(retry = TRUE)` fetches the offerings without a plan (a 404,
a page shell) again and adds them as a new raw file; a retried offering
takes its latest page. *Why:* a page can fail for a while; UiA’s 95
autumn-2023 pages were shells (title and facts box) when harvested but
full plans once on 2026-10-07. *Rejected:* overwriting the raw file (the
store is append-only). *Consequence:* retries are done before a release
is finalized. UiA’s shells did not come back: the full pages were copies
last fetched from FS in July 2024, and every copy rebuilt since
2026-09-30 lacks the plan, so these offerings have no plan (2026-10-08,
\#300). *Since:* 2026-10-07 (#297).

**UiO semester pages are not used:** URLs such as `/h24/` hold logistics
(teachers, timetable, exam dates), not the plan (#76).

**UiT: the Norwegian document of a semester listed twice:** UiT’s
semester picker can list a semester twice, an English document beside
the Norwegian one; the Norwegian one is used. A course with only an
English document keeps it (ENG-1040, ENG-3193, ENG-3194, ENG-3993).
*Why:* the English twin was an empty page in all 5 cases, while the
Norwegian one has the plan. *Rejected:* the first option (the rule
before, which took the English twin). *Consequence:* 5 offerings get a
plan (ENG-2980 spring 2017, ENG-3980 and ENG-3981 spring 2016, LRU-1300
autumn 2010, LER-1150 autumn 2018), 2 of them a plan not in the data
before. *Since:* 2026-10-08 (#313).

## Unit of analysis: the unique plan

**A plan is a unique anonymized text per institution and course code**
(`plan_content_id`, a hash of the normalized text). Offerings with the
same text share a plan: 24 080 offerings have 13 967 plans. *Why:*
consecutive years and both semesters mostly repeat the same text.
*Consequence:* the id changes whenever cleaning changes the text, so
coding is tied to a frozen release and carried to later releases with a
crosswalk.

**Page shells are not plans:** a page with no plan section to read and
under 1,500 characters (only the facts box, a credit-overlap box,
“Emnebeskrivelse”, a pointer to the English page, every section “Se
fagplanen.”, a list of headings with no text under them) gives its
offering no plan. So does an INN page that says the requested semester’s
plan is not published yet and shows the latest version instead (“Her er
den siste versjonen vi har tilgjengelig”): that is a later plan (2026 or
later), not the offering’s. Longer pages without sections stay (OsloMet
practicum courses, below). *Consequence:* 174 plans left the data on
2026-10-06 (UiA 95, UiB 27, OsloMet 26, Nord 11, HVL 10, UiT 4, NTNU 1).
With pages that are not the plan at all (INN’s course search page,
OsloMet’s error page; \#53, \#57), 1 582 offerings have page text but no
plan (hvl 86, inn 1112, nord 58, ntnu 71, oslomet 77, uia 95, uib 80,
uit 3). *Since:* 2026-10-06 (#222); heading lists (UiB NOLI216 and
NOSP216, autumn 2018) from 2026-10-07 (#299); NTNU pages whose only
section was the exam tab from 2026-10-12 (#321, 20 plans, 70 offerings).
INN later versions from 2026-10-07 (#302): 1,077 INN offerings (384 of
407 pages from 2022, about half of 2023-2025) lose their plan; the April
2026 harvest has the same notice on the same pages. *Rejected:* keeping
the later plan with a flag (decided 2026-10-07: an offering is coded
with its own year’s plan or none).

**Tables are read one row per line:** each row of a table on the page is
one line, its cells separated by tabs and each cell on one line, so an
exam table (INN, Volda, UiT from 2022) has one line per exam. *Why:*
splitting the assessment text by exam (#311) needs the rows. The reader
had run a table with `<thead>`/`<tbody>` into one line (INN, Volda) and
joined the lines of a cell without a space (Volda “Fagartikkel2.
Utvikling …”, UiT “Karakterregel:A–E”, “Spring 2009Veiledning”).
*Rejected:* keeping a cell’s own line breaks, which split one exam over
several lines. *Consequence:* the text changes only in whitespace; 725
of 2,314 UiT plans and 77 of 547 Volda plans get new ids, because words
a cell had joined are now apart; no plans merge or split. *Since:*
2026-10-08 (#312). A line break inside a `<span>` or link elsewhere on
the page is now a space as well, not glue (HiOF “minutter.Det tas”, UiB
“KunnskaparStudenten”): 64 UiT, 37 HiOF, 8 UiB and 3 UiA plans get new
ids, and two pairs of UiT plans that differed only by such glue merge
(SOA-3905 spring and autumn 2011; LER-1201 spring 2018). *Since:*
2026-10-08 (#315).

## Anonymization

**All published text is anonymized** (`anonymize_text()`): staff names
(role labels with names, “Name (Role)” lists, signature and approval
lines), e-mail addresses, phone numbers, IP addresses in library links,
and administrative dates and years (“Opprettet 2020”, “2023/2024”);
content years (“etter 1945”) are kept. A check on every build
(`privacy_check`) looks for e-mails, phone numbers, IP addresses and
staff-name layouts in the published files. *Why:* the plans are public,
but the published data should not carry personal data. *Since:* 2026-09
to 2026-10 (#226-#238, \#260, \#281); UiA lists of several course
coordinators and USN approval lines from 2026-10-06 (#231); names after
a staff role anywhere in a line (“… avklares med faglærer <name>”, “Ta
kontakt med <name> eller <name>”), HVL campus contact lines and UiT’s
2008-2011 timetable block (teachers, rooms, first lectures) from
2026-10-07, after the anonymization audit of all institutions found them
in 16 plans (#301).

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
(2016–2017)” into “Meld. St. 21” (#229, 2026-09). *Consequence:* 185
plans keep such dates; an access date next to a government link also
stays (“\[Lesedato: …\]” after a udir.no address). At the same time
administrative dates in more forms go: em-dash year ranges (“Studieår:
2019 — 2020”, NLA), UiS version stamps, approval and update dates in
words (“godkjent i studieutvalget 14. november 2016”) or with a
two-digit year (“Godkjent 22.03.23”), and semester codes (“H23”,
“H2025-V2026”): 1,683 plans lose one; 21 plans that differed only by
such a date merge with another. *Since:* 2026-10-08 (#305). The 80
characters are counted from the start of the date, not its end, so
“nasjonale retningslinjer … av 1. september 2016, revidert 17. oktober
2018” keeps its revision date and “høsten” over a line break stays;
faculty regulations count as such documents (“utfyllende regler … av
01.08.2015”). Dates in references stay too: a dd.mm.yyyy date after a
comma, which in these pages is always a reading-list entry (“Razsviet,
4.11.1923”, “SSB, 07.06.2023,”), and ISO dates (“2001-02-01”, read as
the academic year “2001-02” before). *Why:* a reference’s date
identifies the work cited (audit rubric, \#310). *Consequence:* 138
plans change text (usn 54, oslomet 39, uis 31, uit 9, ntnu 5); none
merge or split. *Since:* 2026-10-12 (#317).

**OsloMet’s “;” glue is a space.** In 2021–2022 OsloMet’s pages put a
“;” where a space or line break belongs (“En;viktig del;av”, “1;000
ord”); a “;” not followed by a space becomes a space (7,254 on 231
pages, none in other years), and a real semicolon, followed by a space,
stays. *Why:* the glue is page rendering, not text; it made the 2022
pages separate plans. *Consequence:* 57 OsloMet plans merge with the
same plan of a neighbouring year (625 → 568). At the same time HVL’s
Nynorsk “Emnet vert ikkje tilbydd 2025/2026.” goes like the Bokmål
notice (#306; 4 plans merge), USN’s Leganto type labels go before quoted
and lowercase titles too, and a second approval date on a stamp line
goes (“… og på fullmakt av leder i utdanningsutvalget 4. mars 2019”).
13,979 plans. *Since:* 2026-10-12 (#318).

**A removal takes the space and separators that joined it.** A removed
date, season or e-mail takes the space before it, a season with year
also the dash, comma or colon before it and a list after it (“15 stp,
høst 2019 og vår 2020” → “15 stp”), and a date or clock-time range goes
as one unit (“Dato: 27.05.2024 –31.05.2024”). *Why:* “Faget går .”,
“(Kull )” and “Dato: –” are artifacts of the removal, not text.
*Consequence:* no content changes, but the dedup hash keeps punctuation
and spacing, so 2,480 plans get a new id and 12 that differed only by
such leftovers merge (13,967 plans). *Since:* 2026-10-12 (#319).

**NTNU: the exam tab is not part of the plan; the responsible unit is.**
The page’s exam tab (“Vurderingsordning: …”, “Karakter: …” and the exam
sessions with their parts, weights, dates and rooms) is read neither
into `course_plan` nor into the sections. `course_plan` keeps “Ansvarlig
enhet” with the department; the contact names and the timetable go. The
dedup hash leaves the unit out. *Why:* the exam tab is the semester’s
exam registration, not the plan text; at the end of the assessment
section it read as coded exam data, and the assessment form, grading
scale and exam parts are coded in TEPS_course_content_coding, not here
(user decision 2026-10-12). *Rejected:* keeping “Vurderingsordning” and
“Karakter” in the assessment section (#245, 2026-10-01) and in
`course_plan` (#307, 2026-10-08) so that pass/fail courses could be told
from graded ones; keeping the ordinary session’s exam parts for the exam
coding (#316); hashing the unit, which split plans only because a
department was renamed. *Consequence:* 743 of 1,684 NTNU plans with an
assessment section lose it (the exam tab was all of it); 941 keep their
own “Mer om vurdering”. 20 pages whose only section was the exam tab
(master’s thesis courses `*3950`, discontinued courses) are page shells:
70 offerings lose their plan (14,060 → 14,040 plans). The facts box at
the top of `course_plan` still names the assessment form
(“Vurderingsordning Oppgave”). 1,889 of 1,988 NTNU plans name their
unit. Other plan ids do not change. *Since:* 2026-10-08 (#307), exam tab
dropped 2026-10-12 (#321).

## Sections

**Seven canonical sections** are cut from each plan by its headings,
mapped through one heading table (`R/section_heading_map.R`): course
content (99 % of plans), learning outcomes (98 %), assessment (90 %),
teaching methods (94 %), coursework requirements (91 %), prerequisites
(56 %) and reading list (33 %). Where a section is missing, the coders
use the whole plan.

**Coursework requirements are kept apart from assessment:** arbeidskrav
and obligatory-activity paragraphs inside an assessment block are moved
to coursework requirements (#212, \#283).

**Practicum stays in teaching methods** rather than a section of its
own: only HiOF and Nord give practicum its own heading (#201, \#247).

**Admission text is not prerequisites** (opptakskrav, study-right
sentences; \#211), and credit-overlap boxes are dropped.

**Placeholders are dropped** (“Ingen”, “-”, “Se fagplanen.”,
reading-list pointers; \#210, \#286).

**Prerequisites written inside the course description are
prerequisites:** a “Forkunnskapskrav” label in course content (UiT’s “Om
emnet”, two MF courses) takes the paragraph it names to prerequisites.
At UiT that paragraph is almost always “Jf. opptakskrav og
progresjonskrav i studieplanen …”, a pointer to the programme’s
admission rules, which is dropped like other admission text, so most UiT
plans lose it from course content without gaining a prerequisites
section. *Rejected:* treating the label as a sub-heading (the course
description continues after the pointer and would be filed as
prerequisites). *Since:* 2026-10-07 (#285).

**Nord’s short description is course content:** the lead paragraph above
the accordions (pages from 2019 on) opens course content, without the
title and course code above it; “Se kursinnhold.” there is a
placeholder. A paragraph of 40 or more characters that a section repeats
is kept once (Nord’s lead is often the start of “Beskrivelse av emnet”;
UiS PDF plans repeat their lead sentence). *Rejected:* filing all text
before the first heading as course content (would take in the title and
code). *Since:* 2026-10-07 (#285).

**OsloMet teaching methods that only say “Se fagplanen.”** take the text
of the subject’s Fagplan on the same page (“Fagets arbeids- og
undervisningsformer”), marked “Se fagplanen. Fagplanen sier: …”: 112
plans. *Why:* the emneplan delegates the teaching methods to the subject
plan, which applies to the course. *Since:* 2026-10-06 (#289).

**OsloMet practicum courses (M1GP/M5GP) have no sections:** every
section of the emneplan says “Se fagplanen.”, and the page’s Fagplan is
the programme’s practicum plan for all study years, with no markup
separating the years. Their plan text keeps the whole page. *Rejected:*
reading the whole practicum plan into each course, or slicing it by year
(fragile). *Since:* 2026-10-02 (#242).
