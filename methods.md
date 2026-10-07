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
institutions, 25 224 of them with a plan, and 14 238 unique plans from
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

**UiO semester pages are not used:** URLs such as `/h24/` hold logistics
(teachers, timetable, exam dates), not the plan (#76).

## Unit of analysis: the unique plan

**A plan is a unique anonymized text per institution and course code**
(`plan_content_id`, a hash of the normalized text). Offerings with the
same text share a plan: 25 224 offerings have 14 238 plans. *Why:*
consecutive years and both semesters mostly repeat the same text.
*Consequence:* the id changes whenever cleaning changes the text, so
coding is tied to a frozen release and carried to later releases with a
crosswalk.

**Page shells are not plans:** a page with no plan section to read and
under 1,500 characters (only the facts box, a credit-overlap box,
“Emnebeskrivelse”, a pointer to the English page, every section “Se
fagplanen.”) gives its offering no plan. Longer pages without sections
stay (OsloMet practicum courses, below). *Consequence:* 174 plans left
the data on 2026-10-06 (UiA 95, UiB 27, OsloMet 26, Nord 11, HVL 10, UiT
4, NTNU 1). With pages that are not the plan at all (INN’s course search
page, OsloMet’s error page; \#53, \#57), 434 offerings have page text
but no plan (hvl 86, inn 35, nord 58, ntnu 1, oslomet 77, uia 95, uib
78, uit 4). *Since:* 2026-10-06 (#222).

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
coordinators and USN approval lines from 2026-10-06 (#231).

## Sections

**Seven canonical sections** are cut from each plan by its headings,
mapped through one heading table (`R/section_heading_map.R`): course
content (98 % of plans), learning outcomes (98 %), assessment (95 %),
teaching methods (94 %), coursework requirements (91 %), prerequisites
(55 %) and reading list (33 %). Where a section is missing, the coders
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
undervisningsformer”), marked “Se fagplanen. Fagplanen sier: …”: 130
plans. *Why:* the emneplan delegates the teaching methods to the subject
plan, which applies to the course. *Since:* 2026-10-06 (#289).

**OsloMet practicum courses (M1GP/M5GP) have no sections:** every
section of the emneplan says “Se fagplanen.”, and the page’s Fagplan is
the programme’s practicum plan for all study years, with no markup
separating the years. Their plan text keeps the whole page. *Rejected:*
reading the whole practicum plan into each course, or slicing it by year
(fragile). *Since:* 2026-10-02 (#242).
