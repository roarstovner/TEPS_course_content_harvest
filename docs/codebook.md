# Codebook


The variables, codes and keys of the published files of a release:
`course_offerings`, `course_plans` and `plan_sections`, of both frames
(all courses and the teacher-ed frame; [Data Files](data-files.md)
“Frames”). It is rendered from `data/variables.csv` (the variables),
`data/codes.csv` (the value labels of the coded columns) and
`data/study_types.csv` (the study types and their groups), which go with
every release, as does this codebook. The same tables make the release’s
`datapackage.json`, a [Data Package](https://datapackage.org) (v2)
descriptor of the Parquet copies of the files
(`datapackage_descriptor()`).

Source: “DBH table …” is the column of a DBH table (its Norwegian name),
“derived” or “harvest” a column the pipeline makes, and a code whose
source is “project” is defined by the project, not by DBH. Codes are
DBH’s unless marked. Missing values are NA (null in Parquet).

## course_offerings

Primary key: `course_id`. Foreign key: `plan_id` →
`course_plans.plan_id`.

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 1%" />
<col style="width: 6%" />
<col style="width: 87%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">variable</th>
<th style="text-align: left;">type</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>course_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Offering id:
{institution}_{course_code_raw}_{year}_{semester}_{status}, with the
semester in English (spring, autumn, summer). An offering keeps the id
it was first harvested under when a later DBH snapshot changes its
status.</td>
</tr>
<tr>
<td style="text-align: left;"><code>institution</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from institution_code</td>
<td style="text-align: left;">Short name of the institution in the
harvest (“oslomet”, “uia”); several DBH codes can map to one (INN: 0264
and 1177).</td>
</tr>
<tr>
<td style="text-align: left;"><code>institution_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonskode</td>
<td style="text-align: left;">DBH’s institution code (“0256”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>institution_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonsnavn</td>
<td style="text-align: left;">DBH’s institution name.</td>
</tr>
<tr>
<td style="text-align: left;"><code>department_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode</td>
<td style="text-align: left;">DBH’s code of the unit (faculty or
department) that offers the course.</td>
</tr>
<tr>
<td style="text-align: left;"><code>department_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingsnavn</td>
<td style="text-align: left;">Name of the unit.</td>
</tr>
<tr>
<td style="text-align: left;"><code>department_code_ssb</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode_SSB</td>
<td style="text-align: left;">Statistics Norway’s code of the unit; NA
in some years.</td>
</tr>
<tr>
<td style="text-align: left;"><code>year</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Årstall</td>
<td style="text-align: left;">Calendar year of the semester.</td>
</tr>
<tr>
<td style="text-align: left;"><code>semester</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Semester</td>
<td style="text-align: left;">Semester code: 0 winter, 1 spring, 2
summer, 3 autumn.</td>
</tr>
<tr>
<td style="text-align: left;"><code>semester_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Semesternavn</td>
<td style="text-align: left;">Semester in Norwegian: “Vinter”, “Vår”,
“Sommer”, “Høst”.</td>
</tr>
<tr>
<td style="text-align: left;"><code>programme_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramkode</td>
<td style="text-align: left;">The study programme DBH registers the
course under in this semester (one per row); unique only within an
institution.</td>
</tr>
<tr>
<td style="text-align: left;"><code>programme_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramnavn</td>
<td style="text-align: left;">Name of the programme.</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_code_raw</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnekode</td>
<td style="text-align: left;">DBH’s course code, with its version number
(“LMUKHV10421-1”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from course_code_raw</td>
<td style="text-align: left;">Course code without a trailing version
number (“LMUKHV10421”): the code plans are kept by.</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnenavn</td>
<td style="text-align: left;">Course name as DBH registers it.</td>
</tr>
<tr>
<td style="text-align: left;"><code>level_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivåkode</td>
<td style="text-align: left;">Level of the course: LN lower degree
level, HN higher degree level, AR one-year programme, VS upper secondary
level, 50 an older code without a name, and others in DBH (FU, B3, M2,
…).</td>
</tr>
<tr>
<td style="text-align: left;"><code>level_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivånavn</td>
<td style="text-align: left;">Name of the level; NA for code 50.</td>
</tr>
<tr>
<td style="text-align: left;"><code>credits</code></td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">DBH table 208 Studiepoeng</td>
<td style="text-align: left;">ECTS credits.</td>
</tr>
<tr>
<td style="text-align: left;"><code>nus_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 NUS-kode</td>
<td style="text-align: left;">Statistics Norway’s standard
classification of education (NUS), six digits; “999999” unknown.</td>
</tr>
<tr>
<td style="text-align: left;"><code>status</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Status</td>
<td style="text-align: left;">Course status: 0 no data, 1 active, 2 new,
3 discontinued, 4 discontinued with active students (exams still
held).</td>
</tr>
<tr>
<td style="text-align: left;"><code>status_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Statusnavn</td>
<td style="text-align: left;">Status in Norwegian (“Aktivt”, “Nytt”,
“Avviklet”, “Avviklet med aktive studenter”, “Ikke data”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>language_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Underv.språk</td>
<td style="text-align: left;">Language of instruction as DBH registers
it: “NOR”, “ENG”, “SME”, …; “999” unknown; a few in lower case
(“nob”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>language_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Navn</td>
<td style="text-align: left;">Name of the language of instruction
(“Norsk”, “Engelsk”, “Ukjent”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>subject_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagkode</td>
<td style="text-align: left;">Subject (fag) code of the course
(“KUNST”); NA for some courses.</td>
</tr>
<tr>
<td style="text-align: left;"><code>subject_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagnavn</td>
<td style="text-align: left;">Name of the subject (“Kunst og
Håndverk”).</td>
</tr>
<tr>
<td style="text-align: left;"><code>thesis</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Oppgave (ny fra h2012)</td>
<td style="text-align: left;">Whether the course is a thesis: 0 no, 1 a
bachelor’s thesis or project, 2 a master’s (or hovedfag) thesis.
Registered from autumn 2012, and for some earlier courses.</td>
</tr>
<tr>
<td style="text-align: left;"><code>plan_year_basis</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">How the year of the offering’s plan is
known: “url” the page’s URL holds the year; “current” the site shows
only the plan in force, so a page is the plan of the academic year it
was fetched in. The data built on 2026-10-16 had “page” for NLA, whose
URL holds the academic year too.</td>
</tr>
<tr>
<td style="text-align: left;"><code>plan_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The offering’s plan in course_plans:
{institution}_{course_code}_{the first 10 characters of
plan_content_id}; NA when the offering has no plan. Join offerings to
plans and sections on it (added 2026-11-19).</td>
</tr>
<tr>
<td style="text-align: left;"><code>plan_content_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Fingerprint of the plan’s text: a SHA-256
hash of course_plan_normalized, so offerings with the same plan text
share it, also across parallel course codes of an institution. Not
unique per plan: join on plan_id. NA when the offering has no plan.</td>
</tr>
<tr>
<td style="text-align: left;"><code>harvested_at</code></td>
<td style="text-align: left;">Date</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">Date of the harvest of the offering’s row
(recorded since 2026-10-06, else NA); at a site that shows only the
current plan, the date of the harvest whose page the offering
takes.</td>
</tr>
<tr>
<td style="text-align: left;"><code>academic_year</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">NLA only: the academic year (“2023-2024”)
whose part of the course page holds the offering’s plan; NA
elsewhere.</td>
</tr>
<tr>
<td style="text-align: left;"><code>has_extracted_text</code></td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Whether text was read from the offering’s
page.</td>
</tr>
<tr>
<td style="text-align: left;"><code>extracted_text_nchar</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Characters of the text read from the page,
before anonymization; NA without text.</td>
</tr>
<tr>
<td style="text-align: left;"><code>exam_registrations</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 905</td>
<td style="text-align: left;">Exam registrations (905 Oppmeldt totalt)
in the course-year (course_code_raw and year), summed over its
semesters. The same on every offering of the course-year; 0 for a
course-year with no row in DBH. DBH zeroes small counts before summing,
so a course with one or two students can have 0 (which is why
had_students uses either count). From the latest counts snapshot, dated
apart from the course list.</td>
</tr>
<tr>
<td style="text-align: left;"><code>credits_earned</code></td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">DBH table 900</td>
<td style="text-align: left;">Credits earned (900 Ny produksjon totalt)
in the course-year, summed over its semesters, in full-time equivalents
(60 credits = 1). The same on every offering of the course-year; 0 for a
course-year with no row in DBH. Counts of the same snapshot as
exam_registrations.</td>
</tr>
<tr>
<td style="text-align: left;"><code>had_students</code></td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">DBH tables 905 and 900</td>
<td style="text-align: left;">Whether the course-year had students:
exam_registrations &gt; 0 or credits_earned &gt; 0. Either count is used
because DBH zeroes small counts before summing, so exam_registrations
can be 0 for a course with 1-2 students. The same on every offering of
the course-year.</td>
</tr>
<tr>
<td style="text-align: left;"><code>counts_provisional</code></td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">DBH tables 905 and 900</td>
<td style="text-align: left;">Whether the year’s counts
(exam_registrations, credits_earned) are not complete: a year after the
last one whose autumn DBH had published in both tables when the counts
snapshot was taken (DBH publishes a semester about one semester later).
The counts of a provisional year can still grow.</td>
</tr>
<tr>
<td style="text-align: left;"><code>study_type_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 347 Studiumkode</td>
<td style="text-align: left;">Study type of the offering’s programme
(programme_code) in that semester (“INTMASTER”, “SYK”, “HUMAN”): DBH’s
classification of a programme as a profession, a programme model or a
broad field. Joined from table 347 of the course list’s snapshot on
institution_code, programme_code, year and semester; NA when the
programme has no table 347 row that semester. Only the type of the
programme DBH registers the offering under, not of other programmes that
use the course (added 2026-11-20).</td>
</tr>
<tr>
<td style="text-align: left;"><code>study_type_name</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 347 Studnavn</td>
<td style="text-align: left;">DBH’s Norwegian name of the study type
(“Femårig lektorutdanning”); NA with study_type_code.</td>
</tr>
<tr>
<td style="text-align: left;"><code>study_type_group</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">project grouping,
data/study_types.csv</td>
<td style="text-align: left;">The project’s group of the study type,
defined by the project, not by DBH: column group of data/study_types.csv
(released with the data), one of teacher_education, health_welfare,
law_police_military, technology_engineering, arts_design_architecture,
primary_industries, other_professional, broad_fields. NA with
study_type_code, or for a study type the file lacks (added
2026-11-20).</td>
</tr>
</tbody>
</table>

## course_plans

Primary key: `plan_id`.

<table>
<colgroup>
<col style="width: 8%" />
<col style="width: 3%" />
<col style="width: 6%" />
<col style="width: 81%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">variable</th>
<th style="text-align: left;">type</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>plan_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Plan id, unique:
{institution}_{course_code}_{the first 10 characters of
plan_content_id}. A plan is a plan_content_id, institution and
course_code (added 2026-11-19).</td>
</tr>
<tr>
<td style="text-align: left;"><code>plan_content_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Fingerprint of the plan’s text: a SHA-256
hash of course_plan_normalized. Parallel course codes of an institution
that publish the same text share it, so it is not unique per plan: join
on plan_id; it finds identical texts across courses.</td>
</tr>
<tr>
<td style="text-align: left;"><code>institution</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;"><code>year_from</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">First year of the offerings with this plan
(double in the data built until 2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;"><code>year_to</code></td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Last year of the offerings with this plan
(double in the data built until 2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;"><code>source_course_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The plan’s earliest offering: course_plan
and the sections are read from its page.</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_plan</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The plan’s text, anonymized
(course_plan_spec.md).</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_plan_normalized</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">course_plan lower-cased, without
structural headings and labels, whitespace squished
(normalize_plan_text()): the text the id is made from. Not for
reading.</td>
</tr>
</tbody>
</table>

## plan_sections

Primary key: `plan_id` + `section`. Foreign key: `plan_id` →
`course_plans.plan_id`.

<table>
<colgroup>
<col style="width: 5%" />
<col style="width: 2%" />
<col style="width: 5%" />
<col style="width: 86%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">variable</th>
<th style="text-align: left;">type</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>plan_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans. A section is a plan_id
and section.</td>
</tr>
<tr>
<td style="text-align: left;"><code>plan_content_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;"><code>institution</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;"><code>course_code</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;"><code>source_course_id</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;"><code>section</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">One of eight sections
(section_codebook.yml): course_content, learning_outcomes,
prerequisites, teaching_methods, coursework_requirements, assessment,
reading_list, and practice, which overlaps: its text is also in another
section (mostly teaching_methods), so section lengths per plan count it
twice.</td>
</tr>
<tr>
<td style="text-align: left;"><code>text</code></td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The section’s text, anonymized.</td>
</tr>
</tbody>
</table>

## Codes

The allowed values of the coded columns, in every file that has the
column. Open DBH code lists (`language_code`, `subject_code`,
`nus_code`, `department_code`, `programme_code`) are not listed: their
values are DBH’s and vary between snapshots.

### `semester`

<table>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>0</code></td>
<td style="text-align: left;">Winter</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;"><code>1</code></td>
<td style="text-align: left;">Spring</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;"><code>2</code></td>
<td style="text-align: left;">Summer</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;"><code>3</code></td>
<td style="text-align: left;">Autumn</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### `semester_name`

<table>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>Vinter</code></td>
<td style="text-align: left;">Winter</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">semester 0</td>
</tr>
<tr>
<td style="text-align: left;"><code>Vår</code></td>
<td style="text-align: left;">Spring</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">semester 1</td>
</tr>
<tr>
<td style="text-align: left;"><code>Sommer</code></td>
<td style="text-align: left;">Summer</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">semester 2</td>
</tr>
<tr>
<td style="text-align: left;"><code>Høst</code></td>
<td style="text-align: left;">Autumn</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">semester 3</td>
</tr>
</tbody>
</table>

### `status`

<table>
<colgroup>
<col style="width: 5%" />
<col style="width: 36%" />
<col style="width: 7%" />
<col style="width: 51%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>0</code></td>
<td style="text-align: left;">No data</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Ikke data</td>
</tr>
<tr>
<td style="text-align: left;"><code>1</code></td>
<td style="text-align: left;">Active</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Aktivt</td>
</tr>
<tr>
<td style="text-align: left;"><code>2</code></td>
<td style="text-align: left;">New</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Nytt</td>
</tr>
<tr>
<td style="text-align: left;"><code>3</code></td>
<td style="text-align: left;">Discontinued</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Avviklet</td>
</tr>
<tr>
<td style="text-align: left;"><code>4</code></td>
<td style="text-align: left;">Discontinued with active students</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Avviklet med aktive studenter: exams still
held</td>
</tr>
</tbody>
</table>

### `status_name`

<table>
<colgroup>
<col style="width: 37%" />
<col style="width: 40%" />
<col style="width: 8%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>Ikke data</code></td>
<td style="text-align: left;">No data</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">status 0</td>
</tr>
<tr>
<td style="text-align: left;"><code>Aktivt</code></td>
<td style="text-align: left;">Active</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">status 1</td>
</tr>
<tr>
<td style="text-align: left;"><code>Nytt</code></td>
<td style="text-align: left;">New</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">status 2</td>
</tr>
<tr>
<td style="text-align: left;"><code>Avviklet</code></td>
<td style="text-align: left;">Discontinued</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">status 3</td>
</tr>
<tr>
<td
style="text-align: left;"><code>Avviklet med aktive studenter</code></td>
<td style="text-align: left;">Discontinued with active students</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">status 4</td>
</tr>
</tbody>
</table>

### `level_code`

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 44%" />
<col style="width: 8%" />
<col style="width: 40%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>LN</code></td>
<td style="text-align: left;">Lower degree level (other)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Lavere nivå (øvrige)</td>
</tr>
<tr>
<td style="text-align: left;"><code>HN</code></td>
<td style="text-align: left;">Higher degree level (other)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Høyere nivå (øvrige)</td>
</tr>
<tr>
<td style="text-align: left;"><code>AR</code></td>
<td style="text-align: left;">One-year programme, 60 credits</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Årsstudium, 60-studiepoeng</td>
</tr>
<tr>
<td style="text-align: left;"><code>B3</code></td>
<td style="text-align: left;">Bachelor, 3-year</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Bachelor, 3-årig</td>
</tr>
<tr>
<td style="text-align: left;"><code>M2</code></td>
<td style="text-align: left;">Master, 2-year</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Master, 2-årig</td>
</tr>
<tr>
<td style="text-align: left;"><code>ME</code></td>
<td style="text-align: left;">Experience-based master, 1.5-2 years</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Master, erfaringsbasert, 1,5-2 år</td>
</tr>
<tr>
<td style="text-align: left;"><code>HK</code></td>
<td style="text-align: left;">University college graduate, 2-year</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Høyskolekandidat, 2-årig</td>
</tr>
<tr>
<td style="text-align: left;"><code>YU</code></td>
<td style="text-align: left;">Vocational education, 4-year</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Yrkesutdanning, 4-årig</td>
</tr>
<tr>
<td style="text-align: left;"><code>FU</code></td>
<td style="text-align: left;">Doctoral (researcher) education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Forskerutdanning</td>
</tr>
<tr>
<td style="text-align: left;"><code>VS</code></td>
<td style="text-align: left;">Upper secondary level</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">Videregående skoles nivå</td>
</tr>
<tr>
<td style="text-align: left;"><code>50</code></td>
<td style="text-align: left;">Older code without a name</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">level_name NA</td>
</tr>
<tr>
<td style="text-align: left;"><code>AN</code></td>
<td style="text-align: left;">Code without a name</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">level_name NA</td>
</tr>
</tbody>
</table>

### `thesis`

<table>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>0</code></td>
<td style="text-align: left;">Not a thesis</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;"><code>1</code></td>
<td style="text-align: left;">Bachelor’s thesis or project</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
<tr>
<td style="text-align: left;"><code>2</code></td>
<td style="text-align: left;">Master’s (or hovedfag) thesis</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### `plan_year_basis`

<table>
<colgroup>
<col style="width: 5%" />
<col style="width: 21%" />
<col style="width: 4%" />
<col style="width: 68%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>url</code></td>
<td style="text-align: left;">The page’s URL holds the year</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">The plan is that of the year (or academic
year) in the URL.</td>
</tr>
<tr>
<td style="text-align: left;"><code>current</code></td>
<td style="text-align: left;">The site shows only the plan in force</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">A page is the plan of the academic year it
was fetched in (docs/harvesting.md, sites that show only the current
plan).</td>
</tr>
</tbody>
</table>

### `section`

<table>
<colgroup>
<col style="width: 13%" />
<col style="width: 19%" />
<col style="width: 4%" />
<col style="width: 63%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>course_content</code></td>
<td style="text-align: left;">Course content</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">What the course is about: subject matter,
topics, themes.</td>
</tr>
<tr>
<td style="text-align: left;"><code>learning_outcomes</code></td>
<td style="text-align: left;">Learning outcomes</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">What the student knows or can do after the
course (knowledge, skills, general competence).</td>
</tr>
<tr>
<td style="text-align: left;"><code>prerequisites</code></td>
<td style="text-align: left;">Prerequisites</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Required or recommended prior knowledge
and courses.</td>
</tr>
<tr>
<td style="text-align: left;"><code>teaching_methods</code></td>
<td style="text-align: left;">Teaching and learning methods</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">How the course is taught and how students
work.</td>
</tr>
<tr>
<td style="text-align: left;"><code>coursework_requirements</code></td>
<td style="text-align: left;">Coursework requirements (arbeidskrav)</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Mandatory work that must be approved
before the exam and is not itself graded.</td>
</tr>
<tr>
<td style="text-align: left;"><code>assessment</code></td>
<td style="text-align: left;">Assessment and examination</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">The final graded assessment(s) that set
the course grade.</td>
</tr>
<tr>
<td style="text-align: left;"><code>reading_list</code></td>
<td style="text-align: left;">Reading list</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Required and recommended literature and
course materials.</td>
</tr>
<tr>
<td style="text-align: left;"><code>practice</code></td>
<td style="text-align: left;">Practice (praksis)</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Text under the plan’s own heading for
practicum or placement; overlaps: also in another section (mostly
teaching_methods).</td>
</tr>
</tbody>
</table>

### `institution`

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 38%" />
<col style="width: 5%" />
<col style="width: 46%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>aho</code></td>
<td style="text-align: left;">Arkitektur- og designhøgskolen i Oslo</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1220</td>
</tr>
<tr>
<td style="text-align: left;"><code>barrattdue</code></td>
<td style="text-align: left;">Barratt Due Musikkinstitutt</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8227</td>
</tr>
<tr>
<td style="text-align: left;"><code>dmmh</code></td>
<td style="text-align: left;">Dronning Mauds Minne Høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8224</td>
</tr>
<tr>
<td style="text-align: left;"><code>fhs</code></td>
<td style="text-align: left;">Forsvarets høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1303</td>
</tr>
<tr>
<td style="text-align: left;"><code>fjellhaug</code></td>
<td style="text-align: left;">Fjellhaug Internasjonale Høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8234</td>
</tr>
<tr>
<td style="text-align: left;"><code>himolde</code></td>
<td style="text-align: left;">Høgskolen i Molde, vitenskapelig høgskole
i logistikk</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 0232</td>
</tr>
<tr>
<td style="text-align: left;"><code>hiof</code></td>
<td style="text-align: left;">Høgskolen i Østfold</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 0256</td>
</tr>
<tr>
<td style="text-align: left;"><code>hivolda</code></td>
<td style="text-align: left;">Høgskulen i Volda</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 0236</td>
</tr>
<tr>
<td style="text-align: left;"><code>hvl</code></td>
<td style="text-align: left;">Høgskulen på Vestlandet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 0238</td>
</tr>
<tr>
<td style="text-align: left;"><code>inn</code></td>
<td style="text-align: left;">Universitetet i Innlandet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH codes 1177 and 0264 (Høgskolen i
Innlandet, the earlier code)</td>
</tr>
<tr>
<td style="text-align: left;"><code>khio</code></td>
<td style="text-align: left;">Kunsthøgskolen i Oslo</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 6220</td>
</tr>
<tr>
<td style="text-align: left;"><code>ldh</code></td>
<td style="text-align: left;">Lovisenberg diakonale høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8202</td>
</tr>
<tr>
<td style="text-align: left;"><code>mf</code></td>
<td style="text-align: left;">MF vitenskapelig høyskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8221</td>
</tr>
<tr>
<td style="text-align: left;"><code>nhh</code></td>
<td style="text-align: left;">Norges handelshøyskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1240</td>
</tr>
<tr>
<td style="text-align: left;"><code>nih</code></td>
<td style="text-align: left;">Norges idrettshøgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1260</td>
</tr>
<tr>
<td style="text-align: left;"><code>nla</code></td>
<td style="text-align: left;">NLA Høgskolen</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8223</td>
</tr>
<tr>
<td style="text-align: left;"><code>nmbu</code></td>
<td style="text-align: left;">Norges miljø- og biovitenskapelige
universitet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1173</td>
</tr>
<tr>
<td style="text-align: left;"><code>nmh</code></td>
<td style="text-align: left;">Norges musikkhøgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1210</td>
</tr>
<tr>
<td style="text-align: left;"><code>nord</code></td>
<td style="text-align: left;">Nord universitet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1174</td>
</tr>
<tr>
<td style="text-align: left;"><code>ntnu</code></td>
<td style="text-align: left;">Norges teknisk-naturvitenskapelige
universitet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1150</td>
</tr>
<tr>
<td style="text-align: left;"><code>onh</code></td>
<td style="text-align: left;">Oslo Nye Høyskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1407</td>
</tr>
<tr>
<td style="text-align: left;"><code>oslomet</code></td>
<td style="text-align: left;">OsloMet – storbyuniversitetet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1175</td>
</tr>
<tr>
<td style="text-align: left;"><code>phs</code></td>
<td style="text-align: left;">Politihøgskolen</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1301</td>
</tr>
<tr>
<td style="text-align: left;"><code>samas</code></td>
<td style="text-align: left;">Samisk høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 0217</td>
</tr>
<tr>
<td style="text-align: left;"><code>steiner</code></td>
<td style="text-align: left;">Steinerhøyskolen</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8225</td>
</tr>
<tr>
<td style="text-align: left;"><code>uia</code></td>
<td style="text-align: left;">Universitetet i Agder</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1171</td>
</tr>
<tr>
<td style="text-align: left;"><code>uib</code></td>
<td style="text-align: left;">Universitetet i Bergen</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1120</td>
</tr>
<tr>
<td style="text-align: left;"><code>uio</code></td>
<td style="text-align: left;">Universitetet i Oslo</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1110</td>
</tr>
<tr>
<td style="text-align: left;"><code>uis</code></td>
<td style="text-align: left;">Universitetet i Stavanger</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1160</td>
</tr>
<tr>
<td style="text-align: left;"><code>uit</code></td>
<td style="text-align: left;">Universitetet i Tromsø - Norges arktiske
universitet</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1130</td>
</tr>
<tr>
<td style="text-align: left;"><code>usn</code></td>
<td style="text-align: left;">Universitetet i Sørøst-Norge</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 1176</td>
</tr>
<tr>
<td style="text-align: left;"><code>vid</code></td>
<td style="text-align: left;">VID vitenskapelige høgskole</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">DBH code 8208</td>
</tr>
</tbody>
</table>

### `study_type_code`

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 46%" />
<col style="width: 3%" />
<col style="width: 43%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>ALU</code></td>
<td style="text-align: left;">General teacher education (allmennlærer,
replaced by GLU in 2010)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Allmennlærerutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>BLU</code></td>
<td style="text-align: left;">Kindergarten teacher education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Barnehagelærerutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>INTMASTER</code></td>
<td style="text-align: left;">Lektor education, years 8-13, 5-year
master</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Femårig lektorutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>PRAKTPED</code></td>
<td style="text-align: left;">Practical-pedagogical education (PPU)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Praktisk-pedagogisk utdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>FLU</code></td>
<td style="text-align: left;">Preschool teacher education
(førskolelærer, replaced by kindergarten teacher education in 2012)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Førskolelærerutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>GLU1-7</code></td>
<td style="text-align: left;">Primary and lower secondary teacher
education, years 1-7 (4-year)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Grunnskolelærerutdanning 1-7 trinn;
group teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>IMALU1-7</code></td>
<td style="text-align: left;">Primary and lower secondary teacher
education, years 1-7, 5-year master</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: 5-årig masterprogram i
grunnskolelærerutdanning 1-7 trinn; group teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>GLU5-10</code></td>
<td style="text-align: left;">Primary and lower secondary teacher
education, years 5-10 (4-year)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Grunnskolelærerutdanning 5-10 trinn;
group teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>IMALU5-10</code></td>
<td style="text-align: left;">Primary and lower secondary teacher
education, years 5-10, 5-year master</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: 5-årig masterprogram i
grunnskolelærerutdanning 5-10 trinn; group teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>FAG</code></td>
<td style="text-align: left;">Subject teacher education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Faglærerutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>LUPE</code></td>
<td style="text-align: left;">Teacher education in practical and
aesthetic subjects, years 1-13, 5-year master</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Lærerutdanning i praktiske og
estetiske fag; group teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>YRKESFAG</code></td>
<td style="text-align: left;">Vocational teacher education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Yrkesfaglærerutdanning; group
teacher_education</td>
</tr>
<tr>
<td style="text-align: left;"><code>AUD</code></td>
<td style="text-align: left;">Audiology</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Audiografutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>BIO</code></td>
<td style="text-align: left;">Biomedical laboratory science</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Bioingeniørutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>BAR</code></td>
<td style="text-align: left;">Child welfare</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Barnevernpedagogutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>TANNPL</code></td>
<td style="text-align: left;">Dental hygiene</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Tannpleier; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>TANNTEKN</code></td>
<td style="text-align: left;">Dental technology</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Tannteknikerutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>ODO</code></td>
<td style="text-align: left;">Dentistry</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Odontologi; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>MEDISIN</code></td>
<td style="text-align: left;">Medicine</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Medisin; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>SYK</code></td>
<td style="text-align: left;">Nursing</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Sykepleierutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>ERNÆRING</code></td>
<td style="text-align: left;">Nutrition</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Ernæring; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>ERG</code></td>
<td style="text-align: left;">Occupational therapy</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Ergoterapeututdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>ORT</code></td>
<td style="text-align: left;">Orthopaedic engineering</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Ortopediingeniørutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>FARMASI</code></td>
<td style="text-align: left;">Pharmacy</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Farmasi; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>FYS</code></td>
<td style="text-align: left;">Physiotherapy</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Fysioterapeututdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>RES</code></td>
<td style="text-align: left;">Prescriptionist (bachelor in
pharmacy)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Reseptarutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>PSYKOLOGI</code></td>
<td style="text-align: left;">Psychology</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Psykologi; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>RAD</code></td>
<td style="text-align: left;">Radiography</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Radiografutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>VER</code></td>
<td style="text-align: left;">Social education (vernepleie)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Vernepleierutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>SOS</code></td>
<td style="text-align: left;">Social work</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Sosionomutdanning/sosialt arbeid;
group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>VET</code></td>
<td style="text-align: left;">Veterinary medicine</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Veterinærutdanning; group
health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>DYR</code></td>
<td style="text-align: left;">Veterinary nursing</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Dyrepleie; group health_welfare</td>
</tr>
<tr>
<td style="text-align: left;"><code>JUS</code></td>
<td style="text-align: left;">Law</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Juridiske fag; group
law_police_military</td>
</tr>
<tr>
<td style="text-align: left;"><code>MILITÆR</code></td>
<td style="text-align: left;">Military education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Militær utdanning; group
law_police_military</td>
</tr>
<tr>
<td style="text-align: left;"><code>POLITI</code></td>
<td style="text-align: left;">Police education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Politiutdanning; group
law_police_military</td>
</tr>
<tr>
<td style="text-align: left;"><code>ING</code></td>
<td style="text-align: left;">Engineering</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Ingeniørutdanning; group
technology_engineering</td>
</tr>
<tr>
<td style="text-align: left;"><code>MAR</code></td>
<td style="text-align: left;">Maritime education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Maritim utdanning; group
technology_engineering</td>
</tr>
<tr>
<td style="text-align: left;"><code>SIVING</code></td>
<td style="text-align: left;">Master of science in engineering
(sivilingeniør, until 2007)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Sivilingeniørutdanning; group
technology_engineering</td>
</tr>
<tr>
<td style="text-align: left;"><code>ARKITEKTUR</code></td>
<td style="text-align: left;">Architecture</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Arkitektur; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>KU</code></td>
<td style="text-align: left;">Artistic research</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Kunstnerisk utviklingsarbeid; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>DESIGN</code></td>
<td style="text-align: left;">Design</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Designutdanning; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>BILDEKUNST</code></td>
<td style="text-align: left;">Fine art</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Kunstfagutdanning; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>ID</code></td>
<td style="text-align: left;">Industrial design</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Industridesign; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>MUSIKK</code></td>
<td style="text-align: left;">Music performance</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Utøvende musikkutdanning; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>KUNST</code></td>
<td style="text-align: left;">Performing arts education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Utøvende kunstutdanning; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>SCEKUN</code></td>
<td style="text-align: left;">Stage arts</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Scenekunst; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>VISKUN</code></td>
<td style="text-align: left;">Visual arts</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Visuell kunst; group
arts_design_architecture</td>
</tr>
<tr>
<td style="text-align: left;"><code>LU</code></td>
<td style="text-align: left;">Agriculture</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Landbruksutdanning; group
primary_industries</td>
</tr>
<tr>
<td style="text-align: left;"><code>UTVMILJØ</code></td>
<td style="text-align: left;">Development and environment</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Utvikling og miljø; group
primary_industries</td>
</tr>
<tr>
<td style="text-align: left;"><code>FISKERIFAG</code></td>
<td style="text-align: left;">Fisheries</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Fiskerifag; group
primary_industries</td>
</tr>
<tr>
<td style="text-align: left;"><code>JOU</code></td>
<td style="text-align: left;">Journalism and photojournalism</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Journalist-/fotoutdanning; group
other_professional</td>
</tr>
<tr>
<td style="text-align: left;"><code>BIB</code></td>
<td style="text-align: left;">Library and information science</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Bibliotekarutdanning; group
other_professional</td>
</tr>
<tr>
<td style="text-align: left;"><code>SIVOK</code></td>
<td style="text-align: left;">Master of business (siviløkonom, until
2007)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Siviløkonomutdanning; group
other_professional</td>
</tr>
<tr>
<td style="text-align: left;"><code>DOV</code></td>
<td style="text-align: left;">Sign language and interpreting</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Tegnspråk; group
other_professional</td>
</tr>
<tr>
<td style="text-align: left;"><code>IU</code></td>
<td style="text-align: left;">Sports and physical education</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Idrettsutdanning; group
other_professional</td>
</tr>
<tr>
<td style="text-align: left;"><code>ØA</code></td>
<td style="text-align: left;">Economics and administration</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Økonomisk-administrativ utdanning;
group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>UVFAG</code></td>
<td style="text-align: left;">Education (pedagogy)</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Pedagogiske fag; group
broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>EXPHIL</code></td>
<td style="text-align: left;">Examen philosophicum</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Examen philosophicum; group
broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>HELSEFAG</code></td>
<td style="text-align: left;">Health sciences</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Helsefag; group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>HUMAN</code></td>
<td style="text-align: left;">Humanities</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Historisk-filosofiske fag; group
broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>REALFAG</code></td>
<td style="text-align: left;">Mathematics and natural sciences</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Matematisk-naturvitenskapelige fag;
group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>ANNET</code></td>
<td style="text-align: left;">Other</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Annet; group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>SAMVIT</code></td>
<td style="text-align: left;">Social sciences</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Samfunnsvitenskap; group
broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>TEKNOLOGI</code></td>
<td style="text-align: left;">Technology</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Teknologi; group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>TEOLOGI</code></td>
<td style="text-align: left;">Theology</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Teologi; group broad_fields</td>
</tr>
<tr>
<td style="text-align: left;"><code>VIDEREG</code></td>
<td style="text-align: left;">Upper secondary level programmes</td>
<td style="text-align: left;">DBH</td>
<td style="text-align: left;">DBH: Studieprogram videregående nivå;
group broad_fields</td>
</tr>
</tbody>
</table>

### `study_type_group`

<table style="width:100%;">
<colgroup>
<col style="width: 26%" />
<col style="width: 38%" />
<col style="width: 7%" />
<col style="width: 27%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">code</th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;"><code>teacher_education</code></td>
<td style="text-align: left;">Teacher education</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Lærerutdanning</td>
</tr>
<tr>
<td style="text-align: left;"><code>health_welfare</code></td>
<td style="text-align: left;">Health and welfare</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Helse- og sosialfag</td>
</tr>
<tr>
<td style="text-align: left;"><code>law_police_military</code></td>
<td style="text-align: left;">Law, police and military</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Jus, politi og militær</td>
</tr>
<tr>
<td style="text-align: left;"><code>technology_engineering</code></td>
<td style="text-align: left;">Technology and engineering</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Teknologi og ingeniørfag</td>
</tr>
<tr>
<td style="text-align: left;"><code>arts_design_architecture</code></td>
<td style="text-align: left;">Arts, design and architecture</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Kunst, design og arkitektur</td>
</tr>
<tr>
<td style="text-align: left;"><code>primary_industries</code></td>
<td style="text-align: left;">Agriculture, fisheries and
environment</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Landbruk, fiskeri og miljø</td>
</tr>
<tr>
<td style="text-align: left;"><code>other_professional</code></td>
<td style="text-align: left;">Other professional programmes</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Andre profesjonsutdanninger</td>
</tr>
<tr>
<td style="text-align: left;"><code>broad_fields</code></td>
<td style="text-align: left;">Broad fields of study</td>
<td style="text-align: left;">project</td>
<td style="text-align: left;">Brede fagområder</td>
</tr>
</tbody>
</table>
