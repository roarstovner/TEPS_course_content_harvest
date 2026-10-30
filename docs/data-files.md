# Data Files


## Data Files: Published and Internal

Files that hold raw page text (`html`, `extracted_text`) contain staff
names, e-mail addresses and phone numbers and are never published.
Everything that is shared is built from the anonymized text
(`anonymize_text()`). The data are kept in one folder per stage, so the
folder says what may be shared:

<table>
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th>Folder</th>
<th>Holds</th>
<th>Written by</th>
<th>In git</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>data/input/</code></td>
<td>the DBH snapshots, the pipeline input</td>
<td><code>data-raw/courses.R</code></td>
<td>yes</td>
</tr>
<tr>
<td><code>data/pilot/</code></td>
<td>the pilot samples: offerings and their strata</td>
<td><code>pilot_sample()</code></td>
<td>yes</td>
</tr>
<tr>
<td><code>data/raw/</code></td>
<td>the harvest, with personal data; append-only</td>
<td>harvesting only</td>
<td>no</td>
</tr>
<tr>
<td><code>data/releases/{tag}/</code></td>
<td>offerings, plans and sections of a finalized release</td>
<td><code>finalize_release()</code></td>
<td>no</td>
</tr>
<tr>
<td><code>data/backup/</code></td>
<td>earlier harvests kept but not read (personal data)</td>
<td>by hand</td>
<td>no</td>
</tr>
<tr>
<td><code>data/interim/</code></td>
<td>text rebuilt from the harvest, with personal data; never shared</td>
<td><code>targets::tar_make()</code></td>
<td>no</td>
</tr>
<tr>
<td><code>data/processed/</code></td>
<td>anonymized data or data without text; can be shared</td>
<td><code>targets::tar_make()</code></td>
<td>no</td>
</tr>
</tbody>
</table>

`data/processed/` may only hold text that went through
`anonymize_text()`. The pipeline checks this on every build: target
`privacy_check` scans the files for e-mail addresses, phone numbers,
“Name (Role)” lines, “Name, dekan” signatures, and role labels followed
by a name (“faglærer Name”, “Emneansvarlige er: Name (sosionom)”, a role
heading over a name line) and warns, and
`tests/testthat/test-anonymize.R` fails, on any hit. The repository is
public.

<table>
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th>File</th>
<th>Contents</th>
<th>Personal data</th>
<th>Status</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>data/input/dbh_courses_{date}.parquet</code>,
<code>dbh_programmes_{date}.parquet</code></td>
<td>DBH tables 208 (courses) and 347 (programmes), the harvest
input</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/raw/{inst}/{year}/{run}.parquet</code>,
<code>data/raw/checkpoint/</code></td>
<td>raw harvest: <code>html</code> (+ text and bytes of PDFs)</td>
<td>yes (raw)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/raw/{inst}/ledger/{run}.parquet</code></td>
<td>fetch ledger: URL, HTTP status, outcome and page hash per offering
and run</td>
<td>none</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/pilot/{inst}.csv</code></td>
<td>an institution’s pilot sample (<a
href="new-institution.md#piloting-an-institution">“Piloting an
Institution”</a>)</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/variables.csv</code></td>
<td>every variable of the published files, with its old name
(“Variables”)</td>
<td>none</td>
<td>in git; released with the data</td>
</tr>
<tr>
<td><code>data/robots_txt.csv</code></td>
<td>each host’s robots.txt: date read, Crawl-delay, the disallow and
allow rules the harvest applies (<a
href="harvesting.md#robotstxt">“robots.txt”</a>)</td>
<td>none</td>
<td>in git</td>
</tr>
<tr>
<td><code>data/interim/plan_gaps.RDS</code></td>
<td>course-years without a plan, reason, URLs tried</td>
<td>none</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/interim/extracted_text.RDS</code></td>
<td><code>extracted_text</code> rebuilt from the raw harvest</td>
<td>yes (raw)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/interim/course_offerings_full.RDS</code></td>
<td>offerings + <code>url</code>, <code>extracted_text</code>,
<code>course_plan</code></td>
<td>yes (<code>extracted_text</code>)</td>
<td>internal</td>
</tr>
<tr>
<td><code>data/processed/course_offerings.RDS</code></td>
<td>DBH metadata + <code>plan_content_id</code>, no text</td>
<td>none</td>
<td>published</td>
</tr>
<tr>
<td><code>data/processed/course_plans.RDS</code></td>
<td>anonymized <code>course_plan</code> (+
<code>course_plan_normalized</code>)</td>
<td>anonymized</td>
<td>published</td>
</tr>
<tr>
<td><code>data/processed/plan_sections.RDS</code></td>
<td>anonymized section <code>text</code> per plan</td>
<td>anonymized</td>
<td>publishable</td>
</tr>
<tr>
<td><code>data/interim/teacher_ed/</code>,
<code>data/processed/teacher_ed/</code></td>
<td>the same files for the teacher-ed frame (“Frames”)</td>
<td>as above</td>
<td>as above</td>
</tr>
<tr>
<td><code>app/course_browser/data/browser_data.RDS</code></td>
<td>course_browser payload: plans, offering metadata, sections</td>
<td>anonymized (no <code>extracted_text</code>)</td>
<td>internal (app)</td>
</tr>
<tr>
<td><code>app/course_browser_ojs/data/*.parquet</code></td>
<td>anonymized <code>course_plan</code> + slim offering metadata</td>
<td>anonymized</td>
<td>published with the OJS page (build artifact)</td>
</tr>
<tr>
<td><code>data/audit/</code></td>
<td>audit findings (quotes from anonymized text)</td>
<td>staff names redacted</td>
<td>in git</td>
</tr>
</tbody>
</table>

The scan catches text that skipped the anonymizer, not names the
anonymizer misses; the anonymization audit
(`/audit-institutions anonymization`) checks that per institution.

## Frames

The pipeline builds every offering in the raw store, and writes the data
files of two frames (`FRAMES`): `all`, every harvested offering, in
`data/interim/` and `data/processed/`; and `teacher_ed`, the offerings
in the teacher-ed frame of the latest DBH snapshot
(`dbh_courses("teacher_ed")`, matched on the offering without its
status, `frame_members()`), in the `teacher_ed/` folders of both
(`frame_dir()`). The text, plan id and sections of a page are the same
in both; the plans are deduplicated within each frame (`frame_plans()`),
so a teacher-ed plan has the years, text and sections of its own
earliest teacher-ed offering, whatever else the raw store holds, and a
harvest of other courses leaves the teacher-ed files as they were. The
teacher-ed files are the releases’ (`finalize_release()`,
`release_data()`, target `frozen_check`), the browsers’ and
`methods.md`’s; the data notes, the gaps and the audits read those of
all courses. Each frame has its metrics snapshot, and target `coverage`
its DBH offerings.

Per-institution prose notes are edited directly in
`data/data_notes.qmd`.

## Data Structure

### Input Data (`data/input/dbh_*_{date}.parquet`)

The harvest’s input is a dated snapshot of DBH, written by
`data-raw/courses.R` from DBH’s bulk files: `dbh_courses_{date}.parquet`
is table 208 (Emner), every course of every institution, year and
semester, all levels and statuses, one row per course and semester;
`dbh_programmes_{date}.parquet` is table 347 (Studieprogram), with the
programmes’ study type (`Studiumkode`). A new snapshot gets a new date
and the old ones stay. The snapshots keep DBH’s column names.
`dbh_courses()` reads the latest (or the one of `date`), gives DBH’s
columns the pipeline’s names (`DBH_COLUMNS`; see “Variables”) and adds
`institution` and `course_code`:

``` r
courses <- dbh_courses("teacher_ed")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution institution_code institution_name department_code department_name 
      <chr>       <chr>            <chr>            <chr>           <chr>           
    1 samas       0217             Samisk høgskole  480000          Avdeling for du…
    2 samas       0217             Samisk høgskole  480000          Avdeling for du…
    # ℹ 20 more variables: department_code_ssb <chr>, year <int>, semester <int>,
    #   semester_name <chr>, programme_code <chr>, programme_name <chr>,
    #   course_code_raw <chr>, course_code <chr>, course_name <chr>,
    #   level_code <chr>, level_name <chr>, credits <dbl>, nus_code <chr>,
    #   status <int>, status_name <chr>, language_code <chr>, language_name <chr>,
    #   subject_code <chr>, subject_name <chr>, thesis <int>

`dbh_courses()` is every course; `dbh_courses("teacher_ed")` keeps the
courses of the teacher education programmes (study types in
`TEACHER_ED_STUDIUM`, matched on institution and programme code: a
programme code is unique only within an institution). The teacher-ed
frame is a filter of the same snapshot, so its offerings have the same
`course_id` in both. `harvest_all()` without `courses` harvests every
course; `tar_make()` builds everything in `data/raw/` and writes the
teacher-ed frame’s files apart (see “Frames”).

Key columns: - `institution`: Short code (e.g., “oslomet”, “uia”); NA
for an institution code no config has - `course_code_raw`: DBH’s course
code (used in `course_id`) - `course_code`: the code with the trailing
version number removed (`canon_remove_trailing_num()`: “ABC123-1” →
“ABC123”) - `year`, `semester_name` (“Vår”, “Høst”, “Sommer”, “Vinter”;
`canon_semester_name()` gives `course_id` spring, autumn, summer) -
`programme_code`, `programme_name`: the programme DBH registers the
course under (one per row) - `status`: course status (1 active, 2 new, 3
discontinued, 4 discontinued with active students). It is the last part
of `course_id`, and a later snapshot can change it: an offering already
in a raw file keeps the id it was first harvested under
(`first_course_id()`)

DBH lists most courses under both semesters of a year, also when they
are taught in one. Where pages are semester-specific, about half the
rows therefore have no plan by design; see “Semester registration in
DBH” in `data/data_notes.qmd` before reading row-level success rates.

`harvest_institution()` returns the offerings with `course_id`, `url`,
`html`, `html_error`, `html_success` and `extracted_text` added, the
rows of the raw store ([“Raw Store and Finalized
Releases”](rebuilding.md#raw-store-and-finalized-releases)).

### Variables (`data/variables.csv`)

`data/variables.csv` documents every variable of the published files
(`course_offerings.RDS`, `course_plans.RDS`, `plan_sections.RDS`, of
both frames): the file, its name, type, source (the column of DBH table
208, or how the pipeline gives it), meaning and codes, and `old_name`,
its name in the releases up to data-2026-10-16. `release_data()` hands
it over with the data, and `tests/testthat/test-dbh-input.R` checks it
against the built files and `DBH_COLUMNS`. `course_offerings_full.RDS`
(internal) adds `url`, `extracted_text`, `course_plan`,
`course_plan_normalized` and `plan_nchar`.

Since 2026-10-16 the DBH columns have English snake_case names
(methods.qmd, “Variable names”); their values are DBH’s (Norwegian
names, DBH’s codes), and `year_from` and `year_to` are integers (double
before). DBH’s snapshots, the raw files of the harvests until 2026-10-16
and the releases up to data-2026-10-16 have DBH’s names: the pipeline
renames them where it reads such data (`dbh_rename()`, in
`dbh_courses()`, `read_raw()`, `frozen_changes()` and `release_data()`).
Code that reads an old release can rename it by the same pairs:

``` r
v <- read.csv("data/variables.csv")
pairs <- unique(v[v$old_name != v$variable, c("variable", "old_name")])
old <- readRDS("data/releases/data-2026-10-16/course_offerings.RDS") |>
  dplyr::rename(dplyr::any_of(setNames(pairs$old_name, pairs$variable)))
```

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 2%" />
<col style="width: 9%" />
<col style="width: 71%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">file</th>
<th style="text-align: left;">variable</th>
<th style="text-align: left;">old_name</th>
<th style="text-align: left;">type</th>
<th style="text-align: left;">source</th>
<th style="text-align: left;">description</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_id</td>
<td style="text-align: left;">course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Offering id:
{institution}<em>{course_code_raw}</em>{year}<em>{semester}</em>{status},
with the semester in English (spring, autumn, summer). An offering keeps
the id it was first harvested under when a later DBH snapshot changes
its status.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from institution_code</td>
<td style="text-align: left;">Short name of the institution in the
harvest (“oslomet”, “uia”); several DBH codes can map to one (INN: 0264
and 1177).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution_code</td>
<td style="text-align: left;">Institusjonskode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonskode</td>
<td style="text-align: left;">DBH’s institution code (“0256”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">institution_name</td>
<td style="text-align: left;">Institusjonsnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Institusjonsnavn</td>
<td style="text-align: left;">DBH’s institution name.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_code</td>
<td style="text-align: left;">Avdelingskode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode</td>
<td style="text-align: left;">DBH’s code of the unit (faculty or
department) that offers the course.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_name</td>
<td style="text-align: left;">Avdelingsnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingsnavn</td>
<td style="text-align: left;">Name of the unit.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">department_code_ssb</td>
<td style="text-align: left;">Avdelingskode_SSB</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Avdelingskode_SSB</td>
<td style="text-align: left;">Statistics Norway’s code of the unit; NA
in some years.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">year</td>
<td style="text-align: left;">Årstall</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Årstall</td>
<td style="text-align: left;">Calendar year of the semester.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">semester</td>
<td style="text-align: left;">Semester</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Semester</td>
<td style="text-align: left;">Semester code: 0 winter, 1 spring, 2
summer, 3 autumn.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">semester_name</td>
<td style="text-align: left;">Semesternavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Semesternavn</td>
<td style="text-align: left;">Semester in Norwegian: “Vinter”, “Vår”,
“Sommer”, “Høst”.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">programme_code</td>
<td style="text-align: left;">Studieprogramkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramkode</td>
<td style="text-align: left;">The study programme DBH registers the
course under in this semester (one per row); unique only within an
institution.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">programme_name</td>
<td style="text-align: left;">Studieprogramnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Studieprogramnavn</td>
<td style="text-align: left;">Name of the programme.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_code_raw</td>
<td style="text-align: left;">Emnekode_raw</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnekode</td>
<td style="text-align: left;">DBH’s course code, with its version number
(“LMUKHV10421-1”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived from course_code_raw</td>
<td style="text-align: left;">Course code without a trailing version
number (“LMUKHV10421”): the code plans are kept by.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">course_name</td>
<td style="text-align: left;">Emnenavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Emnenavn</td>
<td style="text-align: left;">Course name as DBH registers it.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">level_code</td>
<td style="text-align: left;">Nivåkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivåkode</td>
<td style="text-align: left;">Level of the course: LN lower degree
level, HN higher degree level, AR one-year programme, VS upper secondary
level, 50 an older code without a name, and others in DBH (FU, B3, M2,
…).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">level_name</td>
<td style="text-align: left;">Nivånavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Nivånavn</td>
<td style="text-align: left;">Name of the level; NA for code 50.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">credits</td>
<td style="text-align: left;">Studiepoeng</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">DBH table 208 Studiepoeng</td>
<td style="text-align: left;">ECTS credits.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">nus_code</td>
<td style="text-align: left;">NUS-kode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 NUS-kode</td>
<td style="text-align: left;">Statistics Norway’s standard
classification of education (NUS), six digits; “999999” unknown.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">status</td>
<td style="text-align: left;">Status</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Status</td>
<td style="text-align: left;">Course status: 0 no data, 1 active, 2 new,
3 discontinued, 4 discontinued with active students (exams still
held).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">status_name</td>
<td style="text-align: left;">Statusnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Statusnavn</td>
<td style="text-align: left;">Status in Norwegian (“Aktivt”, “Nytt”,
“Avviklet”, “Avviklet med aktive studenter”, “Ikke data”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">language_code</td>
<td style="text-align: left;">Underv.språk</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Underv.språk</td>
<td style="text-align: left;">Language of instruction as DBH registers
it: “NOR”, “ENG”, “SME”, …; “999” unknown; a few in lower case
(“nob”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">language_name</td>
<td style="text-align: left;">Navn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Navn</td>
<td style="text-align: left;">Name of the language of instruction
(“Norsk”, “Engelsk”, “Ukjent”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">subject_code</td>
<td style="text-align: left;">Fagkode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagkode</td>
<td style="text-align: left;">Subject (fag) code of the course
(“KUNST”); NA for some courses.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">subject_name</td>
<td style="text-align: left;">Fagnavn</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 208 Fagnavn</td>
<td style="text-align: left;">Name of the subject (“Kunst og
Håndverk”).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">thesis</td>
<td style="text-align: left;">Oppgave (ny fra h2012)</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">DBH table 208 Oppgave (ny fra h2012)</td>
<td style="text-align: left;">Whether the course is a thesis: 0 no, 1 a
bachelor’s thesis or project, 2 a master’s (or hovedfag) thesis.
Registered from autumn 2012, and for some earlier courses.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_year_basis</td>
<td style="text-align: left;">plan_year_basis</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">How the year of the offering’s plan is
known: “url” the page’s URL holds the year; “current” the site shows
only the plan in force, so a page is the plan of the academic year it
was fetched in. Release data-2026-10-16 has “page” for NLA, whose URL
holds the academic year too.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The offering’s plan in course_plans (with
institution and course_code); NA when the offering has no plan. A
SHA-256 hash of course_plan_normalized, so offerings with the same plan
text share it.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">harvested_at</td>
<td style="text-align: left;">harvested_at</td>
<td style="text-align: left;">Date</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">Date of the harvest of the offering’s row
(recorded since 2026-10-06, else NA); at a site that shows only the
current plan, the date of the harvest whose page the offering
takes.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">academic_year</td>
<td style="text-align: left;">academic_year</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">NLA only: the academic year (“2023-2024”)
whose part of the course page holds the offering’s plan; NA
elsewhere.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">has_extracted_text</td>
<td style="text-align: left;">has_extracted_text</td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Whether text was read from the offering’s
page.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">extracted_text_nchar</td>
<td style="text-align: left;">extracted_text_nchar</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Characters of the text read from the page,
before anonymization; NA without text.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Plan id: a SHA-256 hash of
course_plan_normalized. A plan is a plan_content_id, institution and
course_code.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">year_from</td>
<td style="text-align: left;">year_from</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">First year of the offerings with this plan
(double up to data-2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Last year of the offerings with this plan
(double up to data-2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The plan’s earliest offering: course_plan
and the sections are read from its page.</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_plan</td>
<td style="text-align: left;">course_plan</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The plan’s text, anonymized
(course_plan_spec.md).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">course_plan_normalized</td>
<td style="text-align: left;">course_plan_normalized</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">course_plan lower-cased, without
structural headings and labels, whitespace squished
(normalize_plan_text()): the text the id is made from. Not for
reading.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">institution</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">course_code</td>
<td style="text-align: left;">Emnekode</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_offerings.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">source_course_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">section</td>
<td style="text-align: left;">section</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">One of seven sections
(section_codebook.yml): course_content, learning_outcomes,
prerequisites, teaching_methods, coursework_requirements, assessment,
reading_list.</td>
</tr>
<tr>
<td style="text-align: left;">plan_sections</td>
<td style="text-align: left;">text</td>
<td style="text-align: left;">text</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest, anonymized</td>
<td style="text-align: left;">The section’s text, anonymized.</td>
</tr>
</tbody>
</table>
