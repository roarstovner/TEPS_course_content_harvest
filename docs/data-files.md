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
<td>offerings, plans and sections of a finalized release, both frames
(the teacher-ed frame’s in the folder, all courses’ in
<code>all/</code>), and <code>study_types.csv</code> (none yet: the
first comes when coding starts)</td>
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
public. The plans are public pages too: anonymization improves the data
but is not legally required, and correct sections matter as much
(`methods.md` “Anonymization”).

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
<td><code>data/input/dbh_exams_{date}.parquet</code>,
<code>dbh_credits_{date}.parquet</code></td>
<td>DBH tables 905 (exam registrations) and 900 (credits earned) per
course and semester, which course-years had students; dated apart from
the course list</td>
<td>none (counts)</td>
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
<td><code>data/codes.csv</code></td>
<td>the value labels of the coded columns of the published files, DBH’s
or the project’s (“Variables”, <a href="codebook.md">Codebook</a>)</td>
<td>none</td>
<td>in git; released with the data</td>
</tr>
<tr>
<td><code>data/study_types.csv</code></td>
<td>DBH’s study types (table 347 <code>Studiumkode</code>): Norwegian
and English name, the project’s group (<code>study_type_group</code> of
the offerings, the browser’s tree), years, <code>te_frame</code> (the
teacher-ed frame’s four)</td>
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
<td>DBH metadata + the page’s <code>url</code>, <code>plan_id</code> and
the programme’s study type, no text</td>
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
<td><code>app/browser/public/data/</code></td>
<td>the browser’s data (target <code>browser_data_files</code>, <a
href="../app/browser/README.md"><code>app/browser/README.md</code></a>
“Data Contract”): plans, sections, course-years, metadata</td>
<td>anonymized (no <code>extracted_text</code>)</td>
<td>build output (gitignored), to be published with the page</td>
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
harvest of other courses leaves the teacher-ed files as they were. A
release keeps both frames (`finalize_release()`, since 2026-11-20): it
cuts nothing, and users filter by frame or study type. The teacher-ed
files are the ones target `frozen_check` checks once a release is
finalized (none is yet, and until the first it checks only the raw
store), the ones `release_data()` hands to the coding project by
default, the browser’s and `methods.md`’s; the data notes, the gaps and
the audits read those of all courses. Each frame has its metrics
snapshot, and target `coverage` its DBH offerings.

Per-institution prose notes are edited directly in
`data/data_notes.qmd`.

## Data Structure

### Input Data (`data/input/dbh_*_{date}.parquet`)

The harvest’s input is a dated snapshot of DBH’s course list, written by
`data-raw/courses.R` from DBH’s bulk files: `dbh_courses_{date}.parquet`
is table 208 (Emner), every course of every institution, year and
semester, all levels and statuses, one row per course and semester;
`dbh_programmes_{date}.parquet` is table 347 (Studieprogram), with the
programmes’ study type (`Studiumkode`). Beside it, with dates of their
own, are DBH’s student counts per course (code with its version, as
table 208’s) and semester, at the institutions with a config, all years,
from DBH’s API: `dbh_exams_{date}.parquet` is table 905 (Eksamensdata:
exam registrations `Oppmeldt totalt`, sat, passed) and
`dbh_credits_{date}.parquet` table 900 (Studiepoeng: credits earned,
`Ny produksjon totalt`, 1 = 60 credits). The pipeline reads the latest
of each, now the course list of 2026-11-08 and the counts of 2026-11-08
(methods.qmd, “Population and sampling frame”, says why they differ).
The counts can be refreshed alone, since they change only which
course-years are marked as having had students, not the frames:
`Rscript data-raw/courses.R counts` writes both tables of the day (about
ten minutes, one request per institution code and table, two seconds
apart); `Rscript data-raw/courses.R courses` the course list alone, and
without an argument both. `dbh_offered()` reads the latest counts for
each course-year (target `offered`), and every offering gets
`exam_registrations`, `credits_earned`, `had_students` and
`counts_provisional` (`add_offered()`; see “Variables” and methods.qmd):
its course-year’s counts, whether it had students (either count above
0), and whether the year’s counts can still grow, which follows from the
counts’ own date (the years after the last one whose autumn both tables
have). A new snapshot gets a new date and the old ones stay. The
snapshots keep DBH’s column names. `dbh_courses()` reads the latest (or
the one of `date`), gives DBH’s columns the pipeline’s names
(`DBH_COLUMNS`; see “Variables”) and adds `institution` and
`course_code`:

``` r
courses <- dbh_courses("teacher_ed")
courses |> slice(1:2)
```

    # A tibble: 2 × 25
      institution institution_code institution_name department_code department_name 
      <chr>       <chr>            <chr>            <chr>           <chr>           
    1 samas       0217             Samisk høgskole  510000          Institutt for s…
    2 samas       0217             Samisk høgskole  510000          Institutt for s…
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

A study type (`Studiumkode` in table 347) is DBH’s classification of a
programme: a profession (SYK nursing, ING engineering, PRAKTPED the
practical-pedagogical education), a programme model (GLU1-7, IMALU1-7)
or a broad field (HUMAN, REALFAG, SAMVIT). DBH registers it per
programme and semester; a programme can change type within a year (956
programme-years in the snapshot of 2026-11-08), never within a semester.
`data/study_types.csv` lists the study types of the latest snapshot with
DBH’s Norwegian name, an English name, a group for the browser’s
study-type tree (teacher education, health and welfare, broad fields and
so on), the years they appear, and `te_frame`. The teacher-ed frame is
four of them (INTMASTER, IMALU1-7, IMALU5-10 and LUPE,
`TEACHER_ED_STUDIUM`; methods.qmd, “Population and sampling frame”),
marked `te_frame`; the other teacher education study types (GLU,
PRAKTPED, BLU and others) are in the group but outside the frame.
`tests/testthat/test-study-types.R` checks that the file covers the
latest snapshot and that `te_frame` matches `TEACHER_ED_STUDIUM`: a new
snapshot with a new study type needs a row added by hand.

### Study Types of the Offerings

Every offering carries the study type of its programme that semester
(`programme_study_types()`, `add_study_types()`; methods.qmd, “Study
type of an offering”): `study_type_code` (`Studiumkode`) and
`study_type_name` (`Studnavn`, DBH’s Norwegian name), from table 347 of
the same snapshot as the course list, joined on `institution_code`,
`programme_code`, `year` and `semester`; and `study_type_group`, the
project’s group of the type, column `group` of `data/study_types.csv`
(eight groups made for the browser on 2026-11-19, not a DBH
classification). They are NA where the programme has no table 347 row
that semester (one offering in the snapshot of 2026-11-08). The study
type is that of the one programme DBH registers the offering under
(table 208 has one programme per course and semester): a course that
other programmes use too shows only that programme’s type, and a course
registered under different programmes in different semesters has the
type of each. The file `study_types.csv` goes with every release as the
definition of the groups.

The offerings keep DBH’s keys, so other DBH tables join to them:
`institution_code` (DBH’s institution code), `programme_code` (unique
only within an institution), `course_code_raw` (DBH’s course code with
its version number, as in tables 208, 900 and 905), `year` and
`semester` (0 winter, 1 spring, 2 summer, 3 autumn). A programme table
(such as 347) joins on `institution_code`, `programme_code`, `year` and
`semester`; a course table (such as 900 or 905) on `institution_code`,
`course_code_raw`, `year` and `semester`.

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

### Keys of the Published Files

``` mermaid
erDiagram
    course_plans |o--o{ course_offerings : "plan_id (NA without a plan)"
    course_plans ||--o{ plan_sections : "plan_id"
    course_offerings {
        string course_id PK
        string plan_id FK
        string plan_content_id "text fingerprint, not a key"
    }
    course_plans {
        string plan_id PK "institution_course_code_hash10"
        string plan_content_id "text fingerprint, not a key"
        string institution
        string course_code
    }
    plan_sections {
        string plan_id PK, FK
        string section PK
        string plan_content_id "text fingerprint, not a key"
        string text
    }
```

Join offerings, plans and sections on `plan_id`
(`{institution}_{course_code}_{first 10 characters of plan_content_id}`,
since 2026-11-19), which is unique per plan. `plan_content_id` is a hash
of the normalized text, shared by parallel course codes of an
institution that publish the same text, so it finds identical texts
across courses, and a join on it alone duplicates offerings
(methods.qmd, “Unit of analysis: the unique plan”).

### Variables (`data/variables.csv`)

`data/variables.csv` documents every variable of the published files
(`course_offerings.RDS`, `course_plans.RDS`, `plan_sections.RDS`, of
both frames): the file, its name, type, source (the column of the DBH
table, or how the pipeline gives it), meaning and codes, and `old_name`,
its name until 2026-10-16. `release_data()` hands it over with the data,
and `tests/testthat/test-dbh-input.R` checks it against the built files
and `DBH_COLUMNS`. `course_offerings_full.RDS` (internal) adds
`extracted_text`, `course_plan`, `course_plan_normalized` and
`plan_nchar` (and held `url` alone until 2026-11-26, when the published
offerings gained it).

The allowed values of the coded columns are in `data/codes.csv`
(variable, code, English label, source DBH or project, description; one
row per value, for every file with the column), except the study types
and their groups, which `data/study_types.csv` holds (`release_codes()`
joins the two). Open DBH code lists (language, subject, NUS, unit,
programme) are not listed. The [Codebook](codebook.md)
(`docs/codebook.qmd`) is rendered from these tables with the keys of the
files (`RELEASE_KEYS`), and so is the release’s `datapackage.json`, a
Data Package (v2) descriptor of the Parquet files of the release: field
types, the codes as enum constraints with labels, primary and foreign
keys (`datapackage_descriptor()`). `release_data()` writes the three
tables as Parquet with the descriptor (beside RDS copies until
2026-11-26; Parquet only since), stops if the tables do not match it
(`check_datapackage()`), and hands over `codes.csv` and the codebook;
`tests/testthat/test-release.R` checks the built teacher-ed files
against the descriptor. A new code in the data (a DBH level, a new
institution) needs a row in `codes.csv`, and the codebook rendered
again.

Since 2026-10-16 the DBH columns have English snake_case names
(methods.qmd, “Variable names”); their values are DBH’s (Norwegian
names, DBH’s codes), and `year_from` and `year_to` are integers (double
before). DBH’s snapshots, the raw files of the harvests until 2026-10-16
and the data handed to the coding project on 2026-10-15 (the retired
release data-2026-10-15) have DBH’s names: the pipeline renames them
where it reads such data (`dbh_rename()`, in `dbh_courses()`,
`read_raw()` and `release_data()`). Code that reads such data can rename
them by the same pairs:

``` r
v <- read.csv("data/variables.csv")
pairs <- unique(v[v$old_name != v$variable, c("variable", "old_name")])
old <- readRDS("../TEPS_course_content_coding/data/course_offerings.RDS") |>
  dplyr::rename(dplyr::any_of(setNames(pairs$old_name, pairs$variable)))
```

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 3%" />
<col style="width: 1%" />
<col style="width: 6%" />
<col style="width: 81%" />
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
<td style="text-align: left;">url</td>
<td style="text-align: left;">url</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">harvest</td>
<td style="text-align: left;">URL of the course page requested for the
offering: the page its plan was read from when plan_id is set; NA when
the harvest found no URL. At a site that shows only the current plan,
the page as fetched on harvested_at. Public pages, no personal data
(published from 2026-11-26).</td>
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
was fetched in. The data built on 2026-10-16 had “page” for NLA, whose
URL holds the academic year too.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">The offering’s plan in course_plans:
{institution}<em>{course_code}</em>{the first 10 characters of
plan_content_id}; NA when the offering has no plan. Join offerings to
plans and sections on it (added 2026-11-19).</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Fingerprint of the plan’s text: a SHA-256
hash of course_plan_normalized, so offerings with the same plan text
share it, also across parallel course codes of an institution. Not
unique per plan: join on plan_id. NA when the offering has no plan.</td>
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
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">exam_registrations</td>
<td style="text-align: left;">exam_registrations</td>
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
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">credits_earned</td>
<td style="text-align: left;">credits_earned</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">DBH table 900</td>
<td style="text-align: left;">Credits earned (900 Ny produksjon totalt)
in the course-year, summed over its semesters, in full-time equivalents
(60 credits = 1). The same on every offering of the course-year; 0 for a
course-year with no row in DBH. Counts of the same snapshot as
exam_registrations.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">had_students</td>
<td style="text-align: left;">had_students</td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">DBH tables 905 and 900</td>
<td style="text-align: left;">Whether the course-year had students:
exam_registrations &gt; 0 or credits_earned &gt; 0. Either count is used
because DBH zeroes small counts before summing, so exam_registrations
can be 0 for a course with 1-2 students. The same on every offering of
the course-year.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">counts_provisional</td>
<td style="text-align: left;">counts_provisional</td>
<td style="text-align: left;">logical</td>
<td style="text-align: left;">DBH tables 905 and 900</td>
<td style="text-align: left;">Whether the year’s counts
(exam_registrations, credits_earned) are not complete: a year after the
last one whose autumn DBH had published in both tables when the counts
snapshot was taken (DBH publishes a semester about one semester later).
The counts of a provisional year can still grow.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">study_type_code</td>
<td style="text-align: left;">study_type_code</td>
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
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">study_type_name</td>
<td style="text-align: left;">study_type_name</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">DBH table 347 Studnavn</td>
<td style="text-align: left;">DBH’s Norwegian name of the study type
(“Femårig lektorutdanning”); NA with study_type_code.</td>
</tr>
<tr>
<td style="text-align: left;">course_offerings</td>
<td style="text-align: left;">study_type_group</td>
<td style="text-align: left;">study_type_group</td>
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
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Plan id, unique:
{institution}<em>{course_code}</em>{the first 10 characters of
plan_content_id}. A plan is a plan_content_id, institution and
course_code (added 2026-11-19).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">plan_content_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Fingerprint of the plan’s text: a SHA-256
hash of course_plan_normalized. Parallel course codes of an institution
that publish the same text share it, so it is not unique per plan: join
on plan_id; it finds identical texts across courses.</td>
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
(double in the data built until 2026-10-16).</td>
</tr>
<tr>
<td style="text-align: left;">course_plans</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">year_to</td>
<td style="text-align: left;">integer</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">Last year of the offerings with this plan
(double in the data built until 2026-10-16).</td>
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
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">plan_id</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">derived</td>
<td style="text-align: left;">As in course_plans. A section is a plan_id
and section.</td>
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
<td style="text-align: left;">One of eight sections
(section_codebook.yml): course_content, learning_outcomes,
prerequisites, teaching_methods, coursework_requirements, assessment,
reading_list, and practice, which overlaps: its text is also in another
section (mostly teaching_methods), so section lengths per plan count it
twice.</td>
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
