# Course plan text: what goes in and what stays out

This spec says which parts of a course plan's web page (or PDF) go into the
published text: `course_plan`, the whole plan (`data/processed/course_plans.RDS`),
and the seven sections cut from it (`data/processed/plan_sections.RDS`). Every
rule in the pipeline that keeps or removes text must fit it. The reasons and
the effect of each decision, with numbers, are in `methods.md` ("What the text
holds" and the entries it points to). Which code mechanism does what is in
`README.md` ("What Each Mechanism May Remove"). A change to this spec gets a
`methods.qmd` entry in the same commit. Where the current data do not yet
follow the spec, it says so under "Known deviations".

## Four kinds of text

Every part of a plan's page is one of these:

1. **Personal data** are out of everything published, `course_plan` and
   sections alike: names of staff, e-mail addresses, phone numbers, contact
   and role lines with a name, signatures. A sentence that holds a name keeps
   the rest ("Ta kontakt med faglærer").
2. **Boilerplate** is out of `course_plan` and the sections. Text is
   boilerplate if
   - it is not written for this course's plan but belongs to the website
     around it, usually the same on every page: navigation, breadcrumbs,
     banners, year and semester pickers, buttons and link labels, script
     text, widgets (timetables, library and reading-list tools), site-wide
     link lists; or
   - it is about the page or the record, not the course: fetch, version and
     "last updated" stamps, PDF headers, footers and page numbers, approval
     stamps, and administrative dates and years (approval and publication
     dates, semester labels, exam dates, clock times; not years in the
     content or dates of cited documents).

   The test: would someone studying teacher education lose anything if it
   went? If in doubt, it is plan text.
3. **Plan text** is everything else on the page, and `course_plan` keeps it.
   That holds when the text is repeated in every plan of an institution (stock
   notices, admission rules) and when it belongs to none of the seven
   sections (title, facts box, admission, workload, course evaluation).
4. **Not a plan:** a page with no plan text about the course gives its
   offering no plan. Such pages are error pages, search pages, a later
   version of the plan shown in place of the requested one, and page shells:
   pages with no text about any of the seven sections below, only a title, a
   facts box, headings, placeholders, or plan text about no section (a
   credit-overlap box, a programme list). A page is a shell when no section
   can be cut from it and its plan text without the title and facts box is
   shorter than 1,500 characters; a longer page
   without sections stays a plan, since its text may be plan text that no
   heading marks (OsloMet's practicum courses).

## The sections

The seven sections are defined in `section_codebook.yml`: `course_content`,
`learning_outcomes`, `teaching_methods`, `assessment`,
`coursework_requirements`, `prerequisites` and `reading_list`. A section holds
the plan text about its part, whatever heading the page puts it under. Plan
text about no part stays only in `course_plan`. Where the line runs:

- **Assessment** holds the assessment design: what is assessed and how. That
  is the form, parts and weights, duration, individual or group work, the
  grading scale, the exam language and the permitted aids, with their heading
  ("Hjelpemidler til eksamen" over "Ingen" says that no aids are permitted;
  over only "-" it says nothing and goes). It also holds rules for the use of
  AI and for plagiarism, and temporary changes to any of these (COVID). An
  exam tab or exam table on the page counts too, with its header row.
  Logistics stay out: when, where and how to register (dates, rooms, exam
  systems, exam sessions, registration, resit and appeal rules, the examiner
  arrangement, the assessment semester).
- **Coursework requirements** hold what must be approved before the exam
  (arbeidskrav, compulsory attendance and activities, rules for absence from
  them), also when the page lists them under the assessment heading. When the
  page also holds the programme's or subject's plan (OsloMet's Fagplan), its
  rules for coursework and attendance join the course's own coursework
  requirements, after a lead that names the source.
- **Prerequisites** hold required and recommended prior knowledge and
  courses, not admission (to the programme, study right, who may apply).
- A part whose only text says that it is empty or elsewhere is not a section
  ("Ingen", "-", "Se fagplanen.", a pointer to Leganto). A pointer to another
  part of the same page takes that part's text (OsloMet "Se fagplanen.").

## The plan id

Two offerings share a plan when their `course_plan` texts are the same after
normalization (`methods.md`, "Unit of analysis"). Plan text that changes with
the year or semester while the plan itself stays the same is kept in
`course_plan` but left out of that comparison, so it does not split plans.
Examples are the responsible unit when a department is renamed, the
semester fields of a facts box or an exam tab, the title and facts box as
a whole (a campus, a programme list or a course-code variant that changes
from year to year), and plan text in no section that changes the same way:
admission programme lists, subject areas and admission flags, and stock text
an institution rewords (evaluation, examiner arrangement, credit overlap). For the same reason the shell rule counts the plan text
without the title and facts box.

## Page parts

| Page part | `course_plan` | Sections |
|---|---|---|
| Staff names, e-mail addresses, phone numbers; contact and role lines; signatures | out | out |
| Site navigation, breadcrumbs, banners, year pickers, buttons, link labels ("Kopier lenke", "Vis timeplan", "Alt om eksamen ved NTNU"), script text | out | out |
| Site-wide link lists and notices that are not part of the plan ("Mer om eksamen ved UiO …", UiB's exam footer, MF's library-access notice) | out | out |
| Timetable and exam schedule: weeks, rooms, lecture times, teaching start, exam dates and deadlines (MF's "Eksamensdatoer", whose exam forms the plan's assessment text gives) | out | out |
| Reading-list widget text (Leganto labels, "View online", item-type labels, tab bar); the list itself is plan text | out | out |
| Fetch, version and "last updated" stamps; PDF headers, footers, page numbers | out | out |
| Approval stamps ("Godkjent emneplan", "Godkjent av …" with date and approver) | out | out |
| Administrative dates and years, semester labels, clock times; "Emnet tilbys ikke 2025/2026" | out | out |
| Title, course code | in | out |
| Facts box: credits, level, language, semester, campus, programmes | in | out |
| Responsible unit (department) | in, not in the plan id | out |
| Admission, study right, who may take the course, programme lists | in | out |
| Workload, costs, course evaluation, credit overlap, change log | in | out |
| Assessment design, also from an exam tab or exam table (NTNU, INN, Volda, UiT) | in | assessment |
| Notices that say something about the assessment, stock text or not: AI use, plagiarism, COVID changes of the exam form | in | assessment or coursework requirements |
| Exam logistics: sessions, rooms, exam system, registration, resit and appeal rules, examiner arrangement | in (minus dates) | out |
| Placeholders and pointers with nothing more ("Ingen", "Se fagplanen.", "Informasjon om ChatGPT … finner du her") | in | out |
| Error page, search page, later version, page shell | no plan | no plan |

## Known deviations

The data built on 2026-10-15 (release `data-2026-10-15`) do not yet follow
the spec here:

- UiB's responsible unit, an unlabelled link in the page's sidebar, is not in
  `course_plan`. DBH's `Avdelingsnavn` gives every UiB offering its unit.
- Prerequisites hold admission text when it stands under a prerequisites
  heading: about 1,100 sections mention "opptak", most of them only admission
  or study right ("Forutsetter opptak til femårig lektorutdanning ...", "Emnet
  er forbeholdt studenter på følgende studieprogram") or a pointer to the
  admission rules. Most are NTNU (695) and Nord (264). Admission headings
  themselves are dropped.
- UiT's link to the university's exam page ("Alt du trenger å vite om før,
  under og etter eksamen, oppmelding, fravær klage og vitnemål: UiTs samleside
  om eksamen") is in `course_plan` (683 plans), though link labels are
  boilerplate. It is in no section.
