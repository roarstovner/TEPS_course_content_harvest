# Check: anonymization

Audits **anonymization**: whether `anonymize_fulltext()` (`R/anonymize.R`)
turns `extracted_text` into a `course_plan` with no personal data or
administrative dates left, without destroying course content.

- **Default model:** `opus` — a missed name ends up in the published data, so
  recall matters more than cost here, and the sample is small.
- **Audited output:** `course_plan`, recomputed from `extracted_text` with the
  current `R/anonymize.R` when packets are built
- **Ground truth:** `extracted_text` (shown as removed spans)
- **Rubric:** below

## Pipeline (orchestrator)

Needs only the harvest output; the packet builder runs the current
anonymization code itself, so a fix in `R/anonymize.R` can be re-audited
straight away.

```bash
Rscript R/audit/prepare_anonymization.R [inst ...]   # -> data/audit/anonymization/packets/
```

Per course the packet shows the anonymized plan and every span that
anonymization removed, as `… context [−removed−] context …`.

Pre-pass flags: `email`, `phone` (8-digit sequence), `name_label` (staff label
followed by a capitalised name), `name_like` (2+ mid-sentence capitalised
word pairs outside literature references), `admin_date` (dates, semester-year
labels, "Godkjent: 2021"), `removed` (unusually large share of the text
removed), `artifact` (empty brackets left by a removal).

## Rubric (review agent)

**Must be removed** (personal data and administrative dating):

- names of staff and other private persons in their role at the institution:
  course coordinators (emneansvarlig), teachers, contact persons, approvers
  ("godkjent av …"), student representatives;
- e-mail addresses, phone numbers, office/room numbers tied to a person;
- administrative dates and years: approval/revision stamps ("Opprettet 2020",
  "Godkjent 12.03.2021"), academic years ("2023/2024"), semester labels with
  a year ("Høst 2024"), timestamps, "Sist hentet fra FS …";
- institution-specific boilerplate the handlers target (see `R/anonymize.R`).

**Must be kept** (course content):

- authors, editors and titles in literature references (public, not
  personal data in this sense);
- historical and public persons named as subject matter (Piaget, Vygotsky,
  Ibsen, a minister named in a policy document);
- content years: "etter 1945", "NOU 2015:2", "Kunnskapsløftet 2020",
  "LK20", law years, publication years in references;
- place names, organisation names, programme and course names;
- the word "vår" meaning "our", and season words that are not semester labels.

### `target`

`person_name`, `email`, `phone`, `admin_date`, `boilerplate` (admin text a
handler should strip), `content_text` (course content damaged or removed),
`structure` (line/paragraph structure), or `all`.

### `error_type`

| Value | Meaning |
|---|---|
| `pii_leak` | Personal data left in `course_plan` (name, e-mail, phone of a private person). |
| `over_removal` | Course content removed that should have been kept (reference authors, content years, subject-matter names). |
| `text_corruption` | Removal damaged the surrounding text: words glued together, half-removed phrases, dangling labels that now read wrongly. |
| `boilerplate_left` | Administrative boilerplate that the institution handler is meant to strip is still there. |
| `admin_date_left` | Administrative dates or year stamps left in. |
| `structure_loss` | Line or paragraph structure destroyed by anonymization. |
| `other` | Anything else (explain in `description`). |

Rate every confirmed `pii_leak` at least `medium` severity, `high` when it
affects many courses at the institution.

## Known failure modes — look for these specifically

- Names in **formats the regexes do not expect**: "Etternavn, Fornavn",
  initials ("K. Nordmann"), names with particles ("van der", "de"), names on
  their own line under a label on the line above.
- **Labels in Nynorsk or English** that the handlers do not match
  ("emneansvarleg", "Course coordinator", "Teacher").
- **Contact blocks** at the end of pages (UiS "Fagpersoner", NTNU
  "Kontaktinformasjon") that survive in older page layouts.
- **Over-eager time/date regexes**: the time pattern `\d{1,2}:\d{2}` also
  matches chapter/verse and scale notation ("Joh 3:16", "1:50 000"); the
  academic-year pattern `\d{4}/\d{2,4}` also matches directive numbers such
  as "2008/98/EF".
- Year removal that leaves **dangling text** ("Gyldig fra" with nothing
  after it is fine; "i perioden – ble" is corruption).

## Not a finding

- `…[truncated N chars]` markers — the packet caps long text.
- Dangling admin labels whose value was removed ("Emneansvarlig:" alone) —
  expected and harmless.
- Author names in reading lists.

## Typical fix locations

`R/anonymize.R`: the institution handler `.anon_{inst}()` or
`.anon_generic()`; tests in `tests/testthat/test-anonymize.R`.
