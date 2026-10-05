# Check: sections

Audits **section extraction**: whether `R/extract_sections.R` splits each
course plan into the seven canonical sections correctly.

- **Default model:** `sonnet`
- **Audited output:** `data/processed/sections_raw.RDS` (one row per course × section)
- **Ground truth:** the full anonymized `course_plan` (`data/interim/course_offerings_full.RDS`)
- **Rubric:** `section_codebook.yml` (repo root) — the definitions of the seven
  sections. It is the authority, not the headings the institution uses.

## Pipeline (orchestrator)

Rebuild whatever is stale, in this order. `R/audit/prepare_sections.R` refuses
to run on stale inputs.

```bash
Rscript -e 'targets::tar_make()'   # html_*.RDS -> course_offerings_full.RDS, sections_raw.RDS
Rscript R/audit/qa_sections.R      # -> data/audit/sections/sections_qa_suspects.RDS (deterministic pre-pass)
Rscript R/audit/prepare_sections.R [inst ...]   # -> data/audit/sections/packets/
```

Pre-pass flags shown in packets (`⚑ flags: …`): `empty`, `short`/`long`
(length outlier within institution × section), `blob` (one section ≥ 85% of
the plan), `leak->X` (text contains a heading of section X at a line start),
`dup_in_course`, `boilerplate` (identical text in ≥ 25 courses). Up to five
suspects per packet are courses with plan text but **no sections at all**
(shown as "no sections extracted"); judge what the extractor missed.

## Rubric (review agent)

Read `section_codebook.yml` first. For each course compare the extractor rows
with the full plan and ask: is every section's text **correct** (belongs to
that section by the codebook's definition), **complete** (nothing cut off or
dropped), and **clean** (no admin boilerplate, navigation or exam logistics)?
Was any section present in the plan **not extracted at all**?

### `target`

One of the seven canonical sections (`course_content`, `learning_outcomes`,
`teaching_methods`, `assessment`, `coursework_requirements`, `prerequisites`,
`reading_list`), or `cross_section` (the problem spans several sections), or
`all`.

### `error_type`

| Value | Meaning |
|---|---|
| `missing_section` | A section clearly present in the plan was not extracted at all. |
| `truncated` | Section content is cut off / incomplete relative to the plan. |
| `wrong_content` | The text is mislabelled — it belongs to a different canonical section (e.g. coursework filed as assessment). |
| `merged_sections` | Two+ sections concatenated under one (the splitter failed to cut at a heading). |
| `split_section` | One logical section broken into several rows / lost across boundaries. |
| `boilerplate_only` | The section captured only generic admin boilerplate, not real content (e.g. prerequisites = Studentweb text). |
| `empty_placeholder` | Section is empty or a placeholder ("Ingen.", "-") yet emitted as a row. |
| `field_or_language_junk` | Wrong field captured (e.g. the language name "Engelsk" as assessment). |
| `duplicate` | Same content repeated across sections or rows. |
| `formatting_noise` | Tables, whitespace or navigation cruft polluting otherwise-correct text. |
| `other` | Anything else (explain in `description`). |

## Known failure modes — look for these specifically

- **assessment vs. coursework_requirements.** Arbeidskrav / obligatoriske
  aktiviteter are pass/approve gates, not graded assessment. Institutions often
  list them under a "Vurdering"/"Eksamen" heading; the codebook says they
  still belong in `coursework_requirements`.
- **Boilerplate in prerequisites.** Admission, study-right and Studentweb text
  under "Forkunnskaper/Opptak" headings is not a prerequisite.
- **Learning outcomes cut short.** Sub-headings (Kunnskap / Ferdigheter /
  Generell kompetanse) are sometimes treated as section boundaries, so only
  the first group survives.
- **Exam-logistics tables** (dates, rooms, durations, aids) inside assessment.
- **Headings that map to nothing**, so their text is appended to the previous
  section.

## Not a finding

- `…[truncated N chars]` markers — the packet builder caps long text; the
  extractor did not truncate it.
- A long `reading_list` — expected.
- Headings the institution uses that differ from the codebook, when the text
  still lands in the right section.

## Typical fix locations

`R/section_heading_map.R` (patterns and their order — first match wins),
`R/institution_config.R` (`section_strategy`, `section_heading_level`,
`section_heading_selector`), `R/extract_sections.R` (strategies and
`.clean_sections()`).
