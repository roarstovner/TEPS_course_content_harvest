# Sections audit — Sonnet vs. Opus review agents

Run 2026-09-30, same packets (`data/audit/sections/packets/`, seed 42) for
three institutions of different difficulty: `uio` (13 courses, known heading
problems), `nih` (28 courses, mostly clean), `mf` (20 courses, all suspects,
badly broken). Sonnet findings: `findings/`; Opus findings: `findings-opus/`.

## Numbers

| | Sonnet | Opus |
|---|---|---|
| Findings (uio / nih / mf) | 5 / 3 / 13 | 7 / 3 / 10 |
| Mechanical verification | 20 of 21 pass; 1 invented course id (`uio_PROMO4-1_2025_spring_1`) | 20 of 20 pass |
| Wrote its output file | 2 of 3 — the nih agent reported success, but its Write had been refused; resumed and rewritten | 3 of 3 |
| Tokens per agent | 104k–141k | 146k–197k |
| Wall time per agent | 0.5–2 min | 3.5–6 min |
| Tool calls per agent | 5–13 | 21–30 (also queried the full data and read R code) |

## Agreement on what is broken

Both models found every **high-severity** problem the other found:

- **uio**: coursework requirements never get their own row; prerequisites are
  mostly "Opptak til emnet" admission boilerplate; every assessment row ends
  with the UiO exam-links block; HIS4015L's "Pensum" merged into
  teaching_methods; staff names and e-mails in `sections_raw` although
  `course_plan` is anonymized.
- **mf**: the real course description is never in course_content;
  arbeidskrav filed as assessment; first learning-outcome bullet dropped;
  outcome bullets routed to assessment/reading_list; no teaching_methods;
  prerequisites missed; exam-date tables in assessment; "Overlappende emner"
  in learning_outcomes; contact card (name, e-mail) appended to reading_list.
- **nih**: reading_list holds only pointers ("Se emnearkivet"); prerequisites
  = "Ingen emner i programmet" + the "Hvem kan ta dette emnet?" answer.

## Differences

**Only Opus found (medium severity):**
- nih: a WISEflow/Urkund plagiarism notice inside assessment in 9 of 28
  packet courses (28 of 81 institution-wide) — missed by Sonnet.
- uio: exam-gating statements ("Faglige krav for å kunne avlegge eksamen")
  kept inside assessment.

**Only Sonnet found (low):** nih words glued across lost paragraph breaks
("ungdomskulturer.Emnet").

**Root causes.** Opus explained *why*, checked it against the code and the
full data, and proposed fixes that follow from the cause: mf pages are
WordPress accordions with only three `h2`s, so `html_headings` falls back to
`text_split` on un-anonymized text, whose loose heading rule produces most of
the symptoms (fix: a details/summary pass as in `extract_sections_uib()`,
which it tried on PRA1005); uio's sub-blocks are `h3`; nih's prerequisites
problem is the "Hvem kan ta dette emnet?" pattern in the heading map. Sonnet
listed the same symptoms as separate findings (13 for mf) with shallower,
per-symptom fixes.

Severity calls were mostly the same; Opus rated nih prerequisites medium
where Sonnet said low.

## Recommendation

- **Keep Sonnet as the default for the sections sweep.** It finds the
  high-severity problems, costs less and runs 3–5 times faster, and the two
  reliability failures it showed (an invented course id, a false "file
  written") are caught mechanically by `R/audit/aggregate.R` (stale-report
  and id checks).
- **Use Opus (`--model opus --tag opus`) before fixing an institution**, when
  root causes matter more than coverage, and for the anonymization check
  (default already `opus`).
- The main session's synthesis step should supply root causes for Sonnet
  findings — it can read the code and see patterns across institutions.

Small sample (3 institutions, one run each); treat as indicative.

## Addendum: full Sonnet run (16 institutions, same night)

- Mechanical failures across 19 Sonnet reports (incl. re-runs): one invented
  course id (uio), one paraphrased quote (hvl, "Mer" for "Meir"), one false
  "file written" (nih). All caught by `R/audit/aggregate.R`; fixed by hand.
- **Coverage is the real weakness.** Five agents (nla, nord, oslomet, uia,
  uib) said they read only part of a 185–325 KB packet and checked the rest
  by headings or grep. Asking them to count only fully read courses in
  `n_courses_reviewed` did not help (uib still reported 28 of 28). The
  Opus agents in the comparison read everything and queried the full data.
- Adjusted recommendation: Sonnet is fine for packets up to ~150 KB; for
  larger packets either shrink them (fewer courses, lower `PLAN_TRUNC`) or
  use Opus.
