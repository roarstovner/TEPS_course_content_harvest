# Section review agent — dispatch prompt template

One review agent is launched per institution (within Claude Code, via the Agent
tool — **no external/promptbook API calls**). Substitute `{INST}` and launch a
`general-purpose` (or `Explore`-class) agent per institution with the prompt
below. Agents may run in parallel.

---

You are auditing the quality of automated **section extraction** for Norwegian
course plans, for the institution **`{INST}`**.

Read these three files first (all in the repo root / data dir):

1. `section_codebook.yml` — the rubric. It defines the seven canonical sections
   (course_content, learning_outcomes, teaching_methods, assessment,
   coursework_requirements, prerequisites, reading_list) and the cross-institution
   rules for what text belongs in each. **This is the authority**, not the source
   headings.
2. `section_review_findings_schema.md` — the exact JSON output schema you must
   produce (fields, enums, aggregation rules).
3. `data/section_review/packets/{INST}.md` — your review material: a sample of
   course offerings, each with its **full course plan** (ground truth) and the
   **extractor output** (`sections_raw` rows) to audit. Rows already flagged by
   the deterministic pre-pass are marked `⚑ flags: …`.

Your task:

- For each course, compare the extractor output against the full course plan
  **using the codebook definitions**. Decide whether each extracted section's
  text is correct, complete, and correctly labelled; and whether any section
  present in the plan was missed.
- Explain the flagged (`⚑`) anomalies, and **actively look for issue types the
  flags did not catch** (subtle mislabeling, dropped sub-sections, wrong
  language, truncation, boilerplate-only content, etc.).
- **Aggregate**: report recurring problems as one finding each (with 1–3 example
  `course_id`s), not one finding per course.
- Pay special attention to the **assessment vs. coursework_requirements**
  split and to **boilerplate leaking into prerequisites** — these are the known
  cross-institution failure modes.

Output: write a single JSON object conforming to `section_review_findings_schema.md`
to `data/section_review/findings/{INST}.json` (create the dir if needed). Then
reply with a 3–5 line summary of the most important findings. Do not change any
other files.
