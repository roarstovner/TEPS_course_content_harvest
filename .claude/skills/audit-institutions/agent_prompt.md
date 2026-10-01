# Review agent — dispatch prompt template

The orchestrator (see `SKILL.md`) fills in the `{PLACEHOLDERS}` and passes
everything below the line as the `prompt` of one Agent call per institution.

---

You are auditing one step of a data pipeline that harvests Norwegian course
plans (emnebeskrivelser). Check: **{CHECK}**. Institution: **`{INST}`**.
Repository: `{WORKDIR}` — all paths below are relative to it; use absolute
paths when you call tools.

Read these files first, in this order:

1. `.claude/skills/audit-institutions/checks/{CHECK}.md` — the recipe: what
   this check audits, the rubric, the meaning of every allowed `target` and
   `error_type`, and the known failure modes to look for.
2. `.claude/skills/audit-institutions/findings_schema.md` — the exact JSON you
   must produce.
3. `{PACKET}` — your review material: a sample of this institution's course
   offerings. `[SUSPECT]` courses were picked by a deterministic pre-pass
   (its flags are shown); `[RANDOM]` courses are controls. Read **all** of
   it — use several Read calls with `offset` if it is long.

Your task:

- Audit every course in the packet against the rubric in the recipe. Read
  each course in full; `n_courses_reviewed` counts only courses you read in
  full, so a partial review is visible in the report.
- Explain the pre-pass flags (are they real problems or false alarms?) **and
  actively look for problem types the flags cannot see**. The random controls
  are there so you can tell whether a problem is institution-wide.
- Aggregate: one finding per problem type, with 1–3 example `course_id`s.
- Copy `evidence` verbatim from the packet; findings with quotes that do not
  occur in the packet are rejected by the aggregator.
- You may read pipeline code under `R/` to sharpen `root_cause_hypothesis`
  and `suggested_fix`. Do not edit any file except your output file.
- Do not invent problems to fill the report. If the step works well for this
  institution, say so and return few or no findings.

Output: write one JSON object following the schema to `{OUT}`. If that file
already exists, Read it first (the Write tool refuses to overwrite a file you
have not read), then replace it completely — do not merge with the old
content. Check that the Write succeeded. Set `"check":
"{CHECK}"`, `"institution": "{INST}"`, `"model": "{MODEL}"`). Then reply with a
3–5 line summary of the most important findings.
