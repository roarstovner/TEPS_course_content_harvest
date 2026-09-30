---
name: audit-institutions
description: Audit one pipeline step across institutions with one review agent per institution — section extraction (`sections`), fulltext extraction (`fulltext`) or anonymization (`anonymization`). Builds review packets in R, dispatches parallel review agents, verifies their findings mechanically and writes a ranked cross-institution report plus a synthesis. Use when asked to check, verify, audit or QA a pipeline step "across all institutions" or "with one agent per institution", or to re-check institutions after a fix.
argument-hint: <sections|fulltext|anonymization> [inst ...] [--model sonnet|opus|haiku] [--tag label]
---

# Audit institutions

Each institution publishes course plans differently, so pipeline steps fail in
institution-specific ways. This skill audits one step for every institution in
parallel: a deterministic R pre-pass picks suspect courses, each review agent
gets a self-contained packet for one institution, and the main session
verifies, merges and synthesises what comes back.

The main session orchestrates; it does not review packets itself. Fresh
context per institution is the point.

## Arguments

- `<check>` (required): the name of a recipe in `checks/` — `sections`,
  `fulltext` or `anonymization`. Ask if missing.
- `[inst ...]`: institutions to audit (default: all that get a packet).
- `--model`: model for the review agents (default: the recipe's default).
- `--tag <label>`: write findings to `data/audit/{check}/findings-{label}/`
  instead of `findings/`. Use for model comparisons and experiments that must
  not overwrite the canonical findings.

## Procedure

### 1. Read the recipe

Read `checks/{check}.md`. It names the default model, the inputs and how to
rebuild them, the packet builder, and the rubric the agents apply.

### 2. Fresh inputs, then packets

- Follow the recipe's **Pipeline** section. Rebuild stale inputs in the order
  given; the prepare scripts stop with a message when an input is older than
  its source.
- If a harvest is running (an R session writing `data/html_*.RDS`), audit only
  institutions whose `html_{inst}.RDS` is already written, and say so.
- Build packets: `Rscript R/audit_prepare_{check}.R [inst ...]`.
- Sanity-check the printed summary and skim the head of one packet (not
  empty, not garbled, course ids present). Note institutions with very few
  courses; their findings carry less weight.

### 3. Dispatch one review agent per institution

For each institution in `data/audit/{check}/manifest.csv` (or the requested
subset), call the Agent tool with:

- `subagent_type`: `general-purpose`
- `model`: `--model` or the recipe default
- `description`: `Audit {check}: {inst}`
- `prompt`: everything below the line in `agent_prompt.md`, with
  `{CHECK}`, `{INST}`, `{MODEL}`, `{WORKDIR}` (absolute repo path),
  `{PACKET}` = `data/audit/{check}/packets/{inst}.md` and
  `{OUT}` = `data/audit/{check}/findings[-{tag}]/{inst}.json` filled in.

Send up to ten Agent calls in one message so they run in parallel, then the
rest. Agents run in the background: wait for their completion notifications,
do not poll. If an agent fails or writes no file, dispatch it once more.

### 4. Verify and aggregate

```bash
Rscript R/audit_aggregate.R {check} [--dir data/audit/{check}/findings-{tag}]
```

Read `data/audit/{check}/findings_report.md` (or `findings-{tag}_report.md`).
For every finding marked ✗ (unknown course id, or evidence not found verbatim
in the packet), open the packet at the cited course and decide:

- real problem, paraphrased quote → replace `evidence` in the JSON with a
  verbatim quote;
- not real → delete the finding from the JSON and record why for the
  synthesis.

Re-run the aggregator until no ✗ is left unexplained. The report also lists
patterns seen in 3+ institutions and, when the JSON files exist at `HEAD`,
what changed since then.

### 5. Synthesise across institutions

Write `data/audit/{check}/synthesis.md` (`synthesis-{tag}.md` for a tagged
run). One review agent cannot see that two institutions fail the same way;
this step can. Cover:

1. **Summary** — 3–6 bullets.
2. **Cross-institution patterns** — findings that share a root cause (the same
   heading pattern, the same generic regex, the same strategy), so one fix
   helps several institutions.
3. **Prioritised fix list** — for each: where (file, function, pattern or
   config field), which institutions, 1–2 example `course_id`s, expected
   effect. Check the root cause in the code before writing it down; the
   agents' `root_cause_hypothesis` is a guess.
4. **Changes since the previous run**, when the report has that table:
   fixed, persisting, new.
5. **Rejected findings** and why.

Do not change pipeline code during the audit — it reports; fixing is a
separate task.

### 6. Report

Tell the user in 5–10 lines what matters most, with paths to `synthesis.md`
and the report. Offer to (a) create chainlink issues for the high-severity fix
items and (b) re-run the affected institutions once fixes are in.

## Re-running after a fix

Commit the previous findings first (they are the baseline), rebuild the
inputs, then prepare, dispatch and aggregate for the affected institutions
only. Prepare scripts keep the other institutions' rows in `manifest.csv` and
`sample.csv`, and sample with a fixed seed, so unchanged data gives the same
courses — a direct before/after. The aggregator compares with `HEAD`
automatically (`--compare <ref>` for another baseline, `--compare none` to
skip).

## Comparing models

Run the same packets twice for 2–3 institutions, e.g. the default run plus
`--model opus --tag opus`, aggregate both, and compare in
`data/audit/{check}/model_comparison.md`: agreement on high-severity
findings, real findings only one model found, verification failures, and
false positives. Pick institutions of different difficulty (one clean, one or
two with many problems).

## Cost

Each agent reads its whole packet, typically tens of thousands of tokens; a
full run over all institutions costs millions of input tokens. Use
institution subsets when iterating on a fix.

## Files

| Path | Role |
|---|---|
| `checks/{check}.md` | Recipe: default model, pipeline, rubric, enums, known failure modes |
| `agent_prompt.md` | Prompt template for the review agents |
| `findings_schema.md` | JSON schema the agents write |
| `R/audit_utils.R` | Shared helpers; `AUDIT_CHECKS` holds the allowed enums (keep in sync with the recipes) |
| `R/audit_prepare_{check}.R` | Deterministic pre-pass + packet builder per check |
| `R/audit_aggregate.R` | Verification, merge, comparison with previous run, report |
| `data/audit/{check}/` | `packets/` (gitignored), `manifest.csv`, `sample.csv`, `findings/`, `findings_report.md`, `synthesis.md` |

## Adding a check

1. `checks/{name}.md` with the same sections as the existing recipes.
2. Enums in `AUDIT_CHECKS` in `R/audit_utils.R`.
3. `R/audit_prepare_{name}.R`: deterministic flags → one score per course →
   `audit_sample()` → packet via `audit_packet_header()` → `audit_write_index()`.
