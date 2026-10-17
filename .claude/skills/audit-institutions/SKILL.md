---
name: audit-institutions
description: Audit one pipeline step across institutions with one review agent per institution — section extraction (`sections`), fulltext extraction (`fulltext`) or anonymization (`anonymization`). Builds review packets in R, dispatches parallel review agents, verifies their findings mechanically and writes a ranked cross-institution report plus a synthesis; with --pilot, a verdict on whether each institution is ready for its full harvest. Use when asked to check, verify, audit or QA a pipeline step "across all institutions" or "with one agent per institution", or to re-check institutions after a fix.
argument-hint: <sections|fulltext|anonymization> [inst ...] [--pilot] [--model sonnet|opus|haiku] [--tag label]
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
- `--pilot`: draw each institution's packet from its pilot sample only
  (`data/pilot/{inst}.csv`, README "Piloting an Institution"); the places
  the suspects leave unused go to random controls. Pass it on to the
  prepare script. The synthesis then ends with a verdict per institution
  (step 5).
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
- If a harvest is running (an R session writing `data/raw/{inst}/`), audit only
  institutions that already have a raw harvest, and say so.
- Build packets: `Rscript R/audit/prepare_{check}.R [inst ...] [--pilot]`.
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
  `{CHECK}`, `{INST}`, `{MODEL}`, `{WORKDIR}` (absolute path of the repo or
  worktree that holds the packets), `{PACKET}` =
  `data/audit/{check}/packets/{inst}.md` and `{OUT}` =
  `data/audit/{check}/findings[-{tag}]/{inst}.json` filled in.

Send up to ten Agent calls in one message so they run in parallel, then the
rest. Agents run in the background: wait for their completion notifications,
do not poll. An agent's own "wrote the file" is not proof: the aggregator
lists **stale reports** (findings file older than its packet). For each one,
resume that agent with SendMessage and ask it to write the file; if it has
ended, dispatch it once more.

### 4. Verify and aggregate

```bash
Rscript R/audit/aggregate.R {check} [--dir data/audit/{check}/findings-{tag}]
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
6. **Verdict** (`--pilot` only), see below.

#### Verdict (`--pilot`)

A pilot audit decides whether an institution is ready for its full harvest.
Its gate numbers are an input to that review, not a pass mark: read them in
the current build before the synthesis,

```bash
Rscript -e 'targets::tar_load_globals(); g <- pilot_gate("{inst}"); print(g[1:5]); print(setNames(g$examples, g$metric))'
```

`review` is TRUE for a number beyond its `PILOT_GATE` value
(`R/pipeline_metrics.R`); the audit finds why. Each check judges its own
numbers: `fulltext` judges `fetched` and `plan`; `sections` judges
`assessment`, `coursework`, `no_sections` and `unmapped_headings`;
`anonymization` judges `privacy_hits`. Look up a flagged number's `examples`
(the course ids against it) in the packet, the raw store or on the site.

End the synthesis with one verdict per institution:

- **ready for full harvest**: no number of this check flagged and no
  confirmed high-severity finding (verified, kept in step 4). Medium and low
  findings go to fix issues: text, sections and anonymization are rebuilt
  from the raw store and need no new harvest.
- **ready with known limits**: as above, except flagged numbers whose cause
  is on the site and that no fix removes: name each with its value,
  threshold and reason, e.g. DBH lists course-years the site has no page
  for, or an old layout the section extractor does not read (counted in
  `no_sections`, noted in `data/data_notes.qmd`).
- **not ready**: what must be fixed first. Any of these makes an
  institution not ready: a confirmed high-severity finding on the sample
  (plan text lost, cut or in the wrong section; the wrong page or year read;
  personal data left in); `privacy_hits` above 0; a flagged number with no
  reason found. Weigh most what only a new harvest fixes (URL builder, URL
  discovery, a page whose saved HTML lacks the plan): the full harvest would
  repeat it on every page.

Record the verdict in the institution's chainlink issue with the gate table
(`metric`, `value`, `n`, `threshold`, `review`), the reasons, the course ids
of `no_sections` and the ten most frequent unmapped headings (each mapped,
or not plan text). The institution is ready for its full harvest when all
three checks are; its verdict is the weakest of the three.

Do not change pipeline code during the audit — it reports; fixing is a
separate task.

### 6. Report

Tell the user in 5–10 lines what matters most, with paths to `synthesis.md`
and the report, and with `--pilot` the verdicts. Offer to (a) create chainlink issues for the high-severity fix
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

Packet size matters more than model choice for coverage: in the first full
run, Sonnet agents skimmed packets above ~200 KB (and still reported every
course as reviewed). Keep Sonnet packets to ~150 KB by lowering the
`SUSPECT_N`/`RANDOM_N`/`PLAN_TRUNC` tunables, or use `--model opus` for large
packets. See `data/audit/sections/model_comparison.md`.

## Files

| Path | Role |
|---|---|
| `checks/{check}.md` | Recipe: default model, pipeline, rubric, enums, known failure modes |
| `agent_prompt.md` | Prompt template for the review agents |
| `findings_schema.md` | JSON schema the agents write |
| `R/audit/utils.R` | Shared helpers; `AUDIT_CHECKS` holds the allowed enums (keep in sync with the recipes) |
| `R/audit/prepare_{check}.R` | Deterministic pre-pass + packet builder per check |
| `R/audit/aggregate.R` | Verification, merge, comparison with previous run, report |
| `data/audit/{check}/` | `packets/` (gitignored), `manifest.csv`, `sample.csv`, `findings/`, `findings_report.md`, `synthesis.md` |

## Adding a check

1. `checks/{name}.md` with the same sections as the existing recipes.
2. Enums in `AUDIT_CHECKS` in `R/audit/utils.R`.
3. `R/audit/prepare_{name}.R`: deterministic flags → one score per course →
   `audit_sample()` → packet via `audit_packet_header()` → `audit_write_index()`.
