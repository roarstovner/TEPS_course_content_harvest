# Audit findings — output schema

Every per-institution review agent writes exactly one JSON object in this
format, whatever the check. A fixed schema keeps the reports
machine-mergeable: `R/audit_aggregate.R` verifies them and merges them into
one ranked cross-institution report.

The report is an **audit of a pipeline step**, not a coding of course plans:
each finding is a *type* of problem with that step, backed by examples.

## Output (single JSON object)

```json
{
  "check": "sections",
  "institution": "nord",
  "model": "sonnet",
  "n_courses_reviewed": 28,
  "n_suspects_reviewed": 20,
  "overall_assessment": "1-3 sentence summary of quality for this institution.",
  "findings": [
    {
      "target": "assessment",
      "error_type": "merged_sections",
      "severity": "high",
      "prevalence": "widespread",
      "example_course_ids": ["nord_XXX_2023_autumn_1", "nord_YYY_2022_spring_1"],
      "evidence": "Verbatim quote copied from the packet showing the problem.",
      "description": "What is wrong, in terms of the check's rubric.",
      "root_cause_hypothesis": "Where in the pipeline this originates.",
      "suggested_fix": "A concrete, deterministic change to make.",
      "confidence": "high"
    }
  ]
}
```

## Field reference

### Top level

| Field | Type | Notes |
|---|---|---|
| `check` | string | `sections`, `fulltext` or `anonymization`. |
| `institution` | string | The institution reviewed. |
| `model` | string | The model the dispatcher named in your prompt. |
| `n_courses_reviewed` | integer | Courses actually inspected (should equal the packet size). |
| `n_suspects_reviewed` | integer | Of those, how many were `[SUSPECT]`. |
| `overall_assessment` | string | 1–3 sentence quality summary. |
| `findings` | array | Zero or more finding objects. `[]` is a valid answer when nothing is wrong. |

### `findings[]`

| Field | Type | Required | Notes |
|---|---|---|---|
| `target` | enum | yes | What part is affected. Allowed values are check-specific: listed in the packet header and explained in the check's recipe. |
| `error_type` | enum | yes | Check-specific, as above. |
| `severity` | enum | yes | `high` \| `medium` \| `low` — impact on data quality (see below). |
| `prevalence` | enum | yes | `widespread` \| `common` \| `occasional` \| `rare` — how often within this institution's packet. |
| `example_course_ids` | array<string> | yes | 1–3 `course_id`s **exactly as they appear in the packet**. |
| `evidence` | string | yes | A short **verbatim** quote (≤ ~200 chars) copied from the packet. Use `…` to elide. Do not paraphrase inside the evidence field — explanation goes in `description`. |
| `description` | string | yes | What is wrong, referencing the rubric definition it violates. |
| `root_cause_hypothesis` | string | no | Best guess at the pipeline origin (file / function / config field / pattern). |
| `suggested_fix` | string | yes | A concrete, deterministic change (e.g. "add pattern X mapped to Y in R/section_heading_map.R", "strip lines matching Z in .anon_uis()"). |
| `confidence` | enum | yes | `high` \| `medium` \| `low` — your confidence that this is a real problem. |

### Severity

- `high` — wrong data a downstream analysis would silently use: content in
  the wrong place, content lost, personal data left in, whole records wrong.
- `medium` — noticeable degradation that a careful analyst would have to work
  around: partial loss, recurring noise inside otherwise-correct text.
- `low` — cosmetic or rare: whitespace, stray labels, placeholders.

## Aggregation rules

- **One finding per problem type**, not per course. If 12 courses show the
  same defect, write one finding, set `prevalence`, and give up to three
  examples.
- Split a finding when the root cause differs, even if the symptom looks the
  same (e.g. a heading pattern missing vs. a selector cutting the page).
- **Verification is mechanical.** `R/audit_aggregate.R` rejects findings whose
  `example_course_ids` are not in the packet or whose `evidence` does not
  occur verbatim in the packet (case and whitespace are ignored). Copy quotes;
  do not retype them.
