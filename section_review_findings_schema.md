# Section-extraction review — agent findings schema

This is the **fixed output schema** every per-institution review agent must
emit. It is deliberately *not* a promptbook file — promptbook describes the
*ideal coded output* of a course plan; this describes an *audit report about the
extractor*. Keeping a fixed schema makes the 18 agent reports machine-mergeable
into one ranked issue list.

## What the agent is given

1. `institution_short` it is reviewing.
2. The rendered **section codebook** (`section_codebook.yml`) — the authority on
   what each canonical section means.
3. The deterministic **suspect rows** for that institution, filtered from
   `data/sections_qa_suspects.RDS` (course_id, section, flags, leak_sections,
   raw_text) — known anomalies to explain.
4. A **sample of full course offerings** for that institution: the original
   `extracted_text` (ground truth) alongside the extractor's `sections_raw`
   rows for the same `course_id`. The sample is stratified: all/most suspects
   plus a random slice of non-suspect courses (to catch issues the heuristics
   miss).

## The agent's task

For each sampled course, compare the rule-based `sections_raw` against the
ground-truth `extracted_text`, judged by the codebook definitions. Report every
distinct extraction problem **type** (not one finding per course — aggregate
recurring problems into a single finding with example course_ids). Actively look
beyond the seeded suspects for issue types the deterministic checks cannot see
(e.g. subtly mislabeled content, silently dropped sub-sections, wrong language).

## Output (single JSON object)

```json
{
  "institution_short": "nord",
  "n_courses_reviewed": 40,
  "n_suspects_reviewed": 28,
  "overall_assessment": "1-3 sentence summary of extraction quality for this institution.",
  "findings": [
    {
      "section": "assessment",
      "error_type": "merged_sections",
      "severity": "high",
      "prevalence": "widespread",
      "example_course_ids": ["nord_XXX_2023_autumn_1", "nord_YYY_2022_spring_1"],
      "evidence": "Short verbatim quote from raw_text showing the problem.",
      "description": "What is wrong, in terms of the codebook definitions.",
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
| `institution_short` | string | The institution reviewed. |
| `n_courses_reviewed` | integer | Total courses inspected. |
| `n_suspects_reviewed` | integer | Of those, how many were seeded suspects. |
| `overall_assessment` | string | 1–3 sentence quality summary. |
| `findings` | array | Zero or more finding objects (below). Empty array if none. |

### `findings[]`

| Field | Type | Required | Notes |
|---|---|---|---|
| `section` | enum | yes | One canonical section, or `cross_section` (spans several), or `all`. |
| `error_type` | enum | yes | See enum below. |
| `severity` | enum | yes | `high` \| `medium` \| `low`. Impact on data quality. |
| `prevalence` | enum | yes | `widespread` \| `common` \| `occasional` \| `rare` — how often within this institution. |
| `example_course_ids` | array<string> | yes | 1–3 real `course_id`s exhibiting it. |
| `evidence` | string | yes | A short verbatim quote (≤ ~200 chars) demonstrating the problem. |
| `description` | string | yes | What is wrong, referencing the codebook definition it violates. |
| `root_cause_hypothesis` | string | no | Best guess at the pipeline origin (heading map / strategy / selector / pre_fn). |
| `suggested_fix` | string | yes | A concrete, deterministic change (e.g. "add pattern X mapped to section Y", "cut at `<button>` boundary", "drop nodes matching Z"). |
| `confidence` | enum | yes | `high` \| `medium` \| `low` — the agent's confidence in this finding. |

### `section` enum

`course_content`, `learning_outcomes`, `teaching_methods`, `assessment`,
`coursework_requirements`, `prerequisites`, `reading_list`, `cross_section`,
`all`.

### `error_type` enum

| Value | Meaning |
|---|---|
| `missing_section` | A section clearly present in `extracted_text` was not extracted at all. |
| `truncated` | Section content is cut off / incomplete relative to the source. |
| `wrong_content` | The section's text is mislabeled — it belongs to a different canonical section (e.g. coursework filed as assessment). |
| `merged_sections` | Two+ sections concatenated under one (the splitter failed to cut at a heading). |
| `split_section` | One logical section broken into several rows / lost across boundaries. |
| `boilerplate_only` | Section captured only generic admin boilerplate, not real content (e.g. prerequisites = Studentweb text). |
| `empty_placeholder` | Section is empty or a placeholder ("Ingen.", "-") yet emitted as a row. |
| `field_or_language_junk` | Wrong field captured (e.g. language name "Engelsk" as assessment). |
| `duplicate` | Same content repeated across sections or rows. |
| `formatting_noise` | Tables/whitespace/nav cruft polluting otherwise-correct text. |
| `other` | Anything not covered above (explain in `description`). |

## Aggregation

Merge the per-institution JSON objects (one array of findings across all
institutions, tagged by `institution_short`), sort by `severity` then
`prevalence`, and post the result as a chainlink issue feeding the
deterministic-fix work (#198). Cross-check agent findings against the
deterministic flags in `sections_qa_suspects.RDS`: agreement raises confidence;
agent-only findings are the long tail the heuristics missed.
