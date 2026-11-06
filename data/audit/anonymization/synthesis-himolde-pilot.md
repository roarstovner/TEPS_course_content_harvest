# Anonymization audit, HiMolde pilot (2026-11-06)

Reviewer: Codex gpt-6.1-sol, packet `data/audit/anonymization/packets/himolde.md`
(28 courses: 20 suspects, 8 random), findings
`data/audit/anonymization/findings-codex/himolde.json`, report
`data/audit/anonymization/findings-codex_report.md`. All 3 findings verified.

## Summary

- Coordinators (excluded from the facts box), e-mails and semester labels
  are removed; reference authors and years stay. 18 of the 20 `name_like`
  flags are false alarms (course terms, programme titles, organisations).
- A guest lecturer is named in two plans, in the plan's own text:
  "Lecturer:[name], Technical University of Denmark" (LOG904-130
  2018, in the build) and "Lecturer:" over a biography of the lecturer
  (IDR804-019 spring 2022, whose page the build drops as an earlier
  version). `privacy_hits` does not see them (no label it knows).
  Fixed in the working tree, uncommitted: himolde's `exclude` drops a
  paragraph opening with a bold "Lecturer" and, after a bare "Lecturer:", the
  paragraph under it. `preview_change("himolde")`: these two pages only.

## Gate (anonymization number)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| privacy_hits | 0 | 48 | 0 | FALSE |

## Findings

1. **high** (reviewer: medium; personal data left in): the guest lecturer
   names above (himolde_LOG904-130-1_2018_autumn_1,
   himolde_IDR804-019-1_2022_spring_1). Seminar courses (LOG904-xxx, IDR80x)
   have guest lecturers; the full harvest will have more.
2. low: a clock time left, "starting at 9:15" (LOG904-140 2022).
3. low: "The current reading list for can be found in Leganto": "for" left
   after the semester's removal (IBE321, TRA730, SAF230 2025). The
   reading_list row goes with the placeholder fix; the line stays in
   course_plan.

## Verdict

**not ready** until the lecturer fix is committed and rebuilt; with it,
**ready**: no personal data left in the sample. The fix is in the config
and rebuilt from the raw store; no new harvest.
