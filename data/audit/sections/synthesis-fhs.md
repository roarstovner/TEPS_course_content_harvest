# Sections audit, FHS full harvest (2026-11-05)

Build of 2026-11-05 04:40 (after the "Arbeidskrav og vurdering" fix,
29148a1). Reviewer: Codex gpt-6.1-sol, packet
`data/audit/sections/packets/fhs.md` (18 courses: 14 suspects, 4 random;
drawn from the full harvest, not the pilot sample), findings
`data/audit/sections/findings-codex/fhs.json`, report
`data/audit/sections/findings-codex_report.md`. All 10 findings passed the
aggregator's checks (course id and verbatim evidence); the high ones were
checked again in the packet and across all 838 FHS plans in
`plan_sections.RDS`, and some were downgraded (below). Baseline: the pilot
audit, `synthesis-fhs-pilot.md` (9086e77).

## Summary

- The two high findings of the pilot are gone. The "Arbeidskrav:" lines are
  in coursework_requirements and the assessment lines in assessment, in all
  five pilot plans, ING0321 included (its 2025 plan is the same text as
  `fhs_ING0321-1_2023_autumn_2`). Across all 838 plans: no coursework row
  holds a "Vurderingsform/Karakterskala" line apart from two run-on cases
  (below), and no assessment row starts a line with "Arbeidskrav".
- New high finding, section side: in 21 of the 35 FHS plans with
  prerequisites (mostly the Etterretningsskolen 2023 plans), sub-headings
  such as "Emnets oppbygning" and "Arbeidsformer og læringsaktiviteter" are
  not read as headings, so the organisation and teaching text after
  "Forkunnskapskrav" ends up in prerequisites.
- New high finding, narrow: the heading "Opptakskrav" maps to `.drop`, but
  at FHS it holds the course prerequisites ("Emnet forutsetter bestått
  eksamen i ... SPR1720 og SPR1721"). Eleven plans (2023-2025, Russian) lose
  that sentence from the sections; it stays in course_plan.
- Older plans (2018-2022) show no new high-severity problem. One run-on
  2018 plan keeps the exam in coursework, and 2022 engineering plans mention
  compulsory exercises only inside teaching_methods.
- Nothing needs a new harvest. All fixes are on the section side and need
  only a rebuild.

## Gate (pilot sample, current build)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| fetched | 0.46 | 100 | 0.80 | TRUE (fulltext; judged in the pilot) |
| plan | 0.46 | 100 | 0.80 | TRUE (fulltext; judged in the pilot) |
| assessment | 1.000 | 46 | 0.90 | FALSE (pilot 0.957) |
| coursework | 0.587 | 46 | 0.70 | TRUE (pilot 0.457) |
| no_sections | 0 | 46 | – | – |
| unmapped_headings | 0.00 | 603 | 0.65 | FALSE |
| privacy_hits | 0 | 46 | 0 | FALSE |

`coursework` is still below 0.70, and the cause is on the site. I checked
all 19 example plans: none has an "Arbeidskrav" text that went somewhere
else. Their plans have no coursework requirement, or only mention
"obligatorisk øving/laboratorieoppgave" inside Læringsaktiviteter (e.g.
fhs_ING2506-1_2022_autumn_1, fhs_ING2507-1_2022_autumn_1). In the full
build 467 of 838 plans (0.56) have coursework, about evenly across years
(0.48-0.66). `no_sections`: none.

## Verified findings

High (section side):

1. prerequisites holds the next sub-sections: "Emnets oppbygning" and
   "Arbeidsformer og læringsaktiviteter" are missing from the headings that
   were read (fhs_OPG3701-1_2023_spring_1, fhs_SPR2710-1_2023_spring_1,
   fhs_MILM3702-1_2023_spring_1; 21 plans: 2022 1, 2023 18, 2025 2). Where:
   the FHS reader (`section_*` fields) or the heading map for these labels.
2. "Opptakskrav" → `.drop` (`R/section_heading_map.R`, "opptakskrav") drops
   real FHS prerequisites. 11 plans, all with no prerequisites row
   (fhs_SPR2721-1_2023_spring_1). Where: an FHS exception, or keep sentences
   such as "forutsetter bestått" as prerequisites.

Medium:

3. course_content repeats the teaching block "Praktisk organisering og
   arbeidsformer / Pedagogisk innretning", which also appears in
   teaching_methods (fhs_LED3201-1_2022_spring_1; 6 plans). The reviewer
   rated this high. I downgraded it: the text is duplicated, not lost.
4. "Målgruppe" (admission and target group) is appended to course_content
   (fhs_OPG3701-1_2023_spring_1, fhs_AR301-1_2023_spring_1; 34 plans).
5. Assessment keeps examiner, resit and semester logistics
   (fhs_AR301-1_2023_spring_1, fhs_SPR2721-1_2023_spring_1).
6. The thesis specification (word limits) is under the site's own
   Arbeidskrav heading and so lands in coursework_requirements
   (fhs_OPG3302-1_2022_autumn_1; one thesis course). The reviewer rated
   this high.
7. A compulsory supervision session sits in an assessment paragraph, and the
   plan gets no coursework row (fhs_ING3511-1_2021_autumn_1; one sentence).
   The reviewer rated this high.
8. A coursework description stays in assessment prose ("Emnet inneholder
   ett arbeidskrav: ...") and the plan has no coursework row
   (fhs_MAKE4102-1_2022_autumn_2/2023/2024/2025, fhs_MAKE4103-1_2022_autumn_2;
   about 6 plans). Not in the reviewer's findings; I found it in the
   full-build check.
9. OPG3701: the identical approval sentence is in both teaching_methods and
   coursework_requirements.

Low:

10. An unlabelled "Emnet bygger på ING2320 og ING2321." stays in
    course_content and is not moved to prerequisites
    (fhs_ING3330-1_2025_spring_2). The reviewer rated this high. I
    downgraded it: the text is not lost.
11. Placeholders kept as sections: "Må bestås", "Blir publisert",
    "Avklares", "TBN.", "Kunngjøres", and a course_content of only
    "Hva lærer du" (fhs_CBU1506-1_2019_autumn_2,
    fhs_SPR2723-1_2024_autumn_1).
12. A run-on 2018 page puts the whole line "Obligatorisk arbeidskrav: Paper
    Vurderingsform: Skriftlig eksamen ..." in coursework, so the plan has no
    assessment (fhs_LED4101-1_2018_autumn_2; 1 plan). The same happens
    within a line in fhs_LOG3103-1_2025_spring_1, where the assessment is
    kept.

## Changes since the pilot

- Fixed: pilot findings 1 and 2 (the Arbeidskrav split). `assessment` rose
  from 0.957 to 1.000 and `coursework` from 0.457 to 0.587.
- Persisting: placeholders (pilot 5); course_content holding teaching or
  assessment material (pilot 4).
- New: findings 1, 2 and 8 above. They come from parts of the site the
  pilot sample did not reach (Etterretningsskolen, master courses).

## Verdict

By the skill's rule, sections are **not clean** yet: findings 1 and 2 are
confirmed high. Both are narrow (21 and 11 of 838 plans) and fixable from
the raw store with a rebuild, so no new harvest is needed. `coursework`
0.587 < 0.70 is a known site limit: the plans that lack coursework have no
Arbeidskrav text.
