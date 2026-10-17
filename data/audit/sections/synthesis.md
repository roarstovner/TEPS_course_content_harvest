# Sections audit — synthesis (pilot: 7 institutions)

Run 2026-10-17 with `--pilot` on the build of commit 7510144 (the 16 pilot
harvests): ntnu, oslomet, inn, nord, hvl, hivolda, nla. Each packet holds
plans of the institution's pilot sample (`data/pilot/{inst}.csv`, all
courses), each labelled by its source offering: 16–20 plans, packets
111–153 KB. One Sonnet review agent per institution; every agent read all
its plans. 34 findings for these 7; all pass mechanical verification after
one quote was lengthened (oslomet "Ingen info", too short to check). The
other 11 institutions' findings in `findings_report.md` are from the run of
2026-10-05 (synthesis in git history, 2147fa9).

## Summary

- Heading mapping is sound at all seven: the random controls land in the
  right sections, learning outcomes keep their groups (except INN's labels,
  below), and placeholders like "Ingen" are mostly dropped.
- **English-only plans at NTNU are pointers.** The Norwegian page fills
  each part with "See English version." (IMT4127 2022, IMT4312 2025,
  MACS490 2025, SMF1081 2022), so course content, outcomes and teaching
  hold only the pointer. The English page on www.ntnu.edu holds the plan
  (IMT4127 2022, checked 2026-10-17). Same cause as HVL and Nord in the
  fulltext audit: a fetch fix, before the full harvest.
- **Arbeidskrav stay in assessment at NTNU and Nord.** The spec puts them in
  coursework requirements also when the page lists them under assessment.
  NTNU has no inline split: 876 of its 2,297 assessment sections mention
  arbeidskrav, about 144 with a whole "OBLIGATORISKE ARBEIDSKRAV" block
  (`ntnu_LGU14008-A_2019_autumn_1`). Nord has the split
  (`section_inline_coursework`), but its pattern misses "AK:", "OD:",
  "Coursework requirements:", "To arbeidskrav." and an "Arbeidskrav:" block
  under Læringsaktiviteter; 14 of the 37 Nord plans without a coursework
  section hold such lines. Rebuild only.
- **NLA's English plans lose their teaching methods.** "Teaching and
  learning methods", "Introduction", "Final assessment" and "Syllabus" map
  to no section (`match_heading_to_section()` gives NA); 6 NLA plans.
  Rebuild only.
- The rest is low: admission pointers as prerequisites, placeholder and
  pointer parts kept as sections, resit rules and exam-place notices in
  assessment.

## Cross-institution patterns

1. **English-only plans saved as Norwegian pointers** (NTNU here; HVL and
   Nord in the fulltext audit): follow the English page at fetch time.
2. **Coursework inside assessment** (NTNU, Nord): `.split_inline_coursework()`
   (`R/extract_sections.R`): turn it on for NTNU and widen
   `.inline_coursework_regex` (abbreviations "AK"/"OD", English
   "Coursework requirements", a block headed "OBLIGATORISKE ARBEIDSKRAV"),
   and keep a line that names both a requirement and the graded exam in
   assessment too (Nord SOS5003 "Arbeidskrav og hjemmeeksamen (3 dager)"
   left assessment empty).
3. **Admission pointers as prerequisites** (Nord "Studenter må oppfylle
   gjeldene opptakskrav ved tilhørende studieprogram.", HVL "Studiets
   opptakskrav.", NLA "View the admission requirements for the MA programme
   in Global Journalism.", OsloMet PVUB7000): one drop pattern in
   `.clean_sections()`. Near the spec's known deviation on admission rules.
4. **Placeholder and pointer parts kept** (HVL "_", "Ja, vert spesifisert i
   semesterplan …"; hivolda Nynorsk "Sjå pensumliste for emnet her", "Ingen.
   Sjå opptakskrav i studieplanen."; NLA "Ingen." and "Se plan for
   praksisstudiet …" appended to teaching methods; OsloMet "Ingen info"):
   the spec says such a part is not a section; extend the placeholder and
   pointer patterns (Nynorsk variants included).

## Prioritised fix list

Before the full harvest (new fetches):

1. **NTNU English-only plans**: on "See English version." fetch
   `https://www.ntnu.edu/studies/courses/{CODE}/{YEAR}` (add www.ntnu.edu to
   `data/robots_txt.csv`). `ntnu_IMT4127-1_2022_autumn_1`,
   `ntnu_MACS490-1_2025_autumn_1`.

Rebuild only:

2. **NTNU coursework split** (high): pattern 2.
   `ntnu_LGU14008-A_2019_autumn_1`, `ntnu_NORX2001-1_2025_autumn_2`.
3. **NLA English headings** (high): map "Teaching and learning methods" →
   teaching_methods, "Introduction" → course_content, "Final assessment" →
   assessment, "Syllabus" → reading_list in `R/section_heading_map.R`.
   `nla_3GJ322-1_2019_autumn_1`, `nla_3GJ302-1_2025_autumn_1`.
4. **Nord coursework regex** (medium): pattern 2. `nord_PRO9011-1_2025_autumn_1`,
   `nord_MET2000-1_2022_spring_1`, `nord_SOS5003-1_2022_autumn_3`.
5. **Admission pointers** (low): pattern 3.
6. **Placeholders and pointers** (low): pattern 4, per institution
   (hvl, hivolda, nla, oslomet).
7. **Logistics in assessment** (low): OsloMet resit rights ("Studentens
   retter og plikter ved ny/utsatt eksamen …") and the Skikkethetsvurdering
   pointer; HVL "Tid og sted for eksamen blir opplyst på Studentweb."; Nord
   "Korrekte vurderingsform vises i StudentWeb ved oppmelding …". HVL
   teaching methods also take exam dates and material fees (NHB2, FK215).
8. **INN** (low): the learning-outcome group labels (Kunnskap,
   Ferdigheter, Generell kompetanse) are dropped from the text, leaving a
   repeated "Studenten"; the course-evaluation paragraph and the shared
   Praksisstudiet paragraph join teaching methods (LAN2300, 2MKRLE171-6).
9. **OsloMet** (low): "Vedtatt i avdelingsstyret. Godkjent av rektor." at
   the start of course content is an approval stamp (TOLKDIAPBT); BIOB3210's
   prerequisites end in a dangling "eller" after the admission alternative
   is cut.

## Changes since the previous run

Not a before/after: the previous run (2026-10-05) audited teacher-education
plans; these packets are all-courses pilot samples. Its NLA
`teaching_methods/wrong_content` (practice pointers) persists.

## Rejected findings

- Permitted aids, calculator rules and the exam table with its header row
  in assessment (oslomet medium, inn, hivolda, ntnu low): the spec puts them
  in assessment.
- hivolda: the qualifications-framework lead sentence of learning outcomes
  is the section's lead, as at other institutions.
- nord PRO9011 prerequisites "duplicate": the page holds the passage under
  both Forkunnskapskrav and Anbefalte forkunnskaper, in different wording.
- hvl LSU301: the practice period under "OBLIGATORISK LÆRINGSAKTIVITET" is
  a compulsory activity; coursework by the spec.
- ntnu MET420 repeated paragraphs and nla glued words ("developmentThis"):
  in the source.
- ntnu PPU4725 "Opptak til femårig lærerutdanning og beståtte eksamener i
  henhold til forskrift": the spec keeps that sentence whole.

## Verdict (`--pilot`, sections: `assessment`, `coursework`, `no_sections`, `unmapped_headings`)

| inst | assessment | coursework | no_sections | unmapped | verdict |
|---|---|---|---|---|---|
| ntnu | .96 (n 79) | .78 | 0 | .31 (n 1723) | not ready |
| oslomet | 1.00 (n 93) | .96 | 0 | .03 (n 2126) | ready |
| inn | .98 (n 59) | .98 | 0 | .51 (n 3528) | ready |
| nord | .89 (n 82) ⚑ | .55 ⚑ | 0 | .28 (n 1475) | not ready |
| hvl | .94 (n 79) | .89 | 0 | .02 (n 1137) | ready |
| hivolda | 1.00 (n 77) | 1.00 | 0 | .65 (n 1303) | ready |
| nla | 1.00 (n 67) | .93 | 0 | .52 (n 1421) | not ready |

`no_sections` is 0 at all seven.

- **ntnu — not ready.** English-only plans are pointers (fix 1, a fetch
  fix); arbeidskrav blocks in assessment (fix 2, rebuild).
- **oslomet — ready.** Low findings only.
- **inn — ready.** Low findings only; `unmapped_headings` .51 is the facts
  box.
- **nord — not ready.** `assessment` .89 < .90: 7 of the 9 plans come from
  2019/2022 pages with no assessment part on the page (a known limit), 2
  are English stubs (fulltext fix). `coursework` .55 < .70: 23 of the 37
  plans without a coursework section have no coursework text (a known
  limit), 14 have lines the inline split misses (fix 4). Not ready because
  of the English stubs (fulltext); the section fixes are rebuild only.
- **hvl — ready.** Low and medium findings only (the English stubs are the
  fulltext check's).
- **hivolda — ready.** Low findings only; `unmapped_headings` .649 is just
  under .65: facts labels with colons, "Evaluering og kvalitetssikring" and
  "Sensorordning" (examiner arrangement, logistics by the spec).
- **nla — not ready.** English headings unmapped (fix 3, rebuild only); the
  fetch fix is the fulltext check's.

### Most frequent unmapped headings

All are facts-box labels or plan text about no part; none should map:

- ntnu: course-details-portlet (template label), Fagområder (facts),
  Kontaktinformasjon (contact), Spesielle vilkår (admission conditions),
  Studiepoengreduksjon (credit overlap), Andre sider om emnet (links).
- oslomet: Emneoverlapp (credit overlap), Emneansvarlig (staff), Fagplan
  (programme plan; its coursework rules join coursework by the spec).
- inn: Emnekode, Fagområde, Fakultet, Studieprogram, Undervisningssemestre,
  Emnenivå, Studiepoeng, Undervisnings- og eksamensspråk, Emnenavn på
  engelsk: (facts), Statusmelding (banner).
- nord: Kostnader (costs), Type emne (facts), Adgangsregulering
  (admission), Overlappende emner (credit overlap).
- hvl: Faglig overlapping, Fagleg overlapping, Course reductions (credit
  overlap).
- hivolda: Eksamenssemester:, Emnekode:, Maksimumstal:, Studienivå:,
  Studiepoeng:, Undervisningssemester:, Varighet:, Undervisningsspråk:
  (facts), Evaluering og kvalitetssikring (course evaluation),
  Sensorordning (examiner arrangement).
- nla: Emnekode:, Emnenavn:, Studiepoeng:, Studieår:, Undervisningsspråk:,
  Steder:, Undervisningssemester: (facts), Arbeidsomfang (workload),
  Evaluering av emnet (course evaluation), Relevans i studieprogrammet (the
  course's place in the programme, not its subject matter). Outside the ten:
  the English headings of fix 3, which should map.
