# Sections audit — synthesis (pilot: 6 institutions, after the URL fixes)

Run 2026-10-21 with `--pilot` on the build of commit a8ba3a5 (after the URL
fixes of 2026-10-21, the pilot retry harvests and the USN English-page
headings): hiof, nih, uib, uia, usn, uit. Each packet holds 16–20 plans
from the institution's pilot sample. One Sonnet review agent per
institution (medium effort); packets 110–150 KB; every agent read all its
courses. 23 findings for these 6, all pass mechanical verification (0 ✗).
The synthesis of the 2026-10-17 pilot run (ntnu, oslomet, inn, nord, hvl,
hivolda, nla) is in git history (a8ba3a5).

## Summary

- Sectioning is good at nih, uia and uib (low and medium findings only)
  and mostly good at hiof, usn and uit.
- **hiof**: a "Litteratur:" sub-heading inside the learning outcomes is
  mapped to `reading_list`, so the outcomes about literature leave
  `learning_outcomes` (one plan).
- **usn**: practice-course plans (2018) mix requirements and assessment:
  the descriptions of Arbeidskrav 2 and 3 stay in `assessment`, and the
  Praksis block in `teaching_methods` holds attendance rules, mid/final
  assessment and the "Varsel om ikke bestått" procedure.
- **uit**: arbeidskrav in the single "Eksamensform" block of 2008 plans
  stay in `assessment` (6 of the 27 plans without a coursework section);
  the other 21 have no coursework text at all.
- `no_sections` is 0 at all six. USN's `unmapped_headings` 0 is not
  measured: unmapped headings are not counted for the text-reader
  institutions (separate issue, found 2026-10-21).

## Cross-institution patterns

- **Requirements outside `assessment`**: the inline coursework split reads
  only the assessment section, so gate text under another heading stays
  where it is: under "Hva lærer du" (`uit_ODO-6302-1_2019_autumn_1`), under
  Praksis (usn), under learning activities (NTNU, Nord MET2000, known
  small error). Same class as before; not worth a general rule.
- **Placeholder reading lists** kept as sections: "The reading list is not
  published yet." (usn, 5 of 16), "Ta kontakt med instituttet/fakultetet",
  "Se hjemmeside." (uit 2008). Same class as the open placeholder issue
  for hvl, hivolda, nla and oslomet.
- **Admission text in `prerequisites`** (uia "Forkunnskapskrav": study
  places, Udir): same class as the open admission-pointer issue for nord,
  hvl, nla and oslomet.

## Prioritised fix list

1. **hiof: "Litteratur:" as a sub-heading of the learning outcomes** (high,
   rare; `hiof_SFT11608-1_2022_autumn_1`). Where: `R/section_heading_map.R`
   (the `litteratur` pattern) or the reader: a sub-heading under
   "Læringsutbytte" stays in its parent. Rebuild only.
2. **uit: arbeidskrav in the 2008 "Eksamensform" block** (medium,
   occasional; `uit_HIS-1007-1_2008_autumn_1`, `uit_BED-2017-1_2008_autumn_2`;
   the split works in `uit_HEL-3050-1_2008_autumn_1`). Where:
   `.split_inline_coursework()` / `.inline_coursework_regex` (uit has
   `section_inline_coursework`). 6 of the sample's 27 plans without
   coursework. Also map "Undervisnings- og eksamensspråk" to `.drop` (every
   2018+ page appends it to `teaching_methods`).
3. **usn: practice-course plans** (high by the agent, occasional, 2018
   practice courses; `usn_BS-PLVI30-V-1_2018_autumn_3`,
   `usn_050-E11-1_2018_autumn_1`). The Arbeidskrav 2/3 descriptions in
   `assessment`: USN has no `section_inline_coursework`; check whether
   turning it on helps without side effects. The Praksis block: by the
   codebook practice is `teaching_methods`; its attendance and assessment
   lines are the class above. Relate to the open question whether praksis
   should be a section of its own.
4. **Placeholders and boilerplate** (medium): usn "The reading list is not
   published yet.", usn's generic skikkethetsvurdering notice in
   `coursework_requirements`, uit 2008 placeholder reading lists; add to
   the open placeholder issue. uia admission text: add to the open
   admission-pointer issue.
5. **Low**: uib "Hjelpemiddel til eksamen" label and "Ingen" in
   `assessment`, uib placeholder "Anbefalte forkunnskaper", hiof 2007
   Underveis-/Sluttvurdering and a plagiarism notice in `assessment`, nih
   attendance lines under Vurdering, uia workload line in
   `coursework_requirements`.

## Changes since the previous run

The six were not in the 2026-10-17 pilot sections run. Against the full run
of 2026-10-13 (teacher education), the UiT arbeidskrav-in-assessment
pattern persists for the 2008 layout (the later layouts are split since the
NTNU commit of 2026-10-20).

## Rejected findings

None rejected. `uit_ODO-6302-1_2019_autumn_1` (high by the agent: the
whole "Om emnet" block in `course_content`, the arbeidskrav in
`learning_outcomes`) is kept but rated an unusual layout: the page has one
long "Om emnet" block and puts the arbeidskrav under "Hva lærer du"
itself; one specialist programme, accepted as a known limit (README,
"Piloting an Institution", step 4).

## Verdict (`--pilot`, sections: `assessment`, `coursework`, `no_sections`, `unmapped_headings`)

| inst | assessment | coursework | no_sections | unmapped | verdict |
|---|---|---|---|---|---|
| hiof | 1.00 (n 49) | .92 | 0 | .24 (n 774) | not ready |
| nih | 1.00 (n 22) | .91 | 0 | .01 (n 156) | ready |
| uib | 1.00 (n 46) | .83 | 0 | .37 (n 1774) | ready |
| uia | 1.00 (n 76) | .88 | 0 | .32 (n 877) | ready |
| usn | .97 (n 57) | .93 | 0 | 0 (n 566, not measured) | not ready |
| uit | .98 (n 66) | .59 ⚑ | 0 | .37 (n 718) | not ready |

- **hiof — not ready.** Fix 1 (rebuild only).
- **nih — ready.** Low finding only.
- **uib — ready.** Medium and low findings only.
- **uia — ready.** Medium and low findings only.
- **usn — not ready.** Fix 3 (practice courses), and `unmapped_headings`
  must be counted before it can be judged.
- **uit — not ready.** `coursework` .59 < .70: 21 of the 27 plans without
  a coursework section have no coursework text on the page (mostly 2008,
  a known limit); 6 have arbeidskrav the split misses (fix 2). With fix 2
  the share would be about .68, the rest on the site.

### Most frequent unmapped headings

All are facts-box labels or text about no part; none should map:

- hiof: Emnet er tilknyttet følgende studieprogram (programmes),
  Undervisningssemester (facts), Evaluering av emnet (course evaluation),
  Arbeidsomfang (workload, facts).
- nih: Reduksjon i studiepoeng (credit overlap).
- uib: Undervisningssemester, Karakterskala, Studienivå (studiesyklus),
  Studiepoeng, omfang, Masteremne, Lågaregradsemne (facts),
  Administrativt ansvarlig, Programansvarlig, Emneansvarlig (staff and
  units), Studiepoengsreduksjon (credit overlap).
- uia: Emnet er tilknyttet/knytt til følgende studieprogram (programmes),
  Studentevaluering (course evaluation), Undervisningsspråk, Tilbys som
  enkeltemne, Tilgang for privatister/privatistar (facts), Reduksjon i
  studiepoeng (credit overlap), Annen informasjon (mixed logistics).
- usn: none counted (see above).
- uit: Kontakt oss (contact), Andre år og semester / Previous years and
  semesters (year picker), Emnet administreres av, Emnetype (facts),
  Undervisnings- og eksamensspråk (facts; fix 2 drops it from
  `teaching_methods`), Studiepoengreduksjon (credit overlap),
  Søknadsfrist, Admission requirements, Information to incoming exchange
  students (admission).
