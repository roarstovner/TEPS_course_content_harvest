# Anonymization audit — synthesis (all institutions, 2026-10-07)

18 institutions, one Opus review agent each (#231), on `course_plan`
recomputed from the current `extracted_text` (block model, after #299).
Packets: up to 20 suspects + 8 random controls; 8–28 courses per
institution. All 39 findings pass mechanical verification (0 ✗). Most agents
also scanned all their institution's offerings with the current code, which
found personal data **outside the samples** that the aggregator cannot
accept as findings; those leaks are verified by hand below. Report:
`findings_report.md`. The pilot synthesis (uio, mf, nih; 2026-09-30) is in
git history.

## Summary

- **Personal data is left in 13–20 published plans at five institutions**
  (uio, hvl, uit, usn, uis), all outside the samples: a staff-role word
  followed by a name *inside* a line ("… kontakt med <name> eller <name>",
  "Godkjent emneplan av dekan <name>", "Studiepoeng 15 Emneansvarlig <name>",
  "faglærer <name>"), campus labels with names (hvl), and the 2008–2011 uit
  timetable block. Every role-name rule in `R/anonymize.R` is anchored at
  the start of a line, and `privacy_check` uses the same regexes, so it is
  blind to these.
- Inside the samples **no personal data** is left at any of the 18; the
  institution handlers (uia multi-name blocks, uis staff lists, ntnu and mf
  contact blocks, nmbu, oslomet) work.
- **The generic date/season regexes do most of the damage**: season words
  half removed or cut out of compounds (uib 249 + 417 plans, uia, mf,
  hivolda), year paths in URLs and DOIs broken (`\d{4}/\d{2,4}`: uis 190, uit
  64, hiof 20), regulation and citation dates removed, while admin dates in
  other forms survive (nla em-dash academic years in all 249 plans; uis
  version stamp in 1,512; oslomet approval stamps in 853; usn approval stamp
  with a 2-digit year in 233).
- **inn**: 1,077 of 1,742 offerings with a plan show "Emnebeskrivelsen for
  valgt semester er ikke publisert enda. Her er den siste versjonen …" — a
  later plan version, not the requested year's. The anonymizer removes the
  notice, so nothing recorded it. Decided 2026-10-07: these offerings get no
  plan. The April 2026 backup has the same notice on the same pages.
- **ntnu** loses its exam tab (grading scale, weights) from `course_plan`:
  `.anon_ntnu()` cuts from "Kontaktinformasjon" to the end (3,287 texts); the
  sections keep it.

## Personal data outside the samples (verified)

| institution | where | plans | sections | layout |
| --- | --- | --- | --- | --- |
| uio | `uio_FYS5930L-2_2025_autumn_1` | 1 | 1 (teaching_methods) | "Ta kontakt med <name> eller <name>", initials |
| usn | `usn_MG2SA3-1_2018_autumn_2`, `_2019_autumn_1` | 2 | – | "Godkjent emneplan av dekan <name>" |
| usn | `usn_MG1SA5-1_2020_autumn_2`, `usn_MG2SA5-1_2020_autumn_2`, `usn_LRMF510-1_2024_spring_1` | 3 | 2 (reading_list) | "… avklares med faglærer <name>" |
| usn | 5 plans | 5 | – | "… emneansvarlig: <name>" mid-line |
| uis | `uis_MGL2042-1_2020_autumn_1`, `_2021_spring_3` | 2 | – | "Studiepoeng 15 Emneansvarlig <name>" (PDF line) |
| hvl | `hvl_MGBNA141-1_2022_spring_1` | 1 | – | "Bergen: <name>" / "Stord: <name>" under "Hjelpemidler ved eksamen" |
| uit | `uit_EXF-0723-1_2008_autumn_1`, `uit_LRU-1100-1_2011_autumn_1`, `uit_LRU-1125-1_2011_autumn_1`, `uit_PFF-1002-1_2011_autumn_2` (+ SOS-3905 per agent) | 4–8 | 4 (reading_list) | 2008–2011 "Undervisning" timetable: "Kontaktpersoner er <name>, <name> og <name>", glued "Foreleser<name>" |

A scan of the raw text for a staff-role word (`.staff_roles`, dean roles,
"kontakt med") followed by a capitalised name anywhere in a line finds 12
distinct strings, all real names (no organisation names), at uio, uis, uit
and usn.

## Cross-institution patterns

1. **Role + name inside a line** (uio, usn, uis; uit glued). `.role_name_line_regex`
   needs the role at line start; `.approved_by_name_regex` needs "Godkjent av
   <role>"; `.anon_uio()` strips a name only before "(e-mail)". One generic
   rule in `.anon_generic()` — keep the role, drop the name, allow initials
   and "og/eller/, <name>" — covers all of them, and as a `privacy_check`
   pattern it makes the check see the layout.
2. **Season rule in `.anon_generic()`** (uib, uia, mf, hivolda, oslomet). The
   lookbehind rule `(?<=semester|…[:\s]{0,3})(høst|vår|…)` removes only the
   first season of "Vår og haust" and has no trailing `\b`, so it cuts
   "Haust" out of "Haustsemesteret" (and reaches across a line break after a
   heading ending in "…eksamen").
3. **Year/date regexes hit content** (uis, uit, hiof, usn, nord, nla).
   `\b\d{4}/\d{2,4}\b` removes any year/number pair (URL paths, DOIs,
   "1980/2005"), not only academic years; `\d{1,2}\.\d{1,2}\.\d{4}` removes
   citation and regulation dates ("forskrift 28.10.2020 nr. 2474"); the
   `HH:MM` rule removes film/video running times; consecutive-year ranges in
   brackets and parliamentary sessions go ("[1955-1956]", "Meld. St. 21
   (2016–2017)"; the session years are removed by a test-backed choice, #229).
4. **Admin dates in forms the rules do not know** (nla, uis, oslomet, usn,
   hiof, nord, uib, nih, uit): em dash in year ranges; long-form dates after
   approval/update words ("14. november 2016", "13. desember 2021");
   2-digit-year dates ("16.12.20"); uis "CODE_1 Bokmål Versjon 31.Mai.2026";
   semester codes "H2025-V2026", "-H23".
5. **Handler drift**: mf library notice now ends "Tilgang til litteratur:"
   (colon), so the #228 rule matches 0 of 26; usn Leganto item types not in
   the list; hvl "Emnet tilbys ikke studieåret <year>." reads "not offered"
   once the year is gone; inn's "Engelsk" → "English" rewrite runs over the
   whole text (320 plans, course titles changed in 140).

## Prioritised fix list

1. **Personal data** (`R/anonymize.R`): generic mid-line role + name rule in
   `.anon_generic()` and in `.personal_data_patterns`; `.anon_uit()` cuts the
   2008–2011 timetable block (`\nUndervisning[ \t]{2,}` to the end); a hvl
   rule for "<campus>: <name>" lines. Examples above. Then `privacy_check`
   must stay empty and the raw-text scan must find nothing in published text.
2. **inn later-version pages get no plan** (`.anon_inn()` returns NA when the
   notice is on the page, as for the course-search page): 1,077 offerings;
   `methods.qmd` entry. Example `inn_2MPEL171S-3-1_2022_spring_1`.
3. **Season rule** (pattern 2): whole word with optional suffix
   (`semester(?:et)?|en|a`) and an optional "og/and <season>". Examples
   `uib_RELMIN643-0_2025_autumn_1`, `uia_NO-505-1_2024_autumn_1`,
   `mf_SAM2080L-1_2025_autumn_1`.
4. **Year/date rules** (pattern 3): `/` years only when consecutive (through
   `.drop_academic_year()`) and not inside a path (`(?<![/\w])…(?!/)`);
   keep dates followed by "nr."; examples `uit_PFF-1020-1_2018_autumn_1`,
   `hiof_LMUSAM11221-1_2022_spring_2`, `nord_NAT1002-1_2022_spring_3`.
5. **Admin dates** (pattern 4): em dash; uis version stamp; long-form dates
   after approval words; usn approval stamp with 2-digit year or month name;
   examples `nla_MGL5PE101-1_2024_autumn_1`, `uis_MGL3066-1_2025_autumn_1`,
   `oslomet_M5GNA2100-1_2019_spring_2`, `usn_MG1EN3-1_2024_autumn_1`.
6. **Handlers** (pattern 5): mf library notice with colon; inn "Engelsk"
   rewrite only on the language line (or not at all); hvl offering notice
   line; usn Leganto item types.
7. **ntnu exam tab in `course_plan`**: cut only the contact block, keep
   "Vurdering"/grading/weights; changes what `course_plan` holds →
   `methods.qmd`.
8. Low: running times after "Lengde"/"[mm:ss]" (nla, uis); square-bracket
   ranges (hiof); `Meld. St.` session years (a rule decision, #229).

## Changes since the pilot (2026-09-30)

- **Fixed:** nih/uio inflected season labels (#227), mf "Eksamensdatoer"
  block (#228), uio exam-links tail (#224), parenthesised e-mail (#229).
- **Regressed:** mf library notice (#228 rule no longer matches: the page
  text gained a colon).
- **New:** everything at the 15 institutions audited for the first time.

## Pre-pass blind spots (`R/audit/prepare_anonymization.R`)

`name_like` and `name_label` flag mostly false alarms (journal titles,
exam-table headers, labels across line breaks: hivolda flagged all 594
offerings) and miss the mid-line role + name layout; `NAME_LABEL_RX` has no
"kontakt med" or dean roles and no initials. Add the pattern of fix 1.

## Rejected findings

None: all 39 pass verification and were checked against the code. The
out-of-packet observations in `overall_assessment` (above) were verified by
hand; nord's later-plan text in `nord_NAT1002-1_2018_autumn_2` and uia
PRA047 glued words belong to the fulltext audit (#235).
