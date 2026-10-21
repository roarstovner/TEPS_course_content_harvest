# Anonymization audit — synthesis (pilot: 6 institutions, after the URL fixes)

Run 2026-10-21 with `--pilot` (packets built with the current
`R/anonymize.R` at commit a8ba3a5): hiof, nih, uib, uia, usn, uit. Each
packet holds 21–28 plans from the institution's pilot sample. One Opus
review agent per institution (medium effort); packets 65–225 KB; every
agent read all its courses. 15 findings for these 6, all pass mechanical
verification (0 ✗). The synthesis of the 2026-10-17 pilot run (ntnu,
oslomet, inn, nord, hvl, hivolda, nla) is in git history (a8ba3a5).

## Summary

- **Staff names are left in at four of the six** (hiof, uib, uit, usn), each
  in a wording or layout the rules do not know. `privacy_check` does not
  catch them (0).
- **One of them is in the published teacher-education data and both
  finalized releases** (data-2026-10-15, data-2026-10-16): "Approved by
  [navn], Dean" in one USN plan (English approval
  stamp). The other leaks are only in the all-courses build, which is not
  published (checked 2026-10-21 on `data/processed/teacher_ed/` and
  `data/releases/*/`).
- nih and uia are clean: every staff label and layout in the sample is
  handled; no course content lost.
- Low: admin dates in unusual forms, sentence fragments left after a
  removal, and one content word cut by a USN Leganto rule.

## Cross-institution patterns

- **Staff names under labels or in stamps the rules do not list**: hiof
  "Foreleser studieåret:", uib "Administrativt ansvarlig" (a name where
  the field usually holds a unit), usn "Godkjent av programkoordinator
  <name>", "Første gang godkjent ved professor <name>", "Approved by
  <name>, Dean", change-log signatures "programkoordinator <name>/
  fagansvarlig <name>", uit 2008–2011 English timetable ("Lectures Autumn
  2008" block, names glued to roles: "Kursansvarlig[navn]"). The role
  list (`.staff_roles`) and the approval regex
  (`.approved_by_name_regex`) are shared, so one change helps all.
- **The `(?<!\p{L})` guard** keeps "koordinator" from matching inside
  "programkoordinator" (usn); compound role words need their own entries.

## Prioritised fix list

1. **Staff names** (medium by the agents, but personal data; rare to
   occasional). Where: `R/anonymize.R`.
   - `.staff_roles`: add Foreleser (with "studieåret"), Administrativt
     ansvarlig (a name, not "Det/Den …" units), programkoordinator,
     fagansvarlig.
   - `.approved_by_name_regex`: "Godkjent av/ved" + any role word or none,
     "Første gang godkjent ved", English "Approved by <name>, <role>"
     (usn: `usn_REG3000D-…`, `usn_NETPRL-…`, `usn_OKLI3014-…`).
   - `.anon_uit()`: the timetable-block rule also for "Lectures|Teaching"
     + "Autumn|Spring|Fall" (`uit_SVF-3904-1_2008_autumn_1`), stopping
     before "Ansvarlig enhet"/"Responsible unit" (the Norwegian 2008 pages
     now lose that line), and a backstop for glued role+name lines.
   - usn change log ("Endringsbeskrivelse") signatures
     (`usn_SMERTE100-…`); "[navn]" is also a reference author there and
     must stay.
   Count every pattern in the whole build before and after; add each to
   `test-anonymize.R`.
2. **Admin dates** (low): uia "17 - 21 March, 2025"; usn "Høst-18",
   "27.01.2014/dekan", "06.12..2018"; uit "Spring semester 2009",
   "Søknadsfrist 15. mai 2019"; uib meeting schedule with dates; uib
   reading-list date sentence variants and fragments.
3. **Fragments** (low): nih "Pensumlista for." (drop the dangling "for");
   hiof "vil i koste"; usn "Journalføring" cut to "føring" by the Leganto
   item-type rule.

## Changes since the previous run

The six were not in the 2026-10-17 pilot anonymization run. Earlier
findings for these institutions (full run of 2026-10-13) came from other
courses; the new packets cannot confirm or rule them out (git history).

## Rejected findings

None. All `name_like` and `phone` pre-pass flags at the six were false
alarms (reference authors, philosophers, ISBNs, publication dates, place
names), as the agents found.

## Verdict (`--pilot`, anonymization: `privacy_hits`)

| inst | privacy_hits | verdict |
|---|---|---|
| hiof | 0 (n 49) | not ready |
| nih | 0 (n 22) | ready |
| uib | 0 (n 46) | not ready |
| uia | 0 (n 76) | ready |
| usn | 0 (n 57) | not ready |
| uit | 0 (n 66) | not ready |

`privacy_hits` is 0 everywhere, but personal data is left in the sample at
four: not ready until fix 1 (rebuild only, no new harvest).

- **hiof — not ready.** "Foreleser studieåret: <name>"
  (`hiof_SFB24011-1_2019_spring_1`).
- **nih — ready.** Low finding only.
- **uib — not ready.** A name under "Administrativt
  ansvarlig" (`uib_MEDEPI-0_2025_autumn_1`).
- **uia — ready.** Low finding only.
- **usn — not ready.** Approver names (4 of 28 sampled plans) and
  change-log signatures; one of them is in the published releases.
- **uit — not ready.** Names in the 2008–2011 English timetable block (3
  plans in the build).
