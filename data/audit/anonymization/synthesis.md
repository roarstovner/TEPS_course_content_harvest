# Anonymization audit — synthesis (re-audit of 8 institutions, 2026-10-11)

Re-audit after #301–#309 (#310) of uio, usn, uis, uit, hvl, ntnu, oslomet
and nla, one Opus review agent each, on `course_plan` recomputed with the
current `R/anonymize.R`. Packets: up to 20 suspects + 8 random controls,
10–28 courses per institution; every agent read all its courses. All 12
findings pass mechanical verification (0 ✗), and every root cause below was
reproduced on a minimal input in `R/anonymize.R`. The findings of the other
10 institutions in `findings_report.md` are from the full run of 2026-10-07
(synthesis in git history, edc1f2a) and predate #301–#309; nih is listed as
stale because its packet was rebuilt after that run.

## Summary

- **No personal data is left** in any of the 8 institutions: not in the 190
  sampled plans, not in the agents' scans of every offering with the current
  code, and not in `data/processed/` (a scan for a role word followed by a
  name, on its own line or inside one, finds only reference authors and
  organisation names; `privacy_check` is empty). All seven leaks that the
  2026-10-07 run found outside the samples (uio, usn, uis, hvl, uit) are gone.
- **Fixed since 2026-10-07:** nla em-dash academic year and film running
  times; uis version stamp and year paths in URLs/DOIs; uit 2008–2011
  timetable block, URL damage, semester codes, "Høst 10 stp", white-paper
  sessions; oslomet approval stamps (853 → 48 offerings) and "Gjeldende fra
  høstsemesteret"; hvl Bokmål "not offered" notice; ntnu exam tab mostly kept.
- **What is left is content loss and cosmetics, all low except one medium:**
  ntnu cuts later assessment schemes with the exam sessions (841 offerings);
  date rules still remove some reference and regulation dates; removals leave
  stray separators and spaces; three handlers miss a variant (hvl Nynorsk,
  usn Leganto, oslomet ";").
- **Anonymization does not block the release (#298)** on privacy. The fixes
  below improve content fidelity; 1 and 2 are the ones worth doing first.

## Cross-institution patterns

1. **The government-document guard looks back from the end of the match**
   (uis, oslomet; ntnu). `.not_gov` is a lookbehind placed *after* each date
   rule, so its 80-character window starts at the end of the date, and it
   cannot cross a line break inside the match. "Utdanningsdirektoratets …
   høsten\n2020" loses "høsten 2020" (the same text on one line keeps it);
   "Nasjonale retningslinjer … av 1. september 2016, revidert 17. oktober
   2018" loses the revision date (oslomet, ≥79 offerings per the agent). The
   comment on `.not_gov` says "named up to 80 characters before it", which is
   what the rule would do with the guard in front.
2. **Date rules read reference dates as administrative** (usn, uis).
   `.drop_academic_year` treats ISO year-month dates as academic years
   ("2001-02-01" → "-01"; 77 removals in 50 usn offerings, 50 cases at uis);
   the dd.mm.yyyy rule removes publication dates of newspaper and web
   references ("Razsviet, 4.11.1923" → "Razsviet, ,"; ~100 dates in 36 usn
   offerings, 52 uis plans). The rubric keeps publication dates in references.
3. **Removals leave their separators behind** (uio, usn, uit, nla, hvl; uib
   and nmbu in the earlier run). Rules remove the date, season or e-mail but
   not the space, dash, comma or "og" that joined it: "studieadministrasjonen
   , før" (uio), "Emneplan godkjent ." (usn), "15 stp,  og", "Naturfag 5-10
   -", "Dato:  –" (uit, 50 published plans), a line ":" (nla, 2 plans). No
   content is lost; a few sentences read wrongly ("Faget går .").
4. **Handlers miss a variant of the text they target** (hvl, usn, oslomet).
   hvl's notice rule is Bokmål only; usn's Leganto rule needs a capital, digit
   or `"«(` after the label; oslomet removes ";" only between spaces.

## Prioritised fix list

1. **ntnu: later assessment schemes are cut** (medium; 841 of 3,791
   offerings). `.anon_ntnu()` removes from the first "Ordinær eksamen - …" to
   the end in one greedy cut, so "Vurderingsordning: Hjemmeeksamen og
   oppgave" and its weights after the first session go with it — against the
   handler comment and the #307 methods entry. Remove each session block up
   to the next `^Vurderingsordning:` line. Same code and block as #316 (keep
   the ordinary session's exam parts): fix both together. Examples
   `ntnu_PPU4759-1_2017_spring_1`, `ntnu_PPU4773-1_2017_autumn_3`.
2. **Guard in front of the date** (pattern 1; uis, oslomet). Move `.not_gov`
   to the start of each date rule in `.anon_generic()` (tested by the oslomet
   agent on the "revidert" case). Keeps more government-document dates, so
   `metrics_check` will flag changes; extends the #305 methods entry.
   Examples `uis_MGLT43-1_2025_autumn_1`, `oslomet_M5GRL2100-1_2019_spring_2`.
3. **ISO dates are not academic years** (pattern 2; usn, uis). Add
   `(?![-–]\d)` to the two academic-year patterns so "2001-02-01" stays.
   "2008-09" alone stays ambiguous and goes. Example
   `usn_MG2EN7-1_2022_autumn_1` (reading list).
4. **Handler variants** (pattern 4), each a one-line change with a test:
   - hvl: "Emnet (?:tilbys ikke|vert ikkje tilbydd) …" in `.anon_hvl()`;
     24 of 1,671 published plans read "Emnet vert ikkje tilbydd ." now.
     Example `hvl_MGBMU241-1_2025_spring_1`.
   - usn: widen the Leganto lookahead in `.anon_usn()` to quotes, lowercase
     and `https?:` ("ArticleiPed", "Article“Translanguaging”"); 205 published
     plans. Example `usn_MG2EN7-1_2022_autumn_1`.
   - oslomet: turn a ";" between letters or digits into a space in
     `.anon_oslomet()` (63 published plans, nearly all 2022). This also lets
     the approval-date list rule see "29. januar;2020"; then repeat the
     approval-date rule so later dates on a stamp line go too ("… og på
     fullmakt av leder i utdanningsutvalget 4. mars 2019", 42 RLE offerings).
     Example `oslomet_M5GNA2100-1_2022_autumn_1`.
5. **Separators** (pattern 3): let the e-mail, season+year and dd.mm.yyyy
   rules take the horizontal space before them and `.eol_stop`; remove a date
   range "d1 – d2" as one unit; then drop a separator left at a line end
   ("-", ",", "og"). Cosmetic. Examples `uio_BIOS5000-1_2025_autumn_1`,
   `uit_LER-1402-1_2020_spring_1`, `usn_MG2EN3-1_2024_autumn_1`.
6. **Regulation dates in prose** (ntnu, low): "innlevert fra og med
   01.04.2014 skal sensureres" (28 published plans), "utfyllende regler …
   av 01.08.2015". Add `regler|reglement` to `.gov_doc_regex`; the sensur
   sentence names its scheme more than 80 characters earlier and needs its own
   exemption or stays as is.
7. **Two policy questions** (record either way in `methods.qmd`):
   - Cohort and plan-version years ("kull 2010-2012", "gjelder fra og med
     2013"; uit 239 offerings, usn 30) stay now. Recommendation: keep them as
     content — they name the programme version, and the offering's year is in
     the metadata anyway.
   - Month-year approval stamps ("Godkjent desember 2017.", 11 usn offerings;
     "November 2020:" change-log entries, 19) stay now. Recommendation: remove
     them like the dated approval stamps (#305).
8. **Pre-pass** (`R/audit/prepare_anonymization.R`, #232): `NAME_LABEL_RX`
   has no leading word boundary and its `\s` crosses lines, so
   "naturfaglærer\nInnhold Emne" is a suspect (all 56 oslomet hits, uit);
   use `(?<!\p{L})` and `[ \t]`. Suspect slots then go to real candidates.

## Changes since the previous run (2026-10-07)

The report's change table keys on target/error_type, which hides re-keyed
and changed findings; by content:

| inst | fixed | persisting | new |
| --- | --- | --- | --- |
| nla | em-dash academic year (#305), running times (#308) | – | – |
| uio | out-of-sample name leak (#301) | e-mail leaves a space | – |
| hvl | campus-name leak (#301), Bokmål notice (#306) | Nynorsk notice (was in the old finding) | – |
| ntnu | exam tab / unit / grading cut (#307) | later schemes cut with the sessions | regulation dates in prose |
| uis | version stamp (#305), URL/DOI year paths (#304), "Emneansvarlig <name>" leak (#301) | "høsten 2020" in a Udir report title | media and ISO dates in references (outside the old sample) |
| uit | timetable names (#301), URL damage (#304), semester codes (#305), "Høst 10 stp" (#308), white-paper sessions (#305) | – | separators left; cohort years |
| usn | approval stamps and names (#305, #309), inline role + name (#301) | Leganto label, ISO dates as academic years, "November 2020:" stamps | " ." after a removed date; cohort years |
| oslomet | approval stamps 853 → 48 (#305), "Gjeldende fra høstsemesteret" (#303) | ";" glue (re-keyed from boilerplate_left to other), later dates on a stamp line | – |

## Rejected findings

None: all 12 pass verification and were reproduced against the code. Not
taken as findings (no quote in the packet), but reproduced and folded into the
fix list: usn/uis ISO and media dates (fix 2–3), oslomet "revidert" date
(fix 2), usn month-year stamps (fix 7), nla ":" lines after "Høst 2024:" (fix
5). The uis OpenURL link with HTML escapes in two MGL2122-1 2021 plans
(wrapped before "exlibrisgroup"; its `user_ip` is already removed) belongs to
the fulltext audit (#235).
