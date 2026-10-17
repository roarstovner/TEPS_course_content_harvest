# Anonymization audit — synthesis (pilot: 7 institutions)

Run 2026-10-17 with `--pilot` on the build of commit 7510144 (the 16 pilot
harvests): ntnu, oslomet, inn, nord, hvl, hivolda, nla. Packets hold the
pilot sample's offerings (`data/pilot/{inst}.csv`, all courses), with
`course_plan` recomputed by the current `R/anonymize.R`: 20–28 courses,
86–150 KB. HiVolda and INN have no random controls: the `name_like` flag
hits every page (exam-table headers read as name pairs), so the suspects
fill the packet. One Opus review agent per institution; every agent read
all its courses and scanned all the institution's offerings with the
current code. 14 findings for these 7, all pass mechanical verification
(0 ✗). Two names an agent quoted in its summary are redacted as `[navn]`.
The other 11 institutions' findings in `findings_report.md` are older (the
2026-10-11 synthesis is in git history, 891bc8a).

## Summary

- **No personal data in any of the seven samples**; `privacy_hits` is 0 at
  all seven.
- **OsloMet: staff names left in 4 offerings outside the sample**, in the
  all-courses `data/processed/course_plans.RDS` (gitignored; not in either
  finalized release, which hold teacher education): SOS3000 spring 2022
  ("Delemneansvarlig …: <name>" lines under "Emneansvarlig"), ACIT5900
  autumn 2025 ("… coordinator: professor <name>"), SOSV3101 spring and
  autumn 2022 ("Emneansvarlige er: <name> (sosionom) og <name> (pedagog).").
  `.anon_oslomet()` removes one plain name line under "Emneansvarlig";
  `.role_name_inline_regex` allows no " er:" between role and name. The
  pre-pass and `privacy_check` miss these forms. Checked in the build.
- **INN keeps later plan versions under the language banner.** `.anon_inn()`
  gives no plan only to pages with "Emnebeskrivelsen for valgt semester er
  ikke publisert enda"; pages that say "Kun tilgjengelig på engelsk" show
  the latest version too (2MNK172-2 from spring 2022 to spring 2025 all
  show the version taught from autumn 2025; INN3024 autumn 2023 shows 2027
  spring). 26 INN plans carry the banner. INN has no `page_year`.
- The rest is low: one handler variant missed per institution, and date
  rules that cut a season or year that was content.

## Cross-institution patterns

1. **A rule that keys on one notice misses its variants** (INN's later
   version, HVL's "not offered" notice, NTNU's "Andre sider om emnet" box,
   OsloMet's coordinator block): a structural rule (`page_year` for INN, a
   whole-block rule for OsloMet) instead of the notice's wording.
2. **Removing a year changes a sentence** (NLA "Exception for academic
   year: Spring.", "gjennomført studieåret."; NTNU "neste gang."; Nord
   "Skoleeksamen høst" / "Skoleeksamen vår" made identical): remove the
   clause, or keep the year where it is a condition.
3. **Bare season values in facts boxes are removed by language**:
   "Undervisningssemester: Høst" loses its value (1,658 HVL plans) while
   "Semester of instruction: Autumn" keeps it (55); NLA "Oppstart høst," vs
   "Start in the autumn". The spec counts the semester of the facts box as
   plan text; whether a bare season goes is a decision for the user.

## Prioritised fix list

1. **OsloMet names** (`R/anonymize.R`): remove the whole Emneansvarlig
   block in `.anon_oslomet()`; let `.role_name_inline_regex` take " er:"
   and a parenthesised role after a name, so `privacy_check` catches the
   form too; tests with invented names. The agent's local test changed only
   the 4 leaking plans (and one placeholder "x" line). Rebuild only; must be
   in before any all-courses build is shared.
2. **INN `page_year`** (`R/institution_config.R`): the version on the page
   (its start semester), so `plan_offerings()` gives a later version no plan
   whatever the banner says, as at Nord and UiS. Rebuild only.
3. Low, per institution: HVL "The module is not offered in the academic
   year …" and "Emnet har ikke undervisning våren 2025." (1 plan each);
   NTNU "Faglærers emneside" after the removed box (153 offerings) and
   "neste gang høsten 2007" (26); INN "Statusmelding / Kun tilgjengelig på
   engelsk" lines (26 plans) and "1.8.25"; Nord season words cut from
   course names in the overlap list, "Week 13, Friday 27.3" and "Sprign
   2026"; HiVolda "veke 3 (2023)" (4 offerings); OsloMet "Etablert av
   dekanen 3. juli 2013" (2 plans) and fagplan URLs that lose their year
   (70 links: "…-fagplan-engelsk-2018-2019" → "…-engelsk-"); NLA pattern 2.

## Changes since the previous run

Not a before/after (the previous runs audited teacher-education samples),
but the earlier findings were checked on the whole data: fixed are NTNU's
cut of a second "Vurderingsordning:" scheme, Nord's regulation date
"28.10.2020 nr. 2474" and stray "MERK: :", INN's "Engelsk"→"English"
rewrite and stripped status notice, HVL's Nynorsk "not offered" notice,
OsloMet's ";" artefact.

## Rejected findings

None. Pre-pass flags judged false alarms by the agents: `name_like` on
exam-table terms, programme, product and organisation names, reference
authors and historical persons; `admin_date` on government-document dates
the rule keeps; `phone` on digit runs in URLs and article identifiers (NLA);
`removed` on page shells (OsloMet) and short NTNU pages where the staff
list is a large share. A compendium author named under NTNU's Kursmateriell
is a reference author and stays.

## Verdict (`--pilot`, anonymization: `privacy_hits`)

| inst | privacy_hits (n) | verdict |
|---|---|---|
| ntnu | 0 (79) | ready |
| oslomet | 0 (93) | not ready |
| inn | 0 (59) | not ready |
| nord | 0 (82) | ready |
| hvl | 0 (79) | ready |
| hivolda | 0 (77) | ready |
| nla | 0 (67) | ready |

- **ntnu, nord, hvl, hivolda, nla — ready.** No personal data; low
  findings only.
- **oslomet — not ready.** Personal data left in 4 offerings outside the
  sample (fix 1). No new fetch needed: the harvest itself does not have to
  wait, but no all-courses build may be shared before the fix.
- **inn — not ready.** Later plan versions kept as the offering's plan
  (fix 2, a high finding). Rebuild only.
