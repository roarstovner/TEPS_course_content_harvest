# Anonymization audit — synthesis (pilot re-runs of 2026-10-22: hiof, uib, uit, usn; oslomet, inn)

Run 2026-10-22 with `--pilot` (packets built with the current
`R/anonymize.R` at commit c8c0b29): the four institutions whose verdict was
"not ready" on 2026-10-21 because staff names were left in. Each packet
holds 28 plans from the institution's pilot sample (111–226 KB). One review
agent per institution in the Codex CLI (gpt-6.1-sol, reasoning effort
high); every agent read all 28 courses. 8 findings, all pass mechanical
verification (0 ✗). The 2026-10-21 run (Opus agents) is in git history
(098445b); its verdicts for nih and uia stand.

A second run the same day (packets of commit 784e658): oslomet and inn,
whose anonymization verdict of 2026-10-17 was "not ready" (INN's fix was
rebuild only). 28 and 21 plans (92 and 95 KB); same model and effort.

## Summary

- **No staff name or e-mail is left in at any of the four.** Every
  personal-data finding of 2026-10-21 is gone: hiof "Foreleser
  studieåret:", uib "Administrativt ansvarlig", uit's 2008 English timetable
  block, usn's approval stamps ("Godkjent av programkoordinator", "Første
  gang godkjent ved professor", "Approved by …, Dean") and change-log
  signatures. `privacy_hits` is 0 at all four.
- All `name_like` and `phone` flags were false alarms (reference authors,
  historical figures, a named seminar, publication dates).
- **oslomet and inn are clean**: no personal data, no over-removal; one
  low finding (inn: "1.8.25" in a credit-overlap table's date cell,
  `inn_IDTD2002-1_2025_autumn_1`).
- Left: administrative dates in unusual forms and sentence fragments after
  a removal (low, one medium), and one content word cut at usn.

## Cross-institution patterns

- **Dates the date rules do not know**: usn "20.10. 2017" (space inside),
  "27.01.2014/dekan", "Høst-18", a change-log note "2021 erstatter lista fra
  2020"; uib dated seminar schedules ("tirsdag 18.feb 2025 - rom 8.1 og
  8.2", `uib_FSKLI901-…`); uit three plans. Same class as the open
  anonymizer-dates issue.
- **Fragments after a removal**: hiof date removals that break a sentence
  (15 courses, low), uib "kan søke om utsatt innleveringsfrist til."
  (e-mail removed), "Emnet byrjar å gå frå." (semester removed), and the
  reading-list date sentence cut to "Juni for haustsemesteret og 1. januar
  for vårsemesteret." or left whole in variants ("vil vere tilgjengeleg på
  uib.no", "vil normalt vere klar innan").

## Prioritised fix list

1. **Admin dates** (medium for uib's schedules, otherwise low). Where:
   `R/anonymize.R`, the date rules and `.anon_uib()`. Count each form in
   the whole build first.
2. **Fragments** (low): uib's reading-list date sentence variants, the
   clause left after an e-mail ("… til."), hiof date fragments.
3. **usn "Journalføring" cut to "føring"** (low, persisting): the Leganto
   item-type rule matches inside a word (`usn_OKLI3014-1_2018_autumn_1`, heading "Journalføring
   og dokumentasjon"); a word-boundary guard.

## Changes since the previous run

| inst | fixed | persisting | new |
|---|---|---|---|
| hiof | staff name | date fragments | — |
| uib | staff name | admin dates, reading-list boilerplate and fragments | fragment after an e-mail |
| uit | staff names, over-removal | admin dates | — |
| usn | staff names | admin dates, "Journalføring" | — |

## Rejected findings

None.

## Verdict (`--pilot`, anonymization: `privacy_hits`)

| inst | privacy_hits | verdict |
|---|---|---|
| hiof | 0 (n 47) | ready |
| nih | 0 (n 22) | ready (2026-10-21) |
| uib | 0 (n 46) | ready |
| uia | 0 (n 76) | ready (2026-10-21) |
| usn | 0 (n 57) | ready |
| uit | 0 (n 66) | ready |
| oslomet | 0 (n 93) | ready |
| inn | 0 (n 64) | ready |

No personal data is left in the four samples and `privacy_hits` is 0;
the findings left are dates and fragments (fix issues, rebuild only).

## Note on the model

This run used gpt-6.1-sol in the Codex CLI instead of Opus. It read every
course (81–131 thousand tokens per agent) and confirmed the earlier
findings as fixed, with no false personal-data finding.
