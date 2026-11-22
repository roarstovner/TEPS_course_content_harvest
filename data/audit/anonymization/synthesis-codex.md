# Anonymization audit, pilots of 2026-11-21: DMMH, AHO, LDH, Oslo Nye Høyskole, Barratt Due

Run 2026-11-21 on the build of 2026-11-21 (10:21-20:21, commit d77a254),
`--pilot` samples. Review agents: Codex `gpt-6.1-sol`, one per institution,
28 courses each. Findings: `findings-codex/{dmmh,aho,ldh,onh,barrattdue}.json`,
report `findings-codex_report.md`. All 6 findings of the five passed
mechanical verification; the staff names AHO's finding quotes were then
replaced by `[name]` in the committed JSON (the repository is public), so
the report marks that finding ✗. Its course ids lead to the names in the
packet. The other ✗ rows (hiof, uib, himolde, usn) are earlier runs'
findings redacted the same way.
Oslo Nye Høyskole's packet reads the pilot run itself (`pilot_run_rows()`).

Each finding was checked against its packet and `R/anonymize.R`: **6 real**
(1 high, 1 medium, 4 low; one medium re-rated low), **0 rejected**. Every
fix is rebuild-only.

## Summary

- **AHO, high: staff names left in, in 6 of 20 suspects.** Labelled teacher
  lists ("Lærere: [name] (kursansvarlig), [name], [name] og [name]"), track
  responsibility, supervision ("Supervised by Professor ...") and course or
  research leadership in prose ("will be led by", "led by"). `R/anonymize.R`
  has no AHO handler, and the generic staff-role words lack lærer/lærere,
  underviser/undervisere and kursansvarlig. None of the 8 random controls
  has the defect. `privacy_hits` (e-mail and phone patterns) is 0.
- **DMMH: no findings**; its course leaders are cut by the config's
  `exclude` (the facts list's "Emneansvarlig").
- **Administrative dates in prose** survive at four institutions, each in
  one or two plans: AHO "In 2023 the course will focus on games ..." and
  "In the fall of 2024"; Oslo Nye Høyskole "Emnets tittel ble endret i juni
  2019."; Barratt Due "Kvalitetssikringssystem for Høyskolen Barratt Due
  2023-2027"; LDH's cohort label "Gjelder BIS24".
- **Oslo Nye Høyskole: one content year removed**: "i etterkant av 2007-08
  finanskrisen" loses "2007-08" (`onh_INT1140_2026_spring_1`).

## Cross-institution patterns

| Pattern | Institutions | Root cause (checked) | Batch |
|---|---|---|---|
| Administrative dates in running prose | aho, onh, barrattdue, ldh | the administrative-year rules are keyed to labels and stamps; "In <year> the course will focus", "fall of <year>", "endret i <month> <year>" and a validity range of four years ("2023-2027", which `.drop_academic_year()` keeps as not consecutive) are none of them | B |

## Prioritised fix list

A. **AHO staff names (high).** Generic staff-role vocabulary: "Lærere:",
   "Lærer:", "Undervisere:", "Flere undervisere:", "kursansvarlig", "Track
   responsible:", "foreleser og kursansvarlig er"; prose leadership "(will
   be) led by" and "Supervised by Professor" (an AHO handler if they are too
   loose for the generic rules). Remove the names or the staff-only line,
   keep the course and research description and the reading-list authors.
   Examples `aho_70 161-3_2025_spring_1`, `aho_70 505-1_2025_autumn_3`,
   `aho_70 123-1_2025_spring_1`; the description names all six courses
   (packet courses 1, 4, 12, 13, 14, 20). Effect: the six suspects clean;
   check `privacy_check` and the other institutions' plans with
   `preview_change()` for the generic words.

B. **Administrative dates in prose (medium/low).** AHO's two English forms
   (`aho_70 305-1_2025_autumn_3`, `aho_40 335-1_2025_autumn_2`; medium),
   ONH's title-change note (`onh_INT2210_2026_spring_1`), Barratt Due's
   quality-system range (`barrattdue_PED2-1_2026_autumn_1`), and LDH's
   "Gjelder BIS24" (`ldh_BSY-221-1_2025_autumn_1`,
   `ldh_BSYP-220-1_2025_autumn_1`). LDH's selector keeps the cohort label on
   purpose (`R/institution_config.R`: "the cohort ("Gjelder BIS25")"), but
   it makes the same course's plan differ from cohort to cohort; drop it or
   its year in the anonymizer (low).

C. **Content range removed after "av" (low, re-rated from medium; generic).**
   The running-text guard of 2026-11-02 keeps a range followed by a
   small-letter word unless a preposition stands before it; "i etterkant av
   2007-08 finanskrisen" has "av". One plan in 140 sampled; a narrower cue
   (no "av" before a noun that follows the range) would keep it. Check with
   `preview_change()` on all institutions.

## Changes since the previous run

The five institutions are new; no earlier rows.

## Rejected findings

None. The reviewers' own rejections, kept: the name_like flags at all five
are titles, organisations, places, composers and reference authors; AHO's
phone flag is an attachment id in a reading-list URL; Barratt Due's
repertoire names stay.

## Re-audit of AHO 2026-11-22 (after the fix)

Batches A and B were fixed and rebuilt (build of 2026-11-22, 00:18-10:40);
C too (the content range "finanskrisen i 2007-08" stays). AHO re-run with
the newest Codex model, `gpt-6.2-sol`, on a fresh packet of the same seed
(20 suspects, 8 controls; `findings-codex/aho.json`): "no confirmed
personal data left in the displayed plans"; the staff names show only in
the removal spans. 3 findings, all verified:

- medium, English scheduling dates in two PhD courses ("Preparation of
  Literature review/theoretical framing by April 30, 2026", an AHO
  Colloquium session on 6 November; `aho_PHD-READING-1_2025_autumn_3`,
  `aho_PHD-RESEARCH-1_2025_autumn_3`): English "by <Month> <day>, <year>"
  dates the generic rules do not read.
- low, a clock range ("Forelesning fra 9-12 hver mandag formiddag",
  `aho_70 123-1_2025_spring_1`).
- low, the role left after its name ("... og foreleser og kursansvarlig
  er."): the inline rule always keeps the role.

## Verdict

Gate number of this check (build of 2026-11-22; `PILOT_GATE`: privacy_hits
0): `privacy_hits` 0 at all five.

| Institution | privacy_hits | Verdict |
|---|---|---|
| dmmh | 0 | **ready for full harvest** |
| aho | 0 | **ready for full harvest** (re-audit 2026-11-22; medium and low left, rebuild-only; not ready on 2026-11-21) |
| ldh | 0 | **ready for full harvest** (batch B, low) |
| onh | 0 | **ready for full harvest** (batches B, C, low) |
| barrattdue | 0 | **ready for full harvest** (batch B, low) |

AHO's fix is rebuild-only: the full harvest would store the same pages, so
it does not wait on the fix, but a release does.
