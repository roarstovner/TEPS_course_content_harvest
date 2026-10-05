# Sections audit — synthesis (block model, 2026-10-05)

18 institutions, one review agent each, on `plan_sections.RDS` built by the
block model (#272–#278: `R/blocks.R` readers + `sectionize()`, one row per
plan × section cut from the page of `source_course_id`). Packets: 14 suspects +
up to 6 random controls, fitted to ~150 KB (`prepare_sections.R`); hiof
(170 KB) went to Opus, the other 17 to Sonnet. uit is audited for the first
time since its fulltext was restored (#218); everything else is a re-run of
the 2026-10-01 audit (#216), so the change table in `findings_report.md` is a
before/after of #239's fixes plus the block-model rewrite. All 68 findings
pass mechanical verification (0 ✗). Report: `findings_report.md`. The previous
synthesis is in git history (commit ee65ed9).

## Summary

- **The block model holds up.** 15 of 18 agents call extraction good or
  mostly clean; 12 institutions have no high- or medium-severity finding that
  is a pipeline fault. Every high-severity item from the previous run that was
  fixed in #239 stays fixed (oslomet resit text, nord COVID line, hiof resit
  headings, uis PDF titles, usn table of contents, ntnu script text, uib
  semester picker). Learning-outcome sub-groups and arbeidskrav-vs-assessment
  are now correct almost everywhere.
- **uit is the one institution with structural problems** (5 of the 8 high
  findings). Two causes: (1) 148 plans from 2008–2011 use the old FS layout,
  whose headings are `span.fsemneoverskrift`, which the uit config does not
  read — 51 of them get no sections, the rest are badly merged; (2) on the
  2012–2025 layouts arbeidskrav sit inside the "Eksamen" block, so up to
  ~1,400 uit plans have gates in assessment and no coursework row.
- **Arbeidskrav inside assessment** is the remaining cross-institution
  weakness where the gate has no heading of its own: uit (above), nord (label
  line moved, its paragraph left behind; ~176 plans with gate text in
  assessment and no coursework row), and single cases in hvl, uia.
- **Privacy (not a sections fault):** uis reading lists carry library OpenURL
  strings with `user_ip=` — 233 published `plan_sections` rows and 234
  published `course_plans` contain IP addresses, some public (e.g. a
  residential address). The anonymizer does not strip them. Filed for #231.
- The rest is low-impact noise: unmapped minor headings, placeholder phrases,
  flattened exam tables, Leganto / PDF page furniture.

## Cross-institution patterns (by root cause)

1. **Coursework gates without a heading** (uit, nord, hvl, uia; uio bold
   labels). `.split_inline_coursework()` (`R/extract_sections.R`) is on only
   for nord (`section_inline_coursework`) and moves single *lines* matching
   `.inline_coursework_regex`. nord writes a label line ("Obligatorisk
   deltakelse (OD)") followed by its paragraph, so only the label moves; and
   "Inntil 4 arbeidskrav (AK)" does not start with "Arbeidskrav". uit writes
   "Følgende arbeidskrav må være godkjent før …" + a list (2012–2019, ~700
   plans) or an "Obligatoriske arbeidskrav" table row (2020s, ~700 plans, then
   repeated under "Mer info om arbeidskrav").
2. **Minor headings with no pattern** (`R/section_heading_map.R`): inn
   "Ferdighetsmål" (13 pages — a whole outcome group lost: "ferdigheter" does
   not match "ferdighetsmål"), nla "Progresjonskrav" (43), nmbu
   "Undervisningstider"/"Læringsstøtte" (20/21), usn "Utgifter i emnet", uis
   "Arbeidsmengd", uio "Sensurordning"/"Sensurering" as bold labels. And two
   that match the wrong row by substring: uib "Vurderingssemester" →
   assessment via "vurdering" (218 rows get exam-timing text), uia nynorsk
   "Innhaldsliste" → course_content via "innhald".
3. **Praksis pointers in teaching_methods** (nla, nord, inn, usn — low). A
   side effect of #247 (Praksis → teaching_methods): where the block is only
   "Se egen praksisplan", the pointer and the word "Praksis" are appended.
   Content-bearing Praksis blocks (uia, hiof) are fine.
4. **Placeholders and pointers still emitted** (nih "Ingen emner", nord ".",
   oslomet "Ingen arbeidskrav." / "Se programplanen …", uis/uib "Ingen" in
   front of real text, hvl "Det er ingen vurdering i emnet.").
   `.placeholder_phrases` misses these variants; a leading placeholder line
   before real text is never stripped.
5. **Page furniture** — flattened exam tables (hivolda, inn), uis PDF running
   headers ("Emne X, BOKMÅL, , versjon"), usn/uis Leganto widget text and
   OpenURL strings, steiner PDF page numbers, uit legacy `</div>` fragments.

## Prioritised fix list

1. **Privacy — IP addresses in uis reading lists** (`R/anonymize.R`): strip
   `user_ip=<addr>` (or the whole OpenURL query) for every institution. 233
   section rows, 234 course_plans. Then item 9 removes the URL noise itself.
2. **uit legacy layout** (`R/institution_config.R`, uit): add
   `span.fsemneoverskrift` to `section_heading_selector` (default `h2`), and
   exact rows for the labels it uses that map to nothing or to the wrong
   section: "undervisningsform" → teaching_methods, "mål" and "objective of
   the course" → learning_outcomes, "recommended reading/syllabus" →
   reading_list; "dato for eksamen", "dato for skoleeksamen", "date for
   examination" → .drop (now assessment via "eksamen"). Emnetype, Emnet
   administreres av and the overlap list are unmapped and drop as they should.
   148 plans; examples `uit_NOR-3930-1_2009_autumn_1`,
   `uit_LRU-1400-1_2010_autumn_2`, `uit_SOA-3905-1_2011_spring_1`.
3. **Coursework gates in assessment** (`.split_inline_coursework()`): move a
   gate *paragraph* (a gate line and the lines up to the next blank line /
   next exam-form label), not only the line; accept a leading quantifier
   ("Inntil 4 arbeidskrav"); enable it for uit with "Følgende arbeidskrav må"
   and "Obligatoriske arbeidskrav" as gate starts; drop the duplicated uit
   2020s table row when "Mer info om arbeidskrav" holds the same gates.
   nord ~176 + uit ~1,400 plans. Examples `nord_MUS2015-1_2024_autumn_1`,
   `nord_KRO5001-1_2020_autumn_2`, `uit_LRU-3640-1_2018_autumn_1`,
   `uit_LER-1510-1_2025_spring_1`.
4. **Heading-map additions** (pattern 2): "ferdighetsmål"/"ferdigheitsmål" →
   learning_outcomes; "progresjonskrav" → prerequisites; exact
   "vurderingssemester", "innhaldsliste", "utgifter i emnet", "arbeidsmengd"
   → .drop; "undervisningstider" → teaching_methods; uio "sensurordning:" as
   a sub-heading → .drop like the heading. Check each with `heading_use` and
   `metrics_check`.
5. **Inline "Forkunnskapskrav:" in course_content** (mf PPU1015/1020, uit
   "Om emnet" ~1,000 rows flagged `leak->prerequisites`): a line that starts
   with "Forkunnskapskrav" inside course_content opens prerequisites. For uit
   most of that text is "Jf. opptakskrav …" (admission, .drop by the
   codebook), so the gain is mainly cleaner course_content.
6. **nord preamble** (`section_initial = "course_content"` for nord, as mf
   has): the lead paragraph before "Beskrivelse av emnet" is lost. Check that
   the title/code lines are not swept in. Example
   `nord_MAT5006-1_2022_spring_2`.
7. **Placeholder variants** (pattern 4) in `.placeholder_phrases`, plus
   stripping a leading placeholder line before real text.
8. **Praksis pointers** (pattern 3): drop a teaching_methods line that is only
   a pointer ("Se (egen )?(plan for )?praksis…", "Se praksisplan…") and the
   bare "Praksis" label line.
9. **Page furniture** (pattern 5): uis running headers and OpenURL strings,
   usn Leganto labels ("View online", "In compendium", item-type prefixes),
   steiner trailing page numbers, uit `</div>` fragments and the "Dato for
   eksamen" notice, hiof "Litteratur… sist oppdatert" variants (26 rows).
10. **uio bold labels under Undervisning** ("Obligatoriske forhold …",
    "Seminarundervisning", "Fravær fra obligatorisk aktivitet"): needs a
    `section_subheading_selector` for uio that catches `<p><strong>` labels;
    small n (52 uio plans).

Not pipeline faults (record in `data/data_notes.qmd` where not already):
oslomet praksis courses without sections (decided in #242, option b);
mf/hivolda/hiof/nih reading lists that are only a Leganto link on the page;
uia 2023 stub pages (PED428, IDR418, NAT115, SV-158: harvest got only the page
header — see #222); steiner `M-PEL2.2` carrying the PEL1.5 plan (#225);
oslomet stray ";" (`avansert;kunnskap`) and "¿", which are in the raw HTML
of some years; ntnu PPU attendance rules written inside "Læringsformer".

## Changes since the previous run (2026-10-01, #216)

- **Fixed:** oslomet resit text and accordion sub-headings (#240), nord
  COVID-line loss (#241), hiof "Vilkår for ny/utsatt eksamen" (#241), uis PDF
  title lines and admin headings (#243), usn table of contents (#244), ntnu
  script text and PPU47xx (#245), learning-outcome group labels (#249), uib
  semester picker (block model). Findings fell for nord 10 → 6, ntnu 8 → 3,
  uis 7 → 5, hiof → 2 low.
- **Persisting:** oslomet praksis (by decision), flattened exam tables
  (hivolda, inn), uio coursework/teaching mix, nla Progresjonskrav, nmbu
  unmapped minor headings, usn Leganto text.
- **New:** uit (first audit on its restored fulltext), uis IP addresses,
  nord gate paragraphs left in assessment (the #212 line rule only moves the
  label), Praksis pointers (side effect of #247), uib Vurderingssemester,
  inn Ferdighetsmål.

## Rejected findings and corrections

No finding failed verification, none was deleted. Corrections to agents'
root-cause guesses:

- oslomet M5GP* "no sections" (agent: a filter after `sectionize()`): the
  emneplan sections are "Se fagplanen." (dropped as placeholders) and the
  "Fagplan" block is skipped on purpose — `sectionize()` ignores
  sub-headings under an unmapped heading (#240). Decided in #242.
- oslomet "Fagets arbeids- og undervisningsformer" unmapped (agent: add a
  pattern): the pattern exists; the line is a sub-heading inside the skipped
  "Fagplan" block and sub-headings match exact rows only. Reading the
  subject-level fagplan for non-praksis courses whose emneplan says "Se
  fagplanen" (~450 plans without teaching_methods) is a separate decision,
  not covered by #242.
- mf reading lists (agent: fulltext selector): the page has only a Leganto
  link ("Litteraturlisten for høst 2026"); the list is not on the page.
- oslomet stray ";" (agent: block-reader join): the characters are in the raw
  HTML.
- uio "Karakterskala" → .drop (agent): the grading scale is assessment design
  by the codebook; only "Eksamensspråk" is logistics.
