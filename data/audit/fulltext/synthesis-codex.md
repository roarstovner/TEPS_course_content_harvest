# Fulltext audit, build of 2026-11-06: 13 institutions

Run 2026-11-08 on the build of 2026-11-06 for hiof, hvl, inn, mf, nih, nla,
nmbu, oslomet, steiner, uib, uio, uis and usn. Review agents: Codex
`gpt-6.1-sol`, one per institution. Findings: `findings-codex/{inst}.json`,
report `findings-codex_report.md` (all findings of these 13 verified ✓). The
other institutions in the report (fhs, himolde, khio, nhh, phs, vid) are
older runs, handled before and not covered here. The pilot synthesis of
2026-11-02 that this file replaces is in git history. Harvests were running:
the checks below read the packets, the code, single raw Parquet files and the
teacher-education frame of the build (`data/processed/teacher_ed/`), no
whole-institution reads.

The 13 reports hold 14 findings (inn, oslomet and uio: none). Each was
checked against its packet, the code and `course_plan_spec.md`: **9 real**
(cause found in the code; fix batches A-E below), **5 rejected**, **0 spec
questions**. The audit reads `extracted_text`, which is internal; the
published `course_plan` and the sections are made from it by
`anonymize_text()`, which already removes several of the reported kinds of
page furniture. Each rejected finding was checked on the published text.

## Summary

- **High, HVL English-pointer pages are kept as plans.** 15 of 28 sampled
  HVL entries are a Norwegian notice saying the plan is on the English page
  ("Dette emnet kan du lese om på dei engelske nettsidene våre.",
  "Emneplanen er kun tilgjengelig på engelsk.", "This course will be taught
  in English, please see the English version of the course description."),
  with "Plan in the build: yes". The hvl `english_page$notice` knows four
  wordings; about 200 raw HVL pages (2017-2025) match one of the new
  wordings against 101 for the old ones. The same failure as NTNU's, fixed
  2026-11-05.
- **Low, a few plans for another year.** Three HVL offerings (MAS502
  autumn 2022 and spring 2023, MAS532 spring 2023) show a page whose facts
  say "first time autumn 2023" / "first time spring 2024", after the
  offering; three UiS PDF plans (MGL2071 autumn 2019, LENG116 and LNOR176
  autumn 2015) declare the study year before their archive folder ("dette er
  studieinformasjon for studieår 2018-2019"). Neither is read by
  `page_term`/`page_end_term`.
- **Low, table-of-contents labels in every HiOF and USN plan.** HiOF's
  Vortex `h2.vrtx-toc-header` "Innholdsfortegnelse" (820/820 teacher-ed
  plans; NIH, on the same CMS, already excludes it) and USN's whole
  table-of-contents block (1,177/1,177) stay in `course_plan`.
- **Low, whitespace joins**: NLA accordion HTML with non-HTML
  `<list>`/`<listItem>` tags (1,062 raw pages, 97 courses) and NMBU's April
  2026 layout with `<p>` inside a `<span>` (5 of 23 pages).
- **Low, legacy course codes with a space** ("FAH310 000") at NIH and HiOF
  still have the curl "bad/illegal format" error from the harvests before
  2026-10-29 (nih 266 rows, 152 codes; hiof up to 126 rows). The code was
  fixed that day (`site_request()` encodes spaces); only a retry harvest is
  missing.
- Rejected: MF exam dates, Steiner page numbers, UiS PDF running heads and
  page numbers, USN Leganto labels and item-type prefixes (all removed by
  `anonymize_text()`, absent from the published text), and UiB's empty
  extractions (raw runs written after the build).

## Cross-institution patterns

- **The English-page notice is a list of wordings per institution**, and it
  falls behind whenever a site varies its phrasing (NTNU 2026-11-05, HVL
  now; NMBU and Nord have their own). The fix is the same at each: widen
  `english_page$notice` (it drives the fetch-time follow-up in
  `fetch_html_cols_single()` and the `english_pointer` drop in
  `partition_text()`/`plan_offerings()`), widen `.placeholder_phrases`, then
  a rebuild (pointer offerings lose their plan) and a retry harvest (they get
  the English plan).
- **Table-of-contents labels** (HiOF, USN; NIH excluded already): one
  Vortex selector for HiOF, a bounded block for USN.
- **Plan-year declarations the year check does not read** (HVL facts, UiS
  old-format PDFs): `partition_text()` computes `page_term` from `html`
  only, so PDF rows (UiS archive) are never checked.
- **Page furniture in `extracted_text` that `anonymize_text()` removes**
  (MF, Steiner, UiS, USN): reviewers see it because the packet shows
  `extracted_text`. Not a data defect; the packet could say so (batch F).

## Prioritised fix list

### A. HVL English-pointer notice (high; rebuild, then a retry harvest)

- Where: `R/institution_config.R`, hvl `english_page$notice`;
  `R/extract_sections.R`, `.placeholder_phrases` (some wordings are there:
  "gå til våre engelske nettsider", "klikk på \"English\""); test in
  `tests/testthat/test-fetch-ledger.R` like NTNU's; `docs/institutions.qmd`;
  `methods.qmd` entry.
- Wordings in the packet: "Emneplanen er kun tilgjengelig på engelsk.",
  "Emneomtalen finnest berre på engelsk, gå til ...", "Dette emnet er bare
  beskrevet på engelsk.", "Dette emnet er kun skildra på engelsk.", "Dette
  emnet undervises på engelsk, klikk på \"English\" ...", "Dette emnet vert
  undervist på engelsk. For å sjå emneskildringa, klikk på \"English\" ...",
  "Dette emnet kan du lese om på dei engelske nettsidene våre.", "You can
  read about this course on our English website.", "This course will be
  taught in English, please see the English version of the course
  description.", "Kursinformasjon finnes kun på engelsk. Se informasjon på
  https://www.hvl.no/en/...".
- The notice is matched against the whole HTML: anchor it to the content
  paragraph (as NTNU's, a short tail) so that a full plan that mentions the
  English pages is not dropped; count old vs new matches on the raw store
  before and after.
- Examples: hvl_PHDH913-1_2024_spring_1, hvl_DAT355-1_2025_spring_1,
  hvl_MARE530-1_2025_spring_1, hvl_GE4-301-1_2025_spring_1.
- Effect: about 200 HVL pages (2017-2025, by a loose count on raw HTML) lose
  their pointer "plan" at the rebuild; then
  `harvest_all(institutions = "hvl", retry = "ok")` after `tar_make()`
  fetches their English pages (the retry removes HVL checkpoints first; no
  HVL harvest is running now), and a second `tar_make()`.

### B. Plan-year declarations (low; rebuild only)

- HVL: a `page_term` for hvl in `R/institution_config.R` from the facts
  line "Semester of instruction: ... first time autumn 2023" /
  "Undervisningssemester: ... første gong/gang haust/vår YYYY". Raw: 115
  pages carry it; 3 are after their offering (hvl_MAS502-1_2022_autumn_2,
  hvl_MAS502-1_2023_spring_1, hvl_MAS532-1_2023_spring_2); those lose their
  plan under the rule of 2026-10-07 (an offering is coded with its own
  year's plan or none). The audit heading regex also misses "Course
  description for academic year" (batch F).
- UiS: a `page_end_term` from the PDF text "dette er studieinformasjon for
  studieår YYYY-YYYY" / "Dette er studietilbudet for studieår YYYY-YYYY".
  `partition_text()` in `R/pipeline.R` passes only `html` to the term
  functions, so PDF rows need their `extracted_text` too. Raw: 1,416 PDFs
  declare a year; once spring offerings are moved to their academic year
  (`.menu_year_pages()`, 2026-10-14) all agree with their folder except 3
  old-format PDFs one year earlier (uis_MGL2071-1_2019_autumn_1,
  uis_LENG116-1_2015_autumn_1, uis_LNOR176-1_2015_autumn_1). Check the three
  against the archive before dropping: the old notice may be a stale
  template line.
- Effect: about 6 offerings lose their plan; `methods.qmd` gets a line under
  the later-version entry.

### C. Table-of-contents labels (low; rebuild only)

- HiOF: add `#vrtx-fs-emne-content > h2.vrtx-toc-header` to hiof `exclude`
  in `R/institution_config.R`, as NIH has (every HiOF page of 2023 has it).
  Boilerplate, so `exclude` may cut it (spec: navigation is out).
  Examples: hiof_IRM23116-1_2019_autumn_1, hiof_SFB11615-1_2023_spring_1.
- USN: the block "Innholdsfortegnelse"/"Table of contents" plus the
  repeated section names. It cannot leave `extracted_text`: the text reader
  (`.text_blocks()`, `.text_header()` in `R/blocks.R`) uses it to find the
  headings and the end of the facts box. Remove it from `course_plan` in
  `.anon_usn()` (`R/anonymize.R`), with the reader's own bound (from the
  title to where its first entry repeats; a shared helper beside
  `.toc_titles`). Examples: usn_SL-PTM2000-1_2025_autumn_1,
  usn_PRO3000K-1_2024_autumn_1.
- Effect: the label/block leaves every HiOF and USN `course_plan`; plan ids
  of both change (the plan text is anonymized for the id), so
  `metrics_check` and `frozen_check` list them: an intended change.

### D. Whitespace joins in the block reader (low; rebuild only)

- NLA: `.json_blocks()` in `R/blocks.R` passes accordion HTML with
  `<list listType="bulleted"><listItem>` to `html_text2()`, which gives
  those unknown tags no line break ("seminarsPresent a first draft").
  Rewrite them to `ul`/`ol`/`li` before parsing. 1,062 raw NLA pages (97
  courses, 2010-) carry the tags. Example: nla_IKF302-1_2018_spring_1.
- NMBU: the 2026-04-04 snapshot (academic year 2025/26) writes the facts as
  `<p><strong>Frekvens:</strong><span><p>Årlig</p><p>Tilbys første gang
  ...</p></span></p>`; the nested `<p>` loses its break ("ÅrligTilbys").
  The 2026-09 layout uses `dt`/`dd` and is fine. 5 of 23 pages. Examples:
  nmbu_PPFD201-1_2025_autumn_2, nmbu_M30-LUN-1_2025_autumn_1.
- `R/blocks.R` is shared: a change redoes every partition, so batch it with
  other build-wide changes.

### E. Legacy codes with a space: retry harvest (low; no code change)

- `site_request()` encodes spaces since 2026-10-29; the error rows are
  from checkpoints of earlier runs. The error is classed `timeout`
  (`fetch_outcome()`), so `harvest_all(institutions = c("nih", "hiof"),
  retry = "timeout")` after a build fetches them, when neither full harvest
  is running. Most are 2000-2005 codes ("X 000") and will likely answer
  404. Examples: nih_FAH310 000_2005_spring_0, hiof_LAÅKHX 000_2004_spring_0.

### F. Audit packet (low; tooling)

- `R/audit/prepare_fulltext.R` labels a page that is not in the build
  "not in the build (pilot run)" also when its raw run is newer than the
  build (UiB, below); say "raw run newer than the build" instead.
- Its year regex accepts "the academic year" but not HVL's "Course
  description for academic year 2025/2026", so SABV270's flag came from
  dates in the plan.
- Optionally show `course_plan` beside `extracted_text`, or name the
  furniture `anonymize_text()` removes, so reviewers stop reporting it.

## Changes since the previous run

The previous fulltext reports for these institutions were pilot runs on
other builds; the report has no change table for them.

## Rejected findings

- **mf, exam-date blocks (medium).** `.anon_mf()` removes "Eksamensdatoer"
  up to "Læringsutbytte"; run on the stored pages (PED1010, SAM2080L,
  RL1013, PED5610; 2026-04-04 and 2026-09-30 runs), `extracted_text` has the
  block, `course_plan` and the sections do not. The spec puts it out, and it
  is out.
- **steiner, bare PDF page numbers (medium).** `.anon_steiner()` removes
  them; 0 of 15 Steiner `course_plan` and 0 of 61 sections have a bare
  number line. Moving the rule into the PDF reader would only tidy the
  internal `extracted_text`.
- **uis, PDF running heads, page numbers, TCPDF (medium).** `.anon_uis()`
  removes "EMNE ... Versjon", "Emne X_1, BOKMÅL, ...", the code-title
  footer, "side N" and "Powered by TCPDF"; 0 of 1,307 UiS teacher-ed plans
  and 0 of 7,456 sections keep them.
- **usn, Leganto labels and item types glued to titles (low).** `.anon_usn()`
  removes "Click to view interactive reading list in Leganto", "View online"
  and the item-type label ("BookPort business" → "Port business"); 0 of
  1,177 USN plans keep the labels (the 2 "BookTok 101" hits are a real
  title). The spec puts item-type labels out. The table of contents in the
  same finding is real (batch C).
- **uib, empty extraction on pages with text (medium).** The packet's
  suspects come from raw runs written after the build (2017:
  2026-11-06 22:28; 2015: 2026-11-08; build 2026-11-06 16:45): the UiB full
  harvest is running and their text is made at the next build. The eight
  random controls extract fine. Not a selector failure; the packet label is
  batch F.
