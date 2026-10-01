# Sections audit — synthesis (re-run after #208–#215, 2026-10-01 afternoon)

17 institutions, one review agent each, on `sections_raw.RDS` rebuilt with the
extractor fixes of #208–#215 and #237 (commits 28a383c–664998d). Packets were
cut to 14 suspects + 6 random controls (`prepare_sections.R`) so Sonnet reads
them in full; the four packets still above ~200 KB (hiof, oslomet, uis, usn)
went to Opus, the other 13 to Sonnet. ntnu is audited for the first time on the
new harvest. uit is **not** re-audited (fulltext still empty, #218); its 12
findings in the report are from September and cannot be verified against a
packet. All 76 checkable findings pass mechanical verification after one fix
(below). Report: `findings_report.md`. The morning synthesis is in git history
(commit 5097d5d).

## Summary

- **The fixes hold.** 13 of 17 agents call extraction accurate or mostly
  clean. Findings per institution fell where the fixes landed: mf 12 → 4,
  hivolda 7 → 2, steiner 7 → 1, uis 12 → 7, inn 5 → 3, nmbu 4 → 2. Gone: mf's
  missing course description and arbeidskrav-as-assessment, hivolda's merged
  sections, usn's learning-outcome blob and footer, uio's Studentweb
  prerequisites, nord's coursework inside assessment, nmbu's CSS junk.
- **One new privacy leak (anonymizer, not sections):** usn approval lines
  name the dean ("<Name>, dekan") in 14 usn section rows and **67 published
  `course_plans`**. The #237 rule only catches "Name (Role)" and "Godkjent av
  dekan Name".
- **Three gaps in today's fixes:** (1) oslomet wraps every section in an
  accordion `<li>`, and `.subheading_sections()` skips any `<p>` inside a
  `<li>`, so the oslomet `"p"` sub-heading setting never fires — resit text
  ("Ny/utsatt eksamen …") stays in 1,343 of 1,472 oslomet assessment rows;
  (2) the nord COVID noise rule removes whole lines, and in
  `nord_PO111LS-1_2022_spring_1` that line was the only assessment text;
  (3) the exact `.drop` row "ny/utsatt eksamen" misses hiof's "Vilkår for
  ny/utsatt eksamen" (72 assessment rows) and nynorsk "ny/utsett eksamen".
- **Still badly segmented:** oslomet praksis courses (real text sits in a
  "Fagplan" block the extractor ignores; 135 offerings end with 0 sections),
  the uis PDF path (course titles and book titles open sections), usn's table
  of contents, ntnu's 2014–2016 PPU47xx plans, and uio coursework (still
  rarely separated).
- **Reading lists** are mostly pointers or boilerplate where they exist (mf
  library notice, nih "overgangsfase" pointer, usn "ikke publisert ennå" in
  296 rows) — a source limitation, plus a few placeholder phrases to add.

## Cross-institution patterns (by root cause)

1. **Flattened exam tables in assessment** (hivolda, inn, ntnu, oslomet, uib,
   uis, usn, hvl — `formatting_noise` in 8 institutions). The table header row
   ("Vurderingsform Gruppering Varighet …", "Vurderingsordning Karakterskala
   …") is glued to the data row. The design data (form, scale) is real; the
   header string is noise.
2. **Learning-outcome group labels lost inconsistently** (hiof, hvl, inn,
   nmbu, nord, uia, usn; low). `.clean_section_text()` drops a first line that
   equals the section's own heading pattern ("Kunnskap"), and `<p>`
   sub-headings that map to `learning_outcomes` are consumed as boundaries,
   so the first group loses its label while later ones keep it. Content is
   intact.
3. **"Praksis" heading unmapped** (hiof, inn, uis, usn; uia's "Faget i
   praksis" when run together with its text). The block is dropped
   (html_headings) or appended to the previous section (text_split). The
   codebook puts practicum under `teaching_methods`; #201's default agrees.
4. **Resit / exam-logistics headings that only partly match `.drop`**:
   "Vilkår for ny/utsatt eksamen" (hiof), "Ny/utsett eksamen" (oslomet
   nynorsk), uio "Eksamensspråk"/"Karakterskala" kept while "Hjelpemidler"
   is dropped.
5. **Coursework gates inside assessment/teaching text without a heading**
   (uio, ntnu LÆR2003/MGLU1113/EDU2001, ntnu PPU46xx "sertifiseringsansvar"
   paragraph, uia 70 % attendance, oslomet M5GRL2200). nord's line rule
   (#212) shows the pattern works where the gate is its own line.
6. **Placeholder phrases not yet in `.placeholder_phrases`**: "Oppgis
   senere." (ntnu), "Pensum-/litteraturliste er ikke publisert ennå." (usn,
   296 rows), "Litteratur vil være klart ved semesterstart." (hiof), nih
   "Ettersom vi er i en overgangsfase …" / "Pensumlista for høsten 20xx.",
   leading "Ingen." before real text (hvl, usn).

## Prioritised fix list

1. **Privacy — "Name, dekan" approval lines** (`R/anonymize.R`,
   `.anon_usn()` or `.anon_generic()`): remove a capitalised name before
   ", dekan"/", prodekan"/", instituttleder". 67 course_plans, 14 section
   rows. Re-run `run_dedup.R` and `run_extract_sections.R`.
2. **oslomet sub-headings** (`.subheading_sections()` in
   `R/extract_sections.R`): the `ancestor::li` guard must ignore the
   accordion `<li>` that wraps a whole section — e.g. only skip a `<p>` whose
   `li` ancestor sits inside the current section's content, or make the guard
   configurable. Expected: "Ny/utsatt eksamen" out of ~1,300 assessment rows,
   bold "Arbeidskrav" paragraphs split. Example
   `oslomet_M1GNO3100-1_2022_autumn_1`.
3. **nord COVID rule** (`.section_noise$all`): remove the COVID sentence, not
   the line. Example `nord_PO111LS-1_2022_spring_1` (assessment lost).
4. **Resit headings** (`R/section_heading_map.R`): `.drop` rows for "vilkår
   for ny/utsatt eksamen" and "ny/utsett eksamen", or make "ny/utsatt
   eksamen" a substring row placed before assessment. hiof 72 rows.
5. **oslomet praksis courses**: when the emneplan sections are "Se
   fagplanen.", read the page's "Fagplan" block (Organisering og
   arbeidsmåter → teaching_methods, Vurdering → assessment). 135 offerings
   with 0 sections; ~450 more without teaching_methods. Examples
   `oslomet_M1GP4200-1_2025_autumn_3`, `oslomet_M5GP4200-1_2025_autumn_3`.
6. **uis PDF path** (text_split fallback): ignore the title/header block
   before the first real heading (titles ending in "vurdering"/"litteratur"
   open sections in ~60 courses); exact `.drop` rows for "Åpen for",
   "Emneevaluering", "Overlapping" (appended to sections in ~2,000 courses);
   "Introduksjon" → course_content; stop a book title such as "Kompetansemål
   og vurdering" from splitting a reading list (~40 assessment rows hold
   literature). Example `uis_LENG360-1_2019_autumn_1`; MGL2050/MGL1050 2021.
7. **usn table of contents**: skip the "Innholdsfortegnelse" block in
   `extract_sections_text()` (1,221 of 1,231 course_content rows start with
   "Faglig innhold i emnet"); add usn approval-stamp lines to
   `.section_noise` (~110 assessment rows).
8. **ntnu**: strip `<script>` text (`function toggleRooms(...)` in PPU
   assessment); old PPU47xx plans (2014–2016) put assessment rules in
   teaching_methods because they lack a "Mer om vurdering" heading.
9. **Placeholder and noise phrases** (pattern 6) and **exam-table header
   strings** (pattern 1) in `.clean_sections()`.
10. **"Praksis" → teaching_methods** (exact row), per #201 (pattern 3).
11. **uib semester-picker widget** in every course_content row (persisting
    from the morning audit).
12. Not extractor problems: uia autumn-2023 plans holding only the page
    header (harvest), steiner `M-PEL2.2_2025_spring_1` carrying another
    course's plan (source data), uis web plans whose fulltext lacks the
    assessment text (fulltext selector).

## Changes since the morning run

Per-institution counts (before → now): hiof 3→5, hivolda 7→2, hvl 2→3,
inn 5→3, mf 12→4, nih 3→2, nla 4→3, nmbu 4→2, nord 7→10, ntnu 8→8,
oslomet 7→5, steiner 7→1, uia 4→4, uib 6→5, uio 5→5, uis 12→7, usn 5→5.
Packets are smaller and drawn from the rebuilt data, so courses differ; read
counts as direction, not exact deltas. Rises at hiof and nord are mostly new
low-severity items (label loss, aids lines) that full reading (Opus for hiof;
smaller packets) now surfaces; nord's one high item is the COVID-rule
regression (fix 3).

Persisting: uio coursework rarely separated, uia "Faget i praksis" merges,
mf reading list boilerplate, oslomet praksis courses without sections, uib
semester-picker and exam table, ntnu PPU assessment noise.

## Rejected or corrected findings

- `hvl` (coursework_requirements / empty_placeholder): evidence "Ingen." was
  too short to verify; replaced with the verbatim two-line quote from the
  packet. Finding kept.
- No findings rejected. uit's 12 findings are September's and are not
  checkable until #218 restores uit fulltext.
- Run note: five agents first stopped at the session usage limit; nord had
  already written its file, the four Opus agents were re-run after the reset.
