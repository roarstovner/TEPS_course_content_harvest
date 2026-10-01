# Sections audit — synthesis (harvest of 2026-09-30/10-01)

Sonnet review agents, one per institution, 10–28 courses each (20 suspects +
8 random controls; up to 5 of the suspects are courses with no sections at
all). 16 institutions audited on tonight's harvest: hiof, hivolda, hvl, inn,
mf, nih, nla, nmbu, nord, oslomet, steiner, uia, uib, uio, uis, usn. All 95
current findings pass mechanical verification after two hand fixes (below).
Report: `findings_report.md`. Opus comparison: `model_comparison.md`.

**Not audited yet:** `uit` (fulltext extraction broken — see fix 1) and
`ntnu` (harvest finishes ~15:00 with the 10 s Crawl-delay). Their findings in
`findings/` are from the September run.

## Summary

- **`sections_raw` is not anonymized.** Sections are cut from raw `html` and
  un-anonymized `extracted_text`; e-mail addresses occur in 78 section rows
  (mf 51, uio 18, usn 9) and in no `course_plan`. Staff contact/approval
  blocks also end up in sections (mf contact card with names; hivolda
  "Emneansvarleg"/"Godkjent av" in ~590 rows; nmbu "Emneansvarlig" in 10).
- **Coursework requirements are rarely separated.** Share of courses with a
  `coursework_requirements` row: nmbu 0 %, nord 0 %, uio 0 %, mf 4 %. The
  text sits in assessment or teaching_methods instead.
- **Several institutions are badly segmented:** hivolda (course_content in
  2 % of courses, prerequisites 0 %), mf (no teaching_methods, real course
  description never extracted), usn MG plans (learning_outcomes swallows the
  plan: 70 of 1,237 rows exceed 10k chars), oslomet praksis courses ("Se
  fagplanen." in 1,762 rows across 914 courses).
- **Clean or nearly clean:** hiof, hvl, nih, nla, uia (apart from one
  unmapped heading), inn Norwegian-heading plans. uib splits correctly, but
  every course_content row (1,473 of 1,473) is wrapped in the page's
  semester-picker widget ("Vel emnebeskrivelse for semester …"), and 294
  assessment rows carry the site banner "Vi opplever problemer med å hente
  inn eksamensinformasjon" — exam information failed to load when the page
  was fetched.
- **Reading lists are mostly pointers** (Leganto/archive links) or absent
  (0 % coverage at hvl, nord, oslomet, steiner, uia, uio). Mostly a source
  limitation, not an extractor bug.

## Cross-institution patterns (by root cause)

1. **Un-anonymized input to `extract_sections()`** — all institutions;
   visible at mf, uio, usn, hivolda, nmbu. `R/run_extract_sections.R` passes
   `df$html` and `df$extracted_text`.
2. **Heading map gaps** — verified with `match_heading_to_section()`, each
   returns `NA` so the text is dropped or appended to the previous section:
   | Heading | Institution | Effect |
   |---|---|---|
   | "Obligatorisk aktivitet" | nmbu | no coursework_requirements |
   | "Knowledge", "Skills", "General competence" | inn (English plans) | learning outcomes reduced to the intro sentence |
   | "Arbeids- og undervisingsformer" (nynorsk) | nla | teaching_methods lost |
   | "Arbeidsform og organisering" | mf | teaching_methods lost |
   | "Faget i praksis" | uia | text appended to previous section |
3. **Headings mapped to prerequisites that carry admission text** —
   "Hvem kan ta dette emnet?" (nih) and "Opptak til emnet" (uio) are mapped to
   prerequisites by design, so Studentweb/admission boilerplate fills the
   section (uio, nih, nord, nmbu, oslomet).
4. **Placeholder rows survive `.clean_sections()`** — "Se fagplanen." (oslomet),
   "Se programplan" (nla), "-" (hvl praksis), "Ingen emner i programmet"
   (nih), "No reading list available" (inn), "..." (nord). The placeholder
   rule only catches bare "Ingen"/"None"/"n/a".
5. **Exam logistics and admin notices inside assessment** — 10 institutions
   (exam-aid tables, "Mer om eksamen ved UiO", COVID/AI-cheating notices at
   nord, plagiarism notice at nih, Karakterregel/Hjelpemiddelkode at nmbu).
   Fixable per institution in `.clean_sections()`; the codebook excludes
   exam logistics.
6. **Weak heading detection where `html_headings` falls back to
   `text_split`** — mf (only three `h2`s on the accordion page), hivolda
   (`text_split` treats the "Pensum:" metadata label as a heading), usn MG
   plans, uis older plans. Symptoms: first learning-outcome bullet dropped
   (mf, steiner, uis), sections running to the end of the page.

## Prioritised fix list

1. **uit: fix the fulltext selector, then re-extract** (blocks the uit
   audit). `harvest_institution("uit")` fetched 3,167 pages and extracted text
   from none: `.hovedfelt > main > div.col-md-12` no longer exists. Candidate
   `.mainContent` (the `div.col-md-7.mainContent` holding all six `h2`s)
   matched 200 of 200 sampled pages, 2009–2025. The HTML is in
   `data/html_uit.RDS`, so only re-extraction is needed, no new fetch.
2. **Anonymize section text** (pattern 1): run `anonymize_fulltext()` over
   `raw_text` in `R/run_extract_sections.R` (or extract from `course_plan`).
   Must happen before sections are published or sent to an external model.
3. **Add the five heading patterns** (pattern 2) in `R/section_heading_map.R`;
   use `exact = TRUE` for the short English words. Examples:
   `nmbu_PPFD201-1_2025_autumn_2`, `inn_2ML351-1_2024_autumn_1`,
   `nla_MGL1NO201-1_2025_autumn_1`, `mf_SAM1080L-1_2025_spring_1`.
4. **Extend the placeholder rule** in `.keep_section_row()` (pattern 4):
   pointer-only rows ("Se fagplanen.", "Se programplan", "Se emnearkivet", "-",
   "...", "Ingen emner i programmet", Leganto-only reading lists). Example:
   `oslomet_M1GP3000-1_2023_autumn_3`.
5. **Stop mapping admission headings to prerequisites** (pattern 3): map
   "Hvem kan ta dette emnet?" and "Opptak til emnet" to nothing, or cut the
   Studentweb paragraph. Examples: `uio_TYSK4091-1_2025_spring_1`,
   `nih_LKI110-1_2025_autumn_1`.
6. **mf: a details/summary strategy** like `extract_sections_uib()` instead of
   `h2` (the Opus agent tried the uib pass on PRA1005 and it split coursework,
   assessment and outcomes correctly). Also recovers the intro description.
7. **Inline coursework gates** (nord, uio): no heading to split on; needs a
   line-prefix rule ("Arbeidskrav", "Obligatorisk deltakelse", "Obligatoriske
   aktiviteter:") or the planned LLM step. Examples:
   `uio_HIS1200L-1_2025_spring_1`, nord (no row in any course).
8. **hivolda / usn / uis: stricter `text_split` heading rules** (short line,
   whole-word match, not a "Label:" metadata line). Examples:
   `hivolda_MGL5-10NO2A-1_2024_spring_1`, `usn_MG1PE2-1_2020_spring_2`,
   `uis_LENG270-1_2015_autumn_1`.
9. **uib: exclude the semester picker from the fulltext selector**, and
   re-fetch the 294 pages whose exam information failed to load (the banner
   text identifies them). Example: any uib course_content row.
10. **Courses with plan text but no sections** (388: uis 191, uia 93, oslomet
   56, nord 48). Sampled ones are mostly *harvest* problems, not splitter
   bugs: page shell only (uia autumn 2023, oslomet), or only the
   "Med overlapp menes…" credit-reduction box (nord). Check the fulltext
   selectors for those years.

## Changes since the previous run

Not comparable course by course: tonight's packets are drawn from a fresh
harvest. Problems reported in September and still present: uio coursework
and prerequisites, mf course_content and reading_list, inn learning
outcomes on English plans, hivolda reading_list/merging. The #198 fix for
"Vilkår for å gå opp til eksamen" holds at uia (now in
coursework_requirements).

## Rejected or corrected findings

- `uio` (Sonnet) cited `uio_PROMO4-1_2025_spring_1`, not in the packet; the
  packet has `uio_PROMO4-1_2025_autumn_4`, which shows the problem. Replaced.
- `hvl`: evidence quoted "Mer om hjelpemiddel"; the packet says "Meir om
  hjelpemiddel". Replaced with the verbatim quote.
- Partial reviews: the nla, nord, oslomet, first uia and uib agents said they
  read only part of their packet (packets of ~185–325 KB). Their findings
  verify, but coverage is incomplete. Asking `n_courses_reviewed` to count
  only courses read in full did not work: the uib agent still reported 28 of
  28 while saying it had read about two thirds. Next run: smaller packets
  for Sonnet (e.g. 14 suspects + 6 random, ≤ 150 KB) or Opus for large ones.
