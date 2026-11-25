# Fulltext audit, Fjellhaug close-out (2026-11-25)

Run 2026-11-25 on the build of 2026-11-25 (finished 05:15). First fulltext
audit of Fjellhaug: its pilot (2025 sample) waited for a DBH snapshot with
2026, as the site shows only the plan in force. Not `--pilot` (the pilot
sample's 2025 course-years have no plan): `prepare_fulltext.R fjellhaug --tag
closeout`, 9 suspects, 8 random controls, 3 failed rows. Review agent: Codex
`gpt-6.2-sol`. Findings `findings-closeout/fjellhaug.json`, report
`findings-closeout_report.md`: 2 findings, both verified (the "Les mer"
evidence was lengthened to a verbatim two-line quote).

## Gate numbers (whole build, 2026 offerings)

| metric | value | n | threshold | review |
|---|---|---|---|---|
| fetched | .775 | 275 | .80 | TRUE |
| plan | .771 | 275 | .80 | TRUE |

Reason: DBH codes the subject sitemaps have no page for (62 of 275
course-years without a URL: EVU variants, `N`/`-DK` online variants such as
BTM2008N-DK2, codes not in the sitemaps, as listed in the pilot of
2026-11-05), and one course-year with no page.
On the site; no URL fix finds them, except the two codes below.

## Findings

- **High (rare): wrong course's page.** BTM1003-1 (and BTM1003-DK1,
  -DK2) get `/emne/btm1003-2`, whose page is "RLE1003N Kristen tro, etikk og
  livstolkning"; checked in the raw store, RLE1001-1 likewise gets
  `/emne/rle1001-2`, "RLE1001N Innføring i Bibelen". The site presumably gave the
  online variants' pages the slug of the course they were copied from, and
  `resolve_urls_fjellhaug_batch()` (`R/resolve_course_urls.R`) takes the code
  from the slug before its first hyphen. 8 offerings of 2026 have the wrong page and its 4 autumn offerings
  carry another course's plan; RLE1003N, a DBH code, had no URL in the pilot.
  Fix: read the code from a slug's numbered suffix only when no other page
  has the bare slug, or check the page title's code (the facts box "Kode")
  and fall back; the affected offerings then need a retry harvest. The other
  suffixed slugs (`btm1001n-innføring-i-bibelen-...`, `sje1011-...`) are
  titles and correct.
- Low (occasional): the button label "Les mer" between the paragraphs of an
  expandable description (GRE1002, GRE1003). Fix: an `exclude` for the
  disclosure button in the Fjellhaug config.

The other 8 `dup_code` suspects are the expected sharing of the bare code's
page by `-DK1`, `-DK2`, `-IS`, `-EVU` offerings. The three failed rows have
plausible URLs.

## Verdict

**Not ready** on the wrong-page finding, which needs a resolver fix and a
retry harvest of the BTM1003 and RLE1001 offerings (4 autumn 2026 offerings with a wrong plan);
otherwise complete, clean text.
