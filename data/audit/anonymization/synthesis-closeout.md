# Anonymization audit, Fjellhaug close-out (2026-11-25)

Run 2026-11-25. First anonymization audit of Fjellhaug (its pilot audits
waited for a DBH snapshot with 2026). `prepare_anonymization.R fjellhaug
--tag closeout` (the packet runs the anonymizer of the working tree, which
another task was changing in `R/anonymize.R` that day; none of its changes
touches the clock rule below): 20 suspects, 8 random controls. Review agent:
Codex `gpt-6.2-sol`. Findings `findings-closeout/fjellhaug.json`, report
`findings-closeout_report.md`: 1 finding, verified.

## Gate number

`privacy_hits` 1 of 212 plans (threshold 0), read with the working tree's
code at about 19:40: TM1507's reading list, "*Dean, <author's given
names>; ...", a book's author in a reference, matched `role_name_inline`
("Dean" as a staff role) through a comma after the role that an
uncommitted `R/anonymize.R` change then allowed. Re-checked at 20:30: the
pattern at `HEAD` and in the working tree (now a comma only before a title)
does not match it, and target `privacy_check` of the build is empty. So 0;
not personal data.

## Findings

- **Medium (occasional): the clock-time rule removes Bible verse
  references.** "Genesis (1:1-3:24; 11:27-12:9)" becomes "(1:1-3:24;-12:9)",
  "Exodus (3:1-20; 19:1-20:21)" "(3:1-20; 19:1-)", "1 Corinthians 11:26"
  loses "11:26" (BMM1006, BTM2517-DK1, BMM1503; 3 of 20 suspects, 0 of 8
  controls). A chapter:verse pair with an hour-like chapter (≤ 23) and a
  minute-like verse matches `.clock_regex` in `.anon_generic()`
  (`R/anonymize.R`). Course content, not personal data; fix by guarding a
  number pair after a book name or inside a list of passages.

No staff names, e-mails or phone numbers in the 28 plans; the `name_like`
flags are biblical and church names, the `admin_date` flags historical dates
and citations.

## Verdict

**Ready with known limits**: no personal data; the clock-rule over-removal is
a medium fix (rebuild-only); the one `privacy_hits` row was a reference
author, matched only by an intermediate uncommitted change.
