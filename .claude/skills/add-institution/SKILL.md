---
name: add-institution
description: Add harvesting support for a new Norwegian higher education institution by following docs/new-institution.md — robots.txt first (data/robots_txt.csv), the config entry in R/institution_config.R, a URL builder or resolver, a test on a few courses, then the pilot and, with the user's go-ahead, the full harvest. Use when asked to add, onboard or support a new institution.
argument-hint: <institution>
---

# Add Institution

## Usage

```
/add-institution <institution>
```

## Procedure

Follow `docs/new-institution.qmd` step by step: "Adding a New Institution"
("Before You Start", steps 1-5), "Special Cases" where the site needs them
(URL discovery, current plan only, JavaScript, PDFs or JSON, a missing plan
as a normal page, a plan spread over the page), then "Piloting an
Institution". The doc is the single copy of the procedure: if a step there
is wrong or stale, fix the doc, not this file.

## Rules for the agent

- **robots.txt before anything else.** Read each host's robots.txt with
  `site_request()` (the doc's step 1) and add its row to
  `data/robots_txt.csv` before any other request to the host, also a probe
  of a course page. Every request after that goes through `site_request()`
  or `site_html()`.
- **Never add a `robots_permission`** the user has not reported. If the
  plans are on disallowed paths, stop and ask.
- **R work**: start every R process as `CLAUDE.local.md` "Memory limits"
  says, and a harvest also with its own cap.
- **Ask the user before a full harvest** (the doc's "Piloting an
  Institution", step 6), even when all three pilot verdicts are ready; also
  before going on when the plans are behind a login or only excerpts
  ("Before You Start").
- **Record the pilot verdicts** of the three audits in the institution's
  chainlink issue, with the gate table, the `no_sections` course ids and the
  ten most frequent unmapped headings ("Piloting an Institution", step 4).
