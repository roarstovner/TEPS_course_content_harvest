# Check: fulltext

Audits **fulltext extraction**: whether the harvest's `extracted_text` is a
faithful, clean copy of the course plan on the fetched page: the page's
blocks as text (`R/blocks.R`, `page_fulltext()` in `R/extract_fulltext.R`),
set by `selector`, `exclude` and `post_fn` in `R/institution_config.R`.

- **Default model:** `sonnet`
- **Audited output:** `extracted_text` in `data/interim/extracted_text.RDS` (rebuilt by `targets::tar_make()`)
- **Ground truth:** the fetched page (`html` in `data/raw/html_{inst}.RDS`)
- **Rubric:** below

## Pipeline (orchestrator)

Needs the harvest and `data/interim/extracted_text.RDS`. After a change to
the extraction config, rebuild with `targets::tar_make()` (no new harvest).

```bash
Rscript R/audit/prepare_fulltext.R [inst ...]   # -> data/audit/fulltext/packets/
```

`samas` is skipped (no extracted text by design). Per course the packet shows
metadata, the extracted text, and **page text not in extracted text**: the
page's lines that are not captured, after removing site chrome (lines that
occur on ≥ 40% of the institution's pages — navigation, footers and recurring
headings). A few `[FAILED]` courses show only the URL and the fetch error.

Pre-pass flags: `empty` (page fetched, no text), `short`/`long` (length
outlier within the institution), `wall` (long text, almost no line breaks),
`junk` (cookie banners, page numbers, JavaScript, …), `dup_code` (identical
text for ≥ 3 different course codes), `year` (semester/academic-year labels
all far from the offering's year), `uncaptured` (≥ 1500 chars of non-chrome
page text missing).

## Rubric (review agent)

A good `extracted_text` contains **the whole course plan and nothing else**:

- every part of the plan that is on the page (description, learning
  outcomes, teaching, assessment, requirements, prerequisites, literature,
  and the facts box: credits, level, language, semester);
- no navigation, menus, cookie text, footers, share buttons, page numbers,
  JavaScript, or content from other pages;
- the plan for **this** course code and **this** year/semester (or the
  current plan for institutions without historical plans — UiO, and others
  with `year_in_url FALSE`);
- readable structure: headings and paragraphs on separate lines, table cells
  separated.

### `target`

The part of the plan affected: one of the seven canonical sections
(`course_content`, `learning_outcomes`, `teaching_methods`, `assessment`,
`coursework_requirements`, `prerequisites`, `reading_list`), `metadata` (facts
box: credits, level, language, semester, contact), `page_chrome` (navigation
and other non-plan page text), or `whole_text`.

### `error_type`

| Value | Meaning |
|---|---|
| `missing_content` | Plan content on the page is not in `extracted_text` (selector too narrow, accordion or tab not captured, second column dropped). |
| `junk_included` | Non-plan text captured: navigation, cookie banners, footers, page numbers, JavaScript, related-course lists. |
| `wrong_page` | The text is not this course's plan: a generic fallback page, another course, a programme page, an error page. |
| `wrong_year` | The plan shown is for a different year/semester than the offering. |
| `empty_or_failed` | Nothing extracted although a plan should exist, or the fetch failed because the URL is wrong. |
| `formatting` | Structure lost: wall of text, words glued together, table cells run together, headings merged into paragraphs. |
| `truncated` | The text stops early (the page has more of the same section). |
| `duplicate_content` | The same block appears more than once in the text. |
| `other` | Anything else (explain in `description`). |

## Known failure modes — look for these specifically

- **Generic fallback pages**: a URL that silently returns a default or
  "current plan" page, so many course codes share one text (`dup_code`).
- **Wrong-year content**: the page shows another year's plan (known at INN;
  NORD falls back to a generic page for ASCII semester names).
- **Accordions/tabs** whose content is not in the selected element.
- **Walls of text** where the selector's `html_text()` drops line breaks
  (known at UiA).
- **PDF artefacts**: page numbers ("Siden 12 av 15"), running headers,
  hyphenation (Steiner, UiS PDFs).
- **Staff names and dates** are *not* this check's concern — the
  anonymization step removes them; report them only if they indicate the
  wrong page.

## Not a finding

- `…[truncated N chars]` markers — the packet caps long text.
- Uncaptured page text that is not part of the plan (other courses, news,
  programme info, contact boxes for the department).
- Fetch failures for courses that plausibly have no page (discontinued, not
  offered that year) when the URL pattern looks right.

## Typical fix locations

`R/institution_config.R` (`selector`, `selector_mode`, `pre_fn`, `post_fn`,
`year_in_url`), `R/extract_fulltext.R` (pre/post functions),
`R/add_course_url.R` (URL builders), `R/harvest_strategies.R` (non-standard
strategies).
