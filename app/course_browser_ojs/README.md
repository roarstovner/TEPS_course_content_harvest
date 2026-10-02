# Course Plan Browser — Observable JS (OJS) proof of concept

A **static** alternative to the Shiny `course_browser`: a single Quarto OJS page that loads
the deduplicated, anonymized course plans into the browser and does live full-text search
client-side. No R/Shiny server and no webR runtime to boot — just a static HTML page plus
two Parquet files.

This is a POC (issue #198) to feel the load + search behaviour before deciding whether to
port the full app. Scope: Browse + full-text search. The Diff tab is intentionally out of
scope (it depends on R's `diffobj`).

## Build & run

```sh
# 1. Generate the Parquet data from the RDS datasets (run from the repo root):
Rscript -e 'source("app/course_browser_ojs/build_data.R", chdir=TRUE)'

# 2. Preview the page:
quarto preview app/course_browser_ojs/index.qmd
```

`build_data.R` reads `../../data/course_offerings.RDS` + `../../data/course_plans.RDS` and
writes `data/plans.parquet` (~19 MB, the searchable corpus) and `data/offerings.parquet`
(~1 MB, the "used by" detail) into this folder.

## Prerequisites

- **Quarto** (already used in this repo).
- A Parquet writer R package: **`arrow`** (preferred — real zstd) or **`nanoparquet`**
  (lighter; used with `gzip` because nanoparquet 0.5.1's `zstd` is a silent no-op).

  ```r
  install.packages("nanoparquet")   # or install.packages("arrow")
  ```

## Notes

- The Parquet files are a **regenerable build artifact** (gitignored). The source of truth is
  the RDS in `data/`. `targets::tar_make()` rebuilds them when the RDS change.
- Search/display use the anonymized `course_plan` column only — never the raw `extracted_text`.
- A plan maps to many offerings (same plan reused across years/semesters), so a search hit
  is a *plan*; `offerings.parquet` lists the offerings that use it.

## Possible next steps

- If client-side search feels slow at ~12k plans → DuckDB-WASM (`DuckDBClient.of`) with SQL
  `ILIKE`/FTS, range-requesting only the row groups a query touches.
- If this graduates into the maintained, multi-view published dashboard → migrate to
  Observable Framework (issue #199): R-script-as-data-loader, code-split builds.
