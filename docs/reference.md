# Reference


## Key Functions Reference

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Function</th>
<th>File</th>
<th>Purpose</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>harvest_institution(institution, courses, year, refetch)</code></td>
<td><code>R/harvest.R</code></td>
<td>Harvest one institution with its configured strategy</td>
</tr>
<tr>
<td><code>harvest_all(courses, years, refetch, institutions, retry)</code></td>
<td><code>R/harvest.R</code></td>
<td>Harvest all (or the given) institutions, year by year, newest first;
each year adds a run,
<code>data/raw/{inst}/{year}/{date}.parquet</code></td>
</tr>
<tr>
<td><code>write_raw_run(rows, institution, run)</code>,
<code>read_raw(files)</code></td>
<td><code>R/utils.R</code></td>
<td>Write a harvest run to the raw store; read raw files in harvest
order</td>
</tr>
<tr>
<td><code>fetch_ledger(rows, institution, run)</code>,
<code>raw_ledger(files)</code></td>
<td><code>R/utils.R</code></td>
<td>A run’s fetch ledger; the ledger of raw files, recorded or
reconstructed (target <code>ledger</code>)</td>
</tr>
<tr>
<td><code>fetch_outcome(url, page, status, error)</code>,
<code>offering_outcome(course_id, url, ledger)</code></td>
<td><code>R/utils.R</code></td>
<td>Outcome class of a fetch; how an offering’s URL answered last
(retries)</td>
</tr>
<tr>
<td><code>site_request(url)</code></td>
<td><code>R/fetch_html_cols.R</code></td>
<td>A request to a course site: user agent, timeout, retries of timeouts
and server errors; refuses a URL robots.txt disallows</td>
</tr>
<tr>
<td><code>refusal_watch(config)</code></td>
<td><code>R/fetch_html_cols.R</code></td>
<td>The check of a loop of requests to a site: stops it when the site
refuses the harvest (HTTP 429, or 403 to <code>FORBIDDEN_RUN</code>
requests in a row and to a page known to be public)</td>
</tr>
<tr>
<td><code>robots_allows(url)</code></td>
<td><code>R/fetch_html_cols.R</code></td>
<td>Whether robots.txt (<code>data/robots_txt.csv</code>) lets the
harvest request a URL; an error for a host not read (<a
href="harvesting.md#robotstxt">“robots.txt”</a>)</td>
</tr>
<tr>
<td><code>pilot_sample(institution, oldest_year)</code>,
<code>pilot_gate(institution)</code></td>
<td><code>R/harvest.R</code>, <code>R/pipeline_metrics.R</code></td>
<td>The pilot of an institution: its stratified sample of DBH
course-years; the gate numbers of the sample in the build
(<code>gate_numbers()</code>)</td>
</tr>
<tr>
<td><code>fetch_coverage(frame, ledger, offerings)</code>,
<code>page_changes(ledger, files, config)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Fetch outcome, plan and retry of every offering of a frame; pages
changed between runs (targets <code>coverage</code>,
<code>changed_pages</code>)</td>
</tr>
<tr>
<td><code>raw_partitions()</code>,
<code>read_raw(files, content = FALSE)</code>,
<code>raw_page_id(file, row)</code></td>
<td><code>R/utils.R</code></td>
<td>The raw store’s institution x year partitions; raw rows with the
page id in place of the page</td>
</tr>
<tr>
<td><code>partition_blocks()</code>, <code>partition_text()</code>,
<code>partition_sections()</code></td>
<td><code>R/pipeline.R</code></td>
<td>Per raw partition: every page’s blocks, text and plan
(<code>page_plans()</code>), and the sections of the pages plans are cut
from</td>
</tr>
<tr>
<td><code>plan_offerings(pages, page_text, config)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Every offering with the text, <code>course_plan</code> and plan id
of its page (target <code>courses</code>)</td>
</tr>
<tr>
<td><code>frame_members()</code>, <code>frame_plans()</code>,
<code>frame_sections()</code>, <code>frame_offerings()</code>,
<code>frame_dir()</code></td>
<td><code>R/pipeline.R</code>, <code>R/utils.R</code></td>
<td>The frames: their offerings, plans (<code>plan_table()</code>),
sections and data folders</td>
</tr>
<tr>
<td><code>harvest_files(institution)</code>,
<code>harvested_institutions()</code></td>
<td><code>R/utils.R</code></td>
<td>An institution’s raw files, run by run; the institutions with raw
files</td>
</tr>
<tr>
<td><code>dbh_courses(frame, date)</code></td>
<td><code>R/utils.R</code></td>
<td>DBH offerings of a snapshot: all courses, or the teacher-ed frame
(<code>"teacher_ed"</code>)</td>
</tr>
<tr>
<td><code>dbh_rename(df)</code></td>
<td><code>R/utils.R</code></td>
<td>DBH’s column names (snapshots, raw files and releases until
2026-10-16) as the pipeline’s (<code>DBH_COLUMNS</code>)</td>
</tr>
<tr>
<td><code>institution_from_code(code)</code></td>
<td><code>R/institution_config.R</code></td>
<td>Institution short name for DBH institution codes</td>
</tr>
<tr>
<td><code>get_institution_config(inst)</code></td>
<td><code>R/institution_config.R</code></td>
<td>Config list for one institution</td>
</tr>
<tr>
<td><code>add_course_id(dbh_df)</code></td>
<td><code>R/utils.R</code></td>
<td>Unique id from institution, raw code, year, semester, status</td>
</tr>
<tr>
<td><code>add_course_url(df)</code></td>
<td><code>R/add_course_url.R</code></td>
<td>Institution-specific URL builders; NA where discovery is needed</td>
</tr>
<tr>
<td><code>resolve_course_urls(df, checkpoint_path)</code></td>
<td><code>R/resolve_course_urls.R</code></td>
<td>URL discovery (USN, UiT, hivolda, UiO, HiOF) with checkpointing</td>
</tr>
<tr>
<td><code>fetch_html_with_checkpoint(courses, checkpoint_dir)</code></td>
<td><code>R/checkpoint.R</code></td>
<td>Download HTML, each URL once, skipping URLs already in the
institution’s checkpoints</td>
</tr>
<tr>
<td><code>checkpoint_dir()</code>, <code>read_checkpoint()</code>,
<code>checkpoint_append()</code>, <code>remove_checkpoints()</code></td>
<td><code>R/checkpoint.R</code></td>
<td>The checkpoints: folders of append-only Parquet parts (<a
href="harvesting.md#about-checkpointing">“About Checkpointing”</a>)</td>
</tr>
<tr>
<td><code>extract_fulltext_from_raw(df, config)</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td><code>extracted_text</code> from the raw harvest, per strategy</td>
</tr>
<tr>
<td><code>page_blocks(html, text, cfg)</code></td>
<td><code>R/blocks.R</code></td>
<td>Read one page into a block table</td>
</tr>
<tr>
<td><code>page_fulltext(blocks, config)</code></td>
<td><code>R/extract_fulltext.R</code></td>
<td>A page’s <code>extracted_text</code> from its blocks</td>
</tr>
<tr>
<td><code>read_harvest(files, config, content)</code></td>
<td><code>R/pipeline.R</code></td>
<td>One institution’s raw files combined (<code>harvest_files()</code>):
the page each offering takes; current sites matched by course code and
academic year</td>
</tr>
<tr>
<td><code>harvested_rows(institutions)</code></td>
<td><code>R/pipeline.R</code></td>
<td><code>read_harvest()</code> joined with
<code>data/interim/extracted_text.RDS</code> (audit scripts)</td>
</tr>
<tr>
<td><code>finalize_release(tag)</code>,
<code>frozen_changes()</code></td>
<td><code>R/release.R</code></td>
<td>Lock a release’s raw files; list changes to it (target
<code>frozen_check</code>)</td>
</tr>
<tr>
<td><code>release_data(tag, dest, frame)</code></td>
<td><code>R/release.R</code></td>
<td>Copy the published files of a frame (the teacher-ed one),
<code>methods.md</code>, <code>data/variables.csv</code>, a manifest and
section coverage to the coding project</td>
</tr>
<tr>
<td><code>plan_gaps(offerings, fetch_status)</code></td>
<td><code>R/pipeline.R</code></td>
<td>Course-years without a plan, with the reason (target
<code>gaps</code>)</td>
</tr>
<tr>
<td><code>validate_courses(df, stage)</code></td>
<td><code>R/utils.R</code></td>
<td>Required columns at <code>"initial"</code>, <code>"with_url"</code>,
<code>"with_html"</code></td>
</tr>
<tr>
<td><code>anonymize_text(institution, text)</code></td>
<td><code>R/anonymize.R</code></td>
<td>PII and admin-date removal for <code>course_plan</code> and
sections</td>
</tr>
<tr>
<td><code>normalize_plan_text(course_plan)</code></td>
<td><code>R/normalize_plan_text.R</code></td>
<td>Lossy normalization for dedup hashing</td>
</tr>
<tr>
<td><code>plan_ids(id_text)</code>, <code>plan_table(courses)</code>,
<code>deduplicate_plans(df)</code></td>
<td><code>R/deduplicate_plans.R</code></td>
<td>Content hashes (<code>plan_content_id</code>); the unique plans of a
set of offerings</td>
</tr>
<tr>
<td><code>page_sections(blocks, text, cfg)</code></td>
<td><code>R/extract_sections.R</code></td>
<td>A page’s sections from its blocks</td>
</tr>
<tr>
<td><code>extract_sections(config, html, extracted_text, course_id)</code></td>
<td><code>R/extract_sections.R</code></td>
<td><code>page_sections()</code> for many pages</td>
</tr>
<tr>
<td><code>wayback_cdx(url, match_type, from, to, filter, collapse)</code>,
<code>wayback_capture_url(timestamp, original)</code></td>
<td><code>R/wayback.R</code></td>
<td>Captures of a URL or URL prefix in the Wayback Machine; a capture’s
address as archived. Not used by the harvest</td>
</tr>
</tbody>
</table>

## File Organization

    ├── _targets.R                 # Post-harvest pipeline: targets::tar_make()
    ├── methods.qmd                # Methods choices for articles (rendered to methods.md)
    ├── course_plan_spec.md        # What course_plan and the sections hold, and what stays out
    ├── renv.lock                  # Pinned package versions (renv::restore())
    ├── .github/workflows/tests.yml # Tests on GitHub Actions
    ├── R/
    │   ├── harvest.R              # Entry points: harvest_institution(), harvest_all(); pilot_sample()
    │   ├── harvest_strategies.R   # Strategy implementations
    │   ├── institution_config.R   # Institution registry (strategy, selectors, overrides, section_*)
    │   ├── utils.R                # Helpers (data paths, add_course_id, validation, normalization)
    │   ├── add_course_url.R       # URL generation logic
    │   ├── resolve_course_urls.R  # URL discovery for USN, UiT, hivolda, UiO, HiOF
    │   ├── fetch_html_cols.R      # HTML downloading (httr2)
    │   ├── extract_fulltext.R     # Config-driven text extraction
    │   ├── checkpoint.R           # Checkpoint management
    │   ├── anonymize.R            # PII removal: anonymize_text() for course_plan and sections
    │   ├── normalize_plan_text.R  # Lossy normalization for dedup hashing
    │   ├── deduplicate_plans.R    # Groups identical plans by content hash
    │   ├── section_heading_map.R  # Heading → section patterns (incl. ".drop")
    │   ├── extract_sections.R     # Section extraction strategies + cleanup
    │   ├── pipeline.R             # Steps of the {targets} pipeline (_targets.R)
    │   ├── pipeline_metrics.R     # Regression snapshot: check_pipeline_metrics(); the pilot gate numbers
    │   ├── release.R              # release_data(), finalize_release(), frozen_changes()
    │   ├── wayback.R              # Wayback Machine captures (CDX API); not used by the harvest
    │   └── audit/                # Audit harness for /audit-institutions
    ├── app/
    │   ├── course_browser/        # Shiny concordance browser; build_data.R → data/ (see its README)
    │   └── course_browser_ojs/    # Static Observable JS browser; build_data.R → data/ (see its README)
    ├── docs/                      # Documentation pages, docs/*.qmd rendered to .md (README "Documentation")
    ├── data/                      # docs/data-files.md "Data Files: Published and Internal"
    │   ├── input/                 # DBH snapshots dbh_{courses,programmes}_{date}.parquet (in git)
    │   ├── pilot/                 # Pilot samples {inst}.csv (in git)
    │   ├── raw/                   # Harvest: {inst}/{year}/{run}.parquet, {inst}/ledger/, checkpoint/ (not in git)
    │   ├── interim/               # Rebuilt text with personal data (not in git); teacher_ed/: that frame's
    │   ├── processed/             # Anonymized data files, can be shared (not in git); teacher_ed/: that frame's
    │   ├── releases/              # Finalized releases' data (not in git)
    │   ├── backup/                # Earlier harvests, not read (not in git)
    │   ├── audit/                 # Audit findings and reports
    │   ├── variables.csv          # Every variable of the data files, old and new names
    │   ├── robots_txt.csv         # Each host's robots.txt: date read, crawl delay, disallow/allow rules
    │   └── data_notes.qmd         # Data quality notes per institution
    ├── tests/
    │   ├── testthat/              # Rscript -e 'testthat::test_dir("tests/testthat")'
    │   └── snapshots/             # pipeline_metrics_{frame}.csv; raw_manifest.csv once a release is finalized
    └── data-raw/
        ├── courses.R              # script that writes a DBH snapshot to data/input/
        └── raw_to_parquet.R       # the one-time conversion of the RDS raw files (2026-10-16)
