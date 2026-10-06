# R/pipeline.R
# Steps of the {targets} pipeline in _targets.R (#263): turn one institution's
# raw harvest (data/raw/html_{inst}.RDS) into its derived tables, then combine the
# institutions into the data files. The harvest itself stays outside the
# pipeline (network, checkpoints); see README "Rebuilding Derived Data".

# --- Per institution ---------------------------------------------------------

# The institution's config as a target value. Functions in it (pre_fn,
# post_fn, fetch_fn) are rebuilt from their source: R byte-compiles a function
# after a few calls, which changes its serialized form and with it the hash
# {targets} uses to decide what is outdated.
institution_config_target <- function(institution) {
  lapply(get_institution_config(institution), function(x) {
    if (is.function(x)) eval(parse(text = deparse(x)), envir = globalenv()) else x
  })
}

# One institution's raw harvest, from harvest_files(). A "current" site
# (plan_years) shows only the plan in force, so each of its harvests counts for
# the academic year it was made in: a DBH row gets the page of the latest
# harvest in its own academic year, or no page (#293). Rows with a page keep
# their harvested_at date.
read_harvest <- function(files, config) {
  if (!identical(config$plan_years, "current")) return(readRDS(files[1]))
  rows <- dplyr::bind_rows(lapply(files, function(f) {
    df <- readRDS(f)
    df <- dplyr::rename(df, dplyr::any_of(c(institution = "institution_short")))
    df$harvested_at <- harvest_date(f, df)
    df
  }))
  rows$in_year <- academic_year_of_date(rows$harvested_at) ==
    nla_academic_year(rows$Årstall, rows$Semesternavn)
  rows <- rows |>
    dplyr::arrange(course_id, dplyr::desc(in_year), dplyr::desc(harvested_at)) |>
    dplyr::distinct(course_id, .keep_all = TRUE)
  miss <- !rows$in_year
  rows$html[miss] <- NA_character_
  rows$extracted_text[miss] <- NA_character_
  rows$html_success[miss] <- FALSE
  rows$harvested_at[miss] <- NA
  dplyr::select(rows, -in_year)
}

# extracted_text of every row: the page's blocks as text (#276); rows without
# blocks (PDF plans, usn's rendered text) as extract_fulltext_from_raw() gives
# them.
institution_fulltext <- function(html_file, blocks, config) {
  df <- read_harvest(html_file, config)
  pages <- split(blocks[names(.empty_blocks())], blocks$course_id)
  text <- vapply(pages, page_fulltext, character(1), config = config)[df$course_id]
  rest <- !df$course_id %in% names(pages)
  text[rest] <- extract_fulltext_from_raw(df[rest, ], config)
  tibble::tibble(course_id = df$course_id, institution = config$name,
                 extracted_text = unname(text))
}

# Offerings with the anonymized course_plan and plan ids: list(plans, courses)
# from deduplicate_plans(). Plans are keyed by institution, so this can run
# per institution.
institution_plans <- function(html_file, fulltext, config) {
  df <- read_harvest(html_file, config) |>
    dplyr::select(-dplyr::any_of(c("extracted_text", "fulltext", "html", "html_error",
                                   "html_success", "url.x", "url.y"))) |>
    dplyr::left_join(fulltext[, c("course_id", "extracted_text")], by = "course_id")
  df$course_plan <- anonymize_text(df$institution, df$extracted_text, .progress = FALSE)
  df$plan_year_basis <- config$plan_years   # how the plan's year is known (#293)
  deduplicate_plans(df)
}

# Every page with HTML read once into blocks (R/blocks.R; #272): course_id
# plus the block columns. Institutions read from text (usn, steiner) have no
# blocks here; their sections read the extracted_text.
institution_blocks <- function(html_file, config) {
  df <- read_harvest(html_file, config)
  cfg <- .block_cfg(config)
  html <- df$html %||% rep(NA_character_, nrow(df))
  read <- cfg$reader %in% c("html", "json") & !is.na(html) & nzchar(html)
  blocks <- lapply(which(read), function(i) page_blocks(html[i], NA, cfg, df$course_id[i]))
  dplyr::bind_rows(tibble::tibble(course_id = character()), .empty_blocks(),
                   tibble::tibble(course_id = rep(df$course_id[read], vapply(blocks, nrow, 1L)),
                                  dplyr::bind_rows(.empty_blocks(), blocks)))
}

# Sections per plan (#273), cut from the page of the offering whose text the
# plan keeps (source_course_id), so a plan's sections and its course_plan come
# from the same page. Sections are cut from raw text, so they are anonymized
# here (#208).
institution_sections <- function(blocks, fulltext, config, plans) {
  src <- plans$plans[, c("plan_content_id", "institution", "Emnekode", "source_course_id")]
  cfg <- .block_cfg(config)
  pages <- split(blocks[blocks$course_id %in% src$source_course_id, names(.empty_blocks())],
                 blocks$course_id[blocks$course_id %in% src$source_course_id])
  text <- fulltext$extracted_text[match(src$source_course_id, fulltext$course_id)]
  rows <- lapply(seq_len(nrow(src)), function(i) {
    out <- page_sections(pages[[src$source_course_id[i]]] %||% .empty_blocks(), text[i], cfg)
    dplyr::bind_cols(src[rep(i, nrow(out)), ], out)
  })
  dplyr::bind_rows(src[0, ], .empty_sections(), rows) |>
    dplyr::mutate(text = anonymize_text(institution, raw_text, .progress = FALSE)) |>
    dplyr::filter(!is.na(text)) |>
    dplyr::select(plan_content_id, institution, Emnekode, source_course_id, section, text)
}

# Headings the section extractor meets but cannot map, with the number of
# pages they are on: candidates for R/section_heading_map.R.
unmapped_headings <- function(blocks, config) {
  blocks |>
    dplyr::filter(role == "heading", is.na(section), nzchar(trimws(text))) |>
    dplyr::transmute(institution = config$name, heading = stringr::str_squish(text), course_id) |>
    dplyr::distinct() |>
    dplyr::count(institution, heading, name = "n_pages", sort = TRUE)
}

# Pages on which each heading-map pattern opens a section (#277): headings
# and sub-headings read from the page, or for the text reader (usn, steiner)
# the heading lines of extracted_text. The html reader's text fallback (uis
# PDF plans) is not counted.
institution_heading_hits <- function(blocks, fulltext, config) {
  if (identical(.block_cfg(config)$reader, "text")) {
    text <- fulltext$extracted_text[!is.na(fulltext$extracted_text)]
    ids <- fulltext$course_id[!is.na(fulltext$extracted_text)]
    pages <- lapply(text, .text_blocks, cfg = .block_cfg(config))
    blocks <- dplyr::bind_rows(.empty_blocks(), tibble::tibble(
      course_id = rep(ids, vapply(pages, nrow, 1L)), dplyr::bind_rows(.empty_blocks(), pages)))
  }
  b <- blocks[blocks$role %in% c("heading", "sub") & !is.na(blocks$section), ]
  row <- vapply(stringr::str_squish(b$text), heading_pattern, integer(1),
                word_start = identical(.block_cfg(config)$reader, "text"), USE.NAMES = FALSE)
  # field headings (hivolda) take their section from the field, not the text
  keep <- !is.na(row) & section_heading_patterns$section[row] == b$section
  tibble::tibble(institution = config$name, pattern = section_heading_patterns$pattern[row[keep]],
                 course_id = b$course_id[keep]) |>
    dplyr::distinct() |>
    dplyr::count(institution, pattern, name = "n_pages")
}

# Every heading-map pattern with the pages it opens a section on, over all
# institutions: a pattern on no page is a candidate for removal.
heading_pattern_use <- function(hits) {
  used <- hits |>
    dplyr::group_by(pattern) |>
    dplyr::summarise(n_pages = sum(n_pages),
                     institutions = paste(sort(institution), collapse = ", "), .groups = "drop")
  section_heading_patterns[, c("pattern", "section")] |>
    dplyr::left_join(used, by = "pattern") |>
    dplyr::mutate(n_pages = dplyr::coalesce(n_pages, 0L))
}

# --- Combined data files -----------------------------------------------------

# course_offerings_full: every offering with its text, course_plan and plan id.
# A page with no section to read and under SHELL_MAX_NCHAR characters is a
# page shell, not a plan: the facts box only (uia), a credit-overlap box
# (nord), "Emnebeskrivelse" (uib), a pointer to the English page (hvl), every
# section "Se fagplanen." (oslomet 2018 practicum) (#222). Longer pages without
# sections stay: oslomet practicum courses keep the programme's practicum plan
# (#242). The offerings keep their text but get no plan.
SHELL_MAX_NCHAR <- 1500

.shell_plans <- function(plans, sections) {
  key <- c("plan_content_id", "institution", "Emnekode")
  dplyr::bind_rows(lapply(plans, `[[`, "plans")) |>
    dplyr::anti_join(sections, by = key) |>
    dplyr::filter(nchar(course_plan) < SHELL_MAX_NCHAR) |>
    dplyr::select(dplyr::all_of(key))
}

combine_offerings <- function(plans, sections) {
  shell <- do.call(paste, .shell_plans(plans, sections))
  dplyr::bind_rows(lapply(plans, `[[`, "courses")) |>
    dplyr::mutate(
      has_extracted_text = !is.na(extracted_text) & nchar(extracted_text) > 0,
      extracted_text_nchar = nchar(extracted_text),
      plan_content_id = dplyr::if_else(
        paste(plan_content_id, institution, Emnekode) %in% shell, NA_character_, plan_content_id)
    )
}

# Published slim offerings: DBH metadata and the plan id, no url or text.
slim_offerings <- function(offerings) {
  dplyr::select(offerings, -url, -extracted_text, -course_plan, -course_plan_normalized)
}

combine_plans <- function(plans, sections) {
  dplyr::bind_rows(lapply(plans, `[[`, "plans")) |>
    dplyr::anti_join(.shell_plans(plans, sections),
                     by = c("plan_content_id", "institution", "Emnekode")) |>
    dplyr::arrange(plan_content_id, institution, Emnekode)
}

# Write `x` and return the path, for format = "file" targets.
write_rds_file <- function(x, path, ...) {
  saveRDS(x, path, ...)
  path
}

# Changes beyond tolerance against the metrics snapshot (#255). The warning
# makes them show in tar_make(); the test in test-pipeline-metrics.R fails on
# them too.
metrics_vs_snapshot <- function(snapshot, metrics) {
  changes <- compare_metrics(utils::read.csv(snapshot), metrics)
  if (nrow(changes) > 0) {
    warning(nrow(changes), " metric(s) differ from ", snapshot,
            " beyond tolerance: see tar_read(metrics_check)", call. = FALSE)
  }
  changes
}

# Personal data found in the shareable files (data/processed/). The warning
# makes it show in tar_make(); test-anonymize.R fails on it too.
check_personal_data <- function(files) {
  found <- personal_data_in(files)
  if (nrow(found) > 0) {
    warning("personal data in ", paste(unique(found$file), collapse = ", "),
            ": see tar_read(privacy_check)", call. = FALSE)
  }
  found
}

# Run a script for the files it writes. `inputs` is not used: naming the input
# targets in the call is what makes {targets} rerun the script when they change.
run_r_script <- function(script, outputs, inputs = NULL, chdir = FALSE) {
  source(script, local = new.env(), chdir = chdir)
  outputs
}

render_quarto <- function(qmd, output, inputs = NULL) {
  if (system2("quarto", c("render", qmd)) != 0) stop("quarto render failed: ", qmd)
  output
}
