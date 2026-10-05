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

institution_fulltext <- function(html_file, config) {
  df <- readRDS(html_file)
  tibble::tibble(course_id = df$course_id, institution = config$name,
                 extracted_text = extract_fulltext_from_raw(df, config))
}

# Offerings with the anonymized course_plan and plan ids: list(plans, courses)
# from deduplicate_plans(). Plans are keyed by institution, so this can run
# per institution.
institution_plans <- function(html_file, fulltext) {
  df <- readRDS(html_file) |>
    dplyr::select(-dplyr::any_of(c("extracted_text", "fulltext", "html", "html_error",
                                   "html_success", "url.x", "url.y"))) |>
    dplyr::left_join(fulltext[, c("course_id", "extracted_text")], by = "course_id")
  df$course_plan <- anonymize_text(df$institution, df$extracted_text, .progress = FALSE)
  deduplicate_plans(df)
}

# Sections are cut from raw text, so they are anonymized here (#208).
institution_sections <- function(html_file, fulltext, config) {
  df <- readRDS(html_file)
  text <- fulltext$extracted_text[match(df$course_id, fulltext$course_id)]
  extract_sections(config, df$html %||% rep(NA_character_, nrow(df)), text,
                   df$course_id) |>
    dplyr::mutate(raw_text = anonymize_text(institution, raw_text, .progress = FALSE)) |>
    dplyr::filter(!is.na(raw_text))
}

# Heading texts the section extractor meets but cannot map, from a sample of
# pages (headings repeat across courses): candidates for R/section_heading_map.R.
unmapped_headings <- function(html_file, config, n = 50) {
  none <- tibble::tibble(institution = character(), heading = character())
  strategy <- config$section_strategy
  if (is.null(strategy) || strategy %in% c("noop", "text_split", "json_nla")) return(none)
  html <- readRDS(html_file)$html
  html <- html[!is.na(html) & nzchar(html)]
  if (length(html) > n) {
    set.seed(42)
    html <- sample(html, n)
  }
  found <- unlist(lapply(html, function(h) tryCatch(
    .collect_heading_candidates(h, strategy, config), error = function(e) character())))
  found <- trimws(found[nzchar(trimws(found))])
  heading <- sort(unique(found[is.na(vapply(found, match_heading_to_section, character(1)))]))
  tibble::tibble(institution = rep(config$name, length(heading)), heading = heading)
}

# --- Combined data files -----------------------------------------------------

# course_offerings_full: every offering with its text, course_plan and plan id.
combine_offerings <- function(plans) {
  dplyr::bind_rows(lapply(plans, `[[`, "courses")) |>
    dplyr::mutate(has_extracted_text = !is.na(extracted_text) & nchar(extracted_text) > 0,
                  extracted_text_nchar = nchar(extracted_text))
}

# Published slim offerings: DBH metadata and the plan id, no url or text.
slim_offerings <- function(offerings) {
  dplyr::select(offerings, -url, -extracted_text, -course_plan, -course_plan_normalized)
}

combine_plans <- function(plans) {
  dplyr::bind_rows(lapply(plans, `[[`, "plans")) |>
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
