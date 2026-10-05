# app/course_browser/build_data.R
# Builds the slim payload the course_browser app loads at startup.
#
# The app is plan-centric: one row per unique course plan, with offering
# coverage rolled up onto it. Doing the joins here rather than in the app keeps
# startup fast and keeps the payload small enough to ship to a browser later
# (shinylive / GitHub Pages).
#
# Rebuilt by targets::tar_make() (target browser_data_file). By hand, from
# this directory:  Rscript build_data.R
#
# Inputs:  data/processed/course_plans.RDS, data/interim/course_offerings_full.RDS,
#          data/processed/sections_raw.RDS (optional)
# Output:  app/course_browser/data/browser_data.RDS (gitignored)

library(dplyr, warn.conflicts = FALSE)

message("Loading inputs...")
plans <- readRDS("../../data/processed/course_plans.RDS")
offerings <- readRDS("../../data/interim/course_offerings_full.RDS")

sections_path <- "../../data/processed/sections_raw.RDS"
sections_raw <- if (file.exists(sections_path)) readRDS(sections_path) else NULL

# ── Offering coverage per plan ───────────────────────────────────────────────
# A plan is shared by 1-16 offerings. The researcher needs to see which
# offerings a plan covers, because that is the difference between counting
# documents and counting what was actually taught.

message("Rolling offerings up to plan level...")

# `plans` is keyed by (plan_content_id, institution, Emnekode); a handful
# of plan_content_ids are shared across course codes, so join on all three.
plan_keys <- plans |> select(plan_content_id, institution, Emnekode)

offering_rollup <- offerings |>
  filter(!is.na(plan_content_id)) |>
  semi_join(plan_keys, by = c("plan_content_id", "institution", "Emnekode")) |>
  summarise(
    Emnenavn = first(na.omit(Emnenavn)),
    Fagnavn = {
      f <- na.omit(Fagnavn)
      if (length(f) == 0) NA_character_ else names(sort(table(f), decreasing = TRUE))[1]
    },
    Studiepoeng = first(na.omit(Studiepoeng)),
    Nivanavn = first(na.omit(Nivånavn)),
    n_offerings = n(),
    years = paste(sort(unique(Årstall)), collapse = ", "),
    semesters = paste(sort(unique(Semesternavn)), collapse = ", "),
    url = first(na.omit(url)),
    course_ids = list(course_id),
    .by = c(plan_content_id, institution, Emnekode)
  )

plans <- plans |>
  left_join(offering_rollup,
            by = c("plan_content_id", "institution", "Emnekode")) |>
  mutate(
    n_offerings = coalesce(n_offerings, 0L),
    plan_nchar = nchar(course_plan)
  )

message("  ", nrow(plans), " plans, ",
        sum(plans$n_offerings), " offerings covered")

# ── Sections per plan ────────────────────────────────────────────────────────
# sections_raw is keyed by course_id (offering level). The plan text is
# identical across a plan's offerings, so one representative offering's
# sections describe the plan. Take the longest extraction per (plan, section)
# — extraction occasionally truncates, and the longest is the safest pick.

sections <- NULL
if (!is.null(sections_raw)) {
  message("Rolling sections up to plan level...")
  attr(sections_raw$raw_text, "names") <- NULL

  course_to_plan <- offerings |>
    filter(!is.na(plan_content_id)) |>
    select(course_id, plan_content_id, institution, Emnekode)

  sections <- sections_raw |>
    select(course_id, section, raw_text) |>
    inner_join(course_to_plan, by = "course_id") |>
    filter(!is.na(raw_text), nchar(raw_text) > 0) |>
    slice_max(nchar(raw_text), n = 1, with_ties = FALSE,
              by = c(plan_content_id, institution, Emnekode, section)) |>
    select(plan_content_id, institution, Emnekode, section, raw_text)

  message("  ", nrow(sections), " plan-level sections across ",
          n_distinct(sections$section), " section types")
}

# ── Harvest coverage by institution x year ───────────────────────────────────
# 41% of offerings have no extracted text, and the rate varies hugely by
# institution and year. Any proportion computed from these data has a
# denominator of successfully harvested plans only, so the app shows this
# explicitly rather than letting it stay invisible.

message("Computing harvest coverage...")
coverage <- offerings |>
  summarise(
    n_offerings = n(),
    n_harvested = sum(has_extracted_text, na.rm = TRUE),
    n_plans = n_distinct(plan_content_id[!is.na(plan_content_id)]),
    .by = c(institution, Årstall)
  ) |>
  mutate(pct_harvested = n_harvested / n_offerings) |>
  arrange(institution, Årstall)

# ── Write ────────────────────────────────────────────────────────────────────

browser_data <- list(
  plans = plans,
  sections = sections,
  coverage = coverage,
  built_at = Sys.time()
)

dir.create("data", showWarnings = FALSE)
saveRDS(browser_data, "data/browser_data.RDS", compress = "xz")

message("\nSaved app/course_browser/data/browser_data.RDS (",
        round(file.size("data/browser_data.RDS") / 1e6, 1), " MB)")
message("  plans:     ", nrow(plans))
message("  sections:  ", if (is.null(sections)) 0 else nrow(sections))
message("  coverage:  ", nrow(coverage), " institution-year rows")
message("  overall harvested: ",
        sprintf("%.0f%%", 100 * sum(coverage$n_harvested) / sum(coverage$n_offerings)))
