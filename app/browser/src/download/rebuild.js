// R and Python code that rebuilds a download's rows from the published
// release (course_offerings, course_plans, plan_sections; .parquet or .RDS),
// by the keys in keys.csv (and course_year_keys.csv), never by re-running the
// query. The key of a plan version is made in one function of each script.

const RESULT_R = {
  course: 'unique(offerings[c("institution", "course_code")])',
  course_year: 'unique(offerings[c("institution", "course_code", "year")])',
  plan: 'plans',
  section: 'sections',
  hit: 'plans # the plans the hits are in; the hits themselves are not recomputed',
}
const RESULT_PY = {
  course: 'offerings[["institution", "course_code"]].drop_duplicates()',
  course_year: 'offerings[["institution", "course_code", "year"]].drop_duplicates()',
  plan: 'plans',
  section: 'sections',
  hit: 'plans  # the plans the hits are in; the hits themselves are not recomputed',
}

/** rebuild.R of a download of `unit` with `expected` rows (plans for hits). */
export const rebuildR = (unit, expected, version) => `# Rebuilds the rows of this download from the published release files.
# Data version of the download: ${version} (another release can differ).
#
#   Rscript rebuild.R [folder with the release files]   (default: the working directory)
#
# Reads course_offerings, course_plans and plan_sections as .parquet (package
# arrow) or .RDS, and keeps the rows whose keys are in keys.csv (plan versions)
# or course_year_keys.csv (course-years). Leaves offerings, plans, sections and
# result (one row per ${unit}) in the session; source() it to keep them.

args <- commandArgs(trailingOnly = TRUE)
release <- if (length(args)) args[[1]] else "."
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- if (length(script)) dirname(script[[1]]) else "."

read_release <- function(name) {
  pq <- file.path(release, paste0(name, ".parquet"))
  rds <- file.path(release, paste0(name, ".RDS"))
  if (file.exists(pq)) return(as.data.frame(arrow::read_parquet(pq)))
  if (file.exists(rds)) return(as.data.frame(readRDS(rds)))
  stop("not found: ", pq, " or ", rds)
}
read_keys <- function(name) read.csv(file.path(here, name), colClasses = "character", fileEncoding = "UTF-8-BOM")

# The key of a plan version: plan_id, or the same made from its parts in
# releases built before plan_id was added (2026-11-19).
plan_key <- function(d) {
  if ("plan_id" %in% names(d)) return(d$plan_id)
  ifelse(is.na(d$plan_content_id), NA_character_,
         paste0(d$institution, "_", d$course_code, "_", substr(d$plan_content_id, 1, 10)))
}

keys <- plan_key(read_keys("keys.csv"))
plans <- read_release("course_plans")
plans <- plans[plan_key(plans) %in% keys, ]
sections <- read_release("plan_sections")
sections <- sections[plan_key(sections) %in% keys, ]
offerings <- read_release("course_offerings")
if (file.exists(file.path(here, "course_year_keys.csv"))) {
  cy <- read_keys("course_year_keys.csv")
  offerings <- offerings[paste(offerings$institution, offerings$course_code, offerings$year) %in%
                           paste(cy$institution, cy$course_code, cy$year), ]
} else {
  offerings <- offerings[!is.na(plan_key(offerings)) & plan_key(offerings) %in% keys, ]
}

result <- ${RESULT_R[unit]}
cat(sprintf("offerings %d, plans %d, sections %d; result %d rows (the download: %d)\\n",
            nrow(offerings), nrow(plans), nrow(sections), nrow(result), ${expected}L))
if (nrow(result) != ${expected}L) warning("the release differs from the data version of the download")
`

/** rebuild.py, the same in Python (pandas; pyarrow for Parquet, pyreadr for RDS). */
export const rebuildPy = (unit, expected, version) => `"""Rebuilds the rows of this download from the published release files.

Data version of the download: ${version} (another release can differ).

    python rebuild.py [folder with the release files]   (default: the working directory)

Reads course_offerings, course_plans and plan_sections as .parquet (pandas with
pyarrow) or .RDS (pyreadr), and keeps the rows whose keys are in keys.csv (plan
versions) or course_year_keys.csv (course-years). Leaves offerings, plans, sections
and result (one row per ${unit}).
"""
import os
import sys

import pandas as pd

release = sys.argv[1] if len(sys.argv) > 1 else "."
here = os.path.dirname(os.path.abspath(__file__))


def read_release(name):
    pq = os.path.join(release, name + ".parquet")
    if os.path.exists(pq):
        return pd.read_parquet(pq)
    import pyreadr

    return pyreadr.read_r(os.path.join(release, name + ".RDS"))[None]


def read_keys(name):
    return pd.read_csv(os.path.join(here, name), dtype=str, keep_default_na=False, encoding="utf-8-sig")


def plan_key(d):
    """The key of a plan version: plan_id, or the same made from its parts in
    releases built before plan_id was added (2026-11-19)."""
    if "plan_id" in d:
        return d["plan_id"]
    return d["institution"] + "_" + d["course_code"] + "_" + d["plan_content_id"].str[:10]


keys = set(plan_key(read_keys("keys.csv")))
plans = read_release("course_plans")
plans = plans[plan_key(plans).isin(keys)]
sections = read_release("plan_sections")
sections = sections[plan_key(sections).isin(keys)]
offerings = read_release("course_offerings")
if os.path.exists(os.path.join(here, "course_year_keys.csv")):
    cy = read_keys("course_year_keys.csv")
    wanted = set(cy["institution"] + " " + cy["course_code"] + " " + cy["year"])
    offerings = offerings[(offerings["institution"] + " " + offerings["course_code"] + " "
                           + offerings["year"].astype(int).astype(str)).isin(wanted)]
else:
    offerings = offerings[plan_key(offerings).isin(keys)]

result = ${RESULT_PY[unit]}
print(f"offerings {len(offerings)}, plans {len(plans)}, sections {len(sections)}; "
      f"result {len(result)} rows (the download: ${expected})")
if len(result) != ${expected}:
    print("warning: the release differs from the data version of the download", file=sys.stderr)
`
