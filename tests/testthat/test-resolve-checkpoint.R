# Tests for resolve_course_urls()'s checkpoint: a harvest that resumes with
# some offerings resolved before and others not keeps the checkpoint's URLs.
# "nla" has no batch resolver (its rows resolve to NA): no network.

test_that("offerings in the URL checkpoint keep their URL and html when others are resolved", {
  cp <- file.path(withr::local_tempdir(), "urls")
  checkpoint_append(tibble::tibble(course_id = c("a", "x"), url = c("https://a", "https://x"),
                                   html = c("<p>a</p>", NA)), cp)
  df <- tibble::tibble(course_id = c("a", "b"), institution = "nla", url = NA_character_)
  out <- suppressMessages(resolve_course_urls(df, checkpoint_path = cp, .progress = FALSE))
  expect_equal(out$course_id, c("a", "b"))
  expect_equal(out$url, c("https://a", NA))
  expect_equal(out$html, c("<p>a</p>", NA))
  # the new offering is in the checkpoint now, in a part of its own
  expect_setequal(read_checkpoint(cp)$course_id, c("a", "x", "b"))
  expect_equal(basename(checkpoint_parts(cp)), c("part-000001.parquet", "part-000002.parquet"))
})

test_that("a checkpoint is read part by part; a part left half-written by a crash is deleted", {
  cp <- file.path(withr::local_tempdir(), "html")
  page <- function(url) tibble::tibble(url = url, html = paste("page", url), html_error = list(NULL),
                                       html_success = TRUE, http_status = 200L, requested_at = Sys.time())
  checkpoint_append(page(c("u1", "u2")), cp)
  checkpoint_append(page("u3"), cp)
  writeLines("half a part", file.path(cp, "part-000003.parquet.tmp"))
  expect_equal(read_checkpoint(cp)$url, c("u1", "u2", "u3"))
  expect_false(file.exists(file.path(cp, "part-000003.parquet.tmp")))
  expect_true(is.na(read_checkpoint(cp)$html_error[1]))   # kept as text
  expect_equal(read_checkpoint(cp, "url", c("u3", "u9"))$url, "u3")
  expect_null(read_checkpoint(cp, "url", "u9"))
  expect_null(read_checkpoint(file.path(cp, "none")))
  # the next part is numbered after the highest
  expect_equal(basename(checkpoint_append(page("u4"), cp)), "part-000003.parquet")
  # a PDF's bytes (uis) come back as raw vectors
  pdf <- file.path(dirname(cp), "pdf")
  checkpoint_append(tibble::tibble(course_id = c("a", "b"), pdf = list(charToRaw("%PDF"), NULL)), pdf)
  expect_equal(read_checkpoint(pdf)$pdf, list(charToRaw("%PDF"), NULL))
})

test_that("a current site's harvest resumes from checkpoints of this academic year only", {
  withr::local_dir(withr::local_tempdir())
  config <- list(name = "uio")
  cp <- checkpoint_dir("uio", year = 2025)
  part <- function() checkpoint_append(tibble::tibble(url = "u1", html = "page"), cp)
  marker <- file.path(RAW_DIR, "checkpoint", "uio", "academic_year")
  this <- suppressWarnings(prepare_fresh_harvest(config))
  # a harvest that stopped in this academic year resumes
  part()
  suppressWarnings(prepare_fresh_harvest(config))
  expect_length(checkpoint_parts(cp), 1)
  # one started in an earlier academic year, or with refetch, starts again
  writeLines(as.character(as.integer(this) - 1), marker)
  suppressMessages(suppressWarnings(prepare_fresh_harvest(config)))
  expect_length(checkpoint_parts(cp), 0)
  part()
  suppressMessages(suppressWarnings(prepare_fresh_harvest(config, refetch = TRUE)))
  expect_length(checkpoint_parts(cp), 0)
  expect_equal(readLines(marker), this)
  # the marker goes with the last checkpoint
  part()
  suppressMessages(remove_checkpoints("uio", c("2025", "urls")))
  expect_false(dir.exists(file.path(RAW_DIR, "checkpoint", "uio")))
})
