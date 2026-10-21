# Tests for resolve_course_urls()'s checkpoint: a harvest that resumes with
# some offerings resolved before and others not keeps the checkpoint's URLs.
# "nla" has no batch resolver (its rows resolve to NA): no network.

test_that("offerings in the URL checkpoint keep their URL and html when others are resolved", {
  cp <- withr::local_tempfile(fileext = ".RDS")
  write_checkpoint(tibble::tibble(course_id = c("a", "x"), url = c("https://a", "https://x"),
                                  html = c("<p>a</p>", NA)), cp)
  df <- tibble::tibble(course_id = c("a", "b"), institution = "nla", url = NA_character_)
  out <- suppressMessages(resolve_course_urls(df, checkpoint_path = cp, .progress = FALSE))
  expect_equal(out$course_id, c("a", "b"))
  expect_equal(out$url, c("https://a", NA))
  expect_equal(out$html, c("<p>a</p>", NA))
  # the new offering is in the checkpoint now
  expect_setequal(read_checkpoint(cp)$course_id, c("a", "x", "b"))
})
