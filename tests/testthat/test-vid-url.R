# Tests for VID: the course pages from the sitemap, a code's second page
# chosen by its plans (mocked), the offering's plan read from the page's JSON,
# and a page without any plan, which counts as not found.

vid_page <- function(plans) {
  course <- list(title = "Kunnskapsgrunnlag", code = "BASOS1020", points = 10, language = "Norsk",
                 duration = "Ett semester", studyProgrammes = list(list(title = "Sosialt arbeid")),
                 plans = plans)
  json <- jsonlite::toJSON(list(props = list(component = list(data = list(course = course)))),
                           auto_unbox = TRUE)
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"), body = charToRaw(paste0(
    "<html><body><main><select><option>Velg fra listen</option></select></main>",
    "<script data-react4xp-app-name='vid.no' data-react4xp-ref='root' type='application/json'>",
    json, "</script></body></html>")))
}

vid_plan <- function(year, semester, text = TRUE) {
  blocks <- if (text) list(
    list(paragraph = list(title = "Emnets innhold",
                          content = list(processedHtml = paste0("<p>Innhold ", semester, " ", year, " &amp; mer.</p>"))),
         `_selected` = "paragraph"),
    list(accordion = list(title = "Vurderingsordning", content = "<p>Hjemmeeksamen.</p>",
                          accordionItemSet = list(list(title = "Vurderingsordning", content = "<p>Hjemmeeksamen.</p>"))),
         `_selected` = "accordion"))
  list(year = as.character(year), semester = semester, titleHistory = paste("Kunnskapsgrunnlag", year),
       campus = "Oslo", articleBlocks = blocks %||% list())
}

index <- c("https://www.vid.no/studier/emner/kunnskapsgrunnlag-basos1020",
           "https://www.vid.no/studier/emner/kunnskapsgrunnlag-basos1020-1795878807970940938",
           "https://www.vid.no/studier/emner/folkehelse-basp2040ac-o",
           "https://www.vid.no/studier/emner/folkehelse-basp2040ac",
           "https://www.vid.no/studier/emner/team-og-prosjektledelse-maverd-543",
           "https://www.vid.no/studier/emner/philosophy-of-science-research-ethics-phd-dvp-902")

test_that("VID's pages come from the sitemap by code; a code's pages are read for their plans", {
  local_robots("www.vid.no")
  old <- polite_pause
  polite_pause <<- function(...) invisible(NULL)
  withr::defer(polite_pause <<- old)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(req$url,
      "https://www.vid.no/studier/emner/kunnskapsgrunnlag-basos1020" =
        vid_page(list(vid_plan(2024, "Vår", FALSE), vid_plan(2025, "Vår"))),
      "https://www.vid.no/studier/emner/kunnskapsgrunnlag-basos1020-1795878807970940938" =
        vid_page(list(vid_plan(2024, "Høst"), vid_plan(2025, "Høst"))),
      httr2::response(404))
  })
  df <- tibble::tibble(course_code = c("BASOS1020", "BASOS1020", "BASOS1020", "BASP2040AC", "MAVERD-543",
                                       "MAVERD-543", "BASP1010", "PHD-DVP 902"),
                       year = c(2025L, 2025L, 2023L, 2025L, 2025L, 2023L, 2025L, 2025L),
                       semester_name = c("Vår", "Høst", "Vår", "Høst", "Høst",
                                         "Vår", "Høst", "Høst"),
                       url = NA_character_)
  out <- resolve_urls_vid_batch(df, index = index, .progress = FALSE)
  expect_equal(out$url, c(index[1], index[2], NA, index[4], index[5], NA, NA, index[6]))
  # only the two pages of the code with two pages are read
  expect_equal(length(sent), 2)
})

test_that("the offering's plan is read from the page's JSON as HTML", {
  html <- httr2::resp_body_string(vid_page(list(vid_plan(2024, "Vår", FALSE), vid_plan(2025, "Vår"),
                                                vid_plan(2026, "Vår"))))
  plan <- vid_plan_html(html, "vid_BASOS1020-1_2025_spring_1")
  expect_match(plan, "<h1>Kunnskapsgrunnlag 2025</h1>", fixed = TRUE)
  expect_match(plan, "<dt>Studiested</dt><dd>Oslo</dd>", fixed = TRUE)
  expect_match(plan, "Innhold Vår 2025 &amp; mer", fixed = TRUE)
  expect_true(is.na(vid_plan_html(html, "vid_BASOS1020-1_2024_spring_1")))   # the entry has no text
  expect_true(is.na(vid_plan_html(html, "vid_BASOS1020-1_2025_autumn_1")))   # no entry
  cfg <- .block_cfg(get_institution_config("vid"))
  b <- page_blocks(html, NA, cfg, "vid_BASOS1020-1_2025_spring_1")
  expect_equal(b$section[b$role == "heading"], c("course_content", "assessment"))
  expect_true(all(b$facts[b$text %in% c("Kunnskapsgrunnlag 2025")]))
  expect_equal(nrow(page_blocks(html, NA, cfg, "vid_BASOS1020-1_2027_spring_1")), 0)
})

test_that("a VID page without any plan is not found; VID's config", {
  local_robots("www.vid.no")
  httr2::local_mocked_responses(function(req) switch(req$url,
    "https://www.vid.no/studier/emner/a-basos1020" = vid_page(list(vid_plan(2025, "Vår"))),
    "https://www.vid.no/studier/emner/b-matrs5120" = vid_page(list(vid_plan(2024, "Vår", FALSE))),
    httr2::response(404)))
  expect_equal(httr2::resp_status(fetch_html_cols_single_vid("https://www.vid.no/studier/emner/a-basos1020")), 200L)
  err <- tryCatch(fetch_html_cols_single_vid("https://www.vid.no/studier/emner/b-matrs5120"), error = identity)
  expect_s3_class(err, "vid_not_found_error")
  expect_equal(fetch_outcome("u", FALSE, 200L, fetch_error_text(list(err))), "not_found")
  df <- tibble::tibble(institution = "vid", course_code = "BASOS1020", course_code_raw = "BASOS1020-1",
                       year = 2025L, semester_name = "Høst")
  expect_true(is.na(add_course_url(df)$url))
  expect_equal(get_institution_config("vid")$strategy, "url_discovery")
  expect_equal(institution_from_code("8208"), "vid")
})
