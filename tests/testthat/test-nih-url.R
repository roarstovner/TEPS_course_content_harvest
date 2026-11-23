test_that("NIH page URLs: lower case, Norwegian letters transliterated", {
  expect_equal(as.character(add_course_url_nih("LKI230", 2022L, "Høst")),
               "https://www.nih.no/studier/emner/2022/host/lki230.html")
  expect_equal(as.character(add_course_url_nih("KRØ100", 2025L, "Vår")),
               "https://www.nih.no/studier/emner/2025/var/kro100.html")
})

test_that("NIH: the English tree's page where the Norwegian URL answered 404, else the Norwegian URL", {
  pages <- vortex_pages(paste0("https://www.nih.no/english/studies/courses/2026/",
                               c("spring/thp111.html", "autumn/lki152.html", "spring/kro200.html")))
  df <- tibble::tibble(course_code = c("THP111", "LKI152", "LKI152", "KRØ200", "LKI230"), year = 2026L,
                       semester_name = c("Vår", "Høst", "Vår", "Vår", "Høst"))
  n <- "https://www.nih.no/studier/emner/2026/"
  gone <- paste0(n, c("var/thp111.html", "host/lki152.html", "var/lki152.html", "var/kro200.html"))
  out <- resolve_urls_english_tree_batch(df, "nih", index = list(pages = pages, read = character()), gone = gone)
  expect_equal(out$url, c(pages$url[1:2], paste0(n, "var/lki152.html"),   # no English page that semester
                          pages$url[3],   # the code without its Norwegian letter
                          paste0(n, "host/lki230.html")))
  df$institution <- "nih"
  df$course_code_raw <- paste0(df$course_code, "-1")
  expect_true(all(is.na(add_course_url(df)$url)))
  expect_equal(get_institution_config("nih")$strategy, "url_discovery")
})

test_that("NIH's English pages read as its Norwegian ones: no course leader, the headings map", {
  cfg <- get_institution_config("nih")
  dg <- function(dt, dd) paste0("<div class='dg'><dt>", dt, "</dt><dd>", dd, "</dd></div>")
  part <- function(h, x) paste0("<h2 class='vrtx-fs-infotype-title'>", h, "</h2><div>", x, "</div>")
  html <- paste0("<div id='vrtx-fs-emne-content'><h1>THP111 Training</h1>",
                 "<div id='vrtx-fs-emne-facts'><dl>", dg("ECTS Credits:", "15"),
                 dg("Course Leader:", "Kari Nordmann"), dg("Emneansvarlige:", "Ola Nordmann"), "</dl></div>",
                 part("Introduction", "The course is about training."),
                 part("Prerequisite", "THP110 Basic training"),
                 part("Assessment/exam", "Oral exam. Graded A-F."),
                 part("Core literature", "Smith (2020). Training. Oslo: Press."), "</div>")
  text <- page_fulltext(page_blocks(html, NA, .block_cfg(cfg)), cfg)
  expect_false(grepl("Nordmann|Course Leader|Emneansvarlige", text))
  expect_match(text, "ECTS Credits:", fixed = TRUE)
  sec <- extract_sections(cfg, html, NA_character_, "x")
  sec <- setNames(sec$raw_text, sec$section)
  expect_equal(unname(sec[c("course_content", "prerequisites", "assessment", "reading_list")]),
               c("The course is about training.", "THP110 Basic training", "Oral exam. Graded A-F.",
                 "Smith (2020). Training. Oslo: Press."))
})

test_that("NMBU page URLs: upper case, Norwegian letters transliterated", {
  expect_equal(as.character(add_course_url_nmbu("bio100")), "https://www.nmbu.no/emne/BIO100")
  expect_equal(as.character(add_course_url_nmbu("MILJØ300")), "https://www.nmbu.no/emne/MILJO300")
})
