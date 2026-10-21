# Tests for HiOF's URL discovery: hiof_course_index() reads the folder
# listings (mocked), hiof_match_urls() picks an offering's page.

hiof_links <- function(...) {
  a <- paste0("<a href='", c(...), "'>x</a>", collapse = "")
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body>", a, "</body></html>")))
}

test_that("hiof_course_index() walks to the year folders and reads their listing pages", {
  local_robots("www.hiof.no")
  e <- "https://www.hiof.no/studier/emner/"
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(paste0("/", sub(e, "", req$url, fixed = TRUE)),
      "/" = hiof_links("lusp/", "historiske-emner/", "/om/", paste0(e, "?vrtx=feed")),
      "/lusp/" = hiof_links(paste0(e, "lusp/lusp/"), paste0(e, "lusp/")),
      "/lusp/lusp/" = hiof_links("2021/", "2022/"),
      "/lusp/lusp/2022/" = hiof_links("host/lmat10224.html", "var/l%C3%B8k101.html", "?page=2"),
      "/lusp/lusp/2022/?page=2" = hiof_links("host/abc.html"),
      "/lusp/lusp/2022/?page=3" = hiof_links("host/abc.html", "host/lmat10224.html"),
      "/historiske-emner/" = hiof_links("lu/"),
      "/historiske-emner/lu/" = hiof_links("2022/", "2007/"),
      httr2::response(404))
  })
  idx <- hiof_course_index(2022, delay = 0)
  expect_equal(idx$read, paste0(e, "lusp/lusp/2022/"))   # lu/2022/ failed
  expect_equal(idx$pages$code, c("lmat10224", "løk101", "abc"))
  expect_equal(idx$pages$semester, c("host", "var", "host"))
  expect_equal(unique(idx$pages$year), 2022L)
  expect_false(any(grepl("/2021/|/2007/", sent)))
})

test_that("hiof_match_urls() takes the offering's page, the teacher-ed URL first", {
  e <- "https://www.hiof.no/studier/emner/"
  pages <- tibble::tribble(
    ~url, ~code, ~year, ~semester,
    paste0(e, "historiske-emner/ir/2013/host/hsb1209.html"), "hsb1209", 2013L, "host",
    paste0(e, "historiske-emner/lu/2022/host/lmat10224.html"), "lmat10224", 2022L, "host",
    paste0(e, "lusp/lusp/2022/host/lmat10224.html"), "lmat10224", 2022L, "host",
    paste0(e, "historiske-emner/oss/2022/var/sff42015.html"), "sff42015", 2022L, "var",
    paste0(e, "lusp/slik/2022/var/sff42015.html"), "sff42015", 2022L, "var")
  read <- paste0(e, c("historiske-emner/ir/2013/", "historiske-emner/lu/2013/",
                      "historiske-emner/lu/2022/", "lusp/lusp/2022/"))
  df <- tibble::tibble(course_code = c("HSB1209", "HSB1209", "LMAT10224", "SFF42015", "IRE30507", "LA10VK1"),
                       year = c(2013L, 2013L, 2022L, 2022L, 2013L, 2007L),
                       semester_name = c("Høst", "Vår", "Høst", "Vår", "Høst", "Høst"))
  out <- hiof_match_urls(df, list(pages = pages, read = read))
  expect_equal(out$url, c(
    paste0(e, "historiske-emner/ir/2013/host/hsb1209.html"),   # another faculty's folder
    NA,                                                         # no page that semester
    paste0(e, "lusp/lusp/2022/host/lmat10224.html"),            # add_course_url_hiof()'s
    paste0(e, "lusp/slik/2022/var/sff42015.html"),              # not the historical copy
    NA,
    # the listing of lu/2007/ was not read: the teacher-ed URL as before
    paste0(e, "historiske-emner/lu/2007/host/la10vk1.html")))
})

test_that("add_course_url() leaves HiOF's URL to the resolver", {
  df <- tibble::tibble(institution = "hiof", course_code = "LMAT10224", course_code_raw = "LMAT10224-1",
                       year = 2025L, semester_name = "Vår")
  expect_true(is.na(add_course_url(df)$url))
  expect_equal(get_institution_config("hiof")$strategy, "url_discovery")
})
