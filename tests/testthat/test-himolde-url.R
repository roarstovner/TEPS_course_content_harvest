# Tests for Høgskolen i Molde's URL discovery: its folder listings (mocked)
# through vortex_course_index(), as HiOF's (test-hiof-url.R).

himolde_links <- function(...) {
  a <- paste0("<a href='", c(...), "'>x</a>", collapse = "")
  httr2::response(200, headers = list(`Content-Type` = "text/html; charset=utf-8"),
                  body = charToRaw(paste0("<html><body>", a, "</body></html>")))
}

test_that("HiMolde's pages come from its department folders in both trees", {
  local_robots("www.himolde.no")
  old <- institution_configs$himolde$request_delay
  institution_configs$himolde$request_delay <<- 0
  withr::defer(institution_configs$himolde$request_delay <<- old)
  sent <- character()
  httr2::local_mocked_responses(function(req) {
    sent <<- c(sent, req$url)
    switch(paste0("/", sub("https://www.himolde.no/", "", req$url, fixed = TRUE)),
      "/studier/emner/" = himolde_links("os/", "log/", "?filter.language=english"),
      "/studier/emner/os/" = himolde_links("2021/", "2022/", "2023/"),
      "/studier/emner/os/2023/" = himolde_links("var/adm130.html", "host/adm100.html", "host/bok705.html", "?page=2"),
      "/studier/emner/os/2023/?page=2" = himolde_links("host/adm100.html"),
      "/studier/emner/log/" = himolde_links("2022/", "2023/"),
      "/studier/emner/log/2022/" = himolde_links("host/log954.html"),
      "/studier/emner/log/2022/?page=2" = himolde_links(),
      # a year folder that lists only its semester folders
      "/studier/emner/log/2023/" = himolde_links("host/", "var/", "?vrtx=feed"),
      "/studier/emner/log/2023/host/" = himolde_links("sn15-1.html", "?page=2"),
      "/studier/emner/log/2023/host/?page=2" = himolde_links("sn15-1.html"),
      "/studier/emner/log/2023/var/" = himolde_links(),
      "/english/studies/courses/" = himolde_links("os/"),
      "/english/studies/courses/os/" = himolde_links("2023/"),
      "/english/studies/courses/os/2023/" = himolde_links("autumn/adm120.html", "autumn/adm100.html"),
      "/english/studies/courses/os/2023/?page=2" = himolde_links(),
      httr2::response(404))
  })
  df <- tibble::tibble(course_code = c("ADM130", "ADM100", "ADM120", "LOG954", "SN15-1", "BØK705", "XYZ999"),
                       year = 2023L, semester_name = c("Vår", "Høst", "Høst", "Vår", "Høst", "Høst", "Høst"))
  out <- resolve_urls_himolde_batch(df)
  e <- "https://www.himolde.no/"
  expect_equal(out$url, c(
    paste0(e, "studier/emner/os/2023/var/adm130.html"),
    paste0(e, "studier/emner/os/2023/host/adm100.html"),     # the Norwegian page first
    paste0(e, "english/studies/courses/os/2023/autumn/adm120.html"),   # English only
    paste0(e, "studier/emner/log/2022/host/log954.html"),    # over two semesters: the autumn before
    paste0(e, "studier/emner/log/2023/host/sn15-1.html"),
    paste0(e, "studier/emner/os/2023/host/bok705.html"),     # without the Norwegian letter
    NA))
  # the year before is read for the spring offerings only, other years not
  expect_true(paste0(e, "studier/emner/log/2022/") %in% sent)
  expect_false(any(grepl("/2021/", sent)))
})

test_that("add_course_url() leaves HiMolde's URL to the resolver", {
  df <- tibble::tibble(institution = "himolde", course_code = "ADM130", course_code_raw = "ADM130-1",
                       year = 2024L, semester_name = "Vår")
  expect_true(is.na(add_course_url(df)$url))
  expect_equal(get_institution_config("himolde")$strategy, "url_discovery")
  expect_equal(institution_from_code("0232"), "himolde")
})
