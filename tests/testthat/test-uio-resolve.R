test_that("UiO: a listed course gets its folder, an unlisted one its prefix's or department's", {
  folders <- tibble::tibble(
    code = c("ENG2165", "ENG4165", "SOS9009", "SOS1001", "LIT1000", "MONA2504"),
    folder = c("hf/ilos", "hf/ilos", "sv/iss", "sv/iss", "hf/ilos", "hf/ikos"),
    name = c("ENG2165", "ENG4165", "sos9009", "SOS1001", "LIT1000", "MONA2504")
  )
  df <- tibble::tibble(
    course_code = c("ENG2165", "sos9009", "ENG4429", "LITX300", "MØNA2504", "XYZ100"),
    department_code = c("1", "2", "2", "1", "3", "9")   # ENG4429: the prefix wins
  )
  url <- resolve_urls_uio_batch(df, folders)$url
  base <- "https://www.uio.no/studier/emner/"

  expect_equal(url[1:5], paste0(base, c("hf/ilos/ENG2165/", "sv/iss/sos9009/", "hf/ilos/ENG4429/",
                                        "hf/ilos/LITX300/", "hf/ikos/MONA2504/")))
  # no listed course shares its prefix or department
  expect_true(is.na(url[6]))
})
