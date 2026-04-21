test_that("locf", {
  coding_df = read.csv("junk/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "teen")

  x = convert_to_episodes(coding_df)
  expect_no_error({
    locf(x)
  })
  expect_no_error({
    locf(x$coding)
  })
})
