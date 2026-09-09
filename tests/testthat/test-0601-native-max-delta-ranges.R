library(testthat)

test_that("native max_delta_ranges calculates clipped ordered ranges", {
  expect_equal(
    facereaderconverter:::max_delta_ranges(
      c(NA, 0.1, 0.5, 0.2, 0.8, NA),
      start_row = c(3L, 5L, 6L),
      end_row = c(4L, 5L, 6L),
      group_start_row = c(1L, 1L, 1L),
      k = 2L
    ),
    c(0.4, 0.6, 0.6)
  )
})

test_that("native max_delta_ranges restarts at group boundaries", {
  expect_equal(
    facereaderconverter:::max_delta_ranges(
      c(0.1, 0.5, 0.2, 0.8),
      start_row = c(2L, 4L),
      end_row = c(2L, 4L),
      group_start_row = c(1L, 3L),
      k = 1L
    ),
    c(0.4, 0.6)
  )
})

test_that("native max_delta_ranges validates range vectors", {
  expect_error(
    facereaderconverter:::max_delta_ranges(
      c(0.1, 0.2),
      start_row = 1L,
      end_row = c(1L, 2L),
      group_start_row = 1L,
      k = 0L
    ),
    "Range position vectors must have the same length."
  )
})
