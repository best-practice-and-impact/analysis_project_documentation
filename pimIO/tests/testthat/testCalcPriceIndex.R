context("Calculate Price Index")

test_that("calcPriceIndex returns an error for incorrect input", {
  expect_error(calcPriceIndex("A"))
  expect_error(calcPriceIndex(1, "A"))
  expect_error(calcPriceIndex(1, 1:2))
})

test_that("calcPriceIndex returns the price index", {
  result <- calcPriceIndex(1, 2)
  expect_equal(result, 0.5)

  result <- calcPriceIndex(c(1, 2, 3), c(2, 4, 6))
  expect_equal(result, rep(0.5, 3))
})

test_that("calcPriceIndex returned index of 1 for zero-valued CP input", {
  result <- calcPriceIndex(cpData = c(1, 0, 1),
                           cvmData = c(2, 0, 2))
  expect_equal(result, c(0.5, 1, 0.5))
})
