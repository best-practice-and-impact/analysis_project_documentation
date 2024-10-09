context("Profile Function Options")

test_that("const returns a constant value over the lifetime of an asset",{
  expect_equal(const(1, 3), 1)
  expect_equal(const(2, 3), 1)
  expect_equal(const(2, 2), 0)
})

test_that("lin returns a linearly declining value of the lifetime of an asset",{
  expect_equal(lin(1, 4), 0.75)
  expect_equal(lin(2, 4), 0.5)
  expect_equal(lin(3, 4), 0.25)
  expect_equal(lin(4, 4), 0)

  # with delay parameter
  expect_equal(lin(2, 3, 1), 0.5)
})

test_that("geom returns a geometric pattern of depreciation",{
  expect_equal(geom(2, NULL, 0.5), 0.25)
  expect_equal(geom(3, NULL, 0.5), 0.125)
})

test_that("db returns a declining balance pattern of depreciation",{
  expect_equal(db(2, 4, 1), 0.5625)
  expect_equal(db(3, 4, 1), 0.421875)
})

test_that("hyp returns a hyperbolic pattern of depreciation", {
  expect_error(hyp(1,2,3))
  expect_equal(hyp(1, 3, 0.5), 0.8)
  expect_equal(hyp(2, 3, 0.5), 0.5)
})

