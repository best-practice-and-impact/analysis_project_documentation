context("Retirement distribution parameter transformations")

test_that("transformNormalParameters returns parameters for a Normal distribution",{
  result <- transformNormalParameters(10, 10)
  expect_equal(result, structure(c(10, 100), .Names = c("mean", "sd")))
})

test_that("transformGammaParameters returns parameters for a Gamma distribution",{
  result <- transformGammaParameters(10, 10)
  expect_equal(result, structure(c(0.01, 0.001), .Names = c("shape", "rate")))
})


test_that("transformLogNormalParameters returns parameters for a Log Normal distribution",{
  result <- transformLogNormalParameters(10, 10)
  expect_equal(result,
                structure(c(-0.004975, 2.148283),
                          .Names = c("meanlog", "sdlog")),
                tolerance=1e-6)

  result <- transformLogNormalParameters(10, 0)
  expect_equal(result,
               structure(c(2.302585, 0.0),
                         .Names = c("meanlog", "sdlog")),
               tolerance=1e-6)
})

test_that("transformWeibullParametersInverse returns mean and coefficient of
          variance for a Weibull distribution",{
  result <- transformWeibullParametersInverse(0.233206, 0.26759)
  expect_equal(result, structure(c(10, 10), .Names = c("distMean", "distCOV")),
                tolerance=1e-5)
})

test_that("transformWeibullParameters returns parameters for a Weibull distribution",{
  result <- transformWeibullParameters(10, 10)
  expect_equal(result, structure(c(0.2332067, 0.2675959),
                                  .Names = c("shape", "scale")),
                tolerance=1e-6)
})

test_that("transformNoneParameters returns punif parameters to create all ones",{
  result <- transformNoneParameters()
  expect_equal(result, structure(c(1, 1), .Names = c("min", "max")))
})
