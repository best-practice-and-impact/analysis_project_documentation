context("Cohort Profile Calculation with/without Profile Conversion")

test_that("calcCohortProfile returns a cohort profile using different
          combination methods", {
  values <- seq(1, 0, -0.1)
  config <- list(combinationMethod = "1", profileFunction= lin)
  result <- calcCohortProfile(values, config)
  target <- c(0.7980123, 0.5960245, 0.4440368, 0.3253824, 0.231728, 0.1580736,
              0.1010859, 0.0583838, 0.0281818, 0.0090909, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$combinationMethod = "2"
  result <- calcCohortProfile(values, config)
  target <- c(0.909091, 0.736364, 0.581818, 0.445455, 0.327273, 0.227273,
              0.145455, 0.081818, 0.036364, 0.009091, 0)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcCohortProfile returns a cohort profile for different profile
          functions", {
  values <- seq(1, 0, -0.1)
  config <- list(combinationMethod = "1", profileFunction= lin,
                 profileFunctionParam = 1)
  # test with different value for delay parameter
  result <- calcCohortProfile(values, config)
  target <- c(1, 0.707103, 0.514206, 0.37131, 0.261746, 0.177183, 0.112619,
              0.064722, 0.031111, 0.01, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$profileFunction <- const
  result <- calcCohortProfile(values, config)
  target <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$profileFunction <- geom
  config$profileFunctionParam <- 0.25
  result <- calcCohortProfile(values, config)
  target <- c(0.75, 0.50625, 0.3375, 0.221484, 0.142383, 0.088989, 0.053394,
              0.030034, 0.015017, 0.005631, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$profileFunction <- hyp
  config$profileFunctionParam <- 0.5
  result <- calcCohortProfile(values, config)
  target <- c(0.881913, 0.707103, 0.560023, 0.434206, 0.326355, 0.234643,
              0.158073, 0.09619, 0.048951, 0.016667, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$profileFunction <- db
  config$profileFunctionParam <- 0.25
  result <- calcCohortProfile(values, config)
  target <- c(0.949503, 0.825931, 0.714645, 0.611259, 0.51364, 0.420522,
              0.33107, 0.244688, 0.160933, 0.079462, 0)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcCohortProfile returns a cohort profile with profile conversion", {
  values <- seq(1, 0, -0.1)
  config <- list(combinationMethod = "1", discountRate=.05, inflationRate=.02,
                 profileFunction= lin)
  result <- calcCohortProfile(values, config, efficiencyToPriceProfile)
  target <- c(0.737389, 0.52604, 0.367459, 0.248598, 0.160891, 0.097953,
              0.054673, 0.026762, 0.0105, 0.002579, 0)
  expect_equal(result, target, tolerance=1e-6)

  result2 <- calcCohortProfile(values, config, priceToEfficiencyProfile)
  target <- c(0.974327, 0.732577, 0.569207, 0.446088, 0.347753, 0.266366,
              0.197386, 0.13794, 0.086084, 0.040442, 0)
  expect_equal(result2, target, tolerance=1e-6)
  expect_false(isTRUE(all.equal(result, result2)))

  config$combinationMethod <- "2"
  result3 <- calcCohortProfile(values, config, priceToEfficiencyProfile)
  target <- c(0.909091, 0.736364, 0.581818, 0.445455, 0.327273, 0.227273,
              0.145455, 0.081818, 0.036364, 0.009091, 0)
  expect_equal(result3, target, tolerance=1e-6)
  expect_false(isTRUE(all.equal(result2, result3)))
})


test_that("calcCohortProfile returns a cohort profile when survival rates are
          padded with/without zero's", {
  # start with survival rates that aren't right truncated
  values <- seq(1, 0.1, -0.1)
  config <- list(combinationMethod = "1", profileFunction= lin)
  result <- calcCohortProfile(values, config)
  target <- c(0.798012, 0.596025, 0.444037, 0.325382, 0.231728, 0.158074,
              0.101086, 0.058384, 0.028182, 0.009091)
  expect_equal(result, target, tolerance=1e-6)

  config$combinationMethod <- "2"
  result <- calcCohortProfile(values, config)
  target <- c(0.909091, 0.736364, 0.581818, 0.445455, 0.327273, 0.227273,
              0.145455, 0.081818, 0.036364, 0.009091)
  expect_equal(result, target, tolerance=1e-6)

  # now test for survival rates with multiple zero's at the end and 1's at the
  # beginning
  values <- c(1, 1, seq(1, 0.1, -0.1), 0, 0, 0, 0)
  config <- list(combinationMethod = "1", profileFunction= lin)
  result <- calcCohortProfile(values, config)
  target <- c(0.86532, 0.73064, 0.59596, 0.46128, 0.3516, 0.26192, 0.188906,
              0.130179, 0.083951, 0.048834, 0.023718, 0.007692, 0, 0, 0, 0)
  expect_equal(result, target, tolerance=1e-6)

  config$combinationMethod <- "2"
  result <- calcCohortProfile(values, config)
  target <- c(0.923077, 0.846154, 0.769231, 0.623077, 0.492308, 0.376923,
              0.276923, 0.192308, 0.123077, 0.069231, 0.030769, 0.007692, 0,
              0, 0, 0)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcCohortProfile returns a cohort profile using different survival rates",{
  values <- getSurvivalValues(2, 11, 7, 3/7, "pnorm", FALSE, 0, 11)
  config <- list(combinationMethod = "1", profileFunction= lin)
  result <- calcCohortProfile(values, config)
  target <- c(0.8729653, 0.7459305, 0.6188958, 0.491861, 0.3762263, 0.2747574,
              0.189713, 0.122214, 0.0718539, 0.0367285, 0.0138848)
  expect_equal(result, target, tolerance=1e-6)

  # an average life time that is beyond the maximum life time
  values <- getSurvivalValues(2, 11, 15, 3/7, "pnorm", FALSE, 0, 11)
  result <- calcCohortProfile(values, config)
  target <- c(0.9079424, 0.8158848, 0.7238272, 0.6317696, 0.5421132, 0.4550234,
              0.3707232, 0.2894672, 0.2115216, 0.1371456, 0.0665739)
  expect_equal(result, target, tolerance=1e-6)
})


