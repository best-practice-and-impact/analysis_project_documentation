context("Configuring the calculation")

test_that("pimConfig returns default values when no parameters are given", {
  config <- pimConfig()

  expect_equal(config$profileFunction, lin)
  expect_equal(config$profileType, "age-efficiency")
  expect_equal(config$retirementDistName, "pnorm")
  expect_equal(config$rightTruncate, FALSE)
  expect_equal(config$combinationMethod, "1")
  expect_null(config$discountRate)
  expect_null(config$inflationRate)
  expect_equal(config$offSet, 0)
  expect_equal(config$profileFunctionParam, 0)
  expect_null(config$discountRates)
  expect_equal(length(config), 10)

})

test_that("pimConfig can set configuration parameters", {
  profileType <- "age-efficiency"
  retDist <- "pnorm"
  rightTruncate <- TRUE
  combinationMethod <- "1"
  profileFunctionName <- "lin"
  discountRate <- 0
  inflationRate <- 0
  profileFunctionParam <- 0
  offSet <- 0
  discountRates <- c(.1, .2, .3)
  config <- pimConfig(profileType = profileType,
                      profileFunctionName = profileFunctionName,
                      retirementDistName = retDist,
                      rightTruncate = rightTruncate,
                      combinationMethod = combinationMethod,
                      discountRate = discountRate, inflationRate = inflationRate,
                      offSet = offSet,
                      profileFunctionParam = profileFunctionParam,
                      discountRates = discountRates)

  expect_equal(config$profileFunction, lin)
  expect_equal(config$profileType, profileType)
  expect_equal(config$retirementDistName, retDist)
  expect_equal(config$rightTruncate, rightTruncate)
  expect_equal(config$combinationMethod, combinationMethod)
  expect_equal(config$discountRate, discountRate)
  expect_equal(config$inflationRate, inflationRate)
  expect_equal(config$offSet, offSet)
  expect_equal(config$profileFunctionParam, profileFunctionParam)
  expect_equal(config$discountRates, discountRates)
  expect_equal(length(config), 10)
})

test_that("pimConfig returns and error for incorrect input", {
  expect_error(pimConfig(profileType = "A"))
  expect_error(pimConfig(profileFunctionName = "A"))
  expect_error(pimConfig(retirementDistName = "A"))
  expect_error(pimConfig(rightTruncate = "Yes"))
  expect_error(pimConfig(combinationMethod = "3"))
  expect_error(pimConfig(discountRate = "1"))
  expect_error(pimConfig(discountRate = 2))
  expect_error(pimConfig(inflationRate = "1"))
  expect_error(pimConfig(inflationRate = 2))
  expect_error(pimConfig(offSet = "1"))
  expect_error(pimConfig(offSet = 2))
  expect_error(pimConfig(profileFunctionParam = "1"))
  expect_error(pimConfig(discountRates = "A"))
  expect_error(pimConfig(discountRates = c(0, -1)))
})
