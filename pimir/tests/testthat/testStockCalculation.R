context("Calculation of Gross/Net/Productive Stock")

test_that("convertProfileToMatrix converts a data.frame of vectors to an upper
          triangular matrix",{
  df <- purrr::by_row(data.frame(vintageId=1:4), ~1:4, .to="values")
  result <- convertProfileToMatrix(df)
  target <- structure(c(1, 0, 0, 0, 2, 1, 0, 0, 3, 2, 1, 0, 4, 3, 2, 1),
                      .Dim = c(4L, 4L))
  expect_equal(result, target, tolerance=1e-6)

  # case when there are more vintages than profile values
  df <- purrr::by_row(data.frame(vintageId=1:6), ~1:4, .to="values")
  result <- convertProfileToMatrix(df)
  target <- structure(c(1, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 3, 2, 1, 0, 0,
              0, 4, 3, 2, 1, 0, 0, 0, 4, 3, 2, 1, 0, 0, 0, 4, 3, 2, 1),
            .Dim = c(6L, 6L))
  expect_equal(result, target, tolerance=1e-6)

  # case when there are less vintages than profile values
  df <- purrr::by_row(data.frame(vintageId=1:4), ~1:6, .to="values")
  result <- convertProfileToMatrix(df)
  target <- structure(c(1, 0, 0, 0, 2, 1, 0, 0, 3, 2, 1, 0, 4, 3, 2, 1),
                      .Dim = c(4L, 4L))
  expect_equal(result, target, tolerance=1e-6)
})

test_that("pim applies the perpetual inventory method to a vector of values", {
  gfcf <- c(10, 20, 30, 40)
  df <- purrr::by_row(data.frame(vintageId=1:4), ~1:4, .to="values")
  result <- pim(gfcf, df)
  expect_equal(result, c(10, 40, 100, 200))
})

test_that("pim calculates gross stock correctly when retirement is none", {
  # When the PIM is applied without a retirement distribution, the gross stock
  # should equal the cumulative sum of the GFCF
  gfcf <- c(325.6, 214.7, 259.3, 159.5, 177.4, 390.7, 380.4, 546.8, 506.1)
  ll <- data.frame(Min = rep(2, length(gfcf)),
                   Max = 15, Average = 7, CoV = 3/7)
  # Set retirement distribution to "none"
  config <- pimConfig(retirementDistName = "none", inflationRate = 0.1)
  # Calculate survival values
  sv <- pimir:::calcSurvivalValues(ll, config)
  result <- pimir:::pim(gfcf, sv)
  # Result should be the cumulative sum of GFCF
  target <- cumsum(gfcf)
  expect_equal(result, target)
})

test_that("calcNetStock calculates the net stock", {
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")
  config <- list(combinationMethod = "1", profileFunctionName = "lin",
                 profileType = "age-price", profileFunction = lin)
  result <- calcNetStock(gfcf, df, config)
  target <- c(6.7916667, 17.1666667, 29.1666667, 41.6666667, 54.1666667)

  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcNetStock calculates the net stock with/without profile conversion", {
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")
  config <- list(combinationMethod = "1", profileFunctionName = "lin",
                 profileType = "age-efficiency", discountRate = 0.05,
                 inflationRate = 0.01, profileFunction = lin)
  result <- calcNetStock(gfcf, df, config)
  target <- c(5.610262, 13.802633, 22.964471, 32.356482, 41.748494)

  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcProductiveStock calculates the productive stock", {
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")
  config <- list(combinationMethod = "1", profileFunctionName = "lin",
                 profileType = "age-efficiency", profileFunction = lin)
  result <- calcProductiveStock(gfcf, df, config)
  target <- c(6.7916667, 17.1666667, 29.1666667, 41.6666667, 54.1666667)

  expect_equal(result, target, tolerance=1e-6)
})

test_that("calcProductiveStock calculates the productive stock with/without profile
          conversion", {
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")
  config <- list(combinationMethod = "1", profileFunctionName = "lin",
                 profileType = "age-price", discountRate = 0.05,
                 inflationRate = 0.01, profileFunction = lin)
  result <- calcProductiveStock(gfcf, df, config)
  target <- c(9.647476, 25.121895, 43.896074, 64.112396, 84.328717)

  expect_equal(result, target, tolerance=1e-6)
})

test_that("geometric profile doesn't lead to profile conversion", {
  # Resultant net and productive stocks should be identical using either
  # age-efficiency are age-price profiles as "geom" should not convert the profile
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")

  # productive stock
  config <- list(combinationMethod = "1", profileFunctionName = "geom",
                 profileType = "age-efficiency",
                 profileFunction = geom,
                 profileFunctionParam = 0.01,
                 inflationRate = 0.9)
  resultOne <- calcProductiveStock(gfcf, df, config)
  config$profileType <- "age-price"
  resultTwo <- calcProductiveStock(gfcf, df, config)

  target <- c(9.9, 27.15075, 49.252995, 73.75673, 98.260465)
  expect_equal(resultOne, resultTwo, tolerance=1e-6)
  expect_equal(resultOne, target, tolerance=1e-6)

  # net stock
  config <- list(combinationMethod = "1", profileFunctionName = "geom",
                 profileType = "age-efficiency",
                 profileFunction = geom,
                 profileFunctionParam = 0.01,
                 inflationRate = 0.9)
  resultOne <- calcNetStock(gfcf, df, config)
  config$profileType <- "age-price"
  resultTwo <- calcNetStock(gfcf, df, config)

  target <- c(9.9, 27.15075, 49.252995, 73.75673, 98.260465)
  expect_equal(resultOne, resultTwo, tolerance=1e-6)
  expect_equal(resultOne, target, tolerance=1e-6)
})

test_that("db profile doesn't lead to profile conversion", {
  # Resultant net and productive stocks should be identical using either
  # age-efficiency are age-price profiles as "db" should not convert the profile
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")

  # productive stock
  config <- list(combinationMethod = "1", profileFunctionName = "db",
                 profileType = "age-efficiency",
                 profileFunction = db,
                 profileFunctionParam = 0.01,
                 inflationRate = 0.9)
  resultOne <- calcProductiveStock(gfcf, df, config)
  config$profileType <- "age-price"
  resultTwo <- calcProductiveStock(gfcf, df, config)
  expect_equal(resultOne, resultTwo, tolerance=1e-6)

  # net stock
  config <- list(combinationMethod = "1", profileFunctionName = "db",
                 profileType = "age-efficiency",
                 profileFunction = db,
                 profileFunctionParam = 0.01,
                 inflationRate = 0.9)
  resultOne <- calcNetStock(gfcf, df, config)
  config$profileType <- "age-price"
  resultTwo <- calcNetStock(gfcf, df, config)
  expect_equal(resultOne, resultTwo, tolerance=1e-6)
})

test_that("calcNetStock calculates the net stock with profile conversion and
          time series discount rates",{
  gfcf <- c(10, 20, 30, 40, 50)
  df <- purrr::by_row(data.frame(vintageId=1:5, id=1:5), ~seq(1,0,-0.25),
                      .to="values")
  config <- list(combinationMethod = "1", profileFunctionName = "lin",
                 profileType = "age-efficiency", discountRates = 0.05,
                 inflationRate = 0.01, profileFunction = lin)

  # fails because discount rates is not equal length to gfcf
  expect_error(calcNetStock(gfcf, df, config))

  config$discountRates <- rep(0.05,length(gfcf))
  result <- calcNetStock(gfcf, df, config)
  target <- c(5.610262, 13.802633, 22.964471, 32.356482, 41.748494)
  expect_equal(result, target, tolerance=1e-6)

  config$discountRates <- c(0.05, 0.02, 0.04, 0.08, 0.1)
  result <- calcNetStock(gfcf, df, config)
  target <- c(5.6102616, 13.7214414, 22.8577638, 32.4390388, 42.1565157)
  expect_equal(result, target, tolerance=1e-6)
})

