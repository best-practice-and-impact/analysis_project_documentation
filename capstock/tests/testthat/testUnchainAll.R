context("Unchain All")
suppressWarnings(library(dplyr))
suppressWarnings(library(tibble))
suppressWarnings(library(tidyr))

# Set up some shared test data
set.seed(100)
testData <- tibble(Year = rep(1:5, 2),
                   SeriesId = rep(c("S1", "S2"), each = 5),
                   NetStockCP = runif(10), NetStockCVM = runif(10),
                   OtherStockCP = runif(10), OtherStockCVM = runif(10),
                   Deflator = 10) %>%
  group_by(SeriesId) %>% nest(.key = "data")
testDataPairs <- tribble(~CP, ~CVM,
                         "NetStockCP", "NetStockCVM",
                         "OtherStockCP", "OtherStockCVM")

test_that("unchainAll produces correct result", {

  result <- unchainAll(testData, pairs = testDataPairs, parallelise = FALSE)

  # Check outer data.frame
  expect_equal(c(2, 3), dim(result))
  expect_true(length(setdiff(c("SeriesId", "unchained", "data"), colnames(result))) == 0)

  # Check resultant "unchained" list-column
  resultUC <- result[[1, "unchained"]]
  expect_equal(c(5, 4), dim(resultUC))
  # Should have new columnn names denoting CYP and PYP
  expect_true(length(setdiff(c("NetStockCYP", "NetStockPYP", "OtherStockCYP", "OtherStockPYP"),
                             colnames(resultUC))) == 0)

  # Check the first PYP column to match the reference values
  expect_equal(resultUC$NetStockPYP, c(NA, NA, NA, NA, 0.3755019),
               tolerance = 1e-6)
})

test_that("unchainAll produces correct result using explicit deflator", {

  # Use the Deflator defined within testData (all values are 10)
  result <- unchainAll(testData, pairs = testDataPairs,
                       deflatorColumn = "Deflator", deflatorMethod = "explicit",
                       parallelise = FALSE)

  # Check outer data.frame
  expect_equal(c(2, 3), dim(result))
  expect_true(length(setdiff(c("SeriesId", "unchained", "data"), colnames(result))) == 0)

  # Check resultant "unchained" list-column
  resultUC <- result[[1, "unchained"]]
  expect_equal(c(5, 4), dim(resultUC))
  # Should have new columnn names denoting CYP and PYP
  expect_true(length(setdiff(c("NetStockCYP", "NetStockPYP", "OtherStockCYP", "OtherStockPYP"),
                             colnames(resultUC))) == 0)

  # Check the first column to match the reference values
  expect_equal(resultUC$NetStockPYP, c(NA, NA, NA, NA, 0.4685493),
               tolerance = 1e-6)
})

test_that("unchainAll works correctly with specified pairs not ending with CP/CVM", {

  # Remove trailing CVM from the testData
  set.seed(100)
  testData2 <- tibble(Year = rep(1:5, 2),
                     SeriesId = rep(c("S1", "S2"), each = 5),
                     NetStockCP = runif(10), NetStock = runif(10),
                     OtherStockCP = runif(10), OtherStock = runif(10)) %>%
    group_by(SeriesId) %>% nest(.key = "data")
  testDataPairs2 <- tribble(~CP, ~CVM,
                           "NetStockCP", "NetStock",
                           "OtherStockCP", "OtherStock")

  result <- unchainAll(testData2, pairs = testDataPairs2, parallelise = FALSE)

  # Check outer data.frame
  expect_equal(c(2, 3), dim(result))
  expect_true(length(setdiff(c("SeriesId", "unchained", "data"), colnames(result))) == 0)

  # Check resultant "unchained" list-column
  resultUC <- result[[1, "unchained"]]
  expect_equal(c(5, 4), dim(resultUC))
  # Should have new columnn names denoting CYP and PYP
  expect_true(length(setdiff(c("NetStockCYP", "NetStockPYP", "OtherStockCYP", "OtherStockPYP"),
                             colnames(resultUC))) == 0)

  # Check the first column to match the reference values
  expect_equal(resultUC$NetStockCYP, c(0.307766, 0.257673, 0.552322, 0.056383, 0.468549),
               tolerance = 1e-6)
})

test_that("unchainAll rejects invalid data", {

  # Define pairs that do not exist
  testDataPairsBad <- tribble(~CP, ~CVM,
                            "XXX", "YYY",
                            "OtherStockCP", "OtherStock")
  expect_error(unchainAll(testData, pairs = testDataPairsBad, parallelise = FALSE))

  # Define data without a Year column
  set.seed(100)
  testDataBad <- tibble(TimeCounter = rep(1:5, 2),
                      SeriesId = rep(c("S1", "S2"), each = 5),
                      NetStockCP = runif(10), NetStockCVM = runif(10),
                      OtherStockCP = runif(10), OtherStockCVM = runif(10)) %>%
    group_by(SeriesId) %>% nest(.key = "data")
  expect_error(unchainAll(testDataBad, pairs = testDataPairs, parallelise = FALSE))

  # Use "explicit" deflator Method without defining a deflator
  expect_error(unchainAll(testData, pairs = testDataPairs,
                          deflatorMethod = "explicit", parallelise = FALSE))

  # Use "explicit" deflator Method with a deflator that does not exist
  expect_error(unchainAll(testData, pairs = testDataPairs, deflatorColumn = "BAD",
                          deflatorMethod = "explicit", parallelise = FALSE))

})


test_that("unchainRename renames output columns", {
  # Create flat dataset with only Year, NetStockCP, and NetStockCVM
  series <- testData[[1, "data"]]
  series <- dplyr::select(series, Year, NetStockCP, NetStockCVM)
  # Expect resultant columns to be "NetStockCYP" and "NetStockPYP" instead of
  # the usual "CYP", "PYP" from unchain().
  result <- unchainRename(series, cpColumn = "NetStockCP", cvmColumn = "NetStockCVM",
                          deflatorColumn = NULL, deflatorMethod = "implicit")
  expect_equal(ncol(result), 2)
  expectedCols <- c("NetStockCYP", "NetStockPYP")
  expect_equal(length(setdiff(colnames(result), expectedCols)), 0)
})

test_that("unchainRename renames output columns with explicit deflator", {
  # Create flat dataset with only Year, NetStockCP, and NetStockCVM
  series <- testData[[1, "data"]]
  series <- dplyr::select(series, Year, NetStockCP, NetStockCVM)
  series <- dplyr::mutate(series, Deflator = 10)
  # Expect resultant columns to be "NetStockCYP" and "NetStockPYP" instead of
  # the usual "CYP", "PYP" from unchain().
  result <- unchainRename(series, cpColumn = "NetStockCP", cvmColumn = "NetStockCVM",
                          deflatorColumn = "Deflator", deflatorMethod = "explicit")
  expect_equal(ncol(result), 2)
  expectedCols <- c("NetStockCYP", "NetStockPYP")
  expect_equal(length(setdiff(colnames(result), expectedCols)), 0)
})
