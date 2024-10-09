context("Chain All")
suppressWarnings(library(dplyr))
suppressWarnings(library(tibble))
suppressWarnings(library(tidyr))

test_that("chainAll can apply chaining to nested dataframe.", {

  # Create test data for two series with two pairs of stocks
  testData <- tribble(
    ~Year, ~NetStockCYP, ~NetStockPYP, ~Quarter, ~GrossStockCYP, ~GrossStockPYP, ~SeriesId,
    1998,1.1,1,2,1.1,1,"S1",
    1998,1.1,1,2,1.1,1,"S1",
    1998,1.1,1,3,1.1,1,"S1",
    1998,1.1,1,4,1.1,1,"S1",
    1999,1.1,1,1,1.1,1,"S1",
    1999,1.1,1,2,1.1,1,"S1",
    1999,1.1,1,3,1.1,1,"S1",
    1999,1.1,1,4,1.1,1,"S1",
    2000,1.1,1,1,1.1,1,"S1",
    2000,1.1,1,2,1.1,1,"S1",
    2000,1.1,1,3,1.1,1,"S1",
    2000,1.1,1,4,1.1,1,"S1",
    1998,1.1,1,2,1.1,1,"S2",
    1998,1.1,1,2,1.1,1,"S2",
    1998,1.1,1,3,1.1,1,"S2",
    1998,1.1,1,4,1.1,1,"S2",
    1999,1.1,1,1,1.1,1,"S2",
    1999,1.1,1,2,1.1,1,"S2",
    1999,1.1,1,3,1.1,1,"S2",
    1999,1.1,1,4,1.1,1,"S2",
    2000,1.1,1,1,1.1,1,"S2",
    2000,1.1,1,2,1.1,1,"S2",
    2000,1.1,1,3,1.1,1,"S2",
    2000,1.1,1,4,1.1,1,"S2")

  # Nest by SeriesId
  testDataNest <- testData %>%
    dplyr::group_by(SeriesId) %>%
    tidyr::nest()

  # Specify two stock pairs
  testDataPairs <- tribble(~CYP, ~PYP, ~ChainType,
                           "GrossStockCYP", "GrossStockPYP", "Stock",
                           "NetStockCYP", "NetStockPYP", "Flow")  # just using "Flow" to test other option

  result <- chainAll(testDataNest, pairs = testDataPairs,
                     lastCompleteYear = 2000, parallelise = FALSE)

  expect_equal(dim(result), c(2, 3))
  # Test the results from chain for the first series
  result1 <- dplyr::select(result, chained) %>% dplyr::slice(1) %>% tidyr::unnest()
  expect_equal(dim(result1), c(12, 2))
  expect_true(length(setdiff(c("GrossStockCVM", "NetStockCVM"), colnames(result1))) == 0)

})
