context("Aggregate All")
suppressWarnings(library(dplyr))
suppressWarnings(library(tibble))

# Create some shared test data:
# Sector hierarchy with three levels
secHier <- tribble(~S1, ~S2, ~S3,
                   "TOTAL", "SA", "SA1",
                   "TOTAL", "SB", "SB1",
                   "TOTAL", "SB", "SB2")
# Industry hierarchy with two levels
indHier <- tribble(~I1, ~I2,
                   "TOTAL", "IA",
                   "TOTAL", "IB")
# Asset hierarchy with two levels
assHier <- tribble(~A1, ~A2,
                   "TOTAL", "ASSET1",
                   "TOTAL", "ASSET2")

# Dataset at the lowest level of all hierarchies
testData1 <- expand.grid(Sector = secHier$S3,
            Industry = indHier$I2,
            Asset = assHier$A2,
            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
testData1$Stock1 <- c(7.7, 0.6, 4.6, 8.8, 6.4, 9.2, 3.8, 4.5, 5.4, 5.4, 5.2, 9.2)
testData1$Stock2 <- c(9.5, 1.2, 5, 4.1, 8, 4.2, 5.9, 0.3, 2.5, 6.6, 2.9, 7.5)
testData1$Period <- "1997Q1"

test_that("aggregateAll correctly performs multiple aggregations of one variable.", {

  result <- aggregateAll(testData1, secHier, indHier, assHier, values = "Stock1")

  # The "Group" column denotes all unique combinations of groups. There should
  # be 12 unique groupings in the data (3 * 2 * 2 levels) and all the groupings
  # should total to the same value (70.8)
  groupTotals <- result %>%
    group_by(Group) %>%
    summarise(total = sum(Stock1))
  expect_equal(nrow(groupTotals), 12)
  expect_equal(groupTotals$total, rep(70.8, 12))
})

test_that("aggregateAll correctly performs multiple aggregations of two variables.", {

  result <- aggregateAll(testData1, secHier, indHier, assHier,
                         values = c("Stock1", "Stock2"))

  # The "Group" column denotes all unique combinations of groups. There should
  # be 12 unique groupings in the data (3 * 2 * 2 levels) and all the groupings
  # should total to the same value
  groupTotals <- result %>%
    group_by(Group) %>%
    summarise(total1 = sum(Stock1),
              total2 = sum(Stock2))
  expect_equal(nrow(groupTotals), 12)
  expect_equal(groupTotals$total1, rep(70.8, 12))
  expect_equal(groupTotals$total2, rep(57.7, 12))
})

test_that("aggregateAll throws error if values are not present in data.", {
  expect_error(aggregateAll(testData1, secHier, indHier, assHier,
                            values = "UndefinedStock"),
               "UndefinedStock not present in data")
})

test_that("aggregateAll throws error if data does not have required columns.", {
  badData <- dplyr::select(testData1, -Period)
  expect_error(aggregateAll(badData, secHier, indHier, assHier,
                            values = "Stock1"),
               "Period not present in data")
})

test_that("aggregateAll throws error if all data does not match the lowest hierarchies.", {

  # Create data with an asset missing from the provided asset hierarchy
  badData <- dplyr::mutate(testData1, Asset = recode(Asset, ASSET1 = "NEWASSET"))
  expect_error(aggregateAll(badData, secHier, indHier, assHier,
               values = "Stock1"),
               "Missing Assets:  NEWASSET")
})

