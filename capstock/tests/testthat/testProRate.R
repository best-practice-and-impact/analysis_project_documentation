context("Prorate")

test_that("prorateDown splits the aggregated data according to a given ratio", {
  set.seed(4072017)
  testData <- data.frame(CategoryOne = rep("A", 3), CategoryTwo = c("B", "C", "D"),
                         YearOne = rpois(3, 5), YearTwo = rpois(3, 5))

  ratios <- c("E"=0.6, "F"=0.4)
  result <- prorateDown(testData, "CategoryTwo", "B", c("YearOne", "YearTwo"), ratios)
  target <- structure(list(CategoryOne = structure(c(1L, 1L, 1L, 1L),
                                                   .Label = "A", class = "factor"),
                           CategoryTwo = c("E", "F", "C", "D"),
                           YearOne = c(3, 2, 1, 3),
                           YearTwo = c(2.4, 1.6, 7, 8)),
                      .Names = c("CategoryOne", "CategoryTwo", "YearOne", "YearTwo"),
                      row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(result, target)
})

test_that("prorateDown splits the aggregated data according to a given ratio and
          returns the original data", {
  set.seed(4072017)
  testData <- data.frame(CategoryOne = rep("A", 3), CategoryTwo = c("B", "C", "D"),
                         YearOne = rpois(3, 5), YearTwo = rpois(3, 5))

  ratios <- c("E"=0.6, "F"=0.4)
  result <- prorateDown(testData, "CategoryTwo", "B", c("YearOne", "YearTwo"),
                        ratios, keepCategory = TRUE)
  target <- structure(list(CategoryOne = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "A", class = "factor"),
                           CategoryTwo = c("E", "F", "B", "C", "D"),
                           YearOne = c(3, 2, 5, 1, 3),
                           YearTwo = c(2.4, 1.6, 4, 7, 8)),
                      .Names = c("CategoryOne", "CategoryTwo", "YearOne", "YearTwo"),
                      row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(result, target)
})

test_that("prorateDownWithTimeSeries splits the aggregated data according to a
          given data frame of ratios", {
  set.seed(4072017)
  testData <- data.frame(CategoryOne = rep("A", 3), CategoryTwo = c("B", "C", "D"),
                         YearOne = rpois(3, 5), YearTwo = rpois(3, 5))

  ratios <- data.frame(CategoryTwo = c("E", "F"), YearOne = c(0.6, 0.4),
                       YearTwo = c(0.6, 0.4))

  result <- prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                      c("YearOne", "YearTwo"), ratios)

  target <- structure(list(CategoryOne = structure(c(1L, 1L, 1L, 1L),
                                                   .Label = "A", class = "factor"),
                           CategoryTwo = c("E", "F", "C", "D"),
                           YearOne = c(3, 2, 1, 3),
                           YearTwo = c(2.4, 1.6, 7, 8)),
                      .Names = c("CategoryOne", "CategoryTwo", "YearOne", "YearTwo"),
                      row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(result, target)
})

test_that("prorateDownWithTimeSeries splits the aggregated data according to a
          given data frame of ratios and keeps the original data", {
  set.seed(4072017)
  # now also changing the column names to digits (have to use tibble instead
  # data.frame to be able to do that)
  testData <- tibble::tibble(CategoryOne = rep("A", 3), CategoryTwo = c("B", "C", "D"),
                         "1" = rpois(3, 5), "2" = rpois(3, 5))

  ratios <- tibble::tibble(CategoryTwo = c("E", "F"), "1" = c(0.6, 0.4),
                       "2" = c(0.6, 0.4))

  result <- prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                      c("1", "2"), ratios,
                                      keepCategory = TRUE)

  target <- structure(list(CategoryOne = c("A", "A", "A", "A", "A"),
                           CategoryTwo = c("E", "F", "B", "C", "D"),
                           `1` = c(3, 2, 5, 1, 3), `2` = c(2.4, 1.6, 4, 7, 8)),
                      .Names = c("CategoryOne", "CategoryTwo", "1", "2"),
                      row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(result, target)
})

test_that("prorateDownWithTimeSeries throws an error for incorrect input",{
  set.seed(4072017)
  testData <- data.frame(CategoryOne = rep("A", 3), CategoryTwo = c("B", "C", "D"),
                         YearOne = rpois(3, 5), YearTwo = rpois(3, 5))

  ratios <- data.frame(CategoryTwo = c("E", "F"), YearOne = c(0.6, 0.4),
                       YearTwo = c(0.6, 0.4))

  # error because Category is not a valid column
  expect_error(prorateDownWithTimeSeries(testData, "Category", "B",
                                         c("YearOne", "YearTwo"), ratios))
  # error because Z is not a valid value
  expect_error(prorateDownWithTimeSeries(testData, "CategoryTwo", "Z",
                                         c("YearOne", "YearTwo"), ratios))
  # error because CategoryOne is not a valid column of ratios
  expect_error(prorateDownWithTimeSeries(testData, "CategoryOne", "A",
                                         c("YearOne", "YearTwo"), ratios))
  # error because the value columns are not valid columns
  expect_error(prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                         c("Year1", "Year2"), ratios))

  # error because the value columns are not valid columns in ratios
  colnames(ratios) <- c("CategoryTwo", "Year1", "Year2")
  expect_error(prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                         c("YearOne", "YearTwo"), ratios))
  colnames(ratios) <- c("CategoryTwo", "YearOne", "YearTwo")

  # error because the ratios don't sum to 1
  ratios[1, 2] = 1
  expect_error(prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                         c("YearOne", "YearTwo"), ratios))
  ratios[1, 2] = 0.6

  # error because there is other grouping column
  testData[, "CategoryOne"] <- NULL
  expect_error(prorateDownWithTimeSeries(testData, "CategoryTwo", "B",
                                         c("YearOne", "YearTwo"), ratios))

})

test_that("prorateDown splits the aggregated data according to a given ratio - complex case", {
  testData <- readr::read_csv("../../inst/data/test_prorate.csv")
  ratios <- readr::read_csv("../../inst/data/test_prorate_ratios.csv")

  values <- setdiff(colnames(testData), c("Industry", "Sector", "Asset",
                                          "Change.in.assets", "Prices"))
  result <- prorateDown(testData, "Sector", "S.1", values, ratios)
  expect_equal(dim(result), c(9, 18))
  expect_equal(colSums(result[, values]), colSums(testData[, values]), tolerance=1e-6)
})


test_that("Proration works for multiple groupings (#21)", {
  # Create dataset with three grouping categories and one value
  testData <- tibble::tribble(
    ~Industry, ~Asset, ~Sector, ~P1997Q1,
    "37-39", "PLANT", "S.11PR", 300,
    "37-39", "PLANT", "S.15", 100)
  
  # Prorate Industry 37-39 across 37 and 38-39 (half each)
  result <- prorateDown(testData, column = "Industry", 
              aggregated_category = "37-39", 
              values = "P1997Q1", 
              ratios = c("37"=1/2, "38-39"=1/2))
  target <- structure(list(
    Industry = c("37", "38-39", "37", "38-39"),
    Asset = c("PLANT", "PLANT", "PLANT", "PLANT"),
    Sector = c("S.11PR", "S.11PR", "S.15", "S.15"),
    P1997Q1 = c(150, 150, 50, 50)), 
    .Names = c("Industry", "Asset", "Sector", "P1997Q1"), 
    row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(result, target)
})
