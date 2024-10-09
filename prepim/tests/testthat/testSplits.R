context("Apply splits")
suppressWarnings(library(tibble))
# Redirect all the noisy logger output to a file
logFile <- "unit_tests.log"
flog.appender(appender.file(logFile))

testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                      "S.1PT", "01", "HARDWARE", "Y2000Q1", 1, "S.11")

test_that("applySplits rejects data without the required column headings.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Prices)  # No "Value" column

  expect_error(applySplits(.data = testDf, splits = testSplits,
                           existingCategory = "Sector",
                           newCategory = "Sector_new"))
})

test_that("applySplits stops if newCategory is not a valid variable name.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Prices, ~Value)

  expect_error(applySplits(.data = testDf, splits = testSplits,
                           existingCategory = "Sector",
                           newCategory = "Sector new"),  # Space in variable name
               "invalid variable name.")
})


test_that("applySplits stops when specified categories don't exist in datasets.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100)

  expect_error(applySplits(.data = testDf, splits = testSplits,
                           existingCategory = "Missing",
                           newCategory = "Sector_new"),
               ".data does not contain column: Missing")
  expect_error(applySplits(.data = testDf, splits = testSplits,
                           existingCategory = "Sector",
                           newCategory = "newSector"),
               "splits does not contain column: newSector")

  # Check for success when colnames are present
  expect_is(applySplits(.data = testDf, splits = testSplits,
                        existingCategory = "Sector",
                        newCategory = "Sector_new"),
            "data.frame")
})


test_that("Splits are applied correctly.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100,
                    "S.123", "01", "HARDWARE", "Y2000Q1", 123)  # S.123 won't be matched
  testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.2, "S.11PR",
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.8, "S.14")

  result <- applySplits(.data = testDf, splits = testSplits,
              existingCategory = "Sector",
              newCategory = "Sector_new")
  target <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value,
    "S.123","01","HARDWARE","Y2000Q1",123,
    "S.11PR","01","HARDWARE","Y2000Q1",20,
    "S.14","01","HARDWARE","Y2000Q1",80)

  # expect all rows to match (but order doesn't matter)
  expect_true(nrow(dplyr::anti_join(result, target)) == 0)
})


test_that("Splits are applied correctly when data has additional columns.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value, ~ExtraCol,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100, "x",
                    "S.123", "01", "HARDWARE", "Y2000Q1", 123, "y")
  testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.2, "S.11PR",
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.8, "S.14")

  result <- applySplits(.data = testDf, splits = testSplits,
              existingCategory = "Sector",
              newCategory = "Sector_new")
  target <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value, ~ExtraCol,
    "S.123","01","HARDWARE","Y2000Q1",123,"y",
    "S.11PR","01","HARDWARE","Y2000Q1",20,"x",
    "S.14","01","HARDWARE","Y2000Q1",80,"x")
  # expect all rows to match (but order doesn't matter)
  expect_true(nrow(dplyr::anti_join(result, target)) == 0)
})

test_that("applySplits stops if Proportions are not between 0 and 1.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100)
  testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 1.2, "S.11PR",  # Proportion 1.2
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.8, "S.14")

  expect_error(applySplits(.data = testDf, splits = testSplits,
                        existingCategory = "Sector",
                        newCategory = "Sector_new"))
})

test_that("applySplits stops if Proportions do not sum 1.", {

  # Proportions should sum to 1 when grouped by Sector/Industry/Asset/Period
  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100)
  # splits sum to 1.2
  testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.3, "S.11PR",
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.8, "S.14")

  expect_error(applySplits(.data = testDf, splits = testSplits,
                           existingCategory = "Sector",
                           newCategory = "Sector_new"))
})

test_that("applySplits generates warning for missing coverage.", {

  testDf <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Value,
                    "S.1PT", "01", "HARDWARE", "Y2000Q1", 100,
                    "S.123", "01", "HARDWARE", "Y2000Q1", 123,  # No corresponding row in splits, pass through
                    "S.1PT", "02", "HARDWARE", "Y2000Q1", 222)  # No corresponding row in splits, but S.1PT is specified in splits

  testSplits <- tribble(~Sector, ~Industry, ~Asset, ~Period, ~Proportion, ~Sector_new,
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.2, "S.11PR",
                        "S.1PT", "01", "HARDWARE", "Y2000Q1", 0.8, "S.14")

  expect_warning(applySplits(.data = testDf, splits = testSplits,
                        existingCategory = "Sector",
                        newCategory = "Sector_new"),
                 "Possible missing coverage")
})

test_that("applySplits can take file through whole process from split spec to new gfcf",{
  gfcf <- readr::read_csv("../../inst/data/splits/gfcf.csv")
  spec <- readr::read_csv("../../inst/data/splits/split_spec.csv")
  target <- readr::read_csv("../../inst/data/splits/gfcf_split.csv")

  # Expand sector split definition using gfcf file as the desired coverage
  expanded <- expandSplitSpec(spec, gfcf, joinKeys = c("Sector", "Industry", "Asset"))

  # Apply splits
  result <- applySplits(gfcf, splits = expanded, existingCategory = "Sector", newCategory = "Split")
  result$Expectation <- NULL  # Remove notes column
  # Round Values to remove any tiny differences in proportions
  result$Value <- round(result$Value, 4)
  target$Value <- round(target$Value, 4)
  expect_true(dplyr::all_equal(result, target))
})

test_that("applySplitSpec can take file through whole process from split spec FILE to new gfcf",{
  gfcf <- readr::read_csv("../../inst/data/splits/gfcf.csv")
  specPath <- "../../inst/data/splits/split_spec.csv"
  target <- readr::read_csv("../../inst/data/splits/gfcf_split.csv")

  # Apply splits
  result <- applySplitSpec(gfcf, spec = specPath,
                           existingCategory = "Sector", newCategory = "Split",
                           joinKeys = c("Sector", "Industry", "Asset"))
  result$Expectation <- NULL  # Remove notes column
  # Round Values to remove any tiny differences in proportions
  result$Value <- round(result$Value, 4)
  target$Value <- round(target$Value, 4)
  expect_true(dplyr::all_equal(result, target))
})

test_that("applySplitSpec can take file through whole process from split spec DATAFRAME to new gfcf",{
  gfcf <- readr::read_csv("../../inst/data/splits/gfcf.csv")
  specPath <- "../../inst/data/splits/split_spec.csv"
  target <- readr::read_csv("../../inst/data/splits/gfcf_split.csv")

  # Extract sector split definition into a data.frame
  specDataframe <- extractSplitSpec(specPath)

  # Apply splits
  result <- applySplitSpec(gfcf, spec = specDataframe,
                           existingCategory = "Sector", newCategory = "Split",
                           joinKeys = c("Sector", "Industry", "Asset"))
  result$Expectation <- NULL  # Remove notes column
  # Round Values to remove any tiny differences in proportions
  result$Value <- round(result$Value, 4)
  target$Value <- round(target$Value, 4)
  expect_true(dplyr::all_equal(result, target))
})

test_that("applySplitSpec rejects invalid split spec type.",{
  gfcf <- readr::read_csv("../../inst/data/splits/gfcf.csv")
  specPath <- 100 # invalid spec
  expect_error(applySplitSpec(gfcf, spec = specPath,
                           existingCategory = "Sector", newCategory = "Split",
                           joinKeys = c("Sector", "Industry", "Asset")),
               "Unrecognised spec format")
})

unlink(logFile)
