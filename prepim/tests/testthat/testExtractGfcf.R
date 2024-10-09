context("Data extraction")
suppressWarnings(library(tibble))
suppressWarnings(library(readr))
# Redirect all the noisy logger output to a file
flog.appender(appender.file("unit_tests.log"))

# -------------------------- Balanced Accounts ---------------------------------

test_that("extractBalAccountsCord correctly processes a CORD balanced accounts file in current prices.", {

  result <- extractBalAccountsCord("../../inst/data/test cp all BL.xlsx")

  # Check dimensions
  expect_is(result, "data.frame")
  expect_equal(dim(result), c(54, 6))
  expect_equal(colnames(result), c("Sector","Industry","Asset","Prices","Period","Value"))

  # Compare to checksum
  expect_equal(sum(result$Value), 34705.5)

  # Compare values
  expect_equal(unique(result$Prices), "CP")
  expect_is(result$Value, "numeric")

})

test_that("extractBalAccountsCord correctly processes a CORD ba file when there are no disposals.", {

  result <- extractBalAccountsCord("../../inst/data/test cp all BL no dis.xlsx")

  # Check dimensions
  expect_is(result, "data.frame")
  expect_equal(dim(result), c(54, 6))
  expect_equal(colnames(result), c("Sector","Industry","Asset","Prices","Period","Value"))

  # Compare to checksum
  expect_equal(sum(result$Value), 37003.1)

  # Compare values
  expect_equal(unique(result$Prices), "CP")
  expect_is(result$Value, "numeric")

})

# GFCF input files can also be in a pre-calculated format not directly from
# CORD exports. The agreed format of these files is a very similar format to CORD
# but with net values rather than Aquisitions and Disposals, i.e. Basis values
# of "NET" rather than "ACQ" and "DIS"
test_that("extractBalAccountsCord correctly processes a balanced accounts file in COFOG format.", {

  result <- extractBalAccountsCord("../../inst/data/test gfcf ba cofog.xlsx",
                                    skip = 0)
  # Check dimensions
  expect_is(result, "data.frame")
  expect_equal(dim(result), c(876, 6))
  expect_equal(colnames(result), c("Sector","Industry","Asset","Prices","Period","Value"))

  # Compare to checksum
  expect_equal(sum(result$Value), 52595.639)

  # Compare values
  expect_equal(unique(result$Prices), "CP")
  expect_is(result$Value, "numeric")

})

test_that("extractBalAccountsCord returns error when Basis of ACQ/DIS and NET are mixed in same file.", {

  expect_error(extractBalAccountsCord("../../inst/data/test gfcf ba cofog mixed basis.xlsx",
                                    skip = 0),
               "Basis must either contain combinations of ACQ and DIS, OR just NET, but not both")
})

test_that("extractBalAccountsCord returns error when there are duplicate series.", {

  expect_error(extractBalAccountsCord("../../inst/data/test gfcf ba cofog duplicates.xlsx",
                                      skip = 0),
               "Input file contains duplicates series")
})

test_that("extractBalAccountsCord standarises Industry separator to underscores.", {
  result <- extractBalAccountsCord("../../inst/data/test cp all BL with hyphens.xlsx")

  # Check dimensions
  expect_is(result, "data.frame")
  expect_equal(colnames(result), c("Sector","Industry","Asset","Prices","Period","Value"))

  # Check Industry "05-09" was recoded to "05_09"
  expect_equal(unique(result$Industry), "05_09")

})

# -------------------------- Historic ------------------------------------------

test_that("extractHistoric correctly processes a historic GFCF file.", {

  library(readr)
  testFile <- "../../inst/data/test historic gfcf.csv"
  result <- extractHistoric(testFile)

  # Check no quantities are lost by calculating a checksum and comparing
  rawData <- read_csv(testFile,
                      col_types = cols(
                        .default = col_double(),
                        Industry = col_character(),
                        Asset = col_character(),
                        Sector = col_character()
                      ))
  # Sum across rows and cols to get total quantity of all numeric cols
  totalQuantity <- rawData %>%
    select_if(is.numeric) %>%
    transmute(total = rowSums(., na.rm = TRUE)) %>%
    colSums(.)
  # Compare result to checksum
  expect_equivalent(totalQuantity, sum(result$Value))

  # Check dimensions
  expect_is(result, "data.frame")
  expect_equal(dim(result), c(120, 6))
  expect_equal(colnames(result), c("Sector","Industry","Asset","Prices","Period","Value"))

  # Compare values
  expect_equal(unique(result$Prices), "CP")
  expect_is(result$Value, "numeric")

})

test_that("extractHistoric correctly flags an incorrectly formatted GFCF file.", {

  # Create file with non-contiguous years (1951, *1953*, and 1954)
  nonContiguous <- tribble(~Industry, ~Asset, ~Sector, ~Y1951, ~Y1953, ~Y1954,
                            "10", "HARDWARE", "S.11PR", 5, 10, 15)
  tmpFile <- tempfile()
  write_csv(nonContiguous, path = tmpFile)
  expect_error(extractHistoric(tmpFile))
  unlink(tmpFile)

})

test_that("extractHistoric converts single-digit Industries into two-digits.", {

  # e.g. Industry "1" -> "01"
  # Create file with single-digit industry
  singleDigitInd <- tribble(~Industry, ~Asset, ~Sector, ~Y1951, ~Y1952, ~Y1953,
                           "1", "HARDWARE", "S.11PR", 5, 10, 15)
  tmpFile <- tempfile()
  write_csv(singleDigitInd, path = tmpFile)
  result <- extractHistoric(tmpFile)
  unlink(tmpFile)
  expect_true(unique(result$Industry) == "01")

})
