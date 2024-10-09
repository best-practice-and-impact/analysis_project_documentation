
context("Add Life Lengths")
suppressWarnings(library(tibble))
# Redirect any logger output to a file
flog.appender(appender.file("unit_tests.log"))

# --------------------------- Test Data ----------------------------------------
gfcf <- tribble(
  ~Sector, ~Industry, ~Asset, ~Period, ~Value,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q1",10,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q2",10,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q3",10,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q4",10,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q1",10)

# ------------------------------------------------------------------------------

test_that("addLifeLengths appends life length columns.", {

  result <- addLifeLengths(gfcf, "../../inst/data/test lifelengths.xlsx")

  # Test shape
  expect_equal(dim(result), c(5, 9))
  expect_equal(colnames(result), c("Sector", "Industry", "Asset", "Period", "Value",
                                   "Average", "CoV", "Max", "Min"))

  # Test values
  lifeLengthCols <- data.frame(Average = rep(24,5),
                               CoV = rep(0.15, 5),
                               Max = rep(40, 5),
                               Min = rep(4, 5))
  target <- cbind(gfcf, lifeLengthCols)
  expect_equal(result, target)

})


test_that("addLifeLengths throws an error if there is unmatched coverage.", {

  # test_lifelengths.xlsx does not contain lifelengths for industry 20
  gfcf <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value,
    "S.11001","20","OTHER.BUILDINGS","Y1996Q1",10,
    "S.11001","20","OTHER.BUILDINGS","Y1996Q2",10,
    "S.11001","20","OTHER.BUILDINGS","Y1996Q3",10,
    "S.11001","20","OTHER.BUILDINGS","Y1996Q4",10,
    "S.11001","20","OTHER.BUILDINGS","Y1997Q1",10)
  expect_error(addLifeLengths(gfcf, "../../inst/data/test lifelengths.xlsx"),
               "Missing life length coverage")

})


# --------------------- extractLifeLengths -------------------------------------

test_that("extractLifeLengths pads industry codes.", {
  # e.g. industry codes "1" should be translated to "01"
  gfcf <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q1",10,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q2",10,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q3",10,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q4",10,
    "S.11001","01","OTHER.BUILDINGS","Y1997Q1",10)

  result <- extractLifeLength(llFile = "../../inst/data/test lifelengths bad industry.xlsx",
                    toCover = gfcf, varName = "Average", sheet = "AverageLifeLengths")

  target <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Average,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q1",24,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q2",24,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q3",24,
    "S.11001","01","OTHER.BUILDINGS","Y1996Q4",24,
    "S.11001","01","OTHER.BUILDINGS","Y1997Q1",24)
  expect_equal(result, target)


})
