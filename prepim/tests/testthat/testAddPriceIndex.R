context("Link/Add PriceIndex")
suppressWarnings(library(tibble))
# Redirect any logger output to a file
flog.appender(appender.file("unit_tests.log"))

# --------------------------- Test Data ----------------------------------------
defHist <- tribble(
  ~Sector, ~Industry, ~Asset, ~Period, ~Value,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q1",99.952,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q2",99.755,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q3",99.714,
  "S.11001","05_09","OTHER.BUILDINGS","Y1996Q4",99.842,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q1",100)

defRecent <- tribble(
  ~Sector, ~Industry, ~Asset, ~Period, ~Value,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q1",55.473,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q2",56.015,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q3",56.703,
  "S.11001","05_09","OTHER.BUILDINGS","Y1997Q4",57.171)

linkPeriod <- "Y1997Q1"
# ------------------------------------------------------------------------------

test_that("linkDeflators can link historic and recent deflators.", {

  result <- linkDeflators(defHist, defRecent, linkPeriod)

  # Test shape
  expect_equal(dim(result), c(8, 5))
  expect_equal(colnames(result), c("Sector", "Industry", "Asset", "Period", "Value"))

  # Test values
  target <- c(55.446, 55.337, 55.314, 55.385, 55.473, 56.015, 56.703, 57.171)
  expect_equal(result$Value, target, tolerance = 1e-4)

  # Test range
  # result should span Periods from min(defHist) to max(defRecent)
  targetRange <- c(min(defHist$Period), max(defRecent$Period))
  expect_equal(range(result$Period), targetRange)

})

test_that("linkDeflators rejects inputs containing duplicates.", {
  # Duplicate first row
  defHistDupe <- defHist[c(1, seq(1, nrow(defHist))), ]
  defRecentDupe <- defRecent[c(1, seq(1, nrow(defRecent))), ]

  # Expect error when historic deflators contain duplicate
  expect_error(linkDeflators(defHistDupe, defRecent, linkPeriod), "defHist contains duplicates")
  # Expect error when recent deflators contain duplicate
  expect_error(linkDeflators(defHist, defRecentDupe, linkPeriod), "defRecent contains duplicates")
})

test_that("linkDeflators rejects inputs when linkPeriod is not present in data.", {
  defHistNoLP <- defHist[defHist$Period != linkPeriod, ]
  defRecentNoLP <- defRecent[defRecent$Period != linkPeriod, ]
  # Expect error when deflators don't contain data at the link period
  expect_error(linkDeflators(defHistNoLP, defRecent, linkPeriod), "linkPeriod")
  expect_error(linkDeflators(defHist, defRecentNoLP, linkPeriod), "linkPeriod")
})

test_that("addPriceIndex adds a PriceIndex.", {

  gfcf <- tribble(
    ~Period, ~Sector, ~Industry, ~Asset, ~CP,
    "Y1997Q1","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q2","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q3","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q4","S.11001","05_09","OTHER.BUILDINGS",10)

  deflators <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q1",55.473,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q2",56.015,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q3",56.703,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q4",57.171)

  result <- addPriceIndex(gfcf, deflators)

  # Test shape
  expect_equal(dim(result), c(4, 6))
  expect_true("PriceIndex" %in% colnames(result))

  # Test values
  expect_equal(result$PriceIndex, deflators$Value)

  # Test gfcf data is unchanged except for addition of price index
  expect_equal(dplyr::select(result, -PriceIndex), gfcf)

})


test_that("addPriceIndex rejects deflators with missing coverage.", {
  gfcf <- tribble(
    ~Period, ~Sector, ~Industry, ~Asset, ~CP,
    "Y1997Q1","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q2","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q3","S.11001","05_09","OTHER.BUILDINGS",10,
    "Y1997Q4","S.11001","05_09","OTHER.BUILDINGS",10)

  # Define deflators without Y1997Q4
  deflators <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~Value,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q1",55.473,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q2",56.015,
    "S.11001","05_09","OTHER.BUILDINGS","Y1997Q3",56.703)

  expect_error(addPriceIndex(gfcf, deflators), "Missing coverage")
})
