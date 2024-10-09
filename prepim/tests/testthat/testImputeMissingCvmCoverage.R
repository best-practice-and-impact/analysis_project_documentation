context("Missing CVM coverage imputation")
suppressWarnings(library(tibble))

cp <- tribble(
  ~Sector, ~Industry, ~Asset, ~Prices, ~Period, ~Value,
  "S.11001","02","SOFT.DATA","CP","Y1997Q1",10.5,
  "S.11001","02","SOFT.DATA","CP","Y1997Q2",1.3,
  "S.11001","02","SOFT.DATA","CP","Y1997Q3",11.7,
  "S.11001","02","SOFT.DATA","CP","Y1997Q4",11.8,
  "S.11001","TOTAL","DWELLINGS","CP","Y1997Q1",710.8,
  "S.11001","TOTAL","DWELLINGS","CP","Y1997Q2",117,
  "S.11001","TOTAL","DWELLINGS","CP","Y1997Q3",49.1,
  "S.11001","TOTAL","DWELLINGS","CP","Y1997Q4",279.2,
  "S.11001","TOTAL","SOFT.DATA","CP","Y1997Q1",10.5,
  "S.11001","TOTAL","SOFT.DATA","CP","Y1997Q2",1.3,
  "S.11001","TOTAL","SOFT.DATA","CP","Y1997Q3",11.7,
  "S.11001","TOTAL","SOFT.DATA","CP","Y1997Q4",11.8,
  "S.1PT","02","LAND.IMPROVEMENTS","CP","Y1997Q1",-35.5,
  "S.1PT","02","LAND.IMPROVEMENTS","CP","Y1997Q2",168.1,
  "S.1PT","02","LAND.IMPROVEMENTS","CP","Y1997Q3",38.6,
  "S.1PT","02","LAND.IMPROVEMENTS","CP","Y1997Q4",281.4,
  "S.1PT","TOTAL","DWELLINGS","CP","Y1997Q1",320.5,
  "S.1PT","TOTAL","DWELLINGS","CP","Y1997Q2",5652.7,
  "S.1PT","TOTAL","DWELLINGS","CP","Y1997Q3",4393.3,
  "S.1PT","TOTAL","DWELLINGS","CP","Y1997Q4",4594.5,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CP","Y1997Q1",-35.5,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CP","Y1997Q2",168.1,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CP","Y1997Q3",38.6,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CP","Y1997Q4",281.4)

# CVM with missing series detail for "S.11001","02","SOFT.DATA"
cvm <- tribble(
  ~Sector, ~Industry, ~Asset, ~Prices, ~Period, ~Value,
  "S.11001","TOTAL","DWELLINGS","CVM","Y1997Q1",710.8,
  "S.11001","TOTAL","DWELLINGS","CVM","Y1997Q2",117,
  "S.11001","TOTAL","DWELLINGS","CVM","Y1997Q3",49.1,
  "S.11001","TOTAL","DWELLINGS","CVM","Y1997Q4",279.2,
  "S.11001","TOTAL","SOFT.DATA","CVM","Y1997Q1",10.5,
  "S.11001","TOTAL","SOFT.DATA","CVM","Y1997Q2",1.3,
  "S.11001","TOTAL","SOFT.DATA","CVM","Y1997Q3",11.7,
  "S.11001","TOTAL","SOFT.DATA","CVM","Y1997Q4",11.8,
  "S.1PT","02","LAND.IMPROVEMENTS","CVM","Y1997Q1",-35.5,
  "S.1PT","02","LAND.IMPROVEMENTS","CVM","Y1997Q2",168.1,
  "S.1PT","02","LAND.IMPROVEMENTS","CVM","Y1997Q3",38.6,
  "S.1PT","02","LAND.IMPROVEMENTS","CVM","Y1997Q4",281.4,
  "S.1PT","TOTAL","DWELLINGS","CVM","Y1997Q1",320.5,
  "S.1PT","TOTAL","DWELLINGS","CVM","Y1997Q2",5652.7,
  "S.1PT","TOTAL","DWELLINGS","CVM","Y1997Q3",4393.3,
  "S.1PT","TOTAL","DWELLINGS","CVM","Y1997Q4",4594.5,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CVM","Y1997Q1",-35.5,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CVM","Y1997Q2",168.1,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CVM","Y1997Q3",38.6,
  "S.1PT","TOTAL","LAND.IMPROVEMENTS","CVM","Y1997Q4",281.4)

test_that("imputeMissingCvmCoverage correctly imputes requested coverage.", {

  # CVM data has missing SOFT.DATA series for Sector "S.11001" Industry "02"
  requiredCoverage = data.frame(Sector = "S.11001",
                                Industry = "02",
                                Asset = "SOFT.DATA",
                                stringsAsFactors = FALSE)
  result <- imputeMissingCvmCoverage(requiredCoverage, cp, cvm)

  expected <- structure(
    list(
      Sector = c("S.11001", "S.11001", "S.11001", "S.11001"),
      Industry = c("02", "02", "02", "02"),
      Asset = c("SOFT.DATA", "SOFT.DATA", "SOFT.DATA", "SOFT.DATA"),
      Prices = c("CVM", "CVM", "CVM", "CVM"),
      Period = c("Y1997Q1", "Y1997Q2", "Y1997Q3", "Y1997Q4"),
      Value = c(10.5, 1.3, 11.7, 11.8)),
    class = "data.frame", row.names = c(NA,-4L),
    .Names = c("Sector", "Industry", "Asset", "Prices", "Period", "Value"))

  expect_equal(result, expected)
})


test_that("imputeMissingCvmCoverage throws error if TOTALs are not available in BOTH CP/CVM series for requested coverage", {

  # CVM data has missing SOFT.DATA series for Sector "S.11001" Industry "02"
  # It also doesn't have a fall-back TOTAL series for Sector S.11PR, so should error
  requiredCoverage = data.frame(Sector = "S.11001",
                                Industry = "02",
                                Asset = "SOFT.DATA",
                                stringsAsFactors = FALSE)
  # Create cp data lacking TOTALs for S.11001/SOFT.DATA
  cpNoTotals <- filter(cp, !(Sector == "S.11001" & Industry == "TOTAL" & Asset == "SOFT.DATA"))

  expect_error(imputeMissingCvmCoverage(requiredCoverage, cpNoTotals, cvm),
               regexp = "Fallback sector does not contain TOTALs")
})

test_that("imputeMissingCvmCoverage falls back to S.11PR if no Sector/Asset combo available.", {

  # CVM data has missing SOFT.DATA series for Sector "S.11001" Industry "02"
  requiredCoverage = data.frame(Sector = "S.11001",
                                Industry = "02",
                                Asset = "SOFT.DATA",
                                stringsAsFactors = FALSE)
  # Create cp data lacking TOTALs for S.11001/SOFT.DATA
  cpFallback <- filter(cp, !(Sector == "S.11001" & Industry == "TOTAL" & Asset == "SOFT.DATA"))
  # Add in the SOFT.DATA TOTALs for the fallback sector S.11PR
  fallbackTotals <- tribble(
    ~Sector, ~Industry, ~Asset, ~Prices, ~Period, ~Value,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q1",10.5,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q2",1.3,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q3",11.7,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q4",11.8)
  cpFallback <- bind_rows(cpFallback, fallbackTotals)
  # Add to CVM data too (just use same data)
  fallbackTotals$Prices <- "CVM"
  cvmFallback <- bind_rows(cvm, fallbackTotals)

  result <- imputeMissingCvmCoverage(requiredCoverage, cpFallback, cvmFallback)

  expected <- structure(
    list(
      Sector = c("S.11001", "S.11001", "S.11001", "S.11001"),
      Industry = c("02", "02", "02", "02"),
      Asset = c("SOFT.DATA", "SOFT.DATA", "SOFT.DATA", "SOFT.DATA"),
      Prices = c("CVM", "CVM", "CVM", "CVM"),
      Period = c("Y1997Q1", "Y1997Q2", "Y1997Q3", "Y1997Q4"),
      Value = c(10.5, 1.3, 11.7, 11.8)),
    class = "data.frame", row.names = c(NA,-4L),
    .Names = c("Sector", "Industry", "Asset", "Prices", "Period", "Value"))

  expect_equal(result, expected)
})


test_that("imputeMissingCvmCoverage throws error if fallback sector does not contain TOTALs for both CP and CVM data.", {

  # CVM data has missing SOFT.DATA series for Sector "S.11001" Industry "02"
  # It also doesn't have a fall-back TOTAL series for Sector S.11PR, so should error
  requiredCoverage = data.frame(Sector = "S.11001",
                                Industry = "02",
                                Asset = "SOFT.DATA",
                                stringsAsFactors = FALSE)
  # Create cp data lacking TOTALs for S.11001/SOFT.DATA
  cpNoTotals <- filter(cp, !(Sector == "S.11001" & Industry == "TOTAL" & Asset == "SOFT.DATA"))
  # Add in the SOFT.DATA TOTALs for the fallback sector S.11PR in CP
  # But leave TOTALs out of the CVM data
  fallbackTotals <- tribble(
    ~Sector, ~Industry, ~Asset, ~Prices, ~Period, ~Value,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q1",10.5,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q2",1.3,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q3",11.7,
    "S.11PR","TOTAL","SOFT.DATA","CP","Y1997Q4",11.8)
  cpNoTotals <- bind_rows(cpNoTotals, fallbackTotals)

  expect_error(imputeMissingCvmCoverage(requiredCoverage, cpNoTotals, cvm))
})

