context("Expand config specification")
suppressWarnings(library(tibble))

test_that("expandConfigSpec produces correct result.", {

  configSpec <- tribble(
    ~Sector, ~Industry, ~Asset, ~Notes, ~profileType, ~profileFunctionName, ~retirementDistName, ~rightTruncate, ~combinationMethod, ~discountRate, ~inflationRate, ~offSet, ~profileFunctionParam,
    "ALL","ALL","TRANSPORT","Override for specific Sector and Industry","age-efficiency","lin","pnorm",FALSE,"1",0.1,0.1,0,0,
    "ALL","ALL","HARDWARE","Override for specific Sector and Industry","age-efficiency","lin","pnorm",FALSE,"1",0.1,0.1,0,0)

  toCover <- tribble(
    ~Sector, ~Industry, ~Asset,
    "S.110011", "01", "TRANSPORT",
    "S.110011", "01", "HARDWARE")

  result <- expandConfigSpec(configSpec, toCover, joinKeys = c("Asset", "Industry", "Sector"))
  expect_equal(select(result, Sector, Industry, Asset), toCover)
  expect_equal(dim(result), c(2, 13))

})

test_that("expandConfigSpec warns if coverage is not covered by spec.", {
  configSpec <- tribble(
    ~Sector, ~Industry, ~Asset, ~Notes, ~profileType, ~profileFunctionName, ~retirementDistName, ~rightTruncate, ~combinationMethod, ~discountRate, ~inflationRate, ~offSet, ~profileFunctionParam,
    "ALL","ALL","TRANSPORT","Override for specific Sector and Industry","age-efficiency","lin","pnorm",FALSE,"1",0.1,0.1,0,0,
    "ALL","ALL","HARDWARE","Override for specific Sector and Industry","age-efficiency","lin","pnorm",FALSE,"1",0.1,0.1,0,0)

  toCover <- tribble(
    ~Sector, ~Industry, ~Asset,
    "S.110011", "01", "TRANSPORT",
    "S.110011", "01", "HARDWARE",
    "S.110011", "01", "NOT.COVERED")

  expect_warning({
    result <- expandConfigSpec(configSpec, toCover, joinKeys = c("Asset", "Industry", "Sector"))
  })

  # Expect result to contain just the matched series
  expected <- toCover %>% filter(Asset != "NOT.COVERED")
  expect_equal(select(result, Sector, Industry, Asset), expected)  # Just check coverage columns
  expect_equal(dim(result), c(2, 13))

})

test_that("expandConfigSpec produces correct result using spec with varying specificity.", {
  # Expand specs using coverage of TRANSPORT matching on Asset/Industry/Sector, then Asset/Industry, then Asset.

  configSpec <- tribble(
    ~Sector, ~Industry, ~Asset, ~Notes, ~profileType, ~profileFunctionName, ~retirementDistName, ~rightTruncate, ~combinationMethod, ~discountRate, ~inflationRate, ~offSet, ~profileFunctionParam,
    # profileFunctionName "geom" for specific Sector/Industry/Asset
    "S.11001","02","TRANSPORT","Override for specific Sector and Industry","age-efficiency","geom","pnorm",FALSE,"1",0.1,0.1,0,0,
    # profileFunctionName "const" for specific Industry/Asset
    "ALL","02","TRANSPORT","Override for specific Industry","age-efficiency","const","pnorm",FALSE,"1",0.1,0.1,0,0,
    # Defaults for Assets
    "ALL","ALL","TRANSPORT","Default","age-efficiency","lin","pnorm",FALSE,"1",0.1,0.1,0,0)

toCover <- tribble(
  ~Sector, ~Industry, ~Asset,
  "S.11001", "02", "TRANSPORT",  # should get config with profileFunctionName = "geom"
  "S.15", "02", "TRANSPORT",  # should get config with profileFunctionName = "const"
  "S.15", "99", "TRANSPORT")  # should get the default TRANSPORT config with profileFunctionName type = "lin"

result <- expandConfigSpec(configSpec, toCover, joinKeys = c("Asset", "Industry", "Sector"))

expect_equal(result[result$Sector == "S.11001", ]$profileFunctionName, "geom")
expect_equal(result[result$Sector == "S.15" & result$Industry == "02", ]$profileFunctionName, "const")
expect_equal(result[result$Industry == "99", ]$profileFunctionName, "lin")

})
