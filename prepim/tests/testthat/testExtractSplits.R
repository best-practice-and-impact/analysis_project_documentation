context("Extract splits specification")
suppressWarnings(library(tibble))

categories <- c("Sector", "Industry", "Asset", "Split")

test_that("extractSplitsSpec can succesfully read a splits file.", {
  result <- readSplitsFile("../../inst/data/splits/split_spec.csv")
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 12)
  expect_equal(ncol(result), 6)
})

test_that("Columns are verified correctly.", {

  # Should return true if required cols are present
  good <- tribble(
    ~Industry, ~Asset, ~Split, ~Sector, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "1","OTHER.BUILDINGS","LAND.IMPROVE","S.11PR",0.1,0.1,0.1)
  expect_true(checkColNames(good, categories))

  # Should return false if required cols are not present
  bad <- tribble(
    ~BadCol, ~`Bad Col Name`, ~Split, ~Sector, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "1","OTHER.BUILDINGS","LAND.IMPROVE","S.11PR",0.1,0.1,0.1)
  expect_false(checkColNames(bad, categories))

  # Should return false if Period cols are not in "Y2012Q1" type format
  bad <- tribble(
    ~Industry, ~Asset, ~Split, ~Sector, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3XXX,
    "1","OTHER.BUILDINGS","LAND.IMPROVE","S.11PR",0.1,0.1,0.1)
  expect_false(checkColNames(bad, categories))

})

test_that("Proportions are verified correctly.", {

  # Should return true if all Sector/Industry/Asset/Period splits sum to 1 and are in
  # the range 0-1

  # Test valid splits, e.g. Sector
  good <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "S.1PT","01","OTHER.BUILDINGS","S.11PR",0.8,0.8,0.8,
    "S.1PT","01","OTHER.BUILDINGS","S.14",0.1,0.1,0.1,
    "S.1PT","01","OTHER.BUILDINGS","S.15",0.1,0.1,0.1
    )

  expect_true(checkProportions(good, categories))

  # Test splits with value greater than 1
  bad <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "S.1PT","01","OTHER.BUILDINGS","S.11PR",0.8,0.8,0.8,
    "S.1PT","01","OTHER.BUILDINGS","S.14",0.1,0.1,0.1,
    "S.1PT","01","OTHER.BUILDINGS","S.15",0.1,0.1,10)  # invalid proportion (10)
  expect_false(checkProportions(bad, categories))

  # Test splits that do not sum to 1 (they sum to 1.4 in Y1828Q3)
  bad <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "S.1PT","01","OTHER.BUILDINGS","S.11PR",0.8,0.8,0.8,
    "S.1PT","01","OTHER.BUILDINGS","S.14",0.1,0.1,0.1,
    "S.1PT","01","OTHER.BUILDINGS","S.15",0.1,0.1,0.5
  )
  expect_false(checkProportions(bad, categories))

})

test_that("Split specs of Sector/Industry/Asset of ALL/ALL/ALL are rejected.", {

  bad <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Y1828Q1, ~Y1828Q2, ~Y1828Q3,
    "ALL","ALL","ALL","S.11PR",0.8,0.8,0.8,
    "ALL","ALL","ALL","S.14",0.1,0.1,0.1,
    "ALL","ALL","ALL","S.15",0.1,0.1,0.5
  )
  expect_false(chkSpecCombinations(bad))

})
