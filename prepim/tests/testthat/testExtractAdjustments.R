context("Extract adjustments")
suppressWarnings(library(tibble))
suppressWarnings(library(readr))

test_that("extractAdjustments detects duplicate adjustments.", {

  adjBad <- tribble(~Sector, ~Industry, ~Asset, ~Period,
          ~K1CP, ~K3CP, ~K4CP, ~K5CP, ~K61CP, ~K62CP,
          "S.11001", "02", "TELECOMS", "Y1990Q1",
          0, -10, 0, 0, 0, 0,

          "S.11001", "02", "TELECOMS", "Y1990Q1",
          0, -20, 0, 0, 0, 0)

  adjFile <- tempfile()
  write_csv(adjBad, path = adjFile)
  expect_error(extractAdjustments(adjFile), "Adjustments file contains duplicates")
  unlink(adjFile)

})

test_that("extractAdjustments gives expected output.", {

  adj <- tribble(~Sector, ~Industry, ~Asset, ~Period,
                    ~K1CP, ~K3CP, ~K4CP, ~K5CP, ~K61CP, ~K62CP,
                    "S.11001", "02", "TELECOMS", "Y1990Q1",
                    0, -10, 0, 0, 0, 0,

                    "S.15", "02", "TELECOMS", "Y1990Q1",
                    0, -20, 0, 0, 0, 0)

  adjFile <- tempfile()
  write_csv(adj, path = adjFile)
  result <- extractAdjustments(adjFile)
  unlink(adjFile)

  target <- tribble(
    ~Sector, ~Industry, ~Asset, ~Period, ~K1CP, ~K3CP, ~K4CP, ~K5CP, ~K61CP, ~K62CP,
    "S.11001","02","TELECOMS","Y1990Q1",0,-10,0,0,0,0,
    "S.15","02","TELECOMS","Y1990Q1",0,-20,0,0,0,0)

  expect_equal(result, target)

})
