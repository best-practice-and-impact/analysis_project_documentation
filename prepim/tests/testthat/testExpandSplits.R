context("Expand splits specification")
suppressWarnings(library(tibble))
suppressWarnings(library(readxl))


test_that("expandSplitsSpec produces correct result in wide format.", {

  # Read test data
  gfcf <- readr::read_csv("../../inst/data/splits/gfcf.csv")
  spec <- readr::read_csv("../../inst/data/splits/split_spec.csv")
  target <- readr::read_csv("../../inst/data/splits/splits_expanded.csv")

  result <- expandSplitSpec(spec = spec, toCover = gfcf,
                            joinKeys = c("Sector", "Industry", "Asset"),
                            tidy = FALSE)  # Wide format
  expect_true(dplyr::all_equal(result, target,
                        ignore_row_order = TRUE, ignore_col_order = TRUE))
})


test_that("expandSplitSpec can deal with nested split specs when more detailed specs are unmatched.", {
  # Should be able to deal with situation where similar splits are defined
  # at differing levels of detail, e.g. S.1PT/01/HARDWARE, S.1PT/01, S.1PT but
  # where there are no matches for the detailed levels.
  # Relates to http://np2rvlapxx507/fiaza/capstock/issues/22

  gfcf <- tribble(
    ~Sector, ~Industry, ~Asset, ~Prices, ~Period, ~Value,
    "S.1PT","01","TRANSPORT","CP","Y1997Q1",100)

  spec <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Y1997Q1,
    # Define split at most detailed level (but where there is no match in gfcf)
    "S.1PT","01","HARDWARE","S.11PR",0.6,
    "S.1PT","01","HARDWARE","S.14",0.25,
    "S.1PT","01","HARDWARE","S.15",0.15,
    # Define similar split at more general level (where there is a match in gfcf)
    "S.1PT","01","ALL","S.11PR",0.7,
    "S.1PT","01","ALL","S.14",0.2,
    "S.1PT","01","ALL","S.15",0.1)

  # The specific split should be ignored since it is unmatched by gfcf
  # The more general split should be matched producing just a single (3-way) split
  target <- tribble(
    ~Sector, ~Industry, ~Asset, ~Split, ~Period, ~Proportion,
    "S.1PT","01","TRANSPORT","S.11PR","Y1997Q1",0.7,
    "S.1PT","01","TRANSPORT","S.14","Y1997Q1",0.2,
    "S.1PT","01","TRANSPORT","S.15","Y1997Q1",0.1)

  result <- expandSplitSpec(spec = spec, toCover = gfcf,
                            joinKeys = c("Sector", "Industry", "Asset"))
  expect_equal(result, target)

})
