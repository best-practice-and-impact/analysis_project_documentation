context("Running PIM multiple series.")

# Define error-checking function
is.error <- function(x) inherits(x, "error")

test_that("runAll processes multiple series.", {

  testData <- readRDS("../../inst/data/test_runAll.Rds")

  result <- runAll(testData)

  failures <- unlist(lapply(result$result, is.error))

  expect_true(all(failures == FALSE))

})
