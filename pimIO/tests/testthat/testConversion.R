context("Converting from CVM to CP and vice versa")

test_that("convertCPToCVM returns an error for incorrect input", {
  expect_error(convertCPToCVM("A"))
  expect_error(convertCPToCVM(1, "A"))
  expect_error(convertCPToCVM(1, 1:2))
})

test_that("convertCPToCVM converts data in CP to data in CVM", {
  result <- convertCPToCVM(20, 0.69)
  expect_equal(result, 28.985507, tolerance=1e-6)

  result <- convertCPToCVM(10:20, rep(0.5, 11))
  expect_equal(result,c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40),
               tolerance=1e-6)
})

test_that("convertCVMToCP returns an error for incorrect input", {
  expect_error(convertCVMToCP("A"))
  expect_error(convertCVMToCP(1, "A"))
  expect_error(convertCVMToCP(1, 1:2))
})

test_that("convertCVMToCP converts data in CVM to data in CP", {
  result <- convertCVMToCP(20, 0.69)
  expect_equal(result, 13.8)

  result <- convertCVMToCP(10:20, rep(0.5, 11))
  expect_equal(result, c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))
})
