context("Calculating survival rates from a retirement distribution")

test_that("getSurvivalValues returns an error when the input is incorrect",{
  expect_error(getSurvivalValues("A", 1, 2, 3, "pnorm", FALSE, 1, 1))
  expect_error(getSurvivalValues(1, "A", 2, 3, "pnorm", FALSE, 1, 1))
  expect_error(getSurvivalValues(1, 2, "A", 3, "pnorm", FALSE, 1, 1))
  expect_error(getSurvivalValues(1, 2, 3, "A", "pnorm", FALSE, 1, 1))
  expect_error(getSurvivalValues(2, 1, 3, "A", "pnorm", FALSE, 1, 1))
  expect_error(getSurvivalValues(1, 2, 3, 4, "ppois", FALSE, 1, 1))
  expect_error(getSurvivalValues(1, 2, 3, 4, "pnorm", "FALSE", 1, 1))
  expect_error(getSurvivalValues(1, 2, 3, 4, "pnorm", FALSE, -1, 1))
  expect_error(getSurvivalValues(1, 2, 3, 4, "pnorm", FALSE, 2, 1))
  expect_error(getSurvivalValues(1, 2, 3, 4, "pnorm", FALSE, 1, "A"))
  expect_error(getSurvivalValues(1, 2, 3, 4, "pnorm", FALSE, 1, 1))
})

test_that("getSurvivalValues returns a vector of survival rates for different
          life length values",{
  result <- getSurvivalValues(2, 15, 7, 3/7, "pnorm", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9543999, 0.8835709, 0.785024, 0.6622057, 0.5250944,
              0.3879832, 0.2651649, 0.166618, 0.095789, 0.0501889, 0.0238919,
              0.0103079)
  expect_equal(result, target, tolerance=1e-6)

  # change the minimum
  result <- getSurvivalValues(1, 15, 7, 3/7, "pnorm", FALSE, 0, 15)
  target <- c(1, 1, 0.9743768, 0.9299452, 0.860931, 0.7649092, 0.6452379,
              0.5116399, 0.3780418, 0.2583705, 0.1623487, 0.0933346, 0.0489029,
              0.0232797, 0.0100438)
  expect_equal(result, target, tolerance=1e-6)

  # change the maximum (have to also change the outputLength)
  result <- getSurvivalValues(2, 17, 7 , 3/7, "pnorm", FALSE, 0, 17)
  target <- c(1, 1, 1, 0.9543999, 0.8835709, 0.785024, 0.6622057, 0.5250944,
              0.3879832, 0.2651649, 0.166618, 0.095789, 0.0501889, 0.0238919,
              0.0103079, 0.0040226, 0.0014176)
  expect_equal(result, target, tolerance=1e-6)

  # change the mean
  result <- getSurvivalValues(2, 15, 10 , 3/7, "pnorm", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9791281, 0.9486262, 0.9064025, 0.8510361, 0.7822663,
              0.7013546, 0.6111794, 0.5159821, 0.4207847, 0.3306095, 0.2496978,
              0.180928)
  expect_equal(result, target, tolerance=1e-6)

  # change the cov
  result <- getSurvivalValues(2, 15, 7 , 5/7, "pnorm", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9367677, 0.8626035, 0.7790169, 0.6884927, 0.5942867,
              0.5000807, 0.4095566, 0.32597, 0.2518057, 0.1885734, 0.1367688,
              0.0959852)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("getSurvivalValues returns a vector of survival rates for different
          retirement distributions",{

  result <- getSurvivalValues(2, 15, 7, 3/7, "plnorm", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9705716, 0.8784098, 0.7320395, 0.5687859, 0.419592,
              0.2985448, 0.2073297, 0.1417463, 0.0959872, 0.0646626, 0.0434685,
              0.0292239)
  expect_equal(result, target, tolerance=1e-6)

  result <- getSurvivalValues(2, 15, 7, 3/7, "pgamma", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9542828, 0.8621448, 0.7331025, 0.5882909, 0.4482161,
              0.3263955, 0.2285825, 0.154778, 0.1017902, 0.0652642, 0.0409249,
              0.0251647)
  expect_equal(result, target, tolerance=1e-6)

  result <- getSurvivalValues(2, 15, 7, 3/7, "pweibull", FALSE, 0, 15)
  target <- c(1, 1, 1, 0.9445759, 0.859779, 0.7498419, 0.6234593, 0.491969,
              0.366906, 0.2576218, 0.1696865, 0.1044869, 0.0599532, 0.0319558,
              0.0157753)
  expect_equal(result, target, tolerance=1e-6)

  result <- getSurvivalValues(2, 15, 7, 3/7, "none", FALSE, 0, 15)
  target <- rep(1, times = 15)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("getSurvivalValues returns a vector of survival rates which is right truncated",{
  result <- getSurvivalValues(2, 15, 7, 3/7, "pnorm", TRUE, 0, 15)
  target <- c(1, 1, 1, 0.9542157, 0.8831007, 0.7841558, 0.6608414, 0.5231764,
              0.3855113, 0.262197, 0.1632521, 0.092137, 0.0463527, 0.0199496,
              0.0063107)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("getSurvivalValues returns a vector of survival rates for different
          values of the off set",{
  result <- getSurvivalValues(2, 15, 7, 3/7, "pnorm", FALSE, 0.5, 15)
  target <- c(1, 1, 0.9800287, 0.9224098, 0.8377059, 0.7261662, 0.5946,
              0.4555889, 0.3240227, 0.212483, 0.1277791, 0.0701602, 0.0350516,
              0.0158895, 0.0065213)
  expect_equal(result, target, tolerance=1e-6)

  result <- getSurvivalValues(2, 15, 7, 3/7, "pnorm", FALSE, 1, 15)
  target <- c(1, 1, 0.9543999, 0.8835709, 0.785024, 0.6622057, 0.5250944,
              0.3879832, 0.2651649, 0.166618, 0.095789, 0.0501889, 0.0238919,
              0.0103079, 0.0040226)
  expect_equal(result, target, tolerance=1e-6)
})

test_that("getSurvivalValues returns a vector of survival rates for different
          values of output length",{
  result <- getSurvivalValues(2, 15, 7, 3/7, "pnorm", FALSE, 0.5, 18)
  expect_equal(sum(result[(length(result)-1):length(result)]), 0)
})

test_that("calcSurvivalValues returns an error when the input is incorrect",{
  df <- data.frame(x=c(1,2))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(Min=c("A","B"), Max=c("A","B"), notmean=c("A","B"), CoV=c("A","B"))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(Min=c("A","B"), Max=c("A","B"), Average=c("A","B"), notcov=c("A","B"))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(notmin=c("A","B"), Max=c("A","B"), Average=c("A","B"), CoV=c("A","B"))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(Min=c("A","B"), notmax=c("A","B"), Average=c("A","B"), CoV=c("A","B"))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(Min=c("A","B"), Max=c("A","B"), Average=c("A","B"), CoV=c("A","B"))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))

  df <- data.frame(notmin=c(NA, 2), Max=c(1, 2), Average=c(1, 2), CoV=c(1, 2))
  expect_error(capture.output(calcSurvivalValues(df, NULL)))
})

test_that("calcSurvivalValues returns a vector of survival rates for each vintage",{
  df <- data.frame(Average=c(7, 7, 8), CoV=c(3/7, 3/7, 3/8), Min=c(2, 2, 4),
                   Max=c(15, 15, 14))
  config <- list(rightTruncate=FALSE, retirementDist="pnorm", offSet=0)
  result <- calcSurvivalValues(df, config)

  # there should be a vector of survival rates for each each vintage
  expect_equal(nrow(df), nrow(result))
  expect_equal(colnames(result), c("id", "values", "vintageId"))
  # there are 2 unique "retirement profiles" in the data
  expect_equal(length(unique(result$id)), 2)
  # the two survival rates of these profiles are different
  # NOTE: all.equal returns a character vector if this isn't the case
  expect_true(is.character(all.equal(result$values[2], result$values[3])))
})
