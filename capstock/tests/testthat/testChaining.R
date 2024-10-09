context("Chaining")
suppressWarnings(library(tibble))

getChainTestData <- function(){
  testData <- readr::read_csv("../../inst/data/test_chain.csv")
  withQuarter <- testData %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(Quarter=dplyr::row_number(Year)) %>%
    dplyr::ungroup()
  return(withQuarter)
}

test_that("calcScalingFactor calculates a scaling factor", {
  testData <- getChainTestData()

  result <- calcScalingFactor(testData, 2016)
  target <- c(1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.22, 1.31, 1.31, 1.31, 1.31,
              1.25, 1.25, 1.25, 1.25, 1.21, 1.21, 1.21, 1.21, 1.23, 1.23, 1.23,
              1.23, 1.26, 1.26, 1.26, 1.26, 1.23, 1.23, 1.23, 1.23, 1.25, 1.25,
              1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.26, 1.26, 1.26, 1.26, 1.30,
              1.30, 1.30, 1.30, 1.22, 1.22, 1.22, 1.22, 1.04, 1.04, 1.04, 1.04,
              0.99, 0.99, 0.99, 0.99, 0.98, 0.98, 0.98, 0.98, 0.96, 0.96, 0.96,
              0.96, 0.93, 0.93, 0.93, 0.93, 0.98, 0.98, 0.98, 0.98, 1.00, 1.00,
              1.00, 1.00, 1.00, 1.00)
  expect_equal(result$ScalingFactor, target, tolerance = 1e-2)
})


test_that("calcScalingFactor calculates a scaling factor when CYP/PYP contain zeros", {

  testData <- getChainTestData()
  testData[testData$Year == 2000, c("CYP", "PYP")] <- 0
  result <- calcScalingFactor(testData, 2016)
  target <- c(1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.26, 1.26, 1.26, 1.26,
              1.21, 1.21, 1.21, 1.21, 1.21, 1.21, 1.21, 1.21, 1.23, 1.23, 1.23,
              1.23, 1.26, 1.26, 1.26, 1.26, 1.23, 1.23, 1.23, 1.23, 1.25, 1.25,
              1.25, 1.25, 1.25, 1.25, 1.25, 1.25, 1.26, 1.26, 1.26, 1.26, 1.30,
              1.30, 1.30, 1.30, 1.22, 1.22, 1.22, 1.22, 1.04, 1.04, 1.04, 1.04,
              0.99, 0.99, 0.99, 0.99, 0.98, 0.98, 0.98, 0.98, 0.96, 0.96, 0.96,
              0.96, 0.93, 0.93, 0.93, 0.93, 0.98, 0.98, 0.98, 0.98, 1.00, 1.00,
              1.00, 1.00, 1.00, 1.00)
  expect_equal(result$ScalingFactor, target, tolerance = 1e-2)

})

test_that("calcUnconstrainedCVM calculates the unconstrained CVM", {
  testData <- getChainTestData() %>%
    calcScalingFactor(2016)

  result <- calcUnconstrainedCVM(testData, 2016)
  target <- c(3076.70, 3037.39, 3051.27, 3503.96, 3303.52, 2967.51, 3045.24,
              3650.81, 3217.56, 2803.51, 3128.11, 3229.73, 3185.04, 2927.83,
              3095.37, 3415.20, 3154.54, 2699.91, 2892.98, 3291.95, 2716.45,
              2861.01, 2963.55, 3538.48, 3425.83, 2819.86, 3028.51, 3043.51,
              3264.16, 2643.62, 2674.61, 3146.24, 2458.84, 2538.53, 2684.98,
              3005.91, 2735.26, 2510.42, 2708.48, 3227.89, 3153.48, 3094.58,
              3277.80, 3756.96, 3034.52, 2835.07, 2908.98, 3193.39, 3003.85,
              2323.10, 2360.53, 2669.21, 2279.26, 2167.97, 2501.41, 2781.96,
              2693.22, 2761.39, 3168.50, 3468.73, 3234.95, 2842.98, 3071.81,
              3185.57, 3060.50, 2865.02, 3234.91, 3282.64, 3064.35, 3082.87,
              3331.84, 3194.69, 2607.78, 2624.25, 2884.70, 2877.82, 2586.42,
              2606.71, 2838.45, 3143.20, 2676.98)

  expect_equal(result$UnconstrainedCVM, target, tolerance = 1e-2)
})

test_that("calcAnnualCVMFlow calculates the annual cvm for flows", {
  testData <- getChainTestData()

  result <- data.frame(calcAnnualCVMFlow(testData, 2016))
  target <- c(13028.21, 13334.42, 12729.59, 12981.04, 12380.43, 12421.68,
              12666.64, 12060.89, 10991.04, 11498.82, 13659.09, 12311.10,
              10650.08, 10006.25, 12434.38, 12684.74, 12795.56, 13032.77,
              11306.00, 11174.77)
  expect_equal(result$AnnualCVM, target, tolerance=1e-2)
})

test_that("calcAnnualCVMFlow calculates the annual cvm for flows when PYP/CYP are zero", {
  testData <- getChainTestData()
  testData[testData$Year == 2000, c("CYP", "PYP")] <- 0

  result <- data.frame(calcAnnualCVMFlow(testData, 2016))
  target <- c(12590.48, 12886.4, 12301.9, 0, 12380.43, 12421.68,
              12666.64, 12060.89, 10991.04, 11498.82, 13659.09, 12311.10,
              10650.08, 10006.25, 12434.38, 12684.74, 12795.56, 13032.77,
              11306.00, 11174.77)
  expect_equal(result$AnnualCVM, target, tolerance=1e-2)
})

test_that("calcAnnualCVMStock calculates the annual cvm for stocks", {
  testData <- getChainTestData()

  result <- data.frame(calcAnnualCVMStock(testData, 2016))
  target <- c(3603.22, 3754.23, 3321.22, 3512.0, 3385.20, 3638.72, 3129.73,
              3235.37, 3091.06, 3319.33, 3863.39, 3283.85, 2744.83, 2860.77,
              3566.99, 3275.81, 3375.63, 3285.19, 2959.35, 3143.20)
  expect_equal(result$AnnualCVM, target, tolerance=1e-2)
})

test_that("calcAnnualCVMStock calculates the annual cvm for stocks when PYP/CYP are zero", {
  testData <- getChainTestData()
  testData[testData$Year == 2000, c("CYP", "PYP")] <- 0

  result <- data.frame(calcAnnualCVMStock(testData, 2016))
  target <- c(3482.16, 3628.09, 3209.64, 0, 3385.20, 3638.72, 3129.73,
              3235.37, 3091.06, 3319.33, 3863.39, 3283.85, 2744.83, 2860.77,
              3566.99, 3275.81, 3375.63, 3285.19, 2959.35, 3143.20)
  expect_equal(result$AnnualCVM, target, tolerance=1e-2)
})

test_that("chain calculates the constrained chained volume measure", {
  testData <- getChainTestData()

  result <- chain(testData, 2016, chainType = "Stock")
  target <- c(3398.11579, 3437.127, 3504.23516, 4073.39071, 3864.02778, 3459.46726,
              3526.95797, 4166.45074, 3508.98017, 2991.92684, 3322.66804, 3461.32488,
              3502.56766, 3255.2652, 3463.0825, 3826.86533, 3506.69597, 2997.85217,
              3247.33727, 3788.92052, 3269.07372, 3504.75675, 3612.68447, 4168.35767,
              3618.60086, 2831.61258, 2997.56925, 3071.12588, 3496.55942, 2903.21207,
              2983.78779, 3557.91588, 2805.98219, 2920.10685, 3115.47615, 3522.66543,
              3232.29875, 2974.01564, 3222.15793, 3848.85057, 3710.83139, 3612.39701,
              3806.08317, 4324.23893, 3400.22199, 3124.37963, 3170.47474, 3440.31341,
              3167.18333, 2433.29786, 2491.26874, 2887.5634, 2593.84707, 2532.06243,
              2976.4252, 3340.74581, 3217.83087, 3287.14082, 3749.53275, 4013.46337,
              3523.56753, 3005.96243, 3216.8389, 3356.86739, 3304.92769, 3122.29002,
              3531.20794, 3544.09317, 3210.12737, 3185.65998, 3432.86875, 3312.08755,
              2753.31572, 2804.99224, 3120.72707, 3158.35423, 2905.10901, 2954.62768,
              3214.35022, 3498.69399, 3132.98429)
  expect_equal(as.vector(result$CVM), target, tolerance = 1e-6)

  result <- data.frame(chain(testData, 2016, chainType = "Flow"))
  target <- c(3145.962559, 3122.34968, 3144.79221, 3615.104217,
              3400.402686, 3049.900527, 3129.311893, 3754.80003, 3309.126089,
              2881.731318, 3216.743289, 3321.986109, 3275.059237, 3009.407708,
              3182.775506, 3513.793618, 3245.144139, 2775.276699, 2974.26893,
              3385.744769, 2791.192667, 2940.119539, 3047.563346, 3642.802595,
              3525.565661, 2898.615291, 3113.073146, 3129.386746, 3358.431407,
              2717.533814, 2749.169139, 3235.751944, 2527.149048, 2609.345636,
              2761.406637, 3093.138824, 2812.508943, 2579.950191, 2784.829792,
              3321.528554, 3242.067979, 3180.664368, 3370.499386, 3865.862902,
              3120.603098, 2914.081011, 2990.760259, 3285.657908, 3092.031098,
              2388.121158, 2425.842019, 2744.084413, 2342.437736, 2228.037016,
              2573.006799, 2862.768086, 2767.643248, 2837.529232, 3258.948204,
              3570.263642, 3328.121822, 2922.565503, 3158.243449, 3275.808631,
              3146.712545, 2945.223478, 3327.2961, 3376.326567, 3149.312768,
              3168.346588, 3426.717377, 3288.392796, 2689.242655, 2706.127147,
              2968.631003, 2942.001203, 2606.323, 2607.669906, 2829.74677,
              3131.033595, 2955.624207)
  expect_equal(as.vector(result$CVM), target, tolerance=1e-6)
})


test_that("chain calculates the constrained chained volume measure when PYP/CYP are zero", {
  testData <- tribble(
    ~Year, ~CYP, ~PYP, ~Quarter,
    1997, 1.0, NA,  1,
    1997, 1.0, NA,  2,
    1997, 1.0, NA,  3,
    1997, 1.0, NA,  4,
    1998, 0, 0, 1,
    1998, 0, 0, 2,
    1998, 0, 0, 3,
    1998, 0, 0, 4,
    1999, 1.0, 1.0, 1,
    1999, 1.0, 1.0, 2,
    1999, 1.0, 1.0, 3,
    1999, 1.0, 1.0, 4)
  result <- chain(testData, 1999, chainType = "Stock")
  target <- c(1, 1, 1, 1,
              0, 0, 0, 0,
              1, 1, 1, 1)
  expect_equal(as.vector(result$CVM), target, tolerance=1e-4)
})


test_that("robustChain correctly deals with leading NA values in CYP.", {
  # This is common in metrics for *changes* in some quantity where the first
  # CYP year is usually NA where the *change* cannot be known.
  testData <- tribble(
    ~Year, ~CYP, ~PYP, ~Quarter,
    1997, NA, NA,  1,
    1997, NA, NA,  2,
    1997, NA, NA,  3,
    1997, NA, NA,  4,
    1998, 1.0, NA, 1,
    1998, 1.0, NA, 2,
    1998, 1.0, NA, 3,
    1998, 1.0, NA, 4,
    1999, 1.1, 1.0, 1,
    1999, 1.1, 1.0, 2,
    1999, 1.1, 1.0, 3,
    1999, 1.1, 1.0, 4,
    2000, 1.1, 1.0, 1,
    2000, 1.1, 1.0, 2,
    2000, 1.1, 1.0, 3,
    2000, 1.1, 1.0, 4)
  result <- robustChain(testData, 2000, chainType = "Stock")
  target <- c(NA, NA, NA, NA,
              1.187, 1.2087, 1.2196, 1.2248,
              1.2268, 1.2223,  1.2093, 1.1817,
              1.1267, 1.0996, 1.0878, 1.0858)
  expect_equal(as.vector(result$CVM), target, tolerance = 1e-4)
})

test_that("robustChain correctly deals with a series of all zero CYP.", {
  # This is common in metrics for like "Adjustments" which are not always applied
  testData <- tribble(
    ~Year, ~CYP, ~PYP, ~Quarter,
    1997, 0, 0, 1,
    1997, 0, 0, 2,
    1997, 0, 0, 3,
    1997, 0, 0, 4,
    1998, 0, 0, 1,
    1998, 0, 0, 2,
    1998, 0, 0, 3,
    1998, 0, 0, 4,
    1999, 0, 0, 1,
    1999, 0, 0, 2,
    1999, 0, 0, 3,
    1999, 0, 0, 4,
    2000, 0, 0, 1,
    2000, 0, 0, 2,
    2000, 0, 0, 3,
    2000, 0, 0, 4)
  result <- robustChain(testData, 2000, chainType = "Stock")
  target <- rep(0, nrow(testData))
  expect_equal(as.vector(result$CVM), target, tolerance = 1e-4)
})


test_that("chain throws an error for incorrect input", {
  testData <- data.frame()

  expect_error(chain(testData, 2016, chainType="Error"))

  testData <- data.frame(Year = c(1, 2, 3), PYP = c(1, 2, 3),
                         ThisIsCYP = c(1, 2, 3))
  expect_error(chain(testData, 2016))

  testData <- data.frame(Year = c(1, 2, 3), ThisIsPYP = c(1, 2, 3),
                         CYP = c(1, 2, 3))
  expect_error(chain(testData, 2016))

  testData <- data.frame(Year = c(1, 2, 3), PYP = c(1, 2, 3), CYP = c(1, 2, 3))
  expect_error(chain(testData, 0))

  testData <- data.frame(Year = 1:4, PYP = c(1:4), CYP = c(NA, 1:3))
  expect_error(chain(testData, 4), "CYP values must not be NA")

})

