context("Unchaining")
suppressWarnings(library(tibble))

targetCYP <- c(2514.645011,2482.514324,2493.860505,2863.850997,2524.24003,
            2267.49286,2326.884814,2789.600361,2567.564428,2237.15407,
            2496.183752,2577.275346,2629.969684,2417.591138,2555.931121,
            2820.02223,2563.415236,2193.974601,2350.870691,2675.071779,
            2158.325617,2273.181338,2354.660013,2811.460938,2780.126406,
            2288.366833,2457.689935,2469.866093,2620.938871,2122.681895,
            2147.566396,2526.257733,1962.527663,2026.128299,2143.020438,
            2399.165477,2178.772891,1999.678986,2157.447899,2571.181713,
            2422.568313,2377.325348,2518.071173,2886.176029,2482.970625,
            2319.776383,2380.252808,2612.962193,2883.145107,2229.74903,
            2265.676855,2561.956904,2294.966562,2182.910014,2518.65335,
            2801.136549,2737.154457,2806.435162,3220.181747,3525.309426,
            3372.560293,2963.914512,3202.479429,3321.078125,3299.006819,
            3088.292395,3487.009447,3538.456574,3122.572798,3141.438554,
            3395.148235,3255.384747,2607.778558,2624.248183,2884.697126,
            2877.824218,2586.417187,2606.712038,2838.448823,3143.195223,
            2961.491)
targetPYP <- c(NA,NA,NA,NA,2700.032833,2425.405309,2488.933431,2983.873355,
               2458.557736,2142.175046,2390.207547,2467.856375,2541.607367,
               2336.364364,2470.056369,2725.274485,2604.790797,2229.387095,
               2388.815614,2718.249564,2207.416305,2324.884397,2408.216288,
               2875.40706,2721.956922,2240.486594,2406.266893,2418.188286,
               2648.926811,2145.349152,2170.499384,2553.234612,1974.315491,
               2038.298142,2155.892388,2413.57595,2183.146177,2003.692791,
               2161.778382,2576.342653,2511.9081,2464.996658,2610.932927,
               2992.612801,2331.180813,2177.963017,2234.742377,2453.225692,
               2457.876299,1900.857255,1931.485665,2184.063902,2187.669319,
               2080.851784,2400.898014,2670.174193,2711.7866,2780.425213,
               3190.337208,3492.636974,3287.716214,2889.350806,3121.914103,
               3237.529192,3190.690322,2986.894299,3372.520248,3422.278208,
               3303.157407,3323.114207,3591.496425,3443.650136,2657.325515,
               2674.108058,2939.505447,2932.501957,2515.16744,2534.903216,
               2760.256194,3056.607543,2676.977354)

test_that("unchain calculates CYP and PYP using implied deflator", {
  testData <- readr::read_csv("../../inst/data/test_unchain.csv",
                              col_types = readr::cols(
                                Year = readr::col_integer(),
                                CP = readr::col_double(),
                                CVM = readr::col_double(),
                                Deflator = readr::col_skip()))  # Ignore Deflator column
  result <- unchain(testData,
                    cpColumn = "CP",
                    cvmColumn = "CVM",
                    deflatorColumn = "implied")
  expect_equal(dim(result), c(81, 5))
  expect_equal(result$CYP, targetCYP, tolerance = 1e-6)
  expect_equal(result$PYP, targetPYP, tolerance = 1e-6)
})

test_that("unchain calculates CYP and PYP from provided deflator", {
  testData <- readr::read_csv("../../inst/data/test_unchain.csv",
                              col_types = readr::cols(
                                Year = readr::col_integer(),
                                CP = readr::col_double(),
                                CVM = readr::col_skip(),  # Ignore CVM column
                                Deflator = readr::col_double()))
  result <- unchain(testData,
                    cpColumn = "CP",
                    deflatorColumn = "Deflator",
                    deflatorMethod = "explicit")
  expect_equal(dim(result), c(81, 5))
  expect_equal(result$CYP, targetCYP, tolerance = 1e-6)
  expect_equal(result$PYP, targetPYP, tolerance = 1e-6)

})

test_that("unchain calculates CYP and PYP when zero values present", {
  # Make quarterly dataset
  testData <- data.frame(Year = rep(c(1899, 1900, 1901), each = 4),
                         CP = c(5, 5, 5, 5,
                                0, 10, 10, 10,  # zero numerator
                                0, 11, 11, 11), # zero numer and denom
                         CVM = c(5, 5, 5, 5,
                                 0, 10, 10, 10, # zero numer and denom
                                 10, 10, 10, 0)) # zero denom
  result <- unchain(testData)
  targetCYP <- c(5, 5, 5, 5, 0, 10, 10, 10, 0, 10.5, 10.5, 11.55)
  targetPYP <- c(NA, NA, NA, NA, 0, 10, 10, 10, 0, 10, 10, 11)
  expect_equal(result$CYP, targetCYP)
  expect_equal(result$PYP, targetPYP)
})

test_that("unchain rejects invalid input combinations", {

  # Make quarterly dataset
  testData <- data.frame(Year = rep(c(1899, 1900, 1901), each = 4),
                         CP = c(5, 5, 5, 5,
                                10, 10, 10, 10,
                                11, 11, 11, 11),
                         CVM = c(5, 5, 5, 5,
                                 10, 10, 10, 10,
                                 10, 10, 10, 10))
  # If using "explicit" deflator, deflator column must exist
  expect_error(unchain(testData,
                       cpColumn = "CP",
                       cvmColumn = "CVM",
                       deflatorColumn = NULL,
                       deflatorMethod = "explicit"))

  # If implicit deflator, CVM column must exist
  expect_error(unchain(testData,
                       cpColumn = "CP",
                       cvmColumn = "NOT_PRESENT",
                       deflatorColumn = "implicit"))

  # CP column should always exist
  expect_error(unchain(testData,
                       cpColumn = "NOT_PRESENT",
                       cvmColumn = "CVM",
                       deflatorColumn = "Deflator",
                       deflatorMethod = "implicit"))
  expect_error(unchain(testData,
                       cpColumn = "NOT_PRESENT",
                       cvmColumn = "CVM",
                       deflatorColumn = "Deflator",
                       deflatorMethod = "explicit"))
})
