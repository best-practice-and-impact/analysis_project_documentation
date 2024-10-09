context("Post processing of PIM results")

test_that("postProcess processes the pim results", {
  processedData <- data.frame(Vintage=c(1990, 1991, 1992, 2004, 2005),
                      gfcfCP = c(225, 167, 222, 378, 457),
                      gfcfCVM = c(325.6, 214.7, 259.3, 374.3, 457),
                      K1CP = c(2, 0, 0, 0, 0), K3CP = c(0, 0, 0, -10, 0),
                      K4CP = c(0, 0, 0, 6, 0), K5CP = c(-20, -22, -10, -8, -30),
                      K61CP = 0, K62CP = 0,
                      PriceIndex = c(0.691032, 0.77783, 0.856151, 1.009885, 1),
                      K1CVM = c(2.89, 0, 0, 0, 0), K3CVM = c(0, 0, 0, -9.9, 0),
                      K4CVM = c(0, 0, 0, 5.94, 0),
                      K5CVM = c(-28.94, -28.28, -11.68, -7.92, -30),
                      K61CVM = 0, K62CVM = 0,
                      gfcf_ociv = c(299.55, 186.42, 247.62, 362.42, 427),
                      CPI = c(.04, .04, .04, .02, .02),
                      TaxAdjustmentFactor=rep(1.04, 5),
                      UTIL = c(.95, .95, 1, 1, 1)
                      )

  pimResults <- data.frame(GrossStock = c(2000, 2086.42, 2229.72, 4434.04, 4639.33),
                           ProductiveStock = c(1400, 1376.42, 1417.57, 2183.80, 2283.23),
                           NetStock = c(1200, 1146.42, 1164.75, 1667.1, 1760.68))

  result <- postProcess(processedData, pimResults, 2005, 0.05)
  # the following is obtained by executing dput(round(result,6))
  target <- structure(list(Vintage = c(1990, 1991, 1992, 2004, 2005),
                           gfcfCP = c(225, 167, 222, 378, 457),
                           gfcfCVM = c(325.6, 214.7, 259.3, 374.3, 457),
                           K1CP = c(2, 0, 0, 0, 0), K3CP = c(0, 0, 0, -10, 0),
                           K4CP = c(0, 0, 0, 6, 0),
                           K5CP = c(-20, -22, -10, -8, -30),
                           K61CP = c(0, 0, 0, 0, 0), K62CP = c(0, 0, 0, 0, 0),
                           PriceIndex = c(0.691032, 0.77783, 0.856151, 1.009885, 1),
                           K1CVM = c(2.89, 0, 0, 0, 0), K3CVM = c(0, 0, 0, -9.9, 0),
                           K4CVM = c(0, 0, 0, 5.94, 0),
                           K5CVM = c(-28.94, -28.28, -11.68, -7.92, -30),
                           K61CVM = c(0, 0, 0, 0, 0), K62CVM = c(0, 0, 0, 0, 0),
                           gfcf_ociv = c(299.55, 186.42, 247.62, 362.42, 427),
                           CPI = c(0.04, 0.04, 0.04, 0.02, 0.02),
                           TaxAdjustmentFactor = c(1.04, 1.04, 1.04, 1.04, 1.04),
                           UTIL = c(0.95, 0.95, 1, 1, 1),
                           GrossStock = c(2000, 2086.42, 2229.72, 4434.04, 4639.33),
                           ProductiveStock = c(1400, 1376.42, 1417.57, 2183.8, 2283.23),
                           NetStock = c(1200, 1146.42, 1164.75, 1667.1, 1760.68),
                           TotalChangesInVolumeCVM = c(NA, -53.58, 18.33, 502.35, 93.58),
                           TotalOtherChangesInVolumeCVM = c(-26.05, -28.28, -11.68, -11.88, -30),
                           TotalOtherChangesInVolumeCP = c(-18, -22, -10, -12, -30),
                           ConsumptionOfFixedCapitalCVM = c(NA, 240, 229.29, -139.93, 333.42),
                           NetFixedCapitalFormationCVM = c(NA, -25.3, 30.01, 514.23, 123.58),
                           GrossStockCP = c(1382.064, 1622.880069, 1908.977008, 4477.870485, 4639.33),
                           NetStockCP = c(829.2384,891.719869, 997.201877, 1683.579283, 1760.68),
                           ProductiveStockCP = c(967.4448, 1070.620769, 1213.653973, 2205.386863, 2283.23),
                           TotalChangesInVolumeCP = c(NA, -41.676131, 15.693248, 507.31573, 93.58),
                           ConsumptionOfFixedCapitalCP = c(NA, 186.6792, 196.306863, -141.313208, 333.42),
                           NetFixedCapitalFormationCP =c(NA, -19.679099, 25.693092, 519.313164, 123.58),
                           NominalHoldingGL = c(NA, 104.160568, 89.788917, 179.064243, -16.479283),
                           RealHoldingGL = c(NA, 76.336874, 60.521845, 268.638845, -52.447564),
                           NeutralHoldingGL = c(NA, 27.823694, 29.267072, -89.574602, 35.968281),
                           ReturnToCapital = c(41.46192, 44.585993, 49.860094, 84.178964, 88.034),
                           CapitalServicesCP = c(NA, 161.125452, 193.070916, -338.804012, 492.857627),
                           CapitalServicesCVM = c(NA, 291.012324, 308.267301, 397.347381, 492.857627)),
                      .Names = c("Vintage", "gfcfCP", "gfcfCVM", "K1CP", "K3CP",
                                 "K4CP", "K5CP", "K61CP", "K62CP", "PriceIndex",
                                 "K1CVM", "K3CVM", "K4CVM", "K5CVM", "K61CVM",
                                 "K62CVM", "gfcf_ociv", "CPI", "TaxAdjustmentFactor",
                                 "UTIL", "GrossStock", "ProductiveStock",
                                 "NetStock", "TotalChangesInVolumeCVM",
                                 "TotalOtherChangesInVolumeCVM", "TotalOtherChangesInVolumeCP",
                                 "ConsumptionOfFixedCapitalCVM", "NetFixedCapitalFormationCVM",
                                 "GrossStockCP", "NetStockCP", "ProductiveStockCP",
                                 "TotalChangesInVolumeCP", "ConsumptionOfFixedCapitalCP",
                                 "NetFixedCapitalFormationCP", "NominalHoldingGL",
                                 "RealHoldingGL", "NeutralHoldingGL",
                                 "ReturnToCapital", "CapitalServicesCP",
                                 "CapitalServicesCVM"),
                      row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))
  # need to convert to data frame for some reason
  # values are the same just changing the data structure
  expect_equal(as.data.frame(result), as.data.frame(target), tolerance=1e-6)
})

test_that("addHoldingGainsLosses calculates nominal/real/neutral holding
          gains/losses", {
  input <- data.frame(NetStockCP = c(829.24, 891.72, 997.2),
                      NetFixedCapitalFormationCP = c(NA, -19.68, 25.7),
                      TotalOtherChangesInVolumeCP = c(NA, -22, -10),
                      PriceIndex = c(0.691031941, 0.77782953, 0.856151176),
                      CPI = rep(0.04, 3))

  result <- addHoldingGainsLosses(input)
  target <- structure(list(NetStockCP = c(829.24, 891.72, 997.2),
                           NetFixedCapitalFormationCP = c(NA, -19.68, 25.7),
                           TotalOtherChangesInVolumeCP = c(NA, -22, -10),
                           PriceIndex = c(0.691032, 0.77783, 0.856151),
                           CPI = c(0.04, 0.04, 0.04),
                           NominalHoldingGL = c(NA, 104.16, 89.78),
                           RealHoldingGL = c(NA, 76.336364, 60.52262),
                           NeutralHoldingGL = c(NA, 27.823636, 29.25738)),
                      .Names = c("NetStockCP", "NetFixedCapitalFormationCP",
                                 "TotalOtherChangesInVolumeCP", "PriceIndex",
                                 "CPI", "NominalHoldingGL", "RealHoldingGL",
                                 "NeutralHoldingGL"),
                      row.names = c(NA, -3L), class = "data.frame")
  expect_equal(result, target, tolerance=1e-6)
})

test_that("addCapitalServices returns an error with incorrect input", {
  input <- data.frame(Vintage = c(1991, 1993, 2004, 2005),
                      PriceIndex = c(0.77783, 0.856151, 1.01, 1.05),
                      NetStockCP = rep(10, 4),
                      ProductiveStock = rep(10, 4),
                      ConsumptionOfFixedCapitalCP = rep(10, 4),
                      RealHoldingGL = rep(10, 4),
                      TaxAdjustmentFactor = rep(1, 4),
                      UTIL =rep(1, 4))
  expect_error(addCapitalServices(input, 1990, 0.5))
})

test_that("addCapitalServices calculates capital services", {
  input <- data.frame(Vintage = c(1990, 1991, 1992, 2004, 2005),
                      NetStockCP = c(829.24, 891.72, 997.2, 1683.58, 1760.68),
                      PriceIndex = c(0.691032, 0.77783, 0.856151, 1.01, 1.0),
                      ProductiveStock = c(1400, 1376.42, 1417.57,  2183.80, 2283.23),
                      ConsumptionOfFixedCapitalCP = c(NA, 186.68, 196.30, 329.40, 333.42),
                      RealHoldingGL = c(NA, 76.336364, 60.52262, -1.38, -52.45),
                      TaxAdjustmentFactor = rep(1.04, 5),
                      UTIL = c(0.95, 0.95, 1, 1, 1))

  result <- addCapitalServices(input, 2005, 0.05)

  target <- structure(list(Vintage = c(1990, 1991, 1992, 2004, 2005),
                           NetStockCP = c(829.24, 891.72, 997.2, 1683.58, 1760.68),
                           PriceIndex = c(0.691032, 0.77783, 0.856151, 1.01, 1),
                           ProductiveStock = c(1400, 1376.42, 1417.57, 2183.8, 2283.23),
                           ConsumptionOfFixedCapitalCP = c(NA, 186.68, 196.3, 329.4, 333.42),
                           RealHoldingGL = c(NA, 76.336364, 60.52262, -1.38, -52.45),
                           TaxAdjustmentFactor = c(1.04, 1.04, 1.04, 1.04, 1.04),
                           UTIL = c(0.95, 0.95, 1, 1, 1),
                           ReturnToCapital = c(41.462, 44.586, 49.86, 84.179, 88.034),
                           CapitalServicesCP = c(NA, 161.126821, 193.062875, 431.55736, 492.86016),
                           CapitalServicesCVM = c(NA, 291.01382, 308.268885, 397.349423, 492.86016)),
                      .Names = c("Vintage", "NetStockCP", "PriceIndex",
                                 "ProductiveStock", "ConsumptionOfFixedCapitalCP",
                                 "RealHoldingGL", "TaxAdjustmentFactor", "UTIL",
                                 "ReturnToCapital", "CapitalServicesCP",
                                 "CapitalServicesCVM"),
                      row.names = c(NA, -5L), class = "data.frame")
  expect_equal(result, target, tolerance=1e-6)

})

test_that("addCapitalServices calculates capital services with varying discountRates", {
  input <- data.frame(Vintage = c(1990, 1991, 1992, 2004, 2005),
                      NetStockCP = c(829.24, 891.72, 997.2, 1683.58, 1760.68),
                      PriceIndex = c(0.691032, 0.77783, 0.856151, 1.01, 1.0),
                      ProductiveStock = c(1400, 1376.42, 1417.57,  2183.80, 2283.23),
                      ConsumptionOfFixedCapitalCP = c(NA, 186.68, 196.30, 329.40, 333.42),
                      RealHoldingGL = c(NA, 76.336364, 60.52262, -1.38, -52.45),
                      TaxAdjustmentFactor = rep(1.04, 5),
                      UTIL = c(0.95, 0.95, 1, 1, 1))
  # Should return the same result when the same discount rate is specified
  # either as singular value or as a series
  discountRate <- 0.05
  resultSingular <- addCapitalServices(input, 2005, discountRate)
  discountRates <- rep(discountRate, nrow(input))
  resultMultiple <- addCapitalServices(input, 2005, discountRates)
  expect_equal(resultSingular, resultMultiple)

  # Should apply the different discount rates to each Vintage
  discountRates <- seq(0.05, 0.04, length.out = nrow(input))
  result <- addCapitalServices(input, 2005, discountRates)
  targetCp <- c(NA, 158.808, 187.877, 418.425, 474.549)
  targetCvm <- c(NA, 280.202, 296.816, 382.587, 474.549)
  expect_equal(result$CapitalServicesCP, targetCp, tolerance = 1e-4)
  expect_equal(result$CapitalServicesCVM, targetCvm, tolerance = 1e-4)
})
