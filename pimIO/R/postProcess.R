#' Post-process PIM results.
#'
#' Post-process the time series of pim results. This includes converting from
#' CP to CVM and calculating consumption of fixed capital, holding gains/losses
#' and capital services
#'
#' @param timeSeries The processed data as obtained from preProcess
#' @param pimResults The results from executing the pim function on the
#' processed data
#' @param referenceYear a numeric value indicating the year against which all prices are set
#' @param discountRate a numeric value between [0,1]
#'
#' @return the given data frames combined with the addition of numerous columns
#' @export
postProcess <- function(timeSeries, pimResults, referenceYear, discountRate){
  result <- timeSeries %>%
    dplyr::bind_cols(pimResults) %>%
    # compute total changes in volume
    # compute total other changes in volume
    # compute consumption of fixed capital
    dplyr::mutate(TotalChangesInVolumeCVM = NetStock - dplyr::lag(NetStock),
           TotalOtherChangesInVolumeCVM = K1CVM + K3CVM + K4CVM + K5CVM +
             K61CVM + K62CVM,
           TotalOtherChangesInVolumeCP = K1CP + K3CP + K4CP + K5CP + K61CP + K62CP,
           ConsumptionOfFixedCapitalCVM = gfcfCVM + TotalOtherChangesInVolumeCVM -
             TotalChangesInVolumeCVM,
           NetFixedCapitalFormationCVM = gfcfCVM - ConsumptionOfFixedCapitalCVM) %>%
    # adjust pim results to get values in cp
    dplyr::mutate_each(dplyr::funs(convertCVMToCP(., PriceIndex)),
                setNames(c(GrossStock, NetStock, ProductiveStock,
                         TotalChangesInVolumeCVM, ConsumptionOfFixedCapitalCVM,
                         NetFixedCapitalFormationCVM),
                         c("GrossStockCP", "NetStockCP",
                           "ProductiveStockCP", "TotalChangesInVolumeCP",
                           "ConsumptionOfFixedCapitalCP",
                           "NetFixedCapitalFormationCP"))) %>%
    # calculate various holding gains/losses
    addHoldingGainsLosses() %>%
    # capital services
    addCapitalServices(referenceYear, discountRate)

  return(result)
}

#' Calculate nominal/real/neutral holding gains/losses
#'
#' @param capStockData data.frame with net stock in CP (NetStockCP), net fixed
#' capital formation in CP (NetFixedCapitalFormationCP), total other changes in
#' volume in CP (TotalOtherChangesInVolumeCP), the price index (PriceIndex) and
#' the \% growth in consumer prices index/100 (CPI)
#'
#' @return the given data frame with the addition of nominal/real/neutral
#' holding gains/losses
addHoldingGainsLosses <- function(capStockData){
  result <- capStockData %>%
    # nominal holding gains/losses
    dplyr::mutate(NominalHoldingGL = NetStockCP - ((dplyr::lag(NetStockCP) +
                                            NetFixedCapitalFormationCP +
                                            TotalOtherChangesInVolumeCP)),
    # real holding gains/losses
           RealHoldingGL = (((PriceIndex/dplyr::lag(PriceIndex))-1)-CPI) * NetStockCP,
    # neutral holding gains/losses
           NeutralHoldingGL = NominalHoldingGL - RealHoldingGL)

  return(result)
}

#' Calculate the capital services, both with user costs in CP and with mid
#' period productive stock in reference year user cost prices. A by-product is
#' the return to capital.
#'
#' @param capStockdata data.frame with productive stock in CVM (ProductiveStock),
#'  the price index (PriceIndex), tax adjustment factor (TaxAdjustmentFactor),
#'  productive stock utilisation rate (UTIL),
#'  consumption of fixed capital in cp (ConsumptionOFixedCapitalCP) and
#'  net stock in CVM (NetStock)
#' @param referenceYear a numeric value indicating the year against which all prices are set
#' @param discountRate a numeric value between [0,1]
#'
#' @return the given data.frame with the addition of return to capital, capital
#' services in CP and capital service in CVM
addCapitalServices <- function(capStockData, referenceYear, discountRate){
  # first find the values for the reference year in our data set
  # first value is taken if there are multiple values
  refYearIndex <- which(capStockData$Vintage == referenceYear)[1]
  # error if not found
  stopifnot(!is.na(refYearIndex))

  # calculate the capital services denominator based on the reference year
  csDenominator <- mean(capStockData$ProductiveStock[(refYearIndex-1):refYearIndex])*
    capStockData$UTIL[refYearIndex]

  # calculate capital services with user costs in CP
  result <- capStockData %>%
    dplyr::mutate(ReturnToCapital = NetStockCP * discountRate,
      CapitalServicesCP = (ConsumptionOfFixedCapitalCP - RealHoldingGL +
                             ReturnToCapital) * TaxAdjustmentFactor)

  # update the denominator now that we have capital services in CP
  csCPRefYear <- result$CapitalServicesCP[refYearIndex]
  # calculate capital services using productive stock
  result <- result %>%
    dplyr::mutate(CapitalServicesCVM =
                    (((0.5*ProductiveStock + 0.5*dplyr::lag(ProductiveStock))*UTIL)/
                    csDenominator)*csCPRefYear)

  return(result)
}
