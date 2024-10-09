#' Pre-process Input Data.
#'
#' Pre-process the time series of gfcf and other changes in volume. This includes
#' calculating GFCF and K-Value adjustments in CVM using the provided Price Index.
#'
#' @param timeSeries a data.frame with a column for GFCF in CP (gfcfCP), a Price
#' Index (PriceIndex), K.1 in CP (K1CP), K.3 in CP (K3CP), K.4 in CP (K4CP),
#' K.5 in CP (K5CP), K.61 in CP (K61CP) and K.62 in CP (K62CP)
#'
#' @return the given data.frame with the addition of CVM variables
#' @export
preProcess <- function(timeSeries){

  result <- timeSeries %>%
    # Calculate CVM values using provided PriceIndex
    dplyr::mutate_each(dplyr::funs(convertCPToCVM(., PriceIndex)),
                       # setNames ensures new vars are created instead of replacing existing
                       setNames(c(gfcfCP, K1CP, K3CP, K4CP, K5CP, K61CP, K62CP),
                                c("gfcfCVM", "K1CVM", "K3CVM", "K4CVM", "K5CVM",
                                  "K61CVM", "K62CVM"))) %>%
    # Sum GFCF and other changes in volume in CVM
    dplyr::mutate(gfcf_ociv = gfcfCVM + K1CVM + K3CVM + K4CVM + K5CVM +
                    K61CVM + K62CVM)

  return(result)
}
