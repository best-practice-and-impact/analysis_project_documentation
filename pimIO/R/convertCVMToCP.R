#' Convert Prices from CVM to CP
#'
#' Convert data that is in chained volume measures prices to data that is in
#' current prices by multiplying with a price index
#'
#' @param cvmData one or more values in current prices
#' @param priceIndex one or more values for the price index
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' convertCVMToCP(20, 0.69)
#' convertCVMToCP(10:20, rep(0.5, 10))
convertCVMToCP <- function(cvmData, priceIndex){
  stopifnot(is.numeric(cvmData))
  stopifnot(is.numeric(priceIndex))
  stopifnot(length(cvmData)==length(priceIndex))

  return(cvmData*priceIndex)
}
