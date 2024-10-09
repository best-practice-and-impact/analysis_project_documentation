#' Convert Prices from CVM to CP
#'
#' Convert data that is current prices to data that is chained volume measures
#' by dividing by a price index
#'
#' @param cpData one or more values in current prices
#' @param priceIndex one or more values for the price index
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' convertCPToCVM(20, 0.69)
#' convertCPToCVM(10:20, rep(0.5, 10))
convertCPToCVM <- function(cpData, priceIndex){
  stopifnot(is.numeric(cpData))
  stopifnot(is.numeric(priceIndex))
  stopifnot(length(cpData)==length(priceIndex))

  return(cpData/priceIndex)
}
