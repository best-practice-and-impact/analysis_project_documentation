#' Apply the Perpetual Inventory Method to a timeseries of GFCF to obtain a
#' timeseries of gross/productive/net stock
#'
#' @param gfcf A numeric vector
#' @param lifeLengths lifeLengths data.frame with life length mean (Average),
#' coefficient of variation (CoV), minimum (Min) and maximum (Max) columns
#' @param config a list of configuration parameters (use the pimConfig function)
#'
#' @return a data.frame containing the gross stock (GrossStock), net stock (NetStock) and
#' productive stock (ProductiveStock)
#' @export
run <- function(gfcf, lifeLengths, config){

  # calculate retirement survival values
  survivalValues <- calcSurvivalValues(lifeLengths, config)

  # calculate gross stock
  grossStock <- pim(gfcf, survivalValues)

  # calculate net stock
  netStock <- calcNetStock(gfcf, survivalValues, config)

  # calculate productive stock
  productiveStock <- calcProductiveStock(gfcf, survivalValues, config)

  return(data.frame(GrossStock=grossStock, NetStock=netStock,
              ProductiveStock=productiveStock))
}
