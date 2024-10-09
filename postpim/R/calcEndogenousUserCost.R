#' Calculate Endogenous User Cost
#' 
#' Calculates the Endogenous User Cost, typically using rates of return from Market sector totals.
#' 
#' Endogenous User Cost is calculated by
#' Consumption of Fixed Capital - Real Holidings gains/losses + Endogenous Return on Capital
#' where the Endogenous Return on Capital is calculated by
#' NetStock * Rate of Return
#' 
#' The Rate of Return is typically calculated using Market sector totals using \code{\link{calcRateOfReturn}}.
#' 
#' @param NetStock Vector of Net Stock.
#' @param ConsumptionOfFixedCapital Vector of Consumption of Fixed Capital.
#' @param RealHoldingGL Vector of Real Holding gains/losses.
#' @param RateOfReturn Vector of Rate of Return, typically estimated from Market sector.
#' 
#' @return Vector of endogenous user cost.
#' @export
calcEndogenousUserCost <- function(NetStock, ConsumptionOfFixedCapital, RealHoldingGL, RateOfReturn) {
  # Check arguments
  # All should be same length and numeric
  stopifnot(is.numeric(NetStock))
  stopifnot(is.numeric(ConsumptionOfFixedCapital))
  stopifnot(is.numeric(RealHoldingGL))
  stopifnot(is.numeric(RateOfReturn))
  stopifnot(all.equal(length(NetStock), length(ConsumptionOfFixedCapital), length(RealHoldingGL), length(RateOfReturn)))
  
  endogReturn <- NetStock * RateOfReturn
  endogUserCost <- ConsumptionOfFixedCapital - RealHoldingGL + endogReturn
  return(endogUserCost)
  
}


#' Calculate Rate of Return.
#' 
#' Calculates the Rate of Return, typically from Market sector totals.
#' 
#' Rate of Return is calculated by dividing the Return on Capital by \code{NetStock},
#' where Return on Capital is calculated by one of four methods:
#' 
#' Method 1: "Pure" return on capital. 
#' Gross Operating Surplus - (Consumption of Fixed Capital - Real holding gains/losses).
#' 
#' Method 2: No holding gains/losses.
#' Gross Operating Surplus - Consumption of Fixed Capital.
#' 
#' Method 3: Holding gains with zero ceiling.
#' As Method 1 but with Real holding gains/losses capped at zero.
#' 
#' Method 4: Holding gains smoothed with moving average.
#' As Method 1 but with Real holding gains/losses smoothed with moving average.
#' 
#' @param GrossOperatingSurplus Vector of Gross Operating Surplus, typically for the Market sector.
#' @param NetStock Vector of Net Stock, typically for the Market sector.
#' @param ConsumptionOfFixedCapital Vector of CFC, typically for the Market sector.
#' @param RealHoldingGL Vector of Real Holding gains/losses, typically for the Market sector.
#' @param method String denoting method to use, from "1" to "4", see details.
#' 
#' @return Vector of calculated rate of return.
#' @importFrom zoo rollapply
#' @export
calcRateOfReturn <- function(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, 
                             RealHoldingGL, method = c("1","2","3","4")) {
  # Check arguments
  method <- match.arg(method)
  # All should be same length and numeric
  stopifnot(is.numeric(GrossOperatingSurplus))
  stopifnot(is.numeric(NetStock))
  stopifnot(is.numeric(ConsumptionOfFixedCapital))
  stopifnot(is.numeric(RealHoldingGL))
  stopifnot(all.equal(length(GrossOperatingSurplus), length(NetStock), length(ConsumptionOfFixedCapital), length(RealHoldingGL)))
  
  args = list(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, RealHoldingGL)
  
  rateOfReturn <- switch(method,
                         "1" = do.call(calcRateOfReturn1, args),
                         "2" = do.call(calcRateOfReturn2, args),
                         "3" = do.call(calcRateOfReturn3, args),
                         "4" = do.call(calcRateOfReturn4, args))
  return(rateOfReturn)
  
}


# Calculate Rate of Return. Method 1: "Pure" return on capital. 
calcRateOfReturn1 <- function(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, RealHoldingGL) {
  
  returnOnCapital <- GrossOperatingSurplus - (ConsumptionOfFixedCapital - RealHoldingGL)
  rateOfReturn <- returnOnCapital / NetStock
  
  return(rateOfReturn)
}

# Calculate Rate of Return. Method 2: No holding gains/losses.
calcRateOfReturn2 <- function(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, RealHoldingGL) {
  
  returnOnCapital <- GrossOperatingSurplus - ConsumptionOfFixedCapital
  rateOfReturn <- returnOnCapital / NetStock
  
  return(rateOfReturn)
}

# Calculate Rate of Return. Method 3: Holding gains with zero ceiling.
calcRateOfReturn3 <- function(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, RealHoldingGL) {
  
  RealHoldingGLZeroCeiling <- pmin(RealHoldingGL, 0)
  returnOnCapital <- GrossOperatingSurplus - (ConsumptionOfFixedCapital - RealHoldingGLZeroCeiling)
  rateOfReturn <- returnOnCapital / NetStock
  
  return(rateOfReturn)
}

# Calculate Rate of Return. Method 4: Holding gains smoothed with moving average.
calcRateOfReturn4 <- function(GrossOperatingSurplus, NetStock, ConsumptionOfFixedCapital, RealHoldingGL) {
  RealHoldingGLSmoothed <- zoo::rollapply(RealHoldingGL, width = 5, FUN = mean, fill = "extend")
  returnOnCapital <- GrossOperatingSurplus - (ConsumptionOfFixedCapital - RealHoldingGLSmoothed)
  rateOfReturn <- returnOnCapital / NetStock
  
  return(rateOfReturn)
}
