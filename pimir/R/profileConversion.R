#' Convert a efficiency profile to a price (depreciation) profile
#'
#' @param profile a vector with values from the efficiency profile function
#' @param discountRate a real number between [0,1]
#' @param inflationRate a real number between [0,1]
#'
#' @return a vector with values from the price profile function
#' @export
#'
#' @examples
#' efficiencyProf <- seq(1, 0, -0.125)
#' efficiencyToPriceProfile(efficiencyProf[-1], 0.05, 0.02)
efficiencyToPriceProfile <- function(profile, discountRate, inflationRate) {
  # validation checks
  stopifnot(length(profile)>0)
  stopifnot(is.numeric(profile))
  stopifnot(discountRate>=0 && discountRate<=1)
  stopifnot(inflationRate>=0 && inflationRate<=1)

  # the first value of a profile represents the decrease after 1 full period
  # for the conversion however we need to start at 1 so we add this at the
  # beginning
  profile <- c(1, profile)
  maxAge <- length(profile)
  # calculate the compounded discount rates over time
  discountRate <- (1 + discountRate) ^ -seq_len(maxAge)
  # convert the values to a matrix
  # this will represent the discount rates over time of a new asset acquired
  # in the period as denoted by the column number
  # for example: dismat[9, 4] is the discount rate for a new asset acquired in
  # period 4 after 5 periods
  dismat <- suppressWarnings(matrix(c(discountRate, 0), maxAge, maxAge))
  # get the lower triangular values (there can be no values before an asset is acquired)
  dismat <- lower.tri(dismat, diag = TRUE) * dismat
  # calculate the compounded inflation rate
  # there is no inflation in the first period so we shift the exponent by 1 period
  inflationRate <- (1 + inflationRate) ^ (0:(maxAge - 1))

  # calculate the asset price at the beginning of each period
  # this is the sum of the price of a _new_ asset over time adjusted for
  # efficiency, inflation and the discount rate
  assetPrice <- as.vector((profile * inflationRate) %*% dismat)
  # the price profile is then the asset price of a _new_ asset over time divided
  # by the price of an asset in the first year over time
  priceProfile <- assetPrice / (assetPrice[1] * inflationRate)
  # before returning the result we remove the 1 at the beginning
  return(priceProfile[-1])
}

#' Convert a price (depreciation)profile to a efficiency profile
#'
#' @param profile a vector with values from the price profile function
#' @param discountRate a real number between [0,1]
#' @param inflationRate a real number between [0,1]
#'
#' @return a vector with values from the price profile function
#' @export
#'
#' @examples
#' agePriceProfile <- c(1, 0.785041, 0.594306, 0.428507, 0.288378, 0.174675,
#' 0.088173, 0.029674, 0)
#' priceToEfficiencyProfile(agePriceProfile, 0.05, 0.02)
priceToEfficiencyProfile <- function(profile, discountRate, inflationRate){
  # validation checks
  stopifnot(length(profile)>0)
  stopifnot(is.numeric(profile))
  stopifnot(discountRate>=0 && discountRate<=1)
  stopifnot(inflationRate>=0 && inflationRate<=1)

  # the first value of a profile represents the decrease after 1 full period
  # for the conversion however we need to start at 1 so we add this at the
  # beginning
  profile <- c(1, profile, 0)
  # the price profile is the accumulation of the asset price adjusted for
  # the discount rate and efficiency. Efficiency included inflation so we must
  # subtract this from the price profile. What is left is the efficiency profile.
  efficiencyProfile <-(1 + discountRate) * profile[-length(profile)] -
    (1 + inflationRate) * profile[-1]

  # normalise the values before returning the result
  result <- efficiencyProfile / efficiencyProfile[1]
  # before returning the result we remove the 1 at the beginning
  return(result[-1])
}
