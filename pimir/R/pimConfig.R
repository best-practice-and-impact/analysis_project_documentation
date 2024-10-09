#' Configuration parameters for the calculation of gross/productive/net stock
#' using the perpetual inventory method
#'
#' @param profileType the type of profile: age-efficiency or age-price
#' @param profileFunctionName the name of a functional form of a single asset
#' price (or efficiency) profile. Options are linear (lin), geometric (geom),
#' constant (const), declining balance (db) or hyperbolic (hp)
#' @param retirementDistName the function name of the retirement distribution.
#' Options are Normal (pnorm), Weibull (pweibull), Log-Normal (plnorm),
#' Gamma (pgamma), or None (none)
#' @param rightTruncate logical value indicating if the retirement distribution
#' should be right truncated (TRUE) or not (FALSE)
#' @param combinationMethod the method employed to combine the age-efficieny/age-price
#' profile with the retirement distribution. This can either be the weighted
#' average ("1") or fixed profile ("2")
#' @param discountRate a real number between [0,1]
#' @param inflationRate a real number between [0,1]
#' @param offSet a real number between [0,1] representing the period at which
#' gfcf occurs and at which survival probabilities are evaluated
#' @param profileFunctionParam any parameters to be passed on to the profile function
#' @param discountRates a time series of discount rates with the same length as the
#' time series of GFCF. When set this value will take precedent over the single discount rate.
#'
#' @return a list with all the input values
#' @export
pimConfig <- function(profileType = c("age-efficiency", "age-price"),
                      profileFunctionName = c("lin", "geom", "const", "db", "hyp"),
                      retirementDistName = c("pnorm", "pweibull", "plnorm", "pgamma", "none"),
                      rightTruncate = FALSE,
                      combinationMethod = c("1", "2"),
                      discountRate = NULL, inflationRate = NULL, offSet = 0,
                      profileFunctionParam = 0,
                      discountRates = NULL){
  # validation checks
  profileType <- match.arg(profileType)
  profileFunctionName <- match.arg(profileFunctionName)
  combinationMethod <- match.arg(combinationMethod)
  retirementDistName <- match.arg(retirementDistName)
  stopifnot(is.logical(rightTruncate))
  stopifnot(is.null(discountRate) | is.numeric(discountRate))
  stopifnot(is.null(inflationRate) | is.numeric(inflationRate))
  stopifnot(is.numeric(offSet))
  stopifnot(is.numeric(profileFunctionParam))
  stopifnot(is.null(discountRate)  | (discountRate >= 0 && discountRate <= 1))
  stopifnot(is.null(inflationRate) | (inflationRate >= 0 && inflationRate <= 1))
  stopifnot(offSet >= 0 && offSet <= 1)
  if(!is.null(discountRates)){
    stopifnot(is.numeric(discountRates))
    stopifnot(sum(discountRates>=0 & discountRates<=1)==length(discountRates))
  }

  profileFunction <- get(profileFunctionName)

  list(profileType = profileType,
       profileFunction = profileFunction,
       combinationMethod = combinationMethod,
       retirementDistName = retirementDistName,
       rightTruncate = rightTruncate,
       discountRate = discountRate,
       inflationRate = inflationRate,
       offSet = offSet,
       profileFunctionParam = profileFunctionParam,
       discountRates = discountRates)
}
