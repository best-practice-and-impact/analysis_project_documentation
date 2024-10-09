#' Combines a given retirement distribution and individual asset price (or efficiency)
#' profile to give the price (or efficiency) profile of a cohort of assets.
#' Depending on the given combination method either the weighted average method
#' is used or the fixed profile method.
#'
#' @param survivalValues a vector of real numbers in the interval [0,1]
#' representing the survival function of a retirement distribution
#' @param config the configuration with the combination method, discount rate
#' inflation rate and any parameters for the profile function
#' @param profileConversionFunction one of (priceToEfficiencyProfile, efficiencyToPriceProfile)
#'
#' @return A vector of monotonically decreasing real numbers in the interval [0,1]
#' representing the price (or efficiency) profile of an asset vintage evaluated
#' at regular intervals of one period
#' @export
#'
#' @examples
#' values <- seq(1, 0, by=-0.1)
#' config <- list(combinationMethod="1")
#' calcCohortProfile(values, "lin", config)
calcCohortProfile <- function(survivalValues, config,
                              profileConversionFunction=NULL,
                              discountRate = NULL){

  # validation checks
  stopifnot(is.numeric(survivalValues))
  # if a discount rate is explicitly given, override the discount rate in the config
  if(!is.null(discountRate)) config$discountRate <- discountRate

  result <- switch(config$combinationMethod,
         "1"=cohortProfileOne(survivalValues, config$profileFunction,
                              profileConversionFunction, config$discountRate,
                              config$inflationRate,
                              config$profileFunctionParam),
         "2"=cohortProfileTwo(survivalValues, config$profileFunction,
                              profileConversionFunction, config$discountRate,
                              config$inflationRate,
                              config$profileFunctionParam))

  return(result)
}

#' Combines a given retirement distribution and individual asset price (or efficiency)
#' profile to give the price (or efficiency) profile of a cohort of assets. This version
#' of the function uses the weighted average method of combining the two inputs.
#'
#' @param survivalValues a vector of real numbers in the interval [0,1]
#' representing the survival function of a retirement distribution
#' @param profileFunction a functional form of a single asset price (or efficiency) profile
#' @param profileConversionFunction one of (priceToEfficiencyProfile, efficiencyToPriceProfile)
#' @param discountRate a real number between [0,1]
#' @param inflationRate a real number between [0,1]
#' @param profileFunctionParam any parameter to be passed on to the profile function
#'
#' @return A vector of monotonically decreasing real numbers in the interval [0,1]
#' representing the price (or efficiency) profile of an asset vintage evaluated
#' at regular intervals of one period
cohortProfileOne <- function(survivalValues,
                                      profileFunction,
                                      profileConversionFunction = NULL,
                                      discountRate = NULL, inflationRate = NULL,
                                      profileFunctionParam = 0){
  # find the index of the survival values where it is equal to 0 to find the maximum age
  Tmax <- match(0, c(survivalValues, 0))
  # and the index of the minimum age
  Tmin <- length(survivalValues) + 2 - match(1, rev(survivalValues))
  Trange <- seq.int(Tmin, Tmax)

  # apply the profile function to each combination of numberOfPeriods and retirement age
  # e.g. the price of an 1 period old asset that will be retired after 5 years
  # first generate the periods for each retirement age
  agePeriods <- lapply(Trange, function(x) seq.int(1, x - 1))
  # now pass the periods and the retirement age to the profile function
  profiles <- mapply(profileFunction, numPeriods = agePeriods, retirementAge = Trange,
                     MoreArgs = list(profileFunctionParam),
                     SIMPLIFY = FALSE)
  # to be able to rbind at the end the results must all be of the same length
  profilesRect <- lapply(profiles, function(x) c(x, rep(0, Tmax - length(x) - 1)))
  # finally rbind to get a matrix
  result <- do.call(rbind, profilesRect)

  # truncate the survival values to between Tmax and Tmin and calculate the
  # marginal probabilities
  # diff() takes future value (e.g. #2) and substracts the current value (e.g. #1)
  # as the survival values are a decreasing series this will give negative values
  # so we multiply by -1
  marginalProbs <- -1 * diff(c(survivalValues[Trange - 1], 0))

  # now multiply each column with the marginal probabilities and sum
  result <- colSums(result * marginalProbs)

  if(!is.null(profileConversionFunction)){
    # if a profile conversion function is given, apply it
    result <- profileConversionFunction(result, discountRate, inflationRate)
  }

  # extend if it's missing values at the right tail
  result <- c(result, rep(0, length(survivalValues) - (Tmax - 1)))

  return(result)
}


#' Combines a given retirement distribution and individual asset price (or efficiency)
#' profile to give the price (or efficiency) profile of a cohort of assets. This version
#' of the function uses the fixed profile method of combining the two inputs.
#'
#' @param survivalValues a vector of real numbers in the interval [0,1]
#' representing the survival function of a retirement distribution
#' @param profileFunction a functional form of a single asset price (or efficiency) profile
#' @param profileConversionFunction one of (priceToEfficiencyProfile, efficiencyToPriceProfile)
#' @param discountRate a real number between [0,1]
#' @param inflationRate a real number between [0,1]
#' @param profileFunctionParam any parameter to be passed on to the profile function
#'
#' @return A vector of monotonically decreasing real numbers in the interval [0,1]
#' representing the price (or efficiency) profile of an asset vintage evaluated
#' at regular intervals of one period
#' @examples
#' cohortProfileTwo(seq(1, 0, -0.1), "lin")
cohortProfileTwo <- function(survivalValues,
                             profileFunction,
                             profileConversionFunction = NULL,
                             discountRate = NULL, inflationRate = NULL,
                             profileFunctionParam = 0){
  # determine the maximum retirement age
  maxAge <- match(0, c(survivalValues, 0))
  numberOfPeriods <- length(survivalValues)
  # when not right truncated the survival rates may not end with a zero
  # the maximum age may then be larger than the number of periods

  # when the survival rates is padded with zero's beyond the maximum age
  # we need to make sure we only calculate profile values for the periods
  # before the maximum age
  if (maxAge < numberOfPeriods){
    numberOfPeriods <- maxAge
  }

  # apply the profile function for all the periods
  profileValues <- profileFunction(seq_len(numberOfPeriods), maxAge,
                                   profileFunctionParam)

  # multiply the profile values with the survival values (elementwise)
  # The series of survival values may extend beyond the maximum age so we
  # truncate it before multiplication and extend it below
  result <- profileValues * survivalValues[1:numberOfPeriods]

  # if a conversion function is given, apply it
  if(!is.null(profileConversionFunction)){
    profileValues <- profileConversionFunction(profileValues, discountRate, inflationRate)
  }

  if(length(result) < length(survivalValues)){
    result <- c(result, rep(0, length(survivalValues) - maxAge))
  }

  return(result)
}
