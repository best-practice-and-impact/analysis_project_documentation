#' Calculate the survival values for each vintage given
#' life length parameters and retirement distribution
#'
#' @param lifeLengths data.frame with life length mean (Average),
#' coefficient of variation (CoV), minimum (Min) and maximum (Max) columns
#' @param config configuration
#'
#' @return data.frame with the survival values as a list-column (values),
#' a vintage id (vintageId) and an id for each unique combination of the
#' parameters (id)
#' @export
#'
#' @examples
#' ll <- data.frame(Min=2, Max=15, Average=7, CoV=3/7)
#' config <- list(retirementDist="pnorm", rightTruncate=FALSE,
#' offSet=0)
#' calcSurvivalValues(ll, config)
calcSurvivalValues <- function(lifeLengths, config){
  result <- NULL

  # check inputs
  # lifelengths must have mean, cov, min and max
  lifeLengths <- lifeLengths %>%
    assertr::verify("Average" %in% colnames(.)) %>%
    assertr::verify("CoV" %in% colnames(.)) %>%
    assertr::verify("Min" %in% colnames(.)) %>%
    assertr::verify("Max" %in% colnames(.)) %>%
    assertr::assert(is.numeric, Average, CoV, Min, Max) %>%  # all values should be numeric
    assertr::assert(assertr::not_na, Average, CoV, Min, Max) %>% # no NA's in any column
    dplyr::select(Min, Max, Average, CoV)

  maxLifeLength <- max(lifeLengths$Max)

  result <- lifeLengths %>%
    # determine unique pairs
    dplyr::distinct() %>%
    dplyr::mutate(id = row_number()) %>%
    # find survival values for each unique pair
    purrr::by_row(~ getSurvivalValues(.x$Min, .x$Max, .x$Average, .x$CoV,
                                       config$retirementDist,
                                       config$rightTruncate,
                                       config$offSet,
                                       maxLifeLength),
                  .to="values")

  # convert unique pairs back to dimension of lifelengths
  result <- suppressMessages(dplyr::right_join(result, lifeLengths)) %>%
    # drop redundant columns
    dplyr::select(-Average, -CoV, -Min, -Max) %>%
    dplyr::mutate(vintageId = row_number())

  return(result)
}

#' Calculate the survival values given a combination of parameters
#'
#' @param mininum the minimum life length of an asset
#' @param maximum the maximum life length of an asset
#' @param average average life length of an asset
#' @param CoV coefficient of variation of the retirement distribution of an asset
#' @param dist the retirement distribution, one of (pnorm, plnorm, pgamma, pweibull)
#' @param rightTruncate logical, if FALSE the distibution is not right truncated
#' @param offSet a real number between [-1, 1] indicating the point at which
#' the survival function should be evaluated
#' @param outputLength integer specifying the length of the output vector,
#' mainly to ensure survival vectors of different maximum life length to have
#' same length
#'
#' @return a vector of survival values with values between [0,1]
#'
#' @examples
#' getSurvivalValues(2, 15, 7, 3/7, "pnorm", FALSE, 0, 15)
getSurvivalValues <- function(minimum, maximum, average, CoV, dist,
                              rightTruncate, offSet, outputLength){
  # validation checks
  stopifnot(is.numeric(minimum))
  stopifnot(is.numeric(maximum))
  stopifnot(is.numeric(average))
  stopifnot(is.numeric(CoV))
  stopifnot(minimum <= maximum)
  stopifnot(is.logical(rightTruncate))
  stopifnot(offSet>=0 && offSet<=1)
  stopifnot(is.numeric(outputLength))
  stopifnot(outputLength >= maximum)

  # transform distribution parameters
  transformedParams <- switch(dist,
                              "pnorm"= transformNormalParameters(average, CoV),
                              "plnorm"= transformLogNormalParameters(average, CoV),
                              "pgamma"= transformGammaParameters(average, CoV),
                              "pweibull"= transformWeibullParameters(average, CoV),
                              "none"= transformNoneParameters())

  # if dist = "none" we will create a "none" distribution using punif
  # to create survival probs of all 1s
  none <- punif


  # create regular intervals
  periods <- minimum:maximum + offSet
  # apply the distribution function to the intervals
  # this gives us the cumulative probabilities (not the survival values)
  result <- mapply(dist, periods, MoreArgs = as.list(transformedParams))

  # truncate the survival probabilities, converting from cumulative probs to survival
  # if dist is "none" then skip this step since survival values are all ones
  if (dist != "none") {
    minProb <- do.call(dist, as.list(c(minimum, transformedParams)))
    maxProb <- do.call(dist, as.list(c(maximum, transformedParams)))
    if(rightTruncate){
      # doing both left and right truncation means we take the survival
      # probabilities and divide by the difference between the first and last value
      # we also use the last value to calculate the survival probabilities.
      # a different way of looking at this is that we're scaling the probabilities
      # to be within a [0,1] interval
      result <- (maxProb - result) / (maxProb - minProb)
    }else{
      # for left truncation calculate the survival probabilities and then divide
      # by the survival probability at the minimum period. This is to ensure
      # that the survival probability at the minimum period will equal 1.
      result <- (1 - result) / (1 - minProb)
    }
  }

  # finally we add the left truncated values (all 1) and right truncated
  # values (all zero's)
  result <- c(rep(1, minimum), result, rep(0, outputLength - maximum))
  result <- result[1:outputLength]
  return(result)
}


#' Returns the mean and standard deviation of a Normal distribution based on
#' the given parameters
#'
#' @param distMean the mean of a Normal distribution
#' @param distCOV the coefficient of variation of a Normal distribution
#'
#' @return a vector of length two with elements distMean and distSD
transformNormalParameters <- function(distMean, distCOV) {
  c(mean = distMean, sd = distMean * distCOV)
}

#' Returns the shape and rate parameters of a Gamma distribution based on the
#' given parameters
#'
#' @param distMean the mean of a Gamma distribution
#' @param distCOV the coefficient of variation of a Gamma distribution
#'
#' @return a vector of length two with elements shape and rate
transformGammaParameters <- function(distMean, distCOV) {
  c(shape = 1 / distCOV ^ 2,
    rate = 1 / (distMean * distCOV ^ 2))
}

#' Returns the mean and standard deviation of a Log Normal distribution based
#' on the given parameters
#'
#' @param distMean the mean of a Log Normal distribution
#' @param distCOV the coefficient of variation of a Log Normal distribution
#'
#' @return a vector of length two with elements meanLog and sdLog
transformLogNormalParameters <- function(distMean, distCOV) {
  var <- log(distCOV ^ 2 + 1)
  c(meanlog = log(distMean) - var / 2, sdlog = sqrt(var))
}

#' Returns the shape and scale parameters of a Weibull distribution based on
#' the given parameters
#'
#' @param distMean the mean of a Weibull distribution
#' @param distCOV the coefficient of variation of a Weibull distribution
#'
#' @return a vector of length two with elements shape and scale
transformWeibullParameters <- function(distMean, distCOV) {
  bounds <- c(0, 30)
  out <- c(0, 0)
  while (abs(out[2] - distCOV) >= 0.00001) {
    out <- transformWeibullParametersInverse(mean(bounds), distMean)
    bounds[as.numeric(out[2] - distCOV < 0) + 1] <- mean(bounds)
  }
  shape <- mean(bounds)

  bounds <- c(0, 90)
  while (abs(out[1] - distMean) >= 0.0001) {
    out <- transformWeibullParametersInverse(shape, mean(bounds))
    bounds[as.numeric(out[1] - distMean > 0) + 1] <- mean(bounds)
  }
  scale <- mean(bounds)
  c(shape=shape, scale=scale)
}

#' The inverse of transformWeibullParameter: for a given shape and scale this
#' gives the mean and coefficient of variation of the corresponding Weibull
#' distribution
#'
#' @param scale real value
#' @param shape real value
#'
#' @return a vector of length two with elements distMean and distCOV
transformWeibullParametersInverse <- function(scale, shape) {
  mu <- shape * gamma(1 + 1 / scale)
  sigsq <- shape ^ 2 * (gamma(1 + 2 / scale) - (gamma(1 + 1 / scale)) ^ 2)
  c(distMean = mu, distCOV = sqrt(sigsq) / mu)
}


#' Returns the parameters required to produce a "none" distribution from punif
#' This is a uniform distribution where min = 1 and max = 1
#'
#' @return a vector of length two with elements min = 1 and max = 1
transformNoneParameters <- function() {
 c(min = 1, max = 1)
}
