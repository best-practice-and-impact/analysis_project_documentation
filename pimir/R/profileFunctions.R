#' Constant profile function: asset retains full value (productive capacity)
#' until the point where it retires (given by T)
#'
#' @param numPeriods number of periods after which to evaluate value (productive capacity)
#' @param retirementAge The number of periods after which the value (productive capacity)
#' will be zero
#'
#' @return 1 if numPeriods < retirementAge and 0 otherwise
const <- function(numPeriods, retirementAge, dummy=NULL) {
  ifelse(numPeriods < retirementAge, 1, 0)
}


#' (Delayed) Linear profile function: value (productive capacity) of asset declines
#' linearly after a given number of periods (delay)
#'
#' @param numPeriods number of periods after which to evaluate value (productive capacity)
#' @param retirementAge The number of periods after which the value (productive capacity)
#' will be zero
#' @param delay the number of periods in which the asset retains its value (productive capacity)
#'
#' @return 1 if numPeriods < delay, value between [1,0] otherwise
lin <- function(numPeriods, retirementAge, delay=0) {
  if(is.null(delay)) delay=0
  ifelse(numPeriods < delay,
         1,
         (retirementAge - numPeriods) / (retirementAge - delay))
}


#' Geometric profile function: pattern of depreciation (deterioration) is geometric
#'
#' @param numPeriods number of periods after which to evaluate value (productive capacity)
#' @param retirementAge The number of periods after which the value (productive capacity)
#' will be zero
#' @param rateOfDecay rate at which asset depreciates (deteriorates)
#'
#' @return real value between [1, 0]
geom <- function(numPeriods, retirementAge, rateOfDecay){
  stopifnot(!is.null(rateOfDecay))
  # retirement age isn't used but needed to keep consistent with other functions
  (1-rateOfDecay)^numPeriods
}

#' Declining Balance profile function: similar to geometric decay but now rate of decay
#' represents the declining balance rate
#'
#' @param numPeriods number of periods after which to evaluate value (productive capacity)
#' @param retirementAge The number of periods after which the value (productive capacity)
#' will be zero
#' @param rateOfDecay rate at which asset depreciates (deteriorates)
#'
#' @return real value between [1, 0]
db <- function(numPeriods, retirementAge, rateOfDecay){
  stopifnot(!is.null(rateOfDecay))
  (1-rateOfDecay/retirementAge)^numPeriods
}

#' Hyperbolic profile function: pattern of depreciation (deterioration) is slow
#' at first and increasing with the number of periods
#'
#' @param numPeriods number of periods after which to evaluate value (productive capacity)
#' @param retirementAge The number of periods after which the value (productive capacity)
#' will be zero
#' @param shape value determining the shape of the profile, must be between [0,1]
#'
#' @return real value between [1, 0]
hyp <- function(numPeriods, retirementAge, shape){
  stopifnot(!is.null(shape))
  1-phyperform(numPeriods, retirementAge, shape)
}

#' Distribution function for a hyperbolic curve
#'
#' @param x real valued scalar
#' @param maximum the value at which the distribution is right-truncated
#' @param shape value determining the shape of the profile, must be between [0,1]
#'
#' @return real value between [0,1]
phyperform <- function(x, maximum, shape) {
  if(shape>=1 | shape<0){
    stop("'a' not in [0,1)")
  } else {
    x*(1-shape)/(maximum-shape*x)
  }
}

#' Density function for a hyperbolic curve
#'
#' @param x real valued scalar
#' @param maximum the value at which the distribution is right-truncated
#' @param shape value determining the shape of the profile, must be between [0,1]
#'
#' @return real value between [0,1]
dhyperform <- function(x, maximum, shape) {
  if(shape>=1 | shape<0) {
    stop("'a' not in [0,1)")
  } else {
    maximum*(1-shape)/(maximum-shape*x)^2
  }
}


