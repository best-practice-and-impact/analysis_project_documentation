#' Calculate the net stock by correcting the given GFCF values for the
#' combined age-price and retirement distribution
#'
#' @param gfcf A numeric vector
#' @param survivalValues A data.frame with the following fields: id, vintageId
#' and values (a vector of survival values)
#' @param config a configuration list
#'
#' @return A numeric vector
#' @export
calcNetStock <- function(gfcf, survivalValues, config){
  profileConversionFunction <- NULL

  # Net Stock is calculated with an age-price profile
  # if the given profile (in config) is an age-efficiency profile this
  # needs to be converted. Unless it's a geometric age-efficiency profile, in
  # which case there is no conversion.
  if (config$profileType == "age-efficiency" &
     !identical(config$profileFunction, geom) &
     !identical(config$profileFunction, db)) {
    profileConversionFunction = efficiencyToPriceProfile
  }

  return(calcStock(gfcf, survivalValues, config, profileConversionFunction))
}
