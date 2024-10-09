#' Calculate the productive stock by correcting the given GFCF values for the
#' combined age-efficiency and retirement distribution. This function is almost
#' identical to \code{\link{CalcNetStock}}. The duplication is deliberate in case
#' there might be Productive Stock specific changes in the future.
#'
#' @param gfcf A numeric vector
#' @param survivalValues A data.frame with the following fields: id, vintageId
#' and values (a vector of survival values)
#' @param config a configuration list
#'
#' @return A numeric vector
#' @export
calcProductiveStock <- function(gfcf, survivalValues, config){
  profileConversionFunction <- NULL

  # Productive Stock is calculated with an age-efficiency profile.
  # If the given profile (in config) is an age-price profile this
  # needs to be converted. Unless it's a geometric age-price profile, in
  # which case there is no conversion.
  if (config$profileType == "age-price" &
     !identical(config$profileFunction, geom) &
     !identical(config$profileFunction, db)) {
    profileConversionFunction = priceToEfficiencyProfile
  }

  return(calcStock(gfcf, survivalValues, config, profileConversionFunction))
}
