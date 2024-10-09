#' correct the given GFCF values for the combined age-efficiency and
#' retirement distribution.
#'
#' @param gfcf A numeric vector
#' @param survivalValues A data.frame with the following fields: id, vintageId
#' and values (a vector of survival values)
#' @param config a configuration list
#' @param profileConversionFunction one of (priceToEfficiencyProfile, efficiencyToPriceProfile)
#'
#' @return A numeric vector
calcStock <- function(gfcf, survivalValues, config, profileConversionFunction=NULL){

  if(length(config$discountRates)>0){
    stopifnot(length(config$discountRates)==nrow(survivalValues))
    survivalValues$discountRate <- config$discountRates
  }else if(is.null(config$discountRate)){
    survivalValues$discountRate <- 0
  }else {
    survivalValues$discountRate <- config$discountRate
  }

  # step 1: combine survival values with the age-efficiency profile
  cohortProfile <- purrr::by_row(survivalValues,
                                 # the actual values are in the "values" field of
                                 # the survivalValues data.frame. They're in a
                                 # list so we have to add [[1]] to select the
                                 # actual values
                                 ~ calcCohortProfile(.x$values[[1]], config,
                                                     profileConversionFunction,
                                                     .x$discountRate)) %>%
    dplyr::select(id, vintageId, .out) %>%
    dplyr::rename(values = .out)

  # step 2: call pim
  result <- pim(gfcf, cohortProfile)

  return(result)
}
