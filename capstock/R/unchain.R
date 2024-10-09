#' Unchain Current Price Series.
#'
#' https://www.ons.gov.uk/economy/nationalaccounts/uksectoraccounts/methodologies/chainlinkingmethodsusedwithintheuknationalaccounts
#'
#' This function creates CYPs and PYPs for pairs of CP and 'CVM' series
#'
#' @param .data1 A data.frame with the current price, chained volume measures and
#' year information. The data.frame must also contain a Year column
#' @param cpColumn The column name for the current price data ("CP" by default).
#' @param cvmColumn The column name for the CVM data ("CVM" by default). Can be ommitted if a deflator is provided.
#' @import futile.logger
#' @return The input data.frame with the addition of two columns (CYP and PYP).
#' @export
unchain <- function(.data1, cpColumn = "CP", flow_stock = NULL, refYear = refYear){

  ####################################################################################################

  # Set CVM to CVM equivalent of CP column

  cvmColumn <- gsub('CP', 'CVM', cpColumn)

  #.data1 <- out[[3, "data"]]
  ####################################################################################################

  # Check necessary columns exist

  columns <- colnames(.data1)

  stopifnot(cpColumn %in% columns)
  stopifnot("Year" %in% columns)
  stopifnot(cvmColumn %in% columns)

  result <- .data1

  # Create new columns with CP & KP values

  result <- dplyr::mutate_(result, CP = paste0(cpColumn), KP = paste0(cvmColumn))
  result$CP[is.nan(result$CP)] <- 0
  result$KP[is.nan(result$KP)] <- 0
  result$CP[is.na(result$CP)] <- 0
  result$KP[is.na(result$KP)] <- 0

  # If flow sum quarters to get annual CP and KP values to produce deflator. If stock use Q4

  if (flow_stock == "Flow"){

    result <- dplyr::group_by_(result, "Year")
    result <- dplyr::mutate(result, CYPDeflator = ifelse(sum(KP)==0,mean(PriceIndex), sum(CP)/sum(KP)))
    result <- dplyr::ungroup(result)

  }  else {

    result <- dplyr::mutate(result, CPQ4 = ifelse(grepl("Q4",result$Period), CP,0),
                                    KPQ4 = ifelse(grepl("Q4",result$Period), KP,0))

    result <- dplyr::group_by_(result, "Year")
    result <- dplyr::mutate(result, CYPDeflator = ifelse(sum(KPQ4)==0,0,sum(CPQ4)/sum(KPQ4)))
    result <- dplyr::ungroup(result)
    result <- dplyr::select(result, -CPQ4, -KPQ4)

  }

  result[is.na(result)] <- 0

  #############################################################

  # Create PYP deflator by lagging CYP deflator

  result <- dplyr::mutate(result, PYPDeflator = lag(CYPDeflator,4))

  # Multiply KP by deflators

  result <- dplyr::mutate(result, CYP = KP*CYPDeflator,
                           PYP = KP*PYPDeflator)

  # Replace CYP/PYPs with KP after reference year

  result$PYP <- ifelse(as.numeric(result$Year)<=as.numeric(refYear), result$PYP, result$KP)
  result$CYP <- ifelse(as.numeric(result$Year)<=as.numeric(refYear), result$CYP, result$KP)

  # Replace nan with 0's for CYP

  result$CYP[is.nan(result$CYP)] <- 0

  # Remove deflators/CP/KP

  result <- dplyr::select(result, -CYPDeflator, -PYPDeflator,CP,KP)
  return(result)

}
