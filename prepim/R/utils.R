# Utility functions

#' @title Format Industry Codes
#'
#' @description Formats a character vector of industry codes into a standard format.
#'
#' @details Industry codes can lose their formatting in Excel, e.g. "01" becomes "1".
#' This function left-pads and single digit codes with "0" and ensures data type is character
#'
#' @param industryCodes vector of industry codes
#' @return character vector with single digits padded with leading zeros
#' @export
formatIndustryCodes <- function(industryCodes) {

  stopifnot(class(industryCodes) %in% c("numeric", "character"))

  # Convert to character
  industryCodes <- as.character(industryCodes)

  # Pad single digits with zero ("1" -> "01")
  padZero <- function(x) {
    if_else(grepl("^[0-9]$", x), paste0("0", x), x)
  }
  padZero(industryCodes)

  return(padZero(industryCodes))
}


#' Check Coverage
#'
#' Check two time series dataframes for coverage, based on the specified dimensions
#' @param timeseries1 data.frame in tidy format, typically containing Period, Value, and other categories such as Sector, Industry
#' @param timeseries2 data.frame in tidy format, typically containing Period, Value, and other categories such as Sector, Industry
#' @param dimensions The common column names between the two time series.
#' @import dplyr
#' @export
checkCoverage <- function(timeseries1, timeseries2, dimensions) {

  stopifnot("data.frame" %in% class(timeseries1))
  stopifnot("data.frame" %in% class(timeseries2))
  stopifnot("character" %in% class(dimensions))
  stopifnot(all(dimensions %in% colnames(timeseries1)))
  stopifnot(all(dimensions %in% colnames(timeseries2)))

  name1 <- deparse(substitute(timeseries1))
  name2 <- deparse(substitute(timeseries2))

  in1not2 <- dplyr::anti_join(timeseries1, timeseries2, by = dimensions) %>%
    mutate(info = paste("In", name1, "but not in", name2))
  in2not1 <- dplyr::anti_join(timeseries1, timeseries2, by = dimensions) %>%
    mutate(info = paste("In", name2, "but not in", name1))

  bind_rows(in1not2, in2not1)

}

#' Check Coverage of single dimension
#'
#' Check two time series dataframes for coverage, based on a single shared column and report back discrepancies
#' @param timeseries1 data.frame in tidy format, typically containing Period, Value, and other categories such as Sector, Industry
#' @param timeseries2 data.frame in tidy format, typically containing Period, Value, and other categories such as Sector, Industry
#' @param dimensions String of the common column name.
#' @return printout of differences in categores between dataframes.
#' @import dplyr
#' @export
checkDimCoverage <- function(df1, df2, dimension) {
  df1Name <- deparse(substitute(df1))
  df2Name <- deparse(substitute(df2))
  in1not2 <- paste(setdiff(df1[[dimension]], df2[[dimension]]), collapse = ", ")
  in2not1 <- paste(setdiff(df2[[dimension]], df1[[dimension]]), collapse = ", ")
  cat(paste0(dimension, " in ", df1Name, " but not ", df2Name, ": ", in1not2), "\n")
  cat(paste0(dimension, " in ", df2Name, " but not ", df1Name, ": ", in2not1), "\n")
}


#' Get Series
#'
#' Get the unique combinations of Sector, Industry, and Asset from a data.frame
#' @param .data data.frame in tidy format containing Sector, Industry, and Asset
#' @import dplyr
#' @export
getSeries <- function(.data) {
  .data <- dplyr::select(.data, Sector, Industry, Asset)
  .data <- dplyr::distinct(.data)
  return(.data)
}


#' @title Spline historic data from yearly to quarterly
#' @description Spreads out quantities from yearly amounts to quarterly amounts
#' @details The default method is simply to divide the yearly amount by four.
#' @param .data data.frame as created by \code{extractHistoric}
#' @param method string denoting which method to use to spread yearly Values
#' across four periods.
#' @return data.frame with Values spread from yearly Periods to Quarterly
#' @export
historicToQuarterly <- function(.data, method = "div4") {

  # Define basic splining function
  divideByFour <- function(df){
    splined <- rep(df$Value / 4, each = 4)
    quarts <- paste0("Q", 1:4)
    quartData <- data_frame(Period = paste0(rep(substr(df$Period, 1, 5), each = 4), quarts),
                            Value = splined)
    return(quartData)
  }

  # Pick the splining method
  if (method == "div4") {
    spliningMethod <- divideByFour
  } else {
    stop(paste("Method", method, "not implemented"))
  }

  # Call method to get quarterly data
  result <- .data %>%
    group_by(Industry, Sector, Asset, Prices) %>%
    do(spliningMethod(.)) %>%
    ungroup()

  return(result)
}
