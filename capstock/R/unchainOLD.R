#' Unchain Current Price Series.
#'
#' Unchain current price series into current year prices and previous year prices.
#'
#' CYP and PYP values are calculated from a deflator. If the deflatorMethod is
#' "implicit" a CVM column must be provided from which to calculate a deflator
#' (CP divided by CVM). If the deflatorMethod is "explicit" a deflator column
#' must be provided and its values are used directly.
#'
#' @param .data A data.frame with the current price, chained volume measures and
#' year information. The data.frame must also contain a Year column and optionally
#' a deflator column.
#' @param cpColumn The column name for the current price data ("CP" by default).
#' @param cvmColumn The column name for the CVM data ("CVM" by default). Can be ommitted if a deflator is provided.
#' @param deflatorColumn The column name for an optional deflator, see Details.
#' @param deflatorMethod String denoting deflator calculation method, "implicit"
#' or "explicit", see Details.
#'
#' @return The input data.frame with the addition of two columns (CYP and PYP).
#'
OLDunchain <- function(.data, cpColumn = "CP", cvmColumn = "CVM", deflatorColumn = NULL,
                    deflatorMethod = c("implicit", "explicit")){
  deflatorMethod <- match.arg(deflatorMethod)
  columns <- colnames(.data)

  stopifnot(cpColumn %in% columns)
  stopifnot("Year" %in% columns)
  # If deflatorMethod is implicit, we must have a CVM column from which to calculate
  if (deflatorMethod == "implicit") {
    stopifnot(cvmColumn %in% columns)
  } else {
    stopifnot(!is.null(deflatorColumn))
    stopifnot(deflatorColumn %in% columns)
  }

  result <- .data
  # Set the deflator, or calculate if CP and CVM provided
  if (deflatorMethod == "implicit") {
    result <- dplyr::mutate_(result, .deflator = paste0(cpColumn, "/", cvmColumn))
    # If cp/cvm results in invalid values, use a deflator of 1
    result <- dplyr::mutate(result, .deflator = dplyr::if_else(is.nan(.deflator) |
                                                                .deflator == 0 |
                                                                is.infinite(.deflator),
                                                              1, .deflator))
  }
  else {
    result <- dplyr::mutate_(result, .deflator = deflatorColumn)
  }

  # Group by year to calculate the average deflator
  result <- dplyr::group_by_(result, "Year")
  result <- dplyr::mutate(result, avgDeflator = mean(.deflator))
  result <- dplyr::ungroup(result)
  # Divide the deflator by the average deflator for current year prices
  # Divide the deflator by the average deflator of the previous year for previous year prices
  result <- dplyr::mutate(result,
                          deflatorPYP = .deflator/dplyr::lag(avgDeflator, 4),
                          deflatorCYP = .deflator/avgDeflator)
  # Now divide current prices with the deflators
  # The use of lazyeval::interp is necessary to mix a variable (cpColumn) with
  # a current (deflatorCYP), see vignette("nse")
  result <- dplyr::mutate_(result,
                           CYP = lazyeval::interp("x/deflatorCYP", x = as.name(cpColumn)),
                           PYP = lazyeval::interp("x/deflatorPYP", x = as.name(cpColumn)))
  # Remove interim calculations
  result <- dplyr::select(result, -.deflator, -deflatorCYP, -deflatorPYP, -avgDeflator)
  return(result)
}
