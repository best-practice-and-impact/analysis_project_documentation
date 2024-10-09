#' Add Other Parameters to GFCF data.
#'
#' Add other parameters (TaxAdjustmentFactor, UTIL, CPI) to GFCF data.
#'
#' The other parameters are read from a CSV file, checked for coverage, and then left-joined to the GFCF data.frame using the Period column.
#'
#' @param gfcf data.frame of GFCF series containing Sector, Industry, Asset, Period, and Value columns.
#' @param paramsFile string denoting the path to the parameters CSV file containing Period, TaxAdjustmentFactor, UTIL, and CPI columns.
#' @export
#' @return data.frame with gfcf with a new "PriceIndex" column containing the deflators.
addOtherParams <- function(gfcf, paramsFile) {
  flog.info("Adding other parameters.")

  stopifnot("Period" %in% colnames(gfcf))

  # Extract data from parameters file
  params <- extractOtherParams(paramsFile)

  # Check coverage
  flog.info("Checking parameters data for missing coverage.")
  missingCoverage <- dplyr::anti_join(gfcf, params, by = "Period")
  missingCoverage <- dplyr::distinct(missingCoverage, Period)
  if (nrow(missingCoverage) > 0) {
    msg <- paste("Missing coverage in parameters data.")
    flog.fatal(paste(msg, "Missing Periods:"), data.frame(missingCoverage), capture = TRUE)
    stop(msg)
  }

  # Join parameters to gfcf
  flog.info("Joining parameters.")
  gfcf <- dplyr::left_join(gfcf, params, by = "Period")
  return(gfcf)
}


# internal: read parameters file and check for expected columns
extractOtherParams <- function(paramsFile) {

  params <- read_csv(paramsFile,
                     col_types = cols(
                       Period = col_character(),
                       TaxAdjustmentFactor = col_double(),
                       UTIL = col_double(),
                       CPI = col_double()))
  flog.info("Checking parameters file for expected column headings.")
  expectedCols <- c("Period", "TaxAdjustmentFactor", "UTIL", "CPI")
  if (!all(expectedCols %in% colnames(params))) {
    msg <- paste("Parameters file must contain columns", paste(expectedCols, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  return(params)
}
