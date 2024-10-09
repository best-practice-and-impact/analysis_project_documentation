#' Extract Configuration Specification
#'
#' Extract a configuration specification file and check its consistency.
#'
#' @details The configuration spec is expected to have Sector, Industry, Asset
#' columns, with additional columns for each configuration option.
#'
#' See \code{pimir::pimConfig} for the argument names and options. Columns must match
#' the argument names exactly.
#'
#' Specifications can include the word "ALL" to create default configurations,
#' e.g. Sector:ALL, Industry:ALL, Asset:TRANSPORT would define a specification
#' for TRANSPORT for all Sectors and Industries.
#'
#' @param path String path to a CSV file containing specification.
#' @return verified data.frame of specifications.
#' @export
extractConfigSpec <- function(path) {

  # Define standard column headings (remaining should be Periods)
  x <- readConfigFile(path)
  expectedCols <- c("Sector","Industry","Asset","Notes",
                    "profileType","profileFunctionName","retirementDistName",
                    "rightTruncate","combinationMethod","discountRate",
                    "inflationRate","offSet","profileFunctionParam")

  flog.info("Checking for expected column names.")
  if (chkColNames(x, expectedCols = expectedCols) == FALSE) {
    stop("Could not parse column names.")
  }

  flog.info("Checking for invalid combinations.")
  if (chkSpecCombinations(x) == FALSE) {
    stop("Invalid combinations.")
  }

  # Standarise Industry codings from e.g. "1" to "01"
  flog.info("Left-padding Industry codes with zero if required.")
  x <- mutate(x, Industry = formatIndustryCodes(Industry))

  return(x)
}

# Reads a specs file from disk using the predefined data types
readConfigFile <- function(path) {
  x <- readr::read_csv(path,
                       col_types = readr::cols(
                         .default = readr::col_character(),
                         discountRate = readr::col_number(),
                         inflationRate = readr::col_number(),
                         offSet = readr::col_number(),
                         profileFunctionParam = readr::col_number(),
                         rightTruncate = readr::col_logical()))
  return(x)
}




