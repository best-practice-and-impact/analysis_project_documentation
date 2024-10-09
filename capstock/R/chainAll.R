#' Chain all PYP/CYP pairs.
#'
#' Chain all PYP/CYP variables into CVM.
#'
#' All input variables for should end in "CP" or "CVM". The CVM output variable
#' names are created using the CP variable name and appending with "CVM".
#'
#' @section Specification of pairs data.frame:
#' The \code{pairs} data.frame associates the CYP and PYP variable names for each
#' output metric, e.g. associating GrossStockCYP with GrossStockPYP. The type
#' of chaining required (Stock or Flow) is also denoted. e.g.
#'
#' \tabular{rrr}{
#' PYP \tab CYP \tab ChainType \cr
#' GrossStockPYP \tab GrossStockCYP \tab Stock \cr
#' NetStockPYP \tab NetStockCYP \tab Stock \cr
#' ... \tab ... \tab ...
#' }
#'
#' @param .data A data.frame with one row per Sector/Asset/Industry with a "data" list-column
#' containing pairs of variables with previous and current year prices, and year information.
#' @param pairs A data.frame with PYP, CYP, and ChainType columns containing the variable name pairs for chaining.
#' If not provided it will created using any variable names ending with "PYP" or "CYP".
#' @return The input data.frame with an addional "chained" list-column with new CVM variables.
#' @import doSNOW parallel
#' @importFrom foreach foreach "%dopar%" "%do%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2
#' @export
chainAll <- function(.data, pairs = NULL, lastCompleteYear, parallelise = TRUE, benchType = 4) {

  # --- Input Checks -----------------------------------------------------------
  # .data must contain "data" list-column
  stopifnot("data" %in% colnames(.data))
  # Get the column names from the embedded data list-column
  dataCols <- colnames(.data$data[[1]])

  # Pairs must have colnames of PYP and CYP
  pairCols <- colnames(pairs)
  stopifnot("PYP" %in% pairCols)
  stopifnot("CYP" %in% pairCols)
  stopifnot("ChainType" %in% pairCols)

  # The data list-column within .data must contain all the defined pairs
  stopifnot(all(pairs$PYP %in% dataCols))
  stopifnot(all(pairs$CYP %in% dataCols))
  # The data list-column must contain a Year column
  stopifnot("Year" %in% dataCols)

  # --- Set up parallel processing ---------------------------------------------
  `%doOrDoPar%` <- `%do%`  # define default iterator to be sequential
  # Set up parallel processing for multiple cores (use all except one)
  if (parallelise) {
    `%doOrDoPar%` <- `%dopar%`
    availCores <- parallel::detectCores()
    nCores <- dplyr::case_when(
      is.na(availCores) ~ 1,
      availCores > 1 ~ availCores - 1,
      TRUE ~ 1)
    # Register the cluster
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    # Pass libPath to workers
    clusterCall(cl, function(x) .libPaths(x), .libPaths())
  }

  # --- Chain ------------------------------------------------------------------
  # Set up a progress bar
  pb <- txtProgressBar(min = 0, max = nrow(.data), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  # Create options list for foreach
  opts <- list(progress = progress)
  # .data <- unnest(.data)
  x <- dplyr::mutate(.data, chained =
                       foreach(i = seq_len(nrow(.data)),
                               .packages = c("dplyr", "capstock"),
                               .errorhandling = "pass",
                               .options.snow = opts) %doOrDoPar%
                       dplyr::bind_cols(
                         foreach(p = seq_len(nrow(pairs))) %do%
                           chainRename(.data = .data[[i, "data"]],
                                       lastCompleteYear = lastCompleteYear,
                                       chainType = pairs$ChainType[p],
                                       cypColumn = pairs$CYP[p],
                                       pypColumn = pairs$PYP[p], benchType = benchType)))
  if (parallelise) parallel::stopCluster(cl)
  return(x)

}


# Helper function to rename "CVM" column from chain results based on the supplied cypColumn
chainRename <- function(.data, lastCompleteYear, cypColumn, pypColumn, chainType, benchType) {
  x <- robustChain(.data, lastCompleteYear, cypColumn, pypColumn, chainType, benchType = 4)
  x <- dplyr::select(x, CVM)
  # Rename cols to be cypColumn root + CVM, e.g. CapitalServicesCYP/PYP => CapitalServicesCVM
  # If the cypColumn doesn't have CYP at the end, just use the cypColumn
  # x <- .data
  varNameRoot <- sub("CYP$", "", cypColumn)
  newnames <- paste(varNameRoot, "CVM", sep = "")
  oldnames <- "CVM"
  x <- dplyr::rename_(x, .dots = setNames(oldnames, newnames))
  return(x)
}


#' Call chain with missing values present.
#'
#' Call the chain function whilst dealing with leading missing values, and all zero values.
#'
#' For metrics dealing with \emph{change} in quantities, e.g. "TotalChangesInVolume"
#' the first year in the series is often NA. chain is called without the inital
#' year of data and NAs are added afterwards.
#'
#' Where all the values in the given cypColumn are zero, the series is returned
#' without calling chain.
#'
#' @param .data A data.frame with the Year, current year prices and previous year
#' prices
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#' @param chainType Either Stock or Flow which determines the way the Annualised CVM
#' is calculated
#'
#' @return A data.frame as in the given input data with the addition of a CVM column
#' @importFrom magrittr "%>%"
#' @export
robustChain <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP", chainType = c("Stock", "Flow"), benchType = 4) {
  # If the entire series is zero (often the case with adjustment values) just
  # return CVM as all zeros
  if (all(!is.na(.data[cypColumn]) & .data[cypColumn] == 0)) {
    .data["CVM"] <- 0
    return(.data)
  }

  # If first four PYP values are NA or 0 call chain without them, then add them back
  # otherwise just call chain
  year1 <- seq(1, 4)
  if (all(is.na(.data[year1, pypColumn])) | all(.data[year1, pypColumn] == 0)) {
    leadingRows <- .data[year1, ]
    partialResult <- chain(.data = .data[-year1, ],
                           lastCompleteYear = lastCompleteYear,
                           cypColumn = cypColumn, pypColumn = pypColumn,
                           chainType = chainType, benchType = benchType)
    result <- dplyr::bind_rows(leadingRows, partialResult)
  } else {
    result <- chain(.data = .data[, ],
                    lastCompleteYear = lastCompleteYear,
                    cypColumn = cypColumn, pypColumn = pypColumn,
                    chainType = chainType, benchType = benchType)
  }
  return(result)
}

