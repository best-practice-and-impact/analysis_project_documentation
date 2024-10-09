#' Unchain all CP/CVM pairs.
#'
#' Unchain all CP/CVM variables into Current Year Prices and Previous Year Prices
#' using parallel processing.
#'
#' The default deflatorMethod of "implicit" will infer a deflator from the CP and CVM
#' series (CP divided by CVM). If the "explicit" method is used, a deflatorColumn
#' must be provided and the CVM series are not used. See \code{unchain} for details.
#'
#' @param .data A data.frame with one row per Sector/Asset/Industry with a "data" list-column
#' containing current price, chained volume measures, Year columns, and optionally a price index column.
#' @param pairs A data.frame with CP and CVM columns containing the variable name pairs for unchaining.
#' If deflatorMethod is "explicit" the CVM column is not used.
#' @param deflatorColumn The column name for an optional deflator when deflatorMethod is "explicit".
#' @param deflatorMethod String denoting deflator calculation method, "implicit"
#' or "explicit", see Details.
#' @param parallelise If TRUE (the default) processing will run on a local parallel
#' cluster using all CPU cores except one.
#' @return The input data.frame with an addional "unchained" list-column with CP
#' varnames appended with "CYP", and "PYP".
#' @import doSNOW parallel
#' @importFrom foreach foreach "%dopar%" "%do%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2
#' @export
unchainAll <- function(.data, pairs, refYear, parallelise = TRUE) {
  ######################################################################################################
  # .data <- out
  # library(doSNOW, foreach, magrittr)
  # library(purrr, map2)
  ######################################################################################################
  # --- Input Checks -----------------------------------------------------------
  # .data must contain "data" list-column
  stopifnot("data" %in% colnames(.data))
  # Get the column names from the embedded data list-column
  dataCols <- colnames(.data$data[[1]])

  # Pairs must have colnames of CP and CVM
  pairCols <- colnames(pairs)
  stopifnot("CP" %in% pairCols)
  stopifnot("CVM" %in% pairCols)

  # The data list-column within .data must contain all the defined pairs
  stopifnot(all(pairs$CP %in% dataCols))
  stopifnot(all(pairs$CVM %in% dataCols))
  # The data list-column must contain a Year column
  stopifnot("Year" %in% dataCols)

  # If an explicit deflator is used it must be present in the data

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
  # Set up a progress bar
  pb <- txtProgressBar(min = 0, max = nrow(.data), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  # Create options list for foreach
  opts <- list(progress = progress)

  # --- Unchain ----------------------------------------------------------------
  # For each row
  #   For each CP/CVM pair
  #     unchain
  x <- dplyr::mutate(.data, unchained =
                       foreach(i = seq_len(nrow(.data)),
                               .packages = c("dplyr", "capstock"),
                               .errorhandling = "pass",
                               .options.snow = opts) %doOrDoPar%
                       dplyr::bind_cols(
                               purrr::map2(pairs$CP, pairs$flow_stock,
                                       ~unchainRename(.data = .data[[i, "data"]],
                                                      cpColumn = .x, flow_stock = .y,
                                                      refYear = refYear))

                       )
  )
  if (parallelise) parallel::stopCluster(cl)
  return(x)
}


# Helper function to rename "CYP"/"PYP" columns from unchain results so they are less generic.
# @param .data A data.frame containing Year columns and pairs of variables in CP/CVM
# @param cpColumn String denoting the CP version of a variable pair
# @param cvmColumn String denoting the CVM version of variable pair
# @return data.frame of varnameCYP and varnamePYP columns. Other columns are dropped.
# Unchain provided column pair and return CYP and PYP columns renamed based on
# cpColumn root. Original columns and "Year" are also dropped.
unchainRename <- function(.data, cpColumn, flow_stock, refYear) {
  x <- unchain(.data, cpColumn, flow_stock,refYear)
  x <- dplyr::select(x, CYP, PYP)
  # Rename cols to be cpColumn root + CYP/PYP, e.g.
  # CapitalServicesCP/CVM => CapitalServicesCYP, CapitalServicesPYP
  # If the cpColumn doesn't have CP at the end, just use the cpColumn
  varNameRoot <- sub("CP$", "", cpColumn)
  newnames <- paste(varNameRoot, c("CYP", "PYP"), sep = "")
  oldnames <- c("CYP", "PYP")
  x <- dplyr::rename_(x, .dots = setNames(oldnames, newnames))
  return(x)
}
