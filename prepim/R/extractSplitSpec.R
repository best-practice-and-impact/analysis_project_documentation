#' Extract Split Specification
#'
#' Extract a splits specification file and check its consistency.
#'
#' This function is usually called from \code{applySplitSpec}, but can be used
#' directly if adjustments to the split specification are required before
#' applying the splits.
#'
#' The specification is expected to have Sector, Industry, Asset, Split columns, with
#' additional columns for each Period, containing split proportions,
#' for example Sector, Industry, Asset, Split, Y1990, Y1991, Y1992...
#'
#' Split proportions should be in the range 0-1 and should sum to 1 when grouped
#' by Sector, Industry, and Asset.
#'
#' @param path String path to a CSV file containing splits.
#' @return verified data.frame of splits.
#' @export
extractSplitSpec <- function(path) {
  x <- readSplitsFile(path)
  x <- parseSplitsSpec(x)
  return(x)
}

# Reads a splits file from disk using the predefined data types
readSplitsFile <- function(path) {
  x <- readr::read_csv(
    path,
    col_types = readr::cols(
      .default = col_double(),
      Sector = col_character(),
      Industry = col_character(),
      Asset = col_character(),
      Split = col_character()
    )
  )
  return(x)
}

parseSplitsSpec <- function(x) {

  # Define standard column headings (remaining should be Periods)
  categories <- c("Sector", "Industry", "Asset", "Split")

  flog.info("Checking for expected column names.")
  if (checkColNames(x, expectedCategories = categories) == FALSE) {
    stop("Could not parse column names.")
  }

  flog.info("Checking for invalid combinations.")
  if (chkSpecCombinations(x) == FALSE) {
    stop("Invalid combinations.")
  }

  flog.info("Checking proportions are valid.")
  if (checkProportions(x, categories) == FALSE) {
    stop("Proportions are incorrectly specified.")
  }

  # Standarise Industry codings from e.g. "1" to "01"
  flog.info("Left-padding Industry codes with zero if required.")
  x <- mutate(x, Industry = formatIndustryCodes(Industry))

  return(x)
}

# Checks required column headings
# Split files have 1 col per period, so we don't know in advance how many
# columns there will be.
# Return FALSE if error, otherwise TRUE.
checkColNames <- function(.data, expectedCategories) {

  unmatchedCols <- !(expectedCategories %in% colnames(.data))
  if (any(unmatchedCols)) {
    msg <- paste("Incorrect column names. Could not find",
                 paste(expectedCategories[unmatchedCols], collapse = ", "))
    flog.fatal(msg)
    return(FALSE)
  }

  # Assume all non-category columns are Periods that conform to YxxxxQz
  # where xxxx is a year and z is a quarter, e.g. "Y2012Q1"
  periodCols <- setdiff(colnames(.data), expectedCategories)
  incorrectPeriodCols <- !grepl("^Y[0-9]{4}Q[0-9]{1}$", periodCols)
  if (any(incorrectPeriodCols)) {
    msg <- paste("Incorrect column names for periods. Not expecting",
                 paste(periodCols[incorrectPeriodCols], collapse = ", "))
    flog.fatal(msg)
    return(FALSE)
  }

  # If nothing caused an error return true
  return(TRUE)
}


# Checks proportions are in correct range and all sum to 1 across the grouping
# category
# categories stores the non-Period column names
# Return FALSE if error, otherwise TRUE.
checkProportions <- function(.data, categories) {

  # Temporarily gather the Periods
  .data <- tidyr::gather(.data, Period, Proportion, -dplyr::one_of(categories))

  # Verify all proportions are in correct range (0-1)
  badRangeProps <- !between(.data$Proportion, 0, 1)
  if (any(badRangeProps)) {
    flog.fatal("Proportions out of range 0-1:", as.data.frame(.data[badRangeProps, ]),
               capture = TRUE)
    return(FALSE)
  }

  # Verify all proportions sum to 1
  tol <- 0.0001
  badTotalProps <- .data %>%
    group_by(Sector, Industry, Asset, Period) %>%
    summarise(PropTotal = sum(Proportion)) %>%
    ungroup() %>%
    filter(abs(1 - PropTotal) > tol) %>%
    left_join(.data, by = c("Sector", "Industry", "Asset", "Period"))

  if (nrow(badTotalProps) > 0) {
    flog.fatal("Proportions do not sum to 1:", as.data.frame(badTotalProps),
               capture = TRUE)
    return(FALSE)
  }

  # If no errors so far, return TRUE
  return(TRUE)
}
