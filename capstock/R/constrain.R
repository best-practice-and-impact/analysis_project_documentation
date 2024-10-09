#' Constrain Series.
#'
#' Constrain one series to another.
#'
#' The output series contains old values up until the openIndex. From the open index
#' onwards new values are calculated using the growth rates from new.
#'
#' Series that contain zero values in the open period are adjusted to \code{zeroAdj}
#' before applying growth rates.
#'
#' Data provided in old and new must each have contiguous Periods.
#'
#' If pass = TRUE the new data.frame Period and varName columns are returned unmodified.
#'
#' @param old data.frame with Period and a varName column containing the contraining values, usually from previously published data. Must contain at least one preceeding value from the openPeriod.
#' @param new data.frame with Period and a varName column containing the values to be contrained. Must contain the openPeriod and at least one preceeding value.
#' @param varName String denoting the stock/value to be constrained (must exist in both new and old)
#' @param openPeriod String denoting the open period from which to extend old using new's growth rates
#' @param zeroAdj An adjustment factor if zero values are present in new
#'
#' @return A data.frame with old up till the openPeriod and extended by the growth rates of new from the openPeriod onwards
#' @export
constrain <- function(old = NULL, new, openPeriod = NULL, varName, pass = FALSE, zeroAdj = 0.001) {
  # Note, this function processes dataframes keyed by "Period" rather than
  # just vectors of numbers. This is because when processing old and new series
  # en masse it is easy to misalign the series or select the wrong
  # openPeriod if using vector indices instead of an explicit key

  # --------------------------- Check Data -------------------------------------

  # Check argument presence
  if (is.null(old) & pass == FALSE) stop("Must provide old dataset when pass = FALSE.")
  if (is.null(openPeriod) & pass == FALSE) stop("Must provide openPeriod when pass = FALSE.")
  if (zeroAdj == 0) stop("zeroAdj must be greater than zero.")

  # Check argument values
  if (!is.data.frame(new)) stop("new must be a data.frame.")
  # varName and Period must be in new
  if (!varName %in% colnames(new)) stop(paste0("varName \"", varName, "\" not in new."))
  if (!"Period" %in% colnames(new)) stop("new must contain \"Period\" column.")

  # If not passing through, further checks are necessary
  if (pass == FALSE) {
    if (!is.data.frame(old)) stop("old must be a data.frame.")
    if (!varName %in% colnames(old)) stop(paste0("varName \"", varName, "\" not in old."))
    if (!"Period" %in% colnames(old) ) stop("old must contain \"Period\" column.")
    # openPeriod must be character and length 1
    if (!is.character(openPeriod)) stop("openPeriod must be a string.")
    if (length(openPeriod) > 1) stop("openPeriod must be a single string.")
    # new must contain the open period
    if (!openPeriod %in% new$Period) stop("new must contain the openPeriod.")
    # new must contain at least one period before the openPeriod (to calc 1st growth rate)
    # in checking for this, new is expected to contain contiguous periods
    prePeriods <- new$Period[new$Period < openPeriod]
    if (length(prePeriods) == 0) stop("new must contain at least one period before the openPeriod.")
    # old must contain at least one period before the openPeriod
    prePeriod <- prePeriods[length(prePeriods)]
    if (!prePeriod %in% old$Period) stop(paste("old must contain at least one period before the openPeriod, i.e.", prePeriod))

    # new must not contain any missing values from the value preceeding the openPeriod onwards
    if (any(is.na(new[new$Period >= prePeriod, varName]))) stop("new must not contain missing values from the openPeriod onwards.")

  }


  # ------------------------ Constrain -----------------------------------------

  # pass = TRUE passes through new without any constraining
  if (pass) {
    result <- dplyr::select(new, one_of("Period", varName))

  } else {

    # Set aside data for the closed period
    closed <- dplyr::filter(old, Period < openPeriod)

    # Get the last value of the closed period from which to accumulate new growth
    lastClosed <- dplyr::last(closed[[varName]])
    # If this is zero we will accumulate nothing, so adjust using zeroAdj
    if (lastClosed == 0) lastClosed <- zeroAdj

    # Calculate growth rates from new
    # Any zero values are replaced by zeroAdj to prevent divide by zero errors
    new <- dplyr::mutate_(new, .val = as.name(varName))  # Give varName an internal name for reference
    new <- dplyr::mutate(new, .valAdjusted = dplyr::if_else(.val == 0, zeroAdj, .val))
    new <- dplyr::mutate(new, growth = .valAdjusted / lag(.valAdjusted))

    # Separate out open period rows
    open <- dplyr::filter(new, Period >= openPeriod)

    # Calculate the new values in the open period by extending the last closed value
    # using new's growth rates
    open <- dplyr::mutate(open, .newValues = lastClosed * cumprod(growth))

    # If any values were zero-adjusted, replace them with zero
    open <- dplyr::mutate(open, .newValues = dplyr::if_else(.val == 0, 0, .newValues))

    # Remove interim calculations
    open <- dplyr::select(open, Period, .newValues)

    # Align the column names of the new data and combine with the closed period data
    closed <- dplyr::select(closed, one_of(c("Period", varName)))
    oldname <- ".newValues"
    open <- dplyr::rename_(open, .dots = setNames(oldname, varName))
    result <- dplyr::bind_rows(closed, open)
  }

  return(result)

}

#' Constrain All Series.
#'
#' Constrain all series to previously published data
#'
#' The output series contains old values up until the openIndex. From the open index
#' onwards new values are calculated using the growth rates from new.
#'
#' Series that contain zero values in the open period are adjusted to \code{zeroAdj}
#' before applying growth rates.
#'
#' old and new must contain Sector, Industry, and Asset columns to denote the series
#' and all series data must be provided as a nested list-column named "data".
#'
#' @param old data.frame of existing results, with one row per series and a nested \code{data} column containing Period and varNames columns. Series must be identified by Sector, Industry, and Asset columns.
#' @param new data.frame of new results, with one row per series and a nested \code{data} column containing Period and varNames columns. Series must be identified by Sector, Industry, and Asset columns.
#' @param openPeriod String denoting the open period from which to extend old using new's growth rates
#' @param varNames String vector denoting the columns to be constrained (must exist in both new and old's data list-column)
#' @param zeroAdj An adjustment factor if zero values are present in new
#'
#' @return The provided \code{old} data.frame with an additional "constrained" list-column containing the constrained values.
#' @export
constrainAll <- function(old, new, openPeriod = NULL, varNames, pass = FALSE, zeroAdj = 0.001) {

  # --------------------------- Check Data -------------------------------------
  reqCols <- c("Sector", "Industry", "Asset", "data")
  misCols <- setdiff(reqCols, colnames(new))
  if (length(misCols) > 0) {
    stop(paste0("new is missing columns: ", paste(misCols, collapse = ", "), "."))
  }

  # If not just passing through the new data, the old data needs to be checked and matched.
  if (!pass) {
    misCols <- setdiff(reqCols, colnames(old))
    if (length(misCols) > 0) {
      stop(paste0("old is missing columns: ", paste(misCols, collapse = ", "), "."))
    }

    # Create two matching datasets for new and old
    jk <- c("Sector", "Industry", "Asset")
    # Set aside old series without new series (no action needed)
    oldNotNew <- dplyr::anti_join(old, new, by = jk)
    # Filter old to where there are matching series in new
    old <- dplyr::semi_join(old, new, by = jk)
    # Drop any rows in new that are not in old
    new <- dplyr::semi_join(new, old, by = jk)

    if (nrow(old) != nrow(new)) stop("Unable to match old and new series.")

    # Align old and new datasets (as we are iterating by row)
    old <- ungroup(old)
    new <- ungroup(new)
    old <- arrange(old, Sector, Industry, Asset)
    new <- arrange(new, Sector, Industry, Asset)

    # "Constrain" for series where we have just old data
    # The constrained col is just the old "data" list-col with Period and varNames cols selected
    if (nrow(oldNotNew) > 0) {

      # Unnest the data list-column and pick out the varNames we are constraining
      oldNotNewTrimmed <- tidyr::unnest(oldNotNew)
      oldNotNewTrimmed <- dplyr::select(oldNotNewTrimmed,
                                        one_of(c("Sector", "Industry", "Asset", "Period", varNames)))
      # Renest
      oldNotNewTrimmed <- tidyr::nest(group_by(oldNotNewTrimmed, Sector, Industry, Asset),
                                      .key = constrained)
      # Add the "constrained" values onto the set-aside dataframe
      oldNotNew <- dplyr::left_join(oldNotNew, oldNotNewTrimmed, by = jk)
    }
  }

  # Call constrain() for every row and every varName
  result <- dplyr::mutate(old, constrained =
                            foreach(i = seq_len(nrow(old)),
                                    .packages = c("dplyr", "capstock"),
                                    .errorhandling = "pass") %do%
                            dplyr::bind_cols(
                              foreach(variable = varNames) %do%
                                constrain(old = old[[i, "data"]],
                                          new = new[[i, "data"]],
                                          openPeriod = openPeriod,
                                          varName = variable,
                                          pass = pass,
                                          zeroAdj = zeroAdj)))

  # Add back in the series where we have old but not new data
  if (!pass) result <-  dplyr::bind_rows(oldNotNew, result)

  return(result)
}
