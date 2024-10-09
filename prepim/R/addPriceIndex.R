#' Link historic and new deflators.
#'
#' Combine series of historic and new deflators, linking them together at the specified Period.
#'
#' Historic deflators are referenced to the new deflators at the link period.
#' Both datasets must include data for the link period and should include Sector,
#' Industry, Asset, Period, and Value columns.
#'
#' The recent deflators are returned as-is and the historic deflators are linked
#'
#' The returned data.frame has Sector, Industry, Asset, Period, and Value columns.
#'
#' @param defHist Historic deflators up to and including the link period.
#' @param defRecent Recent deflators from the link period onwards.
#' @param linkPeriod String denoting the link period.
#' @export
#' @return data.frame of deflators from min(defHist$Period) to max(defRecent$Period).
linkDeflators <- function(defHist, defRecent, linkPeriod) {
  flog.info("Linking deflators.")

  # -------------------------- Check input -------------------------------------
  # Check for required columns
  expectedCols <- c("Sector", "Industry", "Asset", "Period", "Value")
  flog.info("Checking input for expected column headings.")
  if (!all(expectedCols %in% colnames(defHist))) {
    msg <- paste("defHist must contain columns", paste(categoryHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }
  if (!all(expectedCols %in% colnames(defRecent))) {
    msg <- paste("defHist must contain columns", paste(categoryHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  flog.info("Checking linkPeriod is present in all series.")
  defHistNoLink <- dplyr::anti_join(defHist,
                                    dplyr::filter(defHist, Period == linkPeriod),
                                    by = c("Sector", "Industry", "Asset"))
  defHistNoLink <- dplyr::distinct(defHistNoLink, Sector, Industry, Asset)
  defRecentNoLink <- dplyr::anti_join(defRecent,
                                    dplyr::filter(defRecent, Period == linkPeriod),
                                    by = c("Sector", "Industry", "Asset"))
  defRecentNoLink <- dplyr::distinct(defRecentNoLink, Sector, Industry, Asset)
  if (nrow(defHistNoLink) > 0) {
    msg <- paste("Some series in defHist do not contain data for", linkPeriod, "linkPeriod.")
    flog.fatal(paste(msg, "Series:"), data.frame(defHistNoLink), capture = TRUE)
    stop(msg)
  }
  if (nrow(defRecentNoLink) > 0) {
    msg <- paste("Some series in defRecent do not contain data for", linkPeriod, "linkPeriod.")
    flog.fatal(paste(msg, "Series:"), data.frame(defRecentNoLink), capture = TRUE)
    stop(msg)
  }

  flog.info("Checking datasets contain no duplicate series.")

  keyCols <- c("Sector", "Industry", "Asset", "Period")
  defHistDupes <- dplyr::select(defHist, dplyr::one_of(keyCols))
  defHistDupes <- defHistDupes[duplicated(defHistDupes), ]
  defRecentDupes <- dplyr::select(defRecent, dplyr::one_of(keyCols))
  defRecentDupes <- defRecentDupes[duplicated(defRecentDupes), ]
  if (nrow(defHistDupes) > 0) {
    msg <- paste("defHist contains duplicates.")
    flog.fatal(paste(msg, "Duplicates:"), data.frame(defHistDupes), capture = TRUE)
    stop(msg)
  }
  if (nrow(defRecentDupes) > 0) {
    msg <- paste("defRecent contains duplicates.")
    flog.fatal(paste(msg, "Duplicates:"), data.frame(defRecentDupes), capture = TRUE)
    stop(msg)
  }

  # ------------------------------ Link Series ---------------------------------

  # Trim historic deflators up to linkPeriod and rename Value to hdef (historic def)
  defHist <- dplyr::filter(defHist, Period <= linkPeriod)
  defHist <- dplyr::rename(defHist, hdef = Value)
  # Trim recent deflators from linkPeriod onwards and rename Value
  defRecentLP <- dplyr::filter(defRecent, Period == linkPeriod)
  defRecentLP <- dplyr::rename(defRecentLP, linkdef = Value)
  # Set aside new deflators after linkPeriod to append later
  defRecentAfterLP <- dplyr::filter(defRecent, Period > linkPeriod)

  # Create the Linked Deflator ldef
  # One period's ldef is dependent its previous ldef
  # Start at the linkPeriod and work backwards in time.
  # If we're at the link period use the deflator from the recent deflators, otherwise use
  # previous ldef / (defHist / previous defHist)
  # We're using cumprod (Cumulative Product) since it's vectorised and avoids a slow for-loop.
  # cumprod is called with the reciprocal of the denominator (since there's no "cumdivide")

  # Calculate linked historic deflators up to and including linkPeriod
  defAll <- defHist %>%
    # Add the recent deflators from the link Period
    left_join(defRecentLP, by = c("Sector", "Industry", "Asset", "Period")) %>%
    # For each series:
    group_by(Sector, Industry, Asset) %>%
    # Sort descending - crucial for the next calculations which use "lag"
    arrange(desc(Period)) %>%
    # Create the linked deflator
    mutate(denom = (lag(hdef) / hdef)) %>%
    mutate(ldef = cumprod(if_else(Period == linkPeriod, linkdef, denom^-1))) %>%
    ungroup() %>%
    rename(Value = ldef) %>%
    select(Sector, Industry, Asset, Period, Value)

  # Combine with BA deflators *after* the linkPeriod
  defAll <- bind_rows(defAll, defRecentAfterLP)
  # Reorder
  defAll <- dplyr::arrange(defAll, Sector, Industry, Asset, Period)
  return(defAll)
}

#' Add PriceIndex to GFCF data.
#'
#' Add PriceIndex to a prepared GFCF data.frame.
#'
#' Deflators are checked for coverage and then left-joined to the GFCF data.frame.
#'
#' @param gfcf data.frame of GFCF series containing Sector, Industry, Asset, Period, and Value columns.
#' @param deflators data.frame containing Sector, Industry, Asset, Period, and Value columns.
#' @export
#' @return data.frame with gfcf with a new "PriceIndex" column containing the deflators.
addPriceIndex <- function(gfcf, deflators) {

  # ----------------------- Check Inputs ---------------------------------------
  flog.info("Checking gfcf for expected column headings.")
  expectedCols <- c("Sector", "Industry", "Asset", "Period")
  if (!all(expectedCols %in% colnames(gfcf))) {
    msg <- paste("gfcf must contain columns", paste(expectedCols, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  flog.info("Checking deflators for expected column headings.")
  expectedCols <- c(expectedCols, "Value")
  if (!all(expectedCols %in% colnames(deflators))) {
    msg <- paste("deflators must contain columns", paste(expectedCols, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  # Check deflators for missing coverage
  flog.info("Checking deflators for missing coverage.")
  # Define join keys
  jk <- c("Sector", "Industry", "Asset", "Period")
  missingCoverage <- anti_join(gfcf, deflators, by = jk)
  missingCoverage <- dplyr::distinct(missingCoverage, Sector, Industry, Asset, Period)
  if (nrow(missingCoverage) > 0) {
    msg <- paste("Missing coverage in deflators.")
    flog.fatal(paste(msg, "Series:"), data.frame(missingCoverage), capture = TRUE)
    stop(msg)
  }

  # -------------------------- Join Datasets -----------------------------------

    # Rename deflator "Value" to "PriceIndex"
  deflators <- dplyr::rename(deflators, PriceIndex = Value)

  # Join deflators to gfcf
  flog.info("Joining deflators as \"PriceIndex\".")
  gfcf <- dplyr::left_join(gfcf, deflators, by = jk)
  return(gfcf)
}
