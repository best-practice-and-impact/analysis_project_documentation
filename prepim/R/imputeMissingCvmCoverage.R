#' @title Impute Missing CVM Coverage
#' @description Imputes CVM series where present in CP balanced accounts but not
#'   in CVM for given Sector, Industry, and Assets using Industry TOTALs.
#' @details The cp and cvm data.frames must include series for Industry TOTALs.
#' A new series is created for each row of the required \code{coverage}
#'   using the following method:
#'
#'   1. Calculate implied deflator using Industry TOTALs for required
#'     Sector/Asset combination (CP TOTAL / CVM TOTAL)
#'
#'   2. Apply the deflator to the required Sector/Industry/Asset CP series.
#'
#'   If the required Sector/Asset TOTALs are not available in the CVM data, a
#'   deflator is created as above but using S.11PR/Asset as a fallback.
#'
#' @param requiredCoverage data.frame with Sector, Industry, Asset variables for the required coverage.
#' @param cp data.frame of Current Prices, e.g. as produced by \code{extractBalAccountsCord}. Must contain Industry TOTALs for required Sector/Asset coverage.
#' @param cvm data.frame of CVM, e.g. as produced by \code{extractBalAccountsCord} which has missing series. Must contain Industry TOTALs for required Sector/Asset coverage.
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @return A tidy data.frame of imputed balanced accounts with \code{Sector},
#'  \code{Industry}, \code{Asset}, \code{Period}, and \code{Value} variables.
imputeMissingCvmCoverage <- function(requiredCoverage, cp, cvm) {

  if (!is.data.frame(requiredCoverage) | !is.data.frame(cp) | !is.data.frame(cvm)) {
    stop("requiredCoverage, cp, and cvm must be data.frames")
  }

  flog.info("Imputing coverage.")

  # ---------------------- Check for TOTALS in supplied series-----------------
  # Check that CVM and CP series both have TOTALs for the requested Sector/Asset combo

  # Get Sector Asset Totals for CVM and CP
  cvmSecAssTotals <- dplyr::filter(cvm, Industry == "TOTAL") %>%
    dplyr::select(-Industry, -Prices, -Period, -Value) %>%
    distinct()
  cpSecAssTotals <- dplyr::filter(cp, Industry == "TOTAL") %>%
    dplyr::select(-Industry, -Prices, -Period, -Value) %>%
    distinct()
  missingCvmTotals <- anti_join(requiredCoverage, cvmSecAssTotals, by = c("Sector", "Asset"))
  missingCpTotals <- anti_join(requiredCoverage, cpSecAssTotals, by = c("Sector", "Asset"))
  missingTotals <- bind_rows(missingCpTotals, missingCvmTotals)

  # If no Sector/Asset TOTALs, look for matching Asset TOTALs in fallback sector
  # Must be present for BOTH CVM and CP
  flog.info("Checking CVM/CP contains Sector/Asset Industry TOTALs for required coverage.")
  fallbackSector <- "S.11PR"
  if (nrow(missingTotals) > 0) {
    flog.warn("Missing Sector/Asset TOTALs for required coverage. Will try fallback to S.11PR.")
    flog.warn("Industry TOTALs missing from CVM data for following required coverage:", as.data.frame(missingCvmTotals), capture = TRUE)
    flog.warn("Industry TOTALs missing from CP data for following required coverage:", as.data.frame(missingCpTotals), capture = TRUE)
    flog.warn("Checking fallback sector %s for TOTALS.", fallbackSector)
    completelyMissingCvmTotals <- missingTotals %>%
      mutate(fbSector = fallbackSector) %>%
      anti_join(cvmSecAssTotals, by = c("fbSector" = "Sector", "Asset" = "Asset"))
    completelyMissingCpTotals <- missingTotals %>%
      mutate(fbSector = fallbackSector) %>%
      anti_join(cpSecAssTotals, by = c("fbSector" = "Sector", "Asset" = "Asset"))
    completelyMissingTotals <- bind_rows(completelyMissingCpTotals, completelyMissingCvmTotals)
    # Both CVM and CP must contain the TOTALs for the required Assets in the fallback sector data
    if (nrow(completelyMissingTotals) > 0 ) {
      msg <- "Fallback sector does not contain TOTALs for required coverage in both CP/CVM series."
      flog.fatal(msg, as.data.frame(completelyMissingCvmTotals), capture = TRUE)
      flog.fatal("Missing Fallback CVM TOTALs", as.data.frame(completelyMissingCvmTotals), capture = TRUE)
      flog.fatal("Missing Fallback CP TOTALs", as.data.frame(completelyMissingCpTotals), capture = TRUE)
      stop(msg)
    }
  }

  # ---------------------- Impute Series ---------------------------------------

  # Split requiredCoverage where we have the full Sector/Asset totals and where
  # we have used the fallback (as we will need to join the data differently)
  primaryRequiredCoverage <- requiredCoverage %>%
    anti_join(missingTotals, by = c("Sector", "Industry", "Asset"))
  fallbackRequiredCoverage <- requiredCoverage %>%
    inner_join(missingTotals, by = c("Sector", "Industry", "Asset"))

  # Create TOTALs where we have the full Sector/Asset series
  primSectorAssetTotals <- primaryRequiredCoverage %>%
    mutate(Industry = "TOTAL") %>%  # Overwrite Industry to join with TOTALs
    left_join(cvm, by = c("Sector", "Industry", "Asset")) %>%
    rename(CVM = Value) %>%
    select(-Prices) %>%
    left_join(cp, by = c("Sector", "Industry", "Asset", "Period")) %>%
    rename(CP = Value) %>%
    select(-Prices)

  # Create TOTALs where we have to fallback to S.11PR/Asset
  fbSectorAssetTotals <- fallbackRequiredCoverage %>%
    mutate(Industry = "TOTAL") %>%  # Overwrite Industry to join with TOTALs
    mutate(fbSector = fallbackSector) %>%  # Create sector to join with TOTALs (we want to retain original sector)
    left_join(cvm, by = c("fbSector" = "Sector", "Industry", "Asset")) %>%
    rename(CVM = Value) %>%
    select(-Prices) %>%
    left_join(cp, by = c("fbSector" = "Sector", "Industry", "Asset", "Period")) %>%
    rename(CP = Value) %>%
    select(-Prices, -fbSector)

  if (nrow(fallbackRequiredCoverage) > 0) {
    flog.warn(paste("Imputing series using", fallbackSector, "for:"),
              as.data.frame(fallbackRequiredCoverage), capture = TRUE)
  }

  sectorAssetTotals <- bind_rows(primSectorAssetTotals, fbSectorAssetTotals)

  # --- Integrity checks ---
  # Check for CVM TOTALs == 0 (i.e. resulting in a divide by zero for Deflator)
  cvmZero <- sectorAssetTotals %>%
    dplyr::filter(CP != 0 & CVM == 0)
  flog.info("Checking CVM TOTALs are all non-zero for non-zero CP periods.")
  if (nrow(cvmZero) > 0) {
    flog.warn("CVM TOTALs of zero present. Will result in divide-by-zero error.",
               as.data.frame(cvmZero), capture = TRUE)
  }

  # Remove duplicate rows and create Deflator (CP/CVM).
  # Avoid divide-by-zero errors where both CP and CVM are zero (use deflator of 1)
  sectorAssetTotals <- sectorAssetTotals %>%
    distinct() %>%
    mutate(Deflator = if_else(CP == 0, 1, CP / CVM),
           Industry = NULL,
           CVM = NULL,
           CP = NULL)

  # Get corresponding series from CP and apply Deflator
  result <- requiredCoverage %>%
    left_join(cp, by = c("Sector", "Industry", "Asset")) %>%
    left_join(sectorAssetTotals, by = c("Sector", "Asset", "Period")) %>%
    mutate(Value = Value / Deflator,
           Prices = "CVM") %>%
    select(Sector, Industry, Asset, Prices, Period, Value)

  # --- Integrity checks ---
  # Check result conforms to expected dimensions
  # Expect 1 row per unique Period for each row in the requiredCoverage
  expectedRows <- nrow(requiredCoverage) * length(unique(cp$Period))
  flog.info("Checking imputed data has correct number of observations.")
  if (nrow(result) != expectedRows) {
    msg <- paste0("Imputed data does not have correct number of observations (", expectedRows, ").")
    flog.fatal(msg)
    stop(msg)
  }

  return(result)
}
