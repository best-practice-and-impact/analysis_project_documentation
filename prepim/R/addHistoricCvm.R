# Process timeseries as numeric vectors to use capstock functions
# However, data will come in as tidy dataframes
# We need to track Period also in order to link at the correct point

#' Add Historic CVM Series to Balanced Accounts
#'
#' Calculate Historic CVM Series from constant price data and historic deflators
#' and add to balanced accounts series.
#'
#' @details Historic GFCF series in CP are converted to CVM using historic deflators.
#' Balanced accounts CVM series are appended to create a dataset spanning the
#' entire time period.
#'
#' The historic CVM series is calculated by using an implied deflator from
#' balanced accounts data (CP / CVM at the link period). The historic
#' deflator is then referenced to this implied deflator:
#' referencedDeflator@period = (deflator@period / deflator@link) / impliedDeflator@link
#'
#' The referenced deflator is then applied to the historic GFCF series to
#' convert it to CVM. Balanced accounts CVM values are appended from the link
#' period onwards.
#'
#' Occasionally the balanced accounts CP or CVM values at the link period
#' are zero so unsuitable for creating an implied deflator. In these cases the
#' next non-zero period is used and a warning is issued.
#'
#' @param historicCp data.frame with Period and Value columns for historic GFCF in CP.
#' @param historicDeflator data.frame with Period and Value columns for historic
#' deflator. Typically as created by extractHistoricDeflators.
#' @param baCp data.frame of balanced accounts in CP with Period and Value columns.
#' @param baCvm data.frame of balanced accounts in CVM with Period and Value columns.
#' @param linkPeriod String denoting the Period at which to join historic and
#' balanced accounts (balanced accounts will start from the linkPeriod).
#' @return data.frame with Period and Value columns with periods covering min(historicCp)
#' to max(baCvm).
#' @import pimIO
addHistoricCvm <- function(historicCp, historicDeflator, baCp, baCvm, linkPeriod) {

  # ------------------- Data Checks --------------------------------------------
  # Sector/Industry/Asset groupings.
  # Expect balanced accounts series in CVM and CP should cover same periods
  if (!setequal(baCp$Period, baCvm$Period)) {
    msg <- "Historic deflator must cover same Sector/Industry/Asset/Periods as historicCp."
    flog.fatal(paste(msg, "Missing:"), data.frame(missingCoverage), capture = TRUE)
    stop(msg)
  }

  # HistoricDeflator must cover historicCp
  missingPeriods <- setdiff(historicCp$Period, historicDeflator$Period)
  if (length(missingPeriods) > 0) {
    msg <- paste0("Historic deflator must cover same period as historicCp. Missing: ",
                  paste(missingPeriods, collapse = ", "), ".")
    flog.fatal(msg)
    stop(msg)
  }

  # All data.frames must contain the reference period, except historicCp which
  # should be at least up to reference period (but not necessarily including)
  getMissing <- function(x, linkPeriod) {
    df <- eval(parse(text = x))
    if (!linkPeriod %in% df$Period) return(x)
  }
  dfsMissingLinkPeriod <- unlist(lapply(c("historicDeflator", "baCp", "baCvm"),
                                       getMissing, linkPeriod))
  if (length(dfsMissingLinkPeriod) > 0) {
    msg <- paste("Reference period", linkPeriod, "not found in",
                 paste(dfsMissingLinkPeriod, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }
  # historicCp must contain periods up to linkPeriod (but not necessarily including)
  # Infer previous linkPeriod. Depends on Period being orderable by time.
  periods <- unique(historicDeflator$Period)
  prevLinkPeriod <- periods[which(periods == linkPeriod) - 1]
  if (!prevLinkPeriod %in% historicCp$Period) {
    msg <- paste("Reference period", linkPeriod, "not found in historicCp.")
  }

  # Warn about zero-value historic deflators
  zeroValueDeflators <- historicCp %>%
    left_join(historicDeflator, by = c("Sector", "Industry", "Asset", "Period")) %>%
    rename(Historic = Value.x, Deflator = Value.y) %>%
    filter(Historic != 0 & Deflator == 0) %>%
    select(Sector, Industry, Asset, Period, Historic, Deflator)
  if (nrow(zeroValueDeflators) > 0) {
    msg <- "Zero-value deflators present. Calculated CVM will be infinite."
    flog.warn(msg, data.frame(zeroValueDeflators), capture = TRUE)
    warning(msg)
  }

  # Trim any superflous coverage from historicDeflator
  historicDeflator <- dplyr::semi_join(historicDeflator, historicCp, by = c("Sector", "Industry", "Asset"))

  # ----------- Calculate historic CVM series ----------------------------------

  standardCols <- c("Sector", "Industry", "Asset", "Period", "Value")
  # Add all datasets together, with one column per measure
  combined <- bind_rows(
    select(historicDeflator, one_of(standardCols)) %>% mutate(measure = "hdef"),
    select(historicCp, one_of(standardCols)) %>% mutate(measure = "hcp"),
    select(baCp, one_of(standardCols)) %>% mutate(measure = "cp"),
    select(baCvm, one_of(standardCols)) %>% mutate(measure = "cvm")
  )
  histCvm <- tidyr::spread(combined, key = measure, value = Value)

  # Filter out and report any series where we have no valid reference data for GFCF
  seriesWithNoRef <- histCvm %>%
    group_by(Sector, Industry, Asset) %>%
    mutate(validCp = !is.na(na_if(cp, 0)),
           validCvm = !is.na(na_if(cvm, 0)),
           validPeriod = Period >= linkPeriod,
           validAll = validCp & validCvm & validPeriod) %>%
    summarise(nValid = sum(validAll)) %>%
    filter(nValid == 0) %>%
    select(-nValid)
  if (nrow(seriesWithNoRef) > 0) {
    msg <- "Some series have no reference GFCF values. Are they all zero?"
    flog.warn(paste(msg, "Removing series:"), data.frame(seriesWithNoRef), capture = TRUE)
    histCvm <- anti_join(histCvm, seriesWithNoRef, by = c("Sector", "Industry", "Asset"))
    warning(msg)
  }

  # Calculate deflator linked to linkPeriod and apply it to Value
  histCvm <- histCvm %>%
    # Create implied deflator from balanced accounts
    mutate(id = cp / cvm) %>%
    group_by(Sector, Industry, Asset) %>%  # For each series:
    mutate(
      ref = linkPeriod,
      # Occasionally the cp/cvm at the ref period are zero so get the nearest
      # ref period to calculate implied deflator.
      idLinkPeriod = min(Period[Period >= linkPeriod & !is.na(na_if(cp, 0)) & !is.na(na_if(cvm, 0))]),
      # Record the deflator values as they are at the reference period(s)
      hdRef = hdef[which(Period == ref)],
      idRef = id[which(Period == idLinkPeriod)],
      # Reference the historical deflator to the deflator values at the ref period
      refDeflator = (hdef / hdRef) * idRef,
      # Deflate historicCp for non-zero values
      Value = if_else(hcp == 0, hcp, hcp / refDeflator))

  # Warn if we had to move the reference period of the implied deflator
  movedRefs <- histCvm %>%
    ungroup() %>%
    filter(ref != idLinkPeriod) %>%
    mutate(LinkPeriod = paste(ref, "->", idLinkPeriod)) %>%
    select(Sector, Industry, Asset, LinkPeriod) %>%
    distinct()
  if (nrow(movedRefs) > 0) {
    msg <- "Some series had GFCF of zero at the link period so a later period was used."
    flog.warn(paste(msg, "Series:"), data.frame(movedRefs), capture = TRUE)
    warning(msg)
  }


  # --------------------- Final filtering and formatting -----------------------

  # Trim timespan of new data from start of historicCp up to linkPeriod (but not including)
  fromPeriod <- min(historicCp$Period)
  histCvm <- dplyr::filter(histCvm, Period >= fromPeriod, Period < linkPeriod)


  # Warn if any suspect values are produced
  suspectValues <- histCvm %>%
    filter(is.na(Value))
  if (nrow(suspectValues) > 0) {
    msg <- "Missing or invalid Values produced."
    flog.warn(paste(msg, "Values:"), data.frame(suspectValues), capture = TRUE)
    warning(msg)
  }

  # Append balanced accounts CVM from the linkPeriod onwards
  histCvm <- bind_rows(histCvm, dplyr::filter(baCvm, Period >= linkPeriod))
  # Record Prices at reference period used
  histCvm <- mutate(histCvm, Prices = paste0("CVM_Ref_", linkPeriod)) %>%
    select(Sector, Industry, Asset, Prices, Period, Value) %>%
    ungroup()
  return(histCvm)
}


# --- Helper Functions ---------------------------------------------------------

# Calculate Balanced Accounts implied deflator
# return data.frame of Period and Value representing the implied deflator
calcBaImpliedDeflator <- function(baCp, baCvm) {
  baImpliedDeflator <- pimIO::calcPriceIndex(cpData = baCp$Value,
                                             cvmData = baCvm$Value)
  return(data.frame(Period = baCp$Period,
                    Value = baImpliedDeflator,
                    stringsAsFactors = FALSE))
}


# Reference the historical deflator to the value of the implied deflator
# to the value in the link period
# historicDeflator zero values are converted to NA (zero is often used as a place-holder
# for missing values). We can't have div by zero.
calcReferencedDeflator <- function(baImpliedDeflator, historicDeflator, linkPeriod) {
  # linkPeriod should appear only once
  if (anyDuplicated(baImpliedDeflator$Period)) stop("Duplicate Periods in baImpliedDeflator")
  if (anyDuplicated(historicDeflator$Period)) stop("Duplicate Periods in historicDeflator")

  historicDeflator %>%
    rename(hd = Value) %>%
    mutate(hd = if_else(hd == 0, NA_real_, hd)) %>%
    left_join(baImpliedDeflator, by = c("Period")) %>%
    rename(id = Value) %>%
    mutate(ref = linkPeriod,
           hdRef = hd[which(Period == ref)],
           idRef = id[which(Period == ref)],
           refDeflator = (hd / hdRef) * idRef) %>%
    select(Period, Value = refDeflator)
}


# Combine implied deflator with referenced deflator
# Use referenced deflator *before* ref period and implied deflator *from* ref period
calcFinalDeflator <- function(baImpliedDeflator, referencedDeflator, linkPeriod) {
  beforeRef <- dplyr::filter(referencedDeflator, Period < linkPeriod)
  afterRef <- dplyr::filter(baImpliedDeflator, Period >= linkPeriod)
  bind_rows(beforeRef, afterRef)
}


convertHistoricCpToCvm <- function(historic, deflator) {
  # Ignore surplus periods in deflator but not in historic
  deflator <- semi_join(deflator, historic, by = c("Period"))
  return(data.frame(
    Period = historic$Period,
    Value = pimIO::convertCPToCVM(historic$Value, deflator$Value),
    stringsAsFactors = FALSE))
}

