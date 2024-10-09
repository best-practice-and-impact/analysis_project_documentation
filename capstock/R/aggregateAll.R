#' Aggregate all hierarchy tables.
#'
#' Aggregate to all combinations of hierarchy tables.
#'
#' Aggregates to all possible levels of the provided Sector, Industry, and Asset
#' hierarchy tables. Data must be provided at the lowest level of the hierarchies.
#'
#' Values are aggregated by summing, with NA values disregarded. Otherwise a single
#' series' having NA for a Period will mean all higher level aggregates must also be
#' NA for that Period.
#'
#' Hierarchy table columns must progress from least detailed to most detailed, i.e.
#' the left-most column represents the most aggregated level, and the right-most
#' column represents the most detailed level.
#' @param .data data.frame containing metrics to aggregate, along with Sector,
#'   Industry, Asset, and Period columns.
#' @param secHier data.frame containing the sector hierarchy.
#' @param indHier data.frame containing the industry hierarchy.
#' @param assHier data.frame containing the asset hierarchy.
#' @param values character vector of column names defining the numeric variables to be aggregated.
#' @import data.table
#' @export
#' @return data.frame aggregated to all combinations across the hierarchies with
#' Sector_Level, Industry_Level, Asset_Level, and Group columns to denote the
#' aggregation level for each row.
aggregateAll <- function(.data, secHier, indHier, assHier, values) {

  # --- Input Checks -----------------------------------------------------------
  #.data <- out
  #values <- colsToAggregate
  # Basic data types
  stopifnot(is.data.frame(.data))
  stopifnot(is.data.frame(secHier))
  stopifnot(is.data.frame(indHier))
  stopifnot(is.data.frame(assHier))
  stopifnot(is.character(values))

  # Must contain Period, Sector, Industry, Asset and whatever was provided in values
  minimalCols <- c("Period", "Sector", "Industry", "Asset")
  missingValues <- setdiff(c(minimalCols, values), colnames(.data))
  if (length(missingValues) > 0) {
   stop(paste(paste(missingValues, collapse = ", "), "not present in data."))
  }

  secLevels <- colnames(secHier)
  indLevels <- colnames(indHier)
  assLevels <- colnames(assHier)
  secLowest <- secLevels[length(secLevels)]
  indLowest <- indLevels[length(indLevels)]
  assLowest <- assLevels[length(assLevels)]

  # All categories in .data Sector, Industry, and Asset columns must match up
  # to the lowest level in the provided Sector, Industry, and Asset hierarchies
  missingSectors <- setdiff(.data$Sector, secHier[[secLowest]])
  missingIndustries <- setdiff(.data$Industry, indHier[[indLowest]])
  missingAssets <- setdiff(.data$Asset, assHier[[assLowest]])

  if (any(lapply(list(missingSectors, missingIndustries, missingAssets),
                 FUN = length) > 0)) {
    msg <- paste("The following categories are in the data but not present in the lowest level of the provided asset hierarchies\n",
      paste("Missing Sectors: ", paste(missingSectors, collapse = ", "), "\n"),
      paste("Missing Industries: ", paste(missingIndustries, collapse = ", "), "\n"),
      paste("Missing Assets: ", paste(missingAssets, collapse = ", ")),
      collapse = "\n")
    stop(msg)
  }

  # --- Aggregate --------------------------------------------------------------

  # Note all combos of hierarchy level are aggregated, *including* the lowest.
  # This is superfluous since the data exists already at the lowest level, but
  # this approach is taken for simplicity. To improve performance the final
  # level of aggregation could be ommitted and the results appended to .data.

  # Rename Sector, Industry, Asset to the lowest level in hierachy
  oldNames <- c("Sector", "Industry", "Asset")
  newNames <- c(secLowest, indLowest, assLowest)
  .data <- dplyr::rename_(.data, .dots = setNames(oldNames, newNames))

  # Data will now join by newNames
  .data <- .data %>%
    dplyr::left_join(secHier) %>%
    dplyr::left_join(indHier) %>%
    dplyr::left_join(assHier)

  # Convert to data.table
  # data.table is approx. an order of magnitude faster than dplyr for a
  # typical aggregation
  .data <- data.table(.data)

  # Create all possible combinations
  groupings <- expand.grid(Sector = secLevels,
                           Industry = indLevels,
                           Asset = assLevels,
                           stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)

  # Perform group_by/summarise for every group combination
  nGroups <- nrow(groupings)
  results <- vector(mode = "list", nGroups)
  for (i in seq_len(nGroups)) {
    secLevel <- groupings[i, "Sector"]
    indLevel <- groupings[i, "Industry"]
    assLevel <- groupings[i, "Asset"]

    # Aggregate using data.table. We need na.rm = TRUE otherwise NAs will propagate
    # into the aggregates
    aggGroups <- paste(secLevel, indLevel, assLevel, "Period", sep = ",")
    tmp <- .data[ , lapply(.SD, sum, na.rm = TRUE), .SDcols = values, by = aggGroups]

    tmp <- dplyr::rename_(tmp, "Sector" = secLevel, "Industry" = indLevel, "Asset" = assLevel)
      # Create cols to denote the applied grouping
    tmp <- dplyr::mutate(tmp,
                  Sector_Level = secLevel,
                  Industry_Level = indLevel,
                  Asset_Level = assLevel)
    results[[i]] <- tmp
  }
  # Convert from list of dataframes to a single dataframe
  results <- dplyr::bind_rows(results)

  # Add a column denoting the aggregation level
  results <- dplyr::mutate(results, Group = paste(Sector_Level, Industry_Level, Asset_Level, sep = "/"))
  # Remove any groupings
  results <- dplyr::ungroup(results)
  # Sort the data
  results <- dplyr::arrange(results, Group, Sector, Industry, Asset, Period)
  return(results)
}
