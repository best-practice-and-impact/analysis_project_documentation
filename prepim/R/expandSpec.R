#' Expand specification dataframe
#'
#' Expand a specification dataframe to match a given coverage. Sector, Industry,
#' or Asset values containing the "ALL" wildcard will be expanded to match the
#' coverage.
#'
#' Up to two of Sector/Industry/Asset can be "ALL" per spec row (but not all three).
#'
#' @details Splits will first be matched to the coverage using the most detailed
#' specification, where all joinKeys match, e.g. by Sector, Industry, and Asset.
#' Then splits will be matched more generally by removing a joinKey, e.g. by
#' matching on only Sector and Industry. Finally, The splits will be matched
#' by just Sector.
#'
#' The ordering of the matching will always go from most detailed to least, but
#' which dimensions take precedence is controlled by the ordering of the joinKeys
#' character vector.
#'
#' The first element of joinKeys should be the main category that is being split,
#' i.e. if we are expanding a sector splits file the first element should be "Sector",
#' and should not contain the wildcard "ALL".
#'
#' @param spec data.frame containing Sector, Industry, Asset columns and others
#' @param toCover data.frame containing Sector, Industry, Asset columns denoting the required coverage
#' @param joinKeys vector denoting the order of precedence for expanding Sector, Industry, Asset, e.g. c("Sector", "Industry", "Asset"). See Details.
#' @return the spec data.frame with any "ALL" values expanded to meet the provided coverage.
#' @export
expandSpec <- function(spec, toCover, joinKeys) {
  reqCols <- c("Sector", "Industry", "Asset")
  stopifnot(all(reqCols %in% colnames(spec)))
  stopifnot(all(reqCols %in% colnames(toCover)))
  stopifnot(length(setdiff(reqCols, joinKeys)) == 0)
  # The first joinKey (i.e. the *last* join) must contain a proper value, not "ALL"
  if ("ALL" %in% spec[[joinKeys[1]]]) stop("The first joinKey must not contain the \"ALL\" wildcard.")
  # The coverage must not contain "ALL" values in the key columns
  if ("ALL" %in% c(toCover$Sector, toCover$Industry, toCover$Asset)) stop("The coverage must not contain \"ALL\" in Sector/Industry/Asset values.")

  # Remove any dplyr groupings applied to input data.frames
  spec <- dplyr::ungroup(spec)
  toCover <- dplyr::ungroup(toCover)

  # Reduce columns of coverage file to just the important ones and remove duplicates
  toCover <- dplyr::select(toCover, Sector, Industry, Asset) %>% distinct()

  # Recursively expand specs. Split with most detailed join first, then make
  # the join more general and repeat.
  # specAcc accumulates each result
  .expandSpec <- function(joinKeys, spec, toCover, specAcc){

    # If we have run out of joinKeys return the accumulated spec
    # TODO: we could also terminate early if nrow(toCover) == 0
    if (length(joinKeys) == 0) {
      return(specAcc)
    } else {
      # Create spec rows where they match the coverage
      specRows <- dplyr::inner_join(spec, toCover, by = joinKeys)

      # Call expandOneSplit again with updated datasets. With each call we want to:
      #   - make the joining more generic, i.e. remove a key
      #   - remove specs we've already expanded
      #   - remove specs cols we've already joined by (to prevent colname clashes in subsequent joins)
      #   - remove coverage we've already expanded
      #   - add the specRows to the accumulator

      .expandSpec(
        joinKeys = joinKeys[-length(joinKeys)],                # Remove spent key
        spec = spec %>%
          filter(rowSums(select(spec, one_of(joinKeys)) == "ALL") > 0) %>%                 # Remove applied specs
          dplyr::select(-one_of(joinKeys[length(joinKeys)])),  # Remove spent column

        toCover = anti_join(toCover, spec, by = joinKeys),   # Remove captured coverage

        specAcc = bind_rows(specAcc, specRows)               # Add specRows to the accumulator
      )
    }
  }

  # Make initial call, with an empty accumulator
  result <- .expandSpec(joinKeys, spec, toCover, specAcc = NULL)

  return(result)
}

