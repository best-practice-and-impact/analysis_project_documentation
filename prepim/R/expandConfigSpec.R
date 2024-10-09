#' Expand Configuration Specification
#'
#' Expands a configuration specification to match the provided coverage.
#'
#' @param spec data.frame with one row per config specification, usually containing
#' Sectory, Industry, and Asset columns, plus colums of named arguments to pimir::pimConfig.
#' @param toCover data.frame with every unique combination of Sector, Industry, and Asset required.
#' @param joinKeys character vector of dimensions defining matching priority, e.g. c("Asset", "Industry", "Sector").
#' @return data.frame with one row for each Sector/Industry/Asset combination in \code{toCover}.
#'
#' @details The configs will first be matched to the coverage using the most detailed
#' specification, where all joinKeys match, e.g. by Asset, Industry, and Sector
#' Then configs will be matched more generally by removing a joinKey, e.g. by
#' matching on only Asset and Industry. Finally, The configs will be matched
#' by just Asset.
#'
#' The ordering of the matching will always go from most specific to least, but
#' which dimensions take precedence is controlled by the ordering of the joinKeys
#' character vector.
#'
#' The first element of joinKeys should usually be Asset.
#'
#' \code{toCover} is deduplicated before matching so for convenience a GFCF file
#' can be provided to create configs matching its coverage. Any configs defined
#' in the specification that are unmatched in the coverage are ignored.
#'
#' @export
expandConfigSpec <- function(spec, toCover, joinKeys) {

  # Remove any dplyr groupings applied to input data.frames
  spec <- dplyr::ungroup(spec)
  toCover <- dplyr::ungroup(toCover)

  # Reduce columns of coverage file to just the important ones and remove duplicates
  toCover <- dplyr::select(toCover, Sector, Industry, Asset) %>% distinct()

  # Recursively expand configs by joining, with most detailed join first, then make
  # the join more general and repeat.
  expandConfig <- function(joinKeys, spec, toCover, acc){

    # If we have run out of joinKeys return the accumulated configs
    # We could also terminate early if nrow(toCover) == 0
    if (length(joinKeys) == 0) {
      return(acc)
    } else {
      # Create config rows where they match the coverage
      configs <- dplyr::inner_join(spec, toCover, by = joinKeys)

      # Call expandSplit again with updated datasets. With each call we want to:
      #   - make the joining more generic, i.e. remove a key
      #   - remove configs we've already expanded. We need to remember that they
      #     may not match the coverage but they still need to be removed.
      #     We can filter out where all the join keys are not "ALL"
      #   - remove specs cols we've already joined by (to prevent colname clashes in subsequent joins)
      #   - remove coverage we've already expanded
      #   - add the configs to the accumulator

      expandConfig(

        joinKeys = joinKeys[-length(joinKeys)],                            # Remove spent key

        spec = spec %>%
          filter(rowSums(select(spec, one_of(joinKeys)) == "ALL") > 0) %>% # Remove applied configs
          dplyr::select(-one_of(joinKeys[length(joinKeys)])),              # Remove spent column

        toCover = anti_join(toCover, configs, by = joinKeys),              # Remove captured coverage

        acc = bind_rows(acc, configs)               # Add result to the accumulator
      )
    }
  }

  # Make initial call, with an empty accumulator
  result <- expandConfig(joinKeys, spec, toCover, acc = NULL)

  # ----------------- Check results --------------------------------------------

  # Warn if any of the intended coverage wasn't covered by the spec
  unmatched <- anti_join(toCover, result, by = c("Sector", "Industry", "Asset")) %>%
    select(Sector, Industry, Asset)
  if (nrow(unmatched) > 0) {
    msg <- "Some of the required coverage was unmatched by the specifications."
    flog.warn(paste(msg, "Series:"), data.frame(unmatched), capture = TRUE)
    warning(msg)
  }

  # If result has more rows than intended coverage, something went wrong
  if (nrow(result) > nrow(toCover)) {
    msg <- "Result has more rows than intended coverage. Spec data could be invalid or contain duplicates."
    flog.fatal(msg)
    stop(msg)
  }

  return(result)
}
