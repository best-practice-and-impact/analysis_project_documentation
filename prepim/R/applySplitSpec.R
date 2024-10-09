#' Apply a split specification to data and apportion its values.
#'
#' Split a category (a Sector, Industry, or Asset) into smaller
#' sub-categories and divide its values between the sub-categories using the
#' provided proportions.
#'
#' @section Split Specification File:
#' The splits spec CSV file should have Sector, Industry, Asset, Split columns,
#' and additional columns for each Period containing the split proportions.
#' E.g. for a single sector split definition:
#'
#' \tabular{llllrrr}{
#' Sector \tab Industry \tab Asset \tab Split \tab Y1990Q1 \tab Y1990Q2 \tab ...\cr
#' S.1PT \tab ALL \tab ALL \tab S.11PR \tab 0.8 \tab 0.8 \tab ...\cr
#' S.1PT \tab ALL \tab ALL \tab S.14   \tab 0.1 \tab 0.1 \tab ...\cr
#' S.1PT \tab ALL \tab ALL \tab S.15   \tab 0.1 \tab 0.1 \tab ...
#' }
#'
#' A data.frame in the same format can be provided instead of a path to allow for
#' creation of split specifications on-the-fly.
#'
#' Split proportions should be in the range 0-1 and should sum to 1 when grouped
#' by Sector, Industry, and Asset. Any splits with proportions of zero are removed.
#'
#' The \emph{ALL} keyword can be used instead of specific Sectors, Industries, or
#' Assets in the specification file. Where \emph{ALL} occurs it will be expanded
#' to match the coverage in the \code{.data}.
#'
#' @section Split Matching:
#'
#' Splits are matched to the data by matching on all standard columns:
#' \code{Sector}, \code{Industry}, \code{Asset}, \code{Period}. Any data that
#' does not have a matching entry in the split specification is passed through
#' unchanged.
#'
#' Splits will first be matched to the coverage using the most detailed
#' specification, where all dimensions of joinKeys match, i.e. by
#' Sector, Industry, and Asset. Any remaining data will be matched more generally
#' by removing a joinKey, e.g. by matching on only Sector and Industry. Finally,
#' The splits will be matched by just Sector.
#'
#' The order of these iterations is controlled by the order of \code{joinKeys},
#' with each iteration removing the last element of \code{joinKeys} to make the
#' join more general.
#'
#' @param .data data.frame of GFCF data to be split.
#' @param spec String path to a CSV file containing split specification or a data.frame in the same format.
#' @param existingCategory String denoting the variable to split in .data. Common
#' to both \code{.data} and \code{splits}, e.g. "Sector".
#' @param newCategory String denoting the variable in the split specification containing the more detailed
#' level of \code{existingCategory}, e.g. "Split". Note, no spaces are allowed.
#' @param joinKeys character vector of dimensions defining matching priority, e.g. c("Asset", "Industry", "Sector").
#' See Split Matching for details.
#' @export
#' @return data.frame with matching rows split out as per the proportions.
applySplitSpec <- function(.data, spec, existingCategory, newCategory, joinKeys, tol) {

  # Check inputs
  if (grepl(" ", newCategory, fixed = TRUE)) stop("newCategory must not contain spaces.")
  if (!(is.data.frame(spec) || is.character(spec))) stop("Unrecognised spec format.")
  if (length(setdiff(joinKeys, c("Sector", "Industry", "Asset"))) != 0) {
    stop("joinKeys must be a character vector containing Sectory, Industry, and Asset.")
  }

  # Read spec from file if necessary
  if (is.character(spec)) {
    splits <- extractSplitSpec(spec)
  } else {
    splits <- parseSplitsSpec(spec)
  }

  # Remove any dplyr groupings applied to input data.frames
  .data <- dplyr::ungroup(.data)
  splits <- dplyr::ungroup(splits)

  # Expand any "ALL" in splits spec to match coverage  in .data, and tidy
  splits <- expandSplitSpec(splits, toCover = .data, joinKeys = joinKeys, tidy = TRUE)

  # Apply the splits
  result <- applySplits(.data = .data,
              splits = splits,
              existingCategory = existingCategory,
              newCategory = newCategory,
              tol = tol)
  return(result)
}


#' Expand Splits Specification
#'
#' Expands a splits specification to match the coverage in the data provided.
#'
#' @param spec data.frame with one row per split, containing Sector, Industry, and Asset columns.
#' @param toCover data.frame with every unique combination of Sector, Industry, and Asset required.
#' @param joinKeys character vector of dimensions defining matching priority, e.g. c("Sector", "Industry", "Asset").
#' @param tidy boolean denoting whether to tidy the resulting data.frame.
#' This will gather the period columns into one called Period, i.e. change the result from wide to long.
#' @return data.frame with one row per split for each Sector/Industry/Asset and Period if tidy = TRUE.
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
#' i.e. if we are expanding a sector splits file the first element should be "Sector".
#'
#' \code{toCover} is deduplicated before matching so for convenience a GFCF file
#' could be provided to create splits matching its coverage. Any splits defined
#' in the specification that are unmatched in the coverage are ignored.
#'
#' Any splits with proportions of zero are removed.
expandSplitSpec <- function(spec, toCover, joinKeys, tidy = TRUE) {

  # Remove any dplyr groupings applied to input data.frames
  spec <- dplyr::ungroup(spec)
  toCover <- dplyr::ungroup(toCover)

  # Reduce columns of coverage file to just the important ones and remove duplicates
  toCover <- dplyr::select(toCover, Sector, Industry, Asset) %>% distinct()

  # Drop all split rows containing zero values for all Periods
  toDrop <- rowSums(abs(dplyr::select(spec, -one_of(c("Sector", "Industry", "Asset", "Split"))))) == 0
  spec <- spec[!toDrop, ]

  # Recursively expand splits. Split with most detailed join first, then make
  # the join more general and repeat.
  expandSplit <- function(joinKeys, spec, toCover, splitsAcc){

    # If we have run out of joinKeys return the accumulated splits
    # TODO: we could also terminate early if nrow(toCover) == 0
    if (length(joinKeys) == 0) {
      return(splitsAcc)
    } else {
      # Create splits where they match the coverage
      splits <- dplyr::inner_join(spec, toCover, by = joinKeys)

      # Call expandSplit again with updated datasets. With each call we want to:
      #   - make the joining more generic, i.e. remove a key
      #   - remove specs we've already expanded
      #   - remove specs cols we've already joined by (to prevent colname clashes in subsequent joins)
      #   - remove coverage we've already expanded
      #   - add the splits to the accumulator

      expandSplit(

        joinKeys = joinKeys[-length(joinKeys)],                # Remove spent key

        spec = spec %>%
          filter(rowSums(select(spec, one_of(joinKeys)) == "ALL") > 0) %>%                 # Remove applied specs
          dplyr::select(-one_of(joinKeys[length(joinKeys)])),  # Remove spent column

        toCover = anti_join(toCover, splits, by = joinKeys),   # Remove captured coverage

        splitsAcc = bind_rows(splitsAcc, splits)               # Add splits to the accumulator
      )
    }
  }

  # Make initial call, with an empty accumulator
  result <- expandSplit(joinKeys, spec, toCover, splitsAcc = NULL)

  # Tidy
  if (tidy) {
    # Assume all non-standard columns are Periods
    periodCols <- setdiff(colnames(result), c("Sector", "Industry", "Asset", "Split"))
    result <- tidyr::gather_(result, key = "Period", value = "Proportion", periodCols)
  }

  return(result)
}


#' Internal: Split a category into smaller categories and pro-rate its values
#'
#' Splits a category (i.e. within a Sector, Industry, or Asset) into smaller
#' sub-categories and divides its values between the sub-categories using the
#' supplied proportions.
#'
#' @details \code{.data} is typically a GFCF dataset, e.g. as imported by
#' extractBalAccountsCord, containing \code{Sector}, \code{Industry}, \code{Asset},
#' \code{Prices}, \code{Period}, and \code{Value}.
#'
#' \code{splits} is a data.frame defining split proportions with one row per split,
#' containing a \code{Sector}, \code{Industry}, \code{Asset}, \code{Period}, and
#' \code{Proportion}, with an additional column denoting the smaller categories
#' e.g. "Sector_new".
#'
#' Splits are matched to the data by matching on all standard columns:
#' \code{Sector}, \code{Industry}, \code{Asset}, \code{Period}.
#' The existing category is then replaced by \code{newCategory} and the
#' proportions are applied.
#'
#' Only one category can be split at a time for a particular splits definition,
#' e.g. a splits definition can be used to split Sectors into smaller Sectors
#' or Assets into smaller Assets, but not both at once.
#'
#' @param .data data.frame of GFCF data.
#' @param splits data.frame with one row per proportion, see details.
#' @param existingCategory String denoting the variable to split in .data. Common
#' to both \code{.data} and \code{splits}, e.g. "Sector".
#' @param newCategory String denoting the variable containing the more detailed
#' level of \code{existingCategory} in \code{splits}, e.g. "Sector_new". Note,
#' no spaces are allowed.
#' @return data.frame with matching rows split out as per the proportions.
applySplits <- function(.data, splits, existingCategory, newCategory, tol = 0.01) {

  # Check expected columns are present
  # "Prices" may be present in .data, but is not wanted for later joins.
  dataHeadings <- c("Sector", "Industry", "Asset", "Period", "Value")
  flog.debug("Checking .data for expected column headings.")
  if (!all(dataHeadings %in% colnames(.data))) {
    msg <- paste(".data must contain columns", paste(dataHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }
  splitsHeadings <- c("Sector", "Industry", "Asset", "Period", "Proportion")
  flog.debug("Checking splits for expected column headings.")
  if (!all(splitsHeadings %in% colnames(splits))) {
    msg <- paste("splits must contain columns", paste(splitsHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  flog.debug("Checking .data for existingCategory.")
  if (!existingCategory %in% colnames(.data)) {
    msg <- paste(".data does not contain column:", existingCategory)
    flog.fatal(msg)
    stop(msg)
  }
  flog.debug("Checking for valid newCategory name.")
  if (newCategory != make.names(newCategory)) {
    msg <- paste0("'", newCategory, "' is an invalid variable name.")
    flog.fatal(msg)
    stop(msg)
  }
  flog.debug("Checking splits for newCategory.")
  if (!newCategory %in% colnames(splits)) {
    msg <- paste("splits does not contain column:", newCategory)
    flog.fatal(msg)
    stop(msg)
  }


  # Add newCategory as a valid heading
  splitsHeadings <- c(splitsHeadings, newCategory)

  # Drop any columns not required in splits in case they clash with .data
  splits <- dplyr::select(splits, one_of(splitsHeadings))

  # Record checksum
  cs <- sum(.data$Value)

  # Combine .data and splits
  x <- left_join(.data, splits, by = c("Sector", "Industry", "Asset", "Period"))

  # Set aside the unmatched rows where no splits were defined
  unmatched <- dplyr::filter(x, is.na(Proportion)) %>%
    select(-one_of("Proportion", newCategory))  # remove the joined columns

  # Get just matched rows
  matched <- dplyr::filter(x, !is.na(Proportion))

  # Apply proportions and drop the Proportions column
  matched <- dplyr::mutate(matched, Value = Value * Proportion) %>%
    dplyr::select(-Proportion)

  # matched will now have cols for existingCategory and newCategory
  # Remove the existingCategory
  matched <- dplyr::select(matched, -one_of(existingCategory))
  # Rename the newCategory col to the existingCategory
  matched <- dplyr::rename_(matched, .dots = setNames(newCategory, existingCategory))

  # Recombine matched and unmatched
  x <- bind_rows(unmatched, matched)

  flog.info("Checking total quantity is unchanged after splits.")
  #tol <- 0.0001 #replacing with dynamic tolerance parameter
  if (abs(sum(x$Value) - cs) > tol) {
    msg <- paste("Some quantities have been lost or gained after splitting. Before:", cs,
                 "After:", sum(x$Value))
    flog.fatal(msg)
    stop(msg)
  }

  # Check that where splits are defined, we do not have missing
  # coverage in the splits file for that category. It is possible we may
  # not want to apply the same splits to every Sector/Industry for a given Asset
  # but it is worth issuing a warning in case this was unintended
  existingCatLevels <- unique(splits[[existingCategory]])
  maybeMissingCoverage <- filter_(unmatched,
                                  lazyeval::interp(~col %in% existingCatLevels,
                                                   col = as.name(existingCategory))) %>%
    select(Sector, Industry, Asset) %>%
    distinct() %>%
    arrange(Sector, Industry, Asset)
  flog.info("Checking for missing coverage where splits are defined.")
  if (nrow(maybeMissingCoverage) > 0) {
    flog.warn(paste("Possible missing coverage. Splits are specified for",
                    paste(existingCatLevels, collapse = ","),
                    "but not for the following combinations present in the data. Did you intend to split these?"),
              as.data.frame(maybeMissingCoverage), capture = TRUE)
    warning("Possible missing coverage.")
  }

  x <- arrange_(x, dataHeadings)

  return(x)

}


