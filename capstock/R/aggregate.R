#' Aggregate levels of a categorical variable.
#'
#' Reduce the levels (unique values) of a categorical variable and aggregate
#' the reduced levels such that no duplicates occur.
#' The latter point is important because it is assumed the input data has no duplicates.
#'
#' @param .data A data.frame
#' @param column The column containing the categorical data
#' @param values The numeric column(s) that will be aggregated
#' @param levels A named character vector specifying the new value for any levels
#' that need updating (format new=old)
#'
#' @return A data.frame
#' @importFrom forcats fct_recode
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' aggregateLevels(iris, "Species",
#'      c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'      c("alien"="setosa", "alien"="virginica"))
aggregateLevels <- function(.data, column, values, levels, keepLowLevel = FALSE){

  # now collecting the column names for the groupby
  columns <- colnames(.data)

  stopifnot(column %in% columns)
  stopifnot(sum(values %in% columns) == length(values))

  columns <- columns[!(columns %in% values)]

  # convert the given column to a factor
  .data[, column] <- factor(.data[[column]])
  # now reduce the levels of the factor as defined by levels
  .data[, column] <- forcats::fct_recode(.data[[column]], levels)

  # dplyr doesn't like the column headers to start with a number
  # these are surrounded with backticks
  isNumbers <- !is.na(suppressWarnings(as.numeric(substr(values,1,2))))
  values[isNumbers] <- paste0('`', values[isNumbers], '`')

  # now we do the aggregation
  result <- .data %>%
    dplyr::group_by_(.dots=columns) %>%
    dplyr::summarise_each_(dplyr::funs_("sum", args=list("na.rm"=TRUE)), vars=values) %>%
    dplyr::ungroup()

  if(keepLowLevel){
    # add result on to original data
    result[, paste0(column, "Aggregated")] <- result[[column]]
    result[, column] <- NA
    .data[, paste0(column, "Aggregated")] <- NA
    result <- dplyr::bind_rows(.data, result)
  }

  return(result)
}

#' Aggregate up hierarchy.
#'
#' Aggregate the data up an hierarchy from the given low level column to the given
#' high level column. Both columns must be categorical data.
#'
#' @param .data A data.frame
#' @param highLevel The high level column to which the data will be aggregated
#' @param lowLevel The low level column from which the data will be aggregated
#' @param values The columns that will be aggregated
#' @param keepLowLevel logical indicator
#'
#' @return A data.frame
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' irisXL <- iris  %>% mutate(Extra=sample(c("one", "two"), nrow(iris), replace=TRUE))
#' res <- aggregateHierarchy(irisXL, "Extra", "Species",
#'    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
aggregateHierarchy <- function(.data, highLevel, lowLevel, values,
                               keepLowLevel = FALSE){
  # collect column names for the groupby
  columns <- colnames(.data)

  stopifnot(highLevel %in% columns)
  stopifnot(lowLevel %in% columns)
  stopifnot(values %in% columns)

  columns <- columns[!(columns %in% c(values, lowLevel))]

  # dplyr doesn't like the column headers to start with a number
  # these are surrounded with backticks
  isNumbers <- !is.na(suppressWarnings(as.numeric(substr(values,1,2))))
  values[isNumbers] <- paste0('`', values[isNumbers], '`')

  # aggregate the data
  result <- .data %>%
    dplyr::group_by_(.dots=columns) %>%
    dplyr::summarise_each_(dplyr::funs_("sum", args=list("na.rm"=TRUE)), vars=values) %>%
    dplyr::ungroup()


  if(keepLowLevel){
    # add result on to original data
    # forcing this to character to prevent warning on factor coercion
    .data[[lowLevel]] <- as.character(.data[[lowLevel]])
    result[, lowLevel] <- "TOTAL"
    result <- dplyr::bind_rows(.data, result)
  }

  return(result)
}

#'  Aggregate up a hierarchy table (implementation).
#'
#'  Aggregate the data up an hierarchy as specified in the given hierarchy table.
#'  The data is assumed to be at the lowest level within a category (e.g. sector)
#'  that resides in the given column. The columns in the values argument are then
#'  aggregated to the given target level
#'
#' @param .data A data.frame
#' @param column The column name of the category for which the hierarchy is specified
#' @param hierarchyTable A data.frame outlining the hierarchy of the category
#' @param targetLevel A column in the hierarchy table
#' @param values The columns that will be aggregated
#'
#' @return A data.frame with the aggregated values added to it
aggregateWithHierarchyTableImp <- function(.data, column, hierarchyTable, targetLevel,
                                       values, keepOriginal){

  allLevels <- colnames(hierarchyTable)
  # assuming the columns are ordered from high to low level
  # keep the target and lowest level
  ht <- hierarchyTable[, c(targetLevel, allLevels[length(allLevels)])]
  # rename to match with the input data
  colnames(ht) <- c(targetLevel, column)

  # coercing to character in case the columns are factors to avoid warning
  .data[[column]] <- as.character(.data[[column]])
  ht[[column]] <- as.character(ht[[column]])

  # join tables
  toAggregate <- suppressMessages(dplyr::left_join(.data, ht))

  # If any categories are unmatched, output a warning
  unmatched <- toAggregate[is.na(toAggregate[, targetLevel]), , drop = FALSE]
  if (nrow(unmatched) > 0) {
  message(paste("The following categories were not found in the hierarchy table:\n",
                paste(unique(unmatched[[column]]), collapse = ", ")))
  }

  # Remove unmatched categories
  toAggregate <- toAggregate[!is.na(toAggregate[, targetLevel]), , drop=FALSE]
  if(nrow(toAggregate)==0){
    stop("No matches found between aggregation levels")
  }

  # aggregate to target level
  result <- aggregateHierarchy(toAggregate, targetLevel, column, values)

  # result will have a column for the target level and the aggregated values
  # we can rename the target level to the original column and bind rows
  dots <- list(lazyeval::interp("as.character(col)", col=as.name(column)))
  result <- dplyr::rename_(result, .dots=setNames(targetLevel, column)) %>%
    dplyr::mutate_(.dots=setNames(dots, column))

  # If retaining original dataset, append it to the aggregated result
  if (keepOriginal) {
    result <- result %>% dplyr::bind_rows(.data)
  }

  return(result)
}

#'  Aggregate up hierarchy table.
#'
#'  Aggregate the data up an hierarchy as specified in the given hierarchy table.
#'  The data is assumed to be at the lowest level within a category (e.g. sector)
#'  that resides in the given column. The numeric \code{values} are then
#'  aggregated to the given target level.
#'
#' @param .data A data.frame
#' @param column The column name of the category for which the hierarchy is specified
#' @param hierarchyTable A data.frame outlining the hierarchy of the category with
#'  columns ordered from lowest to highest detail
#' @param targetLevel A column in the hierarchy table
#' @param values The columns containing numeric values that will be aggregated
#' @param withSubTotals if TRUE subtotals will be added for each level between the
#' lowest level and the target level
#' @param keepOriginal if TRUE the aggregated rows will be added to \code{.data}, if
#'  FALSE then the aggregated rows replace the original data
#'
#' @return A data.frame with same structure as \code{.data}, replaced or appended with aggregated rows
#' @export
aggregateWithHierarchyTable <- function(.data, column, hierarchyTable, targetLevel,
                                        values, withSubTotals = FALSE, keepOriginal = TRUE){
  # warn if hierarchyTable has duplicate rows
  if (anyDuplicated(hierarchyTable) > 0) {
    warning("hierarchyTable contains duplicate rows.")
  }

  if (withSubTotals){
    if (!keepOriginal) {
      warning("keepOriginal set to TRUE (must be TRUE when withSubTotals = TRUE).")
      keepOriginal <- TRUE
    }
    # if subtotals are needed we simply call the same function repeatedly
    hierarchies <- colnames(hierarchyTable)
    # first we find the position of the target level
    indexTargetLevel <- match(targetLevel, hierarchies)
    result <- .data
    # then we loop over all levels between the target level and the lowest level
    # we start at the level that is closest to the lowest level (hence the rev)
    # note we always want to keep the original data in this case in order to build up the result
    for(i in rev(indexTargetLevel:(length(hierarchyTable)-1))){
      result <- aggregateWithHierarchyTableImp(result, column, hierarchyTable,
                                           hierarchies[i], values, keepOriginal)
    }
  }else{
    # else call the function once
    result <- aggregateWithHierarchyTableImp(.data, column, hierarchyTable,
                                         targetLevel, values, keepOriginal)
  }

  return(result)
}
