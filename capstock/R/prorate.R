#' Prorate the specified aggregated category down in to the specified sub-categories
#' per the provided ratios
#'
#' @param .data A data.frame
#' @param column The column containing the aggregated category
#' @param aggregated_category The name of the aggregated category
#' @param values Columns containing the values that need to be prorated
#' @param ratios Named vector where each entry is a sub-category & ratio combination.
#' Or a data.frame where each row is a time series of ratios for each sub-category.
#' Sub-categories are specified in a separate column. Ratio's must sum to 1.
#' @param keepCategory boolean value for if we want to keep the aggregated category (TRUE)
#' or not (FALSE, default)
#'
#' @return A data.frame
#' @importFrom magrittr "%>%"
#' @export
prorateDown <- function(.data, column, aggregated_category, values, ratios,
                        keepCategory=FALSE){
  if(any(class(ratios) == "data.frame")){
    result <- prorateDownWithTimeSeries(.data, column, aggregated_category,
                                        values, ratios, keepCategory)
  }else if(class(ratios) == "numeric"){
    # convert named vector into a data frame with time series
    stopifnot(sum(ratios) == 1)
    rats <- tibble::tibble(Cat = names(ratios))
    colnames(rats) <- column
    rats[, values] <- matrix(rep(ratios, length(values)), nrow=length(ratios))
    result <- prorateDownWithTimeSeries(.data, column, aggregated_category,
                                        values, rats, keepCategory)
  }else{
    stop("Please specify the ratio's in a valid format - either a data frame or a named vector")
  }

  return(result)
}


#' Prorate the specified aggregated category down in to the specified sub-categories
#' per the provided ratios
#'
#' @param .data A data.frame
#' @param column The column containing the aggregated category
#' @param aggregated_category The name of the aggregated category
#' @param values Columns containing the values that need to be prorated
#' @param ratios A data.frame where each row is a time series of ratios for each
#' sub-category. Sub-categories are specified in a separate column. Ratio's must
#' sum to 1.
#' @param keepCategory boolean value for if we want to keep the aggregated category (TRUE)
#' or not (FALSE, default)
#'
#' @return A data.frame
prorateDownWithTimeSeries <- function(.data, column, aggregated_category, values, ratios,
                        keepCategory=FALSE){
  # validation checks
  stopifnot(column %in% colnames(.data))
  stopifnot(aggregated_category %in% .data[[column]])
  stopifnot(column %in% colnames(ratios))
  stopifnot(values %in% colnames(.data))
  stopifnot(values %in% colnames(ratios))
  # check that all ratio's must sum to 1
  stopifnot(sum(colSums(ratios[, values])==1) == length(values))

  .data[[column]] <- as.character(.data[[column]])

  # select the rows that contain the aggregated category
  temp <- .data[.data[[column]]==aggregated_category, , drop=FALSE]
  # replicate the temporary data for each disaggregated category (specified in ratios)
  temp <- temp[rep(seq_len(nrow(temp)), each=nrow(ratios)),]
  # replicate the disaggregated categories and assign it in place of the aggregated category
  disaggregated_categories <- rep(ratios[[column]], length.out=nrow(temp))
  temp[, column] <- as.character(disaggregated_categories)

  # specify a function to disaggregate each value in the time series
  # the function takes in a data frame with the values and multiplies it
  # (element-wise) with the ratios. This will only work if both the data frame
  # and the ratios are of the same dimensions hence the above replications.
  myFunction <- function(df){
    df[, values] <- df[, values] * ratios[, values]
    return(df)
  }

  # apply the disaggregation and add back on to the original data
  # filter the original data if we don't want to keep the aggregated category
  if(!keepCategory) .data <- .data[.data[, column]!=aggregated_category, ]

  # determine the other grouping columns
  columns <- setdiff(colnames(.data), c(column, values))
  # there must be other grouping columns
  stopifnot(length(columns)!=0)

  # now apply the disaggregation
  result <- temp %>%
    # group by the other grouping columns
    dplyr::group_by_(.dots = columns) %>%
    # apply the function
    dplyr::do(myFunction(.)) %>%
    dplyr::ungroup() %>%
    # add back to the original data
    dplyr::bind_rows(.data)

  return(result)
}
