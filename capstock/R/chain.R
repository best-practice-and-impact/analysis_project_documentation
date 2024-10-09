#' Calculate constrained chain volume measure.
#'
#' Calculate the constrained chained volume measure based on current year and
#' previous year prices (data is assumed to be quarterly).
#'
#' CYP values must not contain NAs.
#'
#' The series is benchmarked to the yearly figures. Benchmark cannot process
#' zero values so, where present, a small adjustment is made (+0.001) and removed
#' after benchmarking.
#'
#' @param .data A data.frame with the Year, current year prices and previous year
#' prices
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#' @param chainType Either Stock or Flow which determines the way the Annualised CVM
#' is calculated
#'
#' @return A data.frame as in the given input data with the addition of a CVM column
#' @importFrom magrittr "%>%"
#' @export
chain <- function(.data, lastCompleteYear = 2016, cypColumn = "CYP", pypColumn = "PYP",
                  chainType = c("Stock", "Flow"), benchType = 4){

  ##################################################################################################################################################
  # .data <- toChain2[1,]
  # .data <- unnest(.data)
  # chainType <- 'Stock'
  # cypColumn <- "ConsumptionOfFixedCapitalCYP"
  # pypColumn <- "ConsumptionOfFixedCapitalPYP"

  # cypColumn <- "NetStockCYP"
  # pypColumn <- "NetStockPYP"

  # cypColumn <- "GrossStockCYP"
  # pypColumn <- "GrossStockPYP"
  ##################################################################################################################################################
  # input validation
  chainType <- match.arg(chainType)

  columns <- colnames(.data)
  stopifnot(cypColumn %in% columns)
  stopifnot(pypColumn %in% columns)
  stopifnot("Year" %in% columns)
  stopifnot(lastCompleteYear %in% .data$Year)
  if (any(is.na(.data[cypColumn]))) stop("CYP values must not be NA.")

  # add quarter values, probably already there but this is easier
  withQuarter <- .data %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(Quarter=dplyr::row_number(Year)) %>%
    dplyr::ungroup()

  # calculate the scaling factor and unconstrained CVM
  quarterlyCVM <- calcScalingFactor(withQuarter, lastCompleteYear, cypColumn, pypColumn)
  quarterlyCVM <- calcUnconstrainedCVM(quarterlyCVM, lastCompleteYear, cypColumn, pypColumn)

  # calculate the annual cvm, based on the chain type
  annualCVM <- switch(chainType,
                      "Stock" = calcAnnualCVMStock(withQuarter, lastCompleteYear,
                                                   cypColumn, pypColumn),
                      "Flow" = calcAnnualCVMFlow(withQuarter, lastCompleteYear,
                                                 cypColumn, pypColumn))
  #write_rds(quarterlyCVM, "quarterlyCVM.Rds")
  #write_rds(annualCVM, "annualCVM.Rds")

  # finally feed the quarterly and annual values into the benchmarking function
  benchType <- switch(chainType,
                      "Stock" = 4, #"avg",
                      "Flow" = "sum")
  #a <- read_rds("D:/CVMOutput/quarterlyCVM.Rds")
  #.data[, "CVM"] <- read_rds("D:/CVMOutput/benchMarkData.Rds")
  #View(.data)
  .data[, "CVM"] <- bench::benchmark0(quarterlyCVM$UnconstrainedCVM,
                                     annualCVM$AnnualCVM, periodicity = 4,
                                     type = benchType,
                                     toleranceThreshold = 1e8)

  return(.data)

  ########detach("package:capstock", unload=TRUE)
}


#' Calculate the scaling factor (a function of the current year and previous year
#' prices). This is used to calculate the unconstrained CVM.
#'
#' @param .data A data.frame with the Year, current year prices and previous year
#' prices
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#'
#' @return A data.frame with the additional ScalingFactor column
calcScalingFactor <- function(.data, lastCompleteYear, cypColumn = "CYP",
                              pypColumn = "PYP"){
  # .data <- withQuarter
  # lag the CYP and PYP data as we need next quarter's data
  result <- .data %>%
    dplyr::mutate_(CYPlead = lazyeval::interp("dplyr::lead(col)", col=as.name(cypColumn)),
                   PYPlead = lazyeval::interp("dplyr::lead(col)", col=as.name(pypColumn))) %>%
    # select only the quarter 3 data and all data before the last complete year
    dplyr::filter(Quarter==3, Year < lastCompleteYear) %>%
    dplyr::arrange(dplyr::desc(Year))

  # what is left is a loop over each 3rd quarter to calculate the scaling factor
  # the scaling factor is a function of next year's scaling factor, CYP and PYP
  # values. For the last value in our dataset, next year's scaling factor equals 1
  # Where CYP and PYP are zero, the scaling factor is 1
  # result$ScalingFactor <- dplyr::if_else(result$PYPlead == 0, 1, result$CYPlead / result$PYPlead)
  result$ScalingFactor <- sfDivide(result$CYPlead, result$PYPlead)

  result$ScalingFactor <- cumprod(result$ScalingFactor)    #### Answer of the previous row times (*) the value of the current row

  # join the data back onto the original data
  # all missing quarters will be filled with the value from the 3rd quarter
  result <- result %>%
    dplyr::select(Year, Quarter, ScalingFactor) %>%
    dplyr::right_join(.data, by = c("Year", "Quarter")) %>%
    # note that the first year will have no PYP and hence no ScalingFactor
    # this year too will get the ScalingFactor from next year's 3rd quarter
    tidyr::fill(ScalingFactor, .direction="up")

  # additional fix, any values after and including the last complete year
  # get a ScalingFactor of 1
  result[result$Year >= lastCompleteYear, "ScalingFactor"] <- 1
   # this also applies to the 4th quarter directly preceding the last complete year
  result[result$Year >= (lastCompleteYear-1) & result$Quarter==4,"ScalingFactor"] <- 1

  return(result[, c("Year", "Quarter", cypColumn, pypColumn, "ScalingFactor")])
  # quarterlyCVM <- result[, c("Year", "Quarter", cypColumn, pypColumn, "ScalingFactor")]
}


#' Calculates the unconstrained CVM which is a function of the current year
#' prices, previous year prices and a scaling factor
#'
#' @param .data A data.frame with the Year, current year prices,  previous year
#' prices and scaling factors
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#'
#' @return A data.frame with the additional UnconstrainedCVM column
calcUnconstrainedCVM <- function(.data, lastCompleteYear, cypColumn = "CYP",
                                 pypColumn = "PYP"){
  # .data <- quarterlyCVM
  result <- .data %>%
    # there are two possibilities for the unconstrained cvm
    # one based on the CYP and one on the PYP
    # calculating both here and then filtering in the next step
    dplyr::mutate_(CYPScalingFactor = lazyeval::interp("col * ScalingFactor",
                                                       col=as.name(cypColumn)),
                  PYPScalingFactor = lazyeval::interp("col * ScalingFactor",
                                                      col=as.name(pypColumn))) %>%
    # using the case_when below which is less verbose as a nested if-else
    dplyr::mutate(UnconstrainedCVM = dplyr::case_when(
      is.na(.$PYPScalingFactor) ~  .$CYPScalingFactor,
      .$Quarter == 4 ~ .$CYPScalingFactor,
      TRUE ~ .$PYPScalingFactor)) %>%
    # this wouldn't work in the case_when so placing it outside
    dplyr::mutate(UnconstrainedCVM = ifelse(Year >= lastCompleteYear,
                                            CYPScalingFactor, UnconstrainedCVM)) %>%
    # finally drop unwanted columns
    dplyr::select(-CYPScalingFactor, -PYPScalingFactor)

  return(result)
  # quarterlyCVM <- result
}

#' Calculate the annual CVM where the input time series represent flow values
#'
#' @param .data A data.frame with the Year, current year prices and previous year
#' prices
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#'
#' @return A data.frame where for each year there is a CVM value
calcAnnualCVMFlow <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP"){

  #.data <- withQuarter
  # define the summation here so we can use it in the summarise
  dots <- list(lazyeval::interp(~sum(col), col=as.name(cypColumn)),
               lazyeval::interp(~sum(col), col=as.name(pypColumn)))

  # calculate the total CYP and PYP per year for all years before and including
  # the last complete year
  result <- .data %>%
    dplyr::filter(Year <= lastCompleteYear) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise_(.dots = setNames(dots, c("AnnualCYP", "AnnualPYP"))) %>%
    dplyr::ungroup() %>%
    # then calculate the first part of the annual scaling factor
    dplyr::mutate(AnnualScalingFactor = sfDivide(dplyr::lead(AnnualCYP), dplyr::lead(AnnualPYP))) %>%
    #dplyr::mutate(AnnualScalingFactor =  dplyr::if_else(AnnualPYP == 0, 1, as.numeric(AnnualCYP / AnnualPYP))) %>%
    dplyr::arrange(desc(Year))

  # the scaling factor depends on next year's value
  # the last complete year receives a scaling factor of 1
  result$AnnualScalingFactor[1] <- 1
  result$AnnualScalingFactor <- cumprod(result$AnnualScalingFactor)

  # finally the Annual CVM is the Annual CYP multiplied with Annual Scaling Factor
  result <- result %>%
    dplyr::mutate(AnnualCVM = AnnualCYP * AnnualScalingFactor) %>%
    dplyr::select(Year, AnnualCVM) %>%
    dplyr::arrange(Year)

  return(result)
  # annualCVM <- result
}


#' Calculate the annual CVM where the input time series represent stock values
#'
#' @param .data A data.frame with the Year, current year prices and previous year
#' prices
#' @param lastCompleteYear integer value for the last year in which all quarters were present
#' @param cypColumn Name of the column for the current year prices (defaults to CYP)
#' @param pypColumn Name of the column for the previous year prices (defaults to PYP)
#'
#' @return A data.frame where for each year there is a CVM value
calcAnnualCVMStock <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP"){

  # .data <- withQuarter
  result <- .data %>%
    dplyr::filter(Year <= lastCompleteYear, Quarter==4) %>%
    dplyr::mutate_(AnnualCYP = cypColumn, AnnualPYP = pypColumn) %>%
    # then calculate the first part of the annual scaling factor
    #dplyr::mutate(AnnualScalingFactor =  sfDivide(dplyr::lead(AnnualCYP), dplyr::lead(AnnualPYP))) %>%
    dplyr::mutate(AnnualScalingFactor =  dplyr::if_else(AnnualPYP == 0, 1, as.numeric(AnnualCYP / AnnualPYP))) %>%
    dplyr::arrange(desc(Year))

  # the scaling factor depends on next year's value
  # the last complete year receives a scaling factor of 1
  result$AnnualScalingFactor[1] <- 1
  result$AnnualScalingFactor <- cumprod(result$AnnualScalingFactor)

  # finally the Annual CVM is the Annual CYP multiplied with Annual Scaling Factor
  result <- result %>%
    dplyr::mutate(AnnualCVM = AnnualCYP * AnnualScalingFactor) %>%
    dplyr::select(Year, AnnualCVM) %>%
    dplyr::arrange(Year)

  return(result)
  # annualCVM <- result
}


# Utility function to calculate scaling factor.
# Guards against creating scaling factors of zero or infinity by replacing with 1
# Note we can still have zero divided by a positive value
sfDivide <- function(numerator, denominator) {
  dplyr::if_else(denominator == 0, 1,
                 as.numeric(numerator / denominator))
}

