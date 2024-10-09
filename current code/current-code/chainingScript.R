
chainTogether <- function(.data, pairs = NULL, lastCompleteYear, parallelise = TRUE) {
  
  #.data <-toChain2
  #library("foreach", "%dopar%", "%do%", "magrittr", "%>%", "purrr", "map2", "doSNOW", "parallel")
  
  flog.info(paste0("#### USING CHAININGSCRIPT.R RATHER THAN PACKAGE ####"))
  
  # --- Input Checks -----------------------------------------------------------
  # .data must contain "data" list-column
  #stopifnot("data" %in% colnames(.data))
  # Get the column names from the embedded data list-column
  #dataCols <- colnames(.data$data[[1]])
  
  # Pairs must have colnames of PYP and CYP
  #pairCols <- colnames(pairs)
  #stopifnot("PYP" %in% pairCols)
  #stopifnot("CYP" %in% pairCols)
  #stopifnot("ChainType" %in% pairCols)
  
  # The data list-column within .data must contain all the defined pairs
  #stopifnot(all(pairs$PYP %in% dataCols))
  #stopifnot(all(pairs$CYP %in% dataCols))
  # The data list-column must contain a Year column
  #stopifnot("Year" %in% dataCols)
  
  # --- Set up parallel processing ---------------------------------------------
  #`%doOrDoPar%` <- `%do%`  # define default iterator to be sequential
  # Set up parallel processing for multiple cores (use all except one)
  #if (parallelise) {
  #  `%doOrDoPar%` <- `%dopar%`
  #  availCores <- parallel::detectCores()
  #  nCores <- dplyr::case_when(
  #    is.na(availCores) ~ 1,
  #    availCores > 1 ~ availCores - 1,
  #    TRUE ~ 1)
  #  # Register the cluster
  #  cl <- parallel::makeCluster(nCores)
  #  doSNOW::registerDoSNOW(cl)
  #  # Pass libPath to workers
  #  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  #}
  tryCatch(
    {
      # --- Chain ------------------------------------------------------------------
      # Set up a progress bar
      #pb <- txtProgressBar(min = 0, max = nrow(.data), style = 3)
      #progress <- function(n) setTxtProgressBar(pb, n)
      # Create options list for foreach
      #opts <- list(progress = progress)
      #i <- 1
      #p <- 1
      
      for (i in 1:1)#nrow(.data))
      {
        intDat <- unnest(.data[i,])
        cvmCol <- intDat %>% select(Sector,Industry, Asset, Period)
        intDat <- intDat %>% group_by(Sector, Industry, Asset) %>% nest(.key = "data")
        for (p in 1:nrow(pairs))
        {
          cvmCol[,ncol(cvmCol)+1] <- chainRename2(.data = intDat[[i, "data"]], lastCompleteYear = lastCompleteYear, chainType = pairs$ChainType[p], cypColumn = pairs$CYP[p], pypColumn = pairs$PYP[p])
        }
        cvmCol <- cvmCol %>% group_by(Sector, Industry, Asset) %>% select(-Period) %>% nest(.key = "chained")
        cvmCol <- cvmCol %>% select(-Sector, -Industry, -Asset)
        
        if (!(exists("x")))
        {
          x <- cbind(intDat, cvmCol)
        }
        else
        {
          x <- rbind(x, cbind(intDat, cvmCol))          
        }

        rm(cvmCol, intDat)
      }
      
      #x <- dplyr::mutate(.data, chained =
      #                     foreach(i = seq_len(nrow(.data)), .packages = c("dplyr", "capstock"), .errorhandling = "pass", .options.snow = opts)
      #                   %doOrDoPar%
      #                     dplyr::bind_cols(
      #                       foreach(p = seq_len(nrow(pairs))) %do% chainRename2(.data = .data[[i, "data"]], lastCompleteYear = lastCompleteYear,
      #                                    chainType = pairs$ChainType[p], cypColumn = pairs$CYP[p], pypColumn = pairs$PYP[p])))
      
      #if (parallelise) parallel::stopCluster(cl)
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("chainTogether - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(x)
  
}


# Helper function to rename "CVM" column from chain results based on the supplied cypColumn
chainRename2 <- function(.data, lastCompleteYear, cypColumn, pypColumn, chainType) {
  tryCatch(
    {
      x <- robustChain2(.data, lastCompleteYear, cypColumn, pypColumn, chainType)
      x <- dplyr::select(x, CVM)
      # Rename cols to be cypColumn root + CVM, e.g. CapitalServicesCYP/PYP => CapitalServicesCVM
      # If the cypColumn doesn't have CYP at the end, just use the cypColumn
      varNameRoot <- sub("CYP$", "", cypColumn)
      newnames <- paste(varNameRoot, "CVM", sep = "")
      oldnames <- "CVM"
      x <- dplyr::rename_(x, .dots = setNames(oldnames, newnames))
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("chainRename2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(x)
}


#' Call chain with missing values present.
#'
#' Call the chain function whilst dealing with leading missing values, and all zero values.
#'
#' For metrics dealing with \emph{change} in quantities, e.g. "TotalChangesInVolume"
#' the first year in the series is often NA. chain is called without the inital
#' year of data and NAs are added afterwards.
#'
#' Where all the values in the given cypColumn are zero, the series is returned
#' without calling chain.
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
robustChain2 <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP",
                         chainType = c("Stock", "Flow")) {
  tryCatch(
    {
      # If the entire series is zero (often the case with adjustment values) just
      # return CVM as all zeros
      if (all(!is.na(.data[cypColumn]) & .data[cypColumn] == 0)) {
        .data["CVM"] <- 0
        return(.data)
      }
      
      # If first four PYP values are NA or 0 call chain without them, then add them back
      # otherwise just call chain
      year1 <- seq(1, 4)
      if (all(is.na(.data[year1, pypColumn])) | all(.data[year1, pypColumn] == 0)) {
        leadingRows <- .data[year1, ]
        partialResult <- chain2(.data = .data[-year1, ],
                                lastCompleteYear = lastCompleteYear,
                                cypColumn = cypColumn, pypColumn = pypColumn,
                                chainType = chainType)
        result <- dplyr::bind_rows(leadingRows, partialResult)
      } else {
        result <- chain2(.data = .data[, ],
                         lastCompleteYear = lastCompleteYear,
                         cypColumn = cypColumn, pypColumn = pypColumn,
                         chainType = chainType)
      }
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("robustChain2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(result)
}

#' Calculate constrained chain volume measure.
#'
#' Calculate the constrained chained volume measure based on current year and
#' previous year prices (data is assumed to be quarterly).
#'
#' CYP values must not contain NAs.
#'
#' The series is benchmarked to the yearly figures. Benchmark cannot process
#' zero values so, where present, a small adjustment is made (+0.001) and removed
#' after benchmarking. stopifnot for Years commented out 29 Jan 19
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
chain2 <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP",
                   chainType = c("Stock", "Flow")){
  tryCatch(
    {
      ##################################################################################################################################################
      # .data <- toChain
      # .data <- unnest(.data)
      # chainType <- 'Stock'
      # cypColumn <- "ConsumptionOfFixedCapitalCYP"
      # pypColumn <- "ConsumptionOfFixedCapitalPYP"
      ##################################################################################################################################################
      # input validation
      chainType <- match.arg(chainType)
      
      columns <- colnames(.data)
      stopifnot(cypColumn %in% columns)
      stopifnot(pypColumn %in% columns)
      #stopifnot("Year" %in% columns)
      #stopifnot(lastCompleteYear %in% .data$Year)
      if (any(is.na(.data[cypColumn]))) stop("CYP values must not be NA.")
      
      # add quarter values, probably already there but this is easier
      withQuarter <- .data %>%
        dplyr::group_by(Year) %>%
        dplyr::mutate(Quarter=dplyr::row_number(Year)) %>%
        dplyr::ungroup()
      
      # calculate the scaling factor and unconstrained CVM
      quarterlyCVM <- calcScalingFactor2(withQuarter, lastCompleteYear, cypColumn, pypColumn)
      quarterlyCVM <- calcUnconstrainedCVM2(quarterlyCVM, lastCompleteYear, cypColumn, pypColumn)
      
      # calculate the annual cvm, based on the chain type
      annualCVM <- switch(chainType,
                          "Stock" = calcAnnualCVMStock2(withQuarter, lastCompleteYear,
                                                        cypColumn, pypColumn),
                          "Flow" = calcAnnualCVMFlow2(withQuarter, lastCompleteYear,
                                                      cypColumn, pypColumn))
      
      # finally feed the quarterly and annual values into the benchmarking function
      benchType <- switch(chainType,
                          "Stock" = "avg",
                          "Flow" = "sum")
      .data[, "CVM"] <- bench::benchmark0(quarterlyCVM$UnconstrainedCVM,
                                          annualCVM$AnnualCVM, periodicity = 4,
                                          type = benchType,
                                          toleranceThreshold = 1e8)
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("chain2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(.data)
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
calcScalingFactor2 <- function(.data, lastCompleteYear, cypColumn = "CYP",
                               pypColumn = "PYP"){
  tryCatch(
    {
      #.data <- withQuarter
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
      # TEST result$ScalingFactor1 <- dplyr::if_else(result$PYPlead == 0, 1, as.numeric(result$CYPlead / result$PYPlead))
      # TEST result$ScalingFactor <- dplyr::if_else(result$PYPlead == 0, 1, as.numeric(result$CYPlead / result$PYPlead))
      result$ScalingFactor <- sfDivide2(result$CYPlead, result$PYPlead)
      
      result$ScalingFactor <- cumprod(result$ScalingFactor)
      
      # join the data back onto the original data
      # all missing quarters will be filled with the value from the 3rd quarter
      result <- result %>%
        dplyr::select(Year, Quarter, ScalingFactor) %>%
        dplyr::right_join(.data, by = c("Year", "Quarter")) %>%
        # note that the first year will have no PYP and hence no ScalingFactor
        # this year too will get the ScalingFactor from next year's 3rd quarter
        tidyr::fill(ScalingFactor, .direction="up")
      
      #testDF <- result %>%
      #dplyr::select(Year, Quarter, ScalingFactor, ScalingFactor1) %>%
      #dplyr::right_join(.data, by = c("Year", "Quarter")) %>%
      #tidyr::fill(ScalingFactor,ScalingFactor1, .direction="up")
      
      # additional fix, any values after and including the last complete year
      # get a ScalingFactor of 1
      result[result$Year >= lastCompleteYear, "ScalingFactor"] <- 1
      #testDF[testDF$Year >= lastCompleteYear, "ScalingFactor"] <- 1
      #testDF[testDF$Year >= lastCompleteYear, "ScalingFactor1"] <- 1
      # this also applies to the 4th quarter directly preceding the last complete year
      result[result$Year >= (lastCompleteYear-1) & result$Quarter==4,"ScalingFactor"] <- 1
      #testDF[testDF$Year >= (lastCompleteYear-1) & result$Quarter==4,"ScalingFactor"] <- 1
      #testDF[testDF$Year >= (lastCompleteYear-1) & result$Quarter==4,"ScalingFactor1"] <- 1
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("calcScalingFactor2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(result[, c("Year", "Quarter", cypColumn, pypColumn, "ScalingFactor")])
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
calcUnconstrainedCVM2 <- function(.data, lastCompleteYear, cypColumn = "CYP",
                                  pypColumn = "PYP"){
  tryCatch(
    {
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
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("calcUnconstrainedCVM2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(result)
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
calcAnnualCVMFlow2 <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP"){
  tryCatch(
    {
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
        dplyr::mutate(AnnualScalingFactor = sfDivide2(dplyr::lead(AnnualCYP), dplyr::lead(AnnualPYP))) %>%
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
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("calcAnnualCVMFlow2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(result)
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
calcAnnualCVMStock2 <- function(.data, lastCompleteYear, cypColumn = "CYP", pypColumn = "PYP"){
  tryCatch(
    {
      result <- .data %>%
        dplyr::filter(Year <= lastCompleteYear, Quarter==4) %>%
        dplyr::mutate_(AnnualCYP = cypColumn, AnnualPYP = pypColumn) %>%
        # then calculate the first part of the annual scaling factor
        dplyr::mutate(AnnualScalingFactor =  sfDivide2(dplyr::lead(AnnualCYP), dplyr::lead(AnnualPYP))) %>%
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
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      
    }, error = function(err) {
      print(paste("calcAnnualCVMStock2 - MY_ERROR:  ",err))
      
    }, finally = {
      
    })
  return(result)
}


# Utility function to calculate scaling factor.
# Guards against creating scaling factors of zero or infinity by replacing with 1
# Note we can still have zero divided by a positive value
sfDivide2 <- function(numerator, denominator) {
  dplyr::if_else(denominator == 0, 1,
                 as.numeric(numerator / denominator))
}


