#' Calculate Price Index
#'
#' Calls the function calcPriceIndexUsingAvgs, which does the main processing after joining data up and adding row number
#'
#' @param cpData one or more numeric values in current prices - UPDATED LeighP 15-11-18
#' @param cvmData one or more numeric values in chain volume measures - UPDATED LeighP 15-11-18
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' NEWcalcPriceIndex(10, 20)
#' NEWcalcPriceIndex(1:10, 20:30)
calcPriceIndex <- function(cpData, cvmData){
  stopifnot(is.numeric(cpData))
  stopifnot(is.numeric(cvmData))
  stopifnot(length(cpData)==length(cvmData))
  #print(paste("I AM IN THE NEW CalcPriceIndex METHOD!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
  ####################################################################################################################################################
  #### THIS NEEDS TO BE COMMENTED OUT ONCE TESTING HAS FINISHED
  #dat <- chainedUnnest %>% filter(Sector == 'S.1', Industry == '01', Asset == 'OTH.MACH.EQ.WEAP') %>% select(Period, GrossStockCVM, GrossStockCP)
  #cpData <- as.data.frame(dat$GrossStockCP)
  #cvmData <- as.data.frame(dat$GrossStockCVM)
  ####################################################################################################################################################
  tryCatch(
    {
        cpData <- cpData #as.data.frame(cpData)
        cvmData <- cvmData #as.data.frame(cvmData)
        dat <- cbind(cpData, cvmData)
        names(dat) <- c('cpData','cvmData')
        dat <- as.data.frame(dat)
        #### ADD A ROW NUMBER TO KEEP ORDER
        dat <- tibble::rowid_to_column(dat, "rowNum")

        dat <- calcPriceIndexUsingAvgs(dat)

        # To avoid divide-by-zero problems, where CP == 0 price index should be 1, not 0
        #return(dat$priceInd)

    }, warning = function(war) {
        print(paste("had to go to warning: ", war))
        return(dplyr::if_else(cpData == 0, 1, cpData/cvmData))
    }, error = function(err) {
        print(paste("had to go to Error: ", err))
    }, finally = {

    })

}

#' Calculate Price Index using Averages
#'
#' Calculate the price index as the difference between values in current prices
#' and values in chained volume measures
#' Then create an average of these based on at least 2 quarters from the current year and 2 quarters from thwe previous/next year
#' Example
#' 2010Q1 = (2009Q3 + 2009Q4 + 2010Q1 + 2010Q2) / 4
#' 2010Q2 = (2009Q3 + 2009Q4 + 2010Q1 + 2010Q2) / 4
#' 2010Q3 = (2010Q3 + 2010Q4 + 2011Q1 + 2011Q2) / 4
#' 2010Q4 = (2010Q3 + 2010Q4 + 2011Q1 + 2011Q2) / 4
#'
#' CP & CVM values are BACKCAST 5 Periods and FORECAST 5 Periods then CP divided by CVM to create the orignal index.
#' This then allows you to take the average for the earliest and latest Periods
#'
#' @param dat one or more numeric values in current prices - Updated 15-11-18
#'
#' @return a numeric vector
#'
#' @examples
#' dat(2:12, 2:12)
calcPriceIndexUsingAvgs <- function(dat){

  #### ORDER THE DATA DESCENDING BY PERIOD SO YOU CAN BACKCAST THE START
  dat1 <- dat[order(dat$rowNum, decreasing = TRUE),]
  #### GET THE NUMBER OF ROWS IN THE DATA
  noRows <- max(dat$rowNum)
  #### HOW MANY PERIODS TO FORECAST - USE 5 JUST FOR SAFETY MEASURES
  periods <- 5
  # BACKCAST THE CP VALUES
  cp1 <- auto.arima(dat1$cpData, seasonal = FALSE)
  cp1 <- forecast(cp1,h=periods)
  cp1 <- as.data.frame(cp1$mean)
  names(cp1) <- c('cpData')
  cp1 <- tibble::rowid_to_column(cp1, "rowNum1") - periods
  cp1 <- cp1[order(cp1$rowNum, decreasing = TRUE),]
  cp1 <- tibble::rowid_to_column(cp1, "rowNum") - periods
  cp1 <- cp1 %>% select(rowNum, cpData)
  #### BACKCAST THE CVM VALUES
  cv1 <- auto.arima(dat1$cvmData, seasonal = FALSE)
  cv1 <- forecast(cv1,h=periods)
  cv1 <- as.data.frame(cv1$mean)
  names(cv1) <- c('cvmData')
  cv1 <- tibble::rowid_to_column(cv1, "rowNum") - 5
  cv1 <- cv1[order(cv1$rowNum, decreasing = TRUE),]
  cv1 <- cv1 %>% select(cvmData)

  cp1 <- cbind(cp1, cv1)

  # FORECAST THE CP VALUES
  cp2 <- auto.arima(dat$cpData, seasonal = FALSE)
  cp2 <- forecast(cp2,h=periods)
  cp2 <- as.data.frame(cp2$mean)
  names(cp2) <- c('cpData')
  cp2 <- tibble::rowid_to_column(cp2, "rowNum") + (noRows + 2)
  cp2 <- cp2 %>% select(rowNum, cpData)
  #### FORECAST THE CVM VALUES
  cv2 <- auto.arima(dat$cvmData, seasonal = FALSE)
  cv2 <- forecast(cv2,h=periods)
  cv2 <- as.data.frame(cv2$mean)
  names(cv2) <- c('cvmData')
  cv2 <- cv2 %>% select(cvmData)

  cp2 <- cbind(cp2, cv2)
  dat <- rbind(cp1, dat, cp2)

  #### Create a series of indices, this is the old way of doing it, you then need to create the new indices based on these numbers
  # To avoid divide-by-zero problems, where CP == 0 price index should be 1, not 0
  dat$ind1 <- dplyr::if_else(dat$cpData == 0, 1, dat$cpData/dat$cvmData)

  rm(dat1, cv1, cv2, cp1, cp2)

  #dat <- tibble::rowid_to_column(dat, "rowNum")
  dat$qtr <- with(dat, ifelse(rowNum > 0 & rowNum <= noRows, rowNum / 4, NA))
  dat$qtr <- dat$qtr %% 1
  dat$qtr <- with(dat, ifelse(
    qtr == 0.25, 1, ifelse(
      qtr == 0.50, 2, ifelse(
        qtr == 0.75, 3, ifelse(
          qtr == 0.00, 4, 0)))))

  #### YOU WANT AT LEAST 2 QUARTERS FROM THE CURRENT YEAR AND 2 QUARTERS FROM THE PREVIOUS/NEXT YEAR
  #### CREATE COLUMNS WITH THESE VALUES, LEADING/LAGGING AS APPROPRIATE - SEE COMMENTS AT TOP TO SEE EXPLANATION OF METHOD
  #### INDEX 1 - ind1
  dat$ind2 <- with(dat, ifelse(
    qtr == 1, dplyr::lag(dat$ind1, 1), ifelse(
      qtr == 2, dplyr::lag(dat$ind1, 1), ifelse(
        qtr == 3, dplyr::lead(dat$ind1, 1), ifelse(
          qtr == 4, dplyr::lead(dat$ind1, 1), 0)))))
  #### INDEX 2 - ind2
  dat$ind3 <- with(dat, ifelse(
    qtr == 1, dplyr::lag(dat$ind1, 2), ifelse(
      qtr == 2, dplyr::lag(dat$ind1, 2), ifelse(
        qtr == 3, dplyr::lead(dat$ind1, 2), ifelse(
          qtr == 4, dplyr::lead(dat$ind1, 2), 0)))))
  #### INDEX 3 - ind3
  dat$ind4 <- with(dat, ifelse(
    qtr == 1, dplyr::lag(dat$ind1, 0), ifelse(
      qtr == 2, dplyr::lag(dat$ind1, 3), ifelse(
        qtr == 3, dplyr::lead(dat$ind1, 3), ifelse(
          qtr == 4, dplyr::lead(dat$ind1, 0), 0)))))
  #### INDEX 4 - ind4
  dat$ind5 <- with(dat, ifelse(
    qtr == 1, dplyr::lead(dat$ind1, 1), ifelse(
      qtr == 2, dplyr::lag(dat$ind1, 0), ifelse(
        qtr == 3, dplyr::lead(dat$ind1, 0), ifelse(
          qtr == 4, dplyr::lag(dat$ind1, 1), 0)))))
  #### TAKE THE AVERAGE OF THE 4 INDICES
  dat$priceInd <- (dat$ind2 + dat$ind3 + dat$ind4 + dat$ind5) / 4

  dat <- dat %>% filter(!(is.na(priceInd)))
  stopifnot(nrow(dat)==noRows)
  return(dat)
}


#' Calculate Price Index OLD
#'
#' This was how the original Price Inxdex was calculated, not used anymore
#'
#' @param cpData one or more numeric values in current prices
#' @param cvmData one or more numeric values in chain volume measures
#'
#' @return a numeric vector
#'
#' @examples
#' calcPriceIndex(10, 20)
#' calcPriceIndex(1:10, 20:30)
OLDcalcPriceIndex <- function(cpData, cvmData){
  stopifnot(is.numeric(cpData))
  stopifnot(is.numeric(cvmData))
  stopifnot(length(cpData)==length(cvmData))

  # To avoid divide-by-zero problems, where CP == 0 price index should be 1, not 0
  return(dplyr::if_else(cpData == 0, 1, cpData/cvmData))
}



