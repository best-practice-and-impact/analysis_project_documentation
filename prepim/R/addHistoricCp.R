#' Add Historic CP Series to Balanced Accounts
#'
#' Add Historic CP Series to balanced accounts series at the defined reference period.
#'
#' @param historicCp data.frame with Period and Value columns for historic GFCF in CP.
#' @param baCp data.frame of balanced accounts in CP with Period and Value columns.
#' @param linkPeriod String denoting the Period at which to join historic and
#' balanced accounts (balanced accounts will start from the linkPeriod).
#' @return data.frame with Period and Value columns with periods covering min(historicCp)
#' to max(baCp).
#' @export
addHistoricCp <- function(historicCp, baCp, linkPeriod) {

  # Include historic data up until the linkPeriod
  historicCp <- filter(historicCp, Period < linkPeriod)
  # Include balanced accounts data from the linkPeriod
  baCp <- filter(baCp, Period >= linkPeriod)

  bind_rows(historicCp, baCp)

}
