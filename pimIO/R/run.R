#' Run PIM with pre and post processing.
#'
#' Processes the input data before executing the PIM. Results from the PIM are
#' then further processed to obtain additional time series.
#'
#' @param inputData a data.frame
#' @param config a list of configuration parameters returned from pim::pimConfig
#' @param referenceYear a numeric value indicating the year against which all prices are set
#'
#' @return the given data.frame with additional columns
#' @export
run <- function(inputData, config, referenceYear){

  # preprocess
  processedData <- preProcess(inputData)

  # call the pim, the gfcf_ociv column is created in preProcess
  pimResult <- pimir::run(processedData$gfcf_ociv, processedData, config)

  # postprocess
  result <- postProcess(processedData, pimResult, referenceYear,
                        config$discountRate)
  return(result)
}
