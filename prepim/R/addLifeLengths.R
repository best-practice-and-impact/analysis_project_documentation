#' Add life lengths.
#'
#' Join Life Length Min, Max, Average, and CoVs to GFCF dataset.
#'
#' The life lengths Excel file must include four sheets labelled AverageLifeLengths,
#' CoVs, Min, and Max. Each sheet must contain Sector, Industry, Asset columns and
#' additional columns for each period.
#'
#' @param gfcf data.frame of GFCF series.
#' @param lifeLengthsPath string denoting path to life lengths Excel file.
#' @export
#' @return data.frame of gfcf with additional Min, Max, Average, and CoV columns.
addLifeLengths <- function(gfcf, lifeLengthsPath) {
  flog.info("\nAdding life lengths.")

  flog.info("Reading AverageLifeLengths sheet.")
  lifeAverage <- extractLifeLength(lifeLengthsPath, toCover = as.data.frame(gfcf),
                                   varName = "Average", sheet = "AverageLifeLengths")
  flog.info("Reading CoVs sheet.")
  lifeCov <-  extractLifeLength(lifeLengthsPath, toCover = as.data.frame(gfcf),
                                varName = "CoV", sheet = "CoVs")
  flog.info("Reading Max sheet.")
  lifeMax <-  extractLifeLength(lifeLengthsPath, toCover = as.data.frame(gfcf),
                                varName = "Max", sheet = "Max")
  flog.info("Reading Min sheet.")
  lifeMin <-  extractLifeLength(lifeLengthsPath, toCover = as.data.frame(gfcf),
                                varName = "Min", sheet = "Min")

  # Combine
  jk <- c("Sector", "Industry", "Asset", "Period")  # define join keys
  flog.info("Combining life lengths datasets.")
  LL <- left_join(lifeAverage, lifeCov, by = jk)
  LL <- left_join(LL, lifeMax, by = jk)
  LL <- left_join(LL, lifeMin, by = jk)

  # Check coverage is met
  gfcf_no_ll <- anti_join(gfcf, LL, by = c("Sector", "Industry", "Asset", "Period"))
  flog.info("Checking GFCF and life length specification coverage.")
  if (nrow(gfcf_no_ll) > 0) {
    flog.error("Missing life lengths for series:",
                                      as.data.frame(getSeries(gfcf_no_ll)), capture = TRUE)
    stop("Missing life length coverage.")
  }

  # Add Life Lengths
  flog.info("Joining life lengths to GFCF.")
  gfcf <- left_join(gfcf, LL, by = c("Sector", "Industry", "Asset", "Period"))

  # Check none of the joined life lengths are NA
  gfcf_na <- dplyr::select(gfcf, one_of(c(jk, "Average", "CoV", "Max", "Min")))
  gfcf_na <- dplyr::filter(gfcf_na, is.na(Average) | is.na(CoV) | is.na(Max) | is.na(Min))
  if (nrow(gfcf_na) > 0) {
    flog.warn("NA life lengths produced for series:",
               as.data.frame(getSeries(gfcf_no_ll)), capture = TRUE)
    warning("There was a problem with joining the life lengths. NAs were produced.")
  }

  return(gfcf)
}
