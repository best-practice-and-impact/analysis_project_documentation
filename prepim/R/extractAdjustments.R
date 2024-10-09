#' Extract Adjustments.
#'
#' Extract K-value adjustments from CSV file.
#'
#' The CSV file should contain Sector, Industry, Asset, Period, K1CP, K3CP, K4CP,
#' K5CP, K61CP, and K62CP columns. Other columns may be included but will be ignored.
#'
#' Each row must be unique with respect to Sector, Industry, Asset, and Period.
#'
#' @param path String path to CSV file.
#' @return Verified data.frame with Sector, Industry, Asset, Period, and K values.
#' @import readr dplyr
#' @export
extractAdjustments <- function(path) {

  adjustments <- readr::read_csv(path, col_types = cols(.default = col_character(),
                                                        Sector = col_character(),
                                                        Industry = col_character(),
                                                        Asset = col_character(),
                                                        Period = col_character(),
                                                        K1CP = col_double(),
                                                        K3CP = col_double(),
                                                        K4CP = col_double(),
                                                        K5CP = col_double(),
                                                        K61CP = col_double(),
                                                        K62CP = col_double()))
  seriesCols <- c("Sector", "Industry", "Asset", "Period")
  KCols <- c("K1CP", "K3CP", "K4CP", "K5CP", "K61CP", "K62CP")
  expectedCols <- c(seriesCols, KCols)

  # Check expected columns are present
  chkColNames(adjustments, expectedCols)

  # Check for duplicate rows
  duplicates <- dplyr::group_by(adjustments, Sector, Industry, Asset, Period)
  duplicates <- dplyr::summarise(duplicates, n = n())
  duplicates <- dplyr::filter(duplicates, n > 1)
  if (nrow(duplicates) > 0) {
    msg <- paste("Adjustments file contains duplicates.")
    flog.fatal(paste(msg, "Series:"), data.frame(duplicates), capture = TRUE)
    stop(msg)
  }

  # Replace missing values with zero
  naToZero <- function(x) dplyr::if_else(is.na(x), 0, x)
  adjustments <- mutate_at(adjustments,
                           .cols = KCols,
                           .funs = naToZero)

  # Check that some adjustments sum to zero
  tol <- 1e-6
  if (abs(sum(adjustments$K4CP)) > tol) warning("K4CP does not sum to zero.")
  if (abs(sum(adjustments$K61CP)) > tol) warning("K61CP does not sum to zero.")
  if (abs(sum(adjustments$K62CP)) > tol) warning("K62CP does not sum to zero.")

  # Ignore additional columns
  adjustments <- dplyr::select(adjustments, Sector, Industry, Asset, Period,
                               K1CP, K3CP, K4CP, K5CP, K61CP, K62CP)
  return(adjustments)

}

