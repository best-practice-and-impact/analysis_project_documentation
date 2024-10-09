
#' @title Extract historic deflators from reference file
#'
#' @description Extracts historic deflators pre-defined CSV file.
#'
#' @details This function depends on the historic file being a consistent format.
#'
#'   1. Data is parsed from the CSV file
#'
#'   2. Empty rows (containing all \code{NA}s or zeros) are removed.
#'
#'   To reduce logging verbosity use \code{flog.threshold(WARN, name = "prepim")}
#'   using WARN, ERROR, or FATAL. The default is INFO.
#'
#' @param x File path of the input spreadsheet.
#' @import assertr
#' @import dplyr
#' @import futile.logger
#' @import tidyr
#' @import readr
#' @return tidy data.frame of deflators with \code{Sector},
#'  \code{Industry}, and \code{Asset} variables, plus the time series.
#' @export
extractHistoricDeflators <- function(path) {

  # -------------------------- 1. Parse Data -----------------------------------
  flog.info("Extracting %s", path)
  # Load the data
  x <- readr::read_csv(
    path,
    col_types = cols(
      .default = col_double(),
      Industry = col_character(),
      Asset = col_character(),
      Sector = col_character(),
      Prices = col_character()
    )
  )

  # Remove empty rows. Can happen when worksheet's "last cell" is beyond the data
  x <- x[rowSums(is.na(x)) != ncol(x), ]

  # --- Integrity checks ---
  # Check expected columns are present
  categoryHeadings <- c("Sector","Industry","Asset", "Prices")
  flog.info("Checking input for expected column headings.")
  if (!all(categoryHeadings %in% colnames(x))) {
    msg <- paste("x must contain columns", paste(categoryHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  # Convert remaining variables to numbers
  x <- dplyr::mutate_each(x, dplyr::funs(as.numeric), -dplyr::one_of(categoryHeadings))

  # Category heading vector
  categoryHeadings <- c("Sector","Industry","Asset", "Prices")

  # Replace any missing values with zero
  x[is.na(x)] <- 0

  # -------------------------- 2. Drop empty rows ------------------------------
  # Categories with empty time series should be dropped

  # Record total value as a checksum before dropping
  totVal <- sum(dplyr::select(x, -one_of(categoryHeadings)))

  # Drop all rows containing zero values for all columns
  toDrop <- rowSums(abs(dplyr::select(x, -one_of(categoryHeadings)))) == 0
  nToDrop <- sum(toDrop)
  if (nToDrop > 0) {
    flog.info(paste("Removing", nToDrop, "series with all zero values."))
    flog.info("Removing:", as.data.frame(x[toDrop, categoryHeadings]),
              capture = TRUE)
    x <- x[!toDrop, ]

    # --- Integrity checks ---
    flog.info("Checking removal of empty time series retains all quantities.")
    if (sum(select(x, -one_of(categoryHeadings))) != totVal) {
      msg <- "Removal of rows containing all zeroes has resulted in loss of quantity."
      flog.fatal(msg)
      stop(msg)
    }
  }

  # ------------------------ Cosmetic Changes ----------------------------------

  # Tidy the data
  x <- tidyr::gather(x, Period, Value, -one_of(categoryHeadings))

  # Standarise Industry codings from e.g. "1" to "01"
  x <- mutate(x, Industry = formatIndustryCodes(Industry))

  # Reorder columns to standard layout
  x <- dplyr::select(x, Sector, Industry, Asset, Prices, Period, Value)

  # --- Integrity checks ---
  flog.info("Checking Periods cover a contiguous range.")
  yearCoverage <- x %>%
    transmute(Year = as.numeric(substr(Period, 2, 5))) %>%
    distinct() %>%
    .[["Year"]]  # Convert to vector
  expectedCoverage <- seq(min(yearCoverage), max(yearCoverage), by = 1)
  if (length(setdiff(expectedCoverage, yearCoverage)) > 0) {
    msg <- "Periods do not cover a contiguous range."
    flog.fatal(msg)
    stop(msg)
  }

  return(x)

}
