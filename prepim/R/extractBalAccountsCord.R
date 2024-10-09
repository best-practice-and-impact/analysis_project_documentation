#' @title Extract balanced accounts from CORD outputs
#'
#' @description Extracts balanced accounts data from a standard CORD Excel export.
#'
#' @details This function depends on the output format from CORD being consistent.
#'
#'   Input files should have columns Sector, Industry, Asset & Product, Basis,
#'   followed by columns for each Period, e.g. Y1997Q1, Y1997Q2, etc.
#'
#'   1. Data is parsed from the first worksheet in the provided Excel file path
#'
#'   2. Empty rows (containing all \code{NA}s or zeros) are removed.
#'
#'   3. Net values are calculated (Acquisitions - Disposals)
#'
#'   If net GFCF figures have been pre-calculated they should be provided with a
#'   Basis of "NET" instead of "ACQ" or "DIS". Input files must
#'   have a Basis of either "NET" or "ACQ"/"DIS" but not mixing both.
#'
#'   To reduce logging verbosity use \code{flog.threshold(WARN, name = "prepim")}
#'   using WARN, ERROR, or FATAL. Defaults to INFO level.
#'
#' @param path File path of the input spreadsheet.
#' @param sheet Sheet number within the Excel file. Defaults to first sheet.
#' @param skip Number of rows to skip before reading any data. Defaults to 3
#' based on typical CORD export.
#' @import assertr
#' @import dplyr
#' @import futile.logger
#' @import tidyr
#' @import readxl
#' @return A tidy data.frame of net balanced accounts with \code{Sector},
#'  \code{Industry}, \code{Asset}, \code{Period}, and \code{Value} variables.
#' @export
extractBalAccountsCord <- function(path, sheet = 1, skip = 3) {

  # -------------------------- 1. Parse Data -----------------------------------
  flog.info("Extracting %s", path)

  # Load the data
  x <- readxl::read_excel(path = path, sheet = sheet, skip = skip)

  # Remove empty rows. Can happen when worksheet's "last cell" is beyond the data
  x <- x[rowSums(is.na(x)) != ncol(x), ]

  # --- Integrity checks ---
  # Check expected columns are present
  categoryHeadings <- c("Sector","Industry","Asset & Product", "Basis")
  flog.info("Checking input for expected column headings.")
  if (!all(categoryHeadings %in% colnames(x))) {
    msg <- paste("x must contain columns", paste(categoryHeadings, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }

  # Check Basis variable contains permitted values
  # Files can either be provided with Aquisitions and (optionally) Disposals, or
  # alternatively provided with pre-calculated Net figures. With the former
  # net figures will be calculated, with the latter, figures are passed though
  flog.info("Checking input for expected Basis values.")
  if (!all(unique(x$Basis) %in% c("ACQ", "DIS", "NET"))) {
    msg <- "Basis must only contain values ACQ, DIS, or NET."
    flog.fatal(msg)
    stop(msg)
  }
  # Determine basis mode for later checks
  if ("NET" %in% x$Basis) basisMode <- "net" else basisMode <- "acqdis"

  # Check that use of ACQ/DIS and NET are not mixed in the same file
  if (basisMode == "net") {
   if (any(unique(x$Basis) %in% c("ACQ", "DIS"))) {
     msg <- "Basis must either contain combinations of ACQ and DIS, OR just NET, but not both"
     flog.fatal(msg)
     stop(msg)
   }
  }

  # Convert remaining variables to numbers
  x <- dplyr::mutate_each(x, dplyr::funs(as.numeric), -dplyr::one_of(categoryHeadings))

  # Forward-fill category values because they only appear once, followed by
  # missing values until the next category
  x <- tidyr::fill_(x, categoryHeadings)

  # Rename columns containing special characters
  x <- dplyr::rename(x, Asset = `Asset & Product`)
  # Re-specify category headings (useful for column selection later)
  categoryHeadings <- c("Sector","Industry","Asset", "Basis")

  # Check that there are no duplicates
  flog.info("Checking dataset contains no duplicate series.")
  x_dupes <- dplyr::select(x, one_of(categoryHeadings))
  x_dupes <- x_dupes[duplicated(x_dupes), ]
  if (nrow(x_dupes) > 0) {
    msg <- paste("Input file contains duplicates series.")
    flog.fatal(paste(msg, "Duplicates:"), data.frame(x_dupes), capture = TRUE)
    stop(msg)
  }

  flog.info("Replacing missing values with zero.")
  # Replace any missing values with zero
  x[is.na(x)] <- 0

  # -------------------------- 2. Drop empty rows ------------------------------
  flog.info("Dropping empty time series.")
  # Categories with empty time series should be dropped

  # Record total value as a checksum before dropping
  totVal <- sum(select(x, -Sector, -Industry, -Asset, -Basis))

  # Drop all rows containing zero values for all columns
  x <- x[rowSums(abs(select(x, -Sector, -Industry, -Asset, -Basis))) != 0, ]

  # --- Integrity checks ---
  flog.info("Checking removal of empty time series retains all quantities.")
  if (sum(select(x, -Sector, -Industry, -Asset, -Basis)) != totVal) {
    msg <- "Removal of rows containing all zeroes has resulted in loss of quantity."
    flog.fatal(msg)
    stop(msg)
  }

  # --- Integrity checks ---
  # If this is a standard CORD output with Basis of ACQ or DIS each
  # Sector/Industry/Asset combo should have a row for Aquisitions.
  # (Basis = "ACQ") and *optionally* a row for Disposals (Basis = "DIS")
  # Drop any series that only have DIS with no ACQ
  if (basisMode != "net") {
    flog.info("Checking that all series with disposals also have acquisitions.")
    # Find rows with DIS but no ACQ
    noAcquisitions <- x %>%
      dplyr::group_by(Industry, Sector, Asset) %>%
      dplyr::summarise(nACQ = sum(Basis == "ACQ"),
                       nDIS = sum(Basis == "DIS")) %>%
      dplyr::ungroup() %>%
      dplyr::filter(nACQ == 0) %>%
      left_join(x, by = c("Industry", "Sector", "Asset")) %>%
      dplyr::select(Sector, Industry, Asset, nACQ, nDIS)

    # Remove rows with DIS but no ACQ
    if (nrow(noAcquisitions) > 0) {
      flog.warn("Removing series with disposals but no acquisitions",
                as.data.frame(noAcquisitions), capture = TRUE)
    }
    x <- dplyr::anti_join(x, noAcquisitions, by = c("Sector", "Industry", "Asset"))
    rm(noAcquisitions)
  }

  # -------------------------- 3. Calculate Net --------------------------------

  # For a standard CORD extract net values need to be calculated (ACQ minus DIS).
  # Tidy the data into 1 row per sector/ind/asset/period with cols for DIS and ACQ.
  # Then calculate net.
  x <- x %>%
    tidyr::gather(key = Period, value = Value, -Industry, -Asset, -Sector, -Basis)

  if (basisMode == "acqdis") {
    x <- x %>%
      # Ensure there's at least one row with a "DIS" to ensure consistency when spread
      tidyr::complete(Basis = c("ACQ", "DIS")) %>%
      tidyr::spread(Basis, Value, fill = 0) %>%
      # Remove the fake "DIS" row if it was created by complete()
      dplyr::filter(!is.na(Period)) %>%
      # Calc net values
      dplyr::mutate(Net = ACQ - DIS) %>%
      dplyr::select(-ACQ, -DIS) %>%
      dplyr::rename(Value = Net)  # Rename Net to standard name
  }

  # ------------------------- Cosmetic Changes ---------------------------------

  # Standardise inconsistent Industry separators (replace hyphens with underscores)
  x <- dplyr::mutate(x, Industry = gsub("-", "_", Industry))

  # Add Prices indicator
  x <- dplyr::mutate(x, Prices = "CP")

  # Coerce Period to standard format, e.g. "Y2012Q1" - add "Y" if not present.
  x <- dplyr::mutate(x, Period = dplyr::if_else(substr(Period, 1, 1) != "Y",
                                  paste0("Y", Period),
                                  Period))

  # Reorder columns to standard layout
  x <- dplyr::select(x, Sector, Industry, Asset, Prices, Period, Value)

  return(x)
}
