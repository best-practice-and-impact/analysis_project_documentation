# Data Integrity Checking Functions
# These functions are for general for validating different aspects of an input
# file, e.g. a splits specificatoin file, or a GFCF file.

# Checks that the dataframe contains the expected columns.
# Returns TRUE if no problems, otherwise prints error message and returns FALSE
chkColNames <- function(.data, expectedCols) {

  unmatchedCols <- !(expectedCols %in% colnames(.data))
  if (any(unmatchedCols)) {
    msg <- paste("Incorrect column names. Could not find",
                 paste(expectedCols[unmatchedCols], collapse = ", "))
    flog.fatal(msg)
    return(FALSE)
  }
  # If nothing caused an error return true
  return(TRUE)
}

# Checks dataframe does not have coverage of Sector/Industry/Asset of ALL/ALL/ALL
# Used to ensure spec files do not include the wildcard "ALL" for all coverage dimensions
chkSpecCombinations <- function(.data) {

  allWildcards <- with(.data, .data[Sector == "ALL" & Industry == "ALL" & Asset == "ALL", ])
  if (nrow(allWildcards) > 0) {
    flog.fatal("Specification cannot include \"ALL\" for every dimension.",
               data.frame(allWildcards), capture = TRUE)
    return(FALSE)
  }
  # If nothing caused an error return true
  return(TRUE)
}
