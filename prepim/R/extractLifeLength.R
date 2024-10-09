
#' Extract Life Length sheet from Excel.
#'
#' Extract life length sheet from Excel and expand Sector/Industry/Assets of "ALL".
#'
#' Industry codes are left-padded with zeros if required, e.g. "1" becomes "01".
#'
#' @param llFile Excel file with one sheet per life length spec (e.g. "CoV").
#' @param toCover data.frame containing all the Sector/Industry/Assets that "ALL" will be expanded to.
#' @param varName string denoting the variable type present in the sheet, i.e. Average, CoV, Min, Max.
#' @param sheet the sheet name or number containing a life length type
#' @return data.frame with Sector, Industry, Asset, Period, and varName columns.
#' @import dplyr
#' @export
extractLifeLength <- function(llFile, toCover, varName, sheet = 1) {

  toCover <- ungroup(toCover)

  al <- read_excel(llFile, sheet = sheet)

  # --- Check Inputs -----------------------------------------------------------
  # Check expected columns are present
  expectedColumns <- c("Sector", "Industry", "Asset")
  if (!all(expectedColumns %in% colnames(al))) {
    msg <- paste("Life lengths sheet must contain columns", paste(expectedColumns, collapse = ", "))
    flog.fatal(msg)
    stop(msg)
  }
  # Check all expected columns are character
  if (ncol(Filter(is.character, al[expectedColumns])) < 3) stop("Sector, Industry, and Asset columns must be character data.")
  # Standarise Industry codings from e.g. "1" to "01"
  flog.info("Left-padding Industry codes with zero if required.")
  al <- mutate(al, Industry = formatIndustryCodes(Industry))

  # Ensure unique Sector/Industry/Asset combinations from coverage
  toCover <- toCover %>% select(Sector, Industry, Asset) %>% distinct()

  allSec <- data.frame(Sector = unique(toCover$Sector), join = 1, stringsAsFactors = FALSE)
  allInd <- data.frame(Industry = unique(toCover$Industry), join = 1, stringsAsFactors = FALSE)
  allAss <- data.frame(Asset = unique(toCover$Asset), join = 1, stringsAsFactors = FALSE)
  allKeys <- c("Sector", "Industry", "Asset")

  # Most Specific where everything is not ALL
  specRows <- inner_join(al, toCover, by = c("Sector", "Industry", "Asset"))

  # Expand ALL in Industry (where others are not ALL)
  indToEx <- filter(al, Industry == "ALL", Sector != "ALL", Asset != "ALL")
  indToEx <- mutate(indToEx, Industry = NULL)
  indExpanded <- left_join(mutate(indToEx, join = 1), allInd, by = "join") %>% mutate(join = NULL)
  # Filter to what's in the coverage and to what we don't already have specified
  indExpanded <- indExpanded %>% semi_join(toCover, by = allKeys) %>%
    anti_join(specRows, by = allKeys) %>%
    select(Sector, Industry, Asset, everything())

  # Expand ALL in Sector (where others are not ALL)
  secToEx <- filter(al, Industry != "ALL", Sector == "ALL", Asset != "ALL")
  secToEx <- mutate(secToEx, Sector = NULL)
  secExpanded <- left_join(mutate(secToEx, join = 1), allSec, by = "join") %>% mutate(join = NULL)
  # Filter to what's in the coverage and to what we don't already have specified
  secExpanded <- secExpanded %>% semi_join(toCover, by = allKeys) %>%
    anti_join(specRows, by = allKeys) %>%
    select(Sector, Industry, Asset, everything())

  # combine all specifications so far
  specRows <- bind_rows(specRows, indExpanded, secExpanded)

  # Expand where ALL in Sector and Industry (but not Asset)
  # Expand to the coverage
  secIndToEx <- filter(al, Industry == "ALL", Sector == "ALL", Asset != "ALL")
  secIndToEx <- mutate(secIndToEx, Sector = NULL, Industry = NULL)
  secIndExpanded <- secIndToEx %>% left_join(toCover, by = "Asset")
  # Filter to what's not already specified
  secIndExpanded <- secIndExpanded %>% anti_join(specRows, by = allKeys) %>%
    select(Sector, Industry, Asset, everything())

  # combine everything
  finalSpec <- bind_rows(specRows, secIndExpanded)

  # Gather and rename
  finalSpec <- finalSpec %>% gather(Period, Value, -Sector, -Industry, -Asset) %>%
    rename_(.dots = setNames("Value", varName))

  return(finalSpec)

}
