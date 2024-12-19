############################# PROCESS GFCF #####################################


library(dplyr)

library(tidyr)

library(futile.logger)

library(readr)


# Combine the GFCF and Price Index data.frames from the main sectors and S.13 sectors into one data.frame

# Save results


# ======================= COMBINE GFCF with S.13 ===============================


if (exists("gfcfS13")) {

futile.logger::flog.info("Combining GFCF and Deflators for main sectors and S.13 sectors.")

gfcf <- dplyr::bind_rows(gfcf, gfcfS13)

rm(gfcfS13)

}


# Zero out if projection set


if (nchar(project_from) == 7) {

quarters <- dplyr::distinct(gfcf, Period) %>%

dplyr::mutate(compare = substr(Period, 2, 5)) %>%

dplyr::filter(compare > substr(project_from, 2, 5) |

(compare == substr(project_from, 2, 5) & substr(Period, 7, 7) >= substr(project_from, 7, 7))) %>%

dplyr::pull(Period)


gfcf <- dplyr::mutate(gfcf, gfcfCP = ifelse(Period %in% quarters, 0, gfcfCP))

}


# Re-referencing deflators - probably now unnecessary


if (tolower(correct_referencing) == "true") {

gfcf <- dplyr::group_by(gfcf, Sector, Industry, Asset) %>%

dplyr::mutate(adjustment = sum(PriceIndex[Period == refPeriod])) %>%

dplyr::ungroup()

} else {

gfcf$year <- substr(gfcf$Period, 2, 5)

gfcf <- dplyr::group_by(gfcf, Sector, Industry, Asset) %>%

dplyr::mutate(adjustment = sum(PriceIndex[as.numeric(year) == as.numeric(substr(refPeriod, 2, 5))]) / 4) %>%

dplyr::ungroup()

}


gfcf$PriceIndex <- gfcf$PriceIndex / gfcf$adjustment

gfcf <- dplyr::select(gfcf, -adjustment, -year)


# Write out copy of input files


gfcf_table <- dplyr::filter(gfcf, as.numeric(substr(Period, 2, 5)) > 1996) %>%

dplyr::select(-PriceIndex) %>%

tidyr::spread(key = Period, value = gfcfCP)


readr::write_csv(gfcf_table, paste0(outputDir, "gfcf_table.csv"))


shared_inputs_gfcf <- tidyr::spread(dplyr::select(gfcf, -PriceIndex), Period, gfcfCP)


deflators_table <- dplyr::filter(gfcf, as.numeric(substr(Period, 2, 5)) > 1996) %>%

dplyr::select(-gfcfCP) %>%

tidyr::spread(key = Period, value = PriceIndex)


readr::write_csv(deflators_table, paste0(outputDir, "deflators_table.csv"))


shared_inputs_deflators <- tidyr::spread(dplyr::select(gfcf, -gfcfCP), Period, PriceIndex)


# Remove temporary data frames to free up memory

rm(gfcf_table, deflators_table)


# ============================ OUTPUT DATA =====================================


if (WRITE_FILES) {

readr::write_rds(gfcf, paste0(outputDir, "prepared_gfcf_and_defs", runTime, ".Rds"))

}


futile.logger::flog.info("GFCF and Deflator preparation complete.")


# ============================ CALC SPLITS =====================================


# Calculate industry splits for Transfer Costs in current run

# If current run is unconstrained (no closed period) these outputs can be used

# in input files for the next constrained run to avoid revisions to the closed period


# Industry splits for S.1PT

TC <- dplyr::filter(gfcf, Asset == "LAND.IMPROVEMENTS.TC") %>%

dplyr::select(Period, Sector, Industry, Asset, gfcfCP) %>%

dplyr::filter(Period > "Y1996Q4") %>%

dplyr::group_by(Sector, Asset, Period) %>%

dplyr::mutate(sum = sum(gfcfCP)) %>%

dplyr::ungroup() %>%

dplyr::mutate(split = gfcfCP / sum) %>%

dplyr::select(-c(gfcfCP, sum))


# Manually set S.125PF to be 1 as there is no GFCF in this sector

TC <- dplyr::mutate(TC, split = dplyr::if_else(Sector == "S.125PF", 1, split)) %>%

dplyr::mutate(Asset = "TC") %>%

tidyr::spread(key = Period, value = split, fill = 0)


# Read in old splits to ensure coverage is the same

oldsplits <- readr::read_rds("J:/Annual round/RUN_05/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds")


# Extend old splits file to go up to the latest year

if (length(dplyr::setdiff(names(TC), names(oldsplits))) > 0) {

for (d in dplyr::setdiff(names(TC), names(oldsplits))) {

oldsplits[[d]] <- oldsplits[[ncol(oldsplits)]]

}

}


# Remove extra series in new splits and add any missing splits from old splits file

TC <- dplyr::mutate(TC, concat = paste0(Sector, Industry, Asset))

oldsplits <- dplyr::mutate(oldsplits, concat = paste0(Sector, Split, Asset))


# Calculate difference and merge

no_TC <- dplyr::anti_join(oldsplits, TC, by = "concat")

no_oldsplits <- dplyr::anti_join(TC, oldsplits, by = "concat")


# Clean up the data frames

TC <- dplyr::select(TC, -concat)

no_TC <- dplyr::select(no_TC, -concat)


TC <- dplyr::mutate(TC, Split = Industry, Industry = "68") %>%

dplyr::select(Sector, Split, Asset, everything())


# Match coverage of old splits to new file & write to outputs folder

newSplits <- dplyr::bind_rows(TC, no_TC) %>%

dplyr::select(-concat) %>%

readr::write_rds(paste0(outputDir, "prev_indSplit.Rds"))


# Sector splits for S.12

s12 <- dplyr::filter(gfcf, dplyr::between(Sector, "S.125IN", "S.12IN"), Asset == "LAND.IMPROVEMENTS.TC") %>%

dplyr::select(Period, Sector, Industry, Asset, gfcfCP) %>%

dplyr::filter(Period > "Y1996Q4") %>%

dplyr::group_by(Period) %>%

dplyr::summarise(gfcfCP = sum(gfcfCP)) %>%

dplyr::ungroup() %>%

dplyr::mutate(split = gfcfCP / sum(gfcfCP)) %>%

dplyr::select(-c(gfcfCP)) %>%

dplyr::mutate(Sector = "S.12IN", Industry = "ALL", Asset = "ALL") %>%

tidyr::spread(key = Period, value = split, fill = 0) %>%

readr::write_rds(paste0(outputDir, "prev_s12split.rds"))


# Clean up by removing objects no longer needed

rm(list = c("TC", "s12", "oldsplits", "newSplits", "no_TC", "no_oldsplits"))


futile.logger::flog.info("Splits calculation and processing complete.")