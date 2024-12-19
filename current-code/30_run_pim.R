############################# PROCESS GFCF #####################################


library(dplyr)

library(tidyr)

library(futile.logger)

library(pimIO) # Assuming 'runAll' comes from the 'pimIO' package


# Run PIM on prepared inputData data.frame

# Check for any PIM failures

# Remove input parameters

# Save results


# =========================== Run PIM ==========================================

futile.logger::flog.info("Starting PIM run.")

out <- pimIO::runAll(inputData)

cat("\n")

futile.logger::flog.info("PIM run complete.")


# ========================= Check for PIM failures =============================

# Check for any series where PIM has thrown an error

failures <- unlist(lapply(out$data, FUN = function(x) inherits(x, "error")), use.names = FALSE)


if (sum(failures) > 0) {

futile.logger::flog.warn(paste0(sum(failures), " series failed to process:"),

out[failures, ], capture = TRUE)

} else {

futile.logger::flog.info("All series processed successfully.")

}


# ========================= Save Results =======================================

# Remove original "data" column as the input data is included in the results

# Remove "config" as this is saved already in pimInput.Rds

out <- dplyr::select(out, -data, -config)


# Rename columns to match standard format of "CP" and "CVM" at the end of names

# Some CVM output cols are not labelled as such

# Rename Vintage to Period now that PIM is finished

out <- dplyr::mutate(out, Period = Vintage, Year = as.numeric(substr(Vintage, 2, 5))) %>%

dplyr::rename(GrossStockCVM = GrossStock,

NetStockCVM = NetStock,

ProductiveStockCVM = ProductiveStock) %>%

dplyr::select(-Vintage)


# Remove input parameters

# The output from the PIM also includes all the input parameters. These will

# be removed to save memory and to make the outputs easier to analyse

out <- dplyr::select(out, -dplyr::matches("^Average|K62CVM$"), -gfcfCP)


# Re-nest the output for onward processing

out <- dplyr::group_by(out, Sector, Industry, Asset) %>%

tidyr::nest(.key = data) # unchainAll expects data to be in "data" list-column


# Save entire output dataset

if (OUTPUT_SHARING == TRUE) {

readr::write_rds(out, paste0(otherOutputs, "pimOutput_", runTime, ".Rds"))

} else {

readr::write_rds(out, paste0(outputDir, "pimOutput_", runTime, ".Rds"))

}


# -------------------------- Remove Objects ------------------------------------

rm(failures)