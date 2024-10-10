############################# PROCESS GFCF #####################################

# Run PIM on prepared inputData data.frame
# Check for any PIM failures
# Remove input parameters
# Save results

# =========================== Run PIM ==========================================
flog.info("Starting PIM run.")
out <- runAll(inputData)
cat("\n")
flog.info("PIM run complete.")

# ========================= Check for PIM failures =============================

# Check for any series where PIM has thrown an error
failures <- unlist(lapply(out$data, FUN = function(x) inherits(x, "error")))
if (sum(failures) > 0) {
  flog.warn(paste0(sum(failures), " series failed to process:"),
            out[failures, ], capture = TRUE)
} else {
  flog.info("All series processed sucessfully.") 
}

# ========================= Save Results =======================================

# Remove original "data" column as the input data is included in the results
# Remove "config" as this is saved already in pimInput.Rds
out <- select(out, -data, -config)

# Rename columns to match standard format of "CP" and "CVM" at the end of names
# Some CVM output cols are not labelled as such
# Rename Vintage to Period now that PIM is finished
out <- out %>% unnest() %>%
  rename(GrossStockCVM = GrossStock,
         NetStockCVM = NetStock,
         ProductiveStockCVM = ProductiveStock,
         Period = Vintage) %>%
  mutate(Year = as.numeric(substr(Period, 2, 5)))   # Add a Year column

# Remove input parameters
# The output from the PIM also includes all the input parameters. These will
# be removed to save memory and to make the outputs easier to analyse
out <- out %>%
  select(-(Average:K62CVM), -gfcfCP)

# Re-nest the output for onward processing
out <- out %>% 
  group_by(Sector, Industry, Asset) %>%
  nest(.key = data)  # unchainAll expects data to be in "data" list-column

# Save entire output dataset
if (OUTPUT_SHARING == TRUE) {
  write_rds(out, paste0(otherOutputs, "pimOutput_", runTime, ".Rds"))
} else {
  write_rds(out, paste0(outputDir, "pimOutput_", runTime, ".Rds"))
}

# -------------------------- Remove Objects ------------------------------------
rm(failures)
