######################## OUTPUT PIM RESULTS ####################################

# Select some of the PIM outputs and write to CSV
# Note this is before any unchaining/aggregating/chaining of the results

# Check we have an "out" dataframe
# We may want to load one from a previous run, e.g.
# #### OLD INPUT DATASET
#out <- read_rds("J:/Publication round/Test8/Outputs/pimOutput_2021-12-08_1122.Rds")
#out <- read_rds("J:/Annual round/RUN_22/Outputs/pimOutput_2022-12-14_1209.Rds")
#out <- read_rds("J:/Annual round/RUN_13/Outputs/pimOutput_2022-09-27_1059.Rds")
#out <- read_rds("J:/Annual round/RUN_04/Outputs/pimOutput_2022-04-12_1141.Rds")
#terminal <- read_rds("J:/Personal working area/Trina Evans/terminal.rds")
#out <- filter(out, Sector!="S.1311" | Sector=="S.1311" & Asset=="WEAPONS")
#out <- filter(out, Sector!="S.11PR" | Sector=="S.11PR" & Asset=="TRANSPORT")
  
if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")

# ---------------- Select variables and write to CSV ---------------------------
pimOutput <- unnest(out)

# Save selected variables for output to CSV
baseVariables <- c("Sector", "Industry", "Asset", "Period")

# Define the variables required for output
requiredVariables <- c("GrossStockCVM", "NetStockCVM", "ConsumptionOfFixedCapitalCVM", #ProductiveStockCVM",
                       "GrossStockCP", "NetStockCP", "ConsumptionOfFixedCapitalCP") #ProductiveStockCP")

# Select the variables
pimOutput <- pimOutput %>% select(one_of(c(baseVariables, requiredVariables)))

# QA

prev_files <- (Sys.glob(paste0(last_run, "/Outputs/*.csv")))
for (i in prev_files){
  
  if (grepl("pim_outputs", i))
    
    prev_pimOutput <- i
  
}

prev_pimOutput <- read.csv(prev_pimOutput, stringsAsFactors=FALSE)
prev_pimOutputCC <- filter(prev_pimOutput, Measure=="ConsumptionOfFixedCapitalCP")
prev_pimOutputCC <- gather(prev_pimOutputCC, Period, Value, 5:ncol(prev_pimOutputCC))
prev_pimOutputCC <- select(prev_pimOutputCC, Sector, Industry, Asset, Period, Value)
pimOutputCC <- select(pimOutput, Sector, Industry, Asset, Period, ConsumptionOfFixedCapitalCP)
pimOutputCC <- rename(pimOutputCC, Value = ConsumptionOfFixedCapitalCP)
pimOutputCC$Value[is.na(pimOutputCC$Value)] <- 0
pimOutputCC$Value <- round(pimOutputCC$Value,2)
pimOutputCC$year <- substr(pimOutputCC$Period,2,5)
prev_pimOutputCC$year <- substr(prev_pimOutputCC$Period,2,5)
pimOutputCC$year <- NULL
prev_pimOutputCC$year <- NULL
prev_pimOutputCC <- filter(prev_pimOutputCC, Period > paste0("Y1996"), Period <= publishTo)
pimOutputCC <- filter(pimOutputCC, Period > paste0("Y1996"), Period <= publishTo)
# QA(pimOutputCC, prev_pimOutputCC, "pimoutput_CC", "additive", annual = FALSE, limit = 500)

# Gather up the variables under a new "Measure" column
pimOutput <- pimOutput %>% gather_(key = "Measure", value = "Value", requiredVariables)

# Round
pimOutput <- pimOutput %>% mutate(Value = round(Value, 2))

# Spread the Period across the columns
pimOutput <- pimOutput %>% spread(Period, Value, fill = 0)

# Write to a CSV
write_csv(pimOutput, paste0(outputDir, "pim_outputs_", runTime, ".csv"))

#if (OUTPUT_SHARING == TRUE) {
  #write_excel_csv(pimOutput, path = paste0(otherOutputs, "pim_outputs", runTime, ".csv"))
  #flog.info("PIM output files written.")
#} else {
  #write_csv(pimOutput, paste0(outputDir, "pim_outputs_", runTime, ".csv"))
  #flog.info("PIM output files written.")
#}

# -------------------------- Remove Objects ------------------------------------
rm(baseVariables, requiredVariables, pimOutput)

