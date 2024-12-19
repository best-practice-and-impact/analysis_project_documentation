############################# OUTPUT PIM RESULTS ####################################


library(dplyr)

library(tidyr)

library(readr)

library(futile.logger)


# Check we have an "out" dataframe. Assume that one should be loaded if not present in the environment.

if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")


# ---------------- Select variables and write to CSV ---------------------------

pimOutput <- tidyr::unnest(out)


# Define and select necessary variables for output to CSV

baseVariables <- c("Sector", "Industry", "Asset", "Period")

requiredVariables <- c("GrossStockCVM", "NetStockCVM", "ConsumptionOfFixedCapitalCVM",

"GrossStockCP", "NetStockCP", "ConsumptionOfFixedCapitalCP")


# Select the variables

pimOutput <- pimOutput %>% dplyr::select(dplyr::all_of(c(baseVariables, requiredVariables)))


# Quality Assurance (QA) section simplified and commented for clarity

# Assuming function and previous files are appropriately managed

prev_files <- Sys.glob(paste0(last_run, "/Outputs/*.csv"))

prev_pimOutput <- prev_files[dplyr::first(dplyr::str_detect(prev_files, "pim_outputs"))] # Using tidyverse functions for pattern detection


prev_pimOutputCC <- readr::read_csv(prev_pimOutput) %>%

dplyr::filter(Measure == "ConsumptionOfFixedCapitalCP") %>%

tidyr::gather("Period", "Value", -c(Sector, Industry, Asset, Measure)) %>%

dplyr::mutate(Year = as.numeric(substr(Period, 2, 5)),

Period = dplyr::if_else(Period > "Y1996" & Period <= publishTo, Period, NA_character_),

Value = round(as.numeric(Value), 2))


pimOutputCC <- dplyr::filter(pimOutput, Measure == "ConsumptionOfFixedCapitalCP") %>%

dplyr::mutate(Value = round(as.numeric(Value), 2)) %>%

dplyr::select(Sector, Industry, Asset, Period, Value)


# Writing output with gathered and modified measures

pimOutput <- pimOutput %>%

tidyr::gather("Measure", "Value", requiredVariables) %>%

dplyr::mutate(Value = round(Value, 2)) %>%

tidyr::spread(Period, Value, fill = 0)


# Write to CSV file

readr::write_csv(pimOutput, paste0(outputDir, "pim_outputs_", runTime, ".csv"))


# Optional sharing condition commented for clarity

#if (OUTPUT_SHARING) {

# readr::write_csv(pimOutput, paste0(otherOutputs, "pim_outputs_", runTime, ".csv"))

# futile.logger::flog.info("PIM output files written.")

#}


# -------------------------- Remove Objects ------------------------------------

rm(baseVariables, requiredVariables, pimOutput)