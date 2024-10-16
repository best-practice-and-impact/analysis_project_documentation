setwd("D:/Current code/current-code") # MAKE SURE THIS MATCHES THE VERSION NUMBER ON LINE 3


# Configuration
DELIVERY <- "Q" # must be "A" (annual), "Q" (quarterly) or "P" (publication)
SHARED_INPUT_RUN_NO <- "2024Q2M2R3_R4_run"
USE_SHARED_INPUT <- TRUE # Set to TRUE if you want to use the files from the shared drive folder

ANNUAL_DELIVERY <- ifelse(DELIVERY == "Q",FALSE,TRUE) # What about "P"? This variable should be renamed

runTime <- format(Sys.time(), "%Y-%m-%d_%H%M") # Used in various file out names

outputDir <- "D:/Current code/" # must be different to working directory when using gitlab

DELIVERY <- dplyr::recode(DELIVERY, "A" = "Annual", "P" = "Publication", "Q" = "Quarterly")
inputDir <- paste0("//nsdata1/Capital_Stock_R_System/", DELIVERY, " round/", SHARED_INPUT_RUN_NO,"/Inputs/")

if (USE_SHARED_INPUT==FALSE){
  inputDir <- paste0(getwd(),"/output ", DELIVERY,"/")  # Remember this is relative to your working directory
}

#library(dplyr)
#library(readr)
#library(readxl)
#library(testthat)
#library(tibble)
#library(tidyr)
#library(forecast)
#library(stringr)
#library(sqldf)
#library(cellranger)
#library(RSQLite)
#library(lazyeval)
#library(ggplot2)
#library(writexl)
#library(futile.logger)

# Capstock packages
#library(capstock)
#library(pimIO)
#library(prepim)

# More parameters
params <- read.csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/run_parameters.csv"))[c("Parameter", "Value")]
params <- result_list <- setNames(as.list(params$Value), params$Parameter)
shared_inputs_params <- params[c("refPeriod", "toChainFrom", "toChainTo")]
list2env(params, envir = .GlobalEnv)
nclude_LI_GFCF <- GFCF_LI
correct_referencing <- FALSE
USE_SHARED_INPUT <- params$shared_inputs
OUTPUT_SHARING <- params$output_sharing
WRITE_FILES <- params$write_files
WRITE_LOG <- params$write_log
SHUTDOWN <- params$shutdown
QA_TABLES_CHARTS <- params$qa_tables_charts
ADJUSTED_DELIVERIES <- params$adjusted_del

lastCompleteYear <- as.numeric(substr(forecastTo, 2, 5)) - as.numeric(substr(forecastTo, 7, 7) != 4)

if (WRITE_LOG) {
  futile.logger::flog.appender(futile.logger::appender.tee(paste0(outputDir, "pimrun_", runTime, ".log")))
} else {
  futile.logger::flog.appender(futile.logger::appender.console())
}

# Read in CORD downloads and S1311 GFCF
source("./01_inputs v2.R")

# Read and prepare GFCF and Deflators
source("./10_prep_gfcf_and_deflators v3.R")

# Read and prepare GFCF and Deflators for S13
source("./11_prep_gfcf_and_deflators_S13 v2.R")

# Combine all GFCF and Deflators
source("./12_combine_gfcf_and_deflators v2.R")

# Add PIM parameters and reshape data for PIM
source("./20_add_pim_parameters.R")

# Run PIM
source("./30_run_pim.R")

# Write out selected data (before post-processing)
source("./35_write_PIM_outputs.R")

# Unchain results and perform reclassifications
source("./40_unchain.R")

# Map S13 to A64
source("./45_map_S13_COFOG_to_A64.R") #A copy of the COFOG data is kept to one side and added back into the main dataset after chaining (60)

# Aggregate
source("./50_aggregate.R")

# Chain & ANNUALISATION
source("./60_chain v2.R")

# ARRANGES OUTPUT FOR CORD
source("./70_OutputForCORD.R")

# Publication tables
if (publishTables==T){
  source("./Pub_table.R")
}

#Adjusted Deliveries
if (ADJUSTED_DELIVERIES==T){
  source("./Adjusted_Deliveries.R")
}

if (SHUTDOWN==TRUE){
  system('shutdown -s') # This line will shut down computer after run is complete if SHUTDOWN is TRUE
}

