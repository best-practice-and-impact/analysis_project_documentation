# #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
# #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
# i
# VERSION  WIP
# #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
# #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

############################## PARAMETER SETUP ################################
# iv

## N.B. run the next 2 lines individually and input answer in console
DELIVERY <- "Q"
SHARED_INPUT_RUN_NO <- "2024Q2M2R3_R4_run"
ANNUAL_DELIVERY <- ifelse(DELIVERY == "Q",FALSE,TRUE)



# 
# DELIVERY <- "Q" # Set to "TRUE"A" for annual deliveries, "P" for publication round deliveries and "Q" for quarterly round deliveries
# ANNUAL_DELIVERY <- FALSE # for CSDB status
USE_SHARED_INPUT <- TRUE # Set to TRUE if you want to use the files from the shared drive folder
# SHARED_INPUT_RUN_NO <- "2022Q4M3R1" # Three digit number in quotation marks
# OUTPUT_SHARING <- FALSE # Set to TRUE to write output files in shared drive, FALSE outputs to the outputs folder
# WRITE_FILES <- TRUE  # Set to FALSE for test runs where no files are written to disk
# WRITE_LOG <- FALSE  # Set to TRUE to write log file in outputDir, FALSE outputs to console
# SHUTDOWN <- FALSE # Set to TRUE if you want to run overnight and shutdown computer when run is complete
# QA_TABLES_CHARTS <- TRUE # Set to TRUE if you want QA tables and charts
# last_del <- "RUN_04" # run no. for adjusted deliveries closed period to be consistent with
# ADJUSTED_DELIVERIES <- FALSE # to produce a separate folder of deliveries (closed period consistent with another run)

runTime <- format(Sys.time(), "%Y-%m-%d_%H%M") # Used in various file out names

#################################################################################

##################### PREPARE DATA AND RUN PIM #################################

# Top level script to run each stage of a PIM run
# Prepare GFCF and Deflators
# Add PIM Parameters
# Run PIM
# Process and output results
################################################################################
########## SET LIBRARY PATH AND WORKING DIRECTORY  #############################

# Set library location to capstock-specific versions
# ii
#memory.limit()
#memory.limit(size = 10000)
# Set the working directory
setwd("D:/Current code/current-code") # MAKE SURE THIS MATCHES THE VERSION NUMBER ON LINE 3

outputDir <- "D:/Current code/" # must be different to working directory when using gitlab
################################################################################
################################ LOAD PACKAGES #################################
# CRAN packages
# iii
library(dplyr)
library(readr)
library(readxl)
library(testthat)
library(tibble)
library(tidyr)
# Capstock packages
library(capstock)
library(pimIO)
library(prepim)
# Packages for Forecasting
library(forecast)
library(stringr)
# SQL
library(sqldf)
library(cellranger)
library(RSQLite)

library(lazyeval)
library(ggplot2)
library(writexl)

library(futile.logger)

#################################################################################
######################### INPUT DRIVE SELECTION #################################
# v

if (DELIVERY=="A"){
  
  inputDir <- paste0("D:/RUN_16/Inputs/")
  inputDir <- paste0("//nsdata1/Capital_Stock_R_System/Annual round/", SHARED_INPUT_RUN_NO,"/Inputs/")
  FOLDER <- "Annual"
  
}
if (DELIVERY=="P"){
  
  inputDir <- paste0("//nsdata1/Capital_Stock_R_System/Publication round/", SHARED_INPUT_RUN_NO,"/Inputs/")
  FOLDER <- "Publication"
  
}
if (DELIVERY=="Q") {
  
  inputDir <- paste0("//nsdata1/Capital_Stock_R_System/Quarterly round/", SHARED_INPUT_RUN_NO,"/Inputs/")
  FOLDER <- "Quarterly"
  
}

if (USE_SHARED_INPUT==FALSE){
  inputDir <- paste0(getwd(),"/output ", FOLDER,"/")  # Remember this is relative to your working directory
}

#################################################################################
############################## PARAMETER SETUP 2 ################################
# vii
# Read run parameters file
params <- read_csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/run_parameters.csv"), col_types = cols(.default = col_character()))
# Reformat to allow nicer notation, e.g. "param$linkPeriod"
params <- select(params, Parameter, Value) %>% spread(Parameter, Value)

shared_inputs_params <- select(params, refPeriod, toChainFrom, toChainTo)

linkPeriod <- params$linkPeriod
refPeriod <- params$refPeriod
# Now use a quarter to forecast to rather that set quarters (this did not work when r4.2 and s13 gfcf went up to
# different periods). Last complete year is derived from forecast to in order to reduce inputs and chances that this
# is not updated
forecastTo <- params$forecastTo # used for forecasting gfcf and deflators
lastCompleteYear <- ifelse(as.numeric(substr(forecastTo,7,7))==4, as.numeric(substr(forecastTo,2,5)),
                           as.numeric(substr(forecastTo,2,5))-1)  # Calculating last complete year - used for chaining
toChainFrom <- params$toChainFrom # used to reduce data down to a manageable amount before chaining (60_Chain.R)
toChainTo <- params$forecastTo # used to reduce data down to a manageable amount before chaining (60_Chain.R) (edited to force this to equal foreecastTo)
ms_last_closed_quarter <- params$ms_last_closed_quarter # Keeps previous GFCF delivery for closed period
xms_last_closed_quarter <- params$xms_last_closed_quarter # Keeps previous GFCF delivery for closed period
project_from <- params$project_from # Zero out from this quarter and onwards
last_BB_run <- params$last_BB_run
last_run <- params$last_run
Include_LI_GFCF <- params$GFCF_LI
correct_CVM <- params$correct_CVM
publishTo <- params$publishTo
publishTables <- params$publishTables
correct_referencing <- FALSE

USE_SHARED_INPUT <- params$shared_inputs
OUTPUT_SHARING <- params$output_sharing
WRITE_FILES <- params$write_files
WRITE_LOG <- params$write_log
SHUTDOWN <- params$shutdown
QA_TABLES_CHARTS <- params$qa_tables_charts
last_del <- params$last_del
ADJUSTED_DELIVERIES <- params$adjusted_del


################################################################################
############ OUTPUT DRIVE SELECTION AND FOLDER CREATION ########################
# vi
if (OUTPUT_SHARING) {
  outputDir <- paste0("//nsdata1/Capital_Stock_R_System/Shared Outputs ", FOLDER,"/", runTime, "/")
  otherOutputs <- paste0("output ", FOLDER,"/", runTime, "/")
  if (!dir.exists(otherOutputs)) dir.create(otherOutputs, recursive = TRUE)
} else {
  outputDir <- paste0(outputDir,"output ", FOLDER,"/", runTime, "/")
}
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

rm(FOLDER)

outputDir <- paste0("D:/RUN_16/Outputs/")

#################################################################################
#################################################################################
# viii
if (WRITE_LOG==TRUE) {
  flog.appender(appender.tee(paste0(outputDir, "pimrun_", runTime, ".log")))
} else {
  flog.appender(appender.console())
}
#################################################################################
############################### RUN SCRIPTS #####################################
# ix
# kelly

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


################################################################################
########### ########### CARRY OUT SOME GARBAGE COLLECTION ######################
# x
rm(chainedUnnest, chainedUnnestAnnual)

rm(aggregated)

rm(out)

rm(params)

rm(cofogCenGoV, cofogLocGoV)

rm(gfcf, inputData)

#############################################################################
#############################################################################
if (SHUTDOWN==TRUE){
  
  system('shutdown -s') # This line will shut down computer after run is complete if SHUTDOWN is TRUE
  
}

