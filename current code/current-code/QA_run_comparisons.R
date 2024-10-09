# Enter output files to compare

# Library

.libPaths("D:/R/capstock_lib")

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(readr)

inputDir <- "D:/Current code/"

##################### Parameters ##############################################################

# Directory
inputDir <- "J:/Annual round/RUN_05/Inputs"
setwd(inputDir) # Where other code is
outputDir <- "J:/Comparison Graphs/" # Folder Where charts will be saved

# ANNUAL CORD IMPORT FILES MUST BE USED, quarterly data will automatically be picked up where possible

# Enter run number and folder run output is located in

SHARED_INPUT_RUN_NO <- "RUN_16"

new_folder_location <- "J:/Annual round"

last_run <- "EUN_06"

previous_folder_location <- "J:/Annual round"

latest_q <- "2023Q2"
publishTo <- latest_q

annual<- FALSE

#differences across series of more than this value are graphed individually
limit <- 500

############ END OF PARAMETERS ##########################################

########################################################################

source("./QA.R")

setwd(inputDir)
outputDir <- paste0(outputDir, SHARED_INPUT_RUN_NO, " v ", last_run,"/")

dir.create(file.path(outputDir))

if (1==2){

  new_deflators <- read_excel(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO,
                                     "/Outputs/QA/deflators_open.xlsx"),
                              sheet = "new")
  new_deflators <- gather(new_deflators,"Period", "Value",4:ncol(new_deflators))
  prev_deflators <- read_excel(paste0(previous_folder_location,"/",last_run,
                                      "/Outputs/QA/deflators_open.xlsx"),
                               sheet = "new")
  prev_deflators <- gather(prev_deflators,"Period", "Value",4:ncol(prev_deflators))
  QA(new_deflators, prev_deflators, "deflators_open", "non-additive")

  new_gfcf_r42_open <- read_excel(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO,
                                         "/Outputs/QA/gfcf_r42_open.xlsx"),
                                  sheet = "new")
  new_gfcf_r42_open <- gather(new_gfcf_r42_open,"Period", "Value",4:ncol(new_gfcf_r42_open))
  prev_gfcf_r42_open <- read_excel(paste0(previous_folder_location,"/",last_run,
                                          "/Outputs/QA/gfcf_r42_open.xlsx"),
                                   sheet = "new")
  prev_gfcf_r42_open <- gather(prev_gfcf_r42_open,"Period", "Value",4:ncol(prev_gfcf_r42_open))
  QA(new_gfcf_r42_open, prev_gfcf_r42_open, "gfcf_r42_open", "non-additive")

  new_gfcf_s13_open <- read_excel(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO,
                                         "/Outputs/QA/gfcf_s13_open.xlsx"),
                                  sheet = "new")
  new_gfcf_s13_open <- gather(new_gfcf_s13_open,"Period", "Value",4:ncol(new_gfcf_s13_open))
  prev_gfcf_s13_open <- read_excel(paste0(previous_folder_location,"/",last_run,
                                          "/Outputs/QA/gfcf_s13_open.xlsx"),
                                   sheet = "new")
  prev_gfcf_s13_open <- gather(prev_gfcf_s13_open,"Period", "Value",4:ncol(prev_gfcf_s13_open))
  QA(new_gfcf_s13_open, prev_gfcf_s13_open, "gfcf_s13_open", "non-additive")


}


# Previous files

new_deflators <- read.csv(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO, "/Outputs/deflators_table.csv"), stringsAsFactors = FALSE)
new_gfcf <- read.csv(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO, "/Outputs/gfcf_table.csv"), stringsAsFactors = FALSE)
prev_deflators <- read.csv(paste0(previous_folder_location,"/",last_run, "/Outputs/deflators_table.csv"), stringsAsFactors = FALSE)
prev_gfcf <- read.csv(paste0(previous_folder_location,"/",last_run, "/Outputs/gfcf_table.csv"), stringsAsFactors = FALSE)

# Re-shape

prev_deflators <- gather(prev_deflators,"Period", "Value",4:ncol(prev_deflators))
prev_gfcf <- gather(prev_gfcf,"Period", "Value",4:ncol(prev_gfcf))
deflators_table <- gather(new_deflators,"Period", "Value",4:ncol(new_deflators))
gfcf_table <- gather(new_gfcf,"Period", "Value",4:ncol(new_gfcf))
prev_gfcf <- filter(prev_gfcf, Period > paste0("Y1986"), Period <= paste0("Y",latest_q))
gfcf_table <- filter(gfcf_table, Period > paste0("Y1986"), Period <= paste0("Y",latest_q))
prev_deflators <- filter(prev_deflators, Period > paste0("Y1986"), Period <= paste0("Y",latest_q))
deflators_table <- filter(deflators_table, Period > paste0("Y1986"), Period <= paste0("Y",latest_q))


# Create QA outputs

QA(new_df = deflators_table, prev_df = prev_deflators, output_name = "deflators", additivity = "non-additive", limit = limit)
QA(new_df = gfcf_table, prev_df = prev_gfcf, output_name = "gfcf", additivity = "additive", annual = annual, limit = limit)

# PIMoutput

new_files <- (Sys.glob(paste0(new_folder_location,"/",SHARED_INPUT_RUN_NO, "/Outputs/*.csv")))
for (i in new_files){

  if (grepl("pim_outputs", i))

    new_pimOutput <- i

}

prev_files <- (Sys.glob(paste0(previous_folder_location,"/",last_run, "/Outputs/*.csv")))
for (i in prev_files){

  if (grepl("pim_outputs", i))

    prev_pimOutput <- i

}

# prev_pimOutput <- "J:/Archive/Shared Outputs Annual_BB20/RUN_021/pim_outputs_2020-09-08_1828.csv"

new_pimOutput <- read.csv(new_pimOutput, stringsAsFactors=FALSE)
new_pimOutputCC <- filter(new_pimOutput, Measure=="ConsumptionOfFixedCapitalCP")
new_pimOutputCC <- gather(new_pimOutputCC, Period, Value, 5:ncol(new_pimOutputCC))
new_pimOutputCC <- select(new_pimOutputCC, Sector, Industry, Asset, Period, Value)
new_pimOutputCC$year <- substr(new_pimOutputCC$Period,2,5)
new_pimOutputCC <- filter(new_pimOutputCC, year>1986, Period <= paste0("Y",latest_q))
new_pimOutputCC$year <- NULL

prev_pimOutput <- read.csv(prev_pimOutput, stringsAsFactors=FALSE)
prev_pimOutputCC <- filter(prev_pimOutput, Measure=="ConsumptionOfFixedCapitalCP")
prev_pimOutputCC <- gather(prev_pimOutputCC, Period, Value, 5:ncol(prev_pimOutputCC))
prev_pimOutputCC <- select(prev_pimOutputCC, Sector, Industry, Asset, Period, Value)
prev_pimOutputCC$year <- substr(prev_pimOutputCC$Period,2,5)
prev_pimOutputCC <- filter(prev_pimOutputCC, year>1986, Period <= paste0("Y",latest_q))
prev_pimOutputCC$year <- NULL
QA(new_df = new_pimOutputCC, prev_df = prev_pimOutputCC, output_name = "pimoutput_CC", additivity = "additive", annual = annual, limit = limit)


#Net stocks

#new_pimOutput <- read.csv(new_pimOutput, stringsAsFactors=FALSE)
new_pimOutputNS <- filter(new_pimOutput, Measure=="NetStockCP")
new_pimOutputNS <- gather(new_pimOutputNS, Period, Value, 5:ncol(new_pimOutputNS))
new_pimOutputNS <- select(new_pimOutputNS, Sector, Industry, Asset, Period, Value)
new_pimOutputNS$year <- substr(new_pimOutputNS$Period,2,5)
new_pimOutputNS <- filter(new_pimOutputNS, year>1986, Period <= paste0("Y",latest_q))
new_pimOutputNS$year <- NULL

#prev_pimOutputA <- read.csv(prev_pimOutputA, stringsAsFactors=FALSE)
prev_pimOutputNS <- filter(prev_pimOutput, Measure=="NetStockCP")
prev_pimOutputNS <- gather(prev_pimOutputNS, Period, Value, 5:ncol(prev_pimOutputNS))
prev_pimOutputNS <- select(prev_pimOutputNS, Sector, Industry, Asset, Period, Value)
prev_pimOutputNS$year <- substr(prev_pimOutputNS$Period,2,5)
prev_pimOutputNS <- filter(prev_pimOutputNS, year>1986, Period <= paste0("Y",latest_q))
prev_pimOutputNS$year <- NULL
QA(new_df = new_pimOutputNS, prev_df = prev_pimOutputNS, output_name = "pimoutput_NS", additivity = "additive", annual = annual, limit = limit*10)

