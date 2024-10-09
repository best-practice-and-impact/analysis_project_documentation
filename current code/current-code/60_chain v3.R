# ======================= CHAIN AND ANNUALISE ============================================
#datForI <- sqldf("SELECT * FROM aggregated WHERE Industry = '77'")
#write.table(datForI, file = paste0(outputDir, "Data for Iasmina - Industry 49", "_", runTime, ".csv"), row.names = FALSE, sep=",")
#write_csv(test, paste0(outputDir, "TestDataset.csv"))

# Add in NPISH values for certain Industries based on Ratios
# Filter the aggregation dataset to the required level and time period
# Chain
# Annualise

flog.info("Chaining Subset of Data.")

##################################################################################################################################
# PULL IN SAVED OUTPUT
#### YOU WILL NEED TO CHANGE THE FILENAME BASED ON WHAT IS IN YOUR OUTPUT FOLDER ####
# aggregated <- read_rds("output/AGGREGATED.Rds")

##################### SOURCE FILE FOR FUNCTIONS ###############################

source("miscCapStocksFunctions.R")

##################################################################################################################################
# write_excel_csv(aggregated, path = paste0(outputDir, "AGGREGATED.csv"))
# spareAggregated <- aggregated
# aggregated <- spareAggregated
##################################################################################################################################

cols <- names(aggregated)

##################################################################################################################################
#### SPLIT SOME NPISH VALUES DOWN BY RATIOS PULLED IN FROM SPREADSHEET
NPISHproRate <- read.csv(paste0(inputDir, "Splits/NPISH pro-rate Pattern.csv"))
sect <- 'S.15'
indust <- NPISHproRate$Map
NPISHproRate <- NPISHproRate %>% gather_(key = "Period", value = "Perc", names(NPISHproRate[,5:ncol(NPISHproRate)]))
NPISHproRate <- NPISHproRate %>% filter(Period >= toChainFrom, Period <= toChainTo)
NPISHproRate$Perc <- (as.numeric(NPISHproRate$Perc) / 100)
subDat <- aggregated %>% filter(Sector == sect, Industry %in% indust)
subDat <- subDat %>% filter(Period >= toChainFrom, Period <= toChainTo) %>% gather_(key = "Measure", value = "Value", names(subDat[,5:19]))
subDat <- sqldf("SELECT Sector, Industry, Asset, Period, MAX(Sector_Level) AS Sector_Level, MAX(Industry_Level) AS Industry_Level,
                MAX(Asset_Level) AS Asset_Level, MAX([Group]) AS [Group], Measure, MAX(Value) AS Value FROM subDat
                GROUP BY Sector, Industry, Asset, Period, Measure")

#### JOIN THE SUBSET AND NPISH DATA BY PERIOD CREATING A NEW VALUE BASED ON PERCENTAGE VALUE
subDat <- sqldf("SELECT a.Sector, b.A112 AS Industry, a.Asset, a.period, a.Sector_Level, a.Industry_Level, a.Asset_Level, [Group], Measure,
                a.Value * b.Perc AS Value FROM subDat a
                LEFT JOIN NPISHproRate b ON a.period = b.Period AND a.Industry = b.Map")

# The next line records duplicates if chaining prior to 1995 - could do with resolving this!!!
subDat <- subDat%>%distinct()
subDat <- subDat %>%spread(Measure, Value, fill = 0)
subDat <- subset(subDat, select=cols)
aggregated <- rbind(aggregated, subDat)

rm(subDat, NPISHproRate, indust, sect)

##################################################################################################################################


# ---------------------- Filter Aggregation Dataset ----------------------------

# Note the dataset could get very big at this point as we have every possible combination of aggregations.
# Since "chain" is a time-intensive function we want to be selective now as to which level of hierarchies we chain

## REDUCE THE AGGREGATED DATA DOWN TO WHAT IS NEEDED USING THE COVERAGE TABLE IN AN EXCEL FILE IN THE INPUT FOLDER
## PULL IN THE COMBINATIONS OF SECTOR, INDUSTRY AND ASSET
covTabStr <- paste0(inputDir, "Mapping & Aggregation/CVM_coverage_table.xlsx")               # FILE PATH STRING
##covTabStr <- paste0("Input/Mapping & Aggregation/TEST CVM_coverage_table.xlsx")             # FILE PATH STRING
covTab <- read_excel(covTabStr, sheet = "CVM", col_types = "text")                           # OPEN THE FILE AND SELECT THE SHEET
## CREATE A NEW COLUMN THAT HOLDS A CONCATENATED STRING OF SECTOR, INDUSTRY AND ASSET
aggregated$coverage <- paste0(aggregated$Sector, aggregated$Industry, aggregated$Asset)
## DO THE SAME FOR THE COVERAGE DATA
covTab$Coverage <- paste0(covTab$Sector, covTab$Industry, covTab$Asset)
## SELECT THE DATA NEEDED BASED ON THE NEW COVERAGE COLUMNS AND A DATE FROM AND TO
toChain <- aggregated %>% filter(coverage %in% covTab$Coverage, Period >= toChainFrom)
#toChain <- aggregated %>% filter(Period >= toChainFrom)
toChain <- toChain %>% filter(Period <= toChainTo)

# DO SOME CLEANING
rm(covTabStr, covTab)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
#### WEAPONS AND OTHER.MACHINERY ADDED HERE RATHER THAN ADD ALL THE DIFFERENT COMBINATIONS IN THE COVERAGE FILE
#toChain <- aggregated
othMacWeap <- aggregated %>% filter(Asset %in% c('OTHER.MACHINERY','WEAPONS','MACH.EQ'), Period >= toChainFrom)
othMacWeap <- othMacWeap %>% filter(Period <= toChainTo)
toChain <- rbind(toChain, othMacWeap)
rm(othMacWeap)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# OUTPUT THE DATA JUST IN CASE
#write_rds(toChain, paste0(outputDir, "ReducedAggregatedData", ".Rds"))

##################################################################################################################################

# NOT ALL MEASURES (COLUMNS) ARE NEEDED AT THE MOMENT SO REMOVE THEM
toChain <- toChain %>% select(-ProductiveStockCP, -ProductiveStockCYP, -ProductiveStockPYP)
toChain <- toChain %>% select(-CapitalServicesCP, -CapitalServicesCYP, -CapitalServicesPYP)

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
cont1 <- nrow(toChain)
# JUST IN CASE YOU HAVE DUPLICATE DATA, GROUP TO REMOVE, (Sector/A21d/Asset1, Sector/A38/Asset1, Sector/A64/Asset1) all THE same !!!!
toChain <- sqldf("SELECT Sector, Industry, Asset, Period, GrossStockCYP, GrossStockPYP, GrossStockCP, NetStockCYP, NetStockPYP, NetStockCP,
                 ConsumptionOfFixedCapitalCYP, ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP, MAX([Group]) AS [Group]
                 FROM toChain GROUP BY Sector, Industry, Asset, Period, GrossStockCYP, GrossStockPYP, GrossStockCP, NetStockCYP, NetStockPYP, NetStockCP,
                 ConsumptionOfFixedCapitalCYP, ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP")
cont2 <- nrow(toChain)
if (cont1 != cont2)
{
  paste0("GROUPING STATEMENT HAS reduced toChain Rows by: ", toString(cont1 - cont2))
}
rm(cont1, cont2)
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

#### SECTION TO CALL BESPOKE CALCULATIONS - CALLED FROM SCRIPT bespokeDataCalculations.R ####

source("bespokeDataCalculations.R")
toChain <- createBespokeSecIndAssAggregations(toChain)

####  WEAPONS AND OTHER.MACHINERY ARE NOT NEEDED IN ANY OF THE CURRENT CRITERIA FOR CORD SO REMOVE TO IMPROVE SPEED OF CHAINING
####  NOW NEEDED, SO COMMENTED OUT
####  toChain <- toChain %>% filter(!(Asset %in% c('OTHER.MACHINERY','WEAPONS')))

#######################################################################################################################################################################
#######################################################################################################################################################################
#######################################################################################################################################################################

# CHAIN THE DATA - THIS FUNCTION CHAINS BY GROUP IN A LOOP TO HELP WITH MEMORY PROBLEMS
# benchType can take 3 forms
# 'avg' as a string
# 'sum' as a string
# 4 as an integer
source("miscCapStocksFunctions.R")
refYear <- substr(refPeriod,2,5)
refYear <- as.double(refYear)

# Chaining should be done prior to rounding - until this is corrected
# constraining CYPs to CPs in reference year

if (correct_CVM==TRUE){
  
  post_ref <- as.character(as.numeric(substr(refPeriod,2,5))+1)
  toChain <- toChain %>% group_by(Sector,Industry,Asset) %>% mutate(CYP_adj = (sum(ConsumptionOfFixedCapitalCP[substr(Period,2,5)==substr(refPeriod,2,5)]))/
                                                                      sum(ConsumptionOfFixedCapitalCYP[(substr(Period,2,5))==(substr(refPeriod,2,5))])) %>% ungroup()                                                            
  toChain$CYP_adj <- ifelse(is.na(toChain$CYP_adj),0,toChain$CYP_adj)
  toChain$ConsumptionOfFixedCapitalCYP <- toChain$ConsumptionOfFixedCapitalCYP*toChain$CYP_adj
  toChain$CYP_adj <- NULL
  
  toChain <- toChain %>% group_by(Sector,Industry,Asset) %>% mutate(PYP_adj = (sum(ConsumptionOfFixedCapitalCYP[substr(Period,2,5)==post_ref]))/
                                                                      sum(ConsumptionOfFixedCapitalPYP[(substr(Period,2,5))==post_ref])) %>% ungroup()        
  
  toChain$PYP_adj <- ifelse(is.na(toChain$PYP_adj),0,toChain$PYP_adj)
  toChain$ConsumptionOfFixedCapitalPYP <- toChain$ConsumptionOfFixedCapitalPYP*toChain$PYP_adj
  toChain$PYP_adj <- NULL
  
}

#toChain$concat <- paste0(toChain$Sector, " ", toChain$Industry, " ", toChain$Asset)
chained <- chainDataSkippingErrors(toChain%>%filter(Sector%in%c("S.1")),
                                   benchType = 4, refYear, correct_CVM)
#detach("package:capstock", unload=TRUE)
#temp <- unnest(chained)
#temp <- filter(temp, Asset=="TOTAL" & Industry=="TOTAL"& Sector=="S.1")
#write.csv(temp, "temp.csv")

##################################################################################################################################

# CHECK FOR FAILURES AND LIST IF ANY APPEAR
failures <- unlist(lapply(chained$chained, FUN = function(x) inherits(x, "error")))
if (sum(failures) > 0)
{
  errs <- chained[(failures),]
  flog.warn(paste0(sum(failures), " series failed to process:"), chained[failures, ], capture = TRUE)
  chained <- chained[!(failures),]
  #errs <- unnest(errs)
  ##### OUTPUT THE CHAINING ERRORS
  #if (OUTPUT_SHARING == TRUE) {
  #  write_csv(errs, paste0(outputDir, "FailedChaining_", runTime, ".csv"))
  #  write_csv(errs, paste0(otherOutputs, "FailedChaining_", runTime, ".csv"))
  #} else {
  #  write_csv(errs, paste0(outputDir, "FailedChaining_", runTime, ".csv"))
  #}
  #rm(errs)
} else {
  flog.info("All series chained successfully.")
}
#######################################################################

# ATTEMPT TO UNNEST - MIGHT HAVE TO USE UNNEST BY ROW (FIRST FUNCTION CALL) IF THE OVERALL UNNEST METHOD FAILS
# chainedUnnest <- unnestDataWithDifferentRowNumbers(chained)
chainedUnnest <- chained %>% unnest()
#chainedUnnest <- read_rds("output/chainedUnnest.Rds")


#join failed series back as CPs
errs <- unnest(errs, data)
#errs <- select(errs, !(("chained")))
errs$ConsumptionOfFixedCapitalCVM <- NA
errs$GrossStockCVM <- NA
errs$NetStockCVM <- NA
#errs <- select(errs, !concat)
chainedUnnest <- rbind(chainedUnnest,errs)
chainedUnnest <- chainedUnnest[,-15]

#################################################################################################################################
#################################################################################################################################

# REMOVE SOME COLUMNS THAT ARE NO LONGER NEEDED
#chainedUnnest <- chainedUnnest %>% select(-Group, -Year)
chainedUnnest <- chainedUnnest %>% select(-Group)


#################################################################################################################################
#### DONT NEED CVMs FOR COFOG SO YOU HAVE JUST SKIPPED THE CHAINING PART TO SAVE TIME - TIME TO ADD THEM BACK IN
#### REARANGE THE COFOG DATA TO MATCH THE CHAINED UNNESTED DATA BEFORE JOINING
cofogCenGoV <- sqldf("select Sector, Industry, Asset, Period, 0 AS GrossStockCYP, 0 AS GrossStockPYP, GrossStockCP, 0 AS NetStockCYP,  0 AS NetStockPYP, NetStockCP,
                     0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP, 0 AS ConsumptionOfFixedCapitalCVM, GrossStockCVM, NetStockCVM
                     FROM cofogCenGoV WHERE Asset <> 'TOTAL'")
cofogCenGoV <- cofogCenGoV %>% filter(Period >= toChainFrom)

cofogLocGoV <- sqldf("select Sector, Industry, Asset, Period, 0 AS GrossStockCYP, 0 AS GrossStockPYP, GrossStockCP, 0 AS NetStockCYP,  0 AS NetStockPYP, NetStockCP,
                     0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP, ConsumptionOfFixedCapitalCP, 0 AS ConsumptionOfFixedCapitalCVM, GrossStockCVM, NetStockCVM
                     FROM cofogLocGoV WHERE Asset <> 'TOTAL'")
cofogLocGoV <- cofogLocGoV %>% filter(Period >= toChainFrom)

#### AGGREGATE UP THE GOVERNMENT DATA TO TOTAL LEVEL BY ASSET
subDatCen <- sqldf("select Sector, Industry, 'TOTAL' AS Asset, Period, 0 AS GrossStockCYP, 0 AS GrossStockPYP, SUM(GrossStockCP) AS GrossStockCP, 0 AS NetStockCYP, 0 AS NetStockPYP,
                   SUM(NetStockCP) AS NetStockCP, 0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP,
                   SUM(ConsumptionOfFixedCapitalCP) AS ConsumptionOfFixedCapitalCP, 0 AS ConsumptionOfFixedCapitalCVM,
                   SUM(GrossStockCVM) AS GrossStockCVM, SUM(NetStockCVM) AS NetStockCVM
                   FROM cofogCenGoV GROUP BY Sector, Industry, Period")

subDatLoc <- sqldf("select Sector, Industry, 'TOTAL' AS Asset, Period, 0 AS GrossStockCYP, 0 AS GrossStockPYP, SUM(GrossStockCP) AS GrossStockCP, 0 AS NetStockCYP, 0 AS NetStockPYP,
                   SUM(NetStockCP) AS NetStockCP, 0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP,
                   SUM(ConsumptionOfFixedCapitalCP) AS ConsumptionOfFixedCapitalCP, 0 AS ConsumptionOfFixedCapitalCVM,
                   SUM(GrossStockCVM) AS GrossStockCVM, SUM(NetStockCVM) AS NetStockCVM
                   FROM cofogLocGoV GROUP BY Sector, Industry, Period")

cofogCenGoV <- rbind(cofogCenGoV, subDatCen)
cofogLocGoV <- rbind(cofogLocGoV, subDatLoc)
rm(subDatCen, subDatLoc)

#### CREATE BESPOKE ASSET AGGREGATIONS FOR LOCAL GOVERNMENT
source("bespokeDataCalculations.R")
cofogLocGoV <- rbind(cofogLocGoV, bespokeAggregationsForLocGov(cofogLocGoV))

#################################################################################################################################
#################################################################################################################################

#### CREATE A DATASET FOR INDUSTRY T AS TOTAL ASSET -  NOT PRESENT IN DATA BUT NEEDED FOR DELIVERIES
tTab = sqldf("select 'S.1' AS Sector, 'T' AS Industry, Asset, Period,
             0 AS GrossStockCYP, 0 AS GrossStockPYP, 0 AS GrossStockCP,
             0 AS NetStockCYP,  0 AS NetStockPYP, 0 AS NetStockCP,
             0 AS ConsumptionOfFixedCapitalCYP, 0 AS ConsumptionOfFixedCapitalPYP, 0 AS ConsumptionOfFixedCapitalCP,
             0 AS ConsumptionOfFixedCapitalCVM, 0 AS GrossStockCVM, 0 AS NetStockCVM
             FROM chainedUnnest GROUP BY Asset, Period")

#################################################################################################################################
#################################################################################################################################

#### JOIN UP THE FOUR DATASETS
chainedUnnest <- rbind(chainedUnnest, cofogCenGoV, cofogLocGoV, tTab)
rm(tTab)
#chainedUnnest$Year <- substring(chainedUnnest$Period,2,5)
#################################################################################################################################
#################################################################################################################################
# CHAINING NOT WORKING PROPERLY FOR LASTCOMPLETEDYEAR - THIS METHODS THE LAST COMPLETED YEAR CVMS
# GROUP AND SUM CP & CYP VALUES BY SECTOR, INDUSTRY, ASSET AND YEAR
chainedUnnest$Year <- substring(chainedUnnest$Period,2,5)
cp <- chainedUnnest %>% group_by(Sector, Industry, Asset, Year) %>% summarise(cptot = sum(ConsumptionOfFixedCapitalCP))
cyp <- chainedUnnest %>% group_by(Sector, Industry, Asset, Year) %>% summarise(cyptot = sum(ConsumptionOfFixedCapitalCYP))
# JOIN BACK UP WITH ORIGINAL,
chainedUnnest <-left_join(chainedUnnest , cp)
chainedUnnest <-left_join(chainedUnnest , cyp)
# CALCULATE THE NEW CVM
chainedUnnest$cvm <- chainedUnnest$cptot / chainedUnnest$cyptot * chainedUnnest$ConsumptionOfFixedCapitalCYP
# REPLACE THE OLD CVM WITH THE NEW ONE ONLY FOR THE REFERENCE YEAR
chainedUnnest$ConsumptionOfFixedCapitalCVM[chainedUnnest$Year == refYear] <- chainedUnnest$cvm[chainedUnnest$Year == refYear]
# GARBAGE COLLECT
chainedUnnest$cvm <- NULL
chainedUnnest$cptot <- NULL
chainedUnnest$cyptot <- NULL
rm(cp, cyp)
# JUST TAKE THE CYP IN THE REFERENCE YEAR FOR GROSS STOCK AND NET STOCK AS DEFINED BY MR JOE MURPHY
chainedUnnest$GrossStockCVM[chainedUnnest$Year == refYear] <- chainedUnnest$GrossStockCYP[chainedUnnest$Year == refYear]
chainedUnnest$GrossStockCVM[chainedUnnest$Period == paste0("Y", refYear, "Q4")] <- chainedUnnest$GrossStockCP[chainedUnnest$Period == paste0("Y", refYear, "Q4")]

chainedUnnest$NetStockCVM[chainedUnnest$Year == refYear] <- chainedUnnest$NetStockCYP[chainedUnnest$Year == refYear]
chainedUnnest$NetStockCVM[chainedUnnest$Period == paste0("Y", refYear, "Q4")] <- chainedUnnest$NetStockCP[chainedUnnest$Period == paste0("Y", refYear, "Q4")]

########## Make PYP of refyear + 1 = CVM refyear + 1

chainedUnnest$ConsumptionOfFixedCapitalPYP[chainedUnnest$Year == refYear + 1] <- chainedUnnest$ConsumptionOfFixedCapitalCVM[chainedUnnest$Year == refYear + 1]
chainedUnnest$NetStockCVM[chainedUnnest$Year == refYear + 1] <- chainedUnnest$NetStockPYP[chainedUnnest$Year == refYear + 1]
chainedUnnest$GrossStockCVM[chainedUnnest$Year == refYear + 1] <- chainedUnnest$GrossStockPYP[chainedUnnest$Year == refYear + 1]

# REMOVE THE YEAR COLUMN AS IT IS NO LONGER NEEDED
chainedUnnest <- chainedUnnest %>% select(-Year)

#################################################################################################################################

#### NOW SOME OF THE COLUMNS HAVE BEEN REMOVED YOU MIGHT HAVE DUPLICATE DATA SO JUST INCASE GROUP TOGETHER
chainedUnnest <- sqldf("SELECT * FROM chainedUnnest GROUP BY Sector, Industry, Asset, Period ORDER BY Sector, Industry, Asset, Period")

#################################################################################################################################

#source("miscCapStocksFunctions.R")

# ANNUALISE THE DATA
chainedUnnestAnnual <- annualiseData(chainedUnnest)

