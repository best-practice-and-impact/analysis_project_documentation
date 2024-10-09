# ======================================== OUTPUT FOR CORD =================================================

# SETS THE DATA INTO A SUITABLE FORMAT FOR CORD INPUT - QUARTERLY AND YEARLY
###############################################################################################################################

# ASSIGN NEW VARIABLES TO KEEP THE ORIGINAL VARIABLES FOR QA DATA - 80_OutputForQA.R
CORDQuarterly <- chainedUnnest
CORDAnnual <- chainedUnnestAnnual

#################################################################################################################################

#### WHATEVER YOU DO TO THE QUARTERLY DATA YOU NEED TO DO TO THE ANNUAL DATA ####

#### GET THE COLUMN HEADINGS YOU DO NOT WANT TO GATHER UP
namesToRemove <- c(names(CORDQuarterly[1:4]))
#### GET ALL THE PERIODS (Y1652Q, 1542, ETC)
requiredVariables <- c(names(CORDQuarterly) [! names(CORDQuarterly) %in% namesToRemove])
#### GATHER ALL THE DATA UP AGAINST THE PERIOD
CORDQuarterly <- CORDQuarterly %>% gather_(key = "Measure2", value = "Value", requiredVariables)
CORDAnnual<- CORDAnnual %>% gather_(key = "Measure2", value = "Value", requiredVariables)
#### CLEAN UP
rm(requiredVariables, namesToRemove)

######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
#### THIS HAS BEEN MOVED TO 60_Chain.R
#### AGGREGATE UP THE GOVERNMENT DATA TO TOTAL LEVEL BY ASSET
#subDatQAgg <- sqldf("SELECT Sector, Industry, 'TOTAL' AS Asset, Period, Measure2, SUM(Value) AS Value FROM CORDQuarterly
#                    WHERE Industry LIKE 'GF%' AND Asset <> 'TOTAL' GROUP BY Sector, Industry, Period, Measure2")
#subDatAAgg <- sqldf("SELECT Sector, Industry, 'TOTAL' AS Asset, Period, Measure2, SUM(Value) AS Value FROM CORDAnnual 
#                    WHERE Industry LIKE 'GF%' AND Asset <> 'TOTAL' GROUP BY Sector, Industry, Period, Measure2")

#CORDQuarterly <- rbind(CORDQuarterly, subDatQAgg)
#CORDAnnual <- rbind(CORDAnnual, subDatAAgg)
#rm(subDatAAgg, subDatQAgg)

######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################

# Find where all the NA and NaN values are (TRUE or FALSE Matrix) replace with ZEROs
naVals <- apply(CORDQuarterly, 2, is.na)
CORDQuarterly[naVals] <- 0
rm(naVals) 
naVals <- apply(CORDAnnual, 2, is.na)
CORDAnnual[naVals] <- 0
rm(naVals) 

#################################################################################################################################

#### CREATE A Price COLUMN BASED ON THE STRING IN MEASURE AND A MEASURE COLUMN BASED ON FIRST FEW CHARACTERS IN THE CURRENT MEASURE COLUMN
CORDQuarterly$Price <- ifelse(grepl('CP', CORDQuarterly$Measure2, fixed=TRUE), 'CP', ifelse(grepl('CVM', CORDQuarterly$Measure2), 'CVM', ifelse(grepl('PYP', CORDQuarterly$Measure2), 'PYP','NA')))
CORDAnnual$Price <- ifelse(grepl('CP', CORDAnnual$Measure2, fixed=TRUE), 'CP', ifelse(grepl('CVM', CORDAnnual$Measure2), 'CVM', ifelse(grepl('PYP', CORDAnnual$Measure2), 'PYP','NA')))
#### CREATE NEW MEASURE COLUMN 
CORDQuarterly$Measure <- ifelse(grepl('Con', CORDQuarterly$Measure2, fixed=TRUE), 'CC', ifelse(grepl('Gross', CORDQuarterly$Measure2), 'GS', ifelse(grepl('Net', CORDQuarterly$Measure2), 'NS','NA')))
CORDAnnual$Measure <- ifelse(grepl('Con', CORDAnnual$Measure2, fixed=TRUE), 'CC', ifelse(grepl('Gross', CORDAnnual$Measure2), 'GS', ifelse(grepl('Net', CORDAnnual$Measure2), 'NS','NA')))

######################################################################################################################################################################
######################################################################################################################################################################

#### PULL IN THE MAPPING TABLES
CORDinputs <- paste0(inputDir, "Mapping & Aggregation/CORD_Input_Hierarchies.xlsx")
indMap <- read_excel(CORDinputs, sheet = "Industry", col_types = "text")
assMap <- read_excel(CORDinputs, sheet = "Asset", col_types = "text")
secMap <- read_excel(CORDinputs, sheet = "Sector", col_types = "text")

######################################################################################################################################################################

#### MAP THE SECTOR, INDUSTRY AND ASSET COLUMNS TO THE CORD CODES - PUT CORD IN FRONT OF THE NEW COLUMN NAMES

#### MAP THE ASSET TO THE CORD ASSET SPECIFICATION
CORDQuarterly <- sqldf(" SELECT b.CORDAsset, a.* FROM CORDQuarterly a LEFT JOIN assMap b ON a.Asset = b.CAPSTOCKCode")
CORDAnnual <- sqldf(" SELECT b.CORDAsset, a.* FROM CORDAnnual a LEFT JOIN assMap b ON a.Asset = b.CAPSTOCKCode")
#### MAP THE INDUSTRY TO THE CORD INDUSTRY SPECIFICATION
CORDQuarterly <- sqldf(" SELECT b.CORDIndustry, a.* FROM CORDQuarterly a LEFT JOIN indMap b ON a.Industry = b.CAPSTOCKCode")
CORDAnnual <- sqldf(" SELECT b.CORDIndustry, a.* FROM CORDAnnual a LEFT JOIN indMap b ON a.Industry = b.CAPSTOCKCode")
#### MAP THE SECTOR TO THE CORD SECTOR SPECIFICATION
CORDQuarterly <- sqldf(" SELECT b.CORDSector, a.* FROM CORDQuarterly a LEFT JOIN secMap b ON a.Sector = b.CAPSTOCKCode")
CORDAnnual <- sqldf(" SELECT b.CORDSector, a.* FROM CORDAnnual a LEFT JOIN secMap b ON a.Sector = b.CAPSTOCKCode")

#### PUT ANY COFOG CODES INTO THE CORDINDUSTRY COLUMN AS THESE WILL NOW BE NAs
CORDQuarterly <- CORDQuarterly %>% mutate(CORDIndustry = ifelse(substr(Industry,1,2) == 'GF', Industry, CORDIndustry))
CORDAnnual <- CORDAnnual %>% mutate(CORDIndustry = ifelse(substr(Industry,1,2) == 'GF', Industry, CORDIndustry))

#### CLEAN UP ####
rm(assMap, indMap, secMap)

#####################################################################################################################################################################

#### CREATE A PERIODICITY COLUMN
CORDQuarterly$Periodicity <- 'Q'
CORDAnnual$Periodicity <- 'A'

#### GET RID OF ANY ROWS WITH NAs
CORDQuarterly <- CORDQuarterly %>% filter(!(Price == 'NA'))
CORDAnnual <- CORDAnnual %>% filter(!(Price == 'NA'))
CORDQuarterly <- CORDQuarterly %>% filter(!(is.na(CORDSector)))
CORDAnnual <- CORDAnnual %>% filter(!(is.na(CORDSector)))
CORDQuarterly <- CORDQuarterly %>% filter(!(is.na(CORDIndustry)))
CORDAnnual <- CORDAnnual %>% filter(!(is.na(CORDIndustry)))
CORDQuarterly <- CORDQuarterly %>% filter(!(is.na(CORDAsset)))
CORDAnnual <- CORDAnnual %>% filter(!(is.na(CORDAsset)))
CORDQuarterly <- CORDQuarterly %>% filter(!(is.na(CORDSector)))
CORDAnnual <- CORDAnnual %>% filter(!(is.na(CORDSector)))

####################################################################################################################################################################

#### JUST IN CASE WE ARE STILL GETTING MULTIPLE ROWS WITH THE SAME DATA FROM THE MAPPING, GROUP EVERYTHING TOGETHER
CORDQuarterly <- sqldf("SELECT * FROM CORDQuarterly GROUP BY CORDSector, CORDIndustry, CORDAsset, Period, Measure2, Value, Price, Measure, Periodicity")

CORDAnnual <- sqldf("SELECT * FROM CORDAnnual GROUP BY CORDSector, CORDIndustry, CORDAsset, Period, Measure2, Value, Price, Measure, Periodicity")

####################################################################################################################################################################
#### DEFINE NEW MEASURES for GSB (Gross Capital Stock ?bn) & NSB (Net Capital Stock ?bn). Values already in Millions
#### QUARTERLY
bils <- CORDQuarterly %>% filter(Measure %in% c('GS','NS'))
bils$Measure <- paste0(bils$Measure, "B")
bils$Value <- bils$Value / 1000
CORDQuarterly <- rbind(CORDQuarterly, bils)
rm(bils)
##### ANNUAL
bils <- CORDAnnual %>% filter(Measure %in% c('GS','NS'))
bils$Measure <- paste0(bils$Measure, "B")
bils$Value <- bils$Value / 1000
CORDAnnual <- rbind(CORDAnnual, bils)
rm(bils)
###################################################################################################################################################################

#### DEFINE NEW MEASURES for CCP [Capital Consumtion Percentage], GSP [Gross Capital Stock (?millions) percentage], NSP [Net Capital Stock (?millions) percentage]
#### QUARTERLY
perc1 <- CORDQuarterly %>% filter(!(Measure %in% c('GSB','NSB')), Price == 'CP')
perc2 <- sqldf("SELECT Sector, Industry, Measure, Price, Period, SUM(Value) AS Total FROM perc1 GROUP BY Sector, Industry, Measure, Price, Period")
perc3 <- sqldf("SELECT a.*, b.Total FROM perc1 AS a LEFT JOIN perc2 AS b ON a.Sector = b.Sector AND a.Industry = b.Industry AND a.Price = b.Price AND a.Period = b.Period AND a.Measure = b.Measure")
perc3$Value <- round((perc3$Value / perc3$Total) * 100, digits=2)
perc3$Measure <- paste0(perc3$Measure,'P')
perc3$Total <- NULL
CORDQuarterly <- rbind(CORDQuarterly, perc3)
rm(perc1, perc2, perc3)
##### ANNUAL
perc1 <- CORDAnnual %>% filter(!(Measure %in% c('GSB','NSB')), Price == 'CP')
perc2 <- sqldf("SELECT Sector, Industry, Measure, Price, Period, SUM(Value) AS Total FROM perc1 GROUP BY Sector, Industry, Measure, Price, Period")
perc3 <- sqldf("SELECT a.*, b.Total FROM perc1 AS a LEFT JOIN perc2 AS b ON a.Sector = b.Sector AND a.Industry = b.Industry AND a.Price = b.Price AND a.Period = b.Period AND a.Measure = b.Measure")
perc3$Value <- round((perc3$Value / perc3$Total) * 100, digits=2)
perc3$Measure <- paste0(perc3$Measure,'P')
perc3$Total <- NULL
CORDAnnual <- rbind(CORDAnnual, perc3)
rm(perc1, perc2, perc3)

######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################

#### CENTRAL AND LOCAL GOVERNMENT
#### THIS SECTION SIMULATE THE GOVERNMENT CALCULATIONS IN CORD NEEDED TO DELIVER TO PUBLICATIONS
#### NAMES OF THE SHEETS ARE PULLED IN THROUGH ANOTHER SHEET CALLED CRITERIANAMES_GOV
#### THE PROGRAM THEN LOOPS AROUND THE SHEETS PULLING IN THE CRITERIA PER CORD CALCULATION AND FILTERING THE DATA
critNames <- read_excel(CORDinputs, sheet = "CRITERIANAMES_GOV", col_types = "text")
subDatQ <- array(0,dim=c(0,ncol(CORDQuarterly)))
dimnames(subDatQ) <- list(size=c(), q=c(names(CORDQuarterly)))
subDatA <- array(0,dim=c(0,ncol(CORDAnnual)))
dimnames(subDatA) <- list(size=c(), q=c(names(CORDAnnual)))
#i <- 2
for (i in 1:nrow(critNames))
{
  criteria <- read_excel(CORDinputs, sheet = critNames$CriteriaNames[i], col_types = "text")
  if (grepl("Q", criteria$AnnQrt[1]))
  {
    subDatQ <- rbind(subDatQ, CORDQuarterly %>% filter(CORDSector %in% criteria$Sector, CORDIndustry %in% criteria$Industry, CORDAsset %in% criteria$Asset, Measure %in% criteria$Measure, Price %in% criteria$Price))
  }
  if (grepl("A", criteria$AnnQrt[1]))
  {
    subDatA <- rbind(subDatA, CORDAnnual %>% filter(CORDSector %in% criteria$Sector, CORDIndustry %in% criteria$Industry, CORDAsset %in% criteria$Asset, Measure %in% criteria$Measure, Price %in% criteria$Price))
  }
  rm(criteria)
  print(paste0("Collecting data for CORD GOV calc: ", critNames$CriteriaNames[i]))
}
rm(critNames)

########################################################################################################################################################################
########################################################################################################################################################################

#### THIS SECTION SIMULATES ALL OF THE CALCULATIONS IN CORD NEEDED TO DELIVER TO PUBLICATIONS - LOCAL AND CENTRAL GOV HAVE BEEN DONE ABOVE
#### NAMES OF THE SHEETS ARE PULLED IN THROUGH ANOTHER SHEET CALLED CRITERIANAMES
#### THE PROGRAM THEN LOOPS AROUND THE SHEETS PULLING IN THE CRITERIA PER CORD CALCULATION AND FILTERING THE DATA
critNames <- read_excel(CORDinputs, sheet = "CRITERIANAMES", col_types = "text")
cordDatasetQ <- array(0,dim=c(0,ncol(CORDQuarterly)))
dimnames(cordDatasetQ) <- list(size=c(), q=c(names(CORDQuarterly)))
cordDatasetA <- array(0,dim=c(0,ncol(CORDAnnual)))
dimnames(cordDatasetA) <- list(size=c(), q=c(names(CORDAnnual)))
#i <- 6
for (i in 1:nrow(critNames))
{
  criteria <- read_excel(CORDinputs, sheet = critNames$CriteriaNames[i], col_types = "text")
  if (grepl("Q", criteria$AnnQrt[1]))
  {
    cordDatasetQ <- rbind(cordDatasetQ, CORDQuarterly %>% filter(CORDSector %in% criteria$Sector, CORDIndustry %in% criteria$Industry, CORDAsset %in% criteria$Asset, Measure %in% criteria$Measure, Price %in% criteria$Price))
    if (i == 6) {npi <- CORDQuarterly %>% filter(CORDSector %in% criteria$Sector, CORDIndustry %in% criteria$Industry, CORDAsset %in% criteria$Asset, Measure %in% criteria$Measure, Price %in% criteria$Price)}
  }
  if (grepl("A", criteria$AnnQrt[1]))
  {
    cordDatasetA <- rbind(cordDatasetA, CORDAnnual %>% filter(CORDSector %in% criteria$Sector, CORDIndustry %in% criteria$Industry, CORDAsset %in% criteria$Asset, Measure %in% criteria$Measure, Price %in% criteria$Price))
  }
  rm(criteria)
  print(paste0("Collecting data for CORD calc: ", critNames$CriteriaNames[i]))
}
CORDQuarterly <- rbind(cordDatasetQ, subDatQ)
CORDAnnual <- rbind(cordDatasetA, subDatA)

rm(critNames, subDatQ, subDatA, cordDatasetA, cordDatasetQ)

####################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################

#### SOME DATA IS NO LONGER AVAIALBLE BUT STILL NEEDS TO BE DELIVERED THROUGH CORD - THIS SECTION ADDS IT IN THROUGH AN EXCEL FILE (INDUSTRY T IS ADDED IN PREVIOUS SCRIPT)
####  THE DUMMY DATA SHOULD HAVE THE CORD SECTOR, INDUSTRY, ASSET CODES
#dummyDataPath <- paste0(inputDir, "Current Input Data/DummyData.xlsx")
#dummyDataQ <- read_excel(dummyDataPath, sheet = "Dummy", col_types = "text")
#namesToRemove <- c("Sector", "Industry", "Asset", "Measure", "Price")
#requiredVariables <- c(names(dummyDataQ) [! names(dummyDataQ) %in% namesToRemove])
#### GATHER ALL THE DATA UP AGAINST THE PERIOD
#dummyDataQ <- dummyDataQ %>% gather_(key = "Period", value = "Value", requiredVariables)
#dummyDataQ$Year <- substring(dummyDataQ$Period,1,4)
#if (!(grepl("Y", dummyDataQ$Period[1])))
#{
#  dummyDataQ$Period <- paste0("Y", dummyDataQ$Period)
#}
#dummyDataQ <- dummyDataQ %>%  filter(Period >= toChainFrom, Period <= toChainTo)
#### CREATE THE ANNUAL SUMMED DATA FROM THE QUARTERLY DATA AND ORGANISE THE COLUMNS TO MATCH THE VARIABLE CORDQuarterly AND CORDAnnual WHICH YOU WILL JOIN UP WITH AFTER
#dummyDataA <- sqldf("SELECT Sector AS CORDSector, Industry AS CORDIndustry, Asset AS CORDAsset, Sector, Industry, Asset, MAX(Year) AS Period, 
#                    Measure AS Measure2, SUM(Value) AS Value, Price, Measure, 'A' AS Periodicity 
#                    FROM dummyDataQ GROUP BY Sector, Industry, Asset, Measure, Price, Year")
#dummyDataQ <- sqldf("SELECT Sector AS CORDSector, Industry AS CORDIndustry, Asset AS CORDAsset, Sector, Industry, Asset, Period, 
#                    Measure AS Measure2, Value, Price, Measure, 'Q' AS Periodicity FROM dummyDataQ")
#dummyDataQ$Value <- as.numeric(dummyDataQ$Value)
#dummyDataA$Value <- as.numeric(dummyDataA$Value)
#CORDQuarterly <- rbind(CORDQuarterly, dummyDataQ)
#CORDAnnual <- rbind(CORDAnnual, dummyDataA)

#rm(dummyDataPath, dummyDataQ, dummyDataA, namesToRemove, requiredVariables)

###################################################################################################################################################################

#### BECAUSE THE DATA FOR EACH CALCULATION IS GATHERED SEPARATELY YOU MIGHT END UP WITH DUPLICATE DATA! GROUP TOGETHER TO GET RID...
CORDQuarterly <- sqldf("SELECT CORDSector, CORDIndustry, CORDAsset, Measure, Price, Periodicity, Period, Value FROM CORDQuarterly 
                       GROUP BY CORDSector, CORDIndustry, CORDAsset, Measure, Price, Periodicity, Period, Value")
CORDAnnual <- sqldf("SELECT CORDSector, CORDIndustry, CORDAsset, Measure, Price, Periodicity, Period, Value FROM CORDAnnual 
                    GROUP BY CORDSector, CORDIndustry, CORDAsset, Measure, Price, Periodicity, Period, Value")

############################################################################################################

#### ROUND TO DESIRED LEVEL
CORDQuarterly <- CORDQuarterly %>% mutate(Value = round(Value, 0))
CORDAnnual <- CORDAnnual %>% mutate(Value = round(Value, 0))

############################################################################################################

# GET RID OF THE Y AT THE FRONT OF THE PERIOD FOR QUARTERLY DATA
CORDQuarterly$Period <- substring(CORDQuarterly$Period,2,7)

##################################################################################################################################################
#### MAKE SURE AGAIN YOU HAVE DATA IN THDE CORRECT DATE RANGE
#CORDQuarterly <- CORDQuarterly %>% filter(Period >= toChainFrom, Period <= toChainTo)
#CORDAnnual <- CORDAnnual %>% filter(Period >= toChainFrom, Period <= toChainTo)

#### SPREAD BACK OUT BY PERIOD
CORDQuarterly <- CORDQuarterly %>% spread(Period, Value, fill = 0)
CORDAnnual <- CORDAnnual %>% spread(Period, Value, fill = 0)

############################################################################################################

#### RENAME CORD COLUMNS
names(CORDQuarterly)[1] <- "Sector"
names(CORDQuarterly)[2] <- "Industry"
names(CORDQuarterly)[3] <- "Asset"

names(CORDAnnual)[1] <- "Sector"
names(CORDAnnual)[2] <- "Industry"
names(CORDAnnual)[3] <- "Asset"

############################################################################################################

#### WRITE THE OUTPUT TO EXCEL
if (OUTPUT_SHARING==TRUE){
  write.table(CORDQuarterly, file = paste0(outputDir, "CAP_STOCKS_QUARTERLY_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
  write.table(CORDAnnual, file = paste0(outputDir, "CAP_STOCKS_ANNUAL_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
  write.table(CORDQuarterly, file = paste0(otherOutputs, "CAP_STOCKS_QUARTERLY_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
  write.table(CORDAnnual, file = paste0(otherOutputs, "CAP_STOCKS_ANNUAL_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
} else {
  write.table(CORDQuarterly, file = paste0(outputDir, "CAP_STOCKS_QUARTERLY_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
  write.table(CORDAnnual, file = paste0(outputDir, "CAP_STOCKS_ANNUAL_CORD_Import_File", "_", runTime, ".csv"), row.names = FALSE, sep=",")
}
############################################################################################################

wd <- getwd()
setwd(paste0(inputDir, "Deliveries"))
templates <- (Sys.glob("*.csv"))
setwd(wd)

for (i in templates){
  
  template <- read.csv(paste0(inputDir, "Deliveries/",i), stringsAsFactors = F)
  
  if (grepl("Q",template$Periodicity[1])==T){
    
    templateQ <- template
    templateQ$Periodicity <- "Q"
    
    # Extract each comination for all
    
    if ('All' %in% rbind(templateQ$Sector,
                         templateQ$Asset,
                         templateQ$Industry)){
      
      # Filter all and non-alls
      
      templateQ_all <- filter(templateQ, Sector=="All" |
                                Asset=="All" |
                                Industry=="All")
      templateQ <- filter(templateQ, Sector!="All" &
                            Asset!="All" &
                            Industry!="All")
      
      # Selective filter for all
      
      for (z in 1:nrow(templateQ_all)){
        
        template_row <- templateQ_all[z,]
        
        for (x in names(templateQ_all)){
          
          if ('All' %in% template_row[x]){
            
            if (x=="Industry"){
              
              CORD_extract <- filter(CORDQuarterly, 
                                     Sector==as.character(template_row["Sector"][1,1]) &
                                       Asset==as.character(template_row["Asset"][1,1]) &
                                       Measure==as.character(template_row["Measure"][1,1]) &
                                       Price==as.character(template_row["Price"][1,1]))
              CORD_extract <- select(CORD_extract, Sector, Industry, Price, Periodicity, Asset, Measure)
              
            }
            
          }
          
          templateQ <- rbind(templateQ, CORD_extract)
          
        }
        
      }
      
    }
    
    templateQ <- left_join(templateQ, CORDQuarterly)
    
    if (i=="PNFC.csv"){
      
      PNFC_map <- select(templateQ, CDID, Industry, Measure)
      
      templateQ$CDID <- NULL
      templateQ <- filter(templateQ, Sector!="")
      
      # Sum 06 and 09
      
      templateQ$Industry <- ifelse(templateQ$Industry=="B06" |
                                     templateQ$Industry=="B09",
                                   "B06+B09", templateQ$Industry)
      templateQ <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=templateQ, FUN=sum)
      
      # Create total - 06 and 09
      
      totmin0609 <- filter(templateQ, Industry=="_T" | Industry=="B06+B09")
      columns <- names(totmin0609)[8:ncol(totmin0609)]
      totmin0609$adj <- ifelse(totmin0609$Industry=="_T",1,-1)
      totmin0609$Industry <- "_T_exc_0609"
      
      for (c in columns){
        
        totmin0609[c] <- totmin0609[c]*totmin0609['adj']
        
      }
      
      totmin0609$adj <- NULL
      totmin0609 <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=totmin0609, FUN=sum)
      
      templateQ <- rbind(templateQ, totmin0609)
      
      # Add a mapping.
      
      templateQ <- left_join(templateQ, PNFC_map)
      
      
    }
    
    if (i=="PNFC.csv" | i== "SFA.csv") {
      
      #templateQ <- read.csv("J:/Shared Outputs Quarterly/2020-05-19_1036_RUN_002_R_2020Q1M3/Quarterly_spreadsheet_delivery_2020-05-19_1036_SFA.csv")
      templateQ <- select(templateQ, -Sector, -Price, -Measure, -Periodicity, -Industry, -Asset)
      templateQ <- gather(templateQ, "Period", "Value", -CDID)
      templateQ <- spread(templateQ, CDID, Value)
      templateQ$Period <- gsub("X", "", templateQ$Period)
      
      if (ANNUAL_DELIVERY=="TRUE"){
        
        names(templateQ)[1] <- "*E10IN"
        
      } else {
        
        names(templateQ)[1] <- "*ESIN"
        
      }
      write.csv(templateQ, paste0(outputDir,gsub(".csv","", i),"Quarterly_spreadsheet_delivery_",runTime,".csv"),row.names=FALSE)
      
      
    } else{
      
      write.csv(templateQ, paste0(outputDir,"Quarterly_spreadsheet_delivery_",runTime,"_",i), row.names=FALSE)
      
    }
    
  }
  
  if (grepl("A",template$Periodicity[1])==T){
    
    templateA <- template
    templateA$Periodicity <- "A"
    
    # Extract each comination for all
    
    if ('All' %in% rbind(templateA$Sector,
                         templateA$Asset,
                         templateA$Industry)){
      
      # Filter all and non-alls
      
      templateA_all <- filter(templateA, Sector=="All" |
                                Asset=="All" |
                                Industry=="All")
      templateA <- filter(templateA, Sector!="All" &
                            Asset!="All" &
                            Industry!="All")
      
      # Selective filter for all
      
      for (z in 1:nrow(templateA_all)){
        
        template_row <- templateA_all[z,]
        
        for (x in names(templateA_all)){
          
          if ('All' %in% template_row[x]){
            
            if (x=="Industry"){
              
              CORD_extract <- filter(CORDAnnual, 
                                     Sector==as.character(template_row["Sector"][1,1]) &
                                       Asset==as.character(template_row["Asset"][1,1]) &
                                       Measure==as.character(template_row["Measure"][1,1]) &
                                       Price==as.character(template_row["Price"][1,1]))
              CORD_extract <- select(CORD_extract, Sector, Industry, Price, Periodicity, Asset, Measure)
              
            }
            
          }
          
        }
        
        templateA <- rbind(templateA, CORD_extract)
        
      }
      
    }
    
    templateA <- left_join(templateA, CORDAnnual)
    
    if (i=="PNFC.csv"){
      
      PNFC_map <- select(templateA, CDID, Industry, Measure)
      
      templateA$CDID <- NULL
      templateA <- filter(templateA, Sector!="")
      
      # Sum 06 and 09
      
      templateA$Industry <- ifelse(templateA$Industry=="B06" |
                                     templateA$Industry=="B09",
                                   "B06+B09", templateA$Industry)
      templateA <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=templateA, FUN=sum)
      
      # Create total - 06 and 09
      
      totmin0609 <- filter(templateA, Industry=="_T" | Industry=="B06+B09")
      columns <- names(totmin0609)[8:ncol(totmin0609)]
      totmin0609$adj <- ifelse(totmin0609$Industry=="_T",1,-1)
      totmin0609$Industry <- "_T_exc_0609"
      
      for (c in columns){
        
        totmin0609[c] <- totmin0609[c]*totmin0609['adj']
        
      }
      
      totmin0609$adj <- NULL
      totmin0609 <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=totmin0609, FUN=sum)
      
      templateA <- rbind(templateA, totmin0609)
      
      # Add a mapping.
      
      templateA <- left_join(templateA, PNFC_map)
      
      
    }
    
    
    if (i=="PNFC.csv" | i== "SFA.csv"){
      
      #templateA <- read.csv("J:/Shared Outputs Quarterly/2020-05-19_1036_RUN_002_R_2020Q1M3/Annual_spreadsheet_delivery_2020-05-19_1036_SFA.csv")
      templateA <- select(templateA, -Sector, -Price, -Measure, -Periodicity, -Industry, -Asset)
      templateA <- gather(templateA, "Period", "Value", -CDID)
      templateA <- spread(templateA, CDID, Value)
      templateA$Period <- gsub("X", "", templateA$Period)
      
      if (ANNUAL_DELIVERY=="TRUE"){
        
        names(templateA)[1] <- "*E10IN"
        
      } else {
        
        names(templateA)[1] <- "*ESIN"
        
      }
      
      templateQnames <- as.data.frame(t(names(templateQ)))
      names(templateQnames) <- names(templateQ)
      templateA <- rbind(templateA,templateQnames,templateQ)
      
      write.csv(templateA, paste0(outputDir,gsub(".csv","", i),"_spreadsheet_delivery_",runTime,".csv"),row.names=FALSE)
      
    } else{
      
      write.csv(templateA, paste0(outputDir,"Annual_spreadsheet_delivery_",runTime,"_",i),row.names=FALSE)
      
    }
    
    
  }
  
}


# Make SFA spreadsheet suitable to upload to CORD

setwd('.')

# QA outputs

prev_files <-   list.files(paste0(last_run,"/Outputs/"))[str_detect(list.files(paste0(last_run,"/Outputs/")),"CAP_STOCKS_QUARTERLY_CORD_Import_File_")]


prev_CORDQuarterly <- paste0(last_run,"/Outputs/",prev_files)

prev_CORDQuarterly <- read.csv(prev_CORDQuarterly, stringsAsFactors=FALSE)
prev_CORDQuarterly <- filter(prev_CORDQuarterly, Measure=="CC" & Price=="CP")
prev_CORDQuarterly <- gather(prev_CORDQuarterly, Period, Value, 7:ncol(prev_CORDQuarterly))
prev_CORDQuarterly <- select(prev_CORDQuarterly, Sector, Industry, Asset, Period, Value)

#QA(CORDQuarterly, prev_CORDQuarterly, "outputCC", "additive")

rm(CORDQuarterly, prev_CORDQuarterly, templates,wd)

rm(cofogCenGoV, cofogLocGoV)
############################################################################################################
