####################### Map S.13 COFOG to A64 ##################################

# Maps the S.13 series (S.1311 and S.1313) from their detailed COFOG Industry
# codes to the standard A64 Industry codes, aligning them with the rest of the
# non-S.13 series.
#out <- out %>% nest()
#d <- sqldf("select * from out where Sector = 'S.1313' AND Asset = 'SOFT.DATA(P)'")
# out <- read_rds("output/pimOutput_2018-10-23_1327.Rds")
# -------------------------- Check dataset -------------------------------------

# Check we have an "out" dataframe
# We may want to load one from a previous run, e.g.
# out <- read_rds("pimOutput_<runtime>.Rds")
if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")

flog.info("Aggregating S.13 COFOG industries to A64.")

# Check that dataset contains S.13 series (otherwise this script will do nothing)

if (all(grepl("^S.13", out$Sector) == FALSE)) {
  
  flog.info("No S.13 sectors present in data.")
  
} else {

  # -------------------------- Separate S.13 Sectors ---------------------------
  
  # Separate out S.13 data and split
  
  indSplit <- read.csv(paste0(inputDir, "Splits/split_COFOG.csv"), stringsAsFactors=FALSE) # PULL IN THE SPLITS FROM THE SPREADSHEET
  toSplit <- unique(paste0(indSplit$Sector,indSplit$Industry))
  indSplit <- indSplit %>% gather(key = 'Period', value = "Perc", 4:ncol(indSplit))
  
  outS1311 <- filter(out, Sector == "S.1311")
  outS1313 <- filter(out, Sector == "S.1313")
  
  # Remove it from main data
  out <- anti_join(out, bind_rows(outS1311, outS1313), by = c("Sector", "Industry", "Asset"))
  
  # Warn if there are any remaining S.13 in main dataset
  if (any(grepl("^S.13", out$Sector))) {
    flog.warn("Only S.1311 and S.1313 series will be processed. There are other S.13 sectors present in the data which will be left unchanged.")
  }
  
  #s13adj <- read.csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/s13_consistency_adjustment.csv"))
  
  outS1311 <- unnest(outS1311)
  outS1311 <- filter(outS1311, !is.na(Industry))
  #outS1311 <- left_join(outS1311, s13adj)
  #outS1311$adj[is.na(outS1311$adj)] <- 0
  #outS1311$ConsumptionOfFixedCapitalCP <- outS1311$ConsumptionOfFixedCapitalCP + outS1311$adj
  #outS1311$adj <- NULL
  
  #### TAKE CARE OF ANY NEGATIVE VALUES BY MAKING THEM ZERO
  outS1311$ConsumptionOfFixedCapitalCP [ outS1311$ConsumptionOfFixedCapitalCP<0 ] <- 0
  outS1311$ConsumptionOfFixedCapitalCVM [ outS1311$ConsumptionOfFixedCapitalCVM<0 ] <- 0
  outS1311$NetStockCVM [ outS1311$NetStockCVM<0 ] <- 0
  outS1311$NetStockCP [ outS1311$NetStockCP<0 ] <- 0
  outS1311$GrossStockCVM [ outS1311$GrossStockCVM<0 ] <- 0
  outS1311$GrossStockCP [ outS1311$GrossStockCP<0 ] <- 0
    
  #### ROUND CP VALUES TO ZERO DECIMAL PLACES FOR CENTRAL GOVERNEMNT DATA
  outS1311$GrossStockCVM <- round(outS1311$GrossStockCVM,0)
  outS1311$GrossStockCP <- round(outS1311$GrossStockCP,0)
  outS1311$NetStockCP <- round(outS1311$NetStockCP,0)
  outS1311$NetStockCVM <- round(outS1311$NetStockCVM,0)
  outS1311$ConsumptionOfFixedCapitalCP <- round(outS1311$ConsumptionOfFixedCapitalCP,0)
  outS1311$ConsumptionOfFixedCapitalCVM <- round(outS1311$ConsumptionOfFixedCapitalCVM,0)
  
  # Round CYPs/PYPs - ideally this should be done later
  
  if (correct_CVM==TRUE){
    
    outS1311$GrossStockPYP <- round(outS1311$GrossStockPYP,0)
    outS1311$GrossStockCYP <- round(outS1311$GrossStockCYP,0)
    outS1311$NetStockCYP <- round(outS1311$NetStockCYP,0)
    outS1311$NetStockPYP <- round(outS1311$NetStockPYP,0)
    outS1311$ConsumptionOfFixedCapitalCYP <- round(outS1311$ConsumptionOfFixedCapitalCYP,0)
    outS1311$ConsumptionOfFixedCapitalPYP <- round(outS1311$ConsumptionOfFixedCapitalPYP,0)
    
  }
  
  notSplit <- filter(outS1311, !paste0(outS1311$Sector,outS1311$Industry) %in% toSplit)
  split <- filter(outS1311, paste0(outS1311$Sector,outS1311$Industry) %in% toSplit)
  split <- left_join(split,indSplit)
  
  splitNew <- split
  splitNew$Industry <- splitNew$toIndustry
  
  applySplit <- c( "gfcf_ociv",                    "GrossStockCVM",                "NetStockCVM",                 
                   "ProductiveStockCVM",           "TotalChangesInVolumeCVM",      "TotalOtherChangesInVolumeCVM",
                   "TotalOtherChangesInVolumeCP",  "ConsumptionOfFixedCapitalCVM", "NetFixedCapitalFormationCVM", 
                   "GrossStockCP",                 "NetStockCP",                   "ProductiveStockCP",           
                   "TotalChangesInVolumeCP",       "ConsumptionOfFixedCapitalCP",  "NetFixedCapitalFormationCP",  
                   "NominalHoldingGL",             "RealHoldingGL",                "NeutralHoldingGL",            
                   "CapitalServicesCYP",           "CapitalServicesPYP",           "ConsumptionOfFixedCapitalCYP",
                   "ConsumptionOfFixedCapitalPYP", "GrossStockCYP",                "GrossStockPYP",               
                   "NetStockCYP",                  "NetStockPYP",                  "ProductiveStockCYP",          
                   "ProductiveStockPYP" )
  
  for (i in applySplit){
    splitNew[i] <- split[i]*split$Perc
    split[i] <- split[i]*(1-split$Perc)
  }
  splitNew[ ,c('toIndustry', 'Perc')] <- list(NULL)
  split[ ,c('toIndustry', 'Perc')] <- list(NULL)
  outS1311 <- rbind(notSplit,split,splitNew)
  rm(notSplit, split, splitNew,i)
  
  outS1313 <- unnest(outS1313)
  outS1311 <- filter(outS1311, !is.na(Industry))
  #outS1313 <- left_join(outS1313, s13adj)
  #outS1313$adj[is.na(outS1313$adj)] <- 0
  #outS1313$ConsumptionOfFixedCapitalCP <- outS1313$ConsumptionOfFixedCapitalCP + outS1313$adj
  #outS1313$adj <- NULL
  
  #### TAKE CARE OF ANY NEGATIVE VALUES BY MAKING THEM ZERO
  outS1313$ConsumptionOfFixedCapitalCP [ outS1313$ConsumptionOfFixedCapitalCP<0 ] <- 0
  outS1313$ConsumptionOfFixedCapitalCVM [ outS1313$ConsumptionOfFixedCapitalCVM<0 ] <- 0
  outS1313$NetStockCVM [ outS1313$NetStockCVM<0 ] <- 0
  outS1313$NetStockCP [ outS1313$NetStockCP<0 ] <- 0
  outS1313$GrossStockCVM [ outS1313$GrossStockCVM<0 ] <- 0
  outS1313$GrossStockCP [ outS1313$GrossStockCP<0 ] <- 0
  #outS1313$ConsumptionOfFixedCapitalCP <- round(outS1313$ConsumptionOfFixedCapitalCP,0)
  outS1313$ConsumptionOfFixedCapitalCP <- ifelse(is.na(outS1313$ConsumptionOfFixedCapitalCP),0,
                                                 outS1313$ConsumptionOfFixedCapitalCP)
  #outS1313$ConsumptionOfFixedCapitalCVM <- round(outS1313$ConsumptionOfFixedCapitalCVM,0)
  #### ROUND CP VALUES TO ZERO DECIMAL PLACES FOR LOCAL GOVERNEMNT DATA
  outS1313$ConsumptionOfFixedCapitalCP <- round(outS1313$ConsumptionOfFixedCapitalCP,0)
  outS1313$ConsumptionOfFixedCapitalCVM <- round(outS1313$ConsumptionOfFixedCapitalCVM,0)
  outS1313$GrossStockCVM <- round(outS1313$GrossStockCVM,0)
  outS1313$GrossStockCP <- round(outS1313$GrossStockCP,0)
  outS1313$NetStockCP <- round(outS1313$NetStockCP,0)
  outS1313$NetStockCVM <- round(outS1313$NetStockCVM,0)
  
  if (correct_CVM==TRUE){
    
    outS1313$GrossStockPYP <- round(outS1313$GrossStockPYP,0)
    outS1313$GrossStockCYP <- round(outS1313$GrossStockCYP,0)
    outS1313$NetStockCYP <- round(outS1313$NetStockCYP,0)
    outS1313$NetStockPYP <- round(outS1313$NetStockPYP,0)
    outS1313$ConsumptionOfFixedCapitalCYP <- round(outS1313$ConsumptionOfFixedCapitalCYP,0)
    outS1313$ConsumptionOfFixedCapitalPYP <- round(outS1313$ConsumptionOfFixedCapitalPYP,0)
    
  }
  
  notSplit <- filter(outS1313, !paste0(outS1313$Sector,outS1313$Industry) %in% toSplit)
  split <- filter(outS1313, paste0(outS1313$Sector,outS1313$Industry) %in% toSplit)
  split <- left_join(split,indSplit)
  
  splitNew <- split
  splitNew$Industry <- splitNew$toIndustry
  
  for (i in applySplit){
    splitNew[i] <- split[i]*split$Perc
    split[i] <- split[i]*(1-split$Perc)
  }
  splitNew[ ,c('toIndustry', 'Perc')] <- list(NULL)
  split[ ,c('toIndustry', 'Perc')] <- list(NULL)
  outS1313 <- rbind(notSplit,split,splitNew)
  
  rm(indSplit,notSplit, split, splitNew, toSplit,applySplit,i)
  
  # ----------------------- Aggregate S.1311 COFOG to A64 ----------------------
  
  if (nrow(outS1311) > 0) {
    flog.info("Aggregating COFOG to A64 for S.1311.")
    flog.info(paste("S.1311 Series count:", nrow(getSeries(outS1311))))
    # Read Industry Hierarchy
    sic <- read_excel(paste0(inputDir, "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"),
                      sheet = "COFOG_A64_S1311", col_types = "text")
    # Format A64 industry codes (e.g. "1" => "01") in case Excel has dropped leading zeros
    sic <- mutate(sic, A64 = prepim::formatIndustryCodes(A64))
  
    #### TAKE CARE OF ANY NEGATIVE VALUES BY MAKING THEM ZERO
    outS1311$ConsumptionOfFixedCapitalCP [ outS1311$ConsumptionOfFixedCapitalCP<0 ] <- 0
    outS1311$ConsumptionOfFixedCapitalCVM [ outS1311$ConsumptionOfFixedCapitalCVM<0 ] <- 0
    outS1311$NetStockCVM [ outS1311$NetStockCVM<0 ] <- 0
    outS1311$NetStockCP [ outS1311$NetStockCP<0 ] <- 0
    outS1311$GrossStockCVM [ outS1311$GrossStockCVM<0 ] <- 0
    outS1311$GrossStockCP [ outS1311$GrossStockCP<0 ] <- 0
    #outS1311$ConsumptionOfFixedCapitalCP <- round(outS1311$ConsumptionOfFixedCapitalCP,0)
    #outS1311$ConsumptionOfFixedCapitalCVM <- round(outS1311$ConsumptionOfFixedCapitalCVM,0)
    #### ROUND CP VALUES TO ZERO DECIMAL PLACES FOR CENTRAL GOVERNEMNT DATA
    outS1311$GrossStockCVM <- round(outS1311$GrossStockCVM,0)
    outS1311$GrossStockCP <- round(outS1311$GrossStockCP,0)
    outS1311$NetStockCP <- round(outS1311$NetStockCP,0)
    outS1311$NetStockCVM <- round(outS1311$NetStockCVM,0)
    
    cofogCenGoV <- outS1311
    
    # Define vars to aggregate, all except the series and period identifiers
    toAggregate <- setdiff(colnames(outS1311),
                           c("Sector", "Industry", "Asset", "refYear", "Period", "Year"))
    outS1311 <- aggregateWithHierarchyTable(.data = outS1311,
                                            column = "Industry",
                                            hierarchyTable = sic, targetLevel = "A64",
                                            values = toAggregate,
                                            keepOriginal = FALSE)
    rm(sic)
    flog.info(paste("S.1311 Series count:", nrow(getSeries(outS1311))))
    
    # Combine with main series
    out <- outS1311 %>%
      group_by(Sector, Industry, Asset) %>%
      nest() %>% 
      bind_rows(out)
    
  }
  
  
  # ----------------------- Aggregate S.1313 COFOG to A64 ----------------------
  
  if (nrow(outS1313) > 0) {
    flog.info("Aggregating COFOG to A64 for S.1313.")
    flog.info(paste("S.1313 Series count:", nrow(getSeries(outS1313))))
    # Read Industry Hierarchy
    sic <- read_excel(paste0(inputDir, "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"),
                      sheet = "COFOG_A64_S1313", col_types = "text")
    # Format A64 industry codes (e.g. "1" => "01") in case Excel has dropped leading zeros
    sic <- mutate(sic, A64 = prepim::formatIndustryCodes(A64))

    cofogLocGoV <- outS1313
    
    #### ALSO ROUND TO ZERO DECIMAL PLACES FOR LOCAL GOVERNEMNT DATA
    cofogLocGoV$GrossStockCP <- round(cofogLocGoV$GrossStockCP,0)
    cofogLocGoV$NetStockCP <- round(cofogLocGoV$NetStockCP,0)
    cofogLocGoV$ConsumptionOfFixedCapitalCP <- round(cofogLocGoV$ConsumptionOfFixedCapitalCP,0)
    
    # Define vars to aggregate, all except the series and period identifiers
    toAggregate <- setdiff(colnames(outS1313),
                           c("Sector", "Industry", "Asset", "refYear", "Period", "Year"))
    outS1313 <- aggregateWithHierarchyTable(.data = outS1313,
                                            column = "Industry",
                                            hierarchyTable = sic, targetLevel = "A64",
                                            values = toAggregate,
                                            keepOriginal = FALSE)
    rm(sic)
    flog.info(paste("S.1313 Series count:", nrow(getSeries(outS1313))))
    
    # Combine with main series
    out <- outS1313 %>%
      group_by(Sector, Industry, Asset) %>%
      nest() %>% 
      bind_rows(out)
  }
 
  # --------------------------- Remove objects ---------------------------------
  #rm(outS1311, outS1313) 
  flog.info("Aggregation of S.13 COFOG industries to A64 complete.")
  
}

#a <- select(outS1313, Industry, Year, ConsumptionOfFixedCapitalCP)
#a$ConsumptionOfFixedCapitalCP <- round(a$ConsumptionOfFixedCapitalCP, 0)
#a <- filter(a, Year>=1997)
#a <- aggregate(ConsumptionOfFixedCapitalCP ~ Year+Industry, data=a, FUN=sum)
#a$ConsumptionOfFixedCapitalCP <- round(a$ConsumptionOfFixedCapitalCP,0)
#a <- spread(a, Year, ConsumptionOfFixedCapitalCP)
#write.csv(a, "S1313_ind.csv")

#######################################################################################################################################################################
#### REJOIN THE COFOG SPLITS CARRIED OUT BEFORE THE PIM RUN

#### CENTRAL GOVERNMENT
source("miscCapStocksFunctions.R")
colSeq <- names(cofogCenGoV)
cofogCenGoV$Industry[left(cofogCenGoV$Industry, 2) == 'GF' & right(cofogCenGoV$Industry, 1) == 'b'] <- 
  mid(cofogCenGoV$Industry[left(cofogCenGoV$Industry, 2) == 'GF' & right(cofogCenGoV$Industry, 1) == 'b'], 1, 
      nchar(cofogCenGoV$Industry[left(cofogCenGoV$Industry, 2) == 'GF' & right(cofogCenGoV$Industry, 1) == 'b']) - 1)
cofogCenGoV$Year <- NULL
varsToGatherOn <- names(cofogCenGoV[6:ncol(cofogCenGoV)])  

cofogCenGoV <- cofogCenGoV %>% gather_(key = "Measure", value = "Value", varsToGatherOn)

cofogCenGoV <- sqldf(" SELECT Sector, Industry, Asset, refYear, Period, Measure, SUM(Value) AS Value FROM cofogCenGoV GROUP BY Sector, Industry, Asset, refYear, Period, Measure")
names(cofogCenGoV)[names(cofogCenGoV) == "sum()"] <- "Value"
cofogCenGoV <- cofogCenGoV %>% spread(Measure, Value, fill = 0)
cofogCenGoV$Year <- substr(cofogCenGoV$Period,2,5)
cofogCenGoV <- cofogCenGoV[colSeq]

#######################################################################################################################################################################
#### LOCAL GOVERNMENT
colSeq <- names(cofogLocGoV)
cofogLocGoV$Industry[left(cofogLocGoV$Industry, 2) == 'GF' & right(cofogLocGoV$Industry, 1) == 'b'] <- 
  mid(cofogLocGoV$Industry[left(cofogLocGoV$Industry, 2) == 'GF' & right(cofogLocGoV$Industry, 1) == 'b'], 1, 
      nchar(cofogLocGoV$Industry[left(cofogLocGoV$Industry, 2) == 'GF' & right(cofogLocGoV$Industry, 1) == 'b']) - 1)
cofogLocGoV$Year <- NULL
varsToGatherOn <- names(cofogLocGoV[6:ncol(cofogLocGoV)])  

cofogLocGoV <- cofogLocGoV %>% gather_(key = "Measure", value = "Value", varsToGatherOn)

cofogLocGoV <- sqldf(" SELECT Sector, Industry, Asset, refYear, Period, Measure, SUM(Value) AS Value FROM cofogLocGoV GROUP BY Sector, Industry, Asset, refYear, Period, Measure")
names(cofogLocGoV)[names(cofogLocGoV) == "sum()"] <- "Value"
cofogLocGoV <- cofogLocGoV %>% spread(Measure, Value, fill = 0)
cofogLocGoV$Year <- substr(cofogLocGoV$Period,2,5)
cofogLocGoV <- cofogLocGoV[colSeq]
#######################################################################################################################################################################