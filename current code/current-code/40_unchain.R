############################# UNCHAIN ##########################################

# Unchain the PIM results

# -------------------------- Check dataset -------------------------------------

# Check we have an "out" dataframe
# We may want to load one from a previous run, e.g.
#######################################################################################################################

# out <- read.rds(J:/Annual round/RUN_12/Outputs/pimOutput_2021-05-23_1823.rds") #### LATEST
# out <- read_rds("output/pimOutput_2019-04-12_0756.Rds")
# out <- read_rds("output/pimOutput_2019-04-25_1136.Rds")
#######################################################################################################################

##### Remove any existing series of terminal costs and add terminal costs series ########

out <- unnest(out)
out <- filter(out, Asset!="TERMINAL")
terminal <- terminal %>% 
                 rename(ConsumptionOfFixedCapitalCP = gfcfCP)
terminal$ConsumptionOfFixedCapitalCVM <- terminal$ConsumptionOfFixedCapitalCP/terminal$PriceIndex
terminal <- select(terminal, Period, Sector, Industry, Asset, 
                   ConsumptionOfFixedCapitalCVM, ConsumptionOfFixedCapitalCP, PriceIndex)
terminal$gfcf_ociv <- terminal$ConsumptionOfFixedCapitalCP
terminal$refYear <- refPeriod
terminal$GrossStockCVM <- 0
terminal$NetStockCVM <- 0
terminal$ProductiveStockCVM <- 0
terminal$TotalChangesInVolumeCVM <- 0
terminal$TotalOtherChangesInVolumeCVM <- 0
terminal$TotalChangesInVolumeCP <- 0
terminal$TotalOtherChangesInVolumeCP <- 0
terminal$NetFixedCapitalFormationCVM <- 0
terminal$GrossStockCP <- 0
terminal$NetStockCP <- 0
terminal$ProductiveStockCP <- 0
terminal$TotalChangesInVolumeCP <- 0
terminal$NetFixedCapitalFormationCP <- 0
terminal$CapitalServicesCP <- 0
terminal$CapitalServicesCVM <- 0
terminal$Year <- substr(terminal$Period,2,5)
terminal$NominalHoldingGL <- 0
terminal$RealHoldingGL <- 0
terminal$NeutralHoldingGL <- 0
terminal$ReturnToCapital <- 0
out <- rbind(out, terminal)

# Set KP = CP for stocks in refYear

out <- out %>% group_by(Sector,Industry,Asset) %>% mutate(stockPI = (sum(PriceIndex[Period==refYear])))
out <- ungroup(out)

out$stockPI <- out$PriceIndex/out$stockPI
out$NetStockCVM <- out$NetStockCP/out$stockPI
out$GrossStockCVM <- out$GrossStockCP/out$stockPI
out$stockPI <- NULL

if (correct_CVM==TRUE){

  out <- out %>% group_by(Sector,Industry,Asset) %>% mutate(PI_Flow_adj = (sum(ConsumptionOfFixedCapitalCP[Year==substr(refPeriod,2,5)]))/
                                                              sum(ConsumptionOfFixedCapitalCVM[Year==substr(refPeriod,2,5)]))
  out$PI_Flow_adj <- ifelse(is.na(out$PI_Flow_adj),0,out$PI_Flow_adj)
  out$ConsumptionOfFixedCapitalCVM <- out$ConsumptionOfFixedCapitalCVM*out$PI_Flow_adj
  out$PI_Flow_adj <- NULL

}

out <- out %>%
        group_by(Sector, Industry, Asset) %>%
        nest(.key = "data")
  
#####################################################################

if (!exists("out")) stop("No prepared out data.frame present. Did you run the previous script?")

# --------------------------- Unchain ------------------------------------------
dataCols <- colnames(out$data[[1]])
# Create a data.frame of all the CP/CVM pairs we want to unchain
# This can be derived from colnames since they end in CP or CVM as appropriate
pairs <- data.frame(CP = sort(grep("CP$", dataCols, value = TRUE)),
                    CVM = sort(grep("CVM$", dataCols, value = TRUE)),
                    stringsAsFactors = FALSE)

pairs$flow_stock <- ifelse(grepl("Stock",pairs$CP),"Stock","Flow")

# Filter to a subset of required pairs. Filtering *rows* by CP names will retrieve
# both CP and CVM *columns*
pairs <- filter(pairs, CP %in% c("CapitalServicesCP", "ConsumptionOfFixedCapitalCP",
                                 "GrossStockCP", "NetStockCP", "ProductiveStockCP"))

refYear <- as.numeric(substr(refPeriod,2,5))

flog.info("Starting unchain.")
# Unchain using explicit deflator method
out <- unchainAll(out, pairs = pairs, refYear = refYear)
cat("\n")
flog.info("Unchain complete.")

# Check for any failures
failures <- unlist(lapply(out$unchained, FUN = function(x) inherits(x, "error")))
if (sum(failures) > 0) {
  flog.warn(paste0(sum(failures), " series failed to process:"),
                                 out[failures, ], capture = TRUE)
} else {
  flog.info("All series unchained successfully.")
}

# Remove PriceIndex, no longer required and may get in the way of later data
# aggregation (where we can't add Price Indexes together)
# This will also combine the "data" and "unchained" cols into one "data" col
out <- unnest(out) %>%
  select(-PriceIndex) %>%
  group_by(Sector, Industry, Asset) %>%
  nest(.key = "data")


# --------------------------- Reclassifications -------------------------------

# Reclassifications have to be performed after the PIM output is unchained since only then can the CVM values be added

# Unnesting the PIM outputs
pimOutput <- unnest(out)

# Reading in the reclassifications file
reclassifications <- read_csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/reclassifications.csv"), col_types = cols(.default = col_character()))

for (i in 1:nrow(reclassifications)) {
# Specifying all parameters to make easier to call for them
From_Sector <- reclassifications[i,]$From_Sector
From_Industry <- reclassifications[i,]$From_Industry
From_Asset <- reclassifications[i,]$From_Asset
To_Sector <- reclassifications[i,]$To_Sector
Re_Period <- reclassifications[i,]$Period

# Taking out the Sector/Industry/Asset combination for which the reclassification should be done
from <- pimOutput[which(pimOutput$Sector==From_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset),]

while (is.na(from$Period[1])){
  i <- i+1
  From_Sector <- reclassifications[i,]$From_Sector
  From_Industry <- reclassifications[i,]$From_Industry
  From_Asset <- reclassifications[i,]$From_Asset
  To_Sector <- reclassifications[i,]$To_Sector
  Re_Period <- reclassifications[i,]$Period

  from <- pimOutput[which(pimOutput$Sector==From_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset),]
}

# Reordering the dataset from smallest to largest period
from <- from[order(from$Period),]

if (from$Period[1]>Re_Period){
  Re_Period <- from$Period[1]
}

# Dividing the "from" dataset into 2 parts, where from_unchanged is the data up to the reclassification period
# and the from_changed is the data after the reclassification period, which will have to change
from_unchanged <- from[1:which(from$Period==Re_Period)-1,]
from_changed <- from[which(from$Period==Re_Period):nrow(from),]

# Doing the same to the "to" dataset, which is where the reclassified outputs will go to
to<-pimOutput[which(pimOutput$Sector==To_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset),]
to<-to[order(to$Period),]

if (!is.na(to$Period[1])){

if (to$Period[1]>Re_Period){
  Re_Period <- to$Period[1]
}

# Again dividing it into data that will/will not change
to_unchanged <- to[1:which(to$Period==Re_Period)-1,]
to_changed <- to[which(to$Period==Re_Period):nrow(to),]

# Merging both datasets that will change (the to_changed and from_changed)
merged <- merge(x = to_changed, y = from_changed, by = c("Industry", "Asset", "refYear", "Period", "Year"), all = TRUE)

# Creating a new data frame to which the added individual PIM outputs will go to and using the merged data frame to
# do that to ensure that the table dimensions are of the correct size for the outputs
new_to <- merged[,1:37]

# Chaning the names in the new_to, and removing the ".x" that was added when the table merging happened
colnames(new_to)<-gsub('.x$', '', colnames(new_to))

# Changing the na values to 0's since otherwise we can't add the values, where NA's exist
merged[is.na(merged)] <- 0

# Adding all relevant values to make the reclassifications happen
for (i in 1:31) {
  new_to[,i+6] <- merged[,i+6] + merged[,i+6+32]
}

# Changing the Sector
new_to$Sector <- To_Sector

# Creating a "new_from" data frame and replacing the Sector
new_from <- new_to
new_from$Sector <- From_Sector

# Changing all the values in the new_from data frame to 0, since all the values moved to the different Sector during reclassification
for(i in 7:37) {
  new_from[,i] <- 0
}

# Rearranging the output so that it would fit further tasks
new_from <- new_from[, c(6,1:4,7:27, 5, 28:37)]
new_to <- new_to[, c(6,1:4,7:27, 5, 28:37)]

# Removing the old "from" data frame from the pimOutput
pimOutput <- pimOutput[which(!(pimOutput$Sector==From_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset)),]

# Removing the old "to" data frame from the pimOutput
pimOutput <- pimOutput[which(!(pimOutput$Sector==To_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset)),]

# Adding back in the reclassified output, also including the data frame where no changes were made ("from_unchanged" and
# "to_unchanged") which would be for the PIM outputs up to the point where the reclassifications happened
pimOutput <- rbind(pimOutput,from_unchanged, new_from)
pimOutput <- rbind(pimOutput,to_unchanged, new_to)
} else {
  new_to <- from_changed
  new_to$Sector <- To_Sector

  new_from <- from_changed
  for(i in 7:37) {
    new_from[,i] <- 0
  }

  # Rearranging the output so that it would fit further tasks
  new_from <- new_from[, c(6,1:4,7:27, 5, 28:37)]
  new_to <- new_to[, c(6,1:4,7:27, 5, 28:37)]

  # Removing the old "from" data frame from the pimOutput
  pimOutput <- pimOutput[which(!(pimOutput$Sector==From_Sector & pimOutput$Industry==From_Industry & pimOutput$Asset==From_Asset)),]

  # Adding back in the reclassified output, also including the data frame where no changes were made ("from_unchanged" and
  # "to_unchanged") which would be for the PIM outputs up to the point where the reclassifications happened
  pimOutput <- rbind(pimOutput,from_unchanged, new_from)
  pimOutput <- rbind(pimOutput, new_to)

}
}



write_rds(out, paste0(outputDir, "pimOutput_reclass_", runTime, ".Rds"))

# Produce series from 1948 to 1994

historic <- select(pimOutput, Sector, Period, ConsumptionOfFixedCapitalCP)
historic[is.na(historic)] <- 0
historic$Sector <- ifelse(substr(historic$Sector,1,4)=="S.12", "S.12", historic$Sector)
#historic$ConsumptionOfFixedCapitalCP[historic$Sector=='S.1311' | historic$Sector=='S.1313' | historic$Sector=='S.15'] <- round(historic$ConsumptionOfFixedCapitalCP[historic$Sector=='S.1311' | historic$Sector=='S.1313' | historic$Sector=='S.15'] ,2)
historic$ConsumptionOfFixedCapitalCP <- ifelse(historic$Sector=='S.1311' | historic$Sector=='S.1313' | historic$Sector=='S.15',
                                               round(historic$ConsumptionOfFixedCapitalCP, 2), historic$ConsumptionOfFixedCapitalCP)   


historic <- aggregate(. ~ Sector+Period, data=historic, FUN=sum)
historic <- filter(historic, substr(Period,2,5)>1947)
historic <- filter(historic, substr(Period,2,5)<1995)
historic$ConsumptionOfFixedCapitalCP <- round(historic$ConsumptionOfFixedCapitalCP,0)

historic_agg <- filter(historic, Sector!="S.12")
historic_agg$Sector <- ifelse(substr(historic_agg$Sector,1,4)=="S.11", "S.11", 
                          ifelse(substr(historic_agg$Sector,1,4)=="S.13", "S.13", "S.1M"))
historic_agg <- aggregate(. ~ Sector+Period, data=historic_agg, FUN=sum)

historic_total <- historic
historic_total$Sector <- 'ST'
                             
historic_total <- aggregate(. ~ Sector+Period, data=historic_total, FUN=sum)

historic <- rbind(historic, historic_agg, historic_total)

#read in template
template <- read.csv(paste0(inputDir,"Deliveries/SFA.csv"), stringsAsFactors = FALSE)

#drop anything not cp or n11
template <- filter(template, Price == 'CP'& Asset == 'N11')

#filter price = cp asset = n11
template <- select(template, CDID , Sector)
#select - keep sector CDID
historic$Sector <- str_replace(historic$Sector, '[.]', '')
historic$Sector[historic$Sector== 'S11PR'] <- 'S1100P'
both <- left_join(historic, template)

#left join historic to template

#drop sector
both <- select(both, -Sector)
both$Period <- gsub("Y", "", both$Period)


#CSDB format
quarterlyhistoric <- spread(both, CDID, ConsumptionOfFixedCapitalCP)
names(quarterlyhistoric)[1] <- "*E10IN"

both$Period <- substr(both$Period, 1, 4) 
both <- aggregate(. ~ Period + CDID, data=both, FUN=sum)
annualhistoric <- spread(both, CDID, ConsumptionOfFixedCapitalCP)
names(annualhistoric)[1] <- "*E10IN"

Qnames <- as.data.frame(t(names(quarterlyhistoric)))
names(Qnames) <- names(quarterlyhistoric)
final <- rbind(annualhistoric,Qnames, quarterlyhistoric)


write.csv(historic, paste0(outputDir, "RUN21historic", runTime, ".csv"), row.names = FALSE)



historic <- spread(historic,Period,ConsumptionOfFixedCapitalCP)
write.csv(historic, paste0(outputDir, "historic", runTime, ".csv"), row.names = FALSE)

rm(historic, historic_agg)

# Replacing the current "out" data frame since it is used in further scripts and the actual change of the reclassification
# will only be visible after the data is chained again
out <- pimOutput %>%
  group_by(Sector, Industry, Asset) %>%
  nest(.key = "data")

# Write pimoutputs reclassified

write_rds(out, paste0(outputDir, "pimOutput_reclass_", runTime, ".Rds"))

# --------------------------- Write to CSV -------------------------------------

unchainOutput <- pimOutput

# Define base variables for output
baseVariables <- c("Sector", "Industry", "Asset", "Period")
# Define additional variables for output
# We will select all variables ending with CYP, PYP, CVM, or CP
requiredVariables <- grep("(CYP$|PYP$|CVM$|CP$)", colnames(unchainOutput), value = TRUE)
# Further filter the variables to the main stocks (we can open this out in future)
requiredVariables <- grep("GrossStock|ProductiveStock|NetStock|ConsumptionOfFixedCapital", requiredVariables, value = TRUE)
requiredVariables <- c(requiredVariables, "gfcf_ociv")
# Select the variables
unchainOutput <- unchainOutput %>% select(one_of(c(baseVariables, requiredVariables)))

# Gather up the variables under a new "Measure" column
unchainOutput <- unchainOutput %>% gather_(key = "Measure", value = "Value", requiredVariables)
# Round
unchainOutput <- unchainOutput %>% mutate(Value = round(Value, 2))

# Spread the Period across the columns
unchainOutput <- unchainOutput %>% spread(Period, Value, fill = 0)

# Write to a CSV
#if (WRITE_FILES) write_excel_csv(unchainOutput, path = paste0(outputDir, "unchain_outputs", runTime, ".csv"))

# Create KP table

if (publishTables==TRUE){
 
  Asset_hier <- read_excel(paste0(inputDir,"Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"), sheet = "Asset", col_types = "text")
    
  KP_table <- function(df, name){
    
    df <- rename(df, Asset5 = Asset)
    df <- left_join(df,Asset_hier)
    keep <- c('Asset2', names(df)[match('Y1995Q1', names(df)):match(toChainTo, names(df))])
    df <- df[keep]
    df <- rename(df, Asset = Asset2)
    df$Asset <- ifelse(df$Asset=="OTHER.BUILDINGS.STRUCTURES", "Other buildings and structures",
                       ifelse(df$Asset=="DWELLINGS", "Dwellings",
                              ifelse(df$Asset=="MACH.EQ", "Machinery, equipment and weapons systems",
                                     ifelse(df$Asset=="IPP", "Intellectual property products", "Other"))))
    df <- aggregate(. ~ Asset, data=df, FUN=sum)
    
    df <- gather(df, Period, calc, 2:ncol(df))
    df$calc <- round(df$calc, 0)
    df <- spread(df, Period, calc)
    
    # Add total
    
    Total <- df
    Total$Asset <- "Total"
    Total <- aggregate(. ~ Asset, data=Total, FUN=sum)
    
    df <- rbind(df, Total)
    
    write.csv(df, paste0(outputDir, "Pub_tables/", name,".csv"), row.names = F)
    
  }
  
  # GFCF KP 
  
  GFCF_KP <- filter(unchainOutput, Measure=="gfcf_ociv")
  KP_table(GFCF_KP, "GFCF_KP")
  
  # CC KP
  
  CC_KP <- filter(unchainOutput, Measure=="ConsumptionOfFixedCapitalCVM")
  KP_table(CC_KP, "CC_KP")
  
  # NS KP
  
  NS_KP <- filter(unchainOutput, Measure=="NetStockCVM")
  KP_table(NS_KP, "NS_KP")
  
  
  # NS CP
  
  NS_CP <- filter(unchainOutput, Measure=="NetStockCP")
  KP_table(NS_CP, "NS_CP")
  
  
  # GS KP
  
  GS_KP <- filter(unchainOutput, Measure=="GrossStockCVM")
  KP_table(GS_KP, "GS_KP")
  
  
  # GS CP
  
  GS_CP <- filter(unchainOutput, Measure=="GrossStockCP")
  KP_table(GS_CP, "GS_CP") 
  
}

# -------------------------- Remove Objects ------------------------------------
rm(dataCols, pairs, failures, baseVariables, requiredVariables)
rm(from, from_changed, from_unchanged, From_Asset, From_Industry, From_Sector, to, to_changed, to_unchanged,
   To_Sector, merged, new_from, new_to, reclassifications, Re_Period, i, refYear)

rm(unchainOutput)
rm(pimOutput)
