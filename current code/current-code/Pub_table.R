library(openxlsx)
#library(tidyr)
#library(dplyr)

# Pick up pub year from run

pub_year <- ifelse(substr(publishTo,7,7)=="4",as.numeric(substr(publishTo,2,5)),as.numeric(substr(publishTo,2,5))-1)

# Functions

remove_ext_yr <- function (df, not_year, end_year) {
  
  # Remove surplus years
  
  year <- names(df)
  year <- year[!(year %in% not_year)]
  year <- as.numeric(substr(year, 2,5))
  year <- year[year<=end_year]
  year <- paste0("X", year)
  year <- c(not_year, year)
  df <- select(df, one_of(year))
  return(df)
  
}


remove_prev_yr <- function (df, not_year, start_year) {
  
  # Remove surplus years
  
  year <- names(df)
  year <- year[!(year %in% not_year)]
  year <- as.numeric(substr(year, 2,5))
  year <- year[year>=start_year]
  year <- paste0("X", year)
  year <- c(not_year, year)
  df <- select(df, one_of(year))
  return(df)
  
}

remove_ext_qtr <- function(df, end_year){
  
  #Remove surplus quarters
  df <- gather(df, Period, Value, 2:ncol(df))
  df <- df %>% filter(Period <= paste0("Y",end_year,"Q4"))
  df <- spread(df, Period, Value)
}

### Breakdown NS/GS

# Pick up template to write to

Breakdown <- loadWorkbook(paste0(inputDir, "Deliveries/Breakdown of changes to gross and net capital stock.xlsx"))

# Pick up KPs and CPs from run

NS_KP <- read.csv(paste0(outputDir, "Pub_tables/NS_KP.csv"))
NS_CP <- read.csv(paste0(outputDir, "Pub_tables/NS_CP.csv"))
GS_KP <- read.csv(paste0(outputDir, "Pub_tables/GS_KP.csv"))
GS_CP <- read.csv(paste0(outputDir, "Pub_tables/GS_CP.csv"))
GFCF_KP <- read.csv(paste0(outputDir, "Pub_tables/GFCF_KP.csv"))
CC_KP <- read.csv(paste0(outputDir, "Pub_tables/CC_KP.csv"))

# Create retirement

Ret <- filter(GS_KP, Asset!="TOTAL")
Ret <- gather(Ret, Period, GS, 2:ncol(Ret))
Ret <- Ret %>% filter(Period <= paste0("Y",pub_year,"Q4"))
Ret$Period <- as.numeric(paste0(substr(Ret$Period,2,5),substr(Ret$Period,7,7)))
Ret <- Ret %>% group_by(Asset) %>% mutate(GS = GS-lag(GS)) %>% ungroup()

Sub <- filter(GFCF_KP, Asset!="TOTAL")
Sub <- gather(Sub, Period, GS, 2:ncol(Sub))
Sub <- Sub %>% filter(Period <= paste0("Y",pub_year,"Q4"))
Sub$Period <- as.numeric(paste0(substr(Sub$Period,2,5),substr(Sub$Period,7,7)))
Sub$GS <- Sub$GS*-1

Ret <- rbind(Ret,Sub)
Ret <- aggregate(. ~ Asset+Period, data=Ret, FUN=sum)
Ret$GS <- Ret$GS*-1
Ret$Period <- paste0("Y",substr(Ret$Period,1,4),"Q",substr(Ret$Period,5,5))

Ret <- spread(Ret, Period, GS)

# Add total

Total <- Ret
Total$Asset <- "Total"
Total <- aggregate(. ~ Asset, data=Total, FUN=sum)

Ret <- rbind(Ret, Total)

Ret$Y1995Q1 <- "-"

# Constrain years in tables to publication year

GFCF_KP <- remove_ext_qtr(GFCF_KP,pub_year)
CC_KP <- remove_ext_qtr(CC_KP,pub_year)
NS_KP <- remove_ext_qtr(NS_KP,pub_year)
GS_KP <- remove_ext_qtr(GS_KP,pub_year)
NS_CP <- remove_ext_qtr(NS_CP,pub_year)
GS_CP <- remove_ext_qtr(GS_CP,pub_year)

# Add new worksheets

writeData(Breakdown, sheet='GFCF KP', x = GFCF_KP, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='CC KP', x = CC_KP, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='Net stock KP', x = NS_KP, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='Retirement of assets KP', x = Ret, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='Gross stock KP', x = GS_KP, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='Net stock CP', x = NS_CP, startCol = 1, startRow = 2)
writeData(Breakdown, sheet='Gross stock CP', x = GS_CP, startCol = 1, startRow = 2)

# Write out breakdown file

saveWorkbook(Breakdown, paste0(outputDir, "Pub_tables/Breakdown of changes to gross and net capital stock.xlsx"), overwrite=T)

# CC by ind table

# Read in template to write to

CC_template <- loadWorkbook(paste0(inputDir, "Deliveries/fixedcapitalconsumptionbya64industry.xlsx"))

# Format

Format <- read.xlsx(paste0(xlsxFile = paste0(inputDir, "Deliveries/fixedcapitalconsumptionbya64industry.xlsx")),
                    sheet = 2, startRow = 5)
Format <- select(Format, Industry.code)
Format <- rename(Format, Industry = Industry.code)

# Read in annual CORD import file

data <- read.csv(paste0(outputDir, "CAP_STOCKS_ANNUAL_CORD_Import_File_", substr(outputDir,34,48),".csv"))

CC <- filter(data, Sector=="ST" & Asset=="N11" & Measure=="CC")
CC <- select(CC, -Sector, -Asset, -Measure, -Periodicity)

selected_columns <- names(CC)[3:ncol(CC)]
selected_columns <- as.numeric(substr(selected_columns, 2,5))
selected_columns <- selected_columns[selected_columns<=pub_year]
selected_columns <- paste0("X", selected_columns)
selected_columns <- c("Industry", selected_columns)

CC_CP <- filter(CC, Price=="CP")
CC_CP <- select(CC_CP, -Price)
CC_CP <- left_join(Format,CC_CP)
CC_CP <- CC_CP[selected_columns]

CC_CVM <- filter(CC, Price=="CVM")
CC_CVM <- select(CC_CVM, -Price)
CC_CVM <- left_join(Format,CC_CVM)
CC_CVM <- CC_CVM[selected_columns]

CC_PYP <- filter(CC, Price=="PYP")
CC_PYP <- select(CC_PYP, -Price)
CC_PYP <- left_join(Format,CC_PYP)
CC_PYP <- CC_PYP[selected_columns]

# Add new worksheets

writeData(CC_template, sheet='Current prices', x = CC_CP, startCol = 2, startRow = 6, colNames= F)
writeData(CC_template, sheet='Chained volume measures', x = CC_CVM, startCol = 2, startRow = 6, colNames= F)
writeData(CC_template, sheet="Previous years prices", x = CC_PYP, startCol = 2, startRow = 6, colNames= F)

saveWorkbook(CC_template, paste0(outputDir, "Pub_tables/Capital consumption.xlsx"), overwrite=T)

# Stock by ind table

# Format

S_template <- loadWorkbook(paste0(inputDir, "Deliveries/grossnet.xlsx"))

Format <- read.xlsx(paste0(xlsxFile = paste0(inputDir, "Deliveries/grossnet.xlsx")),
                    sheet = 2, startRow = 5)
Format <- select(Format, Asset.code, Industry.code)
Format <- rename(Format, Asset = Asset.code ,Industry = Industry.code)

# Series

Stock <- filter(data, Sector=="ST" & (Measure=="GS" | Measure=="NS"))
Stock$Asset <- paste0(Stock$Asset, substr(Stock$Measure,1,1))
Stock <- select(Stock, -Sector, -Measure, -Periodicity)

Stock_CP <- filter(Stock, Price=="CP")
Stock_CP <- select(Stock_CP, -Price)
Stock_CP <- left_join(Format,Stock_CP)

Stock_PYP <- filter(Stock, Price=="PYP")
Stock_PYP <- select(Stock_PYP, -Price)
Stock_PYP <- left_join(Format,Stock_PYP)

# Add in z's

Stock_CP <- Stock_CP %>% mutate_all(as.character)
Stock_CP[is.na(Stock_CP)] <- "z"
Stock_PYP <- Stock_PYP %>% mutate_all(as.character)
Stock_PYP[is.na(Stock_PYP)] <- "z"

# Remove surplus years

Stock_CP <- remove_ext_yr(Stock_CP, c("Asset", "Industry"), pub_year)
Stock_PYP <- remove_ext_yr(Stock_PYP, c("Asset", "Industry"), pub_year)

# Add new worksheets

writeData(S_template, sheet='Current prices', x = Stock_CP, startCol = 4, startRow = 6, colNames= F)
writeData(S_template, sheet="Previous years prices", x = Stock_PYP, startCol = 4, startRow = 6, colNames= F)

saveWorkbook(S_template, paste0(outputDir, "Pub_tables/Stocks.xlsx"), overwrite=T)

#T2000 table for OECD

T2000_template <- loadWorkbook(paste0(inputDir, "Deliveries/T2000.xlsx"))
Format <- read.xlsx(paste0(xlsxFile = paste0(inputDir, "Deliveries/T2000.xlsx")),
                    sheet = 1, startRow = 1)
Format <- select(Format, c(INSTR_ASSET, ACTIVITY, PRICES))

Stock_CP$PRICES <- "U"
Stock_PYP$PRICES <- "O"

colnames(Stock_CP)[1] = "INSTR_ASSET" 
colnames(Stock_PYP)[1] = "INSTR_ASSET"
colnames(Stock_CP)[2] = "ACTIVITY" 
colnames(Stock_PYP)[2] = "ACTIVITY" 

T2000_CP <- left_join(Format, Stock_CP)
T2000_CP <- filter(T2000_CP, PRICES == "U")
T2000_PYP <- left_join(Format, Stock_PYP)
T2000_PYP <- filter(T2000_PYP, PRICES == "O")
T2000 <- rbind(T2000_CP, T2000_PYP)

T2000 <- remove_ext_yr(T2000, c("INSTR_ASSET", "ACTIVITY", "PRICES"), pub_year)

name <- names(T2000)
nameToRemove <- name[1:3]
requiredVariables <- c(names(T2000) [! names(T2000) %in% nameToRemove])
requiredVariables <- substring(requiredVariables, 2,5)
names <- c(nameToRemove,requiredVariables)
names(T2000) <- names

T2000[T2000 == "z"] <- 0

write.csv(T2000, paste0(outputDir,"Pub_tables/T2000.csv"))


################### Capital stocks tables #######################

# Merge capstocks import file to CDIDs !!!!!!!!!!!!!!!!

#created after May 2022 publication (saved in PRELIM5 inputs)

CDID_mapping <- read.csv(paste0(inputDir, "Mapping & Aggregation/tables_CDID_mapping.csv"), stringsAsFactors = FALSE)

# Add ICT to OME (not really plant) - should not do this for CVMs

plant <- filter(data, Asset=="N1132" | Asset=="N11O")
plant$Asset <- "PLANT"
plant <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=plant, FUN=sum)
data <- rbind(data, plant)

all_CDIDs <- left_join(CDID_mapping, data)

missing_CDIDs <- filter(all_CDIDs, is.na(X1995))

# Write missing CDID's - comp may be more effective?

write.csv(missing_CDIDs, paste0(outputDir, "Pub_tables/missing_CDIDs.csv"))

all_CDIDs <- filter(all_CDIDs, !is.na(X1995))
all_CDIDs <- select(all_CDIDs, -Sector, -Asset, -Industry, -Price, -Measure, -Periodicity)

# Add empty spaces to all CDIDs

empty <- filter(all_CDIDs, CDID=="CIHA")
empty[!is.na(empty)] <- ""
all_CDIDs <- rbind(all_CDIDs, empty)

# Find sheet names

sheet_names <- getSheetNames(paste0(inputDir, "Deliveries/capitalstockstables2021.xlsx"))
sheet_list <- as.list(rep(NA, length(sheet_names)))
names(sheet_list) = sheet_names

for (sheet in sheet_names){
  
  sheet_list[[sheet]] <- read.xlsx(
    xlsxFile = paste0(inputDir, "Deliveries/capitalstockstables2021.xlsx"),
    sheet = sheet,
    startRow = 2,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE,
    rows = NULL,
    cols = NULL,
    check.names = FALSE,
    namedRegion = NULL,
    na.strings = "NA",
    fillMergedCells = FALSE
  )
  
}

sheet_not_updated <- vector()
missing_table_CDIDs <- vector()
sheet_col <- vector()

pub_year_CDIDs <- select(all_CDIDs, c(CDID, ends_with(paste0("X",pub_year))))

for (sheet in sheet_names){
  print(sheet)
  
  if  (('2010' %in% names(sheet_list[[sheet]])) || grepl("b",sheet)){
    #previous code slots in here
    if ('CDID' %in% names(sheet_list[[sheet]])){
      
      CDID_col <- match('CDID', names(sheet_list[[sheet]]))
      sheet_col <- append(sheet_col, CDID_col)
      CDIDs <- as.data.frame(sheet_list[[sheet]][, "CDID"])
      names(CDIDs) <- "CDID"
      
      # Merge to CDIDs
      
      CDIDs$CDID <- as.character(CDIDs$CDID)
      CDIDs['CDID'][is.na(CDIDs['CDID'])] <- ""
      CDIDs <- left_join(CDIDs, all_CDIDs)
      
      # Remove surplus years
      
      CDIDs <- remove_ext_yr(CDIDs, c('CDID'), pub_year)
      
      # Add missing CDIDs to list
      
      add_miss_table <- filter(CDIDs, !is.na(CDID) & is.na(X1995))
      add_miss_table <- add_miss_table$CDID
      missing_table_CDIDs <- append(missing_table_CDIDs, add_miss_table)
      missing_table_CDIDs <- unique(missing_table_CDIDs)
      
      # Replace NAs with zeroes
      
      CDIDs[is.na(CDIDs)] <- "0"
      CDIDs_cols <- substr(names(CDIDs)[2:ncol(CDIDs)], start = 2, stop = 5)
      names(CDIDs)[2:ncol(CDIDs)] <- CDIDs_cols
      
      # Ovewrite list
      
      sheet_list[[sheet]] <- CDIDs
      
    } else {
      
      sheet_not_updated <- append(sheet_not_updated, paste0(sheet))
      
    }
    
    
  }  else {
    if(!(grepl("Contents",sheet) || grepl("Enquiries",sheet))){
      CDID_sheet <- paste0(sheet, "b")
      cols <- names(sheet_list[[sheet]])
      cols <- cols[-c(1,2,3)]
      CDID_col <- match (cols[1], names(sheet_list[[sheet]]))
      sheet_col <- append(sheet_col, CDID_col)
      #all_CDIDs <- data.frame()
      pub_year_CDIDs <- unique(pub_year_CDIDs)
      temp_format <- as.data.frame(sheet_list[[CDID_sheet]][, "X2"])
    
      for (group in cols){
        print(group)
        CDIDs <- as.data.frame(sheet_list[[CDID_sheet]][, group])
        names(CDIDs) <- "CDID"
        
        # Merge to CDIDs
        
        CDIDs$CDID <- as.character(CDIDs$CDID)
        CDIDs['CDID'][is.na(CDIDs['CDID'])] <- ""
        CDIDs$format <- temp_format$`sheet_list[[CDID_sheet]][, "X2"]`
        CDIDs <- left_join(CDIDs, pub_year_CDIDs, by = 'CDID')
        
        # Remove surplus years
        #xpub_year <- paste0("X",pub_year)
        #CDIDs <- select(CDIDs,c(CDID,format,contains(xpub_year)))
        #CDIDs <- remove_ext_yr(CDIDs, c('CDID'), pub_year)
        #CDIDs <- remove_prev_yr(CDIDs, c('CDID'), pub_year)
        colnames(CDIDs)[3] = "year" 
        
        CDIDs$year <- ifelse(CDIDs$format != "" & CDIDs$year == "","z",CDIDs$year)
        
        
        #remove NAs
        CDIDs$year <- ifelse(is.na(CDIDs$format) & is.na(CDIDs$year), "", CDIDs$year)


        CDIDs <- select(CDIDs, CDID, year)
        # Add missing CDIDs to list

        add_miss_table <- filter(CDIDs, !is.na(CDID) & is.na(year))
        add_miss_table <- add_miss_table$CDID
        missing_table_CDIDs <- append(missing_table_CDIDs, add_miss_table)
        missing_table_CDIDs <- unique(missing_table_CDIDs)
        
        # Replace NAs with zeroes
          
        CDIDs[is.na(CDIDs)] <- "0"
        CDIDs_cols <- group
        names(CDIDs)[2] <- CDIDs_cols
        
        #remove CDIDs
        CDIDs <- CDIDs[2]
            
        #add to data.frame of all groups
        if(exists('all_groups')){
          all_groups <- cbind(all_groups,CDIDs)
        } else{
          all_groups <- CDIDs
        }
        
      
      }
    
    
    # Overwrite list
    
    sheet_list[[sheet]] <- all_groups
    
    
    }else{
      sheet_not_updated <- append(sheet_not_updated, paste0(sheet))
    }
  }
    #reset for next sheet
    remove(all_groups)
  
  
}

sheet_names <- sheet_names[!(sheet_names %in% sheet_not_updated)]

# Write to output table

Main_tables <- loadWorkbook(paste0(inputDir, "Deliveries/capitalstockstables2021.xlsx"))

for (i in 1:length(sheet_names)){
  
  writeData(Main_tables, sheet=sheet_names[i], x = sheet_list[[sheet_names[i]]], startCol = sheet_col[i], startRow = 2)
  
}

saveWorkbook(Main_tables, paste0(outputDir, "Pub_tables/Main tables.xlsx"), overwrite=T)

# Write out CSDB templates

all_CDIDs <- filter(all_CDIDs, CDID!="")
all_CDIDs <- remove_ext_yr(all_CDIDs, c('CDID'), pub_year)
#all_CDIDs <- gather(all_CDIDs, Period, Value, 2:ncol(all_CDIDs))
#all_CDIDs$Period <- substr(all_CDIDs$Period,2,5)
#all_CDIDs <- filter(all_CDIDs, Period<=pub_year)
#all_CDIDs <- spread(all_CDIDs, CDID, Value)
names(all_CDIDs)[1] <- "*CSTHLD"

start <- 1
end <- 500
n <- 1

while (start<nrow(all_CDIDs)){
  
  selected_columns <- unique(c("*CSTHLD", names(all_CDIDs)[start:end]))
  selected_columns <- selected_columns[!is.na(selected_columns)]
  CSDB <- all_CDIDs[selected_columns]
  #CSDB <- select(all_CDIDs, selected_columns)
  start <- min(start+500, nrow(all_CDIDs))
  end <- min(end+500, nrow(all_CDIDs))
  write.csv(CSDB, paste0(outputDir, "Pub_tables/CSDB_", n, ".csv"), row.names = FALSE)
  
  # More work needed to verify if this write.table will work
  
  #write.table(CSDB, paste0("CSDB_", n, ".txt"), sep = " ", dec = ".",
  #            row.names = FALSE, col.names = TRUE, quote=FALSE)
  
  n <- n+1
  
}

# Report any unmatched data - potential to improve

write.csv(missing_table_CDIDs, paste0(outputDir, "Pub_tables/missing_table_CDIDs.csv"))
write.csv(sheet_not_updated, paste0(outputDir, "Pub_tables/sheet_not_updated.csv"))
write.csv(all_CDIDs, paste0(outputDir, "Pub_tables/all_CDIDs.csv"))
write.csv(missing_CDIDs, paste0(outputDir, "Pub_tables/missing_CDIDs.csv"))

#T2000 table for OECD

CDIDs <- left_join(CDID_mapping, data)
CDIDs <- filter(CDIDs, Price == "PYP")

