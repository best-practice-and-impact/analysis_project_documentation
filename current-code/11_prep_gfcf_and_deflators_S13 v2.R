######################### PROCESS GFCF for S13 #################################

# Read in GFCF for historic and balanced accounts for S13 data only
# Read in deflators for historic and balanced accounts
# Apply splits
# Combine historic and balanced accounts
# Remove leading zeros from GFCF series

# ============================ READ DATASETS ===================================
flog.info("Processing GFCF and Deflators for S.13.\n")
flog.info("Extracting Balanced Accounts (BA) GFCF.\n")

# Added ability to keep gfcf the same for specified closed period - this is set in configurations

last_closed_quarter <- xms_last_closed_quarter

for (a in c("CG","LG")){
  
  if (nchar(last_closed_quarter)!=7){
    
    # Reading in open dataset if entire post-1997 data is unconstrained
    
    futile.logger::flog.info("Unconstrained\n")
    cp <-s13_gfcf_open %>% tidyr::pivot_longer(4:ncol(s13_gfcf_open), 
                                              names_to = "Period", 
                                              values_to = "Value")
    
    cp <- dplyr::filter(cp, Sector== ifelse(a=="CG", "S.1311", "S.1313"))
      

    
  }
  
  if (nchar(last_closed_quarter)==7){
    
      futile.logger::flog.info(paste0("Constrained to ", last_closed_quarter,"\n"))
    
    # Constraining data to closed period gfcf when last_closed quarter is set.
    
    method <- "value"
    
    open_dataset <- s13_gfcf_open %>% 
        tidyr::pivot_longer(c(-Sector, -Asset, -Industry), names_to = "Period", values_to = "Value") %>% 
        dplyr::filter(Sector == ifelse(a == "CG", "S.1311", "S.1313")) %>% 
        dplyr::group_by(Period) %>% dplyr::mutate(sum = sum(Value)) %>%
        dplyr::filter(sum!=0)
    open_dataset$sum <- NULL
    
    closed_dataset <- read.csv(paste0(last_run, "/Outputs/gfcf_ba_S13.csv")) %>%
        dplyr::rename(Asset = Asset...Product)
    closed_dataset$Basis <- NULL
    closed_dataset <-  closed_dataset %>% 
        tidyr::gather(Period, Value, -Sector, -Asset, -Industry) %>%
        dplyr::filter(Sector == ifelse(a == "CG", "S.1311", "S.1313"))

    last_qod <- open_dataset %>% 
        dplyr::pull(Period) %>% 
        gsub("[a-zA-Z]", "", .) %>% 
        as.numeric() %>% 
        max()

    if (as.numeric(gsub("[a-zA-Z ]", "", last_closed_quarter))>=last_qod){
      
      cp <- closed_dataset
      print('a')
      
    } else {
      
      open_dataset <- spread(open_dataset, Period, Value)
      closed_dataset <- spread(closed_dataset, Period, Value)
      closed_dataset$X <- NULL
      
      h_columns <- colnames(open_dataset[1:3])
      d_columns <- colnames(open_dataset[4:ncol(open_dataset)])
      # Check if last closed quarter should be included
      open_quarters <- d_columns[substr(d_columns,2,5)>substr(last_closed_quarter,2,5) |
                                   substr(d_columns,2,5)==substr(last_closed_quarter,2,5) &
                                   substr(d_columns,7,7) >= substr(last_closed_quarter,7,7)]
      
      open_dataset <- open_dataset[c(h_columns, open_quarters)]
      colnames(open_dataset)[colnames(open_dataset)==last_closed_quarter] <- "splicing_ratio"
      closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], last_closed_quarter)
      closed_dataset <- closed_dataset[c(h_columns, closed_quarters)]
      
      # Multiply open period data with splicing ratio - see 10_prep... for further details
      
      cp <- left_join(closed_dataset, open_dataset)
      #cp$check <- cp$splicing_ratio
      cp["splicing_ratio"] <- cp[last_closed_quarter]/cp["splicing_ratio"]
      cp$splicing_ratio[is.na(cp$splicing_ratio)] <- 1
      cp$splicing_ratio[cp$splicing_ratio<=0] <- 1
      cp$splicing_ratio[mapply(is.infinite, cp$splicing_ratio)] <- 1
      
      # This allows the ability to decide not to use growth rates for certain series
      
      if (file.exists(paste0(inputDir, "/Parameters, Assumptions & Adjustments/s13_dontusegrowth.csv"))){
        
        override <- read.csv(paste0(inputDir, "/Parameters, Assumptions & Adjustments/s13_dontusegrowth.csv"))
        
        for (r in 1:nrow(override)){
          
          cp$splicing_ratio <- ifelse(cp$Sector==override$Sector[r] &
                                        cp$Industry==override$Industry[r] &
                                        cp$Asset==override$Asset[r], 1, cp$splicing_ratio)
          
        }
        
      }
      
      if (method=="value"){
        
        cp$splicing_ratio <- 1
        
      }
      
      open_quarters <- open_quarters[2:length(open_quarters)]
      
      for (q in open_quarters){
        
        cp[q][is.na(cp[q])] <- 0
        cp[q] <- cp[q]*cp$splicing_ratio
        
      }
      
      cp$splicing_ratio <- NULL
      
      cp <- gather_(cp, "Period", "Value", d_columns)
      cp$Value <- ifelse(is.nan(cp$Value),0,cp$Value)
      cp$Value <- ifelse(is.na(cp$Value),0,cp$Value)
      rm(open_dataset, closed_dataset, open_quarters,q, d_columns,h_columns)
      
    }
    
  }
  
  
  # ===================== CHECKING THE INPUT DATA ================================
  
  # Checking if there are any duplicates in the input data
  #check_duplicated(cp, "GFCF/S13/gfcf_ba_S13.xlsx")
  #if (nchar(last_closed_quarter)!=7){
  
  #  check_duplicated(defRecent, "Current Input Data/deflators_ba.csv")
  
  #}
  
  #if (nchar(last_closed_quarter)==7){
  
  #  check_duplicated(defClosed, "Current Input Data/deflators_ba.csv")
  #  check_duplicated(defOpen, "Current Input Data/deflators_ba.csv")
  
  #}  
  
  # ===================== FORECAST GFCF AND DEFLATORS ============================
  
  # Using the function specified in 10_prep_gfcf_and_deflators.R to forecast GFCF and deflators:
  
  # Forecasting deflators:
  
  #if (nchar(last_closed_quarter)!=7){
  
  #}
  
  #if (nchar(last_closed_quarter)==7){
  
  #  defOpen <- forecasting (deflators_forecastPeriod, defOpen)
  
  #}
  
  # Calculate forecast period from set date as GFCF from R4.2 and S13 may not be to the same date
  
  gfcf_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(cp$Period),1),2,5)))*4 + 
                            as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(cp$Period),1),7,7)))
  
  gfcf_forecastPeriod <- 0
  
  cp <- forecasting (gfcf_forecastPeriod, cp)
  
  ######################### FORECASTING NOT WORKING, SO TAKING THE AVERAGE GROWTH OF THE LAST 5 QUARTERS AND ADDING IT ON TO THE NEW QUARTER. CAN ALSO BE NEGATIVE
  
  gfcf_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(cp$Period),1),2,5)))*4 + 
                            as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(cp$Period),1),7,7)))
  
  forecastByGrowth = TRUE
  if (forecastByGrowth)
  {
    source("miscCapStocksFunctions.R")
    # Find where all the NA and NaN values are (TRUE or FALSE Matrix)
    naVals <- apply(cp, 2, is.na)
    nanVals <- apply(cp, 2, is.nan)
    # Set all the NA and NaN values to 0
    cp[naVals] <- 0.00000
    cp[nanVals] <- 0.00000
    rm(naVals, nanVals)
    cp$Value <- as.double(cp$Value)
    maxQtr <- max(cp$Period)
    cp <- cp %>% spread(Period, Value, fill = 0)
    
    for (i in 1:gfcf_forecastPeriod)
    {
      maxQtr <- addQuarter(maxQtr)
      diff <-   ((cp[,ncol(cp) - 4] - cp[,ncol(cp) - 5]) +
                   (cp[,ncol(cp) - 3] - cp[,ncol(cp) - 4]) +
                   (cp[,ncol(cp) - 2] - cp[,ncol(cp) - 3]) +
                   (cp[,ncol(cp) - 1] - cp[,ncol(cp) - 2]) +
                   (cp[,ncol(cp) - 0] - cp[,ncol(cp) - 1])) / 5
      cp[,ncol(cp) + 1] <- 0
      names(cp)[ncol(cp)] <- paste(maxQtr)
      
      cp[,ncol(cp)] <- cp[,ncol(cp) - 1] + diff
      diff <- NULL
    }
    
    gfcf_save <- rename_(cp, "Asset & Product" = "Asset")
    
    if (a=="CG"){
      
      gfcf_save_CG <- gfcf_save
      
    } else {
      
      gfcf_save <- rbind(gfcf_save, if (exists('gfcf_save_CG')) gfcf_save_CG)
      
    }
    
    cp <- cp %>% gather_(key = "Period", value = "Value", names(cp[4:ncol(cp)]))
    
  }
  
  rm(i)
  
  if (a=="CG"){
    
    cp_CG <- cp
    
  } else {
    
    cp <- rbind(cp_CG, cp)
    
  }
  
}

write.csv(gfcf_save, paste0(outputDir,"gfcf_ba_S13.csv"))

flog.info("Extracting Historic data.")
historic <- extractHistoric(paste0(inputDir, "Historic Input Data/gfcf_hist_S13.csv"))
#historic <- extractHistoric(paste0(inputDir, "gfcf_hist_S13_leigh.csv"))

flog.info("Extracting Historic Deflators.")
defHist <- extractHistoricDeflators(paste0(inputDir, "Historic Input Data/deflators_hist.csv"))

flog.info("Extracting 1997+ Deflators.")
# RefYear == 100. Will divide by 100 later

if (nchar(last_closed_quarter)!=7){
  
  defOpen <- deflators_open
  
}

if (nchar(last_closed_quarter)==7){
  
  defClosed <- read.csv(paste0(last_run, "/Outputs/deflators_ba_S13.csv"))
  defOpen <- deflators_open
  
}

# ===================== CHECKING THE INPUT DATA ================================

# Checking if there are any duplicates in the input data
check_duplicated(cp, "GFCF/S13/gfcf_ba_S13.xlsx")

if (nchar(last_closed_quarter)!=7){
  
  check_duplicated(defOpen, "Deflators/deflators_ba.csv")
  
}

if (nchar(last_closed_quarter)==7){
  
  check_duplicated(defClosed, "Deflators/deflators_ba_closed.csv")
  check_duplicated(defOpen, "Deflators/deflators_ba_open.csv")
  
}

# ===================== FORECAST GFCF AND DEFLATORS ============================

# Using the function specified in 10_prep_gfcf_and_deflators.R to forecast GFCF and deflators:

# Forecasting deflators:

GFCF_FORECAST <- FALSE
DEFLATORS_FORECAST <- FALSE

deflators_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(defOpen$Period),1),2,5)))*4 +
                               as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(defOpen$Period),1),7,7)))

if (deflators_forecastPeriod > 0) {
  DEFLATORS_FORECAST <- TRUE
}

defOpen <- forecasting (deflators_forecastPeriod, defOpen)

# Calculate forecast period from set date as GFCF from R4.2 and S13 may not be to the same date

gfcf_forecastPeriod <- 0

cp <- forecasting (gfcf_forecastPeriod, cp)

######################### FORECASTING NOT WORKING, SO TAKING THE AVERAGE GROWTH OF THE LAST 5 QUARTERS AND ADDING IT ON TO THE NEW QUARTER. CAN ALSO BE NEGATIVE

gfcf_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(cp$Period),1),2,5)))*4 + 
                          as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(cp$Period),1),7,7)))

if (gfcf_forecastPeriod > 0) {
  GFCF_FORECAST <- TRUE
}

if (GFCF_FORECAST==TRUE){
  forecastByGrowth = TRUE
  if (forecastByGrowth)
  {
    source("miscCapStocksFunctions.R")
    # Find where all the NA and NaN values are (TRUE or FALSE Matrix)
    naVals <- apply(cp, 2, is.na)
    nanVals <- apply(cp, 2, is.nan)
    # Set all the NA and NaN values to 0
    cp[naVals] <- 0.00000
    cp[nanVals] <- 0.00000
    rm(naVals, nanVals)
    cp$Value <- as.double(cp$Value)
    maxQtr <- max(cp$Period)
    cp <- cp %>% spread(Period, Value, fill = 0)
    
    for (i in 1:gfcf_forecastPeriod)
    {
      maxQtr <- addQuarter(maxQtr)
      diff <-   ((cp[,ncol(cp) - 4] - cp[,ncol(cp) - 5]) +
                   (cp[,ncol(cp) - 3] - cp[,ncol(cp) - 4]) +
                   (cp[,ncol(cp) - 2] - cp[,ncol(cp) - 3]) +
                   (cp[,ncol(cp) - 1] - cp[,ncol(cp) - 2]) +
                   (cp[,ncol(cp) - 0] - cp[,ncol(cp) - 1])) / 5
      cp[,ncol(cp) + 1] <- 0
      names(cp)[ncol(cp)] <- paste(maxQtr)
      
      cp[,ncol(cp)] <- cp[,ncol(cp) - 1] + diff
      diff <- NULL
    }
    cp <- cp %>% gather_(key = "Period", value = "Value", names(cp[5:ncol(cp)]))
  }
  rm(i)
}

###################################################################################################################################

# ============================ PROCESS DATA ====================================

# Removing ICT because we have the lower-level series (HARDWARE and TELECOMS)
flog.info("Removing ICT from BA CP.")
cp <- filter(cp, Asset != "ICT")

# --- Remove TOTAL Industries (except for DWELLINGS)
# Each Sector/Asset has a "TOTAL" row for all Industries. Usually this is
# restating the lower-level series so should be removed

# Filter out remaining TOTAL industries.
flog.info("Removing industry TOTALs.")
cp <- filter(cp, Industry != "TOTAL")

# Filter out LAND.IMPROVEMENTS as we will create these by splitting OTHER.BUILDINGS
flog.info("Removing LAND.IMPROVEMENTS (derived later).")
cp <- filter(cp, Asset != "LAND.IMPROVEMENTS")

# At this point how many series do we have?
flog.info(paste("BA Series count:", nrow(getSeries(cp))))


# ---------------------- Split BUILDINGS ---------------------------------------

# Splitting OTHER.BUILDINGS into OTHER.BUILDINGS/OTHER.STRUCTURES/LAND.IMPROVE
# is a prerequisite to apportioning Transfer Costs since their allocation
# proportions are partly based on the quantities in OTHER.BUILDINGS *after*
# splitting off Transfer Costs.

flog.info("\nSplitting OTHER.BUILDINGS.")

specPath <- paste0(inputDir, "Splits/splits_otherbuildings.csv")

cp <- applySplitSpec(cp, specPath, existingCategory = "Asset", newCategory = "Split",
                     joinKeys = c("Asset", "Industry", "Sector"),tol = 0.01)
rm(specPath)
flog.info(paste("BA Series count:", nrow(getSeries(cp))))

# ---------------------- Split SOFT.DATA ---------------------------------------

# Split BA SOFT.DATA into SOFT.DATA(P) and SOFT.DATA(OA) to match historic categories.
flog.info("\nSplitting SOFT.DATA.")

specPath <- paste0(inputDir, "Splits/splits_software.csv")

cp <- applySplitSpec(cp, specPath, existingCategory = "Asset", newCategory = "Split",
                     joinKeys = c("Asset", "Sector", "Industry"),tol = 0.01)
rm(specPath)
flog.info(paste("BA Series count:", nrow(getSeries(cp))))

# ---------------------- Process Historic Data ---------------------------------

# - Remove superfluous periods
# - Split OTHER.BUILDINGS

flog.info(paste("Historic Series count:", nrow(getSeries(historic))))
# We only need up to the linkPeriod (but not including)
periods <- sort(unique(historic$Period))
lastHistoricPeriod <- periods[which(periods == linkPeriod) - 1]
historic <- filter(historic, Period <= lastHistoricPeriod)

# Splitting OTHER.BUILDINGS into OTHER.BUILDINGS/OTHER.STRUCTURES/LAND.IMPROVE
flog.info("Splitting historic OTHER.BUILDINGS.")

specPath <- paste0(inputDir, "Splits/splits_otherbuildings.csv")

historic <- applySplitSpec(historic, specPath, existingCategory = "Asset", newCategory = "Split",
                           joinKeys = c("Asset", "Industry", "Sector"),tol = 0.01)
rm(specPath)
flog.info(paste("Historic Series count:", nrow(getSeries(historic))))

# Agreed to drop Industry 99 R&D for historic
flog.info("Removing RESEARCH.DEVELOPMENT/99.")
historic <- filter(historic, !(Asset == "RESEARCH.DEVELOPMENT" & Industry == "99"))
flog.info(paste("Historic Series count:", nrow(getSeries(historic))))

# -------------------- Combine Historic and Bal Acc ----------------------------
# Before we combine we need to check for coverage between historic and balanced accounts
# We may be missing series or there may be coding scheme differences

# Previously, when we needed to calculate CVM series, these checks were more
# important in order to match up series to calculate historic in CVM. Now it
# is important just for checking continuity issues.

# Output coverage mismatches
flog.info("\nChecking BA/Historic coverage.")
# Check missing categories between cp and historic
checkDimCoverage(cp, historic, "Sector")
checkDimCoverage(cp, historic, "Industry")
checkDimCoverage(cp, historic, "Asset")

# In balanced accounts but not historic:
BA_not_Historic <- anti_join(cp, historic, by = c("Sector", "Industry", "Asset")) %>%
  group_by(Sector, Industry, Asset) %>%
  summarise(total = sum(Value)) %>%
  ungroup() %>%
  arrange(desc(total))
flog.info("In BA not historic series:", as.data.frame(BA_not_Historic), capture = TRUE)
if (OUTPUT_SHARING == TRUE) {
  write_excel_csv(BA_not_Historic, paste0(otherOutputs, "coverage_check_BA_not_Historic_S13_", runTime, ".csv"))
} else {
  write_excel_csv(BA_not_Historic, paste0(outputDir, "coverage_check_BA_not_Historic_S13_", runTime, ".csv"))
}

# In historic but not balanced accounts:
# Set aside for separate calculation of CVM later
Historic_not_BA <- anti_join(historic, cp, by = c("Sector", "Industry", "Asset"))
Historic_not_BA_series <- Historic_not_BA %>%
  group_by(Sector, Industry, Asset) %>%
  summarise(total = sum(Value)) %>%
  ungroup() %>%
  arrange(desc(total))
flog.info("In historic not BA series:", as.data.frame(Historic_not_BA_series), capture = TRUE)
if (OUTPUT_SHARING == TRUE) {
  write_excel_csv(Historic_not_BA_series, paste0(otherOutputs, "coverage_check_Historic_not_BA_S13_", runTime, ".csv"))
} else {
  write_excel_csv(Historic_not_BA_series, paste0(outputDir, "coverage_check_Historic_not_BA_S13_", runTime, ".csv"))
}

# --- Combine Historic and BA into "gfcf" dataset
flog.info("\nCombining BA with historic.")
# Combining balacc/historic CP series is simply a case of appending one to the other
gfcfS13 <- addHistoricCp(
  historicCp = historic,
  baCp = cp,
  linkPeriod = linkPeriod)

flog.info(paste("Combined Series count:", nrow(getSeries(gfcf))))

##############          Transfer Costs          #############################

indSplit <- gfcfS13 %>%
  filter(Asset %in% c("DWELLINGS", "OTHER.BUILDINGS")) %>%
  group_by(Sector, Industry) %>%
  filter(Value > 0) %>%  # In case of negative stocks, remove them from apportionment
  summarise(Value = sum(Value)) %>%
  mutate(Proportion = Value / sum(Value),
         Asset = "TC") %>%
  rename(Split = Industry) %>%
  mutate(Industry = "68") %>%
  select(Sector, Industry, Asset, Split, Proportion) %>%
  ungroup()
# Repeat the Proportions for every Period in cpTC
indSplit[, unique(s13_TC$Period)] <- indSplit$Proportion
indSplit$Proportion <- NULL

# indSplit changes in closed period, as fixed proportion is calculated from entire dataset
# Using fixed split from BB20 closed - need to investigate the best way to address this 
# Is fixed proportion credible assumption and would dynamic splits be too volatile

if (nchar(last_closed_quarter)==7){
  
  
  indSplit <- readRDS(paste0(inputDir,"/Splits/S13_split.rds"))
  
}

# Apply the industry split for the TC data
cpTC <- filter(s13_TC, Asset=="LAND.IMPROVEMENTS.TC")
cpTC$Asset <- "TC"
cpTC <- applySplitSpec(cpTC, spec = indSplit, existingCategory = "Industry", newCategory = "Split",
                       joinKeys = c("Sector", "Industry", "Asset"),tol = 0.01)
cpTC$Asset <- "LAND.IMPROVEMENTS.TC"
gfcfS13 <- bind_rows(gfcfS13, cpTC)


# ---------------------- Split MACHINERY ---------------------------------------

# Note we may need to add deflators/lifelengths/configs for new Asset types
# Will also need to be reflected in asset hierarchy. Until this is done this
# section is commented out.

flog.info("\nSplitting OTHER.MACHINERY.")

specPath <- paste0(inputDir, "Splits/splits_othermachinery.csv")

gfcfS13 <- applySplitSpec(gfcfS13, specPath, existingCategory = "Asset", newCategory = "Split",
                          joinKeys = c("Asset", "Sector", "Industry"),tol = 0.01)
rm(specPath)
flog.info(paste("BA Series count:", nrow(getSeries(cp))))

# Change to other.machinery.m - look at properly next BB

gfcfS13$Asset <- ifelse(gfcfS13$Asset=="OTHER.MACHINERY", "OTHER.MACHINERY.M", gfcfS13$Asset)

# --------------------- Add Zero Values ----------------------------------------

# Take the combined cp data and fill out series with zeros if they finish early
# The PIM calculates stocks where we have periods for a series, so if we want
# outputs when GFCF has finished we must provide zero values. This will fill zeros
# *before* as well as *after*, but we'll remove the before-zeros later

gfcfS13$Prices <- NULL
gfcfS13 <- complete(gfcfS13, Period, nesting(Sector, Industry, Asset), fill = list(Value = 0))

# ---------------------- Add Price Index ---------------------------------------

# Rename the Value column to gfcfCP and remove Prices
gfcfS13 <- gfcfS13 %>%
  rename(gfcfCP = Value)

# If last_closed_quarter is set (hence is 7 characters long), link closed period deflators to open period deflators 
# in addition to the historic deflators

if (nchar(last_closed_quarter)!=7){
  
  # Expand historic deflators to match gfcf coverage
  defHist <- expandSpec(defHist, toCover = gfcfS13, joinKeys = c("Asset", "Industry", "Sector"))
  # Expand recent deflators to match historic coverage
  defOpen <- expandSpec(defOpen, toCover = defHist, joinKeys = c("Asset", "Industry", "Sector"))
  
  # Link historic and recent deflators. We must have one overlapping period at the linkPeriod
  defAll <- linkDeflators(defHist, defOpen, linkPeriod)
  defAll <- mutate(defAll, Value = Value / 100)  # Change deflator to be 1 at refPeriod (instead of 100)
  
  defsave <- defAll
  defsave$Prices <- "def"
  defsave <- spread(defsave, Period, Value)
  write.csv(defsave, paste0(outputDir,"/deflators_ba_S13.csv"), row.names=FALSE)
  
  defAll$Prices <- NULL
  
}

if (nchar(last_closed_quarter)==7){
  
  # Expand recent deflators to match historic coverage
  
  defOpen <- expandSpec(defOpen, toCover = gfcfS13, joinKeys = c("Asset", "Industry", "Sector"))
  defOpen <- mutate(defOpen, Value = Value / 100)  # Change deflator to be 1 at refPeriod (instead of 100)
  defOpen <- spread(defOpen, Period, Value)
  defOpen$Prices <- NULL
  
  # Cannot take on revisions to deflators prior to reference year without affecting 
  # constrained estimates. So use later of refPeriod or last_closed_quater
  
  def_join <- ifelse(substr(refPeriod,2,5)>=substr(last_closed_quarter,2,5),
                     refPeriod,last_closed_quarter)
  
  h_columns <- colnames(defOpen[1:3])
  d_columns <- colnames(defOpen[4:ncol(defOpen)])
  hist_columns <- unique(defHist$Period)
  hist_columns <- hist_columns[1:676]
  open_quarters <- d_columns[substr(d_columns,2,5)>substr(def_join,2,5) |
                               substr(d_columns,2,5)==substr(def_join,2,5) &
                               substr(d_columns,7,7) >= substr(def_join,7,7)]
  defOpen <- defOpen[c(h_columns, open_quarters)]
  colnames(defOpen)[colnames(defOpen)==def_join] <- "splicing_ratio"
  closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], def_join)
  defClosed <- defClosed[c(h_columns, hist_columns, closed_quarters)]
  
  # Merge open and closed datasets
  
  # Calculate splicing ratio by dividing closed period gfcf by open period gfcf in the last closed quarter.
  # Then multiply open period gfcf after the closed quarter by the splicing ratio
  
  defAll <- left_join(defClosed, defOpen)
  #cp$check <- cp$splicing_ratio
  defAll["splicing_ratio"] <- defAll[def_join]/defAll["splicing_ratio"]
  
  # Use a ratio of one, where a splicing ratio cannot be calculated or is negative
  
  defAll$splicing_ratio[is.na(defAll$splicing_ratio)] <- 1
  defAll$splicing_ratio[defAll$splicing_ratio<=0] <- 1
  defAll$splicing_ratio[mapply(is.infinite, defAll$splicing_ratio)] <- 1
  
  open_quarters <- open_quarters[2:length(open_quarters)]
  
  for (q in open_quarters){
    
    defAll[q][is.na(defAll[q])] <- 0
    defAll[q] <- defAll[q]*defAll$splicing_ratio
    
  }
  
  defAll$splicing_ratio <- NULL
  
  # Ensure that deflators are 100 in reference year
  
  #defAll %>% mutate_(adjustment = cummean(paste0(substr(refPeriod,1,6),"1")+
  #                  paste0(substr(refPeriod,1,6),"2")+
  #                  paste0(substr(refPeriod,1,6),"3")+
  #                  paste0(substr(refPeriod,1,6),"4"))/100)
  
  t_columns <- colnames(defAll[4:ncol(defAll)])
  defAll <- gather_(defAll, "Period", "Value", t_columns)
  #defAll$Value <- ifelse(is.nan(defAll$Value),0,defAll$Value)
  #defAll$Value <- ifelse(is.na(defAll$Value),0,defAll$Value)
  rm(d_columns,h_columns, t_columns)
  
  defsave <- defAll
  defsave$Prices <- "def"
  defsave <- spread(defsave, Period, Value)
  write.csv(defsave, paste0(outputDir,"/deflators_ba_S13.csv"), row.names=FALSE)
  
  defAll$Prices <- NULL
  
  # Write out constrained deflators (would be needed as closed period deflators if multiple closed periods)
  
  #rm(defOpen, defClosed)
  
}

gfcfS13 <- gfcfS13%>%filter(Period%in%unique(defAll$Period))
gfcfS13 <- gfcfS13%>%filter(Industry %in% unique(defAll$Industry))

gfcfS13 <- addPriceIndex(gfcfS13, defAll)

#gfcf <- select(gfcf, -Prices)  # remove Prices column added by addPriceIndex

# --------------------- Final GFCF processing ----------------------------------

# Leading zeros in any series are superfluous. Removing will speed the processing
flog.info("Removing leading zeros from GFCF series.")
gfcfS13 <- gfcfS13 %>%
  group_by(Sector, Industry, Asset) %>%
  # Strip out leading zeros (no need to process these)
  slice(pmin(which(Period == refPeriod), which.max(gfcfCP != 0)):n()) %>%
  ungroup()

# Old version, which lead to rows of data being deleted if there was no data up to refYear:
# flog.info("Removing leading zeros from GFCF series.")
# gfcfS13 <- gfcfS13 %>%
#   group_by(Sector, Industry, Asset) %>%
#   # Strip out leading zeros (no need to process these)
#   slice(which.max(gfcfCP != 0):n()) %>%
#   ungroup()


# ============================ OUTPUT DATA =====================================

# if (WRITE_FILES) write_rds(gfcfS13, paste0(outputDir, "prepared_gfcf_and_defs_S13", runTime, ".Rds"))


# ============================ REMOVE OBJECTS ==================================

# Remove larger datasets from memory (if we're going straight on to running PIM)
rm(cp, BA_not_Historic, defHist, historic, Historic_not_BA, Historic_not_BA_series,
   defAll, periods, lastHistoricPeriod)

