############################# PROCESS GFCF #####################################

# Read in GFCF for historic and balanced accounts
# Read in deflators for historic and balanced accounts
# Apply splits
# Apply aggregations
# Combine historic and balanced accounts
# Remove leading zeros from GFCF series

# ============================ READ DATASETS ===================================

for (s in c("NPISH", "MS")){

  # Added ability to keep gfcf the same for specified closed period - this is set in configuration

  flog.info("Extracting Balanced Accounts (BA) GFCF.\n")
  
  flog.info("Extracting Historic data.")
  historic <- extractHistoric(paste0(inputDir, "Historic Input Data/gfcf_hist_r42.csv"))
  
  if (s=="NPISH"){
    
    last_closed_quarter <- xms_last_closed_quarter
    
  } else {
    
    last_closed_quarter <- ms_last_closed_quarter
    
  }

  # If last_closed_quarter is set (i.e. is seven characters long) keep gfcf from closed period and then use open period 
  # data after the closed period. If there is no closed period no constraining to old period is required. Input data is
  # constrained during a closed period (rather than outputs) as otherwise input would change for all gfcf in the closed 
  # period and this would be constrained at the last stage, which does not appear to be the optimal way of constraining

  if (nchar(last_closed_quarter)!=7){

    flog.info("Unconstrained\n")
    
    if (!exists("cp")){
    
      cp <- gfcf_r42_open
      
    }

  }
  
  if (nchar(last_closed_quarter)==7){
  
  # Method can be set use 'value' to use the value of the open period
  # Using value can create in discontinuities in gfcf - at some point should seek advice from methodology
  
    method <- "value"
  
    flog.info(paste0("Constrained to ", last_closed_quarter,"\n"))
  
  # Reading in open and closed dataset
    
    if (!exists("open_dataset_all")){
      
      closed_dataset <- read.csv(paste0(last_run, "/Outputs/gfcf_ba_", s, ".csv"))
      if ("Asset...Product" %in% colnames(closed_dataset)){
        closed_dataset <- rename(closed_dataset, Asset = Asset...Product)
      }
      if ("Basis" %in% colnames(closed_dataset)){
        closed_dataset$Basis <- NULL
      }
      # Map industry to correct format (i.e. not dates)
      
      ind_map <- readRDS(paste0(inputDir,"Mapping & Aggregation/SIC_Industry_mapping.rds"))
      
      closed_dataset <- left_join(closed_dataset,ind_map)
      closed_dataset$Industry <- ifelse(is.na(closed_dataset$Corrected),
                                       closed_dataset$Industry,closed_dataset$Corrected)
      closed_dataset$Corrected <- NULL
      
      open_dataset <- gfcf_r42_open
    
    }
      
    h_columns <- colnames(open_dataset[1:3])
    d_columns <- colnames(open_dataset[4:ncol(open_dataset)])
    open_quarters <- d_columns[substr(d_columns,2,5)>substr(last_closed_quarter,2,5) |
                                 substr(d_columns,2,5)==substr(last_closed_quarter,2,5) &
                                 substr(d_columns,7,7) >= substr(last_closed_quarter,7,7)]
    open_dataset <- open_dataset[c(h_columns, open_quarters)]
    colnames(open_dataset)[colnames(open_dataset)==last_closed_quarter] <- "splicing_ratio"
    closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], last_closed_quarter)
    closed_dataset <- closed_dataset[c(h_columns, closed_quarters)]
    
  # Merge open and closed datasets
  
  # Calculate splicing ratio by dividing closed period gfcf by open period gfcf in the last closed quarter.
  # Then multiply open period gfcf after the closed quarter by the splicing ratio
    
    cp <- left_join(closed_dataset, open_dataset)
    #cp$check <- cp$splicing_ratio
    cp["splicing_ratio"] <- cp[last_closed_quarter]/cp["splicing_ratio"]
  
  # Use a ratio of one, where a splicing ratio cannot be calculated or is negative
  
    cp$splicing_ratio[is.na(cp$splicing_ratio)] <- 1
    cp$splicing_ratio[cp$splicing_ratio<=0] <- 1
    cp$splicing_ratio[mapply(is.infinite, cp$splicing_ratio)] <- 1
  
    if (method=="value"){
    
      cp$splicing_ratio <- 1
    
    }
  
    open_quarters <- open_quarters[2:length(open_quarters)]
    open_quarters <- open_quarters[!is.na(open_quarters)]
  
    for (q in open_quarters){
    
      cp[q][is.na(cp[q])] <- 0
      cp[q] <- cp[q]*cp$splicing_ratio
    
    }

    cp$splicing_ratio <- NULL
  
    cp <- gather_(cp, "Period", "Value", d_columns)
    cp$Value <- ifelse(is.nan(cp$Value),0,cp$Value)
    cp$Value <- ifelse(is.na(cp$Value),0,cp$Value)
    
    rm(open_dataset_all, closed_dataset_all,open_dataset, 
    closed_dataset, open_quarters,q, d_columns,h_columns)
    
  }
  
# Write out constrained GFCF R4.2 - this would be needed as closed period GFCF if multiple closed periods
    
  if (nchar(last_closed_quarter)!=7){
    
    gfcf_r42 <- cp 
    cp <- gather(cp, Period, Value, 4:ncol(cp))
    
  } else {
    
    gfcf_r42 <- spread(cp, Period, Value)
    
    
  }
  
    #write.csv(gfcf_r42, paste0(outputDir,"gfcf_ba_", s,".csv"))
  
    rm(gfcf_r42)
  
  ### Remove 05-09 if 05 exists
  
  check <- filter(cp, Industry=="05")
  
  if (nrow(check)>0){
    
    cp <- filter(cp, Industry!="05_09")
    
  }
  
  rm(check)
  
  # ------------------------------ Extend fixed series ---------------------------
  
  # CSV's
  
  fixed_series <- c("Splits/splits_otherbuildings.csv", "Splits/split_COFOG.csv",
                    "Splits/splits_othermachinery.csv", "Splits/splits_sector_s1pt.csv",
                    "Splits/splits_software.csv",
                    "Parameters, Assumptions & Adjustments/series_tax_util_cpi.csv",
                    "Splits/NPISH pro-rate Pattern.csv", "Splits/S13_split.rds")
  
  quarters <- read.csv(paste0(inputDir, "Mapping & Aggregation/quarters.csv"))
  
  for (i in fixed_series){
    
    #i <- fixed_series[1]
    
    if (substr(i, nchar(i)-3,nchar(i))==".csv") {
      
      check <- read.csv(paste0(inputDir, i))
      
    } else {
      
      check <- read_rds(paste0(inputDir, i))
      
    }
    
    difference <- setdiff(names(quarters), names(check))
    
    if (length(difference>0)){
      
      for (d in difference){
        
        check[d] <- check[ncol(check)]
        
      }
      
      #write.csv(check, paste0(inputDir, i), row.names=FALSE)
      
      if (substr(i, nchar(i)-3,nchar(i))==".csv") {
        
        #write.csv(check, paste0(inputDir, i), row.names=FALSE)
        
      } else {
        
        #write_rds(check, paste0(inputDir, i))
        
      }
      
      
      
      rm(check, difference)
      
      
      
      
      
    }
    
  }
  
  #rm(i, d)
  
  # xlsx
  
  AverageLifeLengths <- read_excel(paste0(inputDir, 
                                          "Parameters, Assumptions & Adjustments/asset_lives.xlsx"),
                                   sheet = "AverageLifeLengths", col_types = "text")
  CoVs <- read_excel(paste0(inputDir, 
                            "Parameters, Assumptions & Adjustments/asset_lives.xlsx"),
                     sheet = "CoVs", col_types = "text")
  Min <- read_excel(paste0(inputDir, 
                           "Parameters, Assumptions & Adjustments/asset_lives.xlsx"),
                    sheet = "Min", col_types = "text")
  Max <- read_excel(paste0(inputDir, 
                           "Parameters, Assumptions & Adjustments/asset_lives.xlsx"),
                    sheet = "Max", col_types = "text")
  
  # Function to check if there are any duplicates in the input data
  
  check_duplicated <- function (data, filename) {
    if (nrow(data[duplicated(data[,1:5]),1:5])>0) {
      msg <- paste0("Duplicates in ", filename)
      duplicates <- data[duplicated(data[,1:5]),1:5]
      flog.fatal(paste(msg, "series:"), data.frame(duplicates), capture = TRUE)
      stop(msg)
    }
  }
  
  check_duplicated(AverageLifeLengths, "Parameters, Assumptions & Adjustments/asset_lives.xlsx")
  
  check_duplicated(CoVs, "Parameters, Assumptions & Adjustments/asset_lives.xlsx")
  
  check_duplicated(Max, "Parameters, Assumptions & Adjustments/asset_lives.xlsx")
  
  check_duplicated(Min, "Parameters, Assumptions & Adjustments/asset_lives.xlsx")
  
  add_columns <- function(dat){
    
    if (length(setdiff(names(quarters), names(dat))>0)){
      
      for (d in setdiff(names(quarters), names(dat))){
        
        dat[d] <- dat[ncol(dat)]
        
      }
      
    }
    
    return(dat)
    
  }
  
  AverageLifeLengths <- add_columns(AverageLifeLengths)
  CoVs <- add_columns(CoVs)
  Min <- add_columns(Min)
  Max <- add_columns(Max)
  
  #write_xlsx(
  #  list(AverageLifeLengths = AverageLifeLengths, CoVs = CoVs,
  #       Min = Min, Max = Max),
  #  path = paste0(inputDir, "/Parameters, Assumptions & Adjustments/asset_lives.xlsx"),
  #  col_names = TRUE,
  #  format_headers = TRUE
  #)
  
  rm(AverageLifeLengths,CoVs,Min,Max)
  
  # Will look to make this obsolete for future BBs
  
  if (nchar(last_closed_quarter)==7){
    
    tc_ind <- read_rds(paste0(inputDir, "Parameters, Assumptions & Adjustments/prev_indSplit.Rds"))
    tc_s12 <- read_rds(paste0(inputDir, "Parameters, Assumptions & Adjustments/prev_s12split.rds"))
    
    tc_ind <- add_columns(tc_ind)
    tc_s12 <- add_columns(tc_s12)
    
    #tc_ind <- write_rds(tc_ind, paste0(inputDir, "Parameters, Assumptions & Adjustments/prev_indSplit.Rds"))
    #tc_s12 <- write_rds(tc_s12, paste0(inputDir, "Parameters, Assumptions & Adjustments/prev_s12split.rds"))
    
  }
  
  # ============================ PROCESS DATA ====================================
  
  # Removing ICT because we have the lower-level series (HARDWARE and TELECOMS)
  flog.info("Removing ICT from BA CP.")
  cp <- filter(cp, Asset != "ICT")
  
  # --- Remove TOTAL Industries (except for DWELLINGS)
  # Each Sector/Asset has a "TOTAL" row for all Industries. Usually this is
  # restating the lower-level series so should be removed, except in the case of
  # DWELLINGS where this asset type only exists within the TOTAL Industry.
  # For DWELLINGS we will change the Industry to "68" to match historic
  cp <- filter(cp, Asset!= "DWELLINGS" | Industry!= "68")
  cp$Industry[cp$Asset == "DWELLINGS"] <- "68"
  
  # Filter out remaining TOTAL industries.
  flog.info("Removing industry TOTALs.")
  cp <- filter(cp, Industry != "TOTAL")
  
  # Filter out LAND.IMPROVEMENTS as we will create these by splitting OTHER.BUILDINGS
  flog.info("Removing LAND.IMPROVEMENTS (derived later).")
  cp <- filter(cp, Asset != "LAND.IMPROVEMENTS")
  
  # At this point how many series do we have?
  flog.info(paste("BA Series count:", nrow(getSeries(cp))))
  
  # ------------------------------ Split Sectors ---------------------------------
  
  # Apply S.1PT Sector Splits, apportioning into S.11PR, S.14, S.15
  
  # - Set aside DWELLINGS, R&D, and industries 64,65,66
  # - Remove all lower-level (S.11PR, S.14, S.15) series where S.1PT is present
  # - Apportion S.1PT values to lower-level (S.11PR, S.14, S.15) according to split specification
  # - Put DWELLINGS, etc. back into dataset
  
  flog.info("Splitting S.1PT")
  
  # Find Asset/Industry combos that do not have an entry for S.1PT
  
  # Set aside TC rows to be operated on separately
  cpTC <- filter(cp, Asset == "TC")
  cp <- filter(cp, Asset != "TC")
  
  noS1Pt <- cp %>%
    mutate(isS1PT = if_else(Sector == "S.1PT", 1, 0)) %>%
    group_by(Asset, Industry) %>%
    summarise(nS1PT = sum(isS1PT)) %>%
    mutate(maxCount = max(nS1PT)) %>%
    filter(nS1PT != maxCount) %>%
    select(-maxCount, -nS1PT) %>%
    arrange(Industry)
  flog.info("Industry/Assets without S.1PT series:", as.data.frame(noS1Pt), capture = TRUE)
  
  # Remove the lower level series S.11PR, S.14, S.15 if we have S.1PT, except DWELLINGS
  # Set aside DWELLINGS and R&D
  cpDwell <- filter(cp, Asset %in% c("DWELLINGS", "RESEARCH.DEVELOPMENT") | Industry %in% c("64", "65", "66"))
  cp <- filter(cp, !(Asset %in% c("DWELLINGS", "RESEARCH.DEVELOPMENT") | Industry %in% c("64", "65", "66")))
  
  # Remove S.1PT from DWELLINGS and R&D and assume we have lower-level series (seems that
  # we do have them)
  cpDwell <- filter(cpDwell, Sector != "S.1PT")
  flog.info("Setting aside following series:", getSeries(as.data.frame(cpDwell)), capture = TRUE)
  
  # Get Industry/Asset combos where we have both S.1PT and the lower-level series
  flog.info("Removing exising lower level series S.11PR, S.14, S.15.")
  toRemove <- cp %>%
    group_by(Industry, Asset) %>%
    mutate(has_S.1PT = "S.1PT" %in% Sector) %>%
    ungroup() %>%
    filter((Sector %in% c("S.11PR", "S.14", "S.15") & has_S.1PT))
  
  # Remove lower level series
  cp <- cp %>%
    group_by(Industry, Asset) %>%
    mutate(has_S.1PT = "S.1PT" %in% Sector) %>%
    ungroup() %>%
    filter(!(Sector %in% c("S.11PR", "S.14", "S.15") & has_S.1PT)) %>% # !not
    select(-has_S.1PT)
  
  # Now we have cp without lower level S.1PT splits. We can now apply splits to
  # create new series for S.11PR, S.14, S.15
  
  # Apply Sector splits
  
  flog.info("Applying S.1PT splits.")
  
  specPath <- paste0(inputDir, "Splits/splits_sector_s1pt.csv")
  
  cp <- applySplitSpec(cp, specPath, existingCategory = "Sector", newCategory = "Split",
                       joinKeys = c("Sector", "Industry", "Asset"),tol = 0.01)
  
  # Replace DWELLINGS, etc.
  flog.info("Replacing set-aside series.")
  cp <- bind_rows(cp, cpDwell)
  
  rm(cpDwell, specPath, toRemove)
  
  flog.info(paste("BA Series count:", nrow(getSeries(cp))))
  
  # ---------------------- Split BUILDINGS ---------------------------------------
  
  # Splitting OTHER.BUILDINGS into OTHER.BUILDINGS/OTHER.STRUCTURES/LAND.IMPROVE
  # is a prerequisite to apportioning Transfer Costs since their allocation
  # proportions are partly based on the quantities in OTHER.BUILDINGS *after*
  # splitting off Transfer Costs.
  flog.info("\nSplitting OTHER.BUILDINGS.")
  
  OBsplit <- paste0(inputDir, "Splits/splits_otherbuildings.csv")
  
  cp <- applySplitSpec(cp, OBsplit, existingCategory = "Asset", newCategory = "Split",
                       joinKeys = c("Asset", "Industry", "Sector"),tol = 0.01)
  rm(OBsplit)
  flog.info(paste("BA Series count:", nrow(getSeries(cp))))
  
  # --------------- Allocate Transfer Costs to Sectors ---------------------------
  
  # See docs/transfer_costs_allocations.html for further details
  
  flog.info("\nAllocating TC to Sectors.")
  # As of 2018-01-17 there is no TC in historic, but it will be later included per
  # sector, so this step is only necessary for balanced accounts.
  
  # For TC we're creating split specifications on-the-fly rather than from an input
  # file because we want calculate the split proportions from the GFCF data.
  
  # Remove S.11PR, S.12IN, S.14 & S.15 and then sum TCs and split
  
  tc <- filter(cp, Asset=="FTC")
  tc$Sector <- "S.11PR"
  cp <- filter(cp, Asset!="FTC")
  cpTC <- filter(cpTC, Sector=="S.11001" | Sector=="S.1PT" | 
                   Sector=="S.1311" | Sector=="S.1313")
  
  # Remove terminal costs
  
  cpterm <- filter(s13_gfcf_open, Asset=="TERMINAL")
  cpterm <- gather(cpterm, Period, Value, 4:ncol(cpterm))
  cpterm$Value <- cpterm$Value*-1
  cpterm$Industry <- "68"
  cpterm$Asset <- "TC"
  
  cpTC <- rbind(cpTC, cpterm)
  
  cpTC$Sector <- "TOTAL"
  cpTC <- aggregate(Value ~ Sector+Industry+Asset+Period, data=cpTC, FUN=sum)
  
  tc_sect_spl <- read.csv(paste0(inputDir,"Splits/splits_tc.csv"))
  tc_sect_spl$Industry <- as.character(tc_sect_spl$Industry)
  cpTC$Period <- gsub("X","Y",cpTC$Period)
  tc_sect_spl <- tc_sect_spl[names(tc_sect_spl) %in% c("Sector","Industry","Asset","Split",unique(cpTC$Period))]
  cpTC <- applySplitSpec(cpTC, tc_sect_spl, existingCategory = "Sector", newCategory = "Split",
                         joinKeys = c("Sector", "Industry", "Asset"),tol = 0.01)
  
  # This splits out transfer costs - ideally this should be in GFCF delivery
  
  cpTC <- rbind(cpTC, tc)
  tc$Asset <- "TC"
  tc$Sector <- "S.11PR"
  tc$Industry <- "68"
  tc$Value <- tc$Value*-1
  cpTC <- rbind(cpTC, tc)
  cpTC <- aggregate(Value ~ Sector+Industry+Asset+Period, data=cpTC, FUN=sum)
  rm(tc)
  
  # --- Allocate S.12IN TC across S.12 Sectors
  
  # S.12IN represents all S.12 sectors. We need to spread out the TC across
  # all the S.12 sectors based on their share of DWELLINGS+OTHER.BUILDINGS
  
  # Create a sector split spec for S.12IN, e.g.:
  # Sector Industry Asset Split Y1990Q1 Y1990Q2 ...
  # S.12IN ALL      ALL   S.12FE 0.7    0.7
  # S.12IN ALL      ALL   S.12MF 0.2    0.2, etc.
  
  if (s=="MS"){
  
    secSplitS12 <- cp %>%
      filter(Asset %in% c("DWELLINGS", "OTHER.BUILDINGS")) %>%
      filter(grepl("S.12", Sector)) %>%
      group_by(Sector) %>%
      summarise(Value = sum((Value))) %>%
      mutate(Proportion = Value / sum(Value),
         Asset = "ALL") %>%
      rename(Split = Sector) %>%
      mutate(Industry = "ALL",
         Sector = "S.12IN") %>%
      select(Sector, Industry, Asset, Split, Proportion) %>%
      ungroup()
    # Repeat the Proportions for every Period in cpTC
    secSplitS12[, unique(cpTC$Period)] <- secSplitS12$Proportion
    secSplitS12$Proportion <- NULL
  
    if (nchar(ms_last_closed_quarter)==7 | nchar(xms_last_closed_quarter)==7){
    
      secSplitS12 <- readRDS(paste0(inputDir,"/Parameters, Assumptions & Adjustments/prev_s12split.rds"))
  
    }
  
  # Apply the sector split for the TC data
  cpTC <- applySplitSpec(cpTC, secSplitS12, existingCategory = "Sector", newCategory = "Split",
                         joinKeys = c("Sector", "Industry", "Asset"),tol = 0.01)
  cpTC <- filter(cpTC, Sector!="S.12")
  #cpTC$Asset <- "LAND.IMPROVEMENTS.TC"
  #rm(secSplitS12)
  
  }
  
  coverage_diff <- unique(cpTC$Period)[!unique(cpTC$Period) %in% unique(cp$Period)]
  for (cd in coverage_diff){
    
    cpTC <- filter(cpTC, Period!=cd)
    
  }
  
  # Remove TC from cp and replace with split version.
  
  cp <- bind_rows(cp, cpTC)
  rm(cpTC)
  
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
  # - Aggregate to A64
  # - Split OTHER.BUILDINGS
  
  flog.info(paste("Historic Series count:", nrow(getSeries(historic))))
  # We only need up to the linkPeriod (but not including)
  periods <- sort(unique(historic$Period))
  lastHistoricPeriod <- periods[which(periods == linkPeriod) - 1]
  historic <- filter(historic, Period <= lastHistoricPeriod)
  
  # Aggregate to A64
  flog.info("Aggregating to A64.")
  # Read Industry Hierarchy
  sic <- read_excel(paste0(inputDir, "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"),
                    sheet = "Industry", col_types = "text")
  # Format industry codes (e.g. "1" => "01")
  sic <- as.data.frame(lapply(sic, prepim::formatIndustryCodes), stringsAsFactors = FALSE)
  # Aggregate
  historic <- aggregateWithHierarchyTable(.data = historic,
                                          column = "Industry",
                                          hierarchyTable = sic, targetLevel = "A64",
                                          values = "Value",
                                          keepOriginal = FALSE)
  rm(sic)
  flog.info(paste("Historic Series count:", nrow(getSeries(historic))))
  
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
    #write_excel_csv(BA_not_Historic, paste0(otherOutputs, "coverage_check_BA_not_Historic_", runTime, ".csv"))
  } else {
    #write_excel_csv(BA_not_Historic, paste0(outputDir, "coverage_check_BA_not_Historic_", runTime, ".csv"))
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
    #write_excel_csv(Historic_not_BA_series, paste0(otherOutputs, "coverage_check_Historic_not_BA_", runTime, ".csv"))
  } else {
    #write_excel_csv(Historic_not_BA_series, paste0(outputDir, "coverage_check_Historic_not_BA_", runTime, ".csv"))
  }
  
  # --- Combine Historic and BA into "gfcf" dataset
  flog.info("\nCombining BA with historic.")
  # Combining balacc/historic CP series is simply a case of appending one to the other
  cp <- addHistoricCp(
    historicCp = historic,
    baCp = cp,
    linkPeriod = linkPeriod)
  
  cp$Prices <- NULL
  
  flog.info(paste("Combined Series count:", nrow(getSeries(cp))))
  
  # ---------------------- Further TC Allocations --------------------------------
  
  # Complete the allocation of TC (after historic and BA have been combined). See
  # docs/transfer_costs_allocations.html for details.
  
  # --- Allocate Industry 68 TC across Industries
  
  # Separate off the TC rows
  cpTC <- filter(cp, Asset == "TC")
  
  # All TC is currently in Industry 68 which represents the total for each Sector
  # We need to distribute these sector totals across Industries based on their
  # share of DWELLINGS and OTHER.BUILDINGS assets
  
  # Create an industry split spec for Industry 68, e.g.:
  # Sector Industry Asset Split Y1990Q1 Y1990Q2 ...
  # S.11PR 68       ALL   01    0.1    0.1
  # S.11PR 68       ALL   02    0.2    0.2, etc.
  
  flog.info("Using proportion of DWELLINGS/OTHER.BUILDINGS to distribute TC to Industries")
    
  # Check for negative stocks (these will not be used to calculate proportions)
  
  negSeries <- cpTC %>%
    filter(Asset %in% c("DWELLINGS", "OTHER.BUILDINGS")) %>%
    filter(Value < 0) %>%
    distinct(Sector, Industry, Asset)
  if (nrow(negSeries) > 0) flog.warn("Following series contain negative stocks:", as.data.frame(negSeries), capture = TRUE)
  rm(negSeries)
  
  # Create split spec
  modcp <- cp
  modcp$Industry <- ifelse(modcp$Sector=="S.1311" | modcp$Sector=="S.1313", "68", modcp$Industry)
  indSplit <- modcp %>%
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
  indSplit[, unique(cpTC$Period)] <- indSplit$Proportion
  indSplit$Proportion <- NULL
  
  # indSplit changes in closed period, as fixed proportion is calculated from entire dataset
  # Using fixed split from BB20 closed - need to investigate the best way to address this 
  # Is fixed proportion credible assumption and would dynamic splits be too volatile
  
  if (nchar(ms_last_closed_quarter)==7 | nchar(xms_last_closed_quarter)==7){
    
    indSplit <- readRDS(paste0(inputDir,"/Parameters, Assumptions & Adjustments/prev_indSplit.Rds"))
    indSplit$Split <- as.character(indSplit$Split)
    indSplit$Split <- ifelse(nchar(indSplit$Split)==1,paste0("0",indSplit$Split),indSplit$Split)
    
  }
  
  indSplit$Split <- ifelse(indSplit$Split=="05_09", "08", indSplit$Split)
  
  # Apply the industry split for the TC data
  cpTC <- applySplitSpec(cpTC, spec = indSplit, existingCategory = "Industry", newCategory = "Split",
                         joinKeys = c("Sector", "Industry", "Asset"),tol = 0.01)
  
  cpTC$Asset <- "LAND.IMPROVEMENTS.TC"
  cp <- filter(cp, Asset != "TC")
  cp <- bind_rows(cp, cpTC)
  
  rm(modcp)
  
  if (s=="NPISH"){
    
    s13_TC <- filter(cp, Sector=="S.1311" | Sector=="S.1313")
    s13_TC <- filter(s13_TC, Asset=="LAND.IMPROVEMENTS.TC" | Asset=="TERMINAL")
  
    cp <- filter(cp, Sector=="S.1311" | Sector=="S.1313" | Sector=="S.15")
    
  } else {
    
    cp <- filter(cp, Sector!="S.1311" & Sector!="S.1313" & Sector!="S.15")
    
  }
  
  if (s=="NPISH"){
    
    cp_s15 <- cp
    rm(cp)
    
  } else {
    
    cp <- rbind(cp, cp_s15)
    
  }
  
}

cp$Sector <- ifelse(cp$Asset=="FTC", "S.11PR", cp$Sector)

# Remove if asset == TOTAL

cp <- filter(cp, Asset!="TOTAL")

#rm(cp_s15, i, s, last_closed_quarter)

flog.info("Extracting Historic Deflators.")
defHist <- extractHistoricDeflators(paste0(inputDir, "Historic Input Data/deflators_hist.csv"))

flog.info("Extracting 1997+ Deflators.")
# RefYear == 100. Will divide by 100 later

defOpen <- deflators_open
defClosed <- read.csv(paste0(last_run, "/Outputs/deflators_ba.csv"))
defClosed$Prices <- NULL

# ===================== CHECKING THE INPUT DATA ================================

#check_duplicated(cp, "Current Input Data/gfcf_r42_ba.xlsx")
check_duplicated(historic, "Historic Input Data/gfcf_hist_r42.csv")
check_duplicated(defHist, "Historic Input Data/deflators_hist.csv")

###########################   Finish working through  ###################

check_duplicated(defOpen, "Deflators/deflators_ba.csv")

if (exists("defClosed")){
  
  check_duplicated(defClosed, "Deflators/deflators_ba_closed.csv")
  
}

# Function to check if there are any negative values in the data
check_negative <- function (data, filename) {
  if (nrow(data[rowSums(data < 0) > 0,]) > 0) {
    msg <- paste0("Negative values in ", filename)
    negative <- data[rowSums(data < 0)>0,]
    flog.fatal(paste(msg, "series:"), data.frame(negative), capture = TRUE)
    stop(msg)
  }
}

check_negative(defHist, "Historic Input Data/deflators_hist.csv")

check_negative(defOpen, "Deflators/deflators_ba.csv")
  
# ===================== FORECAST GFCF AND DEFLATORS ============================

# This is a forecasting function, which can forecast recent deflators
# and recent GFCF for a specified number of quarters. 
# The number of quarters to be forecasted is specified in "run_parameters.csv"
# If the numbers are set to 0, no extra quarters will be forecasted and this section
# will be skipped.
GFCF_FORECAST <- FALSE
DEFLATORS_FORECAST <- FALSE

# Calculate forecast period from set date as GFCF from R4.2 and S13 may not be to the same data

# Forecast period set to zero as this is not currently functioning. Using Leigh's work around. Guess best solution might
# be to use ARIMA where possible and use crude methodology for volatile series?

# gfcf_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(cp$Period),1),2,5)))*4 + 
#                          as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(cp$Period),1),7,7)))

# GFCF forecasting does not work as it cannot deal with such noisy series

gfcf_forecastPeriod <- 0

deflators_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(defOpen$Period),1),2,5)))*4 +
                               as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(defOpen$Period),1),7,7)))

gfcf_forecastPeriod <- ((as.numeric(substr(forecastTo,2,5)) - as.numeric(substr(tail(unique(cp$Period),1),2,5)))*4 + 
                          as.numeric(substr(forecastTo,7,7)) - as.numeric(substr(tail(unique(cp$Period),1),7,7)))

if (gfcf_forecastPeriod > 0) {
  GFCF_FORECAST <- TRUE
}

if (deflators_forecastPeriod > 0) {
  DEFLATORS_FORECAST <- TRUE
}

######################### FORECASTING NOT WORKING, SO TAKING THE AVERAGE GROWTH OF THE LAST 5 QUARTERS AND ADDING IT ON TO THE NEW QUARTER. CAN ALSO BE NEGATIVE

forecastByGrowth = TRUE

if (forecastByGrowth == TRUE){
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
  
  # Changed to forecast periods as set in parameters
  
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
  cp <- cp %>% gather_(key = "Period", value = "Value", names(cp[4:ncol(cp)]))
  }
}
###################################################################################################################################

# ARIMA forecasting not working- so set to zero

gfcf_forecastPeriod <- 0

###################################################################################################################################

forecasting <- function (forecastPeriod, extractedData) {
  
  if (forecastPeriod > 0)
  {
    # Undoing the tidyr:;gather that was done using extractBalAccountsCord/extractHistoricDeflators
    extractedData_spread <- extractedData %>% tidyr::spread (key = Period, value = Value)
    
    # Figuring out what is the last available period in given data
    endPeriod <- colnames(extractedData_spread)[ncol(extractedData_spread)]
    
    #Splitting the endPeriod string to get Year and Quarter from the Y****Q* format
    C <- strsplit (endPeriod, 'Q')
    Year <- C[[1]][1]
    Year <- as.numeric(str_sub(Year, start = -4))
    Quarter <- as.numeric(C[[1]][2])
    
    # removing Sector, Indursty, Asset and Prices since they're not used in the forecast
    valuesOnly <- within(extractedData_spread, rm("Sector","Industry", "Asset"))
    
    # Creating an empty matrix to store results in
    results <- matrix (ncol = forecastPeriod, nrow = nrow(valuesOnly))
    
    # Loop to calculate the forecasted value for each row of code
    for (i in 1:nrow(valuesOnly)) {
      
      # Taking one row of data and storing it temporarily
      temp <- valuesOnly[i,]
      # Transpose of temp
      temp <- t(temp)
      
      # The following loop takes care of rows which have all 0's in them, since otherwise ARIMA breaks
      if(sum((temp==0)*1)==nrow(temp)) {
        results[i,]<-0
        i<-i+1
      } else { 
        
        # Changing the temp file so that row names (for each period) would instead be 
        # recognized as a column and giving it a name "Period"
        temp<-rownames_to_column(as.data.frame(temp), var="Period")
        
        # Renaming the other column as well to "Value"
        colnames(temp)[2]<-'Value'
        
        # Specifying that this is time series
        temp$Value <-ts(temp$Value, start=c(1997,1), end=c(Year,Quarter), frequency = 4)
        
        # Getting the differences between each period in time series which is what
        # is used by ARIMA
        temp2 <- diff(temp$Value)
        
        # Training the ARIMA model
        # fit_diff <- arima(temp2, order=c(1,0,0), method="ML")
        fit_diff <- tryCatch({arima(temp2, order=c(1,0,0))}, error=function(e){
          message("\nThe following row has some unusual values:")
          message("Sector: ", extractedData_spread[i,1])
          message("Industry: ", extractedData_spread[i,2])
          message("Asset: ", extractedData_spread[i,3])
          message("Error message: ", e)
          message(" ")
          return(NA)
          })
        
        # Using fit_diff for forecasting:
        # h specifies how many quarters you want to be forecasted
        fit_diff_f <- forecast(fit_diff, h=forecastPeriod)
        
        # Getting the individual forecasts:
        predicted <- as.numeric(fit_diff_f$mean)
        
        # Storing the forecasted result, rounded up to 5 decimal points
        results[i,] <- round(predicted, digits = 5)
      }
      
    }
    
    for (forecast_num in 1:forecastPeriod)
    {
      # Getting the last row from the data
      last_row <- valuesOnly[,ncol(valuesOnly)]
      
      # To get the actual result, the prediction needs to be added to the last column of data
      # since the prediction is only for the change when compared to the last quarter
      ind_result <- as.data.frame(last_row + results[,forecast_num])
      
      Quarter<-Quarter+1
      if (Quarter>4){
        Year<-Year+1
        Quarter<-1
      }
      
      # Creating a string to name the forecasted period
      newname <- paste('Y', Year, 'Q', Quarter, sep='')
      
      # Renaming the predicted row
      colnames(ind_result)[1]<-newname
      
      # Adding the predicted row to the existing data and replacing the data with an updated set
      extractedData_spread <- cbind(extractedData_spread, ind_result)
      
      forecast_num <- forecast_num + 1
      
    }
    
    # Rearranging the data so that it would again be in the same format as it was previously
    # so that it could be used in further calculations in Capital Stocks R Script 
    extractedData <- extractedData_spread %>% tidyr::gather (key = Period, value = Value, -Industry, -Asset, -Sector)
  
  }
  
  return(extractedData)
  
}

# Using the function to forecast GFCF and deflators:
# try(if(error_count>0) stop("Values must be fixed before forecasting."))
# Forecasting deflators:

defOpen <- forecasting (deflators_forecastPeriod, defOpen)

# Forecasting GFCF:
cp <- forecasting (gfcf_forecastPeriod, cp)

# --- Split TC into Asset sub-types: TC.LAND.DWEL and TC.LAND.OB

# Apportion TC across new asset types of TC.LAND.DWEL and TC.LAND.OB based on
# their ratio of DWELLINGS vs. OTHER.BUILDINGS

# - TC.LAND.UDWEL based on the proportions of DWELLINGS
# - TC.LAND.OB based on the proportions OTHER.BUILDINGS

# Create an asset split spec for Asset TC, e.g.:
# Sector Industry Asset Split       Y1990Q1 Y1990Q2 ...
# S.11PR 68       TC   TC.LAND.DWEL 0.2    0.2
# S.11PR 68       TC   TC.LAND.OB   0.8    0.8, etc.
#flog.info("Using proportion of DWELLINGS/OTHER.BUILDINGS to distribute TC to TC.LAND.DWEL/TC.LAND.OB")
#assetSplit <- gfcf %>%
#  filter(Asset %in% c("DWELLINGS", "OTHER.BUILDINGS")) %>%
#  group_by(Sector, Industry, Asset) %>%
#  filter(Value > 0) %>%  # In case of negative stocks, remove them from apportionment
#  summarise(Value = sum(Value)) %>%
#  mutate(Proportion = Value / sum(Value)) %>%
  # Create the asset splits
#  mutate(Split = if_else(Asset == "DWELLINGS", "DWELL.TC",
#                         if_else(Asset == "OTHER.BUILDINGS", "LAND.IMPROVEMENTS.TC",
#                                 "Error"))) %>%
#  mutate(Asset = "TC") %>%
#  select(Sector, Industry, Asset, Split, Proportion) %>%
#  ungroup()
# Repeat the Proportions for every Period in cpTC
#assetSplit[, unique(cpTC$Period)] <- assetSplit$Proportion
#assetSplit$Proportion <- NULL

# Apply the industry split for the TC data
#cpTC <- applySplitSpec(cpTC, spec = assetSplit, existingCategory = "Asset", newCategory = "Split",
#                       joinKeys = c("Sector", "Industry", "Asset"))


# Remove unsplit TC from gfcf and replace with split version.
#gfcf <- filter(gfcf, Asset != "TC")
#gfcf <- bind_rows(gfcf, cpTC)
#rm(cpTC)

#flog.info(paste("Combined Series count:", nrow(getSeries(gfcf))))


# ---------------------- Split MACHINERY ---------------------------------------

 # Note we may need to add deflators/lifelengths/configs for new Asset types
 # Will also need to be reflected in asset hierarchy. Until this is done this
 # section is commented out.

gfcf <- cp
 
flog.info("\nSplitting OTHER.MACHINERY.")

specPath <- paste0(inputDir, "Splits/splits_othermachinery.csv")

gfcf <- applySplitSpec(gfcf, specPath, existingCategory = "Asset", newCategory = "Split",
                joinKeys = c("Asset", "Sector", "Industry"),tol = 0.01)

# Short-term work around for missing coverage - set to entirely OTHER.MACHINERY.M

gfcf$Asset <- ifelse(gfcf$Asset=="OTHER.MACHINERY", "OTHER.MACHINERY.M", gfcf$Asset)
  
rm(specPath)
flog.info(paste("BA Series count:", nrow(getSeries(cp))))

# ---------------------- Remove Series -----------------------------------------

# Filter out S.13 series. These should not be present as they are imported separately
s13series <- filter(gfcf, grepl("^S.13", Sector))
if (nrow(s13series) > 0) {
  flog.warn("Removing S.13 series. These should be imported separately.",
            getSeries(s13series), capture = TRUE)
  gfcf <- anti_join(gfcf, s13series, by = c("Sector", "Industry", "Asset"))
}
rm(s13series)

# Removing WEAPONS, we currently have no Asset Lives
flog.info("Removing WEAPONS series.")
gfcf <- filter(gfcf, Asset != "WEAPONS")

flog.info(paste("Combined Series count:", nrow(getSeries(gfcf))))

# --------------------- Add Zero Values ----------------------------------------

# Take the combined cp data and fill out series with zeros if they finish early
# The PIM calculates stocks where we have periods for a series, so if we want
# outputs when GFCF has finished we must provide zero values. This will fill zeros
# *before* as well as *after*, but we'll remove the before-zeros later
gfcf <- complete(gfcf, Period, nesting(Sector, Industry, Asset), fill = list(Value = 0))

# ---------------------- Add Price Index ---------------------------------------

# Rename the Value column to gfcfCP and remove Prices
gfcf <- gfcf %>%
  rename(gfcfCP = Value)

# If last_closed_quarter is set (hence is 7 characters long), link closed period deflators to open period deflators 
# in addition to the historic deflators

for (i in c("NPISH", "MS")){
  
  if (i=="NPISH"){
    
    last_closed_quarter <- xms_last_closed_quarter
    
  } else {
    
    last_closed_quarter <- ms_last_closed_quarter
    
  }

  if (nchar(last_closed_quarter)!=7){
    
    # Expand historic deflators to match gfcf coverage
    defH <- expandSpec(defHist, toCover = gfcf, joinKeys = c("Asset", "Industry", "Sector"))
    # Expand recent deflators to match historic coverage
    defO <- expandSpec(defOpen, toCover = defH, joinKeys = c("Asset", "Industry", "Sector"))
    
    # Link historic and recent deflators. We must have one overlapping period at the linkPeriod
    defAll <- linkDeflators(defH, defO, linkPeriod)
    defAll <- mutate(defAll, Value = Value / 100)  # Change deflator to be 1 at refPeriod (instead of 100)
    defAll$Prices <- NULL
    
  }
  
  if (nchar(last_closed_quarter)==7){
    
    # Expand recent deflators to match historic coverage
  
    defO <- expandSpec(defOpen, toCover = gfcf, joinKeys = c("Asset", "Industry", "Sector"))
    defO <- mutate(defO, Value = Value / 100)  # Change deflator to be 1 at refPeriod (instead of 100)
    defO <- spread(defO, Period, Value)
    defO$Prices <- NULL
  
    # Cannot take on revisions to deflators prior to reference year without affecting 
    # constrained estimates. So use later of refPeriod or last_closed_quater
  
    def_join <- ifelse(substr(refPeriod,2,5)>=substr(last_closed_quarter,2,5),
                     refPeriod,last_closed_quarter)
  
    h_columns <- colnames(defO[1:3])
    d_columns <- colnames(defO[4:ncol(defO)])
    hist_columns <- unique(defHist$Period)
    hist_columns <- hist_columns[1:676]
    open_quarters <- d_columns[substr(d_columns,2,5)>substr(def_join,2,5) |
                               substr(d_columns,2,5)==substr(def_join,2,5) &
                               substr(d_columns,7,7) >= substr(def_join,7,7)]
    defO <- defO[c(h_columns, open_quarters)]
    colnames(defO)[colnames(defO)==def_join] <- "splicing_ratio"
    closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], def_join)
    defC <- defClosed[c(h_columns, hist_columns, closed_quarters)]
  
    # Merge open and closed datasets
  
    # Calculate splicing ratio by dividing closed period gfcf by open period gfcf in the last closed quarter.
    # Then multiply open period gfcf after the closed quarter by the splicing ratio
  
    defAll <- left_join(defC, defO)
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
  
    #rm(defOpen, defClosed)
    
  }
  
  if (i=="NPISH"){
    
    defAll <- filter(defAll, Sector=="S.15")
    
  } else {
    
    defAll <- filter(defAll, Sector!="S.15")
    
  }
  
  if (i=="NPISH"){
    
    s15_defAll <- defAll
    rm(defAll)
    
  } else {
    
    defAll <- rbind(defAll, s15_defAll)
    
  }
  
}

defsave <- defAll
defsave$Prices <- "def"
defsave <- spread(defsave, Period, Value)
#write.csv(defsave, paste0(outputDir,"/deflators_ba.csv"), row.names=FALSE)

# Write out constrained deflators (would be needed as closed period deflators if multiple closed periods)
gfcf <- gfcf%>%filter(Period%in%unique(defAll$Period)&Sector%in%unique(defAll$Sector))

gfcf <- addPriceIndex(gfcf, defAll)
#gfcf <- select(gfcf, -Prices)  # remove Prices column added by addPriceIndex

# --------------------- Final GFCF processing ----------------------------------

flog.info("Removing leading zeros from GFCF series.")
gfcf <- gfcf %>%
  group_by(Sector, Industry, Asset) %>%
  # Strip out leading zeros (no need to process these)
  slice(pmin(which(Period == refPeriod), which.max(gfcfCP != 0)):n()) %>%
  ungroup()

# Old version, which lead to rows of data being deleted if there was no data up to refYear:
# flog.info("Removing leading zeros from GFCF series.")
# gfcf <- gfcf %>%
#   group_by(Sector, Industry, Asset) %>%
#   # Strip out leading zeros (no need to process these)
#   slice(which.max(gfcfCP != 0):n()) %>%
#   ungroup()

# ============================ OUTPUT DATA =====================================

# if (WRITE_FILES) #write_rds(gfcf, paste0(outputDir, "prepared_gfcf_and_defs", runTime, ".Rds"))


# ============================ REMOVE OBJECTS ==================================

# Remove larger datasets from memory (if we're going straight on to running PIM)
rm(cp, BA_not_Historic, defHist, historic, Historic_not_BA, Historic_not_BA_series,
   defAll, noS1Pt, indSplit, periods, lastHistoricPeriod, gfcf_forecastPeriod, cpTC)

# Can take a subset of gfcf file so that the run would be quicker
#gfcf<-gfcf[which(gfcf$Industry=="02" & gfcf$Sector=="S.11001"),]