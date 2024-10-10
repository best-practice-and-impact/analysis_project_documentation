####################### ADD PIM PARAMETERS #####################################

# Take an existing gfcf data.frame and:
# Add Life Lengths
# Add K-value Adjustments
# Add Other Parameters (Tax, CPI, UTIL)
# Nest series by Sector/Industry/Asset label
# Add per-series PIM configurations

# Check we have gfcf dataframe
# We may want to load one from a previous run, e.g.
# gfcf <- read_rds("prepared_gfcf_and_deflators<runtime>.Rds")
if (!exists("gfcf")) stop("No prepared gfcf data.frame present. Did you run the previous script?")


# ---------------------- Add Life Lengths --------------------------------------

gfcf <- addLifeLengths(gfcf, paste0(inputDir, "Parameters, Assumptions & Adjustments/asset_lives.xlsx"))
gfcf$Average <- as.numeric(gfcf$Average)
gfcf$CoV <- as.numeric(gfcf$CoV)
gfcf$Max <- as.numeric(gfcf$Max)
gfcf$Min <- as.numeric(gfcf$Min)

# ---------------------- Add Adjustments ---------------------------------------

# Add Adjustments (K-values)
flog.info("Adding Adjustments K1 - K6.")
adjustments <- extractAdjustments(paste0(inputDir, "Parameters, Assumptions & Adjustments/adjustments.csv"))
shared_inputs_adjustments <- adjustments
gfcf <- left_join(gfcf, adjustments, by = c("Sector", "Industry", "Asset", "Period"))
# Fill out K-values with zeros
naToZero <- function(x) dplyr::if_else(is.na(x), 0, x)
gfcf <- mutate_at(gfcf, .cols = c("K1CP", "K3CP", "K4CP", "K5CP", "K61CP", "K62CP"),
                  .funs = naToZero)


# ---------------------- Add Other Parameters ----------------------------------

tax_util <- read.csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/series_tax_util_cpi.csv"))
tax_util <- gather_(tax_util, "Period", "Value", names(tax_util[2:ncol(tax_util)]))
tax_util <- spread(tax_util, Adjustment, Value)
shared_inputs_util <- tax_util
write.csv(tax_util, file= paste0(inputDir, "Parameters, Assumptions & Adjustments/series_tax_util_cpi_reshape.csv"), row.names=FALSE)

gfcf <- addOtherParams(gfcf, paste0(inputDir, "Parameters, Assumptions & Adjustments/series_tax_util_cpi_reshape.csv"))

rm(tax_util)
file.remove(paste0(inputDir, "Parameters, Assumptions & Adjustments/series_tax_util_cpi_reshape.csv"))

#################################################################################################################################################################
########################################## BAU FOUND THAT GOVERNMENT WERE SPLITTING OUT SOME COFOGS DIFFERENTLY TO THEM #########################################
################################################# THIS SECTION SPLITS OUT TO MATCH GOVERNMENT DEPARTMENT SPLITS #################################################

# Splitting is now done after the PIM has run

doMoreSplitting <- FALSE
if (doMoreSplitting)
{
  flog.info("Starting to split data, if any!")
  indSplit <- read_excel(paste0(inputDir, "Splits/split_COFOG.xlsx"), sheet = "split_COFOG") # PULL IN THE SPLITS FROM THE SPREADSHEET
  splitCols <- c(names(indSplit[4:ncol(indSplit)])) # GET THE COLUMN NAMES NEEDED FOR GATHERING UP BY PERIOD
  # z <- 1
  for (z in 1:nrow(indSplit)) # LOOP AROUND EACH SPLIT
  {
    indSplitRow <- indSplit[z,] # ASSIGN THE CURRENT SPLIT LINE TO A VARIABLE SO IT CAN BE GATHERED UP
    flog.info(paste0("Splitting Sector:", indSplitRow$Sector, ", From Industry ", indSplitRow$fromIndustry , " To Industry: ", indSplitRow$toIndustry))
    
    oldData <- gfcf %>% filter(Sector == indSplitRow$Sector & Industry == indSplitRow$fromIndustry) # PULL OUT ALL THE DATA FROM OUT DATASET THAT MATCHES THE SECTOR AND INDUSTRY
    oldData[is.na(oldData)] <- 0 # TAKE CARE OF ANY NAs IN THE DATA
    if (nrow(oldData) > 0) # MAKE SURE THERE IS DATA TO PROCESS
    {
      indSplitRow <- indSplitRow %>% gather_(key = "Period", value = "Perc", splitCols) # GATHER UP THE DATA BY PERIOD
      oldData <- sqldf("SELECT a.*, COALESCE(b.Perc,1) AS Perc FROM oldData a LEFT JOIN indSplitRow b ON a.Period = b.Period") # JOIN THE DATA BY PERIOD, GIVING A 1 TO ANY NON MATCHES
      oldData$gfcfCP <- as.double(oldData$gfcfCP) # NEED TO CONVERT VALUE COLUMN TO A NUMERIC
      oldData$PriceIndex <- as.double(oldData$PriceIndex) # NEED TO CONVERT VALUE COLUMN TO A NUMERIC
      oldData$Perc <- as.double(oldData$Perc) # NEED TO CONVERT VALUE COLUMN TO A NUMERIC
      oldInd <- oldData # ASSIGN DATA TO A NEW DATASET FOR ONE SPLIT
      newInd <- oldData # ASSIGN DATA TO A NEW DATASET FOR THE OTHER SPLIT
      #oldInd$PriceIndex <- oldInd$PriceIndex * as.double(as.double(1) - oldInd$Perc) # DO THE FIRST SPLIT, WHICH IS 1 MINUS SPLIT VALUE
      #newInd$PriceIndex <- newInd$PriceIndex * newInd$Perc # DO THE SECOND SPLIT, WHICH IS THE MAIN SPLIT
      oldInd$gfcfCP <- oldInd$gfcfCP * as.double(as.double(1) - oldInd$Perc) # DO THE FIRST SPLIT, WHICH IS 1 MINUS SPLIT VALUE
      newInd$gfcfCP <- newInd$gfcfCP * newInd$Perc # DO THE SECOND SPLIT, WHICH IS THE MAIN SPLIT
      newInd$Industry <- as.character(indSplitRow[1,3]) # CHANGE THE INDUSTRY OF THE NEW DATA
      oldInd$Perc <- NULL # DELETE THE PERC COLUMN
      newInd$Perc <- NULL # DELETE THE PERC COLUMN
      indSplitRow <- indSplit[z,] # NEED THE ORIGINAL SPLIT DATA IN THE COLUMN FORMAT, SO JUST ASSIGN IT FROM THE ORIGINAL AGAIN
      gfcf <- gfcf %>% filter(Sector != indSplitRow$Sector | Industry != indSplitRow$fromIndustry) # REMOVE THE ORIGINAL SECTOR AND INDUSTRY DATA FROM THE OUT DATASET
      gfcf <- rbind(gfcf, oldInd, newInd) # ADD THE TWO NEW SPLIT DATASETS BACK INTO THE OUT DATASET
    }
    
    rm(oldData, oldInd, newInd, indSplitRow) # CLEAN UP SOME VARIABLES
  }
  rm(indSplit, splitCols, z) # CLEAN UP SOME VARIABLES
  flog.info("Finished Splitting AND Nesting Data")
  # WHATEVER THE NEW INDUSTRY IS (RcapStocks01 etc.) IT NEEDS TO BE ADDED TO AN AGGREGATION IN THE SPREADSHEET hierarchies_sector_industry_asset.xlsx
}
#################################################################################################################################################################
#################################################################################################################################################################

#   Set-aside Terminal costs as PIM struggles with immediate consumption of capital

terminal <- filter(gfcf, Asset=="TERMINAL")
gfcf <- filter(gfcf, Asset!="TERMINAL")

# -------------------- Nest Series Ready for PIM -------------------------------

inputData <- gfcf %>%
  rename(Vintage = Period) %>%   # PIM refers to Periods as "Vintage"
  group_by(Sector, Industry, Asset) %>%
  # For each series, roll-up all columns into a single list-column called "data"
  nest()

# ---------------------- Add Configurations ------------------------------------

flog.info("\nAdding PIM configurations to each series.")
configs <- extractConfigSpec(paste0(inputDir, "Parameters, Assumptions & Adjustments/configurations.csv"))
shared_inputs_configs <- configs
# Expand "ALL" in configs to match inputData coverage
configs <- expandConfigSpec(configs, toCover = inputData, joinKeys = c("Asset", "Industry", "Sector"))
# Create config objects and put them into a single list-column
configs <- configs %>%
  group_by(Sector, Industry, Asset) %>%
  do(config = do.call(pimir::pimConfig, select(., -Sector, -Industry, -Asset, -Notes)))
# Join data and configs for each series
inputData <- inputData %>% left_join(configs, by = c("Sector", "Industry", "Asset"))
rm(configs)

# ------------------------ Add Reference Year ----------------------------------
# Set the reference year (for calculation of Capital Services only)
# Some stocks don't last until reference year so just use their max period
flog.info("\nAdding Reference Year.")
refYear <- params$refPeriod

# Check if any series do not have observations in the refPeriod (e.g. they started afterwards)
# Remember we stripped out leading zeros
maxYears <- lapply(inputData$data, FUN = function(x) {
  leRefYear <- x$Vintage <= refYear
  suppressWarnings(max(x$Vintage[leRefYear]))
})
maxYears <- unlist(maxYears)
inputData$refYear <- maxYears
# Remove any series without data in refYear
noData <- inputData[is.na(inputData$refYear), ]
if (nrow(noData) > 0) flog.info("Removing series with no data in refPeriod:",
                                getSeries(noData), capture = TRUE)
inputData <- filter(inputData, !is.na(refYear))
flog.info(paste("Series count:", nrow(getSeries(inputData))))

# ============================ OUTPUT DATA =====================================

# Save the unnested gfcf data (containing all series and parameters)
if (OUTPUT_SHARING == TRUE) {
  write_csv(gfcf, paste0(otherOutputs, "prepared_gfcf_and_defs_and_parameters_", runTime, ".csv"))
} else {
    write_csv(gfcf, paste0(outputDir, "prepared_gfcf_and_defs_and_parameters_", runTime, ".csv"))
}

### Create new folder and write .xlsx for use with
### published capital stocks code

if (publishTables==T){
  
  dir.create(file.path(outputDir, "Pub_tables"))
  
  shared_inputs_params$outputDir <- ""
  
  mean_life_lengths <- read_xlsx(path = paste0(inputDir, "Parameters, Assumptions & Adjustments/asset_lives.xlsx"), sheet = 'AverageLifeLengths')
  mean_life_lengths[, c(4:ncol(mean_life_lengths))] <- sapply(mean_life_lengths[, c(4:ncol(mean_life_lengths))], as.numeric)
  min_life_lengths <- read_xlsx(path = paste0(inputDir, "Parameters, Assumptions & Adjustments/asset_lives.xlsx"), sheet = 'Min')
  min_life_lengths[, c(4:ncol(min_life_lengths))] <- sapply(min_life_lengths[, c(4:ncol(min_life_lengths))], as.numeric)
  max_life_lengths <- read_xlsx(path = paste0(inputDir, "Parameters, Assumptions & Adjustments/asset_lives.xlsx"), sheet = 'Max')
  max_life_lengths[, c(4:ncol(max_life_lengths))] <- sapply(max_life_lengths[, c(4:ncol(max_life_lengths))], as.numeric)
  cov <- read_xlsx(path = paste0(inputDir, "Parameters, Assumptions & Adjustments/asset_lives.xlsx"), sheet = 'CoVs')
  cov[, c(4:ncol(cov))] <- sapply(cov[, c(4:ncol(cov))], as.numeric)
  reclassifications <- read.csv(paste0(inputDir, "Parameters, Assumptions & Adjustments/reclassifications.csv"))
  
  asset_agg <- read.csv(paste0(inputDir, "Mapping & Aggregation/PIM_input_agg.csv"))
  asset_agg$concat <- paste0(asset_agg$Industry,asset_agg$Asset)
  
  # Aggregate relevant assets
  
  PIM_I_agg <- function (df = df, series = comb) {
    
    #df <- shared_inputs_gfcf
    #series <- "gfcf"
    
    if (series!="configs"){

      
      df$concat <- paste0(df$Industry,df$Asset)
      df_agg <- df %>% filter(concat %in% asset_agg$concat)
      df <- df %>% filter(!concat %in% asset_agg$concat)
      
      if (series=="life_lengths"){
        
        df <- df %>% filter(!concat %in% asset_agg$concat)
        
      }
      
      df$concat <- NULL
      df <- df %>% filter(Asset!="WEAPONS")
      
      # Replace asset with asset aggregation
      
      df_agg <- left_join(df_agg, asset_agg)
      df_agg$Asset <- df_agg$Asset_aggregation
      df_agg <- df_agg %>% select(-concat, -Asset_aggregation)
      
    } else {
      
      df_agg <- df %>% filter(Asset %in% asset_agg$Asset)
      df <- df %>% filter(Asset!="WEAPONS")
      asset_agg_config <- asset_agg %>% select(-Industry, -concat)
      df_agg <- left_join(df_agg, asset_agg_config)
      df_agg$Asset <- df_agg$Asset_aggregation
      df_agg <- df_agg %>% select(-Asset_aggregation)
      df_agg <- aggregate(. ~ Sector+Industry+Asset+Notes+profileType+
                            profileFunctionName+retirementDistName+
                            rightTruncate+combinationMethod, 
                          data=df_agg,FUN=mean)
      
    }
    
    # Aggregate series
    
    if (series=="gfcf"){
      
      df_agg[is.na(df_agg)] <- 0
      df_agg <- aggregate(. ~ Sector+Industry+Asset, data=df_agg,FUN=sum)
      
    }
    
    if (series=="life_lengths"){
      
      df_agg <- aggregate(. ~ Sector+Industry+Asset, data=df_agg,FUN=mean)
      
    }
    
    if (series=="deflators"){
        
        df_agg <- gather(df_agg, Period, deflator, 4:ncol(df_agg))
        df_agg <- df_agg %>% group_by(Sector,Industry,Asset) %>%
                            fill(deflator) %>%
                            ungroup()
        df_agg[is.na(df_agg)] <- 0
        df_agg <- aggregate(deflator ~ Sector+Industry+Asset+Period, data = df_agg, FUN = mean)
        df_agg <- spread(df_agg, Period, deflator)
      
    }
    
    df <- rbind(df, df_agg)
    
    return(df)
    
  }
  
  remove_forecasted_years <- function(df, not_year, publishTo){
    year <- names(df)
    year <- year[!(year %in% not_year)]
    year <- substr(year, 2,7)
    end_year <- substr(publishTo, 2,7)
    year <- year[year<=end_year]
    year <- paste0("X", year)
    year <- c(c("Sector", "Industry", "Asset"), year)
    df <- select(df, everything(year))
    return(df)
    
  }
  
  
  shared_inputs_gfcf <- PIM_I_agg(df = shared_inputs_gfcf,
                                  series = "gfcf")
  shared_inputs_gfcf <- remove_forecasted_years(shared_inputs_gfcf, c("Sector", "Industry", "Asset"), publishTo)
  
  shared_inputs_deflators <- PIM_I_agg(df = shared_inputs_deflators,
                                  series = "deflators")
  shared_inputs_deflators <- remove_forecasted_years(shared_inputs_deflators, c("Sector", "Industry", "Asset"), publishTo)
  
  shared_inputs_configs <- PIM_I_agg(df = shared_inputs_configs,
                                       series = "configs")
  
  mean_life_lengths <- PIM_I_agg(df = mean_life_lengths,
                                     series = "life_lengths")
  mean_life_lengths <- remove_forecasted_years(mean_life_lengths, c("Sector", "Industry", "Asset"), publishTo)
  
  min_life_lengths <- PIM_I_agg(df = min_life_lengths,
                                 series = "life_lengths")
  min_life_lengths <- remove_forecasted_years(min_life_lengths, c("Sector", "Industry", "Asset"), publishTo)
  
  max_life_lengths <- PIM_I_agg(df = max_life_lengths,
                                 series = "life_lengths")
  max_life_lengths <- remove_forecasted_years(max_life_lengths, c("Sector", "Industry", "Asset"), publishTo)
  
  cov <- PIM_I_agg(df = cov, series = "life_lengths")
  
  cov <- remove_forecasted_years(cov, c("Sector", "Industry", "Asset"), publishTo)
  
  sheets = list("Run_parameters" = shared_inputs_params,
                "GFCF_CP" = shared_inputs_gfcf,
                "Price_index" = shared_inputs_deflators,
                "Dep_ret_profiles" = shared_inputs_configs,
                "AverageLifeLengths" = mean_life_lengths,
                "Min" = min_life_lengths,
                "Max" = max_life_lengths,
                "CoVs" = cov,
                "OCIV" = shared_inputs_adjustments,
                "Reclassification" = reclassifications,
                "Other" = shared_inputs_util)
  
  write_xlsx(sheets,
             paste0(outputDir, "Pub_tables/PIM_input.xlsx"))
  
}

rm(shared_inputs_params, shared_inputs_gfcf, 
   shared_inputs_deflators, shared_inputs_configs,
   mean_life_lengths, min_life_lengths,
   max_life_lengths, cov, shared_inputs_adjustments,
   reclassifications, shared_inputs_util, sheets)

# Save the inputData ready for runAll
# if (WRITE_FILES) write_rds(inputData, paste0(outputDir, "pimInput_", runTime, ".Rds"))

#inputData <- inputData %>% filter(!(Industry=="84" & Sector=="S.11001"))
flog.info("PIM input file preparation complete.")

# -------------------------- Remove Objects ------------------------------------
rm(adjustments, maxYears, noData, naToZero, refYear, doMoreSplitting,forecastByGrowth)
