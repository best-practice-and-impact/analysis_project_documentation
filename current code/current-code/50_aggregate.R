# ======================= AGGREGATE ============================================

# Read in Sector/Industry/Asset hierarchies
# Aggregate to all Sector/Industry/Asset hierarchies
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#### AGGREAGTES NOT ADDING UP TO SECTOR, INDUSTRY, ASSET GROUPS, SO DECIMAL PLACES VALUES NOW ROUNDED TO ZERO DECIMAL POINTS BEFORE AGGRAGATION

# UNNEST DATA
out <- unnest(out)

measures <- c("GrossStockCVM", "NetStockCVM", "ProductiveStockCVM", "ConsumptionOfFixedCapitalCVM", "GrossStockCP",
              "NetStockCP", "ProductiveStockCP", "ConsumptionOfFixedCapitalCP", "CapitalServicesCP",
              "CapitalServicesCVM", "CapitalServicesCYP", "CapitalServicesPYP", "ConsumptionOfFixedCapitalCYP",
              "ConsumptionOfFixedCapitalPYP", "GrossStockCYP",  "GrossStockPYP",  "NetStockCYP", "NetStockPYP",
              "ProductiveStockCYP", "ProductiveStockPYP")

#### TAKE CARE OF ANY NEGATIVE VALUES BY MAKING THEM ZERO

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

for (m in measures){

  out[m] [ out[m]<0 ] <- 0
  out[m] [ is.nan(out[m]) ] <- 0

}

out [ is.nan(out) ] <- 0

# Not rounding S.1313 as the have already been rounded and rounding after splitting increases inconsistency with GG

out$ConsumptionOfFixedCapitalCP <- ifelse(out$Sector!="S.1313", round(out$ConsumptionOfFixedCapitalCP,0),out$ConsumptionOfFixedCapitalCP)
out$ConsumptionOfFixedCapitalCVM <- ifelse(out$Sector!="S.1313", round(out$ConsumptionOfFixedCapitalCVM,0),out$ConsumptionOfFixedCapitalCVM)

#### ROUND CP VALUES TO ZERO DECIMAL PLACES FOR CENTRAL GOVERNEMNT DATA
out$GrossStockCVM <- ifelse(out$Sector!="S.1313", round(out$GrossStockCVM,0),out$GrossStockCVM)
out$GrossStockCP <- ifelse(out$Sector!="S.1313", round(out$GrossStockCP,0),out$GrossStockCP)
out$NetStockCP <- ifelse(out$Sector!="S.1313", round(out$NetStockCP,0),out$NetStockCP)
out$NetStockCVM <- ifelse(out$Sector!="S.1313", round(out$NetStockCVM,0),out$NetStockCVM)

if (correct_CVM==TRUE){
  
  out$ConsumptionOfFixedCapitalCYP <- ifelse(out$Sector!="S.1313", round(out$ConsumptionOfFixedCapitalCYP,0),out$ConsumptionOfFixedCapitalCYP)
  out$ConsumptionOfFixedCapitalPYP <- ifelse(out$Sector!="S.1313", round(out$ConsumptionOfFixedCapitalPYP,0),out$ConsumptionOfFixedCapitalPYP)
  out$GrossStockPYP <- ifelse(out$Sector!="S.1313", round(out$GrossStockPYP,0),out$GrossStockPYP)
  out$GrossStockCYP <- ifelse(out$Sector!="S.1313", round(out$GrossStockCYP,0),out$GrossStockCYP)
  out$NetStockCYP <- ifelse(out$Sector!="S.1313", round(out$NetStockCYP,0),out$NetStockCYP)
  out$NetStockPYP <- ifelse(out$Sector!="S.1313", round(out$NetStockPYP,0),out$NetStockPYP)
  
}

#### Set capital consumption for cultivated assets to zero - this seems to have have been lost beteen v.1.0.8 and v.1.0.11

out$ConsumptionOfFixedCapitalCP [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalCVM [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalPYP [ out$Asset=="CULTIVATED" ] <- 0
out$ConsumptionOfFixedCapitalCYP [ out$Asset=="CULTIVATED" ] <- 0

# NEST DATA BACK UP BY Sector, Indsutry and Asset
out <- out %>% group_by(Sector, Industry, Asset) %>% nest(.key = "data")

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# ------------------- Read hierarchies for aggregation -------------------------
flog.info("Reading aggregation hierarchies.")
# Note, to avoid category conflicts each hierarchy column should be unique across
# the three hierarchies
hierarchies <- paste0(inputDir, "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx")
secHier <- read_excel(hierarchies, sheet = "Sector", col_types = "text")
indHier <- read_excel(hierarchies, sheet = "Industry", col_types = "text")
assHier <- read_excel(hierarchies, sheet = "Asset", col_types = "text")
# Format industry codes (i.e. "1" => "01")
indHier <- as.data.frame(lapply(indHier, prepim::formatIndustryCodes), stringsAsFactors = FALSE)
# Remove most detailed Industry level column and then remove duplicate rows
indHier <- distinct(mutate(indHier, A88 = NULL))

# ------------------- Aggregate All Hierarchies --------------------------------

out <- tidyr::unnest(out)

# Aggregate CP variables and all CYP/PYP pairs
# Specify variables to aggregate
colsToAggregate <- c("GrossStockCYP", "GrossStockPYP", "GrossStockCP",
                     "NetStockCYP", "NetStockPYP", "NetStockCP",
                     "ProductiveStockCYP", "ProductiveStockPYP", "ProductiveStockCP",
                     "CapitalServicesCYP", "CapitalServicesPYP", "CapitalServicesCP",
                     "ConsumptionOfFixedCapitalCYP", "ConsumptionOfFixedCapitalPYP", "ConsumptionOfFixedCapitalCP")

flog.info("Starting aggregation.")
# Perform the aggregation
aggregated <- aggregateAll(.data = out,
                           secHier = secHier, indHier = indHier, assHier = assHier,
                           values = colsToAggregate)
flog.info("Aggregation complete.")

# Save entire output dataset
#write_rds(aggregated, paste0("output/AGGREGATED", ".Rds"))
#outputForI <- sqldf("SELECT * FROM aggregated WHERE Industry = '52'")
#write_excel_csv(outputForI, path = paste0(outputDir, "All Data Aggregated", runTime, ".csv"))

# -------------------------- Remove Objects ------------------------------------
rm(colsToAggregate, secHier, indHier, assHier, hierarchies)
#rm(out)


