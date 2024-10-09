############################# PROCESS GFCF #####################################

# Combine the GFCF and Price Index data.frames from the main sectors and S.13
# sectors into one data.frame
# Save results

# ======================= COMBINE GFCF with S.13 ===============================

if (exists("gfcfS13")) {
  flog.info("Combining GFCF and Deflators for main sectors and S.13 sectors.")
  gfcf <- bind_rows(gfcf, gfcfS13)
  rm(gfcfS13)
}

# Zero out if projection set

if (nchar(project_from)==7){
  
  quarters <- unique(gfcf$Period)
  quarters <- quarters[substr(quarters,2,5)>substr(project_from,2,5) |
                         substr(quarters,2,5)==substr(project_from,2,5) &
                         substr(quarters,7,7) >= substr(project_from,7,7)]
  
  for (quarter in quarters){
    
    gfcf$gfcfCP <- ifelse(gfcf$Period==quarter, 0, gfcf$gfcfCP)
    
  }
  
}



# Re-referencing deflators - probably now unneccessary

if (correct_referencing == "TRUE"){
  
  gfcf <- gfcf %>% group_by(Sector,Industry,Asset) %>% mutate(adjustment = (sum(PriceIndex[Period==refPeriod])))
  
} else{
  
  gfcf$year <- substr(gfcf$Period,2,5)
  gfcf <- gfcf %>% group_by(Sector,Industry,Asset) %>% mutate(adjustment = (sum(PriceIndex[as.numeric(year)==substr(refPeriod,2,5)]))/4)
}


gfcf$PriceIndex <- gfcf$PriceIndex/gfcf$adjustment
gfcf$adjustment <- NULL
gfcf$year <- NULL
gfcf <- ungroup(gfcf)

# Write out copy of input files

gfcf_table <- filter(gfcf, substr(Period,2,5)>1996)
gfcf_table$PriceIndex <- NULL
gfcf_table <- spread(gfcf_table, Period, gfcfCP)
write.csv(gfcf_table, paste0(outputDir, "gfcf_table.csv"), row.names=FALSE)

shared_inputs_gfcf <- gfcf
shared_inputs_gfcf$PriceIndex <- NULL
shared_inputs_gfcf <- spread(shared_inputs_gfcf, Period, gfcfCP)

deflators_table <- filter(gfcf, substr(Period,2,5)>1996)
deflators_table$gfcfCP <- NULL
deflators_table <- spread(deflators_table, Period, PriceIndex)
write.csv(deflators_table, paste0(outputDir,"deflators_table.csv"), row.names=FALSE)

shared_inputs_deflators <- gfcf
shared_inputs_deflators$gfcfCP <- NULL
shared_inputs_deflators <- spread(shared_inputs_deflators, Period, PriceIndex)


# # QA
# 
# if(QA_TABLES_CHARTS== TRUE) {
#   
#   # Previous files
#   
#   prev_deflators <- read.csv(paste0(last_run, "/Outputs/deflators_table.csv"))
#   prev_gfcf <- read.csv(paste0(last_run, "/Outputs/gfcf_table.csv"))
#   
#   # Re-shape
#   
#   prev_deflators <- gather(prev_deflators,"Period", "Value",4:ncol(prev_deflators))
#   prev_gfcf <- gather(prev_gfcf,"Period", "Value",4:ncol(prev_gfcf))
#   deflators_table <- gather(deflators_table,"Period", "Value",4:ncol(deflators_table))
#   gfcf_table <- gather(gfcf_table,"Period", "Value",4:ncol(gfcf_table))
#   prev_gfcf <- filter(prev_gfcf, Period > paste0("Y1996"), Period <= publishTo)
#   gfcf_table <- filter(gfcf_table, Period > paste0("Y1996"), Period <= publishTo)
#   prev_deflators <- filter(prev_deflators, Period > paste0("Y1996"), Period <= publishTo)
#   deflators_table <- filter(deflators_table, Period > paste0("Y1996"), Period <= publishTo)
#   
#   # Create QA outputs
#   
#   QA(deflators_table, prev_deflators, "deflators", "non-additive", annual = FALSE, limit = 0.02)
#   QA(gfcf_table, prev_gfcf, "gfcf", "additive", annual = FALSE, limit = 500)
#   
# }

rm(gfcf_table, deflators_table)

# ============================ OUTPUT DATA =====================================

# if (WRITE_FILES) write_rds(gfcf, paste0(outputDir, "prepared_gfcf_and_defs", runTime, ".Rds"))

flog.info("GFCF and Deflator preparation complete.")


# ============================ CALC SPLITS =====================================
# Calculate industry splits for Transfer Costs in current run

# If current run is unconstrained (no closed period) these outputs can be used 
# in input files for the next constrained run to avoid revisions to the closed 
# period

# Industry splits for S.1PT

TC <- filter(gfcf, Asset == "LAND.IMPROVEMENTS.TC")
TC = select(TC,c(Period,Sector,Industry,Asset,gfcfCP))
TC <- filter(TC, Period > "Y1996Q4")

TC <- TC %>% group_by(Sector, Asset, Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
TC$split <- TC$gfcfCP/TC$sum

# Manually set S.125PF to be 1 as there is no GFCF in this sector
TC$split[TC$Sector == "S.125PF"] <- 1
TC$Asset <- "TC"
TC = select(TC, -c(gfcfCP,sum))
TC <- TC %>% spread(Period, split , fill =0)

# Read in old splits to ensure coverage is the same
oldsplits <- read_rds("J:/Annual round/RUN_05/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds")

# Extend old splits file to go up to latest year
if (length(setdiff(names(TC), names(oldsplits))>0)){
  
  for (d in setdiff(names(TC), names(oldsplits))){
    
    oldsplits[d] <- oldsplits[ncol(oldsplits)]
    
  }
  
}

# Remove extra series in new splits and add any missing splits from old splits file
TC$concat <- paste0(TC$Sector,TC$Industry,TC$Asset)

oldsplits$concat <- paste0(oldsplits$Sector,oldsplits$Split,oldsplits$Asset)

no_TC <- anti_join(oldsplits,TC, by="concat")

no_oldsplits <- anti_join(TC,oldsplits, by="concat")

TC = select(TC, -c(concat))
no_TC = select(no_TC, -c(concat))

TC$Split <- TC$Industry

TC$Industry <- "68"

# Move new splits variable to start of data frame
a <- 1:ncol(TC)
a <- a[-c(1:3)]
b <- a[length(a)]
a <- head(a,-1)

TC <- TC[,c(1,2,3,b,a)]

# Match coverage of old splits to new file & write to outputs folder
newSplits <- rbind(TC, no_TC)

newSplits$concat <- paste0(newSplits$Sector,newSplits$Split,newSplits$Asset)

no_oldsplits$concat <- paste0(no_oldsplits$Sector,no_oldsplits$Industry,no_oldsplits$Asset)

newSplits <- newSplits[!(newSplits$concat %in% no_oldsplits$concat),]

newSplits = select(newSplits,-c(concat))

write_rds(newSplits,paste0(outputDir,"prev_indSplit.Rds"))

# Sector splits for S.12

s12 <- filter(gfcf, Sector == "S.125IN" | Sector == "S.125PF" | Sector == "S.12FE" | Sector == "S.12MF" | Sector == "S.12IN")
s12 <- filter(s12, Asset == "LAND.IMPROVEMENTS.TC")
s12$Sector[s12$Sector == "S.12IN"] <- "S.125IN"
s12 = select(s12,c(Period,Sector,Industry,Asset,gfcfCP))
s12 <- filter(s12, Period > "Y1996Q4")

# Aggregate up assets
s12 <- aggregate(s12$gfcfCP, list(s12$Period,s12$Sector),sum)

colnames(s12) <- c("Period","Sector","gfcfCP")

s12 <- s12 %>% group_by(Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
s12$split <- s12$gfcfCP/s12$sum
s12 = select(s12, -c(gfcfCP,sum))

s12 <- s12 %>% spread(Period, split , fill =0)

s12$Split <- s12$Sector
s12$Sector <- "S.12IN"
s12$Industry <- "ALL"
s12$Asset <- "ALL"

# Move new splits/industry/asset variables to start of dataframe
a <- 1:ncol(s12)
b <- a[length(a)]
a <- a[-c(1)]
a <- head(a,-3)

s12 <- s12[,c(1,(b-1),b,(b-2),a)]

write_rds(s12,paste0(outputDir,"prev_s12split.rds"))

# S.13 industry splits

# Read in old splits to ensure coverage is the same
s13splits <- read_rds("J:/Annual round/RUN_05/Inputs/Splits/S13_split.rds")

s13 <- filter(gfcf, Sector == "S.1311" | Sector == "S.1313")
s13 <- filter(s13, Asset == "LAND.IMPROVEMENTS.TC")
s13 = select(s13,c(Period,Sector,Industry,Asset,gfcfCP))
s13 <- filter(s13, Period > "Y1996Q4")

s13 <- s13 %>% group_by(Sector, Asset, Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
s13$split <- s13$gfcfCP/s13$sum
s13[is.na(s13)] <- 0
s13$Asset <- "TC"

s13 = select(s13, -c(gfcfCP,sum))

s13 <- s13 %>% spread(Period, split , fill =0)

s1311splits <- filter(s13splits, Sector == "S.1311")


s13$Split <- s13$Industry

s13$Industry <- "68"

s13 <-  s13[, colSums(s13 != 0) > 0]

# Move new splits variable to the start
a <- 1:ncol(s13)
a <- a[-c(1:3)]
b <- a[length(a)]
a <- head(a,-1)

s13 <- s13[,c(1,2,3,b,a)]

if (length(setdiff(names(s13), names(s1311splits))>0)){
  
  for (d in setdiff(names(s13), names(s1311splits))){
    
    s1311splits[d] <- s1311splits[ncol(s1311splits)]
    
  }
  
}



s13 <- s13[,c(1,2,3,b,a)]

if (length(setdiff(names(s1311splits), names(s13))>0)){
  
  for (d in setdiff(names(s1311splits), names(s13))){
    
    s13[d] <- s13[ncol(s13)]
    
  }
  
}



news13splits <- rbind(s1311splits,s13)

write_rds(news13splits,paste0(outputDir,"S13_split.rds"))

remove(TC,s12,s13,a,b,news13splits,s1311splits,newSplits,no_oldsplits,oldsplits)
