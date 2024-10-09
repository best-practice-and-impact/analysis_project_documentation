TC <- filter(gfcf, Asset == "LAND.IMPROVEMENTS.TC")
TC = select(TC,c(Period,Sector,Industry,Asset,gfcfCP))
TC <- filter(TC, Period > "Y1996Q4")

TC <- TC %>% group_by(Sector, Asset, Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
TC$split <- TC$gfcfCP/TC$sum

#manually set S.125PF to be 1 as there is no GFCF in this sector
TC$split[TC$Sector == "S.125PF"] <- 1


#TC[is.na(TC)] <- 0
TC$Asset <- "TC"

#TC$split[TC$split == 0] <- 1

TC = select(TC, -c(gfcfCP,sum))

TC <- TC %>% spread(Period, split , fill =0)

oldsplits <- read_rds(paste0(last_run,"/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds"))

#extend old splits file to go up to latest year
if (length(setdiff(names(TC), names(oldsplits))>0)){
  
  for (d in setdiff(names(TC), names(oldsplits))){
    
    oldsplits[d] <- oldsplits[ncol(oldsplits)]
    
  }
  
}

#remove extra series in new splits and add any missing splits from old splits file
TC$concat <- paste0(TC$Sector,TC$Industry,TC$Asset)

oldsplits$concat <- paste0(oldsplits$Sector,oldsplits$Split,oldsplits$Asset)

no_TC <- anti_join(oldsplits,TC, by="concat")

no_oldsplits <- anti_join(TC,oldsplits, by="concat")

TC = select(TC, -c(concat))
no_TC = select(no_TC, -c(concat))

TC$Split <- TC$Industry

TC$Industry <- "68"

#move new splits variable to start of dataframe
a <- 1:ncol(TC)
a <- a[-c(1:3)]
b <- a[length(a)]
a <- head(a,-1)

TC <- TC[,c(1,2,3,b,a)]

newSplits <- rbind(TC, no_TC)

newSplits$concat <- paste0(newSplits$Sector,newSplits$Split,newSplits$Asset)

no_oldsplits$concat <- paste0(no_oldsplits$Sector,no_oldsplits$Industry,no_oldsplits$Asset)

newSplits <- newSplits[!(newSplits$concat %in% no_oldsplits$concat),]

newSplits = select(newSplits,-c(concat))


write_rds(newSplits,paste0(outputDir,"prev_indSplit.Rds"))



#s12pf needs to be 1

# s125pf <- s12pf %>% gather(Period,Value,5:ncol(s12pf))
# s125pf$Value <- 1
# s125pf <- s125pf %>% spread(Period, Value , fill =0)

# indSplit = filter(indSplit,Sector != "S.125PF")
# 
# indSplit <- rbind(indSplit,s125pf)
# write_rds(indSplit,"J:/Annual round/RUN_06/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds")
# 
# splits <- read_rds("J:/Annual round/RUN_06/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds")
# 
# splits <- filter(splits,Sector != "S.125IN")
# splits <- filter(splits,Sector != "S.125PF")
# splits <- filter(splits,Sector != "S.12FE")
# splits <- filter(splits,Sector != "S.12MF")
# 
# splits <- rbind(splits,TC)
# 
# write_rds(splits,"J:/Annual round/RUN_06/Inputs/Parameters, Assumptions & Adjustments/prev_indSplit.Rds")

# TC <- filter(TC,Sector != "S.11PR")
# TC <- filter(TC,Sector != "S.14")
# TC <- filter(TC,Sector != "S.15")
# TC <- filter(TC,Sector != "S.1313")

################################################################################
#s12 splits
s12 <- filter(gfcf, Sector == "S.125IN" | Sector == "S.125PF" | Sector == "S.12FE" | Sector == "S.12MF" | Sector == "S.12IN")
s12 <- filter(s12, Asset == "LAND.IMPROVEMENTS.TC")
s12$Sector[s12$Sector == "S.12IN"] <- "S.125IN"
s12 = select(s12,c(Period,Sector,Industry,Asset,gfcfCP))
s12 <- filter(s12, Period > "Y1996Q4")


# s122 <- s12

s12 <- aggregate(s12$gfcfCP, list(s12$Period,s12$Sector),sum)

colnames(s12) <- c("Period","Sector","gfcfCP")

s12 <- s12 %>% group_by(Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
s12$split <- s12$gfcfCP/s12$sum

# s12$Asset <- "TC"

#TC$split[TC$split == 0] <- 1

s12 = select(s12, -c(gfcfCP,sum))

s12 <- s12 %>% spread(Period, split , fill =0)

s12$Split <- s12$Sector
s12$Sector <- "S.12IN"
s12$Industry <- "ALL"
s12$Asset <- "ALL"
#move new splits/industry/asset variables to start of dataframe
a <- 1:ncol(s12)
b <- a[length(a)]
a <- a[-c(1)]
a <- head(a,-3)

s12 <- s12[,c(1,(b-1),b,(b-2),a)]

#write_rds(s12,"J:/Annual round/RUN_06/Inputs/Parameters, Assumptions & Adjustments/prev_s12split.rds")
write_rds(s12,paste0(outputDir,"prev_s12split.rds"))

# s12 <- read.csv("D:/s12_5splitcsv.csv", stringsAsFactors = F)
# s12 = select(s12,-c(Grand.Total))
# 
# s12splits <- read_rds("J:/Annual round/RUN_05/Inputs/Parameters, Assumptions & Adjustments/prev_s12split.rds")
# s12 <- filter(gfcf, Sector == "S.125IN" | Sector == "S.125PF" | Sector == "S.12FE" | Sector == "S.12MF")
# s12 <- filter(s12, Period > "Y1996Q4")
# s12$Sector[s12$Sector == "S.125IN"] <- "S.12IN"
# 
# s12 <- s12 %>% group_by(Sector, Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
# s12$split <- s12$gfcfCP/s12$sum
# 
# s12[is.na(s12)] <- 0
# 
# write_rds(s12,"J:/Annual round/RUN_06/Inputs/Parameters, Assumptions & Adjustments/prev_s12split.rds")

################################################################################
#s13
s13splits <- read_rds(paste0(last_run,"/Inputs/Splits/S13_split.rds"))

s13 <- filter(gfcf, Sector == "S.1311" | Sector == "S.1313")

s13 <- filter(s13, Asset == "LAND.IMPROVEMENTS.TC")
s13 = select(s13,c(Period,Sector,Industry,Asset,gfcfCP))
s13 <- filter(s13, Period > "Y1996Q4")

s13 <- s13 %>% group_by(Sector, Asset, Period) %>% mutate(sum = sum(gfcfCP)) %>% ungroup()
s13$split <- s13$gfcfCP/s13$sum


# s11pr <- filter(TC, Sector == "S.11PR" & Period == "Y1997Q1")
# sum(s11pr$split)

s13[is.na(s13)] <- 0
s13$Asset <- "TC"

#TC$split[TC$split == 0] <- 1

s13 = select(s13, -c(gfcfCP,sum))

s13 <- s13 %>% spread(Period, split , fill =0)

s1311splits <- filter(s13splits, Sector == "S.1311")
#s1313splits <- filter(s13splits, Sector == "S.1313")


s13$Split <- s13$Industry

s13$Industry <- "68"

#remove empty columns
s13 <- s13[!!colSums(replace(s13, is.na(s13), 0)!= 0)]

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

s13 <-  s13[, colSums(s13 != 0) > 0]

s13 <- s13[,c(1,2,3,b,a)]

if (length(setdiff(names(s1311splits), names(s13))>0)){
  
  for (d in setdiff(names(s1311splits), names(s13))){
    
    s13[d] <- s13[ncol(s13)]
    
  }
  
}



news13splits <- rbind(s1311splits,s13)

write_rds(news13splits,paste0(outputDir,"S13_split.rds"))
#write_rds(news13splits,"J:/Annual round/RUN_06/Inputs/Splits/S13_split.rds")


# 
# 
# indSplit <- indSplit %>% gather(Period,Value,5:ncol(indSplit))
# indSplit$Value <- 1
# indSplit <- indSplit %>% spread(Period, Value , fill =0)
# 
# write_rds(indSplit,"J:/Annual round/RUN_06/Inputs/Splits/S13_split.rds")
# 
# 
# secSplitS12$Split[secSplitS12$Split == "S.12IN"] <- "S.125IN"

