# Checks

measures <- c("ConsumptionOfFixedCapital", "NetStock", "GrossStock")
prices <- c("CP", "CVM")#, "PYP")

# 1. Check for any negative values

for (x in measures){
  for (y in prices){
  
    check1 <- filter_(chainedUnnestAnnual, paste0(x,y, "<0"))
    check1a <- filter_(chainedUnnest, paste0(x,y, "<0"))
    
    check1 <- rbind(check1, check1a)
  
    if (paste0(x,y)==paste0(measures[1],prices[1])){
    
      check1all <- check1
    
    }
  
    if (paste0(x,y)!=paste0(measures[1],prices[1])){
  
      check1all <- rbind(check1, check1all)
    
    }  
  }
}

if (nrow(check1all) > 0){
  
  write.csv(check1all, paste0(outputDir,"/negative_capital_stock.csv"))
  
}

rm(x,y)

# 2. CP = CVM in reference year

# Changed 

for (i in measures) {

  check2 <- filter(chainedUnnestAnnual, Period==substr(refPeriod,2,5) & substr(Industry,1,2)!="GF")
  check2a <- filter_(check2, paste0(i,"CP -", i,"CVM >1"))
  check2b <- filter_(check2, paste0(i,"CP -", i,"CVM < -1"))
  check2 <- rbind(check2a, check2b)

  if (i==measures[1]) {
    
    check2all <- check2
    
  }
  
  if (i!=measures[1]) {
    
    check2all <- rbind(check2, check2all)
    
  }  
}

if (nrow(check2all) > 0){
  
  write.csv(check2all, paste0(outputDir,"/CP_equals_CVM.csv"))
  
}

# 3. PYP = CVM in reference year+1

for (i in measures) {
  
  check3 <- filter(chainedUnnestAnnual, Period==(as.numeric(substr(refPeriod,2,5))+1) & substr(Industry,1,2)!="GF")
  check3 <- filter_(check3, paste0(i,"PYP != ", i,"CVM"))
  
  
  if (i==measures[1]) {
    
    check3all <- check3
    
  }
  
  if (i!=measures[1]) {
    
    check3all <- rbind(check3, check3all)
    
  }  
}

if (nrow(check3all) > 0){
  
  write.csv(check3all, paste0(outputDir,"/PYP_equals_CVM.csv"))
  
}

# 4. Check if changes greater than doubling/halving

# Quarterly to be added with other coding changes

for (x in measures){
  for (y in prices){
    for (i in c("A")){
  
      if (i=="Q"){
    
      check4 <- select_(chainedUnnest, "Sector", "Industry", "Asset", "Period", paste0(x,y))  
      check4$Period2 <- paste0(substr(check4$Period,2,5),substr(check4$Period,7,7))
      check4 <- filter(check4, Period2<=gfcfForecastTo)
      
      }
    
      if (i=="A"){
    
      check4 <- select_(chainedUnnestAnnual, "Sector", "Industry", "Asset", "Period", paste0(x,y))    
      check4 <- filter(check4, Period<=lastCompleteYear)
      min <- min(check4$Period)+1
      max <- max(check4$Period)
      
      }
    
      check4$Period <- paste0("Y",check4$Period)
      check4 <- spread_(check4, "Period", paste0(x,y))
    
      for (p in max:min){
      
      check4[paste0("Y",p)] <- check4[paste0("Y",p)]/check4[paste0("Y",p-1)]
      
      }
    
      check4[paste0("Y",min-1)] <- NULL
      is.na(check4)<-sapply(check4, is.infinite)
      check4[is.na(check4)]<-0
      check4$min <- apply(check4[4:ncol(check4)],1,FUN=min)
      check4$max <- apply(check4[4:ncol(check4)],1,FUN=max)
      check4 <- filter(check4,min!=0 & min<0.5 | max>2)
      check4["Measure"] <- paste0(x,y)
    
      if (paste0(x,y)==paste0(measures[1],prices[1])){
      
        if (i=="Q"){
          
         check4quarterly <- check4
         
        }
        
        if (i=="A"){
          
          check4annual <- check4
          
        }
      
      }
        
      if (paste0(x,y)!=paste0(measures[1],prices[1])){
          
        if (i=="Q"){
            
          check4quarterly <- rbind(check4quarterly, check4)
            
        }
          
        if (i=="A"){
            
          check4annual <- rbind(check4annual, check4)
            
        }
      }
    }
  }
}

if (nrow(check4annual) > 0){
  
  write.csv(check4annual, paste0(outputDir,"/Large_change.csv"))
  
}

#if (nrow(check2all) > 0){
  
#  write.csv(check2all, "CP_equals_CVM.csv")
  
#}

# Clear out rubbish from environment

rm(check1, check1a, check1all, check2, check2a, check2b, check2all, check3, check3all, check4, check4annual, i, x, y, max, min,
   measures, prices)