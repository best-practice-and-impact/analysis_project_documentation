####################### ADJUSTED DELIVERIES #####################################

# Takes CORD outputs from current run and a separate "closed period "run
# Joins together closed and open period data
# Writes out deliveries to a separate output folder called "GDP"
wdir <- getwd()
wdir <- paste0(wdir, "/")
method <- "value"
xms_last_closed_quarter <- substr(xms_last_closed_quarter, 1, 1) <- paste0("X", substr(xms_last_closed_quarter,2,7))

flog.info(paste0("Constrained to ", xms_last_closed_quarter,"\n"))

for (n in c("QUARTERLY", "ANNUAL")){
  type <- paste0(n, "_CORD_Import_File")
  

prev_files <- (Sys.glob(paste0("J:/Annual round/", last_del, "/Outputs/*.csv")))
for (i in prev_files){
  
  if (grepl(type, i))
    
    prev_spliced <- i
  
}

new_files <- (Sys.glob(paste0(wdir, outputDir, "*.csv")))
for (m in new_files){
  
  if (grepl(type, m)){
    
    current_spliced <- m
  
  }
}

closed_dataset <- read.csv(prev_spliced, stringsAsFactors=FALSE)
open_dataset <- read.csv(current_spliced, stringsAsFactors=FALSE)

if (n == "QUARTERLY"){
  h_columns <- colnames(open_dataset[1:6])
  d_columns <- colnames(open_dataset[7:ncol(open_dataset)])
  open_quarters <- d_columns[substr(d_columns,2,5)>substr(xms_last_closed_quarter,2,5) |
                             substr(d_columns,2,5)==substr(xms_last_closed_quarter,2,5) &
                             substr(d_columns,7,7) >= substr(xms_last_closed_quarter,7,7)]
  open_dataset <- open_dataset[c(h_columns, open_quarters)]
  colnames(open_dataset)[colnames(open_dataset)==xms_last_closed_quarter] <- "splicing_ratio"
  closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], xms_last_closed_quarter)
  closed_dataset <- closed_dataset[c(h_columns, closed_quarters)]
  cp <- left_join(closed_dataset, open_dataset)
  cp["splicing_ratio"] <- cp[xms_last_closed_quarter]/cp["splicing_ratio"]
}

if (n == "ANNUAL"){
  h_columns <- colnames(open_dataset[1:6])
  d_columns <- colnames(open_dataset[7:ncol(open_dataset)])
  open_quarters <- d_columns[substr(d_columns,2,5)>substr(xms_last_closed_quarter,2,5) |
                               substr(d_columns,2,5)==substr(xms_last_closed_quarter,2,5) |
                               substr(d_columns,7,7) >= substr(xms_last_closed_quarter,7,7)]
  open_dataset <- open_dataset[c(h_columns, open_quarters)]
  colnames(open_dataset)[colnames(open_dataset)==substr(xms_last_closed_quarter,1,5)] <- "splicing_ratio"
  closed_quarters <- append(d_columns[!(d_columns %in% open_quarters)], substr(xms_last_closed_quarter,1,5))
  closed_dataset <- closed_dataset[c(h_columns, closed_quarters)]
  last_closed_year <- substr(xms_last_closed_quarter,2,5)
  last_closed_year <- paste0("X",last_closed_year)
  cp <- left_join(closed_dataset, open_dataset)
  cp["splicing_ratio"] <- cp[last_closed_year]/cp["splicing_ratio"]
}


#cp$check <- cp$splicing_ratio



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

#rm(open_dataset_all, closed_dataset_all,open_dataset, 
#   closed_dataset, open_quarters,q, d_columns,h_columns)

#}

# Write out constrained outputs


spliced <- spread(cp, Period, Value)
  
spliced <- gather(spliced, Period, Value, 7:ncol(spliced))
spliced$Period <- gsub("X","",spliced$Period)
spliced <- spread(spliced, Period, Value)

if (!(dir.exists(paste0(outputDir, "GDP/")))){
  outputDir1 <- paste0(wdir, outputDir, "GDP/")
  dir.create(file.path(outputDir1))
} 
write.csv(spliced, paste0(wdir, outputDir, "GDP/",last_del, "_consistent_",n, "_outputs.csv"))
}

################################################################################

# Writing out deliveries using templates in input directory

################################################################################


wd <- getwd()
setwd(paste0(inputDir, "Deliveries"))
templates <- (Sys.glob("*.csv"))
setwd(wd)

CORDQuarterly <- read.csv(paste0(wdir, outputDir, "GDP/", last_del, "_consistent_QUARTERLY_outputs.csv"))
CORDAnnual <- read.csv(paste0(wdir, outputDir, "GDP/", last_del, "_consistent_ANNUAL_outputs.csv"))
CORDAnnual <- CORDAnnual[ -1 ]

for (i in templates){
  print(i)
  if (i =="SFA.csv"){
    next
  }
  if (i == "PNFC.csv"){
    next
  }
  
  template <- read.csv(paste0(inputDir, "Deliveries/",i), stringsAsFactors = F)
  
  if (grepl("Q",template$Periodicity[1])==T){
    
    templateQ <- template
    templateQ$Periodicity <- "Q"
    templateQ$Period <- gsub("X", "", templateQ$Period)
    
    # Extract each comination for all
    
    if ('All' %in% rbind(templateQ$Sector,
                         templateQ$Asset,
                         templateQ$Industry)){
      
      # Filter all and non-alls
      
      templateQ_all <- filter(templateQ, Sector=="All" |
                                Asset=="All" |
                                Industry=="All")
      templateQ <- filter(templateQ, Sector!="All" &
                            Asset!="All" &
                            Industry!="All")
      
      # Selective filter for all
      
      for (z in 1:nrow(templateQ_all)){
        
        template_row <- templateQ_all[z,]
        
        for (x in names(templateQ_all)){
          
          if ('All' %in% template_row[x]){
            
            if (x=="Industry"){
              
              CORD_extract <- filter(CORDQuarterly, 
                                     Sector==as.character(template_row["Sector"][1,1]) &
                                       Asset==as.character(template_row["Asset"][1,1]) &
                                       Measure==as.character(template_row["Measure"][1,1]) &
                                       Price==as.character(template_row["Price"][1,1]))
              CORD_extract <- select(CORD_extract, Sector, Industry, Price, Periodicity, Asset, Measure)
              
            }
            
          }
          
          templateQ <- rbind(templateQ, CORD_extract)
          
        }
        
      }
      
    }
    
    templateQ <- left_join(templateQ, CORDQuarterly)
    
    if (i=="PNFC.csv"){
      
      PNFC_map <- select(templateQ, CDID, Industry, Measure)
      
      templateQ$CDID <- NULL
      templateQ <- filter(templateQ, Sector!="")
      
      # Sum 06 and 09
      
      templateQ$Industry <- ifelse(templateQ$Industry=="B06" |
                                     templateQ$Industry=="B09",
                                   "B06+B09", templateQ$Industry)
      templateQ <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=templateQ, FUN=sum)
      
      # Create total - 06 and 09
      
      totmin0609 <- filter(templateQ, Industry=="_T" | Industry=="B06+B09")
      columns <- names(totmin0609)[8:ncol(totmin0609)]
      totmin0609$adj <- ifelse(totmin0609$Industry=="_T",1,-1)
      totmin0609$Industry <- "_T_exc_0609"
      
      for (c in columns){
        
        totmin0609[c] <- totmin0609[c]*totmin0609['adj']
        
      }
      
      totmin0609$adj <- NULL
      totmin0609 <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=totmin0609, FUN=sum)
      
      templateQ <- rbind(templateQ, totmin0609)
      
      # Add a mapping.
      
      templateQ <- left_join(templateQ, PNFC_map)
      
      
    }
    
    if (i=="PNFC.csv" | i== "SFA.csv"){
      
      #templateQ <- read.csv("J:/Quarterly round/2021Q4M3R1/Outputs/CAP_STOCKS_QUARTERLY_CORD_Import_File_2022-03-03_1158.csv")
      templateQ <- select(templateQ, -Sector, -Price, -Measure, -Periodicity, -Industry, -Asset)
      templateQ <- gather(templateQ, "Period", "Value", -CDID)
      templateQ <- spread(templateQ, CDID, Value)
      templateQ$Period <- gsub("X", "", templateQ$Period)
      
      if (ANNUAL_DELIVERY=="TRUE"){
        
        names(templateQ)[1] <- "*E10IN"
        
      } else {
        
        names(templateQ)[1] <- "*ESIN"
        
      }
      
      
    } else{
      
      write.csv(templateQ, paste0(wdir, outputDir,"GDP/", last_del,"_consistent_Quarterly_spreadsheet_delivery_",runTime,"_",i), row.names=FALSE)
      
    }
    
  }
  
  if (grepl("A",template$Periodicity[1])==T){
    
    templateA <- template
    templateA$Periodicity <- "A"
    
    # Extract each comination for all
    
    if ('All' %in% rbind(templateA$Sector,
                         templateA$Asset,
                         templateA$Industry)){
      
      # Filter all and non-alls
      
      templateA_all <- filter(templateA, Sector=="All" |
                                Asset=="All" |
                                Industry=="All")
      templateA <- filter(templateA, Sector!="All" &
                            Asset!="All" &
                            Industry!="All")
      
      # Selective filter for all
      
      for (z in 1:nrow(templateA_all)){
        
        template_row <- templateA_all[z,]
        
        for (x in names(templateA_all)){
          
          if ('All' %in% template_row[x]){
            
            if (x=="Industry"){
              
              CORD_extract <- filter(CORDAnnual, 
                                     Sector==as.character(template_row["Sector"][1,1]) &
                                       Asset==as.character(template_row["Asset"][1,1]) &
                                       Measure==as.character(template_row["Measure"][1,1]) &
                                       Price==as.character(template_row["Price"][1,1]))
              CORD_extract <- select(CORD_extract, Sector, Industry, Price, Periodicity, Asset, Measure)
              
            }
            
          }
          
        }
        
        templateA <- rbind(templateA, CORD_extract)
        
      }
      
    }
    
    templateA <- left_join(templateA, CORDAnnual)
    templateA <- gather(templateA, Period, Value, 7:ncol(templateA))
    templateA$Period <- gsub("X","",templateA$Period)
    templateA <- spread(templateA, Period, Value)
    
    # if (i=="PNFC.csv"){
    #   
    #   PNFC_map <- select(templateA, CDID, Industry, Measure)
    #   
    #   templateA$CDID <- NULL
    #   templateA <- filter(templateA, Sector!="")
    #   
    #   # Sum 06 and 09
    #   
    #   templateA$Industry <- ifelse(templateA$Industry=="B06" |
    #                                  templateA$Industry=="B09",
    #                                "B06+B09", templateA$Industry)
    #   templateA <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=templateA, FUN=sum)
    #   
    #   # Create total - 06 and 09
    #   
    #   totmin0609 <- filter(templateA, Industry=="_T" | Industry=="B06+B09")
    #   columns <- names(totmin0609)[8:ncol(totmin0609)]
    #   totmin0609$adj <- ifelse(totmin0609$Industry=="_T",1,-1)
    #   totmin0609$Industry <- "_T_exc_0609"
    #   
    #   for (c in columns){
    #     
    #     totmin0609[c] <- totmin0609[c]*totmin0609['adj']
    #     
    #   }
    #   
    #   totmin0609$adj <- NULL
    #   totmin0609 <- aggregate(. ~ Sector+Industry+Asset+Measure+Price+Periodicity, data=totmin0609, FUN=sum)
    #   
    #   templateA <- rbind(templateA, totmin0609)
    #   
    #   # Add a mapping.
    #   
    #   templateA <- left_join(templateA, PNFC_map)
    #   
    #   
    # }
    # 
    # 
    # if (i=="PNFC.csv" | i== "SFA.csv"){
    #   
    #   #templateA <- read.csv("J:/Shared Outputs Quarterly/2020-05-19_1036_RUN_002_R_2020Q1M3/Annual_spreadsheet_delivery_2020-05-19_1036_SFA.csv")
    #   templateA <- select(templateA, -Sector, -Price, -Measure, -Periodicity, -Industry, -Asset)
    #   templateA <- gather(templateA, "Period", "Value", -CDID)
    #   templateA <- spread(templateA, CDID, Value)
    #   templateA$Period <- gsub("X", "", templateA$Period)
    #   
    #   if (ANNUAL_DELIVERY=="TRUE"){
    #     
    #     names(templateA)[1] <- "*E10IN"
    #     
    #   } else {
    #     
    #     names(templateA)[1] <- "*ESIN"
    #     
    #   }
    #   
    #   templateQnames <- as.data.frame(t(names(templateQ)))
    #   names(templateQnames) <- names(templateQ)
    #   templateA <- rbind(templateA,templateQnames,templateQ)
    #   
    #   write.csv(templateA, paste0(outputDir,gsub(".csv","", i),"_spreadsheet_delivery_",runTime,".csv"),row.names=FALSE)
    #   

      write.csv(templateA, paste0(wdir, outputDir, "GDP/",last_del,"_consistent_Annual_spreadsheet_delivery_",runTime,"_",i),row.names=FALSE)
      
  } 
    
  }
