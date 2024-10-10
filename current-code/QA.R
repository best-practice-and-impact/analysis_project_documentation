#This tool compares two datasets with eachother using tables and graphs

#To read this function into your enviroment you can use the following:
#source("./QA.R")

#To compare datasets you need to run with the following line:
#QA(name of new df, name of prev df, output name, additivity, annual)

#new_df should be the latest dataset
#prev_df should be the previous dataset
#output_name should be the name of the output files
#additivity: for series where components do not add to total use...
#'non-additive' e.g price indices
# for series where the components are additive use 'additive'

#IMPORTANT NOTES: 
#The format of the dataset needs to have columns Sector, Asset,
#Industry and periods in the format "Y2000Q1"
#currently for additive series the totals need to be removed 
#e.g using DPLYR new_df <- filter(new_df, Asset!= "TOTAL")
#labelling on graphs needs to be fixed (everything is labelled GFCF)


QA <- function (new_df, prev_df, output_name, additivity, 
                annual = FALSE, limit = 200) {
  
  # Coverage differences

  # new_df <- deflators_table
  # prev_df <- prev_deflators
  # output_name <- "deflators"
  # additivity <- "non-additive"
  
  # Create QA folder if does not exist 
  
  x_lab <- ifelse(annual == TRUE, "year","quarter")
  if (!file.exists(paste0(outputDir, "QA_", x_lab))){
    
    dir.create(file.path(paste0(outputDir, "QA_", x_lab)))
    
  }
  
  #Checking for series that are not present in both datasets 
  
  new_series <- unique(c(paste0(new_df$Sector,new_df$Industry,new_df$Asset)))
  existing_series <- unique(c(paste0(prev_df$Sector,prev_df$Industry,prev_df$Asset)))
  
  #Checking for series present in new dataset but not in previous
  
  if (length(new_series[!new_series %in% existing_series]) != 0){
    
    additionalseries <- data.frame(difference = "additional", series = new_series[!new_series %in% existing_series])
    new_df$concat <- paste0(new_df$Sector,new_df$Industry,new_df$Asset)
    adjust_prev <- new_df[new_df$concat %in% additionalseries$series,]
    adjust_prev$Value <- 0
    new_df$concat <- NULL
    adjust_prev$concat <- NULL
    
  }
  
  #Checking for series not present in new dataset
  
  if (length(existing_series[!existing_series %in% new_series]) != 0){
    
    missingseries <- data.frame(difference = "missing", series = existing_series[!existing_series %in% new_series])
    prev_df$concat <- paste0(prev_df$Sector,prev_df$Industry,prev_df$Asset)
    adjust_new <- prev_df[prev_df$concat %in% missingseries$series,]
    prev_df$concat <- NULL
    adjust_new$concat <- NULL
    adjust_new$Value <- 0
    
  }
  
  #Adding in empty series to ensure dataframes are same length
  
  new_df <- rbind(new_df, if (exists('adjust_new')) adjust_new)
  prev_df <- rbind(prev_df, if (exists('adjust_prev')) adjust_prev)
  
  #Putting together all the differences into one dataframe
  
  if (!exists('additionalseries') & exists('missingseries')){
    
    differences <- missingseries
    
  }
  
  if (exists('additionalseries') & !exists('missingseries')){
    
    differences <- additionalseries
    
  }
  
  if (exists('additionalseries') & exists('missingseries')){
    
    differences <- rbind(additionalseries, missingseries)
    
  }
  
  # Combining deflators that aren't unique

  if (grepl('deflator', output_name)== TRUE) {
  
    unique_mapping <-read.csv(paste0(inputDir,"/Mapping & Aggregation/Unique deflator mapping.csv"), stringsAsFactors = FALSE)
  
    # Map industry to correct format (i.e remove dates)
  
    ind_map <- readRDS(paste0(inputDir,"/Mapping & Aggregation/SIC_Industry_mapping.rds"))
  
    unique_mapping <- left_join(unique_mapping,ind_map)
    unique_mapping$Industry <- ifelse(is.na(unique_mapping$Corrected),
                                   unique_mapping$Industry,unique_mapping$Corrected)
    unique_mapping$Industry <- gsub("-","_",unique_mapping$Industry)
    unique_mapping$Corrected <- NULL
  
  #Mapping to ensure we dont produce duplicated deflators
    
    new_df <- left_join(new_df, unique_mapping)
    prev_df <- left_join(prev_df, unique_mapping)
    
    new_df1 <- filter(new_df, is.na(Unique_ind))
    prev_df1 <- filter(prev_df, is.na(Unique_ind))
    new_df1 <- select(new_df1, -Unique_asset, -Unique_ind)
    prev_df1 <- select(prev_df1, -Unique_asset, -Unique_ind)
    
    new_df <- filter(new_df, !is.na(Unique_ind))
    prev_df <- filter(prev_df, !is.na(Unique_ind))
    
    unique_mapping <- filter(unique_mapping, Industry == "ALL")
    unique_mapping <- select(unique_mapping, -Industry)
    
    new_df1 <- left_join(new_df1, unique_mapping)
    prev_df1 <- left_join(prev_df1, unique_mapping)
    
    new_df <- rbind(new_df, new_df1)
    prev_df <- rbind(prev_df, prev_df1)
    
    new_df <- select(new_df, -Industry, -Asset)	
    prev_df <- select(prev_df, -Industry, -Asset)
    
    new_df <- rename(new_df, Industry = Unique_ind, Asset = Unique_asset)
    prev_df <- rename(prev_df, Industry = Unique_ind, Asset = Unique_asset)    
    
    new_df <- aggregate(Value ~ Sector + Period + Industry + Asset, data = new_df, FUN = mean)
    prev_df <- aggregate(Value ~ Sector + Period + Industry + Asset, data = prev_df, FUN = mean)
    
  }
  
  # Tables
  
  # Changes data into annuals if specified 
  
  if (annual == TRUE){
    
    new_df$Period <- substr(new_df$Period, 2, 5)
    prev_df$Period <- substr(prev_df$Period, 2, 5)
    new_df <- aggregate(Value ~ Sector + Industry + Asset + Period,
                        data = new_df, FUN = sum)
    prev_df <- aggregate(Value ~ Sector + Industry + Asset + Period,
                        data = prev_df, FUN = sum)
  }
  
  #Change value to previous value so we can calculate differences 
  
  prev_df <- rename(prev_df, prev_value = Value)
  tables <- left_join(new_df, prev_df)
  tables$diff <- tables$Value - tables$prev_value
  
  #Removing columns we do not need
  
  tables_diff <- select(tables, -Value, -prev_value)
  tables_new <- select(tables, -diff, -prev_value)
  tables_prev <- select(tables, -diff, -Value)
  
  #Creating dataframe with periods as columns and turning NA into 0
  
  tables_diff <- spread(tables_diff, Period, diff)
  tables_new <- spread(tables_new, Period, Value)
  tables_prev <- spread(tables_prev, Period, prev_value)
  tables_diff[is.na(tables_diff)] <- 0
  
  #Calculating absolute differences and sorting by large differences
  
  tables_diff$abs_diff <- rowSums(abs(tables_diff[4:ncol(tables_diff)]))
  tables_diff <- tables_diff[order(-tables_diff$abs_diff),]
  tables_diff <- filter(tables_diff, abs_diff!=0)
  tables_diff$abs_diff <- NULL
  
  #Creating tables for new, old and differences
  #If differences in coverage exist creates a sheet that 
  #identifies these
  
  if (exists('differences')){
    
    write_xlsx(
      list(coverage = differences, 
           new = tables_new, prev = tables_prev, 
           diff = tables_diff),
      path = paste0(outputDir,"QA_", x_lab, "/", output_name,".xlsx"),
      col_names = TRUE,
      format_headers = TRUE
    )
    
  } else {
    
    write_xlsx(
      list(new = tables_new, prev = tables_prev, 
           diff = tables_diff),
      path = paste0(outputDir,"QA_", x_lab, "/", output_name,".xlsx"),
      col_names = TRUE,
      format_headers = TRUE
    )
    
  }
  
  # Graphs
  if (additivity=="additive"){
  tables_new1 <- tables_new
  tables_diff1 <- tables_diff
  tables_prev1 <- tables_prev
  
  tables_diff1$abs_diff <- rowSums(abs(tables_diff1[4:ncol(tables_diff1)]))
  tables_new1$concat <- paste0(tables_new1$Sector,tables_new1$Industry,tables_new1$Asset)
  tables_prev1$concat <- paste0(tables_prev1$Sector,tables_prev1$Industry,tables_prev1$Asset)
  tables_diff1$concat <- paste0(tables_diff1$Sector,tables_diff1$Industry,tables_diff1$Asset)
  large_diff <- filter(tables_diff1, abs_diff > limit)
  if(!nrow(large_diff)==0){
    pdf(paste0(outputDir, "QA_", x_lab, "/", output_name, " large differences.pdf"))
  
    
  for(x in 1:nrow(large_diff)){
    row <- large_diff[x,]
    concat <- row$concat
    new <- filter(tables_new1, concat == row$concat)
    prev <- filter(tables_prev1, concat == row$concat)
    
    new = subset(new, select = -c(Industry, Asset, concat) )
    new <- gather(new, "Quarter", "GFCF", 2:ncol(new)) 
    
    prev = subset(prev, select = -c(Industry, Asset, concat) )
    prev <- gather(prev, "Quarter", "GFCF", 2:ncol(prev))
    
    plot <-   ggplot() +
      geom_line(data = new, aes(x=Quarter,y=GFCF,color="new", group=1)) +
      geom_line(data = prev, aes(x=Quarter,y=GFCF,color="prev", group=1)) +
      labs(y = output_name, x = x_lab) +
      ggtitle(paste0(SHARED_INPUT_RUN_NO," - ", last_run," ", row$Sector, " Industry ", row$Industry, " ", row$Asset, " ", output_name)) +
      scale_color_manual(name="colors", values = c("new" = "red", "prev" = "blue"))+
      scale_x_discrete(breaks = new$Quarter[c(T, rep(F, 19))],
                       labels = new$Quarter[c(T, rep(F, 19))])+
      theme(legend.position = "top")
    
    print(plot)
  }
  }
  dev.off()
  }
  
  pdf(paste0(outputDir, "QA_", x_lab, "/", output_name, ".pdf"))
  
  #Separate coding for producing graphs for additive
  #and non-additive series
  
  if (additivity=="non-additive"){
    
    #Remove series with small differences
    
    tables_diff$abs_diff <- rowSums(abs(tables_diff[4:ncol(tables_diff)]))
    tables_diff <- filter(tables_diff, abs_diff>0.1)
    
    #Only runs if there are differences
    
    if (nrow(tables_diff)>0)  {
      
      series <- paste0(tables_diff$Sector, tables_diff$Industry, tables_diff$Asset)
    
      #Cleaning series by turning NA into 0
      #Making sure series are in the same order so we can
      #subtract one from the other
      
      tables_new$concat <- paste0(tables_new$Sector, tables_new$Industry, tables_new$Asset)
      tables_new[is.na(tables_new)] <- 0
      tables_new <- tables_new %>% filter(concat %in% series)
      tables_new <- tables_new[ order(match(tables_new$concat,series)), ]
      tables_new$concat <- NULL
    
      tables_prev$concat <- paste0(tables_prev$Sector, tables_prev$Industry, tables_prev$Asset)
      tables_prev[is.na(tables_prev)] <- 0
      tables_prev <- tables_prev %>% filter(concat %in% series)
      tables_prev <- tables_prev[ order(match(tables_prev$concat,series)), ]
      tables_prev$concat <- NULL
    
      for (i in 1:nrow(tables_new)){
      
        new <- tables_new[i,]
        prev <- tables_prev[i,]
        
        if (annual==FALSE) {
          
          publishTo <- gsub("X","Y",publishTo)
          
        } else {
          
          publishTo <- substr(publishTo, 2, 5)
        }
       
        
        end <- match(publishTo,names(new))
      
      # Start graph from where changes are greater than zero
      
        diff <- new[4:end] - prev[4:end]
        graph_from <- gather(diff, key = "Quarter", value = "Value")
        graph_from <- filter(graph_from, !(Value>-0.1 & Value<0.1))
      
        if (nrow(graph_from)>0){
        
          graph_from <- max(match(graph_from[1,1], names(new))-4,4)
        
          new <- cbind(new[1:3], new[graph_from:end])
          prev <- cbind(prev[1:3], prev[graph_from:end])
        
        }
      
        #Gather dataset to be used to graph
        
        new <- new %>% gather_(key = "Quarter", value = "Value", names(new[4:ncol(new)]))
        prev <- prev %>% gather_(key = "Quarter", value = "Value", names(prev[4:ncol(prev)]))
      
        #Make sure quarters are factors and values are numeric 
        
        new$Quarter <- as.factor(new$Quarter)
        prev$Quarter <- as.factor(prev$Quarter)
        new$Value <- as.numeric(new$Value)
        prev$Value <- as.numeric(prev$Value)
      
        if (anyNA(new$Value)){
          print("Problem",i)
        }
      
        #Graph depending period of differences
        #Only scale X discrete changes below (X axis, make sure x axis
        #labels are legible
        
        if (nrow(new)>39){
          
          #Line graph for new and old 
        
          plot <-   ggplot() +
            geom_line(data=new, aes(x=Quarter,y=Value,color="new"), group=1) +
            labs(y = output_name, x = x_lab) +
            geom_line(data=prev, aes(x=Quarter,y=Value,color="prev"), group=1) +
            ggtitle(paste0("New v Prev. Sector: ", new$Sector, "Industry: ",new$Industry,"Asset: ",new$Asset))+
            scale_color_manual(name="colors", values = c("prev" = "red", "new" = "blue"))+
            theme(legend.position = "top")+
            scale_x_discrete(breaks = levels(new$Quarter)[c(T, rep(F, 19))])
        
        } 
      
        if (nrow(new)>7 & nrow(new)<40){
        
          plot <-   ggplot() +
            geom_line(data=new, aes(x=Quarter,y=Value,color="new"), group=1) +
            labs(y = output_name, x = x_lab) +
            geom_line(data=prev, aes(x=Quarter,y=Value,color="prev"), group=1) +
            ggtitle(paste0("New v Prev. Sector: ", new$Sector, "Industry: ",new$Industry,"Asset: ",new$Asset))+
            scale_color_manual(name="colors", values = c("prev" = "red", "new" = "blue"))+
            theme(legend.position = "top")+
            scale_x_discrete(breaks = levels(new$Quarter)[c(T, rep(F, 3))])
        
        }
      
        if (nrow(new)<8){
        
          plot <-   ggplot() +
            geom_line(data=new, aes(x=Quarter,y=Value,color="new"), group=1) +
            labs(y = output_name, x = x_lab) +
            geom_line(data=prev, aes(x=Quarter,y=Value,color="prev"), group=1) +
            ggtitle(paste0("New v Prev. Sector: ", new$Sector, "Industry: ",new$Industry,"Asset: ",new$Asset))+
            scale_color_manual(name="colors", values = c("prev" = "red", "new" = "blue"))+
            theme(legend.position = "top")
        
        }
      
        print(plot)
      
      }
      
    }
    
  }
  
  if (additivity=="additive"){
    
    #Read in QA map to ensure we don't have too many variables 
    #(eg 64 industries)
    
    if (file.exists(paste0(inputDir,"Mapping & Aggregation/QA_map.csv"))) {
      
      QA_map <- read.csv(paste0(inputDir, "Mapping & Aggregation/QA_map.csv"), stringsAsFactors=FALSE ) 
      
    
    } else {
      
      QA_map <- read.csv(paste0("J:/Annual round/RUN_01/Inputs/Mapping & Aggregation/QA_map.csv"), stringsAsFactors=FALSE ) 
      
    }
    
    #First 2 columns relate to industry mappings
    #3 & 4 relate to asset 
    
    
    ind_map <- QA_map[1:2]
    ass_map <- QA_map[3:4]
    
    #Read in industry mapping
    
    if (file.exists(paste0(inputDir, "Mapping & Aggregation/SIC_Industry_mapping.rds"))) {
      
      ind_fix <- read_rds(paste0(inputDir, "Mapping & Aggregation/SIC_Industry_mapping.rds")) 
      
      
    } else {
      
      ind_fix <- read_rds(paste0("J:/Annual round/RUN_01/Inputs/Mapping & Aggregation/SIC_Industry_mapping.rds"))
      
    }
    
    
    ind_map <- left_join(ind_map,ind_fix)
    ind_map$Industry <- ifelse(is.na(ind_map$Corrected),
                               ind_map$Industry,ind_map$Corrected)
    ind_map$Industry <- gsub("-","_",ind_map$Industry)
    ind_map$Corrected <- NULL
    
    #Turn all NA to 0
    
    tables_new[is.na(tables_new)] <- 0
    tables_prev[is.na(tables_prev)] <- 0
    tables_diff[is.na(tables_diff)] <- 0
    
    #Turn S.11PR to S1100P to ensure consistent naming
    
    tables_new$Sector[tables_new$Sector=="S.11PR"] <- "S1100P"
    tables_prev$Sector[tables_prev$Sector=="S.11PR"] <- "S1100P"
    tables_diff$Sector[tables_diff$Sector=="S.11PR"] <- "S1100P"
    
    #Removing full stops in sector
    
    tables_new$Sector <- gsub("\\.","",tables_new$Sector)
    tables_prev$Sector <- gsub("\\.","",tables_prev$Sector)
    tables_diff$Sector <- gsub("\\.","",tables_diff$Sector)
    
    #Remove sub categories for S12
    
    tables_new$Sector[substr(tables_new$Sector,1,3)=="S12"] <- "S12"
    tables_prev$Sector[substr(tables_prev$Sector,1,3)=="S12"] <- "S12"
    tables_diff$Sector[substr(tables_diff$Sector,1,3)=="S12"] <- "S12"
    
    #Convert to top level COFOG
    
    tables_prev$Industry <- ifelse(substr(tables_prev$Industry,1,2)=="GF",substr(tables_prev$Industry,1,4),tables_prev$Industry)
    tables_prev <- left_join(tables_prev, ind_map)
    
    #If industry has not mapped, use orginal industry
    
    tables_prev$Ind_QA <- ifelse(is.na(tables_prev$Ind_QA),tables_prev$Industry,tables_prev$Ind_QA)
    tables_prev$Industry <- NULL
    tables_prev <- rename(tables_prev, Industry = Ind_QA)
    
    #Join assets to QA asset mapping 
    
    tables_prev <- left_join(tables_prev, ass_map)
    tables_prev$Asset <- NULL
    tables_prev <- rename(tables_prev, Asset = Ass_QA)
    
    #Complete same steps for table_new
    
    tables_new$Industry <- ifelse(substr(tables_new$Industry,1,2)=="GF",substr(tables_new$Industry,1,4),tables_new$Industry)
    tables_new <- left_join(tables_new, ind_map)
    tables_new$Ind_QA <- ifelse(is.na(tables_new$Ind_QA),tables_new$Industry,tables_new$Ind_QA)
    tables_new$Industry <- NULL
    tables_new <- rename(tables_new, Industry = Ind_QA)
    tables_new <- left_join(tables_new, ass_map)
    tables_new$Asset <- NULL
    tables_new <- rename(tables_new, Asset = Ass_QA)
    
    #Complete same steps for table_diff
    
    tables_diff$Industry <- ifelse(substr(tables_diff$Industry,1,2)=="GF",substr(tables_diff$Industry,1,4),tables_diff$Industry)
    tables_diff <- left_join(tables_diff, ind_map)
    tables_diff$Ind_QA <- ifelse(is.na(tables_diff$Ind_QA),tables_diff$Industry,tables_diff$Ind_QA)
    tables_diff$Industry <- NULL
    tables_diff <- rename(tables_diff, Industry = Ind_QA)
    tables_diff <- left_join(tables_diff, ass_map)
    tables_diff$Asset <- NULL
    tables_diff <- rename(tables_diff, Asset = Ass_QA)
    
    #Loop through sectors to produce charts for each
    
    for (s in unique(tables_new$Sector)){
      
      # Total by sector
      
      if ( !"Total" %in% tables_new$Asset) {
        
        #Where totals dont exist, sum up data to get totals
         
        total_new <- filter(tables_new, Sector==s)
        total_new <- select(total_new, -Asset, -Industry)
        total_new <- aggregate(. ~ Sector, data=total_new, FUN=sum)
        total_new <- gather(total_new, "Quarter", "GFCF", 2:ncol(total_new))
        
        total_prev <- filter(tables_prev, Sector==s)
        total_prev <- select(total_prev, -Asset, -Industry)
        total_prev <- aggregate(. ~ Sector, data=total_prev, FUN=sum)
        total_prev <- gather(total_prev, "Quarter", "GFCF", 2:ncol(total_prev)) 
        
      } else {
        
        #Create totals 
        
        total_new <- filter(tables_new, Asset=="Total"& Industry=="Total")
        total_new <- select(total_new, -Asset, -Industry)
        
        total_prev <- filter(tables_prev, Asset=="Total"& Industry=="Total")
        total_prev <- select( total_prev, -Asset, -Industry)
        
        #Remove unmapped series
        
        tables_new <- filter(tables_new, Asset!=""& Industry!="")
        tables_prev <- filter(tables_prev, Asset!=""& Industry!="")
        
        
      }
      
      #Remove folder address from run name
    
      last_run_name <- gsub("J:/Quarterly round/", "",last_run)
      last_run_name <- gsub("J:/Annual round/", "",last_run)
      
      #Produce line graph for new and old
      
      plot <-   ggplot() +
        geom_line(data=total_prev, aes(x=Quarter,y=GFCF,color="prev", group=1)) +
        labs(y = output_name, x = x_lab) +
        geom_line(data=total_new, aes(x=Quarter,y=GFCF,color="new", group=1)) +
        ggtitle(paste0(SHARED_INPUT_RUN_NO," - ", last_run_name,"Sector:",s))+
        scale_color_manual(name="colors", values = c("new" = "red", "prev" = "blue"))+
        scale_x_discrete(breaks = total_new$Quarter[c(T, rep(F, 19))],
                         labels = total_new$Quarter[c(T, rep(F, 19))])+
        theme(legend.position = "top")
      
      print(plot)
      
      #Run loop to produce graphs for asset and industry
      
      for (t in c("Asset","Industry")){
        
        if (t=="Asset"){
          
          #Remove sectors not in loop and industry
          
          diff <- filter(tables_diff, Sector==s)
          diff <- select(diff, -Industry)
          
          #If there are no differences skip graph, otherwise
          #aggregate by sector and asset
          
          
          if (nrow(diff)>0) {
            
          diff <- aggregate(. ~ Sector+Asset, data=diff, FUN=sum)
          diff <- gather(diff, "Quarter", "GFCF", 3:ncol(diff))
          
          } else { 
            
            next
            
          }
          
          #Create total by aggregating asset breakdown
          
          diff_tot <- select(diff, -Asset)
          diff_tot <- aggregate(. ~ Sector+Quarter, data=diff_tot, FUN=sum)
          
        }
        
        if (t=="Industry"){
          
          #Filter by sector and remove asset column
          
          diff <- filter(tables_diff, Sector==s)
          diff <- select(diff, -Asset)
          
          #If there are no differences skip graph, otherwise
          #aggregate by sector and industry
          
          
          if (nrow(diff)>0) {
            
          diff <- aggregate(. ~ Sector+Industry, data=diff, FUN=sum)
          diff <- gather(diff, "Quarter", "GFCF", 3:ncol(diff))
          
          } else { 
            
            next
            
          }
          
          #Create total by aggregating industry breakdown
          
          
          diff_tot <- select(diff, -Industry)
          diff_tot <- aggregate(. ~ Sector+Quarter, data=diff_tot, FUN=sum)
          
        }
        
        # Start graph from where changes are greater than zero
        
        graph_from <- filter(diff_tot, !(GFCF>-0.1 & GFCF<0.1))
        graph_fromQ <- graph_from$Quarter
        
        if (nrow(graph_from)>0){
          
          graph_from <- match(graph_from$Quarter[1], diff_tot$Quarter)
          diff_tot <- diff_tot[graph_from:nrow(diff_tot),]
          #diff <- diff[graph_from:nrow(diff),]
          diff <- filter(diff, Quarter >= graph_fromQ)
          
        }
        
        #Removing directory from last run
        
        last_run_name <- gsub("J:/Quarterly round/", "",last_run)
        last_run_name <- gsub("J:/Annual round/", "",last_run)
        
        if (t=="Asset"){
          
          #Produces stacked columns for asset breakdown and a line 
          #for the total
          
          plot <-   ggplot() +
            geom_col(data = diff , aes ( x = Quarter , y = GFCF, fill=Asset,width = 0.5)) +
            labs(y = output_name, x = x_lab) +
            geom_line(data= diff_tot , aes(x=Quarter, y=GFCF), group=1) +
            ggtitle(paste0(SHARED_INPUT_RUN_NO," - ", last_run_name,"Sector:",s))+
            scale_x_discrete(breaks = total_new$Quarter[c(T, rep(F, 19))],
                             labels = total_new$Quarter[c(T, rep(F, 19))])+
            theme(legend.position = "top")
          
        }
        
        if (t=="Industry"){
          
          #Produces stacked columns for industry breakdown and a line 
          #for the total
          
          plot <-   ggplot() +
            geom_col(data = diff , aes ( x = Quarter , y = GFCF, fill=Industry, width = 0.5)) +
            labs(y = output_name, x = x_lab) +
            geom_line(data= diff_tot , aes(x=Quarter, y=GFCF), group=1) +
            ggtitle(paste0(SHARED_INPUT_RUN_NO," - ", last_run_name,"Sector:",s))+
            scale_x_discrete(breaks = total_new$Quarter[c(T, rep(F, 19))],
                             labels = total_new$Quarter[c(T, rep(F, 19))])+
            theme(legend.position = "top")
          
        }
        
        print(plot)
        
      }
      
    }
    
  }
  
  dev.off()
  
}

