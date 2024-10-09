# Additivity function
setwd("D:/Current code") # Where other code is
outputFolder <- "D:/Current code/" # Folder Where charts will be saved

# This only works when only period columns start in 'Y'

#Example Additivity_check(Stock, Industry, c('B', 'C', 'D', 'E'), "BTE",1)

df <- read.csv("J:/Annual round/RUN_21/Outputs/Pub table 2/T2000.csv")
column <- "INSTR_ASSET"
aggregation <- "N11G"
components <- c('N111G', 'N112G', 'N1131G', 'N11321G', 'N11322G', 'N11OG', 'N115G', 'N117G')
tolerance <- 1

df <- select(df, -X)

Additivity_check(df, "INSTR_ASSET", c('N111G', 'N112G', 'N1131G', 'N11321G', 'N11322G', 'N11OG', 'N115G', 'N117G'), "N11G", 1)

Additivity_check(df, "INSTR_ASSET", c('N111G', 'N112G', 'N1131G', 'N1132G', 'N11OG', 'N115G', 'N117G'), "N11G", 1)

Additivity_check(df, "INSTR_ASSET", c('N111N', 'N112N', 'N1131N', 'N1132N', 'N11ON', 'N115N', 'N117N'), "N11N", 1)

Additivity_check(df, "INSTR_ASSET", c('N11321N', 'N11322N'), "N1132N", 1)

Additivity_check(df, "INSTR_ASSET", c('N11321G', 'N11322G'), "N1132G", 1)

Additivity_check(df, "ACTIVITY", c('O', 'P', 'Q'), "OTQ", 1)

Additivity_check(df, "ACTIVITY", c('B', 'C', 'D', 'E'), "BTE", 1)

Additivity_check(df, "ACTIVITY", c('A01', 'A02', 'A03'), "A", 1)

Additivity_check(df, "ACTIVITY", c('C16', 'C17', 'C18'), "C16T18", 1)

Additivity_check(df, "ACTIVITY", c('C22', 'C23'), "C22_23", 1)

Additivity_check(df, "ACTIVITY", c('C24', 'C25'), "C24_25", 1)

Additivity_check(df, "ACTIVITY", c('C29', 'C30'), "C29_30", 1)

Additivity_check(df, "ACTIVITY", c('C', 'C10T12', 'C13T15', 'C16T18', 'C19', 'C20',
                                   'C21', 'C22_23', 'C24_25', 'c26', 'C27', 'C28', 'C29_30', 'C31T33'), "C", 1)

Additivity_check(df, "ACTIVITY", c('E36', 'E37_39'), "E", 1)

Additivity_check(df, "ACTIVITY", c('G45', 'G46', 'G47'), "G", 1)

Additivity_check(df, "ACTIVITY", c('G', 'H', 'I'), "GTI", 1)

Additivity_check(df, "ACTIVITY", c('H49', 'H50', 'H51', 'H52', 'H53'), "H", 1)

Additivity_check(df, "ACTIVITY", c('J58', 'J59_60'), "J58T60", 1)

Additivity_check(df, "ACTIVITY", c('J58T60', 'J61','J62_63'), "J", 1)

Additivity_check(df, "ACTIVITY", c('K64', 'K65','K66'), "K", 1)

Additivity_check(df, "ACTIVITY", c('M', 'N'), "M_N", 1)

Additivity_check(df, "ACTIVITY", c('M69_70', 'M71'), "M69T71", 1)

Additivity_check(df, "ACTIVITY", c('M73', 'M74_75'), "M73T75", 1)

Additivity_check(df, "ACTIVITY", c('M69T71', 'M73T75'), "M", 1)

Additivity_check(df, "ACTIVITY", c('N77', 'N78', 'N79', 'N80T82'), "N", 1)

Additivity_check(df, "ACTIVITY", c('O', 'P', 'Q'), "OTQ", 1)

Additivity_check(df, "ACTIVITY", c('Q86', 'Q87_88'), "Q", 1)

Additivity_check(df, "ACTIVITY", c('R', 'S', 'T', 'U'), "R_U", 1)

Additivity_check(df, "ACTIVITY", c('R90t92', 'R93'), "R", 1)

Additivity_check(df, "ACTIVITY", c('S94', 'S95', 'S96'), "S", 1)

Additivity_check(df, "ACTIVITY", c('A', 'BTE', 'F', 'GTI','J', 'K', 'L', 'M_N', 'OTQ', 'R_U'), "_T", 1)


Additivity_check <- function (df, column, components, aggregation, tolerance = 1, outputFolder = getwd()) {
  
  #filter total and aggregate to get total
  
  if (column == "ACTIVITY"){
  
    aggregation_df <- df %>% filter(ACTIVITY == aggregation)  
    components_df <- df  %>% filter(ACTIVITY %in% components)
    components_df <-  aggregate(.~PRICES+INSTR_ASSET, components_df, sum)
    components_df[column] <- aggregation 
    
  }
 
  
  if (column == "INSTR_ASSET"){
    
    aggregation_df <- df %>% filter(INSTR_ASSET == aggregation) 
    components_df <- df %>% filter(INSTR_ASSET%in% components)
    components_df <-  aggregate(.~ACTIVITY+PRICES, components_df, sum)
    components_df[column] <- aggregation 
    
    
  }
  
  #filter components and aggregate to get total
  
  
  # reshape dataframes - this requires only period columns starting in y
  
  period_columns <- names(df)
  period_columns <- period_columns[substr(period_columns,1,1)=="X"]
  
  aggregation_df <- gather(aggregation_df, "Period", "Value_agg", 4:29)
  components_df <- gather(components_df, "Period", "Value_comp", 4:29)
  
  # Join dfs
  
  merged_df <- left_join(aggregation_df, components_df)
  merged_df$check <- merged_df$Value_agg-merged_df$Value_comp
  merged_df <- filter(merged_df, abs(merged_df$check)>tolerance)
  
 # Write out failures
  
  if (nrow(merged_df)>0){
    
    write.csv(merged_df, paste0(outputFolder, "/agg_check_fail_on", aggregation, ".csv"))
    
  }
  else{
    print("No additivity failures")
  }
  
}

