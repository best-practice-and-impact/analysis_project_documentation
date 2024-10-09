annualiseData <- function(dat)
{
  #dat <- chainedUnnest
  # SOMETIMES THE DATA WILL HAVE THESE COLUMNS OTHER WONT. PUT IN A TRY CATCH SO IT SKIPS OVER IF THE COLUMNS DO NOT EXISTS
  tryCatch(
    {
       dat <- dat %>% select(-Sector_Level, -Industry_Level, -Asset_Level, -Group, -Year)
    }, warning = function(war) {
    }, error = function(err) {
    }, finally = {})

  namesToRemove <- c("Sector", "Industry", "Asset", "Measure", "Period" )
  requiredVariables <- c(names(dat) [! names(dat) %in% namesToRemove])
  dat <- dat %>% gather_(key = "Measure", value = "Value", requiredVariables)

  dat$Value <- round(dat$Value,0)

  namesToRemove <- c("Sector", "Industry", "Asset", "Measure" )
  requiredVariables <- c(names(dat) [! names(dat) %in% namesToRemove])
  ################################################################################################################################################
  # Variables that only need the 4th Quarter for the annual figures. Cannot get this to work as a shortcut at the moment
  varsThatNeedQ4ForAnnual <- c("GrossStockCVM", "GrossStockCP", "GrossStockPYP",
                               "NetStockCVM", "NetStockCP", "NetStockPYP",
                               "ProductiveStockCVM", "ProductiveStockCP", "ProductiveStockPYP")
  ################################################################################################################################################

  print("Variables Set Up")

  ################################################################################################################################################
  # Find where all the NA and NaN values are (TRUE or FALSE Matrix)
  naVals <- apply(dat, 2, is.na)
  nanVals <- apply(dat, 2, is.nan)
  # Set all the NA and NaN values to 0
  dat[naVals] <- 0
  dat[nanVals] <- 0
  # Remove variables from memory
  rm(naVals, nanVals)

  print("NAs and NANs Removed")

  dat$Year <- as.numeric(substr(dat$Period,2,5))
  ################################################################################################################################################
  # Get Annualised Data for variables that take Q4 as the annualised figure

  # Get Quarter 4 figures for all Measures and years and filter out measures not needed
  anQtr <- dat[grepl('Q4', dat$Period, fixed=TRUE),] %>%
    select(Sector, Industry, Asset, as.numeric(Period), Measure, Year, Value) %>%
    filter(Measure %in% varsThatNeedQ4ForAnnual) %>%
    filter(!(Measure %in% c('PriceIndex')))

  # Column now holds all Q4s for the year so replace with just the year as the column is still needed for the rest of the model
  anQtr$Period <- anQtr$Year

  print("Quarter 4 data calculated")
  ############################################################################################################################################
  # Get Annualised Data, only variables that need all 4 Qtrs summed for annualised figures

  # Sum up all of the quarters for each year and measure and filter out measures not needed
  anSum <- dat %>% select(Sector, Industry, Asset, Measure, Year, Value) %>%
    group_by(Sector, Industry, Asset, Measure, Year) %>%
    summarise(Value = sum(Value)) %>%
    filter(!(Measure %in% varsThatNeedQ4ForAnnual)) %>%
    filter(!(Measure %in% c('PriceIndex')))

  # If using limited variables then this ends up with no data but also no year column! check and add one in iff needed ready fro the SQL join
  if (nrow(anSum) == 0) {
    anSum$Year <- "" # Array does not include the column year so create an empty one first just case the Array is empty
  }
  anSum <- anSum %>% mutate(Period = Year)

  print("All Quarter data summed")
  ############################################################################################################################################
  # Get Annualised Data, only variables that need all 4 Qtrs averaged

  # Average all quarters for each year and measure and filter out measures not needed
  anAvg <- dat %>% select(Sector, Industry, Asset, as.numeric(Period), Measure, Year, Value) %>%
    group_by(Sector, Industry, Asset, Measure, Year) %>%
    summarise(Value = mean(Value)) %>%
    filter(Measure == 'PriceIndex')
  # Dataset does not have a period column , which is needed later on, add one and give it the value from the year column
  anAvg <- anAvg %>% mutate(Period = Year)

  print("Averaged data calculated")
  ############################################################################################################################################

  # Join the three datasets together. Cannot get Merge to work!!!!!
  dat <- sqldf("SELECT Sector, Industry, Asset, Period, Measure, Value FROM anQtr
               UNION
               SELECT Sector, Industry, Asset, Period, Measure, Value FROM anSum
               UNION
               SELECT Sector, Industry, Asset, Period, Measure, Value FROM anAvg
               ")

  print("Three data sets Joined")

  rm(anQtr, anSum, anAvg)
  ############################################################################################################################################

  # Just a check for multiple same entries
  dat <- sqldf("SELECT * FROM dat GROUP BY Sector, Industry, Asset, Period, Measure")

  ############################################################################################################################################

  # Spread the data on Measure ready for nesting
  dat <- dat %>% spread(Measure, Value, fill = 0, convert = TRUE)
  ############################################################################################################################################
  tryCatch(
    {
        print("CHANGING CVMs AND PYPs")
        #### CHAINING NOT CURRENTLY WORKING PROPERLY, I THINK DUE TO THE BENCHMARKING IN THE CHAIN METHOD, SEEMS TO BREAK THE CHAINS SO TO SPEAK!
        #### SET CVM FOR REFYEAR TO CP
        dat$ConsumptionOfFixedCapitalCVM[dat$Period == refYear] <- dat$ConsumptionOfFixedCapitalCP[dat$Period == refYear]
        dat$NetStockCVM[dat$Period == refYear] <- dat$NetStockCP[dat$Period == refYear]
        dat$GrossStockCVM[dat$Period == refYear] <- dat$GrossStockCP[dat$Period == refYear]
        #### SET PYP FOR REFYEAR + 1 TO CVM
        dat$ConsumptionOfFixedCapitalPYP[dat$Period == refYear + 1] <- dat$ConsumptionOfFixedCapitalCVM[dat$Period == refYear + 1]
        dat$NetStockPYP[dat$Period == refYear + 1] <- dat$NetStockCVM[dat$Period == refYear + 1]
        dat$GrossStockPYP[dat$Period == refYear + 1] <- dat$GrossStockCVM[dat$Period == refYear + 1]
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))

    }, error = function(err) {
      print(paste("chainDataSkippingErrors - MY_ERROR:  ",err))

    }, finally = {

    })
  ############################################################################################################################################
  print("ANNUALISATION FINISHED")
  return(dat)
}

###################################################################################################################################################
###################################################################################################################################################

chainDataSkippingErrors <- function(cDat, benchType = 4, refYear, correct_CVM)
{
  tryCatch(
  {
    #cDat <- toChain
    source("chainingScript.R")
    
    if (correct_CVM==TRUE){
      
      pairs <- tribble(
        ~PYP, ~CYP, ~ChainType,
        "ConsumptionOfFixedCapitalPYP", "ConsumptionOfFixedCapitalCYP", "Flow",
        "GrossStockPYP", "GrossStockCYP", "Stock",
        "NetStockPYP", "NetStockCYP", "Stock")
      
    } else {
      
      pairs <- tribble(
        ~PYP, ~CYP, ~ChainType,
        "ConsumptionOfFixedCapitalPYP", "ConsumptionOfFixedCapitalCYP", "Stock",
        "GrossStockPYP", "GrossStockCYP", "Stock",
        "NetStockPYP", "NetStockCYP", "Stock")
      
    }

      #"CapitalServicesPYP","CapitalServicesCYP", "Stock",
      #"ProductiveStockPYP","ProductiveStockCYP", "Stock")

    # Add a Year column for the chain function
    cDat <- cDat %>% mutate(Year = as.numeric(substring(Period, 2, 5)))

    #theGroups <- sqldf("SELECT [Group], COUNT(*) FROM cDat GROUP BY [Group]")
    theGroups <- sqldf("SELECT Sector, COUNT(*) AS cnt FROM cDat GROUP BY Sector")
    print(paste0("Their are ", toString(nrow(theGroups)), " Groups to Chain"))
    #i <- 1
    for (i in 1:nrow(theGroups))
    {
       #toChain2 <- cDat %>% filter(Group == theGroups[i,1])
       toChain2 <- cDat %>% filter(Sector == theGroups[i,1])

       stopifnot(lastCompleteYear %in% toChain2$Year)

       # ----------------------- Chain all CYP/PYP Pairs -------------------------------
       # chainAll expects Sector/Industry/Asset labels and all data nested as "data"
       toChain2 <- toChain2 %>% group_by(Sector, Industry, Asset) %>% nest(.key = data)

       if (nrow(toChain2) > 0)
       {
         #flog.info(paste0("Loop: ", toString(i), "/", toString(nrow(theGroups)), " - ", "Starting chain of Group: ", theGroups[i,1], " - Number of Rows: ", theGroups[i,2]))
          flog.info(paste0("Loop: ", toString(i), "/", toString(nrow(theGroups)), " - ", "Starting chain of Sector: ", theGroups[i,1], " - Number of Rows: ", theGroups[i,2]))

         if (correct_CVM==TRUE){

            print('correct_CVM')
            chained <- capstock::chainAll(toChain2, pairs = pairs, lastCompleteYear = lastCompleteYear, parallelise = TRUE, benchType)

         }

         if (correct_CVM==FALSE){

           print('incorrect_CVM')
           chained <- capstock::chainAll_old(toChain2, pairs = pairs, lastCompleteYear = refYear, parallelise = TRUE, benchType)

         }

         cat("\n")
         flog.info("Chain complete.")

         if (!(exists("chainedOut")))
         {
           chainedOut <- chained
         }
         else {
           chainedOut <- bind_rows(chainedOut, chained)
         }
         rm(toChain2, chained)
         #flog.info(paste0("Out of misc Loop"))
       } else{
         flog.info(paste0("No data for Group: ", theGroups[i,1]))
       }
    }
  }, warning = function(war) {
    print(paste("MY_WARNING:  ",war))

  }, error = function(err) {
    print(paste("chainDataSkippingErrors - MY_ERROR:  ",err))

  }, finally = {

  })

  return(chainedOut)
}

###################################################################################################################################################
###################################################################################################################################################
replicateCORDOuputQACHK01or05or06 <- function(dat, oneORfive)
{

  tryCatch(
  {
    namesToRemove <- c("Sector", "Industry", "Asset", "Measure", "Period" )
    requiredVariables <- c(names(dat) [! names(dat) %in% namesToRemove])
    dat <- dat %>% gather_(key = "Measure", value = "Value", requiredVariables)

    dat$Value <- round(dat$Value,0)

    # ADD SOME COLUMNS TO MATCH CORD OUTPUT
    dat$Price <- ifelse(grepl('CP', dat$Measure, fixed=TRUE), 'CP', ifelse(grepl('CVM', dat$Measure), 'CVM', ifelse(grepl('PYP', dat$Measure), 'PYP','NA')))
    dat$Measure2 <- ifelse(grepl('Con', dat$Measure, fixed=TRUE), 'CC', ifelse(grepl('Gross', dat$Measure), 'GS', ifelse(grepl('Net', dat$Measure), 'NS','NA')))
    dat <- dat %>% filter(Price != 'NA', Measure2 != 'NA')
    #dat$Total <- "Total"    #### NOT SURE WHAT THESE TWO COLUMNS ARE FOR OR WHAT THEY MEAN BUT JUST ADDING FOR NOW
    dat$Used <- "USED"
    dat$AnnQtr <- "A"

    # NEED TO REORDER THE COLUMNS TO MATCH CORD. THESE COLUMNS NEED REORDERING
    reorderNames <- c('Price', 'Measure2', 'Sector', 'Asset', 'Measure', 'Industry', 'Used', 'AnnQtr')
    # THESE COLUMNS ARE ALL THE DATES AND DO NOT NEED REORDERING
    yrQtrVars <- c(names(dat) [! names(dat) %in% reorderNames])
    # JOIN UP THE TWO COLUMN NAME VECTORS
    reorderList <- c(reorderNames, yrQtrVars)
    # REORDER THE DATA, AND ALSO FILTER OUT ANY ROWS THAT WERE GIVEN THE VALUE NA WHEN ADDING COLUMNS ABOVE
    dat <- dat[,reorderList] %>% filter(!(Measure2 %in% c('NA')))

    # THESE COLUMNS NOT NEEDED SO REMOVE
    dat$Measure <- NULL

    if (oneORfive == 5)
    {
        # ONLY NEED CP DATA FOR THIS OUTPUT
        dat <- dat %>% filter(Price == 'CP')
    } else if (oneORfive == 6) {
        # ONLY NEED CP DATA FOR THIS OUTPUT
        dat <- dat %>% filter(Measure2 == 'CC')
    }

    # GETTING MULTIPLE ROWS WITH THE SAME DATA SO GROUP TOGETHER!!!!!!!
    dat <- sqldf("SELECT * FROM dat GROUP BY Price, Measure2, Sector, Asset, Industry, Used, AnnQtr, Period, Value")
    dat <- dat %>% spread(Period, Value, fill = 0)

    # ADD A COLUMN WITH THE ROW NUMBER
    dat <- tibble::rowid_to_column(dat, "ID")
    dat <- dat %>% mutate(ID2 = as.numeric(ID))

    # NEED TO REORDER THE COLUMNS AGAIN
    if (oneORfive == 1)
    {
        reorderNames <- c('ID2', 'Price', 'Measure2', 'Sector', 'Asset', 'Industry', 'Used', 'AnnQtr', 'ID')
    } else if (oneORfive == 5) {
        reorderNames <- c('ID2', 'Sector', 'Asset', 'Industry', 'Price', 'Used', 'Measure2', 'AnnQtr', 'ID')
    } else if (oneORfive == 6) {
        dat <- dat %>% filter(!(Price == 'NA'))
        reorderNames <- c('ID2', 'Sector', 'Asset', 'Industry', 'Price', 'Used', 'Measure2', 'AnnQtr', 'ID')
    }


    # THESE COLUMNS ARE ALL THE DATES AND DO NOT NEED REORDERING
    yrQtrVars <- c(names(dat) [! names(dat) %in% reorderNames])
    # JOIN UP THE TWO COLUMN NAME VECTORS
    reorderList <- c(reorderNames, yrQtrVars)
    # REORDER THE DATA, AND ALSO FILTER OUT ANY ROWS THAT WERE GIVEN THE VALUE NA WHEN ADDING COLUMNS ABOVE
    dat <- dat[,reorderList] %>% filter(!(Measure2 %in% c('NA')))

    naVals <- apply(dat, 2, is.na)
    dat[naVals] <- '-'
    # Remove variables from memory
    rm(naVals)

   if (oneORfive == 6) {
      dat <- dat %>% select(-Used, -Measure2, -AnnQtr)
   }

    # ADD A NUMBER OF ROWS TO MATCH TOP OF CORD OUTPUT
    rowsToCreate <- 8
    m <- 0
    while (m > -rowsToCreate) {
        dat[nrow(dat)+1,] <- ""
        dat[nrow(dat),1] <- m
        m = m - 1
    }

    # ADD THE YEAR/QUARTER TO RELEVANT ROW AS A COLUMN HEADING
    dat[nrow(dat) - (rowsToCreate -1),2:ncol(dat)] <- colnames(dat[,2:ncol(dat)])
    # DELETE THE FIRST FEW AS NOT NEEDED
    dat[nrow(dat) - (rowsToCreate - 1),2:9] <- ""

    # ADD A/Q TO RELEVANT ROW AS A COLUMN HEADING
    dat[nrow(dat) - (rowsToCreate - 2),2:ncol(dat)] <- "A"
    # DELETE THE FIRST FEW AS NOT NEEDED
    dat[nrow(dat) - (rowsToCreate - 2),2:9] <- ""

    dat[nrow(dat),2] <- "Capstock R Model output"
    if (oneORfive == 1)
    {
        dat[nrow(dat) - 3,2] <- "Called by R script 80_OutoutForQA.R. Built to Mimic CORD output QACHK01_RUN_ON_RUN"
    }
    else if (oneORfive == 5){
        dat[nrow(dat) - 3,2] <- "Called by R script 80_OutoutForQA.R. Built to Mimic CORD output QACHK05_NEGATIVE_CAPSTOCK_VALUES"
    }
    else if (oneORfive == 6){
        dat[nrow(dat) - 3,2] <- "Called by R script 80_OutoutForQA.R. REPLICATION OF SPREADSHEET QACHK06_T0301_DATA_DELIVERY_for_EUROSTAT"
    }

    # REORDER THE ROWS AND THEN SELECT EVERTHING BAR THE ID COLUMNS
    dat <- dat[order(as.numeric(dat$ID2)), , drop = FALSE]
    dat <- dat %>% select(-ID2, -ID)

    # WRITE FORMATTED DATA OUT TO A CSV WITHOUT COLUMN AND ROW NAMES
    if (OUTPUT_SHARING==TRUE)
    {
    write.table(dat, file = paste0(otherOutputs, "QACHK0", toString(oneORfive), "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
    } else {
      write.table(dat, file = paste0(outputDir, "QACHK0", toString(oneORfive), "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
    }
    # DO SOME CLEANING UP
    rm(reorderNames, yrQtrVars, reorderList)
  }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))

  }, error = function(err) {
    print(paste("MY_ERROR:  ",err))

  }, finally = {

  })
  # SEND BACK THE SORTED AND REARRANGED DATA
  return(dat)
  rm(dat)
}

##############################################################################################################################################################
# THIS FUNCTION REPLICATES THE 2ND CORD OUTPUT EXCEL FILE QACHKO2
# BOTH ANNUAL AND QUARTERLY FIGURES ARE NEEDED FIR THE OUTPUT
# ORDERING ROWS WOULD NOT WORK LIKE THE FUNTION ABOVE SO DONE SLIGHTLY DIFFERENTLY
replicateCORDOuputQACHK02and03or07 <- function(datAnn, datQrt, twoORthree)
{
  tryCatch(
    {
      #datAnn <- QACH2ann
      #datQrt <- QACH2qtr
      #twoORthree <- 7
      datQrt <- sqldf("SELECT * FROM datQrt GROUP BY Sector, Industry, Asset, Period")
      datAnn <- sqldf("SELECT * FROM datAnn GROUP BY Sector, Industry, Asset, Period")

      print("GETTING RID OF CYC DATA")
      # DONT NEED VARIABLES ENDING IN CYP, CREATE AN ARRAY OF THEM
      nmesQ <- names(datQrt)[grep("CYP", names(datQrt))]
      nmesA <- names(datAnn)[grep("CYP", names(datAnn))]
      # VARIABLES THAT ARE NOT NEEDED TO CHECK AGAINST MEASURE
      namesToRemove <- c("Sector", "Asset", "Industry", "Period", "refYear", "Group", "Sector_Level", "Industry_Level", "Asset_Level")
      # VARIABLES THAT ARE NEEDED TO GATHER AGAINST MEASURE
      requiredVariablesQ <- c(names(datQrt) [! names(datQrt) %in% namesToRemove])
      requiredVariablesA <- c(names(datAnn) [! names(datAnn) %in% namesToRemove])
      print("GATHERING DATA UP BY MEASURE")
      #### QUARTERLY
      datQrt <- datQrt %>%
              gather_(key =
                  "Measure", value = "Value", requiredVariablesQ) %>%                          # GATHER UP THE DATA BY MEASURE
        select(Sector, Asset, Industry, as.numeric(Period), Measure, Value) %>%               # SELECT THE VARIABLES THAT DO NOT NEED GATHERING UP
        filter(!(Measure %in% nmesQ)) #%>%                                                      # FILTER OUT THE VARIABLES ENDING WITH CYP
      #### ANNUAL
      datAnn <- datAnn %>%
        gather_(key =
                  "Measure", value = "Value", requiredVariablesA) %>%                          # GATHER UP THE DATA BY MEASURE
        select(Sector, Asset, Industry, as.numeric(Period), Measure, Value) %>%               # SELECT THE VARIABLES THAT DO NOT NEED GATHERING UP
        filter(!(Measure %in% nmesA)) #%>%                                                      # FILTER OUT THE VARIABLES ENDING WITH CYP

      datQrt$Value <- round(datQrt$Value,0)
      datAnn$Value <- round(datAnn$Value,0)

      # CLEAN UP
      rm(nmesQ, nmesA, namesToRemove, requiredVariablesQ, requiredVariablesA)
      print("SPREADING DATA BACK OUT BY PERIOD")
      # SPREAD THE DATA BACK OUT
      datQrt <- datQrt %>% select(Sector, Asset, Industry, Measure, Period, Value) %>% spread(Period, Value, fill = 0)
      datAnn <- datAnn %>% select(Sector, Asset, Industry, Measure, Period, Value) %>% spread(Period, Value, fill = 0)
      print("ADDING COLUMNS")
      # ADD SOME COLUMNS TO MATCH CORD OUTPUT
      datQrt$Measure2 <- ifelse(grepl('Con', datQrt$Measure, fixed=TRUE), 'CC', ifelse(grepl('Gross', datQrt$Measure), 'GS', ifelse(grepl('Net', datQrt$Measure), 'NS','NA')))
      datAnn$Measure2 <- ifelse(grepl('Con', datAnn$Measure, fixed=TRUE), 'CC', ifelse(grepl('Gross', datAnn$Measure), 'GS', ifelse(grepl('Net', datAnn$Measure), 'NS','NA')))
      datQrt <- datQrt %>% filter(!(Measure2 %in% c('NA')))
      datAnn <- datAnn %>% filter(!(Measure2 %in% c('NA')))
      datQrt$Price <- ifelse(grepl('CP', datQrt$Measure, fixed=TRUE), 'CP', ifelse(grepl('CVM', datQrt$Measure), 'CVM', ifelse(grepl('PYP',datQrt$Measure), 'PYP', 'NA')))
      datAnn$Price <- ifelse(grepl('CP', datAnn$Measure, fixed=TRUE), 'CP', ifelse(grepl('CVM', datAnn$Measure), 'CVM', ifelse(grepl('PYP',datAnn$Measure), 'PYP', 'NA')))
      datQrt <- datQrt %>% filter(!(Price %in% c('NA')))
      datAnn <- datAnn %>% filter(!(Price %in% c('NA')))
      # DONT NEED MEASURE FOR THIS OUTPUT
      datQrt$Measure <- NULL
      datAnn$Measure <- NULL
      print("REORDERING COLUMNS")
      # NEED TO REORDER THE COLUMNS TO MATCH CORD. THESE COLUMNS NEED REORDERING
      reorderNames <- c('Measure2', 'Price', 'Sector', 'Industry', 'Asset')
      # THESE COLUMNS ARE ALL THE DATES AND DO NOT NEED REORDERING
      yrQtrVarsQ <- c(names(datQrt) [! names(datQrt) %in% reorderNames])
      yrQtrVarsA <- c(names(datAnn) [! names(datAnn) %in% reorderNames])
      # JOIN UP THE TWO COLUMN NAME VECTORS
      reorderListQ <- c(reorderNames, yrQtrVarsQ)
      reorderListA <- c(reorderNames, yrQtrVarsA)
      # REORDER THE DATA, AND ALSO FILTER OUT ANY ROWS THAT WERE GIVEN THE VALUE NA WHEN ADDING COLUMNS ABOVE
      datQrt <- datQrt[,reorderListQ]
      datAnn <- datAnn[,reorderListA]

      rm(reorderNames, reorderListQ, reorderListA, yrQtrVarsQ, yrQtrVarsA)

      if (twoORthree == 7)
      {
        datQrt$Periodicity <- 'Q'
        datAnn$Periodicity <- 'A'
        datQrt$Used <- 'Used'
        datAnn$Used <- 'Used'
      }
      rowsToCreate <- 8
      print("REORDER COLUMNS AND THEN ADD ROW NUMBER AS A COLUMN")
      # REORDER AND THEN ADD A COLUMN WITH THE ROW NUMBER
      datQrt <- datQrt[with(datQrt, order(Measure2, Price, Sector, Industry, Asset)),]
      datAnn <- datAnn[with(datAnn, order(Measure2, Price, Sector, Industry, Asset)),]
      datQrt <- tibble::rowid_to_column(as.data.frame(datQrt), "ID")
      datAnn <- tibble::rowid_to_column(as.data.frame(datAnn), "ID")
      datQrt$ID <- as.numeric(datQrt$ID) + rowsToCreate  # ADD A NUMBER TO THE ROW NUMBER READY FOR THE ROWS YOU ARE GOING TO ADD
      datAnn$ID <- as.numeric(datAnn$ID) + rowsToCreate  # ADD A NUMBER TO THE ROW NUMBER READY FOR THE ROWS YOU ARE GOING TO ADD
      print("GETTING RID OF NAs and NANs")
      naValsQ <- apply(datQrt, 2, is.na)
      naValsA <- apply(datAnn, 2, is.na)
      # Set all the NA values to 0
      datQrt[naValsQ] <- '-'
      datAnn[naValsA] <- '-'
      # Remove variables from memory
      rm(naValsQ, naValsA)
      print("ADDING EMPTY ROWS")
      # ADD A NUMBER OF ROWS TO MATCH TOP OF CORD OUTPUT
      m <- 1
      while (m <= rowsToCreate) {
        datQrt[nrow(datQrt)+1,] <- ""
        datAnn[nrow(datAnn)+1,] <- ""
        datQrt[nrow(datQrt),1] <- as.numeric(m)
        datAnn[nrow(datAnn),1] <- as.numeric(m)
        m = m + 1
      }
      print("REORDERING DATA BY ROW")
      # REORDER DATA AND THEN SELECT EVERTHING BAR THE ID COLUMNS
      datQrt <- datQrt[with(datQrt, order(as.numeric(ID))),]
      datAnn <- datAnn[with(datAnn, order(as.numeric(ID))),]
      print("ADDING VARIOUS BITS OF TEXT TO EMPTY ROWS")
      # ADD THE YEAR/QUARTER TO RELEVANT ROW AS A COLUMN HEADING
      datQrt[rowsToCreate,2:ncol(datQrt)] <- colnames(datQrt[,2:ncol(datQrt)])
      datAnn[rowsToCreate,2:ncol(datAnn)] <- colnames(datAnn[,2:ncol(datAnn)])
      # DELETE THE FIRST FEW AS NOT NEEDED
      datQrt[rowsToCreate,2:6] <- ""
      datAnn[rowsToCreate,2:6] <- ""
      if (twoORthree == 7)
      {
        datAnn[rowsToCreate,ncol(datAnn)] <- ""
        datAnn[rowsToCreate,ncol(datAnn)-1] <- ""
      }

      # ADD A/Q TO RELEVANT ROW AS A COLUMN HEADING, WILL NEED TO BE AN IF STATEMENT BASED ON IF DATA IS ANNUAL OR QUARTERLY
      datQrt[rowsToCreate - 1,2:ncol(datQrt)] <- "Q"
      datAnn[rowsToCreate - 1,2:ncol(datAnn)] <- "A"
      # DELETE THE FIRST FEW AS NOT NEEDED
      datQrt[rowsToCreate - 1,2:6] <- ""
      datAnn[rowsToCreate - 1,2:6] <- ""

      # REMOVE THE ID COLUMN
      datQrt <- datQrt %>% select(-ID)
      datAnn <- datAnn %>% select(-ID)

      #####################################################################################################
      #####################################################################################################
      if (twoORthree == 2) # REPLICATE NUMBER 2 OUPUT
      {
          print("OUTPUT 2 SO MERGING DATASETS")
          # JOIN THE ANNUAL AND QUARTERLY ARRAY TOGETHER - CAN'T GET MERGE TO WORK!!!!!!!!!!
          mergedData <- cbind(datAnn, datQrt)
          # JOINED ARRAY NOW HAS 2 COLUMNS FOR MEASURE2, ASSET ETC SO NEED TO DELETE THE SECOND ENTRIES
          print("REMOVING DUPLICATE COLUMNS, CREATED FROM CBIND")
          i = 1
          while(i <= 5) # LOOKING AT THE FIRST 5 COLUMNS ONLY
          {
              col <- names(mergedData) # GET A LIST OF THE CURRENT COLUMN NAMES
              k = 6 # FIRST PLACE TO LOOK IS COLUMN 6, BUT WILL ACTUALLY BE FURTHER ON THAN THIS
              while (k < length(col)) # NOW LOOK THROUGH ALL THE OTHER COLUMNS LOOKING FOR A MATCH
              {
                if (col[i] == col[k])   # IF THEY MATCH THEN DELETE THE COLUMN
                {
                  mergedData[,k] = NULL   # DELETE THE COLUMN
                  break # YOU HAVE FOUND WHAT YOU ARE LOOKING FOR SO BREAK OUT OF THE LOOP
                }
                k = k + 1 # INCREMENT THE INDEX
              }
              i = i + 1 # INCREMENT THE INDEX
          }
          print("OUTPUT 2 SO REMOVING PYP DATA")
          # NEED TO DELETE ROWS WITH MEASURE ENDING IN PYP FOR OUTPUT 2
          mergedData <- mergedData %>% filter(Price != "PYP")

          mergedData[1,1] <- "Capstock R Model output"
          mergedData[4,1] <- "Called by R script 80_OutoutForQA.R. Built to Mimic CORD output QACHK02_CHECKING_SPREADSHEET"
      }
      else if (twoORthree == 3) {            # REPLICATE NUMBER 3 OUTPUT
          print("OUTPUT 3 SO JUST USING ANNUAL DATA")
          mergedData <- datAnn

          reorderNames <- c('Industry', 'Sector', 'Asset', 'Measure2', 'Price')
          # THESE COLUMNS ARE ALL THE DATES AND DO NOT NEED REORDERING
          yrs <- c(names(mergedData) [! names(mergedData) %in% reorderNames])
          # JOIN UP THE TWO COLUMN NAME VECTORS
          reorderList <- c(reorderNames, yrs)
          # REORDER THE DATA, AND ALSO FILTER OUT ANY ROWS THAT WERE GIVEN THE VALUE NA WHEN ADDING COLUMNS ABOVE
          mergedData <- mergedData[,reorderList]

          mergedData[1,1] <- "Capstock R Model output"
          mergedData[4,1] <- "Called by R script 80_OutoutForQA.R. Built to Mimic CORD output QACHK03_GRAPH_CHECKS_SPREADSHEET"
          rm(reorderNames, yrs, reorderList)
      }
      else {            # NEW NUMBER 7 OUTPUT, REQUESTED BY DOUG
        print("OUTPUT 7 SO JUST USING ANNUAL DATA")
        mergedData <- datAnn

        reorderNames <- c('Price', 'Measure2', 'Sector', 'Asset', 'Industry', 'Used', 'Periodicity')
        # THESE COLUMNS ARE ALL THE DATES AND DO NOT NEED REORDERING
        yrs <- c(names(mergedData) [! names(mergedData) %in% reorderNames])
        # JOIN UP THE TWO COLUMN NAME VECTORS
        reorderList <- c(reorderNames, yrs)
        # REORDER THE DATA, AND ALSO FILTER OUT ANY ROWS THAT WERE GIVEN THE VALUE NA WHEN ADDING COLUMNS ABOVE
        mergedData <- mergedData[,reorderList]

        mergedData[1,1] <- "Capstock R Model output"
        mergedData[4,1] <- "Called by R script 80_OutoutForQA.R. Built to Mimic CORD output QACHK07"
        rm(reorderNames, yrs, reorderList)
      }
      #####################################################################################################
      #####################################################################################################
      # WRITE FORMATTED DATA OUT TO A CSV WITHOUT COLUMN AND ROW NAMES
      if (OUTPUT_SHARING==TRUE)
      {
        write.table(mergedData, file = paste0(otherOutputs, "QACHK0", toString(twoORthree), "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
      } else {
        write.table(mergedData, file = paste0(outputDir, "QACHK0", toString(twoORthree), "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
      }

    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))

    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))

    }, finally = {

    })
  # SEND BACK THE SORTED AND REARRANGED DATA
  return(mergedData)
}


##############################################################################################################################################################
replicateCORDOuputQACHK04 <- function(dat)
{
  tryCatch(
    {
      dat <- QACH2rep
      tmp1 <- dat[1:8,]
      tmp2 <- dat[9:nrow(dat),]
      tmp2 <- tmp2 %>% filter(Industry %in% c('6', '9', 'C', 'G_T', 'TOTAL'))
      dat <- rbind(tmp1, tmp2)

      namesToRemove <- c("Measure2", "Price", "Sector", "Industry", "Asset")
      # VARIABLES THAT ARE NEEDED TO GATHER AGAINST MEASURE
      requiredVariables <- c(names(dat) [! names(dat) %in% namesToRemove])

      dat <- dat[,c(namesToRemove, requiredVariables)]

      dat[4,1] <- "Called by R script 70_AnnualiseAndOutputForCORD.R. Built to Mimic CORD output QACHK04_DELIVERY_CHECKS_SPREADSHEET"

      print("WRITING DATA TO A CSV FILE")
      # WRITE FORMATTED DATA OUT TO A CSV WITHOUT COLUMN AND ROW NAMES
      if (OUTPUT_SHARING==TRUE)
      {
        write.table(dat, file = paste0(otherOutputs, "QACHK04", "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
      } else {
        write.table(dat, file = paste0(outputDir, "QACHK04", "_", runTime, ".csv"), row.names = FALSE, col.names = FALSE, sep=",")
      }

    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))

    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))

    }, finally = {
    })
  return(dat)
}

#######################################################################################################################################################################
#######################################################################################################################################################################

unnestDataWithDifferentRowNumbers <- function(dat)
{
    er <- 0
    tryCatch(
    {
      print(paste0("Their are ", toString(nrow(dat)), " Rows to Unnest"))
      for (i in 1:nrow(dat))
      {
        print(paste0("Unnesting Row: ", toString(i)))
        tryCatch(
          {
              u1 <- dat[i,] %>% unnest(data)      # UNNEST THE DATA COLUMN
              u2 <- dat[i,] %>% unnest(chained)   # UNNEST THE CHAINED COLUMN
              if (nrow(u1) > nrow(u2))          # SEE IF DATA HAS MORE ROWS THAN CHAINED
              {                                 # ADD THE MISSING ROWS TO THE DATA COLUMN
                newRows <- matrix(0, nrow = (nrow(u1) - nrow(u2)), ncol = ncol(u2))
                colnames(newRows) <- colnames(u2)
                u2 <- rbind(newRows, u2)
                rm(newRows)
              }
              else if (nrow(u1) < nrow(u2))   # SEE IF CHAINED HAS MORE ROWS THAN DATA
              {                               # ADD THE MISSING ROWS TO THE CHAINED COLUMN
                newRows <- matrix(0, nrow = (nrow(u2) - nrow(u1)), ncol = ncol(u1))
                colnames(newRows) <- colnames(u1)
                u1 <- rbind(newRows, u1)
                rm(newRows)
              }
              u3 <- cbind(u1, u2)     # JOIN UP THE ALTERED DATA AND CHAINED COLMNS
              # ADD THE NEW DATA TO THE UNCHAINED ARRAY
              if (i == 1)
              {
                  unn <- u3
              }
              else {
                  unn <- rbind(unn, u3)
              }
              er <- i
              rm(u1, u2, u3)
          }, warning = function(war) {
            print(paste("LOOP_WARNING:  ", war))
          }, error = function(err) {
            print(paste("LOOP_ERROR:  ", err))
          })
      }
      # GET RID OF THE DUPLICATE ROWS
      unn[,25:27] <- NULL
      # GET RID OF THE YEAR
      #unn$Year <- NULL
      #unn <- unn %>% select(Sector, Industry, Asset, Period, ends_with("CP"), ends_with("CVM"))
      #unn <- unn %>% gather(Measure, Value, -Sector, -Industry, -Asset, -Period)
      #unn <- sqldf("SELECT * FROM unn WHERE Sector <> '0'")
      #unn <- sqldf("SELECT * FROM unn GROUP BY Sector, Industry, Asset, Period, Measure")
      #unn <- unn %>% spread(Period, Value)

      rm(dat)
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))

    }, error = function(err) {
      print(paste("MY_ERROR:  ",err, " - ", toString(er)))
    }, finally = {

    })

    return(unn)
    rm(unn)

}


#######################################################################################################################################################################
#######################################################################################################################################################################

addMissingPeriods <-function(dat)
{
    #dat = toChain
    pers <- sqldf("SELECT DISTINCT(Period) FROM dat")
    pers <- as.data.frame(pers)
    gro <- sqldf("SELECT Sector, Industry, Asset, COUNT(*) AS cnt FROM dat GROUP BY Sector, Industry, Asset")
    gro <- sqldf(paste0("SELECT * FROM gro WHERE cnt <> (SELECT MAX(cnt) FROM gro)"))
    names(gro[,4]) <- "cnt"
    maxNoRows <- sqldf("SELECT MAX(cnt) FROM gro")
    minPeriod <- sqldf("SELECT MIN(Period) FROM dat")
    maxPeriod <- sqldf("SELECT MAX(Period) FROM dat")
    i = 1
    for (i in 1:nrow(gro))
    {
      print(paste0("TESTING FOR Loop No: ", toString(i), " - Sector: ", gro[i,1], " - Industry: ", gro[i,2], " - Asset: ", gro[i,3], " - Count: ", gro[i,4], "/", toString(nrow(pers))))
      if (as.numeric(gro[i,4]) < nrow(pers))
      {
        print(paste0("FOUND ONE!!!!!!! Sector: ", gro[i,1], " - Industry: ", gro[i,2], " - Asset: ", gro[i,3]))
        newDat<- sqldf(paste0("SELECT * FROM dat WHERE Sector = '", gro[i,1], "' AND Industry = '", gro[i,2], "' AND Asset = '", gro[i,3], "'"))
        newDat <- sqldf("SELECT b.Period AS pers, a.* FROM pers b LEFT JOIN newDat a ON a.period = b.period")
        newDat$Sector <- gro[i,1]
        newDat$Industry <- gro[i,2]
        newDat$Asset <- gro[i,3]
        newDat$Period <- newDat$pers
        newDat$Group <- newDat$Group[nrow(newDat)]
        newDat$pers <- NULL
        naVals <- apply(dat, 2, is.na)
        newDat[apply(newDat, 2, is.na)] <- 0
        dat <- subset(dat, paste0(Sector, Industry, Asset) != paste0(gro[i,1], gro[i,2], gro[i,3]))
        dat <- rbind(dat,newDat)
        rm(newDat)
      }
    }
    return(dat)
}

#######################################################################################################################################################################
#######################################################################################################################################################################

addMissingPeriodsToAllowChaining <-function(dat)
{
    #dat = toChain
    gro <- sqldf("SELECT Sector, Industry, Asset, COUNT(*) AS cnt FROM dat GROUP BY Sector, Industry, Asset")
    names(gro[,4]) <- "cnt"
    rc <- nrow(gro)
    maxNoRows <- sqldf("SELECT MAX(cnt) FROM gro")
    minPeriod <- sqldf("SELECT MIN(Period) FROM dat")
    maxPeriod <- sqldf("SELECT MAX(Period) FROM dat")

    loopCheck <- 0
    for (i in 1:nrow(gro))
    {
      if (as.numeric(gro[i,4]) != maxNoRows[1])
      {
        looPeriod <- minPeriod[1]
        print(paste0("FOUND ONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!", looPeriod[1]))
        k <- 1
        while ((looPeriod <= maxPeriod) & loopCheck < 1000)
        {
          ch <- sqldf(paste0("SELECT * FROM dat WHERE Sector = '", gro[i,1], "' AND Industry = '", gro[i,2], "' AND Asset = '", gro[i,3], "' AND Period = '", looPeriod[1],  "' ORDER BY Period"))

          if (nrow(ch) == 0)
          {
            print(paste0("SELECT * FROM dat WHERE Sector = '", gro[i,1], "' AND Industry = '", gro[i,2], "' AND Asset = '", gro[i,3], "' AND Period = '", looPeriod[1],  "' ORDER BY Period"))

            dat[nrow(dat)+1,] <- "0"
            dat[nrow(dat),1] <- gro[i,1]  # SECTOR
            dat[nrow(dat),2] <- gro[i,2]  # INDUSTRY
            dat[nrow(dat),3] <- gro[i,3]  # ASSET
            dat[nrow(dat),4] <- looPeriod[1] # PERIOD
            dat[nrow(dat),ncol(dat) - 3] <- dat[1,ncol(dat) - 3]  # SECTOR LEVEL
            dat[nrow(dat),ncol(dat) - 2] <- dat[1,ncol(dat) - 2]  # INDUSTRY LEVEL
            dat[nrow(dat),ncol(dat) - 1] <- dat[1,ncol(dat) - 1]  # ASSET LEVEL
            dat[nrow(dat),ncol(dat) - 0] <- dat[1,ncol(dat) - 0]  # GROUP LEVEL
          }
          looPeriod <- addaQuarter(looPeriod[1])
          loopCheck <- loopCheck + 1
        }

      }
    }
    rm(maxNoRows, maxPeriod, minPeriod, i, gro, loopCheck)
    return(dat)
}

#######################################################################################################################################################################
#######################################################################################################################################################################

addQuarter <- function(qtr)
{
  qt <- as.numeric(substring(qtr,7,7))
  if (qt == 4)
  {
    newMaxQtr <- paste0("Y", as.character(as.numeric(substring(qtr,2,5)) + 1), "Q1")
  } else {
    newMaxQtr <- paste0(substring(qtr,1,6), as.character(qt + 1))
  }
  return(newMaxQtr)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
