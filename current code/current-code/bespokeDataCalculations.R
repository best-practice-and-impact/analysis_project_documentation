createBespokeSecIndAssAggregations <- function(besDat)
{
  #besDat <- toChain
  #besDat <- aggregated
  #################################################################################################################################################################################################################################
  colOrd <- c(names(besDat)) #### KEEP RECORD OF COLUMN ORDER FOR RETURNING DATASET
  ordCols <- c('Sector','Industry','Asset','Period','Group') #### COLUMNS YOU DON'T WANT TO GATHER UP
  ret <- array(0,dim=c(0,ncol(besDat)))
  ret <- ret[colOrd]
  #################################################################################################################################################################################################################################
  tryCatch(
    {
      print("CALCULATING Sector HOUSEHOLDS - AGGREGATED FOR SECTORS S.14 & S.15")
      bes1 <- besDat %>% filter(Sector %in% c('S.14','S.15'))
      print(paste0(toString(nrow(bes1)), " ROWS FOUND"))
      bes1 <- bes1 %>% gather_(key = "Measure", value = "Value", c(names(bes1) [! names(bes1) %in% ordCols]))
      bes1 <- sqldf("SELECT 'S.1HN' AS Sector, Industry, Asset, Period, MAX([Group]) AS [Group], Measure, SUM(Value) AS Value FROM bes1 GROUP BY Industry, Asset, Period, Measure ORDER BY Sector, Industry, Asset, Period, Measure")
      bes1 <- bes1 %>% filter(Sector == 'S.1HN')
      bes1 <- bes1 %>% spread(Measure, Value, fill = 0)
      print(paste0("AGGREGATED DOWN TO ", nrow(bes1), " ROWS"))
      bes1 <- bes1[colOrd]
      
      ret <- rbind(besDat,bes1)
      rm(bes1)
    },  error = function(err) {
      print(paste("S.1HN HOUSEHOLDS ERROR: ",err))
    }, finally = {
      print("S.1HN HOUSEHOLDS FINISHED")
    })
  #################################################################################################################################################################################################################################
  tryCatch(
    {
      print("CALCULATING Industry ABDE - AGGREGATED FOR INDSUTRIES A, B, D, E")
      bes2<- ret %>% filter(Industry %in% c('A','B','D','E'))
      print(paste0(toString(nrow(bes2)), " ROWS FOUND"))
      bes2 <- bes2 %>% gather_(key = "Measure", value = "Value", c(names(bes2) [! names(bes2) %in% ordCols]))
      bes2 <- sqldf("SELECT Sector, 'ABDE' AS Industry, Asset, Period, MAX([Group]) AS [Group], Measure, SUM(Value) AS Value FROM bes2 GROUP BY Sector, Asset, Period, Measure ORDER BY Sector, Industry, Asset, Period")
      bes2 <- bes2 %>% filter(Industry == 'ABDE')
      bes2 <- bes2 %>% spread(Measure, Value, fill = 0)
      print(paste0("AGGREGATED DOWN TO ", toString(nrow(bes2)), " ROWS"))
      bes2 <- bes2[colOrd]
      
      ret <- rbind(ret, bes2)
      rm(bes2)
    },  error = function(err) {
      print(paste("ABDE ERROR: ",err))
    }, finally = {
      print("ABDE FINISHED")
    })
  #################################################################################################################################################################################################################################
  tryCatch(
    {
      print("CALCULATING Asset OTH.MACH.EQ.WEAP (CORD: N11O) - AGGREGATED FOR Assets OTHER.MACHINERY & WEAPONS")
      bes3 <- ret %>% filter(Asset %in% c('OTHER.MACHINERY','WEAPONS'))
      print(paste0(toString(nrow(bes3)), " ROWS FOUND"))
      bes3 <- bes3 %>% gather_(key = "Measure", value = "Value", c(names(bes3) [! names(bes3) %in% ordCols]))
      bes3 <- sqldf("SELECT Sector, Industry, 'OTH.MACH.EQ.WEAP' AS Asset, Period, MAX([Group]) AS [Group], Measure, SUM(Value) AS Value FROM bes3 GROUP BY Sector, Industry, Period, Measure ORDER BY Sector, Industry, Asset, Period")
      bes3 <- bes3 %>% filter(Asset == 'OTH.MACH.EQ.WEAP')
      bes3 <- bes3 %>% spread(Measure, Value, fill = 0)
      
      print(paste0("AGGREGATED DOWN TO ", toString(nrow(bes3)), " ROWS"))
      bes3 <- bes3[colOrd]
      
      ret <- rbind(ret, bes3)
      rm(bes3)
    },  error = function(err) {
      print(paste("OTH.MACH.EQ.WEAP ERROR: ",err))
    }, finally = {
      print("OTH.MACH.EQ.WEAP FINISHED")
    })
  #################################################################################################################################################################################################################################
  tryCatch(
    {
      print("CALCULATING Asset MACH.EQ.WEAP (CORD: N11M) - AGGREGATED FOR Assets MACH.EQ & WEAPONS")
      bes4 <- ret %>% filter(Asset %in% c('MACH.EQ','WEAPONS'))
      print(paste0(toString(nrow(bes4)), " ROWS FOUND"))
      bes4 <- bes4 %>% gather_(key = "Measure", value = "Value", c(names(bes4) [! names(bes4) %in% ordCols]))
      bes4 <- sqldf("SELECT Sector, Industry, 'MACH.EQ.WEAP' AS Asset, Period, MAX([Group]) AS [Group], Measure, SUM(Value) AS Value FROM bes4 GROUP BY Sector, Industry, Period, Measure ORDER BY Sector, Industry, Asset, Period")
      bes4 <- bes4 %>% filter(Asset == 'MACH.EQ.WEAP')
      bes4 <- bes4 %>% spread(Measure, Value, fill = 0)
      
      print(paste0("AGGREGATED DOWN TO ", toString(nrow(bes4)), " ROWS"))
      bes4 <- bes4[colOrd]
      
      ret <- rbind(ret, bes4)
      rm(bes4)
    },  error = function(err) {
      print(paste("MACH.EQ.WEAP ERROR: ",err))
    }, finally = {
      print("MACH.EQ.WEAP FINISHED")
    })
  
  #################################################################################################################################################################################################################################
  
  tryCatch(
    {
      # SPARE BIT FOR OTHER BESPOKE AGGREGATES
    },  error = function(err) {
      print(paste("SPARE: ",err))
    }, finally = {
      print("SPARE")
    })
  #################################################################################################################################################################################################################################
  print(paste0("Checking for missing Periods"))
  
  return(ret)
}

bespokeAggregationsForLocGov <- function(dat)
{
  #### FILTER THE DATA TO WHAT YOU WANT
  dat <- dat %>% filter(Asset %in% c('OTHER.STRUCTURES','SOFT.DATA','TOTAL'))
  colOrd <- c(names(dat))
  #### CREATE DUMMY ARRAYS JUST IN CASE SOMETHING GOES WRONG
  N1122_1173 <- array(0,dim=c(0,ncol(dat)))
  colnames(N1122_1173) <- colOrd
  N11_INT <- array(0,dim=c(0,ncol(dat)))
  colnames(N11_INT) <- colOrd
  tryCatch(
  {
    #### GATHER UP BY MEASURE
    ordCols <- c('Sector','Industry','Asset','Period')
    dat <- dat %>% gather_(key = "Measure", value = "Value", c(names(dat) [! names(dat) %in% ordCols]))

    print("CALCULATING Asset OTHER.STRUCTURES.EQ.SOFT.DATA (CORD: N1122_1173) - AGGREGATED FOR Assets OTHER.STRUCTURES & SOFT.DATA")
    N1122_1173 <- sqldf("select Sector, Industry, 'OTHER.STRUCTURES.EQ.SOFT.DATA' AS Asset, Period, Measure, SUM(Value) AS Value
                        FROM dat WHERE Asset IN ('OTHER.STRUCTURES','SOFT.DATA')
                        GROUP BY Sector, Industry, Period, Measure")

    print("CALCULATING Asset TOTAL.MIN.OTHER.STRUCTURES.AND.SOFT.DATA (CORD: N11_INT) - AGGREGATED FOR Assets total - (OTHER.STRUCTURES + SOFT.DATA)")
    N11_INT <- sqldf("SELECT a.Sector, a.Industry, 'TOTAL.MIN.OTHER.STRUCTURES.AND.SOFT.DATA' AS Asset, a.Period,
            a.Measure, a.Value - b.Value AS Value FROM dat a INNER JOIN N1122_1173 b
            ON a.Sector = b.Sector AND a.Industry = b.Industry AND a.Period = b.Period
            AND a.Measure = b.Measure WHERE a.Asset = 'TOTAL'")

    #### SPREAD BACK OUT AND REORDER
    N11_INT <- N11_INT%>% spread(Measure, Value, fill = 0)
    N11_INT <- N11_INT[colOrd]
    N1122_1173 <- N1122_1173 %>% spread(Measure, Value, fill = 0)
    N1122_1173 <- N1122_1173[colOrd]
  },  error = function(err) {
    print(paste("LOCAL GOV BESPOKE AGGREGATION ERRORS: : ",err))
  }, finally = {
    print("LOCAL GOV BESPOKE AGGREGATIONs FINISHED")
  })
  return(rbind(N1122_1173, N11_INT))
}








#households <- households %>% select(-Group,Group) # THIS MOVES A SPECIFIC COLUMN TO THE END, HANDY
