# This has replaced processing spreadsheets and
# takes inputs and ensures that they are in the
# format required.

# For further details see: 
# J:/CAPSTOCKS_WORK_INSTRUCTIONS/New_instructions/Running R system.docx

###################### GFCF_r42 ############################

# Picking up GFCF from GFCF team

# Read in QA dataset to provide values not available
# in visualisation
qa <- read.csv(paste0(inputDir,"GFCF/R42/QA.csv"), stringsAsFactors=FALSE)
row_qa <- qa[which(qa$X.4 != "", arr.ind = TRUE)[1],]
if(row_qa[,8] == "Q"){
  row.names(row_qa) <- as.numeric(row.names(row_qa)) + 1
}
row_qa <- as.numeric(row.names(row_qa))


qa <- read.csv(paste0(inputDir,"GFCF/R42/QA.csv"), stringsAsFactors=FALSE, skip = row_qa)


# As columns not necessarily consistent look in 
# columns to determine names

for (i in 1:4){
  
  if ('S.11001' %in% (qa[[i]])){
    names(qa)[i] <- "Sector"
    next
  }
  
  if ('ACQ' %in% (qa[[i]])){
    names(qa)[i] <- "Basis"
    next
  }
  
  if ('OTHER.MACHINERY' %in% (qa[[i]])){
    names(qa)[i] <- "Asset"
    next
  }
  
  if ('87_88' %in% (qa[[i]])){
    names(qa)[i] <- "Industry"
    next
  }  
  
}

# Drop historic data as we use our own historic GFCF
col_names <- data.frame(col = names(qa))
col_names$sub_str <- as.numeric(substring(col_names$col,2,5))
col_names <- filter(col_names, sub_str < 1997)
if(nrow(col_names)!=0){
  qa <- select(qa, -c(col_names$col))
}

remove(col_names)

# Check columns are correct

check <- c("Sector", "Basis", "Asset", "Industry", "X1997Q1") %in% names(qa)

if (FALSE %in% check){
  
  flog.fatal("Missing columns/data in QA.csv.")
  
}

# Removing columns with missing data 

qa[qa=="."] <- NA
remove_col <- names(which(colSums(is.na(qa)) > 1))
qa <- qa[ , !(names(qa) %in% remove_col)]
rm(remove_col)

# Check if Vis and R5.2 present

if (file.exists(paste0(inputDir,"GFCF/R42/Vis.csv")) &
    file.exists(paste0(inputDir,"GFCF/R42/R52.csv"))){
  
  print("Cannot run with Vis and R52")
  quit()
  
}

# Read in R42/R52

if (file.exists(paste0(inputDir,"GFCF/R42/Vis.csv"))){
  
  vis <- read.csv(paste0(inputDir,"GFCF/R42/Vis.csv"), stringsAsFactors=FALSE, skip=9)
  names(vis)[1:4] <- c("Sector", "Industry", "Asset", "Basis")
  
  # Removing columns with missing data 
  
  vis[vis=="."] <- NA
  remove_col <- names(which(colSums(is.na(vis)) > 0))
  vis <- vis[ , !(names(vis) %in% remove_col)]
  rm(remove_col)  
  
}


if (file.exists(paste0(inputDir,"GFCF/R42/R52.csv"))){
  
  r52 <- read.csv(paste0(inputDir,"GFCF/R42/R52.csv"), stringsAsFactors=FALSE, skip=9)
  
  for (i in 1:4){
    
    if ('S.11001' %in% (r52[[i]])){
      names(r52)[i] <- "Sector"
      next
    }
    
    if ('ACQ' %in% (r52[[i]])){
      names(r52)[i] <- "Basis"
      next
    }
    
    if ('OTHER.MACHINERY' %in% (r52[[i]])){
      names(r52)[i] <- "Asset"
      next
    }
    
    if ('87_88' %in% (r52[[i]])){
      names(r52)[i] <- "Industry"
      next
    }  
    
  }
  
  # Use Vis TC in QA if available
  
  if ("S.11001" %in% r52$Sector){
    
    # No 05-09 breakdown of R52 in Vis
    
    S11001 <- r52
    S11001 <- S11001[names(qa)]
    S11001_59_PC_OME <- filter(S11001, Industry=="05-Sep" & Asset=="OTHER.MACHINERY" & Sector=="S.11001")
    S11001_59_PC_OME$Industry <- "5"
    S11001 <- filter(S11001, Sector=="S.11001")
    qa <- filter(qa, Sector!="S.11001")
    qa <- rbind(qa, S11001)
    rm(S11001)
    
  }
  
  r52 <- filter(r52, (Asset=="CULTIVATED" | Asset=="HARDWARE" | Asset=="ICT" | Asset=="TC.FOOT.FEES" |
                        Asset=="DWELLINGS" | Asset=="ENTERTAINMENT" | Asset=="TRANSPORT" | Asset=="LAND.IMPROVEMENTS" |
                        Asset=="MIN" | Asset=="OTHER.BUILDINGS" | Asset=="OTHER.MACHINERY" |
                        Asset=="RESEARCH.DEVELOPMENT" | Asset=="TC" | Asset=="SOFT.DATA" | Asset=="TELECOMS") &
                  (Sector=="S.11PR" | Sector=="S.12" | Sector=="S.14" | Sector=="S.15" | Sector=="S.1PT") &
                  (Industry %in% c("1", "2", "3", "05-Sep", "10-Dec",
                                   "13-15", "16", "17", "18", "19",
                                   "20", "21", "22", "23", "24",    
                                   "25", "26", "27", "28", "29",    
                                   "30", "31-32", "33", "35", "36",    
                                   "37-39", "41_43", "45", "46", "47",    
                                   "49", "50", "51", "52", "53",
                                   "55-56", "58", "59-60", "61", "62-63", 
                                   "64", "65", "66", "68", "69-70",
                                   "71", "72", "73", "74-75","77",    
                                   "78", "79", "80-82", "84", "85",    
                                   "86", "87_88", "90-92", "93", "94",    
                                   "95", "96", "TOTAL")))
  r52 <- select(r52, -X1995Q1, -X1995Q2, -X1995Q3, -X1995Q4,
                -X1996Q1, -X1996Q2, -X1996Q3, -X1996Q4)
  
  # Don't have S.12 breakdown - assign to one 
  # sector as this is not used
  
  r52$Sector <- ifelse(r52$Sector=="S.12" & r52$Industry=="65", "S.125IN",
                       ifelse(r52$Sector=="S.12" & r52$Industry=="66", "S.12FE",
                              ifelse(r52$Sector=="S.12" & r52$Industry=="64", "S.12MF", r52$Sector)))
  
  # Removing columns with missing data 
  
  r52[r52=="."] <- NA
  remove_col <- names(which(colSums(is.na(r52)) > 0))
  r52 <- r52[ , !(names(r52) %in% remove_col)]
  rm(remove_col)
  vis <- r52
  
}

# Keep relevant assets in QA

qa_gfcf <- filter(qa, Asset=="CULTIVATED" | Asset=="HARDWARE" | Asset=="ICT" | Asset=="TC.FOOT.FEES" |
                    Asset=="DWELLINGS" | Asset=="ENTERTAINMENT" | Asset=="TRANSPORT" | Asset=="LAND.IMPROVEMENTS" |
                    Asset=="MIN" | Asset=="OTHER.BUILDINGS" | Asset=="OTHER.MACHINERY" |
                    Asset=="RESEARCH.DEVELOPMENT" | Asset=="TC" | Asset=="SOFT.DATA" | Asset=="TELECOMS")

# Rename TC.FOOT.FEES to FTC

qa_gfcf$Asset <- ifelse(qa_gfcf$Asset=="TC.FOOT.FEES",
                        "FTC", qa_gfcf$Asset)

# Assign FTC to industry 93

qa_gfcf$Industry <- ifelse(qa_gfcf$Asset=="FTC" & qa_gfcf$Industry=="TOTAL",
                           '93', qa_gfcf$Industry)

# Extract 05-09 S.1PT qa_gfcf - excluding minerals

S1PT <- filter(qa_gfcf, Sector=="S.1PT")
S1PT <- filter(S1PT, Industry=="5" | Industry=="6" |
                 Industry=="7" | Industry=="8" | Industry=="9" |
                 Industry=="93" & Asset=="FTC")
S1PT <- filter(S1PT, Asset!="MIN")
S1PT$Sector <- ifelse(S1PT$Asset=="FTC"| S1PT$Asset== "RESEARCH.DEVELOPMENT",
                      "S.11PR", S1PT$Sector)

# Extract gov TCs qa_gfcf

S13 <- filter(qa_gfcf, Asset=="TC")
S13 <- filter(S13, Sector=="S.1311" | Sector=="S.1313")
S13 <- filter(S13, Industry=="68")

# Extract public corps qa_gfcf

S11001 <- filter(qa_gfcf, Sector=="S.11001")

# TO DO!!! Create a check with total in case coverage changes

S11001 <- filter(S11001, Industry=="2" | Industry=="5" | Industry=="6" |
                   Industry=="7" | Industry=="8" | Industry=="9" |
                   Industry=="18" | Industry=="21" | Industry=="24" |
                   Industry=="31-32" | Industry=="33" | Industry=="35" | Industry=="36" |
                   Industry=="46" | Industry=="49" | Industry=="50" |
                   Industry=="52" | Industry=="53" | Industry=="55-56" | Industry=="59-60" |
                   Industry=="61" | Industry=="64" | Industry=="66" | Industry=="68" |
                   Industry=="69-70" | Industry=="71" | Industry=="72" | Industry=="74-75" |
                   Industry=="80-82" | Industry=="84" | Industry=="85" |
                   Industry=="86" | Industry=="90-92" | Industry=="93" |
                   Industry=="94" | Industry=="TOTAL")

# 06 Minerals incorrect in qa_gfcf - remove

qa_gfcf <- filter(qa_gfcf, Asset!="MIN")

# Removing industry 05-09 vis except for minerals

min06 <- filter(vis, Sector=="S.1PT", Industry=="05-Sep" | Industry=="05-09", Asset=="MIN")
min06$Industry <- "6"

# Constrain 05-09 to visualisation

vis_59 <- filter(vis, Industry=="05-Sep"| Industry=="05-09")

basis_mapping <- read.csv(paste0(inputDir,"Mapping & Aggregation/Basis_mapping.csv"), stringsAsFactors=FALSE)
basis_mapping$Adjustment <- as.numeric(basis_mapping$Adjustment)
vis_59 <- left_join(vis_59, basis_mapping)
vis_59 <- vis_59[,c(which(colnames(vis_59)=="Adjustment"),which(colnames(vis_59)!="Adjustment"))]

vis <- filter(vis, Industry!="05-Sep"| Industry=="05-09")

S1PT <- gather(S1PT, Period, GFCF, 5:ncol(S1PT))
S1PT <- left_join(S1PT, basis_mapping)
S1PT <- S1PT[,c(which(colnames(S1PT)=="Adjustment"),which(colnames(S1PT)!="Adjustment"))]
S1PT$GFCF <- as.numeric(S1PT$GFCF)
S1PT$GFCF <- S1PT$GFCF*S1PT$Adjustment
S1PT <- select(S1PT, -Adjustment, -Basis)
S1PT <- aggregate(GFCF ~ Industry+Asset+Sector+Period, data=S1PT,FUN=sum)
S1PT <- S1PT[,c(which(colnames(S1PT)=="Adjustment"),which(colnames(S1PT)!="Adjustment"))]
S1PT$Ind_group <- ifelse(S1PT$Industry %in% c("5", "6", "7", "8", "9"),"05-Sep",S1PT$Industry)
S1PT <- S1PT %>% 
  group_by(Asset,Sector,Period,Ind_group) %>% 
  mutate(raw_total = sum(GFCF)) %>% 
  ungroup()

vis_59 <- gather(vis_59, Period, cons_GFCF, 6:ncol(vis_59))
vis_59$cons_GFCF <- vis_59$cons_GFCF*vis_59$Adjustment
vis_59 <- select(vis_59, -Adjustment, -Basis)
vis_59 <- aggregate(cons_GFCF ~ Industry+Asset+Sector+Period, data=vis_59,FUN=sum)
vis_59 <- rename(vis_59, Ind_group = Industry)

S1PT <- left_join(S1PT, vis_59)
S1PT$Adjustment <- S1PT$cons_GFCF/S1PT$raw_total
S1PT$Adjustment[is.na(S1PT$Adjustment)] <- 1
S1PT$GFCF <- S1PT$GFCF*S1PT$Adjustment
S1PT$Basis <- "ACQ"
S1PT <- select(S1PT, Industry, Asset, Sector, Basis, Period, GFCF)
S1PT <- spread(S1PT, Period, GFCF)

# Remove if there any are extra columns in qa_gfcf

S11001 <- S11001[1:ncol(vis)]
S13 <- S13[1:ncol(vis)]
S1PT <- S1PT[1:ncol(vis)]

# Combine visualisation and extracted QA data
vis <- vis[names(S11001)]
min06 <- min06[names(S11001)]
S1PT <- S1PT[names(S11001)]

p <- names(S11001)
# stopifnot(names(S11001)==p&names(S1PT)==p&names(vis)==p&names(min06)==p&names(S11001_59_PC_OME)==p)
rm(p)
gfcf_r42_open <- rbind(S11001, S13, S1PT, vis, 
                       min06,if (exists('S11001_59_PC_OME')) S11001_59_PC_OME)

# Map industry to correct format (i.e remove dates)

ind_map <- readRDS(paste0(inputDir,"Mapping & Aggregation/SIC_Industry_mapping.rds"))

gfcf_r42_open <- left_join(gfcf_r42_open,ind_map)
gfcf_r42_open$Industry <- ifelse(is.na(gfcf_r42_open$Corrected),
                                 gfcf_r42_open$Industry,gfcf_r42_open$Corrected)
gfcf_r42_open$Industry <- gsub("-","_",gfcf_r42_open$Industry)
gfcf_r42_open$Corrected <- NULL


# Calculate Net acquisition

gfcf_r42_open <- left_join(gfcf_r42_open, basis_mapping)
gfcf_r42_open <- gfcf_r42_open[,c(which(colnames(gfcf_r42_open)=="Adjustment"),which(colnames(gfcf_r42_open)!="Adjustment"))]

for (i in (names(gfcf_r42_open[6:ncol(gfcf_r42_open)]))){
  
  gfcf_r42_open[i][is.na(gfcf_r42_open[i])] <- 0
  gfcf_r42_open[i] <- gfcf_r42_open[i]*gfcf_r42_open['Adjustment']
  
}

### Continue from here

gfcf_r42_open$Basis <- NULL
gfcf_r42_open$Adjustment <- NULL
gfcf_r42_open <- aggregate(. ~ Sector+Asset+Industry, data=gfcf_r42_open, FUN=sum)
gfcf_r42_open <- filter(gfcf_r42_open, Industry!="GF_NA")

rm(min06,S11001,S13,S1PT,vis, qa_gfcf)

######################## Deflators ###########################

# Read in GFCF CORD extracts

def02 <- read.csv(paste0(inputDir,"Deflators/R02.csv"), skip=10, stringsAsFactors=FALSE)
names(def02)[1] <- "Product"
def03 <- read.csv(paste0(inputDir,"Deflators/R03.csv"), skip=9, stringsAsFactors=FALSE)
names(def03)[1:3] <- c("Industry", "Product", "Basis")

# Removing columns with missing data 

def03[def03=="."] <- NA
remove_col <- names(which(colSums(is.na(def03)) > 0))
def03 <- def03[ , !(names(def03) %in% remove_col)]
rm(remove_col)

# Making sure deflators cover the same period

def03 <- def03[colnames(def03) %in% c(colnames(def02), "Basis", "Industry")]

def02 <- def02[colnames(def02) %in% colnames(def03)]

def02[def02=="."] <- NA
remove_col <- names(which(colSums(is.na(def02)) > 0))
def02 <- def02[ , !(names(def02) %in% remove_col)]
rm(remove_col)

# Extract 05-09 from qa.csv

def05_09 <- filter(qa, Industry=="5" | Industry=="6" | Industry=="7" |
                     Industry=="8" | Industry=="9")
def05_09 <- filter(def05_09, Asset %in% unique(def03$Product))
def05_09 <- filter(def05_09, Sector=="S.11001" | Sector=="S.1PT")
def05_09$Sector <- "S.1"
def05_09 <- rename(def05_09, Product = Asset)

# Read in spreadsheet delivery

all_ind_sp<- read.csv(paste0(inputDir, "Deflators/Spreadsheet_del.csv"), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
all_ind_sp$DATE <- paste0("X", all_ind_sp$DATE)
all_ind_sp <- gather(all_ind_sp, Asset, price_index , 2:ncol(all_ind_sp))
all_ind_sp<- spread(all_ind_sp, DATE, price_index)
all_ind_sp$Asset <- ifelse(all_ind_sp$Asset== "BUILDINGS.TC", "TC", all_ind_sp$Asset)
all_ind_sp <- filter(all_ind_sp, !is.na(X1997Q1))

#all_ind_sp <- read.csv(paste0(inputDir,"Deflators/Spreadsheet_del.csv"), skip=15, stringsAsFactors=FALSE)
# <- all_ind_sp[4:(ncol(def02)+3)]
#names(all_ind_sp)[1] <- "Asset"
#all_ind_sp <- subset(all_ind_sp, !is.na(all_ind_sp$X1997Q1))

# Read in FTC

#FTC <- read_excel(paste0(inputDir, 
#                                 "Deflators/FTC.xlsx"),
#                          skip=6, col_types = "text")
#FTC[4:ncol(FTC)] <- as.numeric(FTC[4:ncol(FTC)])

# Create deflators that do not differ by industry

# Filter out dwellings and TC from spreadsheet

#all_ind_sp <- filter(all_ind_sp, Asset=="DWELLINGS" | Asset=="TC")

# Add in soft.data, telecomms & hardware from CORD

all_ind_d02 <- filter(def02, Product=="58.29" | Product=="62.01.2" | Product=="26.2" | Product=="TELECOMS" | Product == "HARDWARE")
all_ind_d02$Asset <- ifelse(all_ind_d02$Product=="58.29", "SOFT.DATA(P)",
                            ifelse(all_ind_d02$Product=="62.01.2", "SOFT.DATA(OA)",
                                   ifelse(all_ind_d02$Product=="TELECOMS", "TELECOMS",
                                          ifelse(all_ind_d02$Product == "26.2", "TELECOMS",
                                                 "OTHER"))))
all_ind_d02ext <- filter(all_ind_d02, Asset=="TELECOMS")
all_ind_d02ext$Asset <- "HARDWARE"
all_ind_d02 <- rbind(all_ind_d02,all_ind_d02ext)
all_ind_d02$Product <- NULL

# Merge all industry deflators

all_ind_sp <- all_ind_sp[names(all_ind_d02)]
all_ind <- rbind(all_ind_d02, all_ind_sp)
all_ind$Industry <- "ALL"


# Re-reference price index to reference year

all_ind <- tidyr::pivot_longer(all_ind, cols = -c("Asset", "Industry"), names_to = "Period", values_to = "priceindex")

all_ind$year <- substr(all_ind$Period,2,5)

# Excel rounded priceindex. Why???

all_ind <- all_ind %>% group_by(Industry,Asset) %>% mutate(adjustment = (sum(priceindex[as.numeric(year)==substr(refPeriod,2,5)]))/4)
all_ind$priceindex <- round((all_ind$priceindex/all_ind$adjustment)*100,3)
all_ind$adjustment <- NULL
all_ind$year <- NULL
all_ind <- ungroup(all_ind)
all_ind <- spread(all_ind, Period, priceindex)

# Remove any extra columns in qa_gfcf and merge into def03

def05_09 <- def05_09[,1:(ncol(def03)+1)]
def05_09 <- aggregate(. ~ Industry+Basis+Product+Sector, data=def05_09, FUN=sum)
def05_09$Sector <- NULL

def03 <- rbind(def03,def05_09)

# Implied deflators - calculate inverse growth rates

def02 <- gather_(def02, "Period", "Value", names(def02)[2:ncol(def02)])
def02 <- def02[order(def02$Product,def02$Period),]
def02$inverse_growth <- ifelse(def02$Period=="X1997Q1",0,lag(def02$Value)/def02$Value)
def02$Value <- NULL

# Overwrite some deflators - why???

def02$inverse_growth <- ifelse(def02$Product=="59" & def02$Period=="X2002Q2",
                               (lag(def02$inverse_growth)+lead(def02$inverse_growth))/2,def02$inverse_growth)
def02$inverse_growth <- ifelse(def02$Product=="59" & def02$Period=="X2004Q4",
                               (lag(def02$inverse_growth,2)+lead(def02$inverse_growth))/2,def02$inverse_growth)
def02$inverse_growth <- ifelse(def02$Product=="59" & def02$Period=="X2004Q3",
                               (lag(def02$inverse_growth,2)+lead(def02$inverse_growth))/2,def02$inverse_growth)
def02$inverse_growth <- ifelse(def02$Product=="71.12.33" & def02$Period=="X2009Q1",
                               (lag(def02$inverse_growth)+lead(def02$inverse_growth))/2,def02$inverse_growth)
def02$inverse_growth <- ifelse(def02$Product=="90.03.12" & def02$Period=="X2018Q2",
                               lag(def02$inverse_growth),def02$inverse_growth)

# GFCF used to weight deflators

# Calculate net acquisitions

latest_q <- names(def03)[ncol(def03)]
def03 <- gather_(def03, "Period", "Value", names(def03)[4:ncol(def03)])
def03 <- mutate(def03, Value = ifelse(Basis=="ACQ", 1*Value,-1*Value))
def03 <- aggregate(Value ~ Industry+Product+Period, data=def03, FUN=sum)

# Fill in zeros

def03 <- def03[order(def03$Industry,def03$Product,def03$Period),]
def03 <- def03 %>%
  mutate(Value = ifelse(Value == 0 & Period!="X1997Q1", NA, Value)) %>% 
  fill(Value, .direction = c("down"))
def03 <- def03 %>%
  mutate(Value = ifelse(Value == 0 & Period!=latest_q, NA, Value)) %>% 
  fill(Value, .direction = c("up"))

# Use absolute value (why absolute of net acq?)

def03$Value <- abs(def03$Value)

# Map products to asset

product_mapping <- read.csv(paste0(inputDir,"Mapping & Aggregation/asset_product_mapping.csv"), stringsAsFactors=FALSE)

# Note one product incorrectly assigned to OM when should be weapons

def03 <- left_join(def03, product_mapping)
def03 <- def03 %>% group_by(Industry,Asset,Period) %>% mutate(Total = sum(Value))
def03$weight <- def03$Value/def03$Total
def03 <- filter(def03, Asset!="TC")

# Combine weights with inverse growth rate

deflators <- merge(def02, def03)
deflators$priceindex <- (deflators$weight*deflators$inverse_growth)
deflators <- select(deflators, Industry, Asset, Period, priceindex)
deflators <- aggregate(priceindex ~ Industry+Asset+Period, data=deflators, FUN=sum)
deflators$priceindex <- 1/deflators$priceindex

# Create price index

deflators <- deflators[order(deflators$Industry,deflators$Asset,deflators$Period),]
deflators$priceindex <- ifelse(deflators$Period=="X1997Q1",100,deflators$priceindex)

for (i in (unique(deflators$Period)[2:length(unique(deflators$Period))])){
  
  deflators <- deflators %>%
    mutate(priceindex = ifelse(Period==i,priceindex*lag(priceindex),priceindex))
  
}

deflators$year <- substr(deflators$Period,2,5)

deflators <- deflators %>% group_by(Industry,Asset) %>% mutate(adjustment = (sum(priceindex[as.numeric(year)==substr(refPeriod,2,5)]))/4)
deflators$priceindex <- (deflators$priceindex/deflators$adjustment)*100
deflators$adjustment <- NULL
deflators$year <- NULL
deflators <- ungroup(deflators)

# Reshape deflators

deflators_open <- spread(deflators, Period, priceindex)

# Join deflators that differ and do not differ by industry

deflators_open <- rbind(deflators_open,all_ind)
deflators_open$Sector <- "ALL"

deflators_open <- left_join(deflators_open,ind_map)
deflators_open$Industry <- ifelse(is.na(deflators_open$Corrected),
                                  deflators_open$Industry,deflators_open$Corrected)
deflators_open$Corrected <- NULL
deflators_open <- filter(deflators_open, !is.na(X1997Q1))
deflators_open$Industry <- gsub("-", "_", deflators_open$Industry)

#Remove industry 84 buildings from deflators file (will use industry 85 instead as it has larger GFCF to weight with)
deflators_open <- filter(deflators_open, !(Asset=="OTHER.BUILDINGS" & Industry == "84"))

# Create 84 other buildings deflator from 85

ob85 <- filter(deflators_open, Asset=="OTHER.BUILDINGS" & Industry=="85")
ob85$Industry <- "84"
deflators_open <- rbind(deflators_open, ob85)

# Create deflators for other buildings and land improvements

os <- filter(deflators_open, Asset=="OTHER.BUILDINGS")
os$Asset <- "OTHER.STRUCTURES"
li <- os
li$Asset <- "LAND.IMPROVEMENTS.CON"

# Create deflators for other buildings and land improvements

ftc <- filter(deflators_open, Asset=="TC")
ftc$Asset <- "FTC"

# Create deflators for other machinery splits

ometc <- filter(deflators_open, Asset=="OTHER.MACHINERY")
ometc$Asset <- "OTHER.MACHINERY.TC"
deflators_open$Asset <- ifelse(deflators_open$Asset=="OTHER.MACHINERY",
                               "OTHER.MACHINERY.M", deflators_open$Asset)
deflators_open <- rbind(deflators_open,os,li,ometc,ftc)

# Set some industries to all industry

deflators_open$Industry <- ifelse(deflators_open$Asset=="CULTIVATED" & deflators_open$Industry=="01" |
                                    deflators_open$Asset=="RESEARCH.DEVELOPMENT" & deflators_open$Industry=="01" |
                                    deflators_open$Asset=="ENTERTAINMENT" & deflators_open$Industry=="58" |
                                    deflators_open$Asset=="WEAPONS" & deflators_open$Industry=="84" |
                                    deflators_open$Asset=="MIN" & deflators_open$Industry=="05_09",
                                  "ALL", deflators_open$Industry)
deflators_open$Asset <- ifelse(deflators_open$Asset=="TC",
                               "LAND.IMPROVEMENTS.TC",
                               deflators_open$Asset)

# Expand deflators to cover COFOGs

s1311_COFOG <- read_excel(paste0(inputDir, 
                                 "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"),
                          sheet = "COFOG_A64_S1311", col_types = "text")
s1311_COFOG <- rename(s1311_COFOG, Industry = A64)
s1311_COFOG <- left_join(deflators_open, s1311_COFOG)
s1311_COFOG$Sector <- "S.1311"

s1313_COFOG <- read_excel(paste0(inputDir, 
                                 "Mapping & Aggregation/hierarchies_sector_industry_asset.xlsx"),
                          sheet = "COFOG_A64_S1313", col_types = "text")
s1313_COFOG <- rename(s1313_COFOG, Industry = A64)
s1313_COFOG <- left_join(deflators_open, s1313_COFOG)
s1313_COFOG$Sector <- "S.1313"

s13_deflators <- rbind(s1311_COFOG,s1313_COFOG)
s13_deflators$Industry <- s13_deflators$COFOG
s13_deflators <- filter(s13_deflators, !is.na(Industry))
s13_deflators$COFOG <- NULL

# Remove soft.data

s13_deflators <- filter(s13_deflators, Asset!="SOFT.DATA(OA)")
s13_deflators <- filter(s13_deflators, Asset!="SOFT.DATA(P)")

deflators_open <- rbind(deflators_open, s13_deflators)

# Terminal costs deflator

term_deflator <- filter(deflators_open, Industry=="35", Asset=="OTHER.BUILDINGS")
term_deflator$Industry <- "ALL"
term_deflator$Asset <- "TERMINAL"
#FTC <- FTC[names(FTC) %in% names(deflators_open)]

deflators_open <- rbind(deflators_open, term_deflator)#, FTC)
deflators_open <- deflators_open %>% rename_(.dots=setNames(names(.), (gsub("X", "Y", names(.)))))
deflators_open <- gather(deflators_open, Period, Value, -Sector, -Industry, -Asset)

rm(all_ind, all_ind_d02, all_ind_d02ext, all_ind_sp,
   def02, def03, def05_09, deflators, ftc,
   product_mapping, qa, s13_deflators,
   s1311_COFOG, s1313_COFOG, term_deflator)

# S13 GFCF

# Read in industry and asset mappings

# Add S1311 mappings

s13_ind_map <- read.csv(paste0(inputDir,"Mapping & Aggregation/S13_Industry_mapping.csv"), stringsAsFactors=FALSE)
s13_ind_map <- s13_ind_map%>%mutate(G_COFOG=ifelse(G_COFOG=="1","01",
                                     ifelse(G_COFOG=="3.1","03.1",
                                            ifelse(G_COFOG=="3.2","03.2",
                                                   ifelse(G_COFOG=="3.6","03.6",
                                                          ifelse(G_COFOG=="4.1","04.1",
                                                                 ifelse(G_COFOG=="4.2","04.2",
                                                                        ifelse(G_COFOG=="4.5","04.5",
                                                                               ifelse(G_COFOG=="5","05",
                                                                                      ifelse(G_COFOG=="6.2","06.2",
                                                                                             ifelse(G_COFOG=="6.3","06.3",
                                                                                                    ifelse(G_COFOG=="6.4","06.4",
                                                                                                           ifelse(G_COFOG=="7","07",
                                                                                                                  ifelse(G_COFOG=="8","08",
                                                                                                                         ifelse(G_COFOG=="9","09",G_COFOG)))))))))))))))
s13_ass_map <- read.csv(paste0(inputDir,"Mapping & Aggregation/S13_Asset_mapping.csv"), stringsAsFactors=FALSE)

# Read in S13 GFCF

s1311_gfcf <- read.csv(paste0(inputDir,"GFCF/S13/CG.csv"), skip=10, stringsAsFactors=FALSE)
names(s1311_gfcf)[1:4] <- c("Industry", "G_Asset", "Descr", "Basis")

s1313_gfcf <- read.csv(paste0(inputDir,"GFCF/S13/LG.csv"), skip=7, stringsAsFactors=FALSE)
names(s1313_gfcf)[1:4] <- c("Basis", "G_COFOG", "G_Asset", "Descr")

# S1311 de-trunking adjustment

detrunking <- read.csv(paste0(inputDir,"Parameters, Assumptions & Adjustments/Detrunking_adjustment.csv") , stringsAsFactors=FALSE)
s1311_detrunking <- filter(detrunking, X=="GF0405")
names(s1311_detrunking)[1:4] <- c("Industry", "G_Asset", "Descr", "Basis")
s1311_detrunking <- s1311_detrunking[1:86]
existing <- filter(s1311_gfcf, Industry=="GF0405" &
                     G_Asset=="N112G_N1"& Descr=="P5113" &
                     Basis=="DISP")
existing <- existing[87:ncol(existing)]
s1311_detrunking <- cbind(s1311_detrunking,existing)
s1311_gfcf <- filter(s1311_gfcf, !(Industry=="GF0405" &
                                     G_Asset=="N112G_N1"& Descr=="P5113" &
                                     Basis=="DISP"))
s1311_gfcf <- rbind(s1311_gfcf,s1311_detrunking)

# Drop unmapped S1313 industries

s1313_gfcf <- left_join(s1313_gfcf, s13_ind_map)


s1313_gfcf$G_COFOG <- NULL
s1313_gfcf <- s1313_gfcf[,c(which(colnames(s1313_gfcf)=="Industry"),which(colnames(s1313_gfcf)!="Industry"))]
for (i in names(s1313_gfcf[5:ncol(s1313_gfcf)])){
  
  s1313_gfcf[i] <- as.numeric(unlist(s1313_gfcf[i]))
  
}

s1313_gfcf <- filter(s1313_gfcf, !is.na(Industry))

#s1313_check <- filter(s1313_gfcf, G_Asset=="T" & G_COFOG=="T")

# Combine S1311 & S1313

s1311_gfcf$Sector <- "S.1311"
s1313_gfcf$Sector <- "S.1313"
s13_gfcf_open <- rbind(s1311_gfcf, s1313_gfcf)
s13_gfcf_open <- left_join(s13_gfcf_open, s13_ass_map)
s13_gfcf_open$G_Asset <- NULL
s13_gfcf_open$Descr <- NULL
s13_gfcf_open <- filter(s13_gfcf_open, !is.na(Asset))

# Duplicate TERMINAL COSTS series for time being
# removing counting terminal costs twice

# duplicate <- filter(s13_gfcf_open, Asset=="TERMINAL")
# duplicate$Asset <- "OTHER.MACHINERY"
# s13_gfcf_open <- rbind(s13_gfcf_open, duplicate)

# Make adjustment for S1313 de-trunking

detrunking <- filter(detrunking, X=="S.1313")
names(detrunking)[1:4] <- c("Sector", "Industry", "Asset", "Basis")
detrunking <- detrunking[names(s13_gfcf_open)]
s13_gfcf_open <- rbind(s13_gfcf_open, detrunking)


# Calculate Net acquisition

basis_mapping <- read.csv(paste0(inputDir,"Mapping & Aggregation/Basis_mapping.csv"), stringsAsFactors=FALSE)
basis_mapping$Adjustment <- as.numeric(basis_mapping$Adjustment)
s13_gfcf_open <- left_join(s13_gfcf_open, basis_mapping)
s13_gfcf_open <- s13_gfcf_open[,c(which(colnames(s13_gfcf_open) %in% c("Sector","Asset","Adjustment")),which(!(colnames(s13_gfcf_open) %in% c("Sector","Asset","Adjustment"))))]

for (i in (names(s13_gfcf_open[6:ncol(s13_gfcf_open)]))){
  
  s13_gfcf_open[i][is.na(s13_gfcf_open[i])] <- 0
  s13_gfcf_open[i] <- s13_gfcf_open[i]*s13_gfcf_open['Adjustment']
  
}

s13_gfcf_open$Basis <- NULL
s13_gfcf_open$Adjustment <- NULL
s13_gfcf_open <- aggregate(. ~ Sector+Asset+Industry, data=s13_gfcf_open, FUN=sum)
s13_gfcf_open <- filter(s13_gfcf_open, Industry!="GF_NA")

rm(basis_mapping, detrunking, existing, s13_ass_map,
   s13_ind_map, s1311_detrunking, s1311_gfcf, s1313_gfcf)

# Rename columns

gfcf_r42_open <- gfcf_r42_open %>% rename_(.dots=setNames(names(.), (gsub("X", "Y", names(.)))))
s13_gfcf_open <- s13_gfcf_open %>% rename_(.dots=setNames(names(.), (gsub("X", "Y", names(.)))))

# Quality assurance checks

# Check for duplication of series

check_duplicated <- function (data, end_col) {
  if (nrow(data[duplicated(data[,1:end_col]),1:end_col])>0) {
    msg <- paste0("Duplicates in ", data)
    duplicates <- data[duplicated(data[,1:end_col]),1:end_col]
    flog.fatal(paste(msg, "series:"), data.frame(duplicates), capture = TRUE)
    stop(msg)
  }
}

check_duplicated(gfcf_r42_open,4)
check_duplicated(s13_gfcf_open,4)
deflators_open <- deflators_open[,c(which(colnames(deflators_open)=="Sector"),which(colnames(deflators_open)!="Sector"))]
check_duplicated(deflators_open,4)

# Check totals against sum of breakdown

# Check files against previous - tables & graphs

source("../QA.R")

# Remove series with no GFCF

s13_gfcf_open$total <- rowSums(abs(s13_gfcf_open[4:ncol(s13_gfcf_open)]))
s13_gfcf_open <- filter(s13_gfcf_open, total!=0)
s13_gfcf_open$total <- NULL
gfcf_r42_open$total <- rowSums(abs(gfcf_r42_open[4:ncol(gfcf_r42_open)]))
gfcf_r42_open <- filter(gfcf_r42_open, total!=0)
gfcf_r42_open$total <- NULL

# Read in old gfcf, deflators and compare

#if (QA_TABLES_CHARTS== TRUE) {


dir.create(file.path(outputDir, "QA"))

if(1 == 2) {
  
  if (exists('prev_deflators')){
    
    prev_deflators <- read_excel(paste0(last_run, 
                                        "/Outputs/QA/deflators_open.xlsx"),
                                 sheet = "new")
    prev_deflators <- gather(prev_deflators,"Period", "Value",4:ncol(prev_deflators))
    QA(deflators_open, prev_deflators, "deflators_open", "non-additive")
    
  }
  
  prev_gfcf_r42_open <- read_excel(paste0(last_run, 
                                          "/Outputs/QA/gfcf_r42_open.xlsx"),
                                   sheet = "new")
  
  if (exists('prev_gfcf_r42_open')){
    
    prev_gfcf_r42_open <- gather(prev_gfcf_r42_open,"Period", "Value",4:ncol(prev_gfcf_r42_open))
    cur_gfcf_r42_open <- gather(gfcf_r42_open,"Period", "Value",4:ncol(gfcf_r42_open))
    QA(cur_gfcf_r42_open, prev_gfcf_r42_open, "gfcf_r42_open", "non-additive")
    
  }
  
  #prev_gfcf_s13_open <- read_excel(paste0(last_run, 
  #  "/Outputs/QA/gfcf_s13_open.xlsx"),
  #  sheet = "new")
  
  if (exists('prev_gfcf_s13_open')){
    
    prev_gfcf_s13_open <- gather(prev_gfcf_s13_open,"Period", "Value",4:ncol(prev_gfcf_s13_open))
    cur_gfcf_s13_open <- gather(s13_gfcf_open,"Period", "Value",4:ncol(s13_gfcf_open))
    QA(cur_gfcf_s13_open, prev_gfcf_s13_open, "gfcf_s13_open", "non-additive")
    
  }
  
}

if (Include_LI_GFCF==TRUE){
  
  gfcf_r42_open$Asset <- ifelse(gfcf_r42_open$Asset=="LAND.IMPROVEMENTS","OTHER.BUILDINGS",gfcf_r42_open$Asset)
  gfcf_r42_open <- aggregate(. ~ Sector+Asset+Industry, data=gfcf_r42_open, FUN=sum)
  
}

# Remove quarters after forecastTo

quarters <- read.csv(paste0(inputDir, "Mapping & Aggregation/quarters.csv"))
quarters <- names(quarters)
quarters <- quarters[which(quarters==forecastTo)+1:length(quarters)]

gfcf_r42_open <- gfcf_r42_open %>% select(-one_of(quarters[quarters %in% names(gfcf_r42_open)]))
s13_gfcf_open <- s13_gfcf_open %>% select(-one_of(quarters[quarters %in% names(s13_gfcf_open)]))

# Create function taking two datasets
# Inputs - dfs, total,

# Remove files not needed from environment

rm(check_duplicated)

# Produce CS deflators

CS_def <- read.csv(paste0(inputDir,"Mapping & Aggregation/CS_deflators_template.csv"), stringsAsFactors=FALSE)
CS_def <- left_join(CS_def,ind_map)
CS_def$Industry <- ifelse(is.na(CS_def$Corrected),
                          CS_def$Industry,CS_def$Corrected)
CS_def$Corrected <- NULL
CS_def <- left_join(CS_def, deflators_open)
CS_def$Asset <- ifelse(CS_def$Asset=="LAND.IMPROVEMENTS.CON","LAND.IMPROVEMENTS",
                       ifelse(CS_def$Asset=="OTHER.MACHINERY.TC", "OTHER.M.SPLIT",
                              ifelse(CS_def$Asset=="OTHER.MACHINERY.TC","OTHER.MACHINERY",CS_def$Asset)))
CS_def <- filter(CS_def, !is.na(Period))
CS_def[is.na(CS_def$Value)] <- 0
CS_def <- spread(CS_def,Period, Value)
write.csv(CS_def, paste0(outputDir, "capital_services_deflators.csv"), row.names=FALSE)
