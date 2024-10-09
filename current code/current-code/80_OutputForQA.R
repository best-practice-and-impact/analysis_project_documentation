# ======================================== OUTPUT FOR QA CHECKS =================================================
# FORMATS DATA READY FOR 6 QA OUTPUTS
# 01 QACHK01_RUN_ON_RUN
# 02 QACHK02_CHECKING_SPREADSHEET
# 03 QACHK03_GRAPH_CHECKS_SPREADSHEET
# 04 QACHK04_DELIVERY_CHECKS_SPREADSHEET
# 05 QACHK05_NEGATIVE_CAPSTOCK_VALUES
# 06 QACHK06_T0301_DATA_DELIVERY_for_EUROSTAT

# CALLS FUNCTIONS FROM R SCRIPT FILE miscCapStocksFunctions

##################################################################################################################################
##################### SOURCE FILE FOR FUNCTIONS ##################################################################################
source("miscCapStocksFunctions.R")
##################################################################################################################################

#################################################### REPLICATION OF SPREADSHEET QACHK01_RUN_ON_RUN ###############################
#### GET THE DATA NEEDED FOR THE FIRST OUTPUT
QACH1ann <- chainedUnnestAnnual %>% filter(Industry == 'TOTAL')

#### REPLICATE CORD OUTPUT 1
QACH1rep <- replicateCORDOuputQACHK01or05or06(QACH1ann, 1)

################################ NEW SPREADSHEET REQUESTED BY DOUOG BAKER QACHK07_NEW_RUN_ON_RUN #################################

rm(QACH1ann)

###################################################################################################################################
#################################################### REPLICATION OF SPREADSHEET QACHK02_CHECKING_SPREADSHEET #####################
# NEED ALL THE DATA FOR THIS. ASSIGN DATA TO VARIABLES, COULD JUST USE THE ORIGINAL VARIABLES BUT WHATS LIFE WITHOUT WHIMSY.....
QACH2qtr <- chainedUnnest
QACH2ann <- chainedUnnestAnnual

# Quarterly and annual data needed for this
QACH2rep <- replicateCORDOuputQACHK02and03or07(QACH2ann, QACH2qtr, 2)

##################################################################################################################################
################################################# REPLICATION OF SPREADSHEET QACHK03_GRAPH_CHECKS_SPREADSHEET ####################
# Quarterly and annual data needed for this
QACH3rep <- replicateCORDOuputQACHK02and03or07(QACH2ann, QACH2qtr, 3)

QACH7rep <- replicateCORDOuputQACHK02and03or07(QACH2ann, QACH2qtr, 7)

rm(QACH2ann, QACH2qtr)

##################################################################################################################################
################################################### QACHK04_DELIVERY_CHECKS_SPREADSHEET ##########################################
# Number 4 is just a rearranged version of 2
QACH4rep <- replicateCORDOuputQACHK04(QACH2rep)

##################################################################################################################################
###################################### REPLICATION OF SPREADSHEET QACHK05_NEGATIVE_CAPSTOCK_VALUES ###############################
#### GET THE DATA NEEDED FOR THE FIRST OUTPUT
QACH5ann <- chainedUnnestAnnual %>% filter(Sector == 'S.1')

#### REPLICATE CORD OUTPUT 1
QACH5rep <- replicateCORDOuputQACHK01or05or06(QACH5ann, 5)
rm(QACH5ann)

##################################################################################################################################
############################## REPLICATION OF SPREADSHEET QACHK06_T0301_DATA_DELIVERY_for_EUROSTAT ###############################
#### GET THE DATA NEEDED FOR THE FIRST OUTPUT
QACH6ann <- chainedUnnestAnnual %>% filter(Sector == 'S.1', Asset == 'TOTAL')

#### REPLICATE CORD OUTPUT 1
QACH6rep <- replicateCORDOuputQACHK01or05or06(QACH6ann, 6)
rm(QACH6ann)
##################################################################################################################################
##################################################################################################################################

rm(QACH1rep, QACH2rep, QACH3rep, QACH4rep, QACH5rep, QACH6rep)
rm(QACH7rep)
