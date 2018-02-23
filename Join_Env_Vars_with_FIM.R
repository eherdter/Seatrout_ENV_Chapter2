# About ####

# R script for getting all of the FIM habitat and environmental variables together.
# Some of this script borrows from Delta_Method_For_Producing_Nominal
# Environmental variables collected by FIM include: Dissolved O2, Salinity, Temp, pH, secchi depth
# test change for github demonstration

# Set Working Directory ####
#must change working directory for data when working on personal vs work computer
rm(list=ls())

enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
enviro_data = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData"

personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
phys_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data"
phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"

# nutrient_dat = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Nutrients/Nitrogen"
# nutrient_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Nutrients"
# salinity = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Salinity"
# watertemp = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/WaterTemp"
# rainfall = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Rainfall"
# streamflow = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Streamflow"
# airtemp = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/AirTemp"
# airtemp = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/AirTemp"
# palmerz = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/PalmerZ"
# palmerz = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/PalmerZ"
# seagrass = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/SeagrassCover"
# 

out =   "U:/PhD_projectfiles/Exported_R_Datafiles"
out =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes"

setwd(personal_comp)
setwd(work_comp)

# Load Packages
####
library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
library(tictoc)
library(dplyr) 
library(tidyverse)

# About- Import Data Sets ####
# These data sets were produced using the spp_comb_5_13_EG_2bays_yoy_2015_EHedits.sas program which is stored in my scratch folder
# For more description see Delta_Method_for_Producing R script 

# select the important recruitment months for each zone and also check on gear codes
# _C$month => depends on recruitment window in each estuary
#               => Jax 5<=x<=11, => nor. IRL 5<=x<=11, => CK  5<=x<=11, => TB  4<=x<=10, => CH  4<=x<=10, => AP  6<=x<=10

# Load the data, select the peak reproductive months.
# Load the hydro dataset that contains Depth where the observation was taken, Temperature, Conductivity, pH, Salinity, Dissolved O2
# Load the physicical dataset that contains secchi depth, was secchi on bottom, depth (Depth) where hydrolab observation was taken
        # Note: this Depth is different then depth of location (That is StartDepth and EndDepth)
        # The physical data set is newly added so it contains additional sampling years but this should be taken care of when I join be reference. 
# There are some duplicated reference numbers in ap_hyd because the hydrolab took a few obsercations at each haul so I'll
# just have to chose one- drop all others for that reference. 
# Left join them by reference.
# Careful to see that the size of the original catch dataset doesn't change in size because we want to keep all of the catch 
# observations and not drop any that maybe do not have associated enviro variables or add observations of enviro variables to referecence numbers
# Reorder columns alphabetically so I can combine dataframes (some columns were in different position in other df)

# __________________ ####
#TAMPA BAY ####
#____________________####

# import catch ####
tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) 
tb_hyd <- subset(read_sas("tb_yoy_cn_hyd.sas7bdat")) 
tb_hyd <- tb_hyd[!duplicated(tb_hyd$Reference),]
#tb_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/tbm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
tb_phys <- read_sas(paste(phys_dat, "tbm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
tb_phys$Reference <- as.character(tb_phys$Reference)

tb <- left_join(tb, tb_hyd, by="Reference")
tb <- left_join(tb, tb_phys, by="Reference")
tb <- tb %>% select(noquote(order(colnames(tb))))  #reorders the columns alphabetically 

# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(tb$Zone)
unique(subset(tb, is.na(Longitude))$Zone) #assume if its missing Long then its also missing Lat

#Determine Zone-based medoids to apply to reference values (aka hauls) that have missing Lat Longs

tb_medA <- matrix(pam(subset(tb, Zone=="A" & !is.na(Longitude) & !is.na(Latitude), select=c("Longitude", "Latitude")),1)$medoids)
sum(is.na(tb_medA))

tb_medC <- matrix(pam(subset(tb, Zone=="C" & !is.na(Longitude) & !is.na(Latitude), select=c("Longitude", "Latitude")),1)$medoids)
sum(is.na(tb_medC))

tb_medD <- matrix(pam(subset(tb, Zone=="D" & !is.na(Longitude) & !is.na(Latitude), select=c("Longitude", "Latitude")),1)$medoids)
sum(is.na(tb_medD))

tb_medE <- matrix(pam(subset(tb, Zone=="E" & !is.na(Longitude) & !is.na(Latitude), select=c("Longitude", "Latitude")),1)$medoids)
sum(is.na(tb_medE))

tb_medM <- matrix(pam(subset(tb, Zone=="M" & !is.na(Longitude) & !is.na(Latitude), select=c("Longitude", "Latitude")),1)$medoids)
sum(is.na(tb_medM))

TB_cat <- tb %>% mutate(NewLong = ifelse(Zone == "A" & is.na(Longitude), tb_medA[1,],  ifelse(Zone=="C" & is.na(Longitude), tb_medC[1,], ifelse(Zone == "D" & is.na(Longitude), tb_medD[1,], ifelse(Zone=="E" & is.na(Longitude), tb_medE[1,], ifelse(Zone == "M" & is.na(Longitude), tb_medM[1,], tb$Longitude))))), NewLat = ifelse(Zone == "A" & is.na(Latitude), tb_medA[2,],  ifelse(Zone=="C" & is.na(Latitude), tb_medC[2,], ifelse(Zone == "D" & is.na(Latitude), tb_medD[2,], ifelse(Zone=="E" & is.na(Latitude), tb_medE[2,], ifelse(Zone == "M" & is.na(Latitude), tb_medM[2,], tb$Latitude))))))

#import length data ####
tb_length = subset(read_sas("tb_yoy_cn_l.sas7bdat"))
tb_length = subset(tb_length, bio_reference %in% unique(TB_cat$bio_reference))

tb_length_exp <- tb_length[rep(row.names(tb_length), tb_length$COUNT), 1:3]
tb_length_ag <- aggregate(sl ~ bio_reference, data=tb_length_exp, FUN= "median") 
colnames(tb_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

TB_cat <- left_join(TB_cat, tb_length_ag, by="bio_reference")
TB_cat$approx_age_mo <- round((2.476*TB_cat$median_sl - 0.012*(TB_cat$median_sl)^2)*0.0328767)
TB_cat$spawn_month <- TB_cat$month - TB_cat$approx_age_mo

#filter these lengths that don't make sense
TB_cat <- subset(TB_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values


#tidy catch ####

# *************ONLY USE WHEN RUNNING THROUGH JOINENV function 

#Format year to match the year in tb_nit. 
#Then trim TB_main to select the most important zones that occur at least 10% of the time. Also, select only unique
# Reference values for the dataset moving through the function to minimize computation. 

tidy_catch = function(catch) {
  catch <- catch %>% mutate(year = substr(as.character(catch$year),3,4)) #make year format match that of the coming nitrogen data
  catch$year <- as.factor(catch$year)
  catch$month <- as.numeric(catch$month)
  catch <- data.frame(catch)
  Zone.prop <- as.data.frame(prop.table(xtabs(~Zone, data=catch)))
  zone1 <- droplevels(Zone.prop[Zone.prop$Freq > 0.1,])  
  sel_zone=unique(zone1$Zone)
  catch_main <- subset(catch,!duplicated(Reference) & Zone %in% sel_zone)
  catch_main
}

TB_red <- tidy_catch(TB_cat)

# import environment #### 
#add in air temp
tb_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD3.csv", sep="/"), skip=4)
tb_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD3.csv", sep="/"),skip=4)

#add in nitrogen
tb_nit1 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_nit2 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Middle_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit3 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit <- rbind(tb_nit1, tb_nit2, tb_nit3)

#add in phosphorous
tb_ph1 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_ph2 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph3 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Middle_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph4 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph <- rbind(tb_ph1, tb_ph2, tb_ph3, tb_ph4)

# add in Palmer Z
tb_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD3.csv", sep="/" ), skip=3)

#add in rainfall
tb_rf1 <- read.csv(paste(enviro_data, "Rainfall/TB_Rainfall_89_97.csv", sep="/"))
tb_rf2 <- read.csv(paste(enviro_data, "Rainfall/TB_Rainfall_98_07.csv", sep="/"))
tb_rf3 <- read.csv(paste(enviro_data, "Rainfall/TB_Rainfall_08_17.csv", sep="/"))
tb_rf <- rbind(tb_rf1, tb_rf2, tb_rf3)

#add in salinity 
tb_sal1 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_HillsboroughBay_EPCRoutine.csv", sep="/")) 
tb_sal2 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_LowerTampaBay_EPCRoutine.csv", sep="/")) 
tb_sal3 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_MiddleTampaBay_EPCRoutine.csv", sep="/")) 
tb_sal4 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_OldTampaBay_EPCRoutine.csv", sep="/")) 
tb_sal <- rbind(tb_sal1, tb_sal2, tb_sal3, tb_sal4) 

 
#add in seagrass cover
tb_sg <- read.csv(paste(enviro_data, "SeagrassCover/SWFWMD_Seagrass_TB.csv", sep="/"))

#add in streamflow
tb_AR <- read.csv(paste(enviro_data, "Streamflow/TB/Alafia_River.csv", sep="/"), skip=28)
tb_HR <- read.csv(paste(enviro_data, "Streamflow/TB/Hillsborough_river.csv", sep="/"), skip=28)
tb_LMR <- read.csv(paste(enviro_data, "Streamflow/TB/Little.Manatee_River.csv", sep="/"), skip=28)

#add in water temp
tb_wt1 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Hillsborough_Bay.csv", sep="/"))
tb_wt2 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Lower_Tampa_Bay.csv", sep="/"))
tb_wt3 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Middle_Tampa_Bay.csv", sep="/"))
tb_wt <- rbind(tb_wt1, tb_wt2, tb_wt3)


#clean epc data and build DIN ####
nit <- tb_nit %>% select(-c(Characteristic, Original_Result_Unit, Original_Result_Value, QACode, DEP_WBID, ActivityDepth, Sample_Fraction, Result_Comment, Actual_StationID))
nit$SampleDate <- as.Date(nit$SampleDate , format= "%m/%d/%Y")
nit$SampleDate <- as.character(nit$SampleDate)
nit$StationID <- as.factor(nit$StationID)
nit <- nit[order(nit$SampleDate),]
nit <- nit %>% distinct(StationID, SampleDate, Parameter, .keep_all=TRUE) 

#fabricate the rows and then merge them in to the real dataset
nit2 <- nit %>% distinct(StationID, SampleDate, .keep_all=TRUE) %>% select(-c(Parameter, Result_Value))
library(splitstackshape)
nit2$rowcount <- 4
nit3 <- expandRows(nit2, 11)
nit3$Parameter <- c("NH3_N_ugl", "NOx_ugl", "norg_ugl", "TN_ugl")
nit3$source <- "new3"

#join this fabricated set to the real data
#n_sm <- new3 %>% select(StationID, SampleDate, Result_Value, Parameter)
nit1 <- nit %>% select(StationID, StationName, SampleDate, Parameter, Result_Value)
nit_join <- left_join(nit3, nit1, by=c("StationID", "StationName", "SampleDate", "Parameter")) 

#test <- nit_join %>% group_by(Parameter) %>% mutate(group_row=1:n()) 
nit_spread <- nit_join %>% group_by(Parameter) %>% mutate(group_row=1:n()) %>% spread(Parameter, Result_Value)
nit_spread$test <- 1:17339
#nit_spread$test2 <- test2$group_row - test2$test

nit_spread$DIN <- rowSums(nit_spread[,c(13,15)], na.rm=TRUE)


# build joinEV function ####

# THIS WORKS FOR TB_MAIN column references when PHYSICAL DATASET IS INCLUDED

#provide the catch dataset, the environmental data set, the fuzzy lat/long, variable name, and parameter name (as a character)
#function works for TB and CH for enviro variables nitrogen, phosphorous and salinity

#Param Name for Phosph   "TP_ugl"
#Param Name for Salinity "Salinity_ppt"
#Param Name for Water temp  "TempW_F"

#Param name for Phosphorous "OP_mgl" DIP #not enough stations 


# Define fuzzy lat/long boundaries 
# fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile
# fuzzy_long = 0.01 # 1 mile 


joinNit <- function(catch, env, fuzzy_lat, fuzzy_long, var_name){
  env <- data.frame(env)
  env$Date <- as.character(env$SampleDate)
  env <- droplevels( env %>% mutate(Year = substr(SampleDate, 3,4), Month = substr(SampleDate, 6,7)) %>% select(Actual_Latitude, Actual_Longitude,DIN,Result_Unit, Year, Month, StationID))
  env$Month <- as.numeric(env$Month)
  env$Year <- as.character(env$Year)
  
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  main_Year <- unique(catch$year)
  env = subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year)
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  
  # Start For Loop
  for(i in 1:nrow(catch))
  {
    # re-initialize this data structs for new pass
    #match_matrix <- as.list(seq_len(140))
    match_matrix <- as.data.frame(matrix(data=NA, nrow=100, ncol=7))
    hit_counter = 1 #initialize the hit counter that will be used in the below loop instead of indexing based off of j
    
    #define some variables
    filler_dist = 9999
    catch_year=catch[i,61]
    catch_month=catch[i,30]-1  #lag 1 month
    catch_lat=catch[i,65]
    catch_long=catch[i, 64]
    catch_ref = catch[i,41]
    TB_cor = as.numeric(c(catch[i,64], catch[i,65]))
    
    for(j in 1:nrow(env))
    {
      #define some more variables
      nit_year=env[j,5]
      nit_month=env[j,6]
      nit_lat =env[j,1]
      nit_long = env[j,2]
      nit_val = env[j,3]
      
      if (is.na(catch_year) | is.na(catch_month) | is.na(catch_lat) | is.na(catch_long))
      {
        print("NA Found")
      }
      else if((catch_year==nit_year)&(catch_month==nit_month))
      {
        #defining box to place around each coordinate pair assocaited with catch
        # this reduces the amount of coordinate points that are selected from within env that are then used in a pairwise distance comparison
        
        fuzzymax_lat = catch_lat +fuzzy_lat #catch_sub$NewLat
        fuzzymin_lat = catch_lat -fuzzy_lat
        fuzzymax_long = catch_long + fuzzy_long #TBcatch$NewLong
        fuzzymin_long = catch_long - fuzzy_long
        
        if((nit_lat > fuzzymin_lat) & (nit_lat < fuzzymax_lat) & (nit_long > fuzzymin_long) & (nit_long < fuzzymax_long)) 
          #if the lat long of nitrogen falls within the fuzzy lat long boundaries of the catch lat long found above then it will add into the dataframe below 
        {
          
          #m= as.data.frame(matrix(data=NA,nrow=1, ncol=7))
          m= as.list(seq_len(7))
          #m = data.frame(catch_month, catch_year, catch_ref, nit_val, nit_long, nit_lat, filler_dist)
          m[1] = catch_month
          m[2]= catch_year
          m[3]= catch_ref     
          m[4] = nit_val
          m[5] = nit_long
          m[6] = nit_lat
          m[7]= filler_dist #placeholder for distance 
          m_df <- data.frame(m, stringsAsFactors = FALSE)
          #colnames(m_df) <- c("x1", "x2","X3", "X4", "X5", "X6", "X7")
          
          match_matrix[hit_counter,] <- m_df
          hit_counter=hit_counter + 1
          
        }
        
      }
      
    }
    match_matrix[,7] = distm(match_matrix[,5:6], TB_cor) 
    match_matrix <- na.omit(match_matrix)
    
    nit_station_match = as.list(seq_len(1))
    nit_station_match <- match_matrix[match_matrix$V7 == min(match_matrix[,7], na.rm=T),] #select the nitrogen val from station that is closest of all 
    
    selected[[i]] <- nit_station_match #nitrogen station match adds in to predefined seleciton
    var_name = do.call(rbind, selected) #do.call bind
    
  }
  var_name
  
} 

  

joinEV <- function(catch, env, fuzzy_lat, fuzzy_long, var_name, Param_name) {
  
  #do some selection/cleaning on the enviro data based on the catch data to thin the enviro set out
  env$SampleDate <- as.factor(env$SampleDate)
  env <- env %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
  env$Date <- as.character(env$Date)
  
  env <- droplevels( env %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Parameter == Param_name) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID))
  env$Month <- as.numeric(env$Month)
  
  
  
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  main_Year <- unique(catch$year)
  env = subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year)
  
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  
  # Start For Loop
  for(i in 1:nrow(catch))
  {
    # re-initialize this data structs for new pass
    #match_matrix <- as.list(seq_len(140))
    match_matrix <- as.data.frame(matrix(data=NA, nrow=100, ncol=7))
    hit_counter = 1 #initialize the hit counter that will be used in the below loop instead of indexing based off of j
    
    #define some variables
    filler_dist = 9999
    catch_year=catch[i,61]
    catch_month=catch[i,30]-1  #lag 1 month
    catch_lat=catch[i,65]
    catch_long=catch[i, 64]
    catch_ref = catch[i,41]
    TB_cor = as.numeric(c(catch[i,64], catch[i,65]))
    
    for(j in 1:nrow(env))
    {
      #define some more variables
      nit_year=env[j,7]
      nit_month=env[j,8]
      nit_lat =env[j,1]
      nit_long = env[j,2]
      nit_val = env[j,6]
      
      if (is.na(catch_year) | is.na(catch_month) | is.na(catch_lat) | is.na(catch_long))
      {
        print("NA Found")
      }
      else if((catch_year==nit_year)&(catch_month==nit_month))
      {
        #defining box to place around each coordinate pair assocaited with catch
        # this reduces the amount of coordinate points that are selected from within env that are then used in a pairwise distance comparison
        
        fuzzymax_lat = catch_lat +fuzzy_lat #catch_sub$NewLat
        fuzzymin_lat = catch_lat -fuzzy_lat
        fuzzymax_long = catch_long + fuzzy_long #TBcatch$NewLong
        fuzzymin_long = catch_long - fuzzy_long
        
        if((nit_lat > fuzzymin_lat) & (nit_lat < fuzzymax_lat) & (nit_long > fuzzymin_long) & (nit_long < fuzzymax_long)) 
          #if the lat long of nitrogen falls within the fuzzy lat long boundaries of the catch lat long found above then it will add into the dataframe below 
        {
          
          #m= as.data.frame(matrix(data=NA,nrow=1, ncol=7))
          m= as.list(seq_len(7))
          #m = data.frame(catch_month, catch_year, catch_ref, nit_val, nit_long, nit_lat, filler_dist)
          m[1] = catch_month
          m[2]= catch_year
          m[3]= catch_ref     
          m[4] = nit_val
          m[5] = nit_long
          m[6] = nit_lat
          m[7]= filler_dist #placeholder for distance 
          m_df <- data.frame(m, stringsAsFactors = FALSE)
          #colnames(m_df) <- c("x1", "x2","X3", "X4", "X5", "X6", "X7")
          
          match_matrix[hit_counter,] <- m_df
          hit_counter=hit_counter + 1
          
        }
        
      }
      
    }
    match_matrix[,7] = distm(match_matrix[,5:6], TB_cor) 
    match_matrix <- na.omit(match_matrix)
    
    nit_station_match = as.list(seq_len(1))
    nit_station_match <- match_matrix[match_matrix$V7 == min(match_matrix[,7], na.rm=T),] #select the nitrogen val from station that is closest of all 
    
    selected[[i]] <- nit_station_match #nitrogen station match adds in to predefined seleciton
    var_name = do.call(rbind, selected) #do.call bind
    
  }
  var_name
  
} 

# END FUNCTION

#build joinCD function ####

#where env = the CD datasets (palmerZ, maxT and minT) and catch is the _main 
joinCD <- function(catch,env,env2, env3){ 
  env <- env %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(Z_val=Value, Z_anom = Anomaly)
  env$month <- as.numeric(env$month)
  env$year <- as.numeric(env$year)
  
  env2 <- env2 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(MaxT_val =Value, MaxT_anom = Anomaly)
  env2$month <- as.numeric(env2$month)
  env2$year <- as.numeric(env2$year)
  
  env3 <- env3 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(MinT_val=Value, MinT_anom = Anomaly)
  env3$month <- as.numeric(env3$month)
  env3$year <- as.numeric(env3$year)
  
  new <- left_join(catch, env, by =c("year", "month")) %>% select(-c(Date))
  new2 <- left_join(new, env2, by =c("year", "month")) %>% select(-c(Date))
  new3 <- left_join(new2, env3, by =c("year", "month")) %>% select(-c(Date))
  
  new3
} #END FUNCTION


#build clean riverflow function ####
cleanSF <- function(sf, name){
  sf <- sf %>% mutate(Date = as.Date(datetime, format="%m/%d/%Y"), year=substr(Date, 1,4), month=substr(Date, 6,7))
  colnames(sf) <- c("agency", "site_no", "datetime", "value", "code", "Date", "year", "month")
  sf <- sf %>% select(-c(agency, site_no, datetime, code))
  sf$value <- as.numeric(as.character(sf$value))
  av_sf <- aggregate(value ~ year + month, FUN= "mean", data=sf)
  av_sf$month <- as.numeric(av_sf$month)
  av_sf$year <- as.numeric(av_sf$year)
  colnames(av_sf) <- c("year", "month", "streamflow")
  av_sf$riv_name <- name
  av_sf
}


#build closest River function #### 
# To be used with TB_cat BEFORE joining of all other variables 

closestRiver = function(catch, riv){
  
  for(i in 1:nrow(catch)){
    cor = catch[i,64:65] 
    distance = distm(riv[,1:2], cor)
    dcomb= cbind(riv, distance)
    selec <- dcomb[dcomb[,4] == min(dcomb[,4]),]
    selec_riv <- selec[3]
    catch[i,69] <- selec_riv 
  }
  catch
}

#build join riverflow function ####

#TB_cat$river_flow <- 1

join_riverflow = function(catch, streamflow){

for (i in 1:nrow(TB_cat)){
  cat_month = TB_cat[i,30]
  cat_year = TB_cat[i,61] 
  cat_riv = TB_cat[i,69] 
  
  
  for (j in 1:nrow(streamfl)){
    riv_year = streamfl[j,1]
    riv_month = streamfl[j,2]
    riv_dis = streamfl[j,3]
    riv_name = streamfl[j,4]
    
    if((cat_riv==riv_name) & (cat_year == riv_year) & (cat_month == riv_month)){
      TB_cat[i,70] <- riv_dis
      
    } 
    
  }
  
}
  catch
}


#build clean rainfall function ####
cleanRF <- function(rf, name) {
  rf <- rf %>% dplyr::mutate(Date = as.Date(DATE, format= "%m/%d/%Y"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  dplyr::select(year, month, STATION_NAME, HOURLYPrecip)
  rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
  tot_rf <- aggregate(HOURLYPrecip ~ year + month, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
  tot_rf$month <- as.numeric(tot_rf$month)
  tot_rf$year <- as.numeric(tot_rf$year)
  #tb_tot_rf$month <- as.numeric(tb_tot_rf$month)
  colnames(tot_rf) <- c("year", "month", name)
  tot_rf
}


#join nitrogen, phosphorous, salinity, water temp ####
# tic()
#TB_shrt <- TB_red[1:100,]
#nit_full <- joinNit(TB_shrt, nit_spread, 0.017, 0.017, nitrogen)
# toc()
# write.csv(nit_full, paste(out, "TB_nit_join.csv", sep="/"))


#DIN
tic()
full <- joinNit(TB_red,nit_spread, 0.017, 0.017, nitrogen) 
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_017_DIN.csv", sep="/")) 


tic()
full <- joinNit(TB_red,nit_spread, 0.0288, 0.0288, nitrogen)
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_028_DIN.csv", sep="/")) 


tic()
full <- joinNit(TB_red,nit_spread, 0.043, 0.043, nitrogen) 
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_043_DIN.csv", sep="/")) 

# tic()
# ph_full <- joinEV(TB_red, tb_ph, 0.017, 0.017, phosphorous, "TP_ugl")
# toc()
# write.csv(ph_full, paste(out, "TB_ph_join.csv", sep="/"))
# 
# tic()
# sal_full <- joinEV(TB_red, tb_sal, 0.017, 0.017, salinity, "Salinity_ppt")
# toc()
# write.csv(sal_full, paste(out, "TB_sal_join.csv", sep="/"))
# 
# tic()
# wt_full <- joinEV(TB_red, tb_wt, 0.017, 0.017, watertemp, "TempW_F")
# toc()
# write.csv(wt_full, paste(out, "TB_wt_join.csv", sep="/"))

# merge closest river mouth ####
AR_mouth = c(-82.398480, 27.853702) #, "AR") #long, lat format
HR_mouth = c(-82.461944, 27.937778) #, "HR")
LMR_mouth = c(-82.486, 27.716) #, "LMR")
riv_name= c("AR", "HR", "LMR")
rivers = data.frame(rbind(AR_mouth, HR_mouth, LMR_mouth))
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)


TB_cat$closest_riv <- ""
TB_cat <- closestRiver(TB_cat, rivers)

# merge riverflow ####
tb_av_AR <- cleanSF(tb_AR, "AR") #mean discharge in cubic feet/second
tb_av_HR <- cleanSF(tb_HR, "HR") #mean discharge in cubic feet/second
tb_av_LMR <- cleanSF(tb_LMR, "LMR")     #mean discharge in cubic feet/second

streamfl <- rbind(tb_av_AR, tb_av_HR, tb_av_LMR)
streamfl <- subset(streamfl, month >= min(unique(TB_cat$month)) & year>= min(unique(TB_cat$year)))


TB_cat$river_flow <- 1
tic()
for (i in 1:nrow(TB_cat)){
  cat_month = TB_cat[i,30]
  cat_year = TB_cat[i,61] 
  cat_riv = TB_cat[i,69] 

    
    for (j in 1:nrow(streamfl)){
      riv_year = streamfl[j,1]
      riv_month = streamfl[j,2]
      riv_dis = streamfl[j,3]
      riv_name = streamfl[j,4]
      
      if((cat_riv==riv_name) & (cat_year == riv_year) & (cat_month == riv_month)){
        TB_cat[i,70] <- riv_dis
        
      } 
      
    }
    
  }
toc()

# TO DO- merge nitrogen, phos, salinity, water temp ####

#DIN
TB_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(TB_nit) <- c("Reference", "Nit_val")

#this is nh3
#TB_nit2 <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_043_nh3.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
#colnames(TB_nit2) <- c("Reference", "Nit_val")

#TB_nit <-left_join(TB_nit1, TB_nit2, by="Reference")
#adding them together will get total DIN
#TB_nit <- TB_nit %>% mutate(total_DIN = rowSums(TB_nit[,2:3])) %>% select(Reference, total_DIN)

TB_phos <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_ph_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_phos) <- c("Reference", "Phos_val")

TB_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_sal) <- c("Reference", "Sal_val")

TB_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_wat) <- c("Reference", "WatTemp_val")


#join back to original catch dataset- not the reduced one. 
TB_new <-  left_join(TB_cat, TB_nit, by="Reference")
TB_new2 <-  left_join(TB_new, TB_phos, by="Reference")
TB_new3 <-  left_join(TB_new2, TB_sal, by="Reference")
TB_new4 <-  left_join(TB_new3, TB_wat, by="Reference")

# merge airtemp and palmerZ (climate zones-CD) ####

TB_new5 <- joinCD(TB_new4, tb_PZ,tb_maxT,tb_minT)

#merge rainfall ####
tb_tot_rf <- cleanRF(tb_rf, "MeanMonthlyRF")

TB_new6 <- left_join(TB_new5, tb_tot_rf, by=c("year", "month"))

#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO

TB_new6 <- TB_new6 %>% mutate(aten_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(TB_new6, paste(out, "Seatrout_ENV_Chapter2/TB_all_env_no_lag.csv", sep="/"))

# . ####
# Lag Variable Calculations ####
# . ####

TB_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_all_env_no_lag.csv",sep="/"), header=T)
TB_cat_env <- TB_cat_env[,-1]
TB_cat_env$closest_riv <- as.character(TB_cat_env$closest_riv)
#works
# for(i in 1:nrow(TB_cat)){
# cor = TB_cat[i,62:63] #64,65
# distance = distm(rivers[,1:2], cor)
# dcomb= cbind(rivers, distance)
# selec <- dcomb[dcomb[,4] == min(dcomb[,4]),]
# selec_riv <- selec[3]
# TB_cat[i,64] <- selec_riv #change this index too
# }

# TB_cat$closest_riv <- as.factor(TB_cat$closest_riv)
# table(TB_cat$closest_riv)

# build clean/create seasonal streamflow function ####
# makes mean seasonal 
# seasonally averaged discharge (Purtlebaugh and Allen)
#spring discharge ( March - May); align catch to river discharge that is closest based on lat and long 
# mean discharge during March - May months (3,4,5); first must determine the approximate lat and long location for each river mouth 
#mean discharge in cubic feet/second
# For Tampa Bay - USGS 02301500 Alafia River at Lithia FL; 27,52, 19 (=27.8719) ; -82.12.41 (= -82.2114)
# Hillsborough river USGS 02304500 Hillsborough River near Tampa FL; 28.01.25 (= 28.0236 ); -82.25.40 (=-82.4278)
# Little manatee river USGS 02300500; 27.40.15 (=27.6708) ; -82.21.10 (= -82.3528)


cleanSF_withSeason <- function(sf, name){
  sf <- sf[-1,]
  sf <- sf %>% mutate(Date = as.Date(datetime, format="%m/%d/%Y"), year=as.numeric(substr(Date, 1,4)), month=substr(Date, 6,7)) %>% select(-c(Date))
  colnames(sf) <- c("agency", "site_no", "datetime", "value", "code", "year", "month")
  sf <- sf %>% select(-c(agency, site_no, datetime, code))
  sf$value <- as.numeric(as.character(sf$value))
  sf$season <- ifelse(sf$month %in% c("03","04","05"), "spring", ifelse(sf$month %in% c("06","07","08","09"), "summer", ifelse(sf$month %in% c("10","11", "12"), "autumn", ifelse(sf$month %in% c("01","02"), "winter", "NA"))))
  av_seas_sf <- aggregate(value ~ year + season, FUN= "mean", data=sf)
  #av_seas_sf <- subset(av_seas_sf, season =="spring")
  av_seas_sf$year <- as.numeric(av_seas_sf$year)
  colnames(av_seas_sf) <- c("year", "season", name)
  av_seas_sf
} 

# build join seasonal streamflow function ####
#throws this error:   restarting interrupted promise evaluation 
# not sure if its a problem but I have pulled a random subsample 

join_seas_streamflow= function(catch, seas_sf){

catch$spring_dis <- NA #83
catch$summer_dis <- NA #84
catch$winter_dis <- NA #85
catch$prev_autumn_dis <-NA #86

for (i in 1:nrow(catch)){
  
  cat_month = catch[i,30]
  cat_year = catch[i,61] 
  previous_year = catch[i,61] - 1 #will this work: yes
  cat_riv = catch[i,69] #69

  if (cat_month >=6) { 
    
    for (j in 1:nrow(seas_sf)){
      riv_year = seas_sf[j,1]
      riv_seas = seas_sf[j,2]
      riv_dis = seas_sf[j,3]
      riv_name = seas_sf[j,4]
      
      if((cat_riv == riv_name) & (cat_year == riv_year) & (riv_seas == "spring")){
        catch[i,83] <- riv_dis
      }
        
    }
  }

#assign summer flow to all months after entire summer season
    
  if (cat_month >=9) { 
    
    for (j in 1:nrow(seas_sf)){
      riv_year = seas_sf[j,1]
      riv_seas = seas_sf[j,2]
      riv_dis = seas_sf[j,3]
      riv_name = seas_sf[j,4]
      
      if((cat_riv == riv_name) & (cat_year == riv_year) & (riv_seas == "summer")){
        catch[i,84] <- riv_dis
      }
    }
  }
  
 #  #assign autumn flow to all months after entire autumn season 
 # if (cat_month >=11) { 
 #    
 #    for (j in 1:nrow(seas_sf)){
 #      riv_year = seas_sf[j,1]
 #      riv_seas = seas_sf[j,2]
 #      riv_dis = seas_sf[j,3]
 #      riv_name = seas_sf[j,4]
 #      
 #      if((cat_riv==riv_name) & (cat_year == riv_year) & (riv_seas == "autumn")){
 #        catch[i,82] <- riv_dis
 #      }
 #    }
 # }  
 
  #assign winter flow to closer months that might be affected
  if (cat_month >=3 & cat_month <=5) { 
    
    for (j in 1:nrow(seas_sf)){
      riv_year = seas_sf[j,1]
      riv_seas = seas_sf[j,2]
      riv_dis = seas_sf[j,3]
      riv_name = seas_sf[j,4]
      
      if((cat_riv==riv_name) & (cat_year == riv_year) & (riv_seas == "winter")){
        catch[i,85] <- riv_dis
      }
    }
  }

  #assign previous years autumn to early early months
   if (cat_month <=5) { 
  
    for (j in 1:nrow(seas_sf)){
      riv_year = seas_sf[j,1]
      riv_seas = seas_sf[j,2]
      riv_dis = seas_sf[j,3]
      riv_name = seas_sf[j,4]
      
      if((cat_riv==riv_name) & (previous_year == riv_year) & (riv_seas == "autumn")){
        catch[i,86] <- riv_dis
        
      }
    }
  }
  
}
 catch
}
    
    
# build clean/create seasonal CD (airtemp and palmerZ) #### 

# env= tb_PZ
#env2= tb_maxT
#env3= tb_minT

# turn to env

clean_seasCD= function(env, env2, env3){

env <- env %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(Z_val=Value, Z_anom = Anomaly) %>% select(-Date)
env$month <- as.numeric(env$month)
env$year <- as.numeric(env$year)

env2 <- env2 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(MaxT_val =Value, MaxT_anom = Anomaly) %>% select(-Date)
env2$month <- as.numeric(env2$month)
env2$year <- as.numeric(env2$year)

env3 <- env3 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% rename(MinT_val =Value, MinT_anom = Anomaly)%>% select(-Date)
env3$month <- as.numeric(env3$month)
env3$year <- as.numeric(env3$year)

join <- left_join(env, env2, by=c("year", "month")) %>% left_join(env3, by=c("year", "month"))
join$season <- ifelse(join$month %in% c("3","4","5"), "spring", ifelse(join$month %in% c("6","7","8","9"), "summer", ifelse(join$month %in% c("10","11", "12"), "autumn", ifelse(join$month %in% c("1","2"), "winter", "NA"))))
join_seas <- aggregate(cbind(Z_val, Z_anom, MaxT_val, MaxT_anom, MinT_val, MinT_anom) ~ year + season, FUN= "mean", data=join)

join_seas
}


#build join seasonal CD (airtemp and palmerZ) #### 

#TB_shrt <- TB_cat_new[1000:4000,] #brings it back to original configuration

join_seasCD= function(catch, seasonal_CD){
  
  catch$spring_Z_val <- NA #87
  catch$spring_Z_anom <- NA #88
  catch$spring_MaxT_val <- NA #89
  catch$spring_MaxT_anom <- NA #90
  catch$spring_MinT_val <- NA #91
  catch$spring_MinT_anom <- NA #92
  
  catch$summer_Z_val <- NA #93
  catch$summer_Z_anom <- NA #94
  catch$summer_MaxT_val <- NA #95
  catch$summer_MaxT_anom <- NA #96
  catch$summer_MinT_val <- NA #97
  catch$summer_MinT_anom <- NA #98
  
  catch$winter_Z_val <- NA #99
  catch$winter_Z_anom <- NA #100
  catch$winter_MaxT_val <- NA #101
  catch$winter_MaxT_anom <- NA #102
  catch$winter_MinT_val <- NA #103
  catch$winter_MinT_anom <- NA #104
  
  catch$prev_autumn_Z_val <- NA #105
  catch$prev_autumn_Z_anom <- NA #106
  catch$prev_autumn_MaxT_val <- NA #107
  catch$prev_autumn_MaxT_anom <- NA #108
  catch$prev_autumn_MinT_val <- NA #109
  catch$prev_autumn_MinT_anom <- NA #110

  for (i in 1:nrow(catch)){
    
    cat_month = catch[i,30]
    cat_year = catch[i,61] 
    previous_year = catch[i,61] - 1 #will this work: yes
    
    if (cat_month >=6) { 
      
      for (j in 1:nrow(seasonal_CD)){
        CD_year = seasonal_CD[j,1]
        CD_seas = seasonal_CD[j,2]
        CD_zval = seasonal_CD[j,3]
        CD_zanom = seasonal_CD[j,4]
        CD_maxTval = seasonal_CD[j,5]
        CD_maxTanom = seasonal_CD[j,6]
        CD_minTval = seasonal_CD[j,7]
        CD_minTanom = seasonal_CD[j,8]
        
        if((cat_year == CD_year) & (CD_seas == "spring")){
          catch[i,87] <- CD_zval
          catch[i,88] <- CD_zanom
          catch[i,89] <- CD_maxTval
          catch[i,90] <- CD_maxTanom
          catch[i,91] <- CD_minTval
          catch[i,92] <- CD_minTanom
         
        }
      }
    }
  
    #assign summer flow to all months after entire summer season
    if (cat_month >=9) { 
      
      for (j in 1:nrow(seasonal_CD)){
        CD_year = seasonal_CD[j,1]
        CD_seas = seasonal_CD[j,2]
        CD_zval = seasonal_CD[j,3]
        CD_zanom = seasonal_CD[j,4]
        CD_maxTval = seasonal_CD[j,5]
        CD_maxTanom = seasonal_CD[j,6]
        CD_minTval = seasonal_CD[j,7]
        CD_minTanom = seasonal_CD[j,8]
        
        if((cat_year == CD_year) & (CD_seas == "summer")){
          catch[i,93] <- CD_zval
          catch[i,94] <- CD_zanom
          catch[i,95] <- CD_maxTval
          catch[i,96] <- CD_maxTanom
          catch[i,97] <- CD_minTval
          catch[i,98] <- CD_minTanom
        }
      }
    }
  
    #assign winter flow to closer months that might be affected
    if (cat_month >=3 & cat_month <=5) { 
      
      for (j in 1:nrow(seasonal_CD)){
        CD_year = seasonal_CD[j,1]
        CD_seas = seasonal_CD[j,2]
        CD_zval = seasonal_CD[j,3]
        CD_zanom = seasonal_CD[j,4]
        CD_maxTval = seasonal_CD[j,5]
        CD_maxTanom = seasonal_CD[j,6]
        CD_minTval = seasonal_CD[j,7]
        CD_minTanom = seasonal_CD[j,8]
        
        if((cat_year == CD_year) & (CD_seas == "winter")){
          catch[i,99] <- CD_zval
          catch[i,100] <- CD_zanom
          catch[i,101] <- CD_maxTval
          catch[i,102] <- CD_maxTanom
          catch[i,103] <- CD_minTval
          catch[i,104] <- CD_minTanom
          
        }
      }
    }
  
    #assign previous years autumn to early early months
    if (cat_month <=5) { 
      
      for (j in 1:nrow(seasonal_CD)){
        CD_year = seasonal_CD[j,1]
        CD_seas = seasonal_CD[j,2]
        CD_zval = seasonal_CD[j,3]
        CD_zanom = seasonal_CD[j,4]
        CD_maxTval = seasonal_CD[j,5]
        CD_maxTanom = seasonal_CD[j,6]
        CD_minTval = seasonal_CD[j,7]
        CD_minTanom = seasonal_CD[j,8]
        
        if((previous_year == CD_year) & (CD_seas == "autumn")){
          catch[i,105] <- CD_zval
          catch[i,106] <- CD_zanom
          catch[i,107] <- CD_maxTval
          catch[i,108] <- CD_maxTanom
          catch[i,109] <- CD_minTval
          catch[i,110] <- CD_minTanom
          
        }
      }
    }
  }
  catch
}

#build clean/create seasonal rainfall ####

clean_seasRF <- function(rf) {
  rf <- rf
  rf <- rf %>% mutate(Date = as.Date(DATE, format= "%m/%d/%Y"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  select(year, month, HOURLYPrecip)
  rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
  rf$season <- ifelse(rf$month %in% c("03","04","05"), "spring", ifelse(rf$month %in% c("06","07","08","09"), "summer", ifelse(rf$month %in% c("10","11", "12"), "autumn", ifelse(rf$month %in% c("01","02"), "winter", "NA"))))
  tot_rf <- aggregate(HOURLYPrecip ~ year + season, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
  tot_rf$year <- as.numeric(tot_rf$year)
  colnames(tot_rf) <- c("year", "season", "total_rf")
  tot_rf
}

#build join seasonal rainfall ####

join_seasRF= function(catch, seasonal_RF){
  
  catch$spring_RF <- NA #111
  catch$summer_RF <- NA #112
  catch$winter_RF <- NA #113
  catch$prev_autumn_RF <- NA #114
  
  for (i in 1:nrow(catch)){
    
    cat_month = catch[i,30]
    cat_year = catch[i,61] 
    previous_year = catch[i,61] - 1 #will this work: yes
    
    if (cat_month >=6) { 
      
      for (j in 1:nrow(seasonal_RF)){
        RF_year = seasonal_RF[j,1]
        RF_seas = seasonal_RF[j,2]
        RF_precip = seasonal_RF[j,3]
           
        if((cat_year == RF_year) & (RF_seas == "spring")){
          catch[i,111] <- RF_precip
        }
      }
    }
    
    #assign summer flow to all months after entire summer season
    if (cat_month >=9) { 
      
      for (j in 1:nrow(seasonal_RF)){
        RF_year = seasonal_RF[j,1]
        RF_seas = seasonal_RF[j,2]
        RF_precip = seasonal_RF[j,3]
            
        if((cat_year == RF_year) & (RF_seas == "summer")){
          catch[i,112] <- RF_precip
        }
      }
    }
    #assign winter flow to closer months that might be affected
    if (cat_month >=3 & cat_month <=5) { 
      
      for (j in 1:nrow(seasonal_RF)){
        RF_year = seasonal_RF[j,1]
        RF_seas = seasonal_RF[j,2]
        RF_precip = seasonal_RF[j,3]
           
        if((cat_year == RF_year) & (RF_seas == "winter")){
          catch[i,113] <- RF_precip
        }
      }
    }
    #assign previous years autumn to early early months
    if (cat_month <=5) { 
      
      for (j in 1:nrow(seasonal_RF)){
        RF_year = seasonal_RF[j,1]
        RF_seas = seasonal_RF[j,2]
        RF_precip = seasonal_RF[j,3]
        
        if((previous_year == RF_year) & (RF_seas == "autumn")){
          catch[i,114] <- RF_precip
        }
      }
    }
  }
  catch
}


# build join seasonal ALL streams ####
# it does not match based on river like the one above

join_seas_streamALL= function(catch, seas_sf){
  
  catch$spring_dis_ALL <- NA #115
  catch$summer_dis_ALL <- NA #116
  catch$winter_dis_ALL <- NA # 117
  catch$prev_autumn_dis_ALL <-NA #118
  
  for (i in 1:nrow(catch)){
    
    cat_month = catch[i,30]
    cat_year = catch[i,61] 
    previous_year = catch[i,61] - 1 #will this work: yes
    #cat_riv = catch[i,66] #66
    
    if (cat_month >=6) { 
      
      for (j in 1:nrow(seas_sf)){
        riv_year = seas_sf[j,1]
        riv_seas = seas_sf[j,2]
        riv_dis = seas_sf[j,3]
        #riv_name = seas_sf[j,4]
        
        if((cat_year == riv_year) & (riv_seas == "spring")){
          catch[i,115] <- riv_dis
        }
        
      }
    }
    
    #assign summer flow to all months after entire summer season
    
    if (cat_month >=9) { 
      
      for (j in 1:nrow(seas_sf)){
        riv_year = seas_sf[j,1]
        riv_seas = seas_sf[j,2]
        riv_dis = seas_sf[j,3]
        #riv_name = seas_sf[j,4]
        
        if((cat_year == riv_year) & (riv_seas == "summer")){
          catch[i,116] <- riv_dis
        }
      }
    }
    
    if (cat_month >=3 & cat_month <=5) { 
      
      for (j in 1:nrow(seas_sf)){
        riv_year = seas_sf[j,1]
        riv_seas = seas_sf[j,2]
        riv_dis = seas_sf[j,3]
        #riv_name = seas_sf[j,4]
        
        if((cat_year == riv_year) & (riv_seas == "winter")){
          catch[i,117] <- riv_dis
        }
      }
    }
    
    #assign previous years autumn to early early months
    if (cat_month <=5) { 
      
      for (j in 1:nrow(seas_sf)){
        riv_year = seas_sf[j,1]
        riv_seas = seas_sf[j,2]
        riv_dis = seas_sf[j,3]
        #riv_name = seas_sf[j,4]
        
        if((previous_year == riv_year) & (riv_seas == "autumn")){
          catch[i,118] <- riv_dis
          
        }
      }
    }
    
  }
  catch
}




#build clean/create seasonal & @spawning mean salinity and water temp####

# determine spawning location for each bay area if possible and just use data on salinity and water temp from those areas
#for Tampa Bay its the lower bay 

selected_stations = c(93,92,24,22,95,91,96,90,25,23,21,28,19,16,84,81,9)

clean_seas_sal_wt <- function(env, env2, Param_name1, Param_name2, selected_stations, flag){

  if (flag == "seasonal") {
#do some selection/cleaning on the enviro data based on the catch data to thin the enviro set out
env = env %>% subset(StationID %in% selected_stations)
env$SampleDate <- as.factor(env$SampleDate)
env <- env %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
env$Date <- as.character(env$Date)
env <- droplevels( env %>% mutate(year = substr(Date, 1,4), month = substr(Date, 6,7)) %>% subset(Parameter == Param_name1) %>% dplyr::select(Characteristic,Parameter,Result_Unit, Result_Value, year, month) %>% rename(Salinity=Result_Value))
env$month <- as.numeric(env$month)

env2 = env2 %>% subset(StationID %in% selected_stations)
env2$SampleDate <- as.factor(env2$SampleDate)
env2 <- env2 %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
env2$Date <- as.character(env2$Date)
env2 <- droplevels( env2 %>% mutate(year = substr(Date, 1,4), month = substr(Date, 6,7)) %>% subset(Parameter == Param_name2) %>% select(Characteristic,Parameter,Result_Unit, Result_Value, year, month) %>% rename(Temperature=Result_Value))
env2$month <- as.numeric(env2$month)

join <- left_join(env, env2, by=c("year", "month"))
join$season <- ifelse(join$month %in% c("4","5"), "first", ifelse(join$month %in% c("6","7"), "second", ifelse(join$month %in% c("8", "9"), "third", "NA")))
join<- aggregate(cbind(Salinity, Temperature) ~ year + season, FUN= "mean", data=join)
join$year <- as.factor(join$year)
join
  }


else if (flag == "monthly") {
env = env %>% subset(StationID %in% selected_stations)
env$SampleDate <- as.factor(env$SampleDate)
env <- env %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
env$Date <- as.character(env$Date)
env <- droplevels( env %>% mutate(year = substr(Date, 1,4), month = substr(Date, 6,7)) %>% subset(Parameter == Param_name1) %>% dplyr::select(Characteristic,Parameter,Result_Unit, Result_Value, year, month) %>% rename(Salinity=Result_Value))
env$month <- as.numeric(env$month)

env2 = env2 %>% subset(StationID %in% selected_stations)
env2$SampleDate <- as.factor(env2$SampleDate)
env2 <- env2 %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
env2$Date <- as.character(env2$Date)
env2 <- droplevels( env2 %>% mutate(year = substr(Date, 1,4), month = substr(Date, 6,7)) %>% subset(Parameter == Param_name2) %>% select(Characteristic,Parameter,Result_Unit, Result_Value, year, month) %>% rename(Temperature=Result_Value))
env2$month <- as.numeric(env2$month)

join <- left_join(env, env2, by=c("year", "month")) %>% select(year, month, Salinity, Temperature)
join<- aggregate(cbind(Salinity, Temperature) ~ year + month, FUN= "mean", data=join)
join$year <- as.factor(join$year)
join
}
  join
}

#test1 <- clean_seas_sal_wt(tb_sal, tb_wt, "Salinity_ppt", "TempW_F", selected_stations, "seasonal")
#test2 <- clean_seas_sal_wt(tb_sal, tb_wt, "Salinity_ppt", "TempW_F", selected_stations, "monthly")


# build join @spawning salinity & watertemp ####

join_spawn_SAWT = function(catch, seas_SAWT) {
  catch$atspawn_salinity <- NA #119
  catch$atspawn_waterT <- NA #120
  
  for (i in 1:nrow(catch)){
    
    spawn_month = catch[i,68]
    cat_year = catch[i,61] 
  
    
    for (j in 1:nrow(seas_SAWT)){
      year = seas_SAWT[j,1]
      time = seas_SAWT[j,2]
      sal = seas_SAWT[j,3]
      temp = seas_SAWT[j,4]
      
      
      if((cat_year == year) & (spawn_month == time)){
        catch[i,119] <- sal
        catch[i,120] <- temp
      }
    }
  }
  catch
}



# build join seasonal salinity & watertemp  ####

join_seas_SAWT= function(catch, seas_SAWT){
  
  catch$first_spawn_salinity <- NA #121
  catch$first_spawn_waterT <- NA #122
  
  catch$second_spawn_salinity <- NA #123
  catch$second_spawn_waterT <- NA #124
  
  catch$third_spawn_salinity <- NA #125
  catch$third_spawn_waterT <- NA #126
  
  for (i in 1:nrow(catch)){
    
    cat_month = catch[i,30]
    cat_year = catch[i,61] 
    previous_year = catch[i,61] - 1 #will this work: yes

    #assign first season- exclude those caught before length of first season =  if they existed in months 6,7,8,9,10 then they could have been born from efforts from the first spawning season
    if (cat_month >= 4) { 
      
      for (j in 1:nrow(seas_SAWT)){
        year = seas_SAWT[j,1]
        seas = seas_SAWT[j,2]
        sal = seas_SAWT[j,3]
        temp = seas_SAWT[j,4]
 
        
        if((cat_year == year) & (seas == "first")){
          catch[i,121] <- sal
          catch[i,122] <- temp
        }
      }
    }

    # assign second season- exclude those cause before length of second season =  if they existed in months 8,9,10,11,12 then they could have been born from efforts form the second spawning season (i.e if they were caught in mo=5 then they were already spawned-MUST be from first spawn season)
    
    if (cat_month >= 7) { 
         
      for (j in 1:nrow(seas_SAWT)){
        year = seas_SAWT[j,1]
        seas = seas_SAWT[j,2]
        sal = seas_SAWT[j,3]
        temp = seas_SAWT[j,4]
        
        
        if((cat_year == year) & (seas == "second")){
          catch[i,123] <- sal
          catch[i,124] <- temp
        }
      }
    }
    
    # assign third season- only those caught after the end of the third season could have been from that effort 
    
    if (cat_month >= 9) { 
      
      for (j in 1:nrow(seas_SAWT)){
        year = seas_SAWT[j,1]
        seas = seas_SAWT[j,2]
        sal = seas_SAWT[j,3]
        temp = seas_SAWT[j,4]
        
        if((cat_year == year) & (seas == "third")){
          catch[i,125] <- sal
          catch[i,126] <- temp
        }
      }
    }
  }
  catch
}


# build clean/create seasonal nitro ####

clean_seas_nitro <- function(nit_spread, selected_stations){

env = nit_spread %>% subset(StationID %in% selected_stations)
env$SampleDate <- as.factor(env$SampleDate)
env <- droplevels( env %>% mutate(year = substr(SampleDate, 1,4), month = substr(SampleDate, 6,7)) %>% subset %>% dplyr::select( DIN, year, month)) 
env$month <- as.numeric(env$month)

nit_ag <- aggregate(DIN ~ year + month, FUN= "mean", data=env)
nit_ag $year <- as.factor(nit_ag $year)
nit_ag 
}

#TO DO - build join seasonal nitro ####

join_spawn_nitro = function(catch, seas_nitro) {
  catch$atspawn_nitro <- NA #127
  catch$avg_last2_nitro <- NA #128
  catch$avg_last3_nitro <- NA #129
  
  for (i in 1:nrow(catch)){
    
  
    before1_spawn = catch[i,68]-1
    before2_spawn = catch[i,68]-2
    before3_spawn= catch[i,68]-3
    cat_year = catch[i,61] 
  
    
    for (j in 1:nrow(seas_SAWT)){
      DIN = seas_nitro[j,1]
      year = seas_nitro[j,2]
     month = seas_nitro[j,3]
  
    }
     
     if((cat_year == year) & (before1_spawn == month)) {
        spawn_1[i] <- DIN
       catch[i,127] <- DIN
     }
     
     if((cat_year == year) & (before2_spawn == month)) {
       spawn_2[i] <- DIN
     }
    if((cat_year == year) & (before3_spawn == month)) {
       spawn_3[i] <- DIN
     }
  }
  catch[128] <- mean(spawn_1, spawn_2)
  catch[129] <- mean(spawn_1, spawn_2, spawn_3)
  catch
}



#merge seasonal streamflow ####    
tb_seas_AR <- cleanSF_withSeason(tb_AR, "Mean_dis") #mean discharge in cubic feet/second
tb_seas_AR$riv <- "AR"
tb_seas_HR <- cleanSF_withSeason(tb_HR, "Mean_dis") #mean discharge in cubic feet/second
tb_seas_HR$riv <- "HR"
tb_seas_LMR <- cleanSF_withSeason(tb_LMR, "Mean_dis")     #mean discharge in cubic feet/second
tb_seas_LMR$riv <- "LMR"
tb_seas_All <- rbind(tb_seas_AR, tb_seas_HR, tb_seas_LMR)

#takes a long time
tic()
TB_cat_new <- join_seas_streamflow(TB_cat_env, tb_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works

#test <- TB_cat_env_new[sample(nrow(TB_cat_new),50), ]

#merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(tb_PZ, tb_maxT, tb_minT)

TB_cat_new2 <- join_seasCD(TB_cat_new, seasonal_CD)

#test_set <- TB_cat_new2[,30:107][sample(nrow(TB_cat_new2), 10),]

# merge seasonal rainfall #### 

tb_seas_rf <- clean_seasRF(tb_rf)

TB_cat_env3 <- join_seasRF(TB_cat_new2, tb_seas_rf)

#test_set <- TB_cat_env3[,30:111][sample(nrow(TB_cat_env3), 10),]

# merge seasonal ALL streamflow ####
# rbind the streams together and then run through cleanSF_withSeason
#then run through join_seas_streamALL

colnames(tb_HR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(tb_LMR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(tb_AR) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- rbind(tb_HR, tb_LMR, tb_AR)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
test <- TB_cat_env3[1:10,]

TB_cat_env4 <- join_seas_streamALL(TB_cat_env3, seas_ALLsf)

#merge seasonal salinity and water temp ####
#clean_seas_sal_wt <- function(env, env2, Param_name1, Param_name2, selected_stations, flag){

spawn_SAWT <- clean_seas_sal_wt(tb_sal2,tb_wt2, "Salinity_ppt", "TempW_F", selected_stations, "monthly")
seas_SAWT <- clean_seas_sal_wt(tb_sal2,tb_wt2, "Salinity_ppt", "TempW_F", selected_stations, "seasonal")

#replace NAs in spawn_month
TB_cat_env4 <- TB_cat_env4 %>% replace_na(list(spawn_month=9999))

TB_cat_env5 <- join_spawn_SAWT(TB_cat_env4, spawn_SAWT)
TB_cat_env6 <- join_seas_SAWT(TB_cat_env5, seas_SAWT)

# TO DO merge seasonal nitro ####





















# trim and output ####
write.csv(TB_cat_env6, paste(out, "Seatrout_ENV_Chapter2/TB_all_env_with_lag.csv", sep="/"))

# _____________________ ####
# CHARLOTTE HARBOR ####
# _____________________ ####

#import catch ####
ch = subset(read_sas("ch_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) %>% dplyr::mutate(bUnk=bunk) %>% dplyr::select(-bunk) 
ch_hyd <- subset(read_sas("ch_yoy_cn_hyd.sas7bdat")) 
ch_hyd <- ch_hyd[!duplicated(ch_hyd$Reference),]
#ch_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/chm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ch_phys <- read_sas(paste(phys_dat, "chm_physical.sas7bdat", sep="/")) %>% dplyr::select(Reference, Secchi_on_bottom, Secchi_depth)
ch_phys$Reference <- as.character(ch_phys$Reference)

ch <- left_join(ch, ch_hyd, by="Reference")
ch <- left_join(ch, ch_phys, by="Reference")
ch <- ch %>% dplyr::select(noquote(order(colnames(ch))))  #reorders the columns alphabetically 


# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(ch$Zone)
unique(subset(ch, is.na(Latitude))$Zone) #assume if its missing Long then its also missing Lat

#No missing lats and longs
CH_cat <- ch %>% mutate(NewLong = Longitude, NewLat = Latitude)

#import length data ####
ch_length = subset(read_sas("ch_yoy_cn_l.sas7bdat"))
ch_length = subset(ch_length, bio_reference %in% unique(CH_cat$bio_reference))

ch_length_exp <- ch_length[rep(row.names(ch_length), ch_length$COUNT), 1:3]
ch_length_ag <- aggregate(sl ~ bio_reference, data=ch_length_exp, FUN= "median") 
colnames(ch_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

CH_cat <- left_join(CH_cat, ch_length_ag, by="bio_reference")
CH_cat$approx_age_mo <- round((2.476*CH_cat$median_sl - 0.012*(CH_cat$median_sl)^2)*0.0328767)
CH_cat$spawn_month <- CH_cat$month - CH_cat$approx_age_mo

#filter these lengths that don't make sense
CH_cat <- subset(CH_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values


#tidy catch ####
CH_red <- tidy_catch(CH_cat)

#prop.table(xtabs(~Zone, data=CH_cat))
#unique(CH_red$Zone)

# import environment #### 
#add in air temp
ch_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD5.csv", sep="/"), skip=4)
ch_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD5.csv", sep="/"), skip=4)

#add in nitrogen
ch_nit <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_CH.csv", sep="/"))

#add in phosphorous
ch_ph <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_CH.csv", sep="/"))

# add in Palmer Z
ch_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD5.csv", sep="/" ),skip=3)

#add in rainfall
ch_rf1 <- read.csv(paste(enviro_data, "Rainfall/CH_Rainfall_88_97.csv", sep="/"))
ch_rf2 <- read.csv(paste(enviro_data, "Rainfall/CH_Rainfall_98_07.csv", sep="/"))
ch_rf3 <- read.csv(paste(enviro_data, "Rainfall/CH_Rainfall_08_17.csv", sep="/"))
ch_rf <- rbind(ch_rf1, ch_rf2, ch_rf3)

#add in salinity
ch_sal <- read.csv(paste(enviro_data, "Salinity/CH/Salinity_CharlotteHarbor_MultipleSources.csv", sep="/"))

#add in Seagrass Cover
ch_sg <- read.csv(paste(enviro_data, "SeagrassCover/Seagrass_Cover_CharlotteHarbor.csv", sep="/"))

#add in streamflow
ch_CaloR <- read.csv(paste(enviro_data, "Streamflow/CH/Caloosahatchee_River.csv", sep="/"), skip=28)
ch_MyakR <- read.csv(paste(enviro_data, "Streamflow/CH/Myakka_River.csv", sep="/"), skip=28)
ch_PeaR <- read.csv(paste(enviro_data, "Streamflow/CH/Peace_River.csv", sep="/"), skip=28)
ch_ShellC <- read.csv(paste(enviro_data, "Streamflow/CH/Shell_Creek.csv", sep="/"), skip=28)

#add in water temp
ch_wt <- read.csv(paste(enviro_data, "WaterTemp/CH/WaterTemp_CH.csv", sep="/"))

#clean epc data and build DIN ####
nit <- ch_nit %>% select(-c(Characteristic, Original_Result_Unit, Original_Result_Value, QACode, DEP_WBID, ActivityDepth, Sample_Fraction, Result_Comment, Actual_StationID))
nit$SampleDate <- as.Date(nit$SampleDate , format= "%m/%d/%Y")
nit$SampleDate <- as.character(nit$SampleDate)
nit$StationID <- as.factor(nit$StationID)
nit <- nit[order(nit$SampleDate, nit$StationID, nit$Parameter),]
nit <- nit %>% distinct(StationID, SampleDate, Parameter, .keep_all=TRUE) 

#fabricate the rows and then merge them in to the real dataset
nit2 <- nit %>% distinct(StationID, SampleDate, .keep_all=TRUE) %>% select(-c(Parameter, Result_Value))
library(splitstackshape)
nit2$rowcount <- 4
nit3 <- expandRows(nit2, 11)
nit3$Parameter <- c("NH4_ugl", "NOx_ugl", "TKN_ugl",  "TN_ugl")
nit3$source <- "new3"

#join this fabricated set to the real data
#n_sm <- new3 %>% select(StationID, SampleDate, Result_Value, Parameter)
nit1 <- nit %>% select(StationID, StationName, SampleDate, Parameter, Result_Value)
nit_join <- left_join(nit3, nit1, by=c("StationID", "StationName", "SampleDate", "Parameter")) 

#test <- nit_join %>% group_by(Parameter) %>% mutate(group_row=1:n()) 
nit_spread <- nit_join %>% group_by(Parameter) %>% mutate(group_row=1:n()) %>% spread(Parameter, Result_Value)
#nit_spread$test <- 1:17339
#nit_spread$test2 <- test2$group_row - test2$test

nit_spread$DIN <- rowSums(nit_spread[,c(13,14)], na.rm=TRUE)

#TO DO join nitrogen, phosphorous, salinity, water temp ####

#PROBLEM- there are years in CH_main that are not present in ch_nit so the function is failing
# Need to fix this..... 
#PROBLEM- additionally, there are multiple readings in the ch_nit for a station, year and month (i.e. a station took two measurements within the same month and year)
# which will cause the reference value to be duplicated so that it records both nitrogen measurements.
# Not sure the best way to deal with this so will just produce it and then select unique reference values afterward.

#DIN
tic()
nit_full <- joinNit(CH_red, nit_spread, 0.017, 0.017, nitrogen)
toc()
#nit_full <- subset(nit_full, !duplicated(V3)) #V3=reference
write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_017_DIN.csv", sep="/"))

tic()
nit_full <- joinNit(CH_red, nit_spread, 0.0288, 0.0288, nitrogen)
toc()
#nit_full <- subset(nit_full, !duplicated(V3))
write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_028_DIN.csv", sep="/"))

tic()
nit_full <- joinNit(CH_red, nit_spread, 0.0432, 0.0432, nitrogen)
toc()
#nit_full <- subset(nit_full, !duplicated(V3))
write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_043_DIN.csv", sep="/"))
# 
# #phos
# tic()
# pho_full <- joinEV(CH_main, ch_ph, 0.017, 0.017, phosphorous, "TP_ugl")
# toc()
# pho_full <- subset(pho_full, !duplicated(V3)) #V3=reference
# write.csv(pho_full, paste(out, "Seatrout_ENV_Chapter2/CH_ph_join_017.csv", sep="/"))
# 
# tic()
# pho_full <- joinEV(CH_main, ch_ph, 0.0288, 0.0288, phosphorous, "TP_ugl")
# toc()
# pho_full <- subset(pho_full, !duplicated(V3))
# write.csv(pho_full, paste(out, "Seatrout_ENV_Chapter2/CH_ph_join_028.csv", sep="/"))
# 
# tic()
# pho_full <- joinEV(CH_main,ch_ph, 0.0432, 0.0432, phosphorous, "TP_ugl" ) 
# toc()
# pho_full <- subset(pho_full, !duplicated(V3))
# write.csv(pho_full, paste(out, "Seatrout_ENV_Chapter2/CH_ph_join_043.csv", sep="/")) 
# 
# #salinity
# tic()
# sal_full <- joinEV(CH_main, ch_sal, 0.017, 0.017, salinity, "Salinity_ppt")
# toc()
# sal_full <- subset(sal_full, !duplicated(V3)) #V3=reference
# write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/CH_sal_join_017.csv", sep="/"))
# 
# tic()
# sal_full <- joinEV(CH_main, ch_sal, 0.0288, 0.0288, salinity, "Salinity_ppt")
# toc()
# sal_full <- subset(sal_full, !duplicated(V3))
# write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/CH_sal_join_028.csv", sep="/"))
# 
# tic()
# sal_full <- joinEV(CH_main,ch_sal, 0.0432, 0.0432, salinity, "Salinity_ppt" ) 
# toc()
# sal_full <- subset(sal_full, !duplicated(V3))
# write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/CH_sal_join_043.csv", sep="/")) 
# 
# #watertemp
# tic()
# wt_full <- joinEV(CH_main, ch_wt, 0.017, 0.017, watertemp, "TempW_F")
# toc()
# wt_full <- subset(wt_full, !duplicated(V3)) #V3=reference
# write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/CH_wt_join_017.csv", sep="/"))
# 
# tic()
# wt_full <- joinEV(CH_main, ch_wt, 0.0288, 0.0288,  watertemp, "TempW_F")
# toc()
# wt_full <- subset(wt_full, !duplicated(V3))
# write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/CH_wt_join_028.csv", sep="/"))
# 
# tic()
# wt_full <- joinEV(CH_main,ch_wt, 0.0432, 0.0432,  watertemp, "TempW_F" ) 
# toc()
# wt_full <- subset(wt_full, !duplicated(V3))
# write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/CH_wt_join_043.csv", sep="/")) 

#merge closest river mouth ####
CR_mouth = c(-82.022044, 26.519627) #, "CR") #caloosahatchee
MR_mouth = c(-82.203318, 26.957466) #, "MR") #myakka
PR_mouth = c(-82.044703, 26.953794) #, "PR") #peace river 
# SC_mouth = c(-82.486, 27.716) # shell creek, much lesser 

riv_name= c("CR", "MR", "PR")

rivers = data.frame(rbind(CR_mouth, MR_mouth, PR_mouth))
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)

CH_cat$closest_riv <- ""
CH_cat <- closestRiver(CH_cat, rivers)


#merge riverflow ####

ch_av_CR <- cleanSF(ch_CaloR, "CR") #mean discharge in cubic feet/second
ch_av_MR <- cleanSF(ch_MyakR, "MR") #mean discharge in cubic feet/second
ch_av_PR <- cleanSF(ch_PeaR, "PR")     #mean discharge in cubic feet/second

streamfl <- rbind(ch_av_CR, ch_av_MR, ch_av_PR)
streamfl <- subset(streamfl, month >= min(unique(CH_cat$month)) & year>= min(unique(CH_cat$year)))


CH_cat$river_flow <- 1
tic()
for (i in 1:nrow(CH_cat)){
  cat_month = CH_cat[i,30]
  cat_year = CH_cat[i,61] 
  cat_riv = CH_cat[i,69] #69
  
  
  for (j in 1:nrow(streamfl)){
    riv_year = streamfl[j,1]
    riv_month = streamfl[j,2]
    riv_dis = streamfl[j,3]
    riv_name = streamfl[j,4]
    
    if((cat_riv==riv_name) & (cat_year == riv_year) & (cat_month == riv_month)){
      CH_cat[i,70] <- riv_dis
      
    } 
    
  }
  
}
toc()

#TO DO- merge nitrogen, phos, salinity, water temp ####

#DIN
CH_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(CH_nit) <- c("Reference", "Nit_val")

CH_phos <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_ph_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(CH_phos) <- c("Reference", "Phos_val")

CH_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(CH_sal) <- c("Reference", "Sal_val")

CH_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(CH_wat) <- c("Reference", "WatTemp_val")

#join back to original catch dataset- not the reduced one. 
CH_new <-  left_join(CH_cat, CH_nit, by="Reference")
CH_new2 <-  left_join(CH_new, CH_phos, by="Reference")
CH_new3 <-  left_join(CH_new2, CH_sal, by="Reference")
CH_new4 <-  left_join(CH_new3, CH_wat, by="Reference")

# merge airtemp and palmerZ (climate zones-CD) ####
CH_new5 <- joinCD(CH_new4, ch_PZ,ch_maxT,ch_minT)

#merge rainfall ####
#Charlotte harbor Date rainfall datasheet is set up differently than all of the rest so I can't do this using the function-must do this manually 

cleanRF_CH <- function(rf, name) {
  rf <- rf %>% dplyr::mutate(Date = as.Date(DATE, format= "%Y-%m-%d"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  dplyr::select(year, month, STATION_NAME, HOURLYPrecip)
  rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
  tot_rf <- aggregate(HOURLYPrecip ~ year + month, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
  tot_rf$month <- as.numeric(tot_rf$month)
  tot_rf$year <- as.numeric(tot_rf$year)
  #tb_tot_rf$month <- as.numeric(tb_tot_rf$month)
  colnames(tot_rf) <- c("year", "month", name)
  tot_rf
}


ch_tot_rf <- cleanRF_CH(ch_rf, "MeanMonthlyRF")
CH_new6 <- left_join(CH_new5, ch_tot_rf, by=c("year", "month"))


#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
CH_new6 <- CH_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(CH_new6, paste(out, "Seatrout_ENV_Chapter2/CH_all_env_no_lag.csv", sep="/"))

#.####
# Lag Variable Calculations ####
#.####

CH_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_all_env_no_lag.csv",sep="/"), header=T)
CH_cat_env <- CH_cat_env[,-1]
CH_cat_env$closest_riv <- as.character(CH_cat_env$closest_riv)


# TO DO clean create seasonal nitro/phos ####

#merge seasonal streamflow ####

ch_seas_CR <- cleanSF_withSeason(ch_CaloR, "Mean_dis") #mean discharge in cubic feet/second
ch_seas_CR$riv <- "CR"
ch_seas_MR <- cleanSF_withSeason(ch_MyakR, "Mean_dis") #mean discharge in cubic feet/second
ch_seas_MR$riv <- "MR"
ch_seas_PR <- cleanSF_withSeason(ch_PeaR, "Mean_dis")     #mean discharge in cubic feet/second
ch_seas_PR$riv <- "PR"

ch_seas_All <- rbind(ch_seas_CR, ch_seas_MR, ch_seas_PR)

#takes a long time
tic()
CH_cat_new <- join_seas_streamflow(CH_cat_env, ch_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works


#merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(ch_PZ, ch_maxT, ch_minT)

CH_cat_new2 <- join_seasCD(CH_cat_new, seasonal_CD)


#merge seasonal rainfall #### 
#by hand because column names are different for some reason

clean_seasRF_CH <- function(rf) {
  rf <- rf
  rf <- rf %>% mutate(Date = as.Date(DATE, format= "%Y-%m-%d"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  select(year, month, HOURLYPrecip)
  rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
  rf$season <- ifelse(rf$month %in% c("03","04","05"), "spring", ifelse(rf$month %in% c("06","07","08","09"), "summer", ifelse(rf$month %in% c("10","11", "12"), "autumn", ifelse(rf$month %in% c("01","02"), "winter", "NA"))))
  tot_rf <- aggregate(HOURLYPrecip ~ year + season, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
  tot_rf$year <- as.numeric(tot_rf$year)
  colnames(tot_rf) <- c("year", "season", "total_rf")
  tot_rf
}



ch_seas_rf <- clean_seasRF_CH(ch_rf)

CH_cat_env3 <- join_seasRF(CH_cat_new2, ch_seas_rf)


#merge seasonal ALL streamflow ####

colnames(ch_CaloR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ch_MyakR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ch_PeaR) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- rbind(ch_CaloR, ch_MyakR, ch_PeaR)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

CH_cat_env4 <- join_seas_streamALL(CH_cat_env3, seas_ALLsf)

#TODO- merge seasonal salinity and water temp ####
selected_stations <- c("141", "177", "179", "CHV011", "159" )
#which 

spawn_SAWT <- clean_seas_sal_wt(ch_sal,ch_wt, "Salinity_ppt", "TempW_F", selected_stations, "monthly")
seas_SAWT <- clean_seas_sal_wt(ch_sal,ch_wt, "Salinity_ppt", "TempW_F", selected_stations, "seasonal")

#replace NAs in spawn_month
CH_cat_env4 <- CH_cat_env4 %>% replace_na(list(spawn_month=9999))

CH_cat_env5 <- join_spawn_SAWT(CH_cat_env4, spawn_SAWT)
CH_cat_env6 <- join_seas_SAWT(CH_cat_env5, seas_SAWT)

# TO DO - merge seasonal nitro, phos ####
# trim and output ####
write.csv(CH_cat_env6, paste(out, "Seatrout_ENV_Chapter2/CH_all_env_with_lag.csv", sep="/"))




# ________________####
# BUILD OUT - AP####
# _______________ ####

#import catch ####
ap = subset(read_sas("ap_yoy_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) %>% mutate(bUnk=bunk) %>% select(-bunk) 
#ap_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/apm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ap_phys <- read_sas(paste(phys_dat, "apm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ap_phys$Reference <- as.character(ap_phys$Reference)
#There are duplicated References in ap_hyd
ap_hyd <- subset(read_sas("ap_yoy_cn_hyd.sas7bdat")) 
#There are duplicated References in ap_hyd
ap_hyd <- ap_hyd[!duplicated(ap_hyd$Reference),]
ap <- left_join(ap, ap_phys, by="Reference") %>% left_join(ap_hyd, by="Reference")
ap <- ap %>% select(noquote(order(colnames(ap))))  #reorders the columns alphabetically 


# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(ap$Zone)
unique(subset(ap, is.na(Latitude))$Zone) #assume if its missing Long then its also missing Lat

#No missing lats and longs
AP_cat <- ap %>% mutate(NewLong = Longitude, NewLat = Latitude)

#import length data ####
ap_length = subset(read_sas("ap_yoy_cn_l.sas7bdat"))
ap_length = subset(ap_length, bio_reference %in% unique(AP_cat$bio_reference))

ap_length_exp <- ap_length[rep(row.names(ap_length), ap_length$COUNT), 1:3]
ap_length_ag <- aggregate(sl ~ bio_reference, data=ap_length_exp, FUN= "median") 
colnames(ap_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

AP_cat <- left_join(AP_cat, ap_length_ag, by="bio_reference")
AP_cat$approx_age_mo <- round((2.476*AP_cat$median_sl - 0.012*(AP_cat$median_sl)^2)*0.0328767)
AP_cat$spawn_month <- AP_cat$month - AP_cat$approx_age_mo

#filter these lengths that don't make sense
AP_cat <- AP_cat[AP_cat$spawn_month > 2,]


#tidy catch ####
AP_red <- tidy_catch(AP_cat)


# import environment #### 
#add in air temp
ap_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD1.csv", sep="/"), skip=4)
ap_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD1.csv", sep="/"), skip=4)

#add in nitrogen
ap_nit <- read.csv(paste(enviro_data, "Nutrients/AP/3522.csv", sep="/")) %>% dplyr::select(StationCode, DateTimeStamp, NH4F,F_NH4F, NO2F, F_NO2F, NO3F, F_NO3F)

#add in phosphorous
ap_ph <- read.csv(paste(enviro_data, "Nutrients/AP/3522.csv", sep="/")) %>% dplyr::select(StationCode, DateTimeStamp, PO4F,F_PO4F)

# add in Palmer Z
ap_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD1.csv", sep="/" ),skip=3)

#add in rainfall
ap_rf1 <- read.csv(paste(enviro_data, "Rainfall/AP_Rainfall_88_97.csv", sep="/"))
ap_rf2 <- read.csv(paste(enviro_data, "Rainfall/AP_Rainfall_98_07.csv", sep="/"))
ap_rf3 <- read.csv(paste(enviro_data, "Rainfall/AP_Rainfall_08_17.csv", sep="/"))
ap_rf <- rbind(ap_rf1, ap_rf2, ap_rf3)

#add in salinity
ap_sal <- read.csv(paste(enviro_data, "Salinity/AP/363383.csv", sep="/"))

# Cat Point			CP				apacpwq   (29.7021, -84.8802)
# Dry Bar			DB				apadbwq  (29.6747 degrees latitude and -85.0583667 degrees longitude)
# East Bay Bottom		EB				apaebwq 29.7858	84.8752
# East Bay Surface		ES				apaeswq 29.7858	84.8752
# Pilots Cove			PC				apapcwq  29.601	85.0277
# Little St. Marks		LM				apalmwq 29.755689	85.003525

#add in Seagrass Cover
#ch_sg <- read.csv(paste(enviro_data, "SeagrassCover/Seagrass_Cover_CharlotteHarbor.csv", sep="/"))

#add in streamflow
ap_AR <- read.csv(paste(enviro_data, "Streamflow/AP/Apalachicola_River_NR_Sumatra_FL.csv", sep="/"), skip=28)

#add in water temp
ap_wt <- read.csv(paste(enviro_data, "WaterTemp/AP/765372.csv", sep="/"))

# Cat Point			CP				apacpwq   (29.7021, -84.8802)
# Dry Bar			DB				apadbwq  (29.6747 degrees latitude and -85.0583667 degrees longitude)
# East Bay Bottom		EB				apaebwq 29.7858	84.8752
# East Bay Surface		ES				apaeswq 29.7858	84.8752
# Pilots Cove			PC				apapcwq  29.601	85.0277
# Little St. Marks		LM				apalmwq 29.755689	85.003525


#TO-DO join nitrogen, phosphorous, salinity, water temp ####
# requires by hand

#convert mg/L to ug/L
# add ammonia, nitrite, nitrate to get total DIN

ap_nit <- ap_nit %>% mutate(total_DIN = rowSums(ap_nit[,c(3,5,7)], na.rm=TRUE)*1000)











ap_nit <- ap_nit %>% mutate(DINug = DIN*1000) %>% select(-DIN) %>%rename(DIN=DINug)


ap_nit$DateTimeStamp <- as.factor(ap_nit$DateTimeStamp)
ap_nit <- ap_nit %>% mutate(Date = as.Date(DateTimeStamp, format = " %m/%d/%Y"))
ap_nit$Date <- as.character(ap_nit$Date)

env <- droplevels( ap_nit %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Parameter == Param_name) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID))
env$Month <- as.numeric(env$Month)



joinEV <- function(catch, env, fuzzy_lat, fuzzy_long, var_name, Param_name) {
  
  #do some selection/cleaning on the enviro data based on the catch data to thin the enviro set out
  env$SampleDate <- as.factor(env$SampleDate)
  env <- env %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
  env$Date <- as.character(env$Date)
  
  env <- droplevels( env %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Parameter == Param_name) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID))
  env$Month <- as.numeric(env$Month)
  
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  main_Year <- unique(catch$year)
  env = subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year)
  
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  
  # Start For Loop
  for(i in 1:nrow(catch))
  {
    # re-initialize this data structs for new pass
    #match_matrix <- as.list(seq_len(140))
    match_matrix <- as.data.frame(matrix(data=NA, nrow=100, ncol=7))
    hit_counter = 1 #initialize the hit counter that will be used in the below loop instead of indexing based off of j
    
    #define some variables
    filler_dist = 9999
    catch_year=catch[i,61]
    catch_month=catch[i,30]
    catch_lat=catch[i,65]
    catch_long=catch[i, 64]
    catch_ref = catch[i,41]
    TB_cor = as.numeric(c(catch[i,64], catch[i,65]))
    
    for(j in 1:nrow(env))
    {
      #define some more variables
      nit_year=env[j,7]
      nit_month=env[j,8]
      nit_lat =env[j,1]
      nit_long = env[j,2]
      nit_val = env[j,6]
      
      if (is.na(catch_year) | is.na(catch_month) | is.na(catch_lat) | is.na(catch_long))
      {
        print("NA Found")
      }
      else if((catch_year==nit_year)&(catch_month==nit_month))
      {
        #defining box to place around each coordinate pair assocaited with catch
        # this reduces the amount of coordinate points that are selected from within env that are then used in a pairwise distance comparison
        
        fuzzymax_lat = catch_lat +fuzzy_lat #catch_sub$NewLat
        fuzzymin_lat = catch_lat -fuzzy_lat
        fuzzymax_long = catch_long + fuzzy_long #TBcatch$NewLong
        fuzzymin_long = catch_long - fuzzy_long
        
        if((nit_lat > fuzzymin_lat) & (nit_lat < fuzzymax_lat) & (nit_long > fuzzymin_long) & (nit_long < fuzzymax_long)) 
          #if the lat long of nitrogen falls within the fuzzy lat long boundaries of the catch lat long found above then it will add into the dataframe below 
        {
          
          #m= as.data.frame(matrix(data=NA,nrow=1, ncol=7))
          m= as.list(seq_len(7))
          #m = data.frame(catch_month, catch_year, catch_ref, nit_val, nit_long, nit_lat, filler_dist)
          m[1] = catch_month
          m[2]= catch_year
          m[3]= catch_ref     
          m[4] = nit_val
          m[5] = nit_long
          m[6] = nit_lat
          m[7]= filler_dist #placeholder for distance 
          m_df <- data.frame(m, stringsAsFactors = FALSE)
          #colnames(m_df) <- c("x1", "x2","X3", "X4", "X5", "X6", "X7")
          
          match_matrix[hit_counter,] <- m_df
          hit_counter=hit_counter + 1
          
        }
        
      }
      
    }
    match_matrix[,7] = distm(match_matrix[,5:6], TB_cor) 
    match_matrix <- na.omit(match_matrix)
    
    nit_station_match = as.list(seq_len(1))
    nit_station_match <- match_matrix[match_matrix$V7 == min(match_matrix[,7], na.rm=T),] #select the nitrogen val from station that is closest of all 
    
    selected[[i]] <- nit_station_match #nitrogen station match adds in to predefined seleciton
    var_name = do.call(rbind, selected) #do.call bind
    
  }
  var_name
  
} 






#TO DO - merge closest river mouth ####
AR_mouth = c(-84.9806, 29.729) #, "AR") #alafia

riv_name= c("AR")

rivers = data.frame(AR_mouth)
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)

AP_cat$closest_riv <- ""
AP_cat <- closestRiver(AP_cat, rivers)


#TO-DO merge riverflow ####

ap_av_AR <- cleanSF(ap_AR, "AR") #mean discharge in cubic feet/second

streamfl <- ap_av_AR
streamfl <- subset(streamfl, month >= min(unique(AP_cat$month)) & year>= min(unique(AP_cat$year)))

AP_cat$river_flow <- 1
tic()
for (i in 1:nrow(AP_cat)){
  cat_month = AP_cat[i,30]
  cat_year = AP_cat[i,61] 
  cat_riv = AP_cat[i,66] #66
  
  
  for (j in 1:nrow(streamfl)){
    riv_year = streamfl[j,1]
    riv_month = streamfl[j,2]
    riv_dis = streamfl[j,3]
    riv_name = streamfl[j,4]
    
    if((cat_riv==riv_name) & (cat_year == riv_year) & (cat_month == riv_month)){
      AP_cat[i,67] <- riv_dis
      
    } 
    
  }
  
}
toc()


#TO DO - merge nitrogen, phos, salinity, water temp ####

#TO DO- merge airtemp and palmerZ (climate zones-CD) ####
CH_new5 <- joinCD(CH_new4, ch_PZ,ch_maxT,ch_minT)

# TO-DO merge rainfall ####
#Charlotte harbor rainfall datasheet is set up differently than all of the rest so I can't do this using the function-must do this manually 

ch_rf <- ch_rf %>% mutate(year=substr(DATE, 3,4), month = substr(DATE, 6,7))%>%  select(year, month, PRCP) %>% rename(TotMonthlyRF = PRCP)
ch_rf$year <- as.numeric(ch_rf$year)
ch_rf$month <- as.numeric(ch_rf$month)

CH_new6 <- left_join(CH_new5, ch_rf, by=c("year", "month"))

# TO DO - produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
CH_new6 <- CH_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

# TO DO- output ####
write.csv(CH_new6, paste(out, "Seatrout_ENV_Chapter2/CH_all_env_no_lag.csv", sep="/"))

#.####
#Lag Variable Calculations ####
#.####






# BUILD OUT - IR ####
ir = subset(read_sas("ir_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11)) 
#ir_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/irm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ir_phys <- read_sas(paste(phys_dat, "irm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ir_phys$Reference <- as.character(ir_phys$Reference)
ir_hyd <- subset(read_sas("ir_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in ir_hyd
ir_hyd <- ir_hyd[!duplicated(ir_hyd$Reference),]
ir <- left_join(ir, ir_phys, by="Reference") %>% left_join(ir_hyd, by="Reference")
ir <- ir %>% select(noquote(order(colnames(ir))))  #reorders the columns alphabetically 

# BUILD OUT - JX ####
jx = subset(read_sas("jx_yoy_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11)) 
#jx_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/jxm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
jx_phys <- read_sas(paste(phys_dat, "jxm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
jx_phys$Reference <- as.character(jx_phys$Reference)
jx_hyd <- subset(read_sas("jx_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in jx_hyd
jx_hyd <- jx_hyd[!duplicated(jx_hyd$Reference),]
jx <- left_join(jx, jx_phys, by="Reference") %>% left_join(jx_hyd, by="Reference")
jx <- jx %>% select(noquote(order(colnames(jx))))  #reorders the columns alphabetically 




# BUILD OUT -  CK ####
ck = subset(read_sas("ck_yoy_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11))
#ck_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/ckm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ck_phys <- read_sas(paste(phys_dat, "ckm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ck_phys$Reference <- as.character(ck_phys$Reference)
ck_hyd <- subset(read_sas("ck_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in ck_hyd
ck_hyd <- ck_hyd[!duplicated(ck_hyd$Reference),]
ck <- left_join(ck, ck_phys, by="Reference") %>% left_join(ck_hyd, by="Reference")
ck <- ck %>% select(noquote(order(colnames(ck))))  #reorders the columns alphabetically 








# AGGREGATE CATEGORICAL HABITAT VARIABLES ########

#This section is nearly identical to that in Delta Method script
#Based on FWRI code the three variables were bottom type (bStr, bsan, bmud), bottom vegetation (bveg), and shoreline (Shore)
#There are three different bottom type variables each of them coded in a binary form.
#I want to take bStr, bsan, and bmud and put them into 1 variable so I will make a new variable entirely = 'bottom'
#I also want to turn bveg into a new variable = 'veg' based on the entries. If alg or Sav then turn to SAV because there are only 9 entries for Alg. 
#Same thing for the shore variable = 'shore'. Decided to have only emergent, structure, terrestrial, and mangrove. 
#Removed old variables (bStr, bSan, bMud, bveg, Shore)
#Removed rows when there was no shoreline variable. 

full <-  full %>% 
  mutate(bottom = ifelse(full$bStr ==1, "structure", ifelse(full$bSan>0 | full$bMud>0, "mudsand", "unknown")), 
         veg= ifelse(full$bveg == "SAVAlg", "SAV", ifelse(full$bveg == "Alg", "SAV", ifelse(full$bveg =="SAV", "SAV", "Noveg"))),
         shore = ifelse(substr(full$Shore,1,3)=="Eme", "Emerge", ifelse(substr(full$Shore,1,3) =="Man", "Mangrove", 
                                                                        ifelse(substr(full$Shore,1,3)=="Str", "Structure", ifelse(substr(full$Shore, 1,3)=="Ter", "Terrestrial", "Non")))))    %>%
  select(-c(bStr, bSan, bMud, bveg, Shore)) %>% subset(!shore=="Non") %>% 
  mutate(avgDepth = mean(c(StartDepth, Enddepth)))

#Turn habitat variables into factors so they can be treated as categorical
full[,c(2,5:9)] <- lapply(full[,c(2,5:9)], factor)

#select from full the estuaries so that the categorical varaibles can be sorted to make sure that there are enough observations per level of each
ap.fl <- droplevels(full %>% subset(bay =='AP'))
ck.fl <- droplevels(full %>% subset(bay =='CK'))
tb.fl <- droplevels(full %>% subset(bay =='TB'))
ch.fl <- droplevels(full %>% subset(bay =='CH'))
jx.fl <- droplevels(full %>% subset(bay =='JX'))
ir.fl <- droplevels(full %>% subset(bay =='IR'))

with(ap.fl,tapply(number, list(year,month),sum))
with(ap.fl,tapply(number, list(year,veg),sum))
with(ap.fl,tapply(number, list(year,bottom),sum))
with(ap.fl,tapply(number, list(year,shore),sum))
ap.fl <- subset(ap.fl, shore != "Mangrove") %>% droplevels(ap.fl$shore)
ap.fl$month <- as.factor(as.character(ap.fl$month))

with(ck.fl,tapply(number, list(year,month),sum))
with(ck.fl,tapply(number, list(year,veg),sum))
with(ck.fl,tapply(number, list(year,bottom),sum))
ck.fl = droplevels(subset(ck.fl, bottom != "unknown"))
with(ck.fl,tapply(number, list(year,shore),sum))
#drop terrestrial and join structure 
ck.fl <- droplevels(subset(ck.fl, shore != "Terrestrial"))
ck.fl$shore[ck.fl$shore== "Mangrove"] = "Structure"
ck.fl <- droplevels(subset(ck.fl, shore != "Mangrove"))
ck.fl$month <- as.factor(as.character(ck.fl$month))

with(tb.fl,tapply(number, list(year,month),sum))
with(tb.fl,tapply(number, list(year,veg),sum))
with(tb.fl,tapply(number, list(year,bottom),sum))
tb.fl <- droplevels(subset(tb.fl, bottom != "unknown"))
with(tb.fl,tapply(number, list(year,shore),sum))
tb.fl$month <- as.factor(as.character(tb.fl$month))

with(ch.fl,tapply(number, list(year,month),sum))
with(ch.fl,tapply(number, list(year,veg),sum))
with(ch.fl,tapply(number, list(year,bottom),sum))
with(ch.fl,tapply(number, list(year,shore),sum))
ch.fl$month <- as.factor(as.character(ch.fl$month))

with(jx.fl,tapply(number, list(year,month),sum))
with(jx.fl,tapply(number, list(year,veg),sum))
with(jx.fl,tapply(number, list(year,bottom),sum))
jx.fl <- droplevels(subset(jx.fl, bottom != "unknown"))
with(jx.fl,tapply(number, list(year,shore),sum))
jx.fl <- na.omit(jx.fl)
jx.fl$month <- as.factor(as.character(jx.fl$month))

with(ir.fl,tapply(number, list(year,month),sum))
with(ir.fl,tapply(number, list(year,veg),sum))
with(ir.fl,tapply(number, list(year,bottom),sum))
with(ir.fl,tapply(number, list(year,shore),sum))
ir.fl <- na.omit(ir.fl)
ir.fl$month <- as.factor(as.character(ir.fl$month))







