# ABOUT ####
# R script for getting all of the FIM habitat and environmental variables together.
# Some of this script borrows from Delta_Method_For_Producing_Nominal
# Environmental variables collected by FIM include: Dissolved O2, Salinity, Temp, pH, secchi depth

# SET WORKING DIRECTORY ####
#must change working directory for data when working on personal vs work computer
rm(list=ls())
personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
work_comp= "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
nutrient_dat = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Nutrients/Nitrogen"
nutrient_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Nutrients"
salinity = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Salinity"
watertemp = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/WaterTemp"
rainfall = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Rainfall"
streamflow = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Streamflow"
airtemp = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/AirTemp"
palmerz = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/PalmerZ"
seagrass = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/SeagrassCover"
out =   "U:/PhD_projectfiles/Exported_R_Datafiles"

#setwd(personal_comp)
setwd(work_comp)

# LOAD PACKAGES ####
library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
library(tictoc)

# ABOUT- IMPORT DATA SETS ####
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

# AP####
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

# CK ####
ck = subset(read_sas("ck_yoy_cn_c.sas7bdat"),  month %in% c(5,6,7,8,9,10,11))
#ck_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/ckm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ck_phys <- read_sas(paste(phys_dat, "ckm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ck_phys$Reference <- as.character(ck_phys$Reference)
ck_hyd <- subset(read_sas("ck_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in ck_hyd
ck_hyd <- ck_hyd[!duplicated(ck_hyd$Reference),]
ck <- left_join(ck, ck_phys, by="Reference") %>% left_join(ck_hyd, by="Reference")
ck <- ck %>% select(noquote(order(colnames(ck))))  #reorders the columns alphabetically 

#TB ####

# import catch ####

tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) 
#tb_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/tbm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
tb_phys <- read_sas(paste(phys_dat, "tbm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
tb_phys$Reference <- as.character(tb_phys$Reference)
tb_hyd <- subset(read_sas("tb_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in tb_hyd
tb_hyd <- tb_hyd[!duplicated(tb_hyd$Reference),]
tb <- left_join(tb, tb_phys, by="Reference") %>% left_join(tb_hyd, by="Reference")
tb <- tb %>% select(noquote(order(colnames(tb))))  #reorders the columns alphabetically 


# clean catch ####
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

#Apply zone-specific medoids to missing Long and Lat. Do this with new varaibles NewLong and NewLat. 
#Format year to match the year in tb_nit. 
#Then trim TB_main to select the most important zones that occur at least 10% of the time. Also, select only unique
# Reference values for the dataset moving through the function to minimize computation. 

TB_cat <- tb %>% mutate(NewLong = ifelse(Zone == "A" & is.na(Longitude), tb_medA[1,],  ifelse(Zone=="C" & is.na(Longitude), tb_medC[1,], ifelse(Zone == "D" & is.na(Longitude), tb_medD[1,], ifelse(Zone=="E" & is.na(Longitude), tb_medE[1,], ifelse(Zone == "M" & is.na(Longitude), tb_medM[1,], tb$Longitude))))), NewLat = ifelse(Zone == "A" & is.na(Latitude), tb_medA[2,],  ifelse(Zone=="C" & is.na(Latitude), tb_medC[2,], ifelse(Zone == "D" & is.na(Latitude), tb_medD[2,], ifelse(Zone=="E" & is.na(Latitude), tb_medE[2,], ifelse(Zone == "M" & is.na(Latitude), tb_medM[2,], tb$Latitude))))))
TB_cat <- TB_cat %>% mutate(year = substr(as.character(TB_cat$year),3,4)) #make year format match that of the coming nitrogen data

TB_cat$year <- as.factor(TB_cat$year)
TB_cat$month <- as.numeric(TB_cat$month)
TB_cat <- data.frame(TB_cat)
Zone.prop <- as.data.frame(prop.table(xtabs(~Zone, data=TB_cat)))
zone1 <- droplevels(Zone.prop[Zone.prop$Freq > 0.1,])  
sel_zone=unique(zone1$Zone)

TB_main <- subset(TB_cat,!duplicated(Reference) & Zone %in% sel_zone)

# import environment #### 
#add in air temp
tb_maxT <- read.csv(paste(airtemp, "Max_Temp_CD3.csv", sep="/"), skip=4)
tb_minT <- read.csv(paste(airtemp, "Min_Temp_CD3.csv", sep="/"),skip=4)

#add in nitrogen
tb_nit1 <- read.csv(paste(nutrient_dat, "Nitrogen_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_nit2 <- read.csv(paste(nutrient_dat, "Nitrogen_Middle_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit3 <- read.csv(paste(nutrient_dat, "Nitrogen_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit <- rbind(tb_nit1, tb_nit2, tb_nit3)

#add in phosphorous
tb_ph1 <- read.csv(paste(nutrient_dat, "Phosphorous_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_ph2 <- read.csv(paste(nutrient_dat, "Phosphorous_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph3 <- read.csv(paste(nutrient_dat, "Phosphorous_Middle_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph4 <- read.csv(paste(nutrient_dat, "Phosphorous_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph <- rbind(tb_ph1, tb_ph2, tb_ph3, tb_ph4)

# add in Palmer Z
tb_PZ <- read.csv(paste(palmerz,"PalmerZ_CD3.csv", sep="/" ), skip=3)

#add in rainfall
tb_rf1 <- read.csv(paste(rainfall, "TB_Rainfall_89_97.csv", sep="/"))
tb_rf2 <- read.csv(paste(rainfall, "TB_Rainfall_98_07.csv", sep="/"))
tb_rf3 <- read.csv(paste(rainfall, "TB_Rainfall_08_17.csv", sep="/"))
tb_rf < - rbind(tb_rf1, tb_rf2, tb_rf3)

#add in salinity
tb_sal1 <- read.csv(paste(salinity, "Salinity_HillsboroughBay_EPCRoutine.csv", sep="/"))
tb_sal2 <- read.csv(paste(salinity, "Salinity_LowerTampaBay_EPCRoutine.csv", sep="/"))
tb_sal3 <- read.csv(paste(salinity, "Salinity_MiddleTampaBay_EPCRoutine.csv", sep="/"))
tb_sal4 <- read.csv(paste(salinity, "Salinity_OldTampaBay_EPCRoutine.csv", sep="/"))
tb_sal <- rbind(tb_sal1, tb_sal2, tb_sal3, tb_sal4)

#add in seagrass cover
tb_sg <- read.csv(paste(seagrass, "SWFWMD_Seagrass_TB.csv", sep="/"))

#add in streamflow
tb_AlafR <- read.csv(paste(streamflow, "TB/Alafia_River.csv", sep="/"), skip=28)
tb_HillR <- read.csv(paste(streamflow, "TB/Hillsborough_river.csv", sep="/"), skip=28)
tb_ManR <- read.csv(paste(streamflow, "TB/Little.Manatee_River.csv", sep="/"), skip=28)

#add in water temp
tb_wt1 <- read.csv(paste(watertemp, "Water_temperature_Hillsborough_Bay.csv", sep="/"))
tb_wt2 <- read.csv(paste(watertemp, "Water_temperature_Lower_Tampa_Bay.csv", sep="/"))
tb_wt3 <- read.csv(paste(watertemp, "Water_temperature_Middle_Tampa_Bay.csv", sep="/"))
tb_wt <- rbind(tb_wt1, tb_wt2, tb_wt3)


# build join function ####

#provide the catch dataset, the environmental data set, the fuzzy lat/long, variable name, and parameter name (as a character)
#function works for TB and CH for enviro variables nitrogen, phosphorous and salinity

#Param Name for Nitrogen "TN_ugl"
#Param Name for Phosph   "TP_ugl"
#Param Name for Salinity "Salinity_ppt"
#Param Name for Water temp  "TempW_F"

# Define fuzzy lat/long boundaries 
# fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile
# fuzzy_long = 0.01 # 1 mile 


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
    catch_year=catch[i,59]
    catch_month=catch[i,30]
    catch_lat=catch[i,63]
    catch_long=catch[i, 62]
    catch_ref = catch[i,41]
    TB_cor = as.numeric(c(catch[i,62], catch[i,63]))
    
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

# join nitrogen, phosphorous, salinity, water temp ####
# tic()
# nit_full <- joinEV(TB_main, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl")
# toc()
# write.csv(nit_full, paste(out, "TB_nit_join.csv", sep="/"))
# 
# tic()
# ph_full <- joinEV(TB_main, tb_ph, 0.017, 0.017, phosphorous, "TP_ugl")
# toc()
# write.csv(ph_full, paste(out, "TB_ph_join.csv", sep="/"))
# 
# tic()
# sal_full <- joinEV(TB_main, tb_sal, 0.017, 0.017, salinity, "Salinity_ppt")
# toc()
# write.csv(sal_full, paste(out, "TB_sal_join.csv", sep="/"))
# 
# tic()
# wt_full <- joinEV(TB_main, tb_wt, 0.017, 0.017, watertemp, "TempW_F")
# toc()
# write.csv(wt_full, paste(out, "TB_wt_join.csv", sep="/"))

# build join function for airtemp and palmerZ ####

joinCD

#turn Date in to year and month
#adjust name of Value column and Anomaly column to specifically list the variable name
# i.e  Value = MaxT_value, Anomaly = MaxT_Anom
#Anomaly (departure from mean)
#Where year == year & month == month then append the value of Value and anomaly
# on to the main catch dataset


# CH ####

#import catch ####
ch = subset(read_sas("ch_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) %>% mutate(bUnk=bunk) %>% select(-bunk) 
#ch_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/chm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ch_phys <- read_sas(paste(phys_dat, "chm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ch_phys$Reference <- as.character(ch_phys$Reference)
ch_hyd <- subset(read_sas("ch_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in ch_hyd
ch_hyd <- ch_hyd[!duplicated(ch_hyd$Reference),]
ch <- left_join(ch, ch_phys, by="Reference") %>% left_join(ch_hyd, by="Reference")
ch <- ch %>% select(noquote(order(colnames(ch))))  #reorders the columns alphabetically 

#clean catch ####

# import environment #### 
#add in air temp
ch_maxT <- read.csv(paste(airtemp, "Max_Temp_CD5.csv", sep="/"), skip=4)
ch_minT <- read.csv(paste(airtemp, "Min_Temp_CD5.csv", sep="/"), skip=4)

#add in nitrogen
ch_nit <- read.csv(paste(nutrient_dat, "Nitrogen_CH.csv", sep="/"))

#add in phosphorous
ch_ph <- read.csv(paste(nutrient_dat, "Phosphorous_CH.csv", sep="/"))

# add in Palmer Z
ch_PZ <- read.csv(paste(palmerz,"PalmerZ_CD5.csv", sep="/" ),skip=3)

#add in rainfall
ch_rf <- read.csv(paste(rainfall, "CH_Rainfall_89_17.csv", sep="/"))

#add in salinity
ch_sal <- read.csv(paste(salinity, "Salinity_CharlotteHarbor_MultipleSources.csv", sep="/"))

#add in Seagrass Cover
ch_sg <- read.csv(paste(seagrass, "Seagrass_Cover_CharlotteHarbor.csv", sep="/"))

#add in streamflow
ch_CaloR <- read.csv(paste(streamflow, "CH/Caloosagatchee_River.csv", sep="/"), skip=28)
ch_MyakR <- read.csv(paste(streamflow, "CH/Myakka_River.csv", sep="/"), skip=28)
ch_PeaR <- read.csv(paste(streamflow, "CH/Peace_River.csv", sep="/"), skip=28)
ch_ShellC <- read.csv(paste(streamflow, "CH/Shell_Creek.csv", sep="/"), skip=28)

#add in water temp
ch_wt <- read.csv(paste(watertemp, "WaterTemp_CH.csv", sep="/"))

# join nitrogen, phosphorous, salinity, water temp ####
# tic()
# nit_full <- joinEV(CH_main, ch_nit, 0.017, 0.017, nitrogen, "TN_ugl")
# toc()
# write.csv(nit_full, paste(out, "CH_nit_join.csv", sep="/"))
# 
# tic()
# ph_full <- joinEV(CH_main, ch_ph, 0.017, 0.017, phosphorous, "TP_ugl")
# toc()
# write.csv(ph_full, paste(out, "CH_ph_join.csv", sep="/"))
# 
# tic()
# sal_full <- joinEV(CH_main, ch_sal, 0.017, 0.017, salinity, "Salinity_ppt")
# toc()
# write.csv(sal_full, paste(out, "CH_sal_join.csv", sep="/"))
# 
# tic()
# wt_full <- joinEV(CH_main, ch_wt, 0.017, 0.017, watertemp, "TempW_F")
# toc()
# write.csv(wt_full, paste(out, "CH_wt_join.csv", sep="/"))




# IR ####
ir = subset(read_sas("ir_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11)) 
#ir_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/irm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ir_phys <- read_sas(paste(phys_dat, "irm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
ir_phys$Reference <- as.character(ir_phys$Reference)
ir_hyd <- subset(read_sas("ir_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in ir_hyd
ir_hyd <- ir_hyd[!duplicated(ir_hyd$Reference),]
ir <- left_join(ir, ir_phys, by="Reference") %>% left_join(ir_hyd, by="Reference")
ir <- ir %>% select(noquote(order(colnames(ir))))  #reorders the columns alphabetically 

# JX ####
jx = subset(read_sas("jx_yoy_cn_c.sas7bdat") , month %in% c(5,6,7,8,9,10,11)) 
#jx_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/jxm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
jx_phys <- read_sas(paste(phys_dat, "jxm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
jx_phys$Reference <- as.character(jx_phys$Reference)
jx_hyd <- subset(read_sas("jx_yoy_cn_hyd.sas7bdat")) 
#Also duplicated References in jx_hyd
jx_hyd <- jx_hyd[!duplicated(jx_hyd$Reference),]
jx <- left_join(jx, jx_phys, by="Reference") %>% left_join(jx_hyd, by="Reference")
jx <- jx %>% select(noquote(order(colnames(jx))))  #reorders the columns alphabetically 

#JOIN ALL CATCH DATA ####
full <- rbind(ap, ck, tb, ch, ir, jx)



# Nitrogen data 













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

# PROCUDE ATTENUATION COEFFICIENT FROM SECCHI DEPTH ####
# Only want to do this for Secchi_on_bottom = NO
full <- full %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))










