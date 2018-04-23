# About ####

# R script for getting all of the FIM habitat and environmental variables together.
# Some of this script borrows from Delta_Method_For_Producing_Nominal
# Environmental variables collected by FIM include: Dissolved O2, Salinity, Temp, pH, secchi depth
# test change for github demonstration

# Set Working Directory ####
#must change working directory for data when working on personal vs work computer
rm(list=ls())

# Set Location
IS_HOME = FALSE

if (IS_HOME == TRUE) {
  personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
  phys_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data"
  enviro_data = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData"
  out =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes"
  source_location
  setwd(personal_comp)
} else {
  work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
  phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
  enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
  out =   "U:/PhD_projectfiles/Exported_R_Datafiles"
  source_location = "U:/R_For_School/Seatrout_ENV_Chapter2"
  setwd(work_comp)
}

# source functions ####

source(paste(source_location, "functions_for_Join_Env_Vars_with_FIM.R", sep="/"))

# Load Packages
####
library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
library(tictoc)
library(dplyr) 
library(tidyverse)
library(lubridate)

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
TB_red <- tidy_catch(TB_cat)

# import environment #### 
#add in air temp
tb_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD4.csv", sep="/"), skip=4)
tb_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD4.csv", sep="/"),skip=4)

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
tb_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD4.csv", sep="/" ), skip=3)

#add in rainfall
tb_rf1 <- read.csv(paste(enviro_data, "Rainfall/TB_Rainfall_89_07.csv", sep="/"))
tb_rf2 <- read.csv(paste(enviro_data, "Rainfall/TB_Rainfall_08_17.csv", sep="/"))
tb_rf <- rbind(tb_rf1, tb_rf2)

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

#remove outliers from streamflow during interested time frame 
tb_AR$X25177_00060_00003 <- as.numeric(as.character(tb_AR$X25177_00060_00003))
tb_AR <- tb_AR[tb_AR$X25177_00060_00003<=10000,] #in 2004 there was a crazy wet season that matches up with the data

#add in water temp
tb_wt1 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Hillsborough_Bay.csv", sep="/"))
tb_wt2 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Lower_Tampa_Bay.csv", sep="/"))
tb_wt3 <- read.csv(paste(enviro_data, "WaterTemp/TB/Water_temperature_Middle_Tampa_Bay.csv", sep="/"))
tb_wt <- rbind(tb_wt1, tb_wt2, tb_wt3)

# HERE convert water temp from F to C ####
tb_wt <- tb_wt %>% 


#clean epc data and build DIN ####
nit <- tb_nit %>% dplyr::select(-c(Characteristic, Original_Result_Unit, Original_Result_Value, QACode, DEP_WBID, ActivityDepth, Sample_Fraction, Result_Comment, Actual_StationID))
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

nit_spread$DIN <- rowSums(nit_spread[,c(13,15)], na.rm=TRUE) #NH3 and N0x and amonium (if present)
#remove outliers
nit_spread <- nit_spread[nit_spread$DIN<=150,]


#join nitrogen, phosphorous, salinity, water temp ####
# tic()
#TB_shrt <- TB_red[1:100,]
#nit_full <- joinNit(TB_shrt, nit_spread, 0.017, 0.017, nitrogen)
# toc()
# write.csv(nit_full, paste(out, "TB_nit_join.csv", sep="/"))

#DIN
# tic()
# full <- joinNit(TB_red,nit_spread, 0.017, 0.017, nitrogen) 
# toc()
# write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_017_DIN.csv", sep="/")) 


# tic()
# full <- joinNit(TB_red,nit_spread, 0.0288, 0.0288, nitrogen)
# toc()
# write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_028_DIN.csv", sep="/")) 
# 
# 
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
tb_av_AR <- cleanSF(tb_AR, "AR") #monthly mean discharge in cubic feet/second
tb_av_HR <- cleanSF(tb_HR, "HR") # monthly mean discharge in cubic feet/second
tb_av_LMR <- cleanSF(tb_LMR, "LMR")     # monthly mean discharge in cubic feet/second

streamfl <- rbind(tb_av_AR, tb_av_HR, tb_av_LMR)
streamfl <- subset(streamfl, month >= min(unique(TB_cat$month)) & year>= min(unique(TB_cat$year)))


TB_cat$riv_flow <- 1
TB_cat <- join_riverflow(TB_cat, streamfl)

#merge nitrogen, phos, salinity, water temp ####

#DIN
TB_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(TB_nit) <- c("Reference", "Nit_val")

TB_phos <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_ph_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_phos) <- c("Reference", "Phos_val")

TB_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_sal) <- c("Reference", "Sal_val")
hist(TB_sal$Sal_val)

TB_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(TB_wat) <- c("Reference", "WatTemp_val")
hist(TB_wat$WatTemp_val)


#join back to original catch dataset- not the reduced one. 
TB_new <-  left_join(TB_cat, TB_nit, by="Reference")
TB_new2 <-  left_join(TB_new, TB_phos, by="Reference")
TB_new3 <-  left_join(TB_new2, TB_sal, by="Reference")
TB_new4 <-  left_join(TB_new3, TB_wat, by="Reference")

# merge airtemp and palmerZ (climate zones-CD) ####

TB_new5 <- joinCD(TB_new4, tb_PZ,tb_maxT,tb_minT)

#merge rainfall ####
tb_tot_rf <- cleanRF(tb_rf, "TotalMonthlyRF")

TB_new6 <- left_join(TB_new5, tb_tot_rf, by=c("year", "month"))

#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO

TB_new6 <- TB_new6 %>% mutate(aten_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(TB_new6, paste(out, "Seatrout_ENV_Chapter2/TB_all_env_no_lag.csv", sep="/"))

# . ####
# Lag Variable Calculations ####
# . ####

TB_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
TB_cat_env$closest_riv <- as.character(TB_cat_env$closest_riv)

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
TB_cat_env <- join_seas_streamflow(TB_cat_env, tb_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works
#test <- TB_cat_env_new[sample(nrow(TB_cat_new),50), ]

#merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(tb_PZ, tb_maxT, tb_minT)
TB_cat_env2 <- join_seasCD(TB_cat_env, seasonal_CD)

#test_set <- TB_cat_new2[,30:107][sample(nrow(TB_cat_new2), 10),]

# merge seasonal rainfall #### 

tb_seas_rf <- clean_seasRF(tb_rf)
TB_cat_env3 <- join_seasRF(TB_cat_env2, tb_seas_rf)

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
selected_stations = c(93,92,24,22,95,91,96,90,25,23,21,28,19,16,84,81,9)

spawn_SAWT <- clean_seas_sal_wt(tb_sal2,tb_wt2, "Salinity_ppt", "TempW_F", selected_stations, "monthly")
seas_SAWT <- clean_seas_sal_wt(tb_sal2,tb_wt2, "Salinity_ppt", "TempW_F", selected_stations, "seasonal")

#replace NAs in spawn_month
TB_cat_env4 <- TB_cat_env4 %>% replace_na(list(spawn_month=9999))

TB_cat_env5 <- join_spawn_SAWT(TB_cat_env4, spawn_SAWT)
TB_cat_env6 <- join_seas_SAWT(TB_cat_env5, seas_SAWT)

#merge seasonal nitro ####

seas_nitro <- clean_seas_nitro(nit_spread, selected_stations)
TB_cat_env7 = join_spawn_nitro(TB_cat_env6, seas_nitro)
TB_cat_env7$avg_last2_nitro[TB_cat_env7$avg_last2_nitro == "NaN"] <- NA
TB_cat_env7$avg_last3_nitro[TB_cat_env7$avg_last3_nitro == "NaN"] <- NA


# trim and output ####
write.csv(TB_cat_env7, paste(out, "Seatrout_ENV_Chapter2/TB_all_env_with_lag.csv", sep="/"))

#add in chlor A ####
#multiple obs per day and station
library(lubridate)
clorA <- read.csv(paste(enviro_data,"chlorophyll_TB.csv", sep="/"))
clorA <- clorA %>% mutate(DATE = mdy_hm(SampleDate), Year=year(DATE), Month=month(DATE), DAY=day(DATE))
clorA <- clorA %>% ungroup() %>% group_by(StationName,StationID, Actual_Latitude, Actual_Longitude,Year, Month, DAY ) %>% summarize(Result_Value=mean(Result_Value))
clorA$Year <- substr(clorA$Year, 3,4)


TB_red$year <- as.numeric(as.character(TB_red$year))


TB_shrt <- TB_red[TB_red$year %in% c( 98,99, 00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15),]
tic()
clor_full <- joinCL(TB_shrt, clorA, 0.0432, 0.0432, chlor)
toc()
write.csv(clor_full, paste(out, "Seatrout_ENV_Chapter2/TB_clor_join_043.csv", sep="/"))

TB_clor <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_clor_join_043.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(TB_clor) <- c("Reference", "Clor_val")


TB_cat_env7 <- read.csv(paste(out, "Seatrout_ENV_Chapter2/TB_all_env_with_lag.csv", sep="/"))
TB_cat_env8 <- left_join(TB_cat_env7, TB_clor, by="Reference")
write.csv(TB_cat_env8, paste(out, "Seatrout_ENV_Chapter2/TB_all_env_with_lag_plus_clor.csv", sep="/"))


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
ch_rf1 <- read.csv(paste(enviro_data, "Rainfall/CH_Rainfall_89_97.csv", sep="/"))
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

#explore streamflow
ch_CaloR$X171001_00060_00003 <- as.numeric(as.character(ch_CaloR$X171001_00060_00003))
ch_MyakR$X24691_00060_00003 <- as.numeric(as.character(ch_MyakR$X24691_00060_00003))
ch_PeaR$X24488_00060_00003 <- as.numeric(as.character(ch_PeaR$X24488_00060_00003))
ch_ShellC$X24600_00060_00003 <- as.numeric(as.character(ch_ShellC$X24600_00060_00003))

hist(ch_CaloR$X171001_00060_00003)
subset(ch_CaloR, ch_CaloR$X171001_00060_00003 > 10000)
hist(ch_MyakR$X24691_00060_00003) 
ch_MyakR$X24691_00060_00003[ch_MyakR$X24691_00060_00003 > 4000] <- 4000

max(ch_PeaR$X24488_00060_00003, na.rm=T)
ch_PeaR$X24488_00060_00003[ch_PeaR$X24488_00060_00003 > 15000] <- NA
hist(ch_PeaR$X24488_00060_00003)

hist(ch_ShellC$X24600_00060_00003)
max(ch_ShellC$X24600_00060_00003, na.rm=T)
subset(ch_ShellC, ch_ShellC$X24600_00060_00003>6000)

#add in water temp
ch_wt <- read.csv(paste(enviro_data, "WaterTemp/CH/WaterTemp_CH.csv", sep="/"))

# HERE convert water temp from F to C ####
ch_wt <- ch_wt %>% mutate(Result_value = Original_Result_Value)
ch_wt$Result_Value = ch_wt$Result_value
ch_wt <- ch_wt %>% select(-Result_value)
ch_wt$Result_unit <- "deg C"
ch_wt$Result_Unit = ch_wt$Result_unit
ch_wt <- ch_wt %>% select(-Result_unit)
  

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

subset(nit_spread, nit_spread$DIN>1500)
nit_spread$DIN <- rowSums(nit_spread[,c(13,14)], na.rm=TRUE) #ammonium, nitrite, nitrate if present

#remove outliers
hist(nit_spread$DIN)
nit_spread$DIN[nit_spread$DIN > 400] <- 400

#join nitrogen, phosphorous, salinity, water temp ####

#PROBLEM- there are years in CH_main that are not present in ch_nit so the function is failing
# Need to fix this..... 
#PROBLEM- additionally, there are multiple readings in the ch_nit for a station, year and month (i.e. a station took two measurements within the same month and year)
# which will cause the reference value to be duplicated so that it records both nitrogen measurements.
# Not sure the best way to deal with this so will just produce it and then select unique reference values afterward.

#DIN
#tic()
#nit_full <- joinNit(CH_red, nit_spread, 0.017, 0.017, nitrogen)
#toc()
#write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_017_DIN.csv", sep="/"))

#tic()
#nit_full <- joinNit(CH_red, nit_spread, 0.0288, 0.0288, nitrogen)
#toc()
#nit_full <- subset(nit_full, !duplicated(V3))
#write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/CH_nit_join_028_DIN.csv", sep="/"))

tic()
nit_full <- joinNit(CH_red, nit_spread, 0.0432, 0.0432, nitrogen)
toc()
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


CH_cat$riv_flow <- 1
CH_cat <- join_riverflow(CH_cat, streamfl)

# merge nitrogen, phos, salinity, water temp ####

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
ch_tot_rf <- cleanRF(ch_rf, "TotalMonthlyRF")

CH_new6 <- left_join(CH_new5, ch_tot_rf, by=c("year", "month"))

#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
CH_new6 <- CH_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(CH_new6, paste(out, "Seatrout_ENV_Chapter2/CH_all_env_no_lag.csv", sep="/"))

#.####
# Lag Variable Calculations ####
#.####

CH_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CH_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
CH_cat_env$closest_riv <- as.character(CH_cat_env$closest_riv)

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
CH_cat_env <- join_seas_streamflow(CH_cat_env, ch_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works


#merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(ch_PZ, ch_maxT, ch_minT)

CH_cat_env2 <- join_seasCD(CH_cat_env, seasonal_CD)

#merge seasonal rainfall #### 
ch_seas_rf <- clean_seasRF(ch_rf)
CH_cat_env3 <- join_seasRF(CH_cat_env2, ch_seas_rf)

#merge seasonal ALL streamflow ####

colnames(ch_CaloR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ch_MyakR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ch_PeaR) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- rbind(ch_CaloR, ch_MyakR, ch_PeaR)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

CH_cat_env4 <- join_seas_streamALL(CH_cat_env3, seas_ALLsf)

#merge seasonal salinity and water temp ####
selected_stations <- c("141", "177", "179", "CHV011", "159" )
#which 

spawn_SAWT <- clean_seas_sal_wt(ch_sal,ch_wt, "Salinity_ppt", "TempW_F", selected_stations, "monthly")
seas_SAWT <- clean_seas_sal_wt(ch_sal,ch_wt, "Salinity_ppt", "TempW_F", selected_stations, "seasonal")

#replace NAs in spawn_month
CH_cat_env4 <- CH_cat_env4 %>% replace_na(list(spawn_month=9999))

CH_cat_env5 <- join_spawn_SAWT(CH_cat_env4, spawn_SAWT)
CH_cat_env6 <- join_seas_SAWT(CH_cat_env5, seas_SAWT)

# merge seasonal nitro ####
seas_nitro <- clean_seas_nitro(nit_spread, selected_stations)
CH_cat_env7 = join_spawn_nitro(CH_cat_env6, seas_nitro)

# trim and output ####
write.csv(CH_cat_env7, paste(out, "Seatrout_ENV_Chapter2/CH_all_env_with_lag.csv", sep="/"))

# ________________####
# APPALACHICOLA####
# _______________ ####
#import catch ####
ap = subset(read_sas("ap_yoy_cn_c.sas7bdat"), month %in% c(6,7,8,9,10,11)) %>% dplyr::mutate(bUnk=bunk) %>% dplyr::select(-bunk) 
ap_hyd <- subset(read_sas("ap_yoy_cn_hyd.sas7bdat")) 
ap_hyd <- ap_hyd[!duplicated(ap_hyd$Reference),]
ap_phys <- read_sas(paste(phys_dat, "apm_physical.sas7bdat", sep="/")) %>% dplyr::select(Reference, Secchi_on_bottom, Secchi_depth)
ap_phys$Reference <- as.character(ap_phys$Reference)

ap <- left_join(ap, ap_hyd, by="Reference") 
ap <- left_join(ap, ap_phys, by="Reference")
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
AP_cat <- subset(AP_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values

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
ap_rf <- read.csv(paste(enviro_data, "Rainfall/AP_Rainfall_98_17.csv", sep="/"))
hist(ap_rf$PRCP)

#add in salinity
ap_sal <- read.csv(paste(enviro_data, "Salinity/AP/363383.csv", sep="/")) %>% select(StationCode, DateTimeStamp, Sal)

#add in Seagrass Cover
#ch_sg <- read.csv(paste(enviro_data, "SeagrassCover/Seagrass_Cover_CharlotteHarbor.csv", sep="/"))

#add in streamflow
ap_AR <- read.csv(paste(enviro_data, "Streamflow/AP/Apalachicola_River_NR_Sumatra_FL.csv", sep="/"), skip=28)
ap_AR$X26942_00060_00003 <- as.numeric(as.character(ap_AR$X26942_00060_00003))
hist(ap_AR$X26942_00060_00003)
max(ap_AR$X26942_00060_00003, na.rm=T)

#add in water temp - Temp is in Cels
ap_wt <- read.csv(paste(enviro_data, "WaterTemp/AP/765372.csv", sep="/")) %>% select(StationCode, DateTimeStamp, Temp)

# Cat Point			CP				apacpwq, apacpnut   (29.7021, -84.8802)
# Dry Bar			DB				apadbwq, apadbnut  (29.6747 degrees latitude and -85.0583667 degrees longitude)
# East Bay Bottom		EB				apaebwq, apaebwq 29.7858	84.8752
# East Bay Surface		ES				apaeswq, apaesnut 29.7858	84.8752
# Pilots Cove			PC				apapcwq, apapcnut  29.601	85.0277
# Little St. Marks		LM				apalmwq 29.755689	85.003525


#clean and build DIN ####
#convert mg/L to ug/L
# add ammonia, nitrite, nitrate to get total DIN
ap_nit <- ap_nit %>% mutate(total_DIN = rowSums(ap_nit[,c(3,5,7)], na.rm=TRUE))
ap_nit <- ap_nit %>% mutate(DINug = total_DIN*1000) %>% select(-total_DIN) %>% rename(DIN=DINug)
ap_nit$DateTimeStamp <- as.factor(ap_nit$DateTimeStamp)
ap_nit <- ap_nit %>% mutate(Date = as.Date(DateTimeStamp, format = " %m/%d/%Y"))
ap_nit$Date <- as.character(ap_nit$Date)

hist(ap_nit$DIN)
range(ap_nit$DIN)
mean(ap_nit$DIN)
ap_nit$StationCode <- as.factor(as.character(ap_nit$StationCode))
boxplot(DIN ~ StationCode, data=ap_nit)
ap_nit$DIN[ap_nit$DIN>=500] <- 500 

ap_nit <- ap_nit %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
ap_nit <- ap_nit %>% distinct(StationCode, Year, Month, Day, DateTimeStamp, .keep_all=TRUE)
ap_nit_up <- ap_nit %>% group_by(StationCode, Year, Month) %>% summarize(DIN = mean(DIN, na.rm=T))

#add lat/long to the nit data set
unique(ap_nit$StationCode)
sampling_stations <- read.csv(paste(enviro_data,"Nutrients/AP/sampling_stations.csv", sep="/"), header=T) %>% dplyr::select(Station.Code, Latitude, Longitude, Station.Name) %>% rename(StationCode=Station.Code)
sampling_stations$Longitude <- sampling_stations$Longitude * (-1) 

ap_nit_up <- left_join(ap_nit_up, sampling_stations)

# Cat Point			CP				apacpwq, apacpnut   (29.7021, -84.8802)
# Dry Bar			DB				apadbwq, apadbnut  (29.6747 degrees latitude and -85.0583667 degrees longitude)
# East Bay Bottom		EB				apaebwq, apaebwq 29.7858	84.8752
# East Bay Surface		ES				apaeswq, apaesnut 29.7858	84.8752
# Pilots Cove			PC				apapcwq, apapcnut  29.601	85.0277
# Nicks Hole                   apanhnut 29.6504	84.9289
#Sikes Cut              apascnut  29.6067	84.9467
#East Bay Bridge apaegnut 29.7308	84.9452
# West Pass apawpnut 29.6379	85.089

#adjust salinity ####
ap_sal$Sal[ap_sal$Sal >50] <- NA
ap_sal$DateTimeStamp <- as.factor(ap_sal$DateTimeStamp)
ap_sal <- ap_sal %>% mutate(Date = as.Date(DateTimeStamp, format = " %m/%d/%Y"))
ap_sal$Date <- as.character(ap_sal$Date)
ap_sal <- ap_sal %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day =substr(Date,9,10))

#there are multiple daily observations but only for certain stations so just need to select unique rows based on 
#StationCode, Year, Month, Day 
ap_sal <- ap_sal %>% distinct(StationCode, Year, Month, Day, .keep_all=TRUE)
ap_sal_up <- ap_sal %>% group_by(Year, Month, StationCode) %>% summarize(Sal = mean(Sal, na.rm=T))
ap_sal_up$Sal[ap_sal_up$Sal == "NaN"] <- NA

ap_sal_up <- left_join(ap_sal_up, sampling_stations)

#adjust water temp ####
ap_wt$Temp <- as.numeric(ap_wt$Temp)
hist(ap_wt$Temp)
ap_wt$Temp[ap_wt$Temp >= 40] <- NA  #one outlier 
hist(ap_wt$Temp)

ap_wt$DateTimeStamp <- as.factor(ap_wt$DateTimeStamp)
ap_wt <- ap_wt %>% mutate(Date = as.Date(DateTimeStamp, format = " %m/%d/%Y"))
ap_wt$Date <- as.character(ap_wt$Date)
ap_wt <- ap_wt %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))

#there are multiple daily observations but only for certain stations so just need to select unique rows based on 
#StationCode, Year, Month, Day 
ap_wt <- ap_wt %>% distinct(StationCode, Year, Month, Day, .keep_all=TRUE)
ap_wt_up <- ap_wt %>% group_by(Year, Month, StationCode) %>% summarize(Temp = mean(Temp, na.rm=T))
ap_wt_up$Temp[ap_wt_up$Temp == "NaN"] <- NA
ap_wt_up <- left_join(ap_wt_up, sampling_stations)


#join nitrogen, salinity, water temp ####
#DIN
#tic()
#nit_full <- joinNit_AP(AP_red, ap_nit_up, 0.0432, 0.0432, nitrogen)
#toc()
#write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/AP_nit_join_043_DIN.csv", sep="/"))

#Sal
#tic()
#sal_full <- joinSal_AP(AP_red, ap_sal_up, 0.043, 0.043, salinity) 
#toc()
#write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/AP_sal_join_043.csv", sep="/")) 

#wat temp
#tic()
#wt_full <- joinSal_AP(AP_red, ap_wt_up, 0.0432, 0.0432, waterTemp) 
#toc()
#write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/AP_wt_join_043.csv", sep="/")) 

#merge closest river mouth ####
AR_mouth = c(-84.9806, 29.729) #, "AR") #alafia

riv_name= c("AR")
AP_cat$closest_riv <- "AR"


#merge riverflow ####
ap_av_AR <- cleanSF(ap_AR, "AR") #mean discharge in cubic feet/second

streamfl <- ap_av_AR
streamfl <- subset(streamfl, month >= min(unique(AP_cat$month)) & year>= min(unique(AP_cat$year)))

AP_cat$riv_flow <- 1
AP_cat <- join_riverflow(AP_cat, streamfl)


# merge nitrogen, phos, salinity, water temp ####

#DIN
AP_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/AP_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(AP_nit) <- c("Reference", "Nit_val")

AP_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/AP_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(AP_sal) <- c("Reference", "Sal_val")

AP_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/AP_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(AP_wat) <- c("Reference", "WatTemp_val")

#join back to original catch dataset- not the reduced one. 
AP_new2 <-  left_join(AP_cat, AP_nit, by="Reference")
AP_new2$Phos_val <- "NA"
AP_new3 <-  left_join(AP_new2, AP_sal, by="Reference")
AP_new4 <-  left_join(AP_new3, AP_wat, by="Reference")

#merge airtemp and palmerZ (climate zones-CD) ####
AP_new5 <- joinCD(AP_new4, ap_PZ,ap_maxT,ap_minT)

#merge rainfall ####
ap_tot_rf <- cleanRF(ap_rf, "TotalMonthlyRF")

AP_new6 <- left_join(AP_new5, ap_tot_rf, by=c("year", "month"))

#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
AP_new6 <- AP_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(AP_new6, paste(out, "Seatrout_ENV_Chapter2/AP_all_env_no_lag.csv", sep="/"))

#.####
#Lag Variable Calculations ####
#.####

AP_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/AP_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
AP_cat_env$closest_riv <- as.character(AP_cat_env$closest_riv)

#merge seasonal streamflow ####
ap_seas_AR <- cleanSF_withSeason(ap_AR, "Mean_dis") #mean discharge in cubic feet/second
ap_seas_AR$riv <- "AR"

ap_seas_All <- ap_seas_AR

#takes a long time
tic()
AP_cat_env <- join_seas_streamflow(AP_cat_env, ap_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works

#merge seasonal_CD airtemp and palmerZ ####  
seasonal_CD <- clean_seasCD(ap_PZ, ap_maxT, ap_minT)

AP_cat_env2 <- join_seasCD(AP_cat_env, seasonal_CD)

#merge seasonal rainfall #### 
ap_seas_rf <- clean_seasRF(ap_rf)
AP_cat_env3 <- join_seasRF(AP_cat_env2, ap_seas_rf)

#merge seasonal ALL streamflow ####
colnames(ap_AR) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- ap_AR

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

AP_cat_env4 <- join_seas_streamALL(AP_cat_env3, seas_ALLsf)


# merge seasonal salinity and water temp ####
# select spawning stations
selected_stations <- c("apawpnut", "apawpwq", "apapcnut", "apapcwq", "apascnut", "apascwq", "apanhnut", "apanhwq", "apadbnut", "apadbwq")
                       
spawn_SAWT <- AP_clean_seas_sal_wt(ap_sal_up,ap_wt_up, selected_stations, "monthly")
seas_SAWT <- AP_clean_seas_sal_wt(ap_sal_up,ap_wt_up, selected_stations, "seasonal")

#replace NAs in spawn_month
AP_cat_env4 <- AP_cat_env4 %>% replace_na(list(spawn_month=9999))

AP_cat_env5 <- join_spawn_SAWT(AP_cat_env4, spawn_SAWT)
AP_cat_env6 <- join_seas_SAWT(AP_cat_env5, seas_SAWT)

#merge seasonal nitro ####
seas_nitro <- AP_clean_seas_nitro(ap_nit_up, selected_stations)
AP_cat_env7 <- join_spawn_nitro(AP_cat_env6, seas_nitro)

# trim and output ####
write.csv(AP_cat_env7, paste(out, "Seatrout_ENV_Chapter2/AP_all_env_with_lag.csv", sep="/"))


# ________________####
# INDIAN RIVER LAGOON ####
# _______________ ####
#import catch ####
ir = subset(read_sas("ir_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11))
ir_hyd <- subset(read_sas("ir_yoy_cn_hyd.sas7bdat")) 
ir_hyd <- ir_hyd[!duplicated(ir_hyd$Reference),]
ir_phys <- read_sas(paste(phys_dat, "irm_physical.sas7bdat", sep="/")) %>% dplyr::select(Reference, Secchi_on_bottom, Secchi_depth)
ir_phys$Reference <- as.character(ir_phys$Reference)

ir <- left_join(ir, ir_hyd, by="Reference") 
ir <- left_join(ir, ir_phys, by="Reference")
ir <- ir %>% select(noquote(order(colnames(ir))))  #reorders the columns alphabetically 


# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(ir$Zone)
unique(subset(ir, is.na(Latitude))$Zone) #assume if its missing Long then its also missing Lat

#No missing lats and longs
IR_cat <- ir %>% mutate(NewLong = Longitude, NewLat = Latitude)

#import station long/lat data ####
station1 <- read.csv(paste(enviro_data, "active and inactive WQ stations_LSJ and IRL.csv", sep="/")) %>% select(-STN_STTS_CD)
station2 <- read.csv(paste(enviro_data,"SJR_STN_ET_IRL and LSJ_active mainstem no IRLSG.csv", sep="/"))
station2 <- station2 %>% dplyr::rename(MJR_BSN_NM= Major.Basin, STN_ID=Station.ID, STN_NAME=Station.Name, PNT_LOC_DSC=Location.Description, LAT_NO=LAT..DDMMSS.S., LONG_NO=LONG..DDMMSS.S.) %>%
                  dplyr::select(-c(Comments, All.surfacewater.stations))
remaining_stations <- read.csv(paste(enviro_data, "remaining_stations.csv", sep="/")) 
remaining_stations$MJR_BSN_NM <- NA
remaining_stations <- remaining_stations %>% dplyr::select(-Status) %>% dplyr::rename(STN_ID=ID, STN_NAME=Station.Name, PNT_LOC_DSC=Location.Description,LAT_NO=Lat_DDMMSS.SS, LONG_NO=LONG_DDMMSS.SS )

stations <- rbind(station1, station2,remaining_stations) %>% distinct(STN_NAME, .keep_all=TRUE) 

stations[,c("LAT_NO","LONG_NO")] <- as.character(unlist(stations[,c("LAT_NO","LONG_NO")]))

stations <- stations %>% mutate(LatDeg = as.numeric(substr(LAT_NO,1,2)), 
                                LongDeg = as.numeric(substr(LONG_NO, 1,2)), 
                                LatMin = as.numeric(substr(LAT_NO,3,4)), 
                                LongMin= as.numeric(substr(LONG_NO,3,4)), 
                                LatSec = as.numeric(substr(LAT_NO, 5,6)), 
                                LongSec= as.numeric(substr(LONG_NO,5,6)), 
                                LAT = (LatDeg + (LatMin/60) + (LatSec/3600)),
                                LONG = -1*(LongDeg + (LongMin/60) + (LongSec/3600))) %>% select (-c(LAT_NO, LONG_NO, LatDeg, LongDeg, LatMin, LongMin, LatSec, LongSec))


#import length data ####
ir_length = subset(read_sas("ir_yoy_cn_l.sas7bdat"))
ir_length = subset(ir_length, bio_reference %in% unique(IR_cat$bio_reference))

ir_length_exp <- ir_length[rep(row.names(ir_length), ir_length$COUNT), 1:3]
ir_length_ag <- aggregate(sl ~ bio_reference, data=ir_length_exp, FUN= "median") 
colnames(ir_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

IR_cat <- left_join(IR_cat, ir_length_ag, by="bio_reference")
IR_cat$approx_age_mo <- round((2.476*IR_cat$median_sl - 0.012*(IR_cat$median_sl)^2)*0.0328767)
IR_cat$spawn_month <- IR_cat$month - IR_cat$approx_age_mo

#filter these lengths that don't make sense
IR_cat <- subset(IR_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values

#tidy catch ####
IR_red <- tidy_catch(IR_cat)

# import environment #### 
#add in air temp
ir_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD3.csv", sep="/"), skip=4)
ir_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD3.csv", sep="/"), skip=4)

#add in nitrogen
ir_nit <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/IR/IR_nit.csv", sep="/"), skip=2) 

# add in Palmer Z
ir_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD3.csv", sep="/" ),skip=3)

#add in rainfall
ir_rf1 <- read.csv(paste(enviro_data, "Rainfall/IR_Rainfall_89_97.csv", sep="/"))
ir_rf2 <- read.csv(paste(enviro_data, "Rainfall/IR_Rainfall_98_07.csv", sep="/"))
ir_rf3 <- read.csv(paste(enviro_data, "Rainfall/IR_Rainfall_08_17.csv", sep="/"))
ir_rf <- rbind(ir_rf1, ir_rf2, ir_rf3)
hist(ir_rf$PRCP)

#add in salinity
ir_sal <- read.csv(paste(enviro_data, "Salinity/IR/IR_sal.csv", sep="/"), skip=2) 

#add in streamflow
ir_CC <- read.csv(paste(enviro_data, "Streamflow/IR/Crane_Creek.csv", sep="/"), skip=28)
ir_CC$X23062_00060_00003 <- as.numeric(as.character(ir_CC$X23062_00060_00003))
hist(ir_CC$X23062_00060_00003)

ir_EG <- read.csv(paste(enviro_data, "Streamflow/IR/Eau_Gallie.csv", sep="/"), skip=28)
ir_EG$X23060_00060_00003 <- as.numeric(as.character(ir_EG$X23060_00060_00003))
hist(ir_EG$X23060_00060_00003)

ir_TC <- read.csv(paste(enviro_data, "Streamflow/IR/Turkey_Creek.csv", sep="/"), skip=28)
ir_TC$X23082_00060_00003 <- as.numeric(as.character(ir_TC$X23082_00060_00003))
hist(ir_TC$X23082_00060_00003)
ir_TC <- ir_TC[,1:5]

ir_FC <- read.csv(paste(enviro_data, "Streamflow/IR/Fellsmere_Canal.csv", sep="/"), skip=28)
ir_NPSS <- read.csv(paste(enviro_data, "Streamflow/IR/North_Prong_St_Sebastian.csv", sep="/"), skip=28)
ir_SPSS <- read.csv(paste(enviro_data, "Streamflow/IR/South_Prong_St_Sebastian.csv", sep="/"), skip=28)

ir_FC <- ir_FC[-1,]
ir_FC$datetime <- mdy(ir_FC$datetime)
ir_FC <- ir_FC %>% mutate(Year = year(datetime), Month=month(datetime), Day =day(datetime))
ir_FC$X23105_00065_00003 <- as.numeric(as.character(ir_FC$X23105_00065_00003))

ir_NPSS <- ir_NPSS[-1,]
ir_NPSS$datetime <- mdy(ir_NPSS$datetime)
ir_NPSS <- ir_NPSS %>% mutate(Year = year(datetime), Month=month(datetime), Day =day(datetime))
ir_NPSS$X23101_00060_00003 <- as.numeric(as.character(ir_NPSS$X23101_00060_00003))

ir_SPSS <- ir_SPSS[-1,]
ir_SPSS$datetime <- mdy(ir_SPSS$datetime)
ir_SPSS <- ir_SPSS %>% mutate(Year = year(datetime), Month=month(datetime), Day =day(datetime))
ir_SPSS$X23096_00060_00003 <- as.numeric(as.character(ir_SPSS$X23096_00060_00003))

library(plyr)
comb <- plyr::join_all(list(ir_FC, ir_NPSS,ir_SPSS), by=c("Year", "Month", "Day"), type='full') 
comb$TotDis <-  rowSums(comb[c('X23105_00065_00003', 'X23101_00060_00003', "X23096_00060_00003")], na.rm=T)
ir_SScomb <- comb %>% dplyr::select(agency_cd, site_no, datetime, TotDis)
ir_SScomb$code <-NA
hist(ir_SScomb$TotDis)
max(ir_SScomb$TotDis)


#add in water temp - Temp is in Cels
ir_wt <- read.csv(paste(enviro_data, "WaterTemp/IR/IR_wt.csv", sep="/"), skip=2) %>% select(-c(Comments, X, X.1))

#clean and build DIN ####
#convert mg/L to ug/L
#ir_nit <- ir_nit[ir_nit$Analyte == "NOx-D",]
ir_nit <- subset(ir_nit, Analyte == "NOx-D" & !(Qualifier.Code %in% c("T", "TQ", "I", "IQ", "Y", "YT","YIQ", "YQ", "YI", "Q", "YTQ")))
ir_nit <- ir_nit %>% mutate(Valueug = Value*1000) %>% dplyr::select(-Value) %>% dplyr::rename(Value=Valueug)
ir_nit <- ir_nit %>% mutate(DATE = mdy_hm(Date)) %>% mutate(Year = year(DATE), Month=month(DATE), Day = day(DATE))

hist(ir_nit$Value)
range(ir_nit$Value, na.rm=T)
mean(ir_nit$Value, na.rm=T)
ir_nit$Value[ir_nit$Value>=1500] <- NA
ir_nit$Value[ir_nit$Value<0] <- NA

ir_nit <- ir_nit %>% distinct(Station, Year, Month, Day, .keep_all=TRUE)
ir_nit_up <- ir_nit %>% group_by(Station, Year, Month) %>% dplyr::summarize(Value = mean(Value, na.rm=T)) %>% dplyr::rename(STN_NAME=Station)

#add lat/long to the nit data set
ir_nit_match <- left_join(ir_nit_up, stations, by="STN_NAME")
ir_nit_select <- left_join(ir_nit_up, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))

#select stations that don't have a match 
no_match <- ir_nit_match[is.na(ir_nit_match$STN_ID),] %>% ungroup() %>% distinct(STN_NAME)
write.csv(no_match, paste(out, "Seatrout_ENV_Chapter2/still_missing_SJRWMD_stations.csv", sep="/"))


#adjust salinity ####
ir_sal <- ir_sal %>% dplyr::rename(STN_NAME = Station)
hist(ir_sal$Value)
ir_sal$Val[ir_sal$Val >40] <- NA

ir_sal <- ir_sal %>% mutate(Date = as.Date(Date, format = " %m/%d/%Y"))
ir_sal$Date <- as.character(ir_sal$Date)
ir_sal <- ir_sal %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
ir_sal <- ir_sal %>% distinct(STN_NAME, Year, Month, Day, .keep_all=TRUE)
ir_sal <- ir_sal %>% group_by(STN_NAME, Year, Month) %>% dplyr::summarize(Val = mean(Value, na.rm=T))

ir_sal_select <- left_join(ir_sal, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))

#adjust water temp ####
ir_wt <- ir_wt %>% dplyr::rename(STN_NAME = Station)
hist(ir_wt$Value)

ir_wt <- ir_wt %>% mutate(Date = as.Date(Date, format = " %m/%d/%Y"))
ir_wt$Date <- as.character(ir_wt$Date)
ir_wt <- ir_wt %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
ir_wt <- ir_wt %>% distinct(STN_NAME, Year, Month, Day, .keep_all=TRUE)
ir_wt <- ir_wt %>% group_by(STN_NAME, Year, Month) %>% dplyr::summarize(Val = mean(Value, na.rm=T))

ir_wt_select <- left_join(ir_wt, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))


# join nitrogen, salinity, water temp ####
#DIN
tic()
nit_full <- joinEnv_IRJX(IR_red, ir_nit_select, 0.0432, 0.0432, nitrogen)
toc()
write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/IR_nit_join_043_DIN.csv", sep="/"))

#Sal
tic()
sal_full <- joinEnv_IRJX(IR_red, ir_sal_select, 0.043, 0.043, salinity) 
toc()
write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/IR_sal_join_043.csv", sep="/")) 

#wat temp
tic()
wt_full <- joinEnv_IRJX(IR_red, ir_wt_select, 0.0432, 0.0432, waterTemp) 
toc()
write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/IR_wt_join_043.csv", sep="/")) 

# merge closest river mouth ####
#ir_CC <- read.csv(paste(enviro_data, "Streamflow/IR/Crane_Creek.csv", sep="/"), skip=28)
#ir_EG <- read.csv(paste(enviro_data, "Streamflow/IR/Eau_Gallie.csv", sep="/"), skip=28)
#ir_FC <- read.csv(paste(enviro_data, "Streamflow/IR/Fellsmere_Canal.csv", sep="/"), skip=28)
#ir_NPSS <- read.csv(paste(enviro_data, "Streamflow/IR/North_Prong_St_Sebastian.csv", sep="/"), skip=28)
#ir_SPSS <- read.csv(paste(enviro_data, "Streamflow/IR/South_Prong_St_Sebastian.csv", sep="/"), skip=28)
#ir_TC <- read.csv(paste(enviro_data, "Streamflow/IR/Turkey_Creek.csv", sep="/"), skip=28)

CC_mouth = c(-80.5979, 28.0765) #, "CC") #crane creek
EG_mouth = c(-80.6226, 28.1246) #, "CC") #eau gallie river
SS_mouth = c(-80.4895, 27.8541) #, "CC") #san sebastian river 
TC_mouth = c(-80.5804, 28.0371) #, "CC") #turkey creek 

riv_name= c("CC", "EG", "SS", "TC")

rivers = data.frame(rbind(CC_mouth, EG_mouth, SS_mouth, TC_mouth))
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)

IR_cat$closest_riv <- ""
IR_cat <- closestRiver(IR_cat, rivers)

#merge riverflow ####
ir_av_CC <- cleanSF(ir_CC, "CC") #mean discharge in cubic feet/second
ir_av_EG <- cleanSF(ir_EG, "EG") #mean discharge in cubic feet/second
ir_av_TC <- cleanSF(ir_TC, "TC") #mean discharge in cubic feet/second
ir_av_SScomb <- cleanSF(ir_SScomb, "SS") #mean discharge in cubic feet/second

streamfl <- rbind(ir_av_CC, ir_av_EG, ir_av_SScomb, ir_av_TC)

streamfl <- subset(streamfl, month >= min(unique(IR_cat$month)) & year>= min(unique(IR_cat$year)))

IR_cat$riv_flow <- NA
IR_cat <- join_riverflow(IR_cat, streamfl)


# merge nitrogen, phos, salinity, water temp ####

#DIN
IR_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/IR_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
colnames(IR_nit) <- c("Reference", "Nit_val")

IR_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/IR_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(IR_sal) <- c("Reference", "Sal_val")

IR_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/IR_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(IR_wat) <- c("Reference", "WatTemp_val")

#join back to original catch dataset- not the reduced one. 
IR_new2 <-  left_join(IR_cat, IR_nit, by="Reference")
IR_new2$Phos_val <- "NA"
IR_new3 <-  left_join(IR_new2, IR_sal, by="Reference")
IR_new4 <-  left_join(IR_new3, IR_wat, by="Reference")

# merge airtemp and palmerZ (climate zones-CD) ####
IR_new5 <- joinCD(IR_new4, ir_PZ,ir_maxT,ir_minT)

# merge rainfall ####
ir_tot_rf <- cleanRF(ir_rf, "TotalMonthlyRF")

IR_new6 <- left_join(IR_new5, ir_tot_rf, by=c("year", "month"))

# produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
IR_new6 <- IR_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(IR_new6, paste(out, "Seatrout_ENV_Chapter2/IR_all_env_no_lag.csv", sep="/"))

#.####
#Lag Variable Calculations ####
#.####
IR_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/IR_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
IR_cat_env$closest_riv <- as.character(IR_cat_env$closest_riv)

#merge seasonal streamflow ####
#ir_av_CC <- cleanSF(ir_CC, "CC") #mean discharge in cubic feet/second
#ir_av_EG <- cleanSF(ir_EG, "EG") #mean discharge in cubic feet/second
#ir_av_SS <- cleanSF(ir_SS, "SS") #mean discharge in cubic feet/second
#ir_av_TC <- cleanSF(ir_TC, "TC") #mean discharge in cubic feet/second

ir_seas_CC <- cleanSF_withSeason(ir_CC, "Mean_dis") #mean disirarge in cubic feet/second
ir_seas_CC$riv <- "CC"
ir_seas_EG <- cleanSF_withSeason(ir_EG, "Mean_dis") #mean disirarge in cubic feet/second
ir_seas_EG$riv <- "EG"
ir_seas_SS <- cleanSF_withSeason(ir_SScomb, "Mean_dis")     #mean disirarge in cubic feet/second
ir_seas_SS$riv <- "SS"
ir_seas_TC <- cleanSF_withSeason(ir_TC, "Mean_dis")     #mean disirarge in cubic feet/second
ir_seas_TC$riv <- "TC"

ir_seas_All <- rbind(ir_seas_CC, ir_seas_EG, ir_seas_SS, ir_seas_TC)

#takes a long time
tic()
IR_cat_env <- join_seas_streamflow(IR_cat_env, ir_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works

#merge seasonal_CD airtemp and palmerZ ####  
seasonal_CD <- clean_seasCD(ir_PZ, ir_maxT, ir_minT)

IR_cat_env2 <- join_seasCD(IR_cat_env, seasonal_CD)

#merge seasonal rainfall #### 
ir_seas_rf <- clean_seasRF(ir_rf)
IR_cat_env3 <- join_seasRF(IR_cat_env2, ir_seas_rf)

#merge seasonal ALL streamflow ####
colnames(ir_CC) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ir_EG) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ir_SScomb) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ir_TC) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- rbind(ir_CC, ir_EG, ir_SScomb, ir_TC)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

IR_cat_env4 <- join_seas_streamALL(IR_cat_env3, seas_ALLsf)

#merge seasonal salinity and water temp ####
selected_stations <- c("UFSYN28", "IRLI06", "UFSYN26", "IRLB01", "IRLB02", "IRLI28" )
#which 
ir_sal_select <- ir_sal_select %>% dplyr::rename(Sal=Val)
ir_wt_select <- ir_wt_select %>% dplyr::rename(Temp=Val)
spawn_SAWT <- IRJX_clean_seas_sal_wt(ir_sal_select,ir_wt_select,selected_stations, "monthly")

seas_SAWT  <- IRJX_clean_seas_sal_wt(ir_sal_select, ir_wt_select, selected_stations, "seasonal")

#replace NAs in spawn_month
IR_cat_env4 <- IR_cat_env4 %>% replace_na(list(spawn_month=9999))
spawn_SAWT$Year <- as.character(spawn_SAWT$Year)
spawn_SAWT[,c("Year","Month")] <- as.character(unlist(spawn_SAWT[,c("Year","Month")]))

IR_cat_env5 <- join_spawn_SAWT(IR_cat_env4, spawn_SAWT)
IR_cat_env6 <- join_seas_SAWT(IR_cat_env5, seas_SAWT)

#merge seasonal nitro ####
# there are not observations of nitro near spawning locations ###
seas_nitro <- IRJX_clean_seas_nitro(ir_nit_select, selected_stations)
IR_cat_env7 = join_spawn_nitro(IR_cat_env6, seas_nitro)

#trim and output ####
write.csv(IR_cat_env7, paste(out, "Seatrout_ENV_Chapter2/IR_all_env_with_lag.csv", sep="/"))

# ________________####
# BUILD OUT - JX####
# _______________ ####

#import catch ####
jx = subset(read_sas("jx_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11))
jx_hyd <- subset(read_sas("jx_yoy_cn_hyd.sas7bdat")) 
jx_hyd <- jx_hyd[!duplicated(jx_hyd$Reference),]
jx_phys <- read_sas(paste(phys_dat, "jxm_physical.sas7bdat", sep="/")) %>% dplyr::select(Reference, Secchi_on_bottom, Secchi_depth)
jx_phys$Reference <- as.character(jx_phys$Reference)

jx <- left_join(jx, jx_hyd, by="Reference") 
jx <- left_join(jx, jx_phys, by="Reference")
jx <- jx %>% select(noquote(order(colnames(jx))))  #reorders the columns alphabetically 


# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(jx$Zone)
unique(subset(jx, is.na(Latitude))$Zone) #assume if its missing Long then its also missing Lat

#No missing lats and longs
JX_cat <- jx %>% mutate(NewLong = Longitude, NewLat = Latitude)

#import length data ####
jx_length = subset(read_sas("jx_yoy_cn_l.sas7bdat"))
jx_length = subset(jx_length, bio_reference %in% unique(JX_cat$bio_reference))

jx_length_exp <- jx_length[rep(row.names(jx_length), jx_length$COUNT), 1:3]
jx_length_ag <- aggregate(sl ~ bio_reference, data=jx_length_exp, FUN= "median") 
colnames(jx_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

JX_cat <- left_join(JX_cat, jx_length_ag, by="bio_reference")
JX_cat$approx_age_mo <- round((2.476*JX_cat$median_sl - 0.012*(JX_cat$median_sl)^2)*0.0328767)
JX_cat$spawn_month <- JX_cat$month - JX_cat$approx_age_mo

#filter these lengths that don't make sense
JX_cat <- subset(JX_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values

#tidy catch ####
JX_red <- tidy_catch(JX_cat)

# import environment #### 
#add in air temp
jx_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD2.csv", sep="/"), skip=4)
jx_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD2.csv", sep="/"), skip=4)

#add in nitrogen
jx_nit <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/JX/JX_nitrogen.csv", sep="/"), skip=2) 

# add in Palmer Z
jx_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD2.csv", sep="/" ),skip=3)

#add in rainfall
jx_rf3 <- read.csv(paste(enviro_data, "Rainfall/JX_Rainfall_08_17.csv", sep="/"))
jx_rf1 <- read.csv(paste(enviro_data, "Rainfall/JX_Rainfall_89_97.csv", sep="/")) %>% select(names(jx_rf3))
jx_rf2 <- read.csv(paste(enviro_data, "Rainfall/JX_Rainfall_98_07.csv", sep="/")) %>% select(names(jx_rf3))
#jx_rf3 <- read.csv(paste(enviro_data, "Rainfall/JX_Rainfall_08_17.csv", sep="/"))
jx_rf <- rbind(jx_rf1, jx_rf2, jx_rf3)

#add in streamflow
jx_SJB <- read.csv(paste(enviro_data, "Streamflow/JX/St.Johns_River_at_Buff_Bluff.csv", sep="/"), skip=28)
jx_SJB <- jx_SJB[,c(1:3,6,7)] #tidally filtered
jx_SJB$X22695_72137_00003 <- as.numeric(as.character(jx_SJB$X22695_72137_00003))
jx_SJJ <- read.csv(paste(enviro_data, "Streamflow/JX/St.Johns_River_at_Jax.csv", sep="/"), skip=28)
jx_SJJ <- jx_SJJ[,c(1:3,6,7)] #tidally filtered

jx_SMR <- read.csv(paste(enviro_data, "Streamflow/JX/St.Marys_River.csv", sep="/"), skip=28)
jx_SMR <- jx_SJJ[,c(1:5)] #tidally filtered

#make new combined river ####
jx_CR <- read.csv(paste(enviro_data, "Streamflow/JX/Cedar_River.csv", sep="/"), skip=28)
jx_CR <- jx_CR[-1,]
jx_CR$datetime <- mdy(jx_CR$datetime)
jx_CR <- jx_CR %>% mutate(Year = year(datetime), Month=month(datetime), Day =day(datetime))
jx_CR$X22860_72137_00003 <- as.numeric(as.character(jx_CR$X22860_72137_00003))

jx_OR <- read.csv(paste(enviro_data, "Streamflow/JX/Ortega_River.csv", sep="/"), skip=28)
jx_OR <- jx_OR[-1,]
jx_OR$datetime <- mdy(jx_OR$datetime)
jx_OR <- jx_OR %>% mutate(Year = year(datetime), Month=month(datetime), Day =day(datetime))
jx_OR$X22845_00060_00003 <- as.numeric(as.character(jx_OR$X22845_00060_00003))

comb <- full_join(jx_OR, jx_CR, by=c("Year", "Month", "Day")) 
comb$TotDis <-  rowSums(comb[c('X22845_00060_00003', 'X22860_72137_00003')], na.rm=T)
jx_OCR <- comb %>% select(agency_cd.x, site_no.x, datetime.x, TotDis) %>% rename(agency_cd=agency_cd.x, site_no = site_no.x, datetime=datetime.x)
jx_OCR$code <-NA

#add in Cedar key station data that contains nitrogen, salinity, water temp 
station1 <- read.csv(paste(enviro_data, "station_JX_stjohn.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
station2 <- read.csv(paste(enviro_data, "station_JX_duval.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
stations <- rbind(station1, station2)

result1 <- read.csv(paste(enviro_data, "result_stjohn.csv", sep="/")) 
result2 <- read.csv(paste(enviro_data, "result_duval.csv", sep="/")) 
results <- rbind(result1, result2)

jx_data <- left_join(stations, results, by="MonitoringLocationIdentifier")

#select salinity
jx_sal <- jx_data[jx_data$CharacteristicName == "Salinity",]
jx_sal$ResultMeasureValue <- as.numeric(as.character(jx_sal$ResultMeasureValue))
hist(jx_sal$ResultMeasureValue)
jx_sal <- jx_sal %>% dplyr::select(ActivityStartDate, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier) %>%
  dplyr::mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE)) %>% dplyr::arrange(Year, Month) %>%
  dplyr::select(Year, Month, LatitudeMeasure, LongitudeMeasure, ResultMeasureValue)

jx_sal$ResultMeasureValue[jx_sal$ResultMeasureValue>45] <- NA

#select water temp
jx_wt <- jx_data[jx_data$CharacteristicName == "Temperature, water",]
jx_wt$ResultMeasureValue <- as.numeric(as.character(jx_wt$ResultMeasureValue))
hist(jx_wt$ResultMeasureValue)
jx_wt <- jx_wt %>% select(ActivityStartDate, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier) %>%
  mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE)) %>% arrange(Year, Month) %>%
  select(Year, Month, LatitudeMeasure, LongitudeMeasure, ResultMeasureValue, MonitoringLocationIdentifier)


#select nitrogen
#select any kind of nitrogen to see what's available and then select the ones we want
jx_nit <- jx_data[jx_data$CharacteristicName %in% c("Ammonia", "Nitrate", "Nitrite"),]
jx_nit$ResultMeasureValue <- as.numeric(as.character(jx_nit$ResultMeasureValue))
jx_nit <- jx_nit %>% select(ActivityStartDate, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier, CharacteristicName) %>%
  mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE), Day=day(DATE)) %>% arrange(Year, Month) %>%
  select(Year, Month, Day, LatitudeMeasure, LongitudeMeasure, ResultMeasureValue, MonitoringLocationIdentifier, CharacteristicName)

jx_spread <- jx_nit %>% ungroup(ResultMeasureValue) %>% group_by(Year, Month,Day, MonitoringLocationIdentifier) %>% mutate(group_row=1:n()) %>% spread(CharacteristicName, ResultMeasureValue)

jx_new <- jx_spread %>% summarize(Nitrate = mean(Nitrate, na.rm=T), Nitrite=mean(Nitrite, na.rm=T), Ammonia=mean(Ammonia, na.rm=T)) 
jx_new$Ammonia[jx_new$Ammonia == "NaN"] <- NA
jx_new$Nitrite[jx_new$Nitrite == "NaN"] <- NA
jx_new$Nitrate[jx_new$Nitrate == "NaN"] <- NA

jx_new <- jx_new  %>% ungroup() %>% dplyr::mutate(DIN =rowSums(.[5:7],na.rm=T))

#DIN isnt going to work here because either only nitrate, nitrite or ammonia is reported for each sampling station


# #clean and build DIN ####
# #convert mg/L to ug/L
# #ir_nit <- ir_nit[ir_nit$Analyte == "NOx-D",]
# jx_nit <- subset(jx_nit, Analyte == "NOx-D" & !(Qualifier.Code %in% c("T", "TQ", "I", "IQ", "Y", "YT","YIQ", "YQ", "YI", "Q", "YTQ")))
# jx_nit <- jx_nit %>% mutate(Valueug = Value*1000) %>% dplyr::select(-Value) %>% dplyr::rename(Value=Valueug)
# jx_nit <- jx_nit %>% mutate(DATE = mdy_hm(Date), Year = year(DATE), Month=month(DATE), Day = day(DATE))
# 
# hist(jx_nit$Value)
# range(jx_nit$Value, na.rm=T)
# mean(jx_nit$Value, na.rm=T)
# jx_nit$Value[jx_nit$Value>=1500] <- NA
# jx_nit$Value[jx_nit$Value<0] <- NA
# 
# jx_nit <- jx_nit %>% dplyr::distinct(Station, Year, Month, Day, .keep_all=TRUE)
# jx_nit_up <- jx_nit %>% dplyr::group_by(Station, Year, Month) %>% dplyr::summarize(Value = mean(Value, na.rm=T)) %>% dplyr::rename(STN_NAME=Station)
# 
# #add lat/long to the nit data set
# jx_nit_match <- left_join(jx_nit_up, stations, by="STN_NAME")
# jx_nit_select <- left_join(jx_nit_up, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))
# 
# #select stations that don't have a match 
# no_match <- jx_nit_match[is.na(jx_nit_match$STN_ID),] %>% ungroup() %>% distinct(STN_NAME)
# write.csv(no_match, paste(out, "Seatrout_ENV_Chapter2/still_missing_SJRWMD_stations.csv", sep="/"))


#join nitrogen, salinity, water temp ####
#DIN
# tic()
# nit_full <- joinEnv_IRJX(JX_red, jx_new, 0.0432, 0.0432, nitrogen)
# toc()
# write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/JX_nit_join_043_DIN.csv", sep="/"))
# 
# #Sal
 tic()
 sal_full <- joinEnv_IRJX(JX_red, jx_sal, 0.043, 0.043, salinity) 
 toc()
 write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/JX_sal_join_043.csv", sep="/")) 
# 
# #wat temp
 tic()
 wt_full <- joinEnv_IRJX(JX_red, jx_wt, 0.0432, 0.0432, waterTemp) 
 toc()
 write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/JX_wt_join_043.csv", sep="/")) 

#merge closest river mouth ####
#jx_CR <- read.csv(paste(enviro_data, "Streamflow/JX/Cedar_River.csv", sep="/"), skip=28)
#jx_OR <- read.csv(paste(enviro_data, "Streamflow/JX/Ortega_River.csv", sep="/"), skip=28)

#jx_OCR
#CR_mouth = c(-81.711330, 30.2793) #, "CC") 
#OR_mouth = c(-80.6226, 28.1246) #, "CC") 

SJB_mouth = c(-81.6833, 29.5961) #, "CC") #at guage spot in SJR
SJJ_mouth = c(-81.6655, 30.3222) #, "CC") #at guage spot in SJJ
SMR_mouth = c(-81.4695, 30.7073) #, "CC") #at mouth
OCR_mouth = c(-81.7085, 30.2792) #a mouth of cedar/ortega river

riv_name= c("OCR", "SJB", "SJJ", "SMR")

rivers = data.frame(rbind(OCR_mouth,SJB_mouth, SJJ_mouth, SMR_mouth))
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)

JX_cat$closest_riv <- ""
JX_cat <- closestRiver(JX_cat, rivers)


#merge riverflow ####
jx_av_OCR <- cleanSF(jx_OCR, "OCR")
jx_av_SJB <- cleanSF(jx_SJB, "SJB") #mean discharge in cubic feet/second
jx_av_SJJ <- cleanSF(jx_SJJ, "SJJ") #mean discharge in cubic feet/second
jx_av_SMR <- cleanSF(jx_SMR, "SMR") #mean discharge in cubic feet/second


streamfl <- rbind(jx_av_OCR, jx_av_SJB, jx_av_SJJ, jx_av_SMR)

streamfl <- subset(streamfl, month >= min(unique(JX_cat$month)) & year>= min(unique(JX_cat$year)))


JX_cat$riv_flow <- 1
JX_cat <- join_riverflow(JX_cat, streamfl)


#merge nitrogen, phos, salinity, water temp ####

#DIN
#JX_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/JX_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
#colnames(JX_nit) <- c("Reference", "Nit_val")

JX_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/JX_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(JX_sal) <- c("Reference", "Sal_val")

JX_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/JX_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(JX_wat) <- c("Reference", "WatTemp_val")

#join back to original catch dataset- not the reduced one. 
JX_cat$Nit_val <- NA
JX_cat$Phos_val <- "NA"
JX_new2 <- JX_cat
JX_new3 <-  left_join(JX_new2, JX_sal, by="Reference")
JX_new4 <-  left_join(JX_new3, JX_wat, by="Reference")

#merge airtemp and palmerZ (climate zones-CD) ####
JX_new5 <- joinCD(JX_new4, jx_PZ,jx_maxT,jx_minT)

#merge rainfall ####
jx_tot_rf <- cleanRF(jx_rf, "TotalMonthlyRF")

JX_new6 <- left_join(JX_new5, jx_tot_rf, by=c("year", "month"))

#produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
JX_new6 <- JX_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#output ####
write.csv(JX_new6, paste(out, "Seatrout_ENV_Chapter2/JX_all_env_no_lag.csv", sep="/"))

#.####
#Lag Variable Calculations ####
#.####
JX_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/JX_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
JX_cat_env$closest_riv <- as.character(JX_cat_env$closest_riv)

#merge seasonal streamflow ####
#jx_av_      <- cleanSF()
#jx_av_SJB <- cleanSF(jx_SJB, "SJB") #mean discharge in cubic feet/second
#jx_av_SSJ <- cleanSF(jx_SJJ, "SSJ") #mean discharge in cubic feet/second
#jx_av_SMR <- cleanSF(jx_SMR, "SMR") #mean discharge in cubic feet/second


jx_seas_SJB <- cleanSF_withSeason(jx_SJB, "Mean_dis") #mean disjxarge in cubic feet/second
jx_seas_SJB$riv <- "SJB"
jx_seas_SJJ <- cleanSF_withSeason(jx_SJJ, "Mean_dis") #mean disjxarge in cubic feet/second
jx_seas_SJJ$riv <- "SJJ"
jx_seas_SMR <- cleanSF_withSeason(jx_SMR, "Mean_dis")     #mean disjxarge in cubic feet/second
jx_seas_SMR$riv <- "SMR"
jx_seas_OCR <- cleanSF_withSeason(jx_OCR, "Mean_dis") 
jx_seas_OCR$riv <- "OCR"

jx_seas_All <- rbind( jx_seas_OCR,jx_seas_SJB, jx_seas_SJJ, jx_seas_SMR)

#takes a long time
tic()
JX_cat_env <- join_seas_streamflow(JX_cat_env, jx_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works


#merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(jx_PZ, jx_maxT, jx_minT)

JX_cat_env2 <- join_seasCD(JX_cat_env, seasonal_CD)

# merge seasonal rainfall #### 
jx_seas_rf <- clean_seasRF(jx_rf)
JX_cat_env3 <- join_seasRF(JX_cat_env2, jx_seas_rf)

#merge seasonal ALL streamflow ####
colnames(jx_OCR) <- c("agency", "site_no", "datetime", "value", "code")


colnames(jx_SJB) <- c("agency", "site_no", "datetime", "value", "code")
jx_SJB$datetime <- mdy(jx_SJB$datetime)
colnames(jx_SJJ) <- c("agency", "site_no", "datetime", "value", "code")
jx_SJJ$datetime <- mdy(jx_SJJ$datetime)
colnames(jx_SMR) <- c("agency", "site_no", "datetime", "value", "code")
jx_SMR$datetime <- mdy(jx_SMR$datetime)

all_streams <- rbind(jx_OCR, jx_SJB,jx_SJJ,jx_SMR)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

JX_cat_env4 <- join_seas_streamALL(JX_cat_env3, seas_ALLsf)

# # HERE TO DO merge seasonal salinity and water temp ####
# selected_stations <- c("141", "177", "179", "CHV011", "159" )
# #which 
# 
# spawn_SAWT <- clean_seas_sal_wt(jx_sal_select,jx_wt_select,selected_stations, "monthly")
# seas_SAWT  <- clean_seas_sal_wt(jx_sal_select, jx_wt_select, selected_stations, "seasonal")
# 
# #replace NAs in spawn_month
# JX_cat_env4 <- JX_cat_env4 %>% replace_na(list(spawn_month=9999))
# 
# JX_cat_env5 <- join_spawn_SAWT(JX_cat_env4, spawn_SAWT)
# JX_cat_env6 <- join_seas_SAWT(JX_cat_env5, seas_SAWT)

# #TO DO merge seasonal nitro ####
# seas_nitro <- clean_seas_nitro(jx_nit_select, selected_stations)
# JX_cat_env7 = join_spawn_nitro(JX_cat_env6, seas_nitro)

#trim and output ####
write.csv(JX_cat_env4, paste(out, "Seatrout_ENV_Chapter2/JX_all_env_with_lag.csv", sep="/"))
















# ________________####
# BUILD OUT - CK####
# _______________ ####
#import catch ####
ck = subset(read_sas("ck_yoy_cn_c.sas7bdat"), month %in% c(5,6,7,8,9,10,11))
ck_hyd <- subset(read_sas("ck_yoy_cn_hyd.sas7bdat")) 
ck_hyd <- ck_hyd[!duplicated(ck_hyd$Reference),]
ck_phys <- read_sas(paste(phys_dat, "ckm_physical.sas7bdat", sep="/")) %>% dplyr::select(Reference, Secchi_on_bottom, Secchi_depth)
ck_phys$Reference <- as.character(ck_phys$Reference)

ck <- left_join(ck, ck_hyd, by="Reference") 
ck <- left_join(ck, ck_phys, by="Reference")
ck <- ck %>% select(noquote(order(colnames(ck))))  #reorders the columns alphabetically 

# fill missing lat/long ####
#Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 
unique(ck$Zone)
unique(subset(ck, is.na(Latitude))$Zone) #assume if its missing Long then its also missing Lat

#No missing lats and longs
CK_cat <- ck %>% mutate(NewLong = Longitude, NewLat = Latitude)

#import length data ####
ck_length = subset(read_sas("ck_yoy_cn_l.sas7bdat"))
ck_length = subset(ck_length, bio_reference %in% unique(CK_cat$bio_reference))

ck_length_exp <- ck_length[rep(row.names(ck_length), ck_length$COUNT), 1:3]
ck_length_ag <- aggregate(sl ~ bio_reference, data=ck_length_exp, FUN= "median") 
colnames(ck_length_ag)[2] <- "median_sl"

#assign median standard length to the catch data 
#assign age based on age length equation in mcmichael and peters 1989

CK_cat <- left_join(CK_cat, ck_length_ag, by="bio_reference")
CK_cat$approx_age_mo <- round((2.476*CK_cat$median_sl - 0.012*(CK_cat$median_sl)^2)*0.0328767)
CK_cat$spawn_month <- CK_cat$month - CK_cat$approx_age_mo

#filter these lengths that don't make sense
CK_cat <- subset(CK_cat, !spawn_month == 2 | is.na(spawn_month)) # to retain NA values

#tidy catch ####
CK_red <- tidy_catch(CK_cat)

# import environment #### 
#add in air temp
ck_maxT <- read.csv(paste(enviro_data, "AirTemp/Max_Temp_CD2.csv", sep="/"), skip=4)
ck_minT <- read.csv(paste(enviro_data, "AirTemp/Min_Temp_CD2.csv", sep="/"), skip=4)

#add in Cedar key station data that contains nitrogen, salinity, water temp 
station1 <- read.csv(paste(enviro_data, "station_levy.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
station2 <- read.csv(paste(enviro_data, "station_dixie.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
stations <- rbind(station1, station2)

result1 <- read.csv(paste(enviro_data, "result_levy.csv", sep="/")) 
result2 <- read.csv(paste(enviro_data, "result_dixie.csv", sep="/")) 
results <- rbind(result1, result2)

ck_data <- left_join(stations, results, by="MonitoringLocationIdentifier")

#select salinity
ck_sal <- ck_data[ck_data$CharacteristicName == "Salinity",]
ck_sal$ResultMeasureValue <- as.numeric(as.character(ck_sal$ResultMeasureValue))
hist(ck_sal$ResultMeasureValue)
ck_sal <- ck_sal %>% dplyr::select(ActivityStartDate, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier) %>%
            dplyr::mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE)) %>% dplyr::arrange(Year, Month) %>%
            dplyr::select(Year, Month, LatitudeMeasure, LongitudeMeasure, ResultMeasureValue)

#select water temp
ck_wt <- ck_data[ck_data$CharacteristicName == "Temperature, water",]
ck_wt$ResultMeasureValue <- as.numeric(as.character(ck_wt$ResultMeasureValue))
hist(ck_wt$ResultMeasureValue)
ck_wt <- ck_wt %>% select(ActivityStartDate, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier) %>%
  mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE)) %>% arrange(Year, Month) %>%
  select(Year, Month, LatitudeMeasure, LongitudeMeasure, ResultMeasureValue)


#select nitrogen
#select any kind of nitrogen to see what's available and then select the ones we want
ck_nit <- ck_data[str_detect(ck_data$CharacteristicName, c("Am","Nitr")),]
ck_nit <- ck_data[ck_data$CharacteristicName %in% c("Ammonia", "Nitrate"),]
#looks like there are no (n=19) nitrogen observations

# add in Palmer Z
ck_PZ <- read.csv(paste(enviro_data,"PalmerZ/PalmerZ_CD2.csv", sep="/" ),skip=3)

#add in rainfall
ck_rf1 <- read.csv(paste(enviro_data, "Rainfall/CK_Rainfall_89_97.csv", sep="/"))
ck_rf2 <- read.csv(paste(enviro_data, "Rainfall/CK_Rainfall_98_07.csv", sep="/"))
ck_rf3 <- read.csv(paste(enviro_data, "Rainfall/CK_Rainfall_08_17.csv", sep="/"))
ck_rf <- rbind(ck_rf1, ck_rf2, ck_rf3)
range(ck_rf$PRCP, na.rm=T)

#add in streamflow
ck_SR <- read.csv(paste(enviro_data, "Streamflow/CK/Suwannee_River.csv", sep="/"), skip=28)
ck_SR$X26706_72137_00003 <- as.numeric(as.character(ck_SR$X26706_72137_00003)) #tidally filtered
hist(ck_SR$X26706_72137_00003)
ck_SR <- ck_SR[,c(1:3,6,7)]

ck_WR <- read.csv(paste(enviro_data, "Streamflow/CK/Waccasassa_River.csv", sep="/"), skip=28)
ck_WR$X26352_00060_00003 <- as.numeric(as.character(ck_WR$X26352_00060_00003))
hist(ck_WR$X26352_00060_00003)
range(ck_WR$X26352_00060_00003, na.rm=T)
ck_WR$X26352_00060_00003[ck_WR$X26352_00060_00003 <0] <- 0


# join nitrogen, salinity, water temp ####
#DIN
# tic()
# nit_full <- joinEnv_IRJX(CK_red, jx_nit_select, 0.0432, 0.0432, nitrogen)
# toc()
# write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/JX_nit_join_043_DIN.csv", sep="/"))

#Sal
tic()
sal_full <- joinEnv_CK(CK_red, ck_sal, 0.0432, 0.0432, salinity) 
toc()
write.csv(sal_full, paste(out, "Seatrout_ENV_Chapter2/CK_sal_join_043.csv", sep="/")) 

#wat temp
tic()
wt_full <- joinEnv_CK(CK_red, ck_wt, 0.0432, 0.0432, waterTemp) 
toc()
write.csv(wt_full, paste(out, "Seatrout_ENV_Chapter2/CK_wt_join_043.csv", sep="/")) 

# TO DO - merge closest river mouth ####
SR_mouth = c(-83.1702, 29.2968) #, "CC") 
WR_mouth = c(-82.8159, 29.1606) #, "CC") 

riv_name= c("SR", "WR")

rivers = data.frame(rbind(SR_mouth, WR_mouth))
rivers = cbind(rivers, riv_name)
rivers$riv_name <- as.character(rivers$riv_name)

CK_cat$closest_riv <- ""
CK_cat <- closestRiver(CK_cat, rivers)

# TO DO merge riverflow ####
ck_av_SR <- cleanSF(ck_SR, "SR") #mean discharge in cubic feet/second
ck_av_WR <- cleanSF(ck_WR, "WR") #mean discharge in cubic feet/second

streamfl <- rbind(ck_av_SR, ck_av_WR)
streamfl <- subset(streamfl, month >= min(unique(CK_cat$month)) & year>= min(unique(CK_cat$year)))

CK_cat$riv_flow <- 1
CK_cat <- join_riverflow(CK_cat, streamfl)

# TO DO - merge nitrogen, phos, salinity, water temp ####

#DIN
#JX_nit <- read.csv(paste(out, "Seatrout_ENV_Chapter2/JX_nit_join_043_DIN.csv", sep="/"), header=T) %>% select(V3, V4) %>% subset(!duplicated(V3))
#colnames(AP_nit) <- c("Reference", "Nit_val")

CK_sal <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CK_sal_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(CK_sal) <- c("Reference", "Sal_val")

CK_wat <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CK_wt_join_043.csv", sep="/"), header=T) %>% select(V3, V4)%>% subset(!duplicated(V3))
colnames(CK_wat) <- c("Reference", "WatTemp_val")

#join back to original catch dataset- not the reduced one. 
#JX_new2 <-  left_join(JX_cat, JX_nit, by="Reference")
#JX_new2$Phos_val <- "NA"
#JX_new3 <-  left_join(JX_new2, JX_sal, by="Reference")
#JX_new4 <-  left_join(JX_new3, JX_wat, by="Reference")

CK_cat$Nit <- NA
CK_cat$Phos <- NA
CK_new2 <- CK_cat
CK_new3 <-  left_join(CK_new2, CK_sal, by="Reference")
CK_new4 <-  left_join(CK_new3, CK_wat, by="Reference")

# TO DO merge airtemp and palmerZ (climate zones-CD) ####
CK_new5 <- joinCD(CK_new4, ck_PZ,ck_maxT,ck_minT)

#TO DO - merge rainfall ####
ck_tot_rf <- cleanRF(ck_rf, "TotalMonthlyRF")

CK_new6 <- left_join(CK_new5, ck_tot_rf, by=c("year", "month"))

# TO DO- produce attenuation coefficient ####
# Only want to do this for Secchi_on_bottom = NO
CK_new6 <- CK_new6 %>% mutate(ext_ceof = ifelse(Secchi_on_bottom =="NO", 1.7/(Secchi_depth), NA))

#TO DO - output ####
write.csv(CK_new6, paste(out, "Seatrout_ENV_Chapter2/CK_all_env_no_lag.csv", sep="/"))

#.####
#Lag Variable Calculations ####
#.####

CK_cat_env <- read.csv(paste(out, "Seatrout_ENV_Chapter2/CK_all_env_no_lag.csv",sep="/"), header=T, row.names=1)
CK_cat_env$closest_riv <- as.character(CK_cat_env$closest_riv)

#TO DO merge seasonal streamflow ####
#jx_av_      <- cleanSF()
#jx_av_SJB <- cleanSF(jx_SJB, "SJB") #mean discharge in cubic feet/second
#jx_av_SSJ <- cleanSF(jx_SJJ, "SSJ") #mean discharge in cubic feet/second
#jx_av_SMR <- cleanSF(jx_SMR, "SMR") #mean discharge in cubic feet/second


ck_seas_SR <- cleanSF_withSeason(ck_SR, "Mean_dis") #mean disjxarge in cubic feet/second
ck_seas_SR$riv <- "SR"
ck_seas_WR <- cleanSF_withSeason(ck_WR, "Mean_dis") #mean disjxarge in cubic feet/second
ck_seas_WR$riv <- "WR"

ck_seas_All <- rbind(ck_seas_SR, ck_seas_WR)

#takes a long time
tic()
CK_cat_env <- join_seas_streamflow(CK_cat_env, ck_seas_All)
toc()
#select some rows at random to do by-hand checks to make sure this works


# TO DO merge seasonal_CD airtemp and palmerZ ####  

seasonal_CD <- clean_seasCD(ck_PZ, ck_maxT, ck_minT)

CK_cat_env2 <- join_seasCD(CK_cat_env, seasonal_CD)

# TO DO merge seasonal rainfall #### 
ck_seas_rf <- clean_seasRF(ck_rf)
CK_cat_env3 <- join_seasRF(CK_cat_env2, ck_seas_rf)

#TO DO merge seasonal ALL streamflow ####
colnames(ck_SR) <- c("agency", "site_no", "datetime", "value", "code")
colnames(ck_WR) <- c("agency", "site_no", "datetime", "value", "code")

all_streams <- rbind(ck_SR,ck_WR)

seas_ALLsf <- cleanSF_withSeason(all_streams, "Mean_dis_ALL_sf")
#test <- TB_cat_env3[1:10,]

CK_cat_env4 <- join_seas_streamALL(CK_cat_env3, seas_ALLsf)

# HERE TO DO merge seasonal salinity and water temp ####
selected_stations <- c("141", "177", "179", "CHV011", "159" )
#which 

#spawn_SAWT <- clean_seas_sal_wt(jx_sal_select,jx_wt_select,selected_stations, "monthly")
#seas_SAWT  <- clean_seas_sal_wt(jx_sal_select, jx_wt_select, selected_stations, "seasonal")

#replace NAs in spawn_month
#CK_cat_env4 <- CK_cat_env4 %>% replace_na(list(spawn_month=9999))

#JX_cat_env5 <- join_spawn_SAWT(JX_cat_env4, spawn_SAWT)
#JX_cat_env6 <- join_seas_SAWT(JX_cat_env5, seas_SAWT)

#TO DO merge seasonal nitro ####
#seas_nitro <- clean_seas_nitro(jx_nit_select, selected_stations)
#JX_cat_env7 = join_spawn_nitro(JX_cat_env6, seas_nitro)

#TO DO  trim and output ####
write.csv(CK_cat_env4, paste(out, "Seatrout_ENV_Chapter2/CK_all_env_with_lag.csv", sep="/"))

























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







