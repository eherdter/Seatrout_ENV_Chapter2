
rm(list=ls())
personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
work_comp= "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
nutrient_dat = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Nutrients/Nitrogen"
#nutrient_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Nutrients"
salinity = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Salinity"
watertemp = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/WaterTemp"
setwd(work_comp)
out =   "U:/PhD_projectfiles/Exported_R_Datafiles"

# LOAD PACKAGES ####
library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
library(tictoc)
#
# LOAD AND CLEAN DATA  ####
tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) 
tb_hyd <- subset(read_sas("tb_yoy_cn_hyd.sas7bdat")) 
tb_hyd <- tb_hyd[!duplicated(tb_hyd$Reference),]

tb <- left_join(tb, tb_hyd, by="Reference")
tb <- tb %>% select(noquote(order(colnames(tb))))  #reorders the columns alphabetically 

#Clean TB. Add in missing lat and long based on the Zone. I.e. if lat and long is missing then use lat and long for similar zone. This is necessary to be able to match any environmental data based on time and space. 

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

### ADD IN ENVIRO DATA ####
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

#add in salinity
tb_sal1 <- read.csv(paste(salinity, "Salinity_HillsboroughBay_EPCRoutine.csv", sep="/"))
tb_sal2 <- read.csv(paste(salinity, "Salinity_LowerTampaBay_EPCRoutine.csv", sep="/"))
tb_sal3 <- read.csv(paste(salinity, "Salinity_MiddleTampaBay_EPCRoutine.csv", sep="/"))
tb_sal4 <- read.csv(paste(salinity, "Salinity_OldTampaBay_EPCRoutine.csv", sep="/"))
tb_sal <- rbind(tb_sal1, tb_sal2, tb_sal3, tb_sal4)

#add in water temp
tb_wt1 <- read.csv(paste(watertemp, "Water_temperature_Hillsborough_Bay.csv", sep="/"))
tb_wt2 <- read.csv(paste(watertemp, "Water_temperature_Lower_Tampa_Bay.csv", sep="/"))
tb_wt3 <- read.csv(paste(watertemp, "Water_temperature_Middle_Tampa_Bay.csv", sep="/"))
tb_wt <- rbind(tb_wt1, tb_wt2, tb_wt3)


#BUILD THE JOIN FUNCTION ####

#provide the catch dataset, the environmental data set, the fuzzy lat/long, variable name, and parameter name (as a character)
#function works for TB and CH for enviro variables nitrogen, phosphorous and salinity

#Param Name for Nitrogen "TN_ugl"
#Param Name for Phosph   "TP_ugl"
#Param Name for Salinity "Salinity_ppt"
#Param Name for Water temp  "TempW_F"

# Define fuzzy lat/long boundaries 
# fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile, 0.0288 = 2 miles
# fuzzy_long = 0.01 # 1 mile 

# #do some selection/cleaning on the tb_nitiro data based on the TB_main data to thin the tb_nitiro set out
# tb_nit$SampleDate <- as.factor(tb_nit$SampleDate)
# tb_nit <- tb_nit %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
# tb_nit$Date <- as.character(tb_nit$Date)
# 
# tb_nit <- droplevels( tb_nit %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Parameter == Param_name) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID))
# tb_nit$Month <- as.numeric(tb_nit$Month)
# 
# #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the TB_main dataset) 
# main_Year <- unique(TB_main$year)
# tb_nit = subset(tb_nit, Month <= max(unique(TB_main$month)) & Year %in% main_Year)


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
# END FUNCTION #### 


#TEST FUNCTION ####
#with a shortened dataframe and different fuzzy coordinate boundaries

TB_shrt=data.frame(TB_main[1:10,])
TB_shrt2 = data.frame(TB_main[1:5000,])

# TB_shrt=data.frame(TB_main[1:10,])
# joinEV(TB_shrt, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") #0 matches 
# joinEV(TB_shrt, tb_nit, 0.015, 0.015, nitrogen, "TN_ugl") # 10 matches
# joinEV(TB_shrt, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") # 10 matches

# TB_shrt2 = data.frame(TB_main[1:50,])
# joinEV(TB_shrt2, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") # 3 matches, 1.61 seconds
# joinEV(TB_shrt2, tb_nit, 0.015, 0.015, nitrogen, "TN_ugl") # 20 matches, 1.75 seconds
# joinEV(TB_shrt2, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") # 23, 1.71 seconds
# joinEV(TB_shrt2, tb_nit, 0.0216, 0.0216, nitrogen, "TN_ugl") # 26, 1.71 seconds

# TB_shrt3 = data.frame(TB_main[1:200,])
# nit_shrt3 <- joinEV(TB_shrt3, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") # 18 matches, 9.8 seconds
# nit_shrt3 <- joinEV(TB_shrt3, tb_nit, 0.015, 0.015, nitrogen, "TN_ugl") # 44 matches, 10.31 seconds
# nit_shrt3 <- joinEV(TB_shrt3, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") # 59 matches, 10.23 seconds
# nit_shrt3 <- joinEV(TB_shrt3, tb_nit, 0.0216, 0.0216, nitrogen, "TN_ugl") # 71 matches, 10.23 seconds
# nit_shrt3 <- joinEV(TB_shrt3, tb_nit, 0.0288, 0.0288, nitrogen, "TN_ugl") # 101 matches, 10.23 seconds

# TB_shrt4 = data.frame(TB_main[1:500,])
# nit_shrt4 <- joinEV(TB_shrt4, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") # 42, 44.67 seconds
# nit_shrt4 <- joinEV(TB_shrt4, tb_nit, 0.015, 0.015, nitrogen, "TN_ugl") # 102 matches, 45.3 seconds
# nit_shrt4 <- joinEV(TB_shrt4, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") # 129, 45.94 seconds

# TB_shrt5 = data.frame(TB_main[1:2000,])
# joinEV(TB_shrt5, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") # 200, 496.6 seconds
# joinEV(TB_shrt5, tb_nit, 0.015, 0.015, nitrogen, "TN_ugl") # 169 matches, 45.3 seconds
# joinEV(TB_shrt5, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") # 181, 45.94 seconds


#TB_shrt5 = data.frame(TB_main[1:3200,]) #no years after 99 
#joinEV(TB_shrt5, tb_nit, 0.01, 0.01, nitrogen) #265 matches, 1049.66 seconds 

#TB_test =  data.frame(TB_main[3300:4000,]) # 99,00,01 years
#nit_test <- joinEV(TB_test, tb_nit, 0.01, 0.01, nitrogen, "TN_ugl") #72 matches, 73.1 seconds

#TB_test2 = data.frame(TB_main[4000:5000,]) # 01,02,03,04,05 #89 matches, 182.97 seconds

#Shortened datasets
# TB_beg=data.frame(TB_main[1:3000,])
# TB_mid = data.frame(TB_main[3001:6000,])
# TB_end = data.frame(TB_main[6001:8580,])
# 
# tic()
# beg <- joinEV(TB_beg, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") #614, 1015.15
# toc()
# 
# tic()
# mid<- joinEV(TB_mid, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") #690, 1239.75
# toc()
# 
# tic()
# end<- joinEV(TB_end, tb_nit, 0.017, 0.017, nitrogen, "TN_ugl") #715, 944.97
# toc()
# 
# nit_full <- rbind(beg, mid, end)
# #write.csv(nit_full, "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes/TB_nit_join.csv")
# write.csv(nit_full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join.csv", sep="/"))


#Full dataset
tic()
full <- joinEV(TB_main,tb_nit, 0.017, 0.017, nitrogen, "TN_ugl" ) #2019, 9610.85 seconds
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_017.csv", sep="/"))

tic()
full <- joinEV(TB_main,tb_nit, 0.0288, 0.0288, nitrogen, "TN_ugl" ) #3773, 10166.69
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_028.csv", sep="/")) 

tic()
full <- joinEV(TB_main,tb_nit, 0.0432, 0.0432, nitrogen, "TN_ugl" ) #4963, 10761.23
toc()
write.csv(full, paste(out, "Seatrout_ENV_Chapter2/TB_nit_join_043.csv", sep="/")) 


# # WORKS ####
# 
# 
# fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile
# fuzzy_long = 0.01 # 1 mile 
# 
# #selected <- NULL
# nRow=nrow(TB_main)
# selected <- as.list(seq_len(nRow))
# #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
# 
# # Start For Loop 
# for(i in 1:nrow(TB_main))
# {
#   # re-initialize this data structs for new pass
#   #match_matrix <- as.list(seq_len(140))
#   match_matrix <- as.data.frame(matrix(data=NA, nrow=100, ncol=7))
#   hit_counter = 1 #initialize the hit counter that will be used in the below loop instead of indexing based off of j
#   
#   #define some variables
#   filler_dist = 9999
#   catch_year=TB_main[i,59]
#   catch_month=TB_main[i,30]
#   catch_lat=TB_main[i,63]
#   catch_long=TB_main[i, 62]
#   catch_ref = TB_main[i,41]
#   TB_cor = as.numeric(c(TB_main[i,62], TB_main[i,63]))
#   
#   for(j in 1:nrow(tb_nit))
#   {
#     #define some more variables
#     nit_year=tb_nit[j,7]
#     nit_month=tb_nit[j,8]
#     nit_lat =tb_nit[j,1]
#     nit_long = tb_nit[j,2]
#     nit_val = tb_nit[j,6]
#     
#    if (is.na(catch_year) | is.na(catch_month) | is.na(catch_lat) | is.na(catch_long))
#     {
#       print("NA Found")
#     }
#     else if((catch_year==nit_year)&(catch_month==nit_month))
#     {
#       #defining box to place around each coordinate pair assocaited with catch
#       # this reduces the amount of coordinate points that are selected from within tb_nit that are then used in a pairwise distance comparison
#       
#       fuzzymax_lat = catch_lat +fuzzy_lat #TB_main_sub$NewLat
#       fuzzymin_lat = catch_lat -fuzzy_lat
#       fuzzymax_long = catch_long + fuzzy_long #TBmain$NewLong
#       fuzzymin_long = catch_long - fuzzy_long
#       
#       if((nit_lat > fuzzymin_lat) & (nit_lat < fuzzymax_lat) & (nit_long > fuzzymin_long) & (nit_long < fuzzymax_long)) 
#         #if the lat long of nitrogen falls within the fuzzy lat long boundaries of the catch lat long found above then it will add into the dataframe below 
#       {
#         
#         #m= as.data.frame(matrix(data=NA,nrow=1, ncol=7))
#         m= as.list(seq_len(7))
#         #m = data.frame(catch_month, catch_year, catch_ref, nit_val, nit_long, nit_lat, filler_dist)
#         m[1] = catch_month
#         m[2]= catch_year
#         m[3]= catch_ref     
#         m[4] = nit_val
#         m[5] = nit_long
#         m[6] = nit_lat
#         m[7]= filler_dist #placeholder for distance 
#         m_df <- data.frame(m, stringsAsFactors = FALSE)
#         #colnames(m_df) <- c("x1", "x2","X3", "X4", "X5", "X6", "X7")
#         
#         match_matrix[hit_counter,] <- m_df
#         hit_counter=hit_counter + 1
#         
#       }
#       
#     }
#     
#   }
#   match_matrix[,7] = distm(match_matrix[,5:6], TB_cor) 
#   match_matrix <- na.omit(match_matrix)
#   
#   nit_station_match = as.list(seq_len(1))
#   nit_station_match <- match_matrix[match_matrix$V7 == min(match_matrix[,7], na.rm=T),] #select the nitrogen val from station that is closest of all 
#   
#   selected[[i]] <- nit_station_match #nitrogen station match adds in to predefined seleciton
#   selection = do.call(rbind, selected) #do.call bind
# 
# }
# selection
# 
# library(tictoc)
# 
# tic()
# print("test")
# toc()


 