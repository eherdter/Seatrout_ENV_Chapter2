
rm(list=ls())
personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
work_comp= "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
nutrient_dat = "U:/PhD_projectfiles/Raw_Data/Environmental_Data/Nutrients/Nitrogen"
#nutrient_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData/Nutrients"
setwd(work_comp)
# ## Load Packages 
# * haven-to load sas7bdat daat
# * dplyr- to do df manipulation
# * geosphere-  closest lat/long neighbors
# * cluster- to determine medoids

library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
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
tb_nit1 <- read.csv(paste(nutrient_dat, "Nitrogen_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_nit2 <- read.csv(paste(nutrient_dat, "Nitrogen_Middle_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit3 <- read.csv(paste(nutrient_dat, "Nitrogen_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit <- rbind(tb_nit1, tb_nit2, tb_nit3)

#Get Year and Month in the right format to match to the TB_main dataframe. 

tb_nit$SampleDate <- as.factor(tb_nit$SampleDate)
tb_nit <- tb_nit %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
tb_nit$Date <- as.character(tb_nit$Date)

tb_nit <- tb_nit %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Characteristic %in% c("Nitrogen")) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID)
tb_nit$Month <- as.numeric(tb_nit$Month)
#tb_nit$Year <- as.factor(tb_nit$Year)

#Trim some of the nitrogen data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
main_Year <- unique(TB_main$year)
tb_nit = subset(tb_nit, Month <= max(unique(TB_main$month)) & Year %in% main_Year)


#BUILD THE JOIN FUNCTION ####

# Define fuzzy lat/long boundaries 
# fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile
# fuzzy_long = 0.01 # 1 mile 

joinEV <- function(catch, env, fuzzy_lat, fuzzy_long) {
  
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
    selection = do.call(rbind, selected) #do.call bind
    
  }
  selection
  
}

#TEST FUNCTION ####
#with a shortened dataframe and different fuzzy coordinate boundaries

TB_shrt=data.frame(TB_main[1:200,])
TB_shrt2 = data.frame(TB_main[1:500,])

# joinEV(TB_shrt, tb_nit, 0.01, 0.01) # 18 matches 
# joinEV(TB_shrt, tb_nit, 0.015, 0.015) # 111 matches
# joinEV(TB_shrt, tb_nit, 0.017, 0.017) # 111 matches

# joinEV(TB_shrt2, tb_nit, 0.01, 0.01) # 42 matches 
# joinEV(TB_shrt, tb_nit, 0.015, 0.015) # ~ 277.5
# joinEV(TB_shrt, tb_nit, 0.017, 0.017) # 























# WORKS ####


fuzzy_lat = 0.01 # 0.007 = 0.5 miles, 0.0144 = 1 mile
fuzzy_long = 0.01 # 1 mile 

#selected <- NULL
nRow=nrow(TB_main)
selected <- as.list(seq_len(nRow))
#selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))

# Start For Loop 
for(i in 1:nrow(TB_main))
{
  # re-initialize this data structs for new pass
  #match_matrix <- as.list(seq_len(140))
  match_matrix <- as.data.frame(matrix(data=NA, nrow=100, ncol=7))
  hit_counter = 1 #initialize the hit counter that will be used in the below loop instead of indexing based off of j
  
  #define some variables
  filler_dist = 9999
  catch_year=TB_main[i,59]
  catch_month=TB_main[i,30]
  catch_lat=TB_main[i,63]
  catch_long=TB_main[i, 62]
  catch_ref = TB_main[i,41]
  TB_cor = as.numeric(c(TB_main[i,62], TB_main[i,63]))
  
  for(j in 1:nrow(tb_nit))
  {
    #define some more variables
    nit_year=tb_nit[j,7]
    nit_month=tb_nit[j,8]
    nit_lat =tb_nit[j,1]
    nit_long = tb_nit[j,2]
    nit_val = tb_nit[j,6]
    
   if (is.na(catch_year) | is.na(catch_month) | is.na(catch_lat) | is.na(catch_long))
    {
      print("NA Found")
    }
    else if((catch_year==nit_year)&(catch_month==nit_month))
    {
      #defining box to place around each coordinate pair assocaited with catch
      # this reduces the amount of coordinate points that are selected from within tb_nit that are then used in a pairwise distance comparison
      
      fuzzymax_lat = catch_lat +fuzzy_lat #TB_main_sub$NewLat
      fuzzymin_lat = catch_lat -fuzzy_lat
      fuzzymax_long = catch_long + fuzzy_long #TBmain$NewLong
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
  selection = do.call(rbind, selected) #do.call bind

}
selection


 