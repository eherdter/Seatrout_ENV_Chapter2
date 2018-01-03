# ---
#   title: "Join Enviro Variables for Tampa Bay"
# output: html_document
# ---
#   
# #   ```{r setup, include=FALSE}
# # require ("knitr")
# # knitr::opts_chunk$set(echo = TRUE)
# # opts_knit$set(root.dir ="~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7")
# # ```
# 
# # ## About
# # 
# # This is an R Markdown document. 
# # This document details how environmental data not available within the FIM dataset are appended to the FIM catch data set. 
# # 
# # ## Set File Locations ###
# # 
# # Can't use setwd for knitr so one is chosen as the root directory in the set up chunk and then if I have to reference other places then I will use other locations defined below. 
# # 

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

#```{r}
library(haven) 
library(dplyr) 
library(geosphere)
library(cluster)
#```
####TB
#jEV <- function() 
#{
tb = subset(read_sas("tb_yoy_cn_c.sas7bdat"), month %in% c(4,5,6,7,8,9,10)) 
#tb_phys <- read_sas("~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data/tbm_physical.sas7bdat") %>% select(Reference, Secchi_on_bottom, Secchi_depth)
#tb_phys <- read_sas(paste(phys_dat, "tbm_physical.sas7bdat", sep="/")) %>% select(Reference, Secchi_on_bottom, Secchi_depth)
#tb_phys$Reference <- as.character(tb_phys$Reference)
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

#Apply zone-specific medoids to missing Long and Lat. Do this with new varaibles NewLong and NewLat 

TB_main <- tb %>% mutate(NewLong = ifelse(Zone == "A" & is.na(Longitude), tb_medA[1,],  ifelse(Zone=="C" & is.na(Longitude), tb_medC[1,], ifelse(Zone == "D" & is.na(Longitude), tb_medD[1,], ifelse(Zone=="E" & is.na(Longitude), tb_medE[1,], ifelse(Zone == "M" & is.na(Longitude), tb_medM[1,], tb$Longitude))))), NewLat = ifelse(Zone == "A" & is.na(Latitude), tb_medA[2,],  ifelse(Zone=="C" & is.na(Latitude), tb_medC[2,], ifelse(Zone == "D" & is.na(Latitude), tb_medD[2,], ifelse(Zone=="E" & is.na(Latitude), tb_medE[2,], ifelse(Zone == "M" & is.na(Latitude), tb_medM[2,], tb$Latitude))))))
TB_main <- TB_main %>% mutate(year = substr(as.character(TB_main$year),3,4)) #make year format match that of the coming nitrogen data

TB_main$year <- as.numeric(TB_main$year)
TB_main$month <- as.numeric(TB_main$month)
TB_main <- data.frame(TB_main)

sum(is.na(TB_main$NewLong))                         
sum(is.na(TB_main$NewLat))   

### Add in Enviro Data 
### Nitrogen

tb_nit1 <- read.csv(paste(nutrient_dat, "Nitrogen_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_nit2 <- read.csv(paste(nutrient_dat, "Nitrogen_Middle_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit3 <- read.csv(paste(nutrient_dat, "Nitrogen_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit <- rbind(tb_nit1, tb_nit2, tb_nit3)

#Need to match the nitrogen observation spatially and temporally with the catch observations (to create observations at month of haul- monthly present observations) AS WELL as creating an average nitrogen load prior to 'recruitment' timeframe (to create time lagged). 

#Get Year and Month in the right format to match to the TB_main dataframe. 

tb_nit$SampleDate <- as.factor(tb_nit$SampleDate)
tb_nit <- tb_nit %>% mutate(Date = as.Date(SampleDate, format = " %m/%d/%Y"))
tb_nit$Date <- as.character(tb_nit$Date)

tb_nit <- tb_nit %>% mutate(Year = substr(Date, 3,4), Month = substr(Date, 6,7)) %>% subset(Characteristic %in% c("Nitrogen")) %>% select(Actual_Latitude, Actual_Longitude, Characteristic,Parameter,Result_Unit, Result_Value, Year, Month, StationID)
tb_nit$Year <- as.numeric(tb_nit$Year)
tb_nit$Month <- as.numeric(tb_nit$Month)

tb_nit = data.frame(tb_nit)

#Match the nitrogen observation with catch observation based on year, month, and closest Lat and Long.Must have same year, same month, and closest Lat observation then append on nitrogen observation. Going to match lat and long by fuzzy matching and month and year exactly. 

#Starting with lat and long fuzzy matching because it's trickier and requires some determination of the fuzzy match. 


range(unique(tb_nit$Actual_Latitude)) #determine the total range of latitudes in the TB_nit
#27.5737 - 27.9904
range(unique(TB_main$NewLat)) #determine the total range of lat in the TB_main
# 27.466 -  28.03467

range(unique(tb_nit$Actual_Longitude)) #determine the total range of longs in the TB_nit
# -82.7441 - -82.4093
range(unique(TB_main$NewLong))
# -82.831 - -82.333

#Determine difference between minimum lats of tb_nit and TB_main and difference between maximum lats of tb_nit and TB_main to determine how far or fuzzy things could be.
#Do this same thing for longitude. 

min(unique(tb_nit$Actual_Latitude)) - min(unique(TB_main$NewLat)) #0.1068669
max(unique(tb_nit$Actual_Latitude)) - max(unique(TB_main$NewLat)) #0.0442667

min(unique(tb_nit$Actual_Longitude)) - min(unique(TB_main$NewLong)) #0.0868976
max(unique(tb_nit$Actual_Longitude)) - max(unique(TB_main$NewLong)) #0.07563469

#Fuzzy lat distance must be at least 0.1068669 or else the minimum lat observations from the TB_main will not be able to find a fuzzy match. In other words, a fuzzy match of less than that, say for the distance between maximum (0.044), will mean that the minimum lats will not find a match because they are separated by 0.1068669

#Fuzzy long distance must be at least 0.0868976 per above logic. 

#Run the loop using the defined fuzzy match.

#fuzzy_lat = max(min(unique(tb_nit$Actual_Latitude)) - min(unique(TB_main$NewLat)), max(unique(tb_nit$Actual_Latitude)) - max(unique(TB_main$NewLat)))
#fuzzy_long = max(min(unique(tb_nit$Actual_Longitude)) - min(unique(TB_main$NewLong)),max(unique(tb_nit$Actual_Longitude)) - max(unique(TB_main$NewLong)))

fuzzy_lat = 0.5
fuzzy_long = 0.5
#TB_main =data.frame(TB_main[1:10,])
TB_main = TB_main[1,]


selected <- NULL

for(i in 1:nrow(TB_main))
{
  # re-initialize this data structs for new pass
  match_matrix <- NULL
  #TBcomb <- data.frame(Month=numeric(),Year=numeric(),RefID=numeric(),Nitrogen=numeric(),Latitude=numeric(),Longitude=numeric())
  #hit_counter = 1
  
  #define some variables
  catch_year=TB_main[1,59]
  catch_month=TB_main[1,30]
  catch_lat=TB_main[1,63]
  catch_long=TB_main[1, 62]
  catch_ref = TB_main[1,41]
  TB_cor = as.numeric(c(TB_main[1,62], TB_main[1,63]))
  
  for(j in 1:nrow(tb_nit))
  {
    #define some variables
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
        
        # TBcomb[hit_counter,1] = catch_month
        # TBcomb[hit_counter,2] = catch_year
        # TBcomb[hit_counter,3] = catch_ref
        # TBcomb[hit_counter,4] = nit_val #result value
        # TBcomb[hit_counter, 5] = nit_lat
        # TBcomb[hit_counter, 6] = nit_long
        # hit_counter = hit_counter + 1
        
        m=NULL
        m[1] = catch_month
        m[2]= catch_year
        m[3]= catch_ref     
        m[4] = nit_val
        m[5] = nit_long
        m[6] = nit_lat
        m[7]= NA # placeholder for distance 
        
        match_matrix <- rbind(match_matrix,m)
        print(match_matrix)
        
        
      }
      mm_mat<- data.frame(matrix(unlist(match_matrix),nrow=nrow(match_matrix)))
      mm_mat$X5 = as.numeric(as.character(mm_mat$X5))
      mm_mat$X6 = as.numeric(as.character(mm_mat$X6))
      mm_mat[,7] = distm(mm_mat[,5:6], TB_cor)
      
      nit_station_match = NULL
      nit_station_match <- as.matrix(mm_mat[mm_mat$X7 == min(mm_mat$X7),])
      
    }

 }
  selected <- rbind(selected, nit_station_match)
}


  

  
  

  
  
  
  







 
  
  
  
  
  #TBComb is a temp data frame
  #for each row determine that rows distance from TbMain
  #Then take the closess and store it in a long term data frame (not the temp one)
  # then reset your temp data frame TBComb and hit_counter to 0
  #
  # distance_array=()
  # for i = 1 to hit_counter
  # {
  #
  # }
  
  
}


} #Close functionso

out=NULL
for (i in 1:2){
  a <- c(2, (i+10))
  b <- c((i+10), 5)
  c <- data.frame(a,b)
  out=rbind(out,c)
}
out