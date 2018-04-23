# functions for Join_Env_Vars_with_FIM.R


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
  zone1 <- droplevels(Zone.prop[Zone.prop$Freq > 0.04,])  
  sel_zone=unique(zone1$Zone)
  catch_main <- subset(catch,!duplicated(Reference) & Zone %in% sel_zone)
  catch_main
}

# build joinEV and joinNit function ####

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
  env$Year <- as.factor(env$Year)
  
  
  
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

joinCL <- function(catch, env, fuzzy_lat, fuzzy_long, var_name) {
  env <- clorA
  env <-  env %>% ungroup() %>% dplyr::select(-c(StationName, DAY))
  env$Month <- as.numeric(env$Month)
  env$Year <- as.numeric(env$Year)
  
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
    catch_long=catch[i,64]
    catch_ref = catch[i,41]
    TB_cor = as.numeric(c(catch[i,64], catch[i,65]))
    
    for(j in 1:nrow(env))
    {
      #define some more variables
      nit_year=env[j,4]
      nit_month=env[j,5]
      nit_lat =env[j,2]
      nit_long = env[j,3]
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



joinNit_AP <- function(catch, env, fuzzy_lat, fuzzy_long, var_name) {
  env <- as.data.frame(env)
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  env$Month <- as.character(as.numeric(env$Month))
  env <- env %>% ungroup() %>% mutate(Year = substr(Year, 3,4)) 
  
  main_Year <- unique(catch$year)
  env = as.data.frame(subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year))
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  catch$year <- as.character(catch$year)
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
      nit_year=env[j,2]
      nit_month=env[j,3]
      nit_lat =env[j,5]
      nit_long = env[j,6]
      nit_val = env[j,4]
      
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




joinSal_AP <- function(catch, env, fuzzy_lat, fuzzy_long, var_name) {
  env <- as.data.frame(env)
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  env$Month <- as.character(as.numeric(env$Month))
  env <- env %>% ungroup() %>% mutate(Year = substr(Year, 3,4)) 
  
  main_Year <- unique(catch$year)
  env = as.data.frame(subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year))
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  catch$year <- as.character(catch$year)  
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
      nit_year=env[j,1]
      nit_month=env[j,2]
      nit_lat =env[j,5]
      nit_long = env[j,6]
      nit_val = env[j,4]
      
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

joinEnv_IRJX <- function(catch, env, fuzzy_lat, fuzzy_long, var_name) {

  env <- as.data.frame(env)
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  env$Month <- as.numeric(env$Month)
  env <- env %>% ungroup() %>% mutate(Year = substr(Year, 3,4)) 
  
  main_Year <- unique(catch$year)
  env = as.data.frame(subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year))
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  catch$year <- as.character(catch$year)
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
      nit_year=env[j,2]
      nit_month=env[j,3]
      nit_lat =env[j,8]
      nit_long = env[j,9]
      nit_val = env[j,4]
      
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

joinEnv_CK <- function(catch, env, fuzzy_lat, fuzzy_long, var_name) {
  
  env <- as.data.frame(env)
  #Trim some of the data based on months (want to retain the earlier months) and years(only want the exact same years as in the catch dataset) 
  env$Month <- as.numeric(env$Month)
  env <- env %>% ungroup() %>% mutate(Year = substr(Year, 3,4)) 
  
  main_Year <- unique(catch$year)
  env = as.data.frame(subset(env, Month <= max(unique(catch$month)) & Year %in% main_Year))
  
  #selected <- NULL
  nRow=nrow(catch)
  selected <- as.list(seq_len(nRow))
  #selected <- as.data.frame(matrix(data=NA, nrow=15000, ncol=7))
  catch$year <- as.character(catch$year)
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
      nit_year=env[j,1]
      nit_month=env[j,2]
      nit_lat =env[j,3]
      nit_long = env[j,4]
      nit_val = env[j,5]
      
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

#build joinCD function ####

#where env = the CD datasets (palmerZ, maxT and minT) and catch is the _main 
joinCD <- function(catch,env,env2, env3){ 
  env <- env %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(Z_val=Value, Z_anom = Anomaly)
  env$month <- as.numeric(env$month)
  env$year <- as.numeric(env$year)
  
  env2 <- env2 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(MaxT_val =Value, MaxT_anom = Anomaly)
  env2$month <- as.numeric(env2$month)
  env2$year <- as.numeric(env2$year)
  
  env3 <- env3 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(MinT_val=Value, MinT_anom = Anomaly)
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

join_riverflow = function(catch, streamfl){
  
  for (i in 1:nrow(catch)){
    cat_month = catch[i,30]
    cat_year = catch[i,61] 
    cat_riv = catch[i,69] 
    
    
    for (j in 1:nrow(streamfl)){
      riv_year = streamfl[j,1]
      riv_month = streamfl[j,2]
      riv_dis = streamfl[j,3]
      riv_name = streamfl[j,4]
      
      if((cat_riv==riv_name) & (cat_year == riv_year) & (cat_month == riv_month)){
        catch[i,70] <- riv_dis
        
      } 
      
    }
    
  }
  catch
}


#build clean rainfall function ####
cleanRF <- function(rf, name) {
  rf <- rf
  rf$DATE <- as.character(rf$DATE)
  rf <- rf %>% dplyr::mutate(year = substr(DATE,1,4), month= substr(DATE, 6,7)) %>%  dplyr::select(year, month, PRCP)
  rf$PRCP <- as.numeric(as.character(rf$PRCP))
  rf$month <- as.numeric(rf$month)
  rf$year <- as.numeric(rf$year)
  colnames(rf) <- c("year", "month", name)
  rf
}

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
  
    #assign spring dis to all months after entire spring season  
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
    
    #assign winter flow to all months after entire winter season
    #if (cat_month >=3 & cat_month <=5) {  <- this was for closest months to winter
    if  (cat_month >= 3) {
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
    
    #assign previous years autumn to all months after entire season
    #if (cat_month <=5) { <- #assign previous years autumn to early early months
    if(cat_month>=3){
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
  
  env <- env %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(Z_val=Value, Z_anom = Anomaly) %>% select(-Date)
  env$month <- as.numeric(env$month)
  env$year <- as.numeric(env$year)
  
  env2 <- env2 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(MaxT_val =Value, MaxT_anom = Anomaly) %>% select(-Date)
  env2$month <- as.numeric(env2$month)
  env2$year <- as.numeric(env2$year)
  
  env3 <- env3 %>% mutate(year = substr(Date, 1, 4), month= substr(Date,5,6)) %>% dplyr::rename(MinT_val =Value, MinT_anom = Anomaly)%>% select(-Date)
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
    #assign spring CD to all months after entire spring season 
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
    
    #assign winter flow to all months after entire winter season
    #if (cat_month >=3 & cat_month <=5) { #assign winter flow to closer months that might be affected
    
     if (cat_month >= 3) {
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
    #assign previous years autumn to all months after entire season
    #if (cat_month <=5) {#assign previous years autumn to early early months 
     if (cat_month >=3) { 
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
  rf$DATE <- as.character(rf$DATE)
  rf <- rf %>% dplyr::mutate(year = substr(DATE,1,4), month= substr(DATE, 6,7)) %>%  dplyr::select(year, month, PRCP)
  rf$season <- ifelse(rf$month %in% c("03","04","05"), "spring", ifelse(rf$month %in% c("06","07","08","09"), "summer", ifelse(rf$month %in% c("10","11", "12"), "autumn", ifelse(rf$month %in% c("01","02"), "winter", "NA"))))
  rf <- rf %>% group_by(year, season) %>% dplyr::summarize(Seas_precip = mean(PRCP)) 
  rf$year <- as.numeric(rf$year)
  colnames(rf) <- c("year", "season", "total_rf")
  rf
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
    
    #assign spring flow to all months after entire spring season 
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
    
    #assign winter to flow to all months after entire winter season
    #if (cat_month >=3 & cat_month <=5) { #assign winter flow to closer months that might be affected
     if (cat_month >=3) {
      for (j in 1:nrow(seasonal_RF)){
        RF_year = seasonal_RF[j,1]
        RF_seas = seasonal_RF[j,2]
        RF_precip = seasonal_RF[j,3]
        
        if((cat_year == RF_year) & (RF_seas == "winter")){
          catch[i,113] <- RF_precip
        }
      }
     }
    
    #assign previous years autumn to all months after entire season
    #if (cat_month <=5) { #assign previous years autumn to early early months
     if (cat_month >= 3) {
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
    
    #assign spring flow to all months after entire spring season
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
    #assign winter to flow to all months after entire winter season
    #if (cat_month >=3 & cat_month <=5) { #assign winter flow to closer months that might be affected
    if (cat_month >=3) { 
      
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
    
    #assign previous years autumn to all months after entire season
    #if (cat_month <=5) { #assign previous years autumn to early early months
    if (cat_month >= 3) {
      
      
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

#selected_stations = c(93,92,24,22,95,91,96,90,25,23,21,28,19,16,84,81,9)
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



AP_clean_seas_sal_wt <- function(env, env2,  selected_stations, flag){
  
    #do some selection/cleaning on the enviro data based on the catch data to thin the enviro set out
    
    env$StationCode <- trimws(env$StationCode, "right")
    env2$StationCode <- trimws(env2$StationCode, "right")
    
    env = env[env$StationCode %in% selected_stations,] 
    env$Month <- as.numeric(env$Month)
    
    env2 = env2 %>% subset(StationCode %in% selected_stations)
    env2$Month <- as.numeric(env2$Month)
    
    if (flag == "seasonal") {   
      join <- left_join(env, env2, by=c("Year", "Month", "StationCode"))
      join$season <- ifelse(join$Month %in% c("4","5"), "first", ifelse(join$Month %in% c("6","7"), "second", ifelse(join$Month %in% c("8", "9"), "third", "NA")))
      join<- join %>% group_by(Year, season) %>% summarize(Sal=mean(Sal), Temp=mean(Temp))
    }
    else if (flag == "monthly") {
      join <- left_join(env, env2, by=c("Year", "Month", "StationCode")) 
      join <- join %>% group_by(Year, Month) %>% summarize(Sal=mean(Sal), Temp=mean(Temp)) 
    
    }
    join$Year <- as.factor(join$Year)
    join
}


IRJX_clean_seas_sal_wt <- function(env, env2,  selected_stations, flag){
  
  #do some selection/cleaning on the enviro data based on the catch data to thin the enviro set out
  
  env = env[env$STN_NAME %in% selected_stations,] 
  env$Month <- as.numeric(env$Month)
  
  env2 = env2[env2$STN_NAME %in% selected_stations,] 
  env2$Month <- as.numeric(env2$Month)
  
  if (flag == "seasonal") {   
    join <- left_join(env, env2, by=c("Year", "Month", "STN_NAME"))
    join$season <- ifelse(join$Month %in% c("4","5"), "first", ifelse(join$Month %in% c("6","7"), "second", ifelse(join$Month %in% c("8", "9"), "third", "NA")))
    join<- join %>% dplyr::group_by(Year, season) %>% dplyr::summarize(Sal=mean(Sal), Temp=mean(Temp))
  }
  else if (flag == "monthly") {
    join <- left_join(env, env2, by=c("Year", "Month", "STN_NAME")) 
    join <- join %>% dplyr::group_by(Year, Month) %>% dplyr::summarize(Sal=mean(Sal), Temp=mean(Temp)) 
    
  }
  join$Year <- as.factor(join$Year)
  join
}


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

clean_seas_nitro <- function(env, selected_stations){
  
  env = env %>% subset(StationID %in% selected_stations)
  env$SampleDate <- as.factor(env$SampleDate)
  env <- droplevels( env %>% mutate(year = substr(SampleDate, 1,4), month = substr(SampleDate, 6,7)) %>% subset %>% dplyr::select( DIN, year, month)) 
  env$month <- as.numeric(env$month)
  
  nit_ag <- aggregate(DIN ~ year + month, FUN= "mean", data=env)
  nit_ag$year <- as.numeric(nit_ag$year)
  nit_ag[order(nit_ag$year),]
}

AP_clean_seas_nitro <- function(env, selected_stations){
  
  env$Month <- as.numeric(env$Month)
  env$StationCode <- trimws(env$StationCode, "right")
   
  env = env %>% subset(StationCode %in% selected_stations)
  }


IRJX_clean_seas_nitro <- function(env, selected_stations){
  
  env$Month <- as.numeric(env$Month)
  env$STN_NAME <- trimws(env$STN_NAME, "right")
  
  env = env %>% subset(STN_NAME %in% selected_stations)
}





#build join seasonal nitro ####
join_spawn_nitro = function(catch, seas_nitro) {
  catch$atspawn_nitro <- NA #127
  catch$avg_last2_nitro <- NA #128
  catch$avg_last3_nitro <- NA #129
  spawn_1 <- as.data.frame(matrix(data=NA,nrow=nrow(catch),ncol=1))
  spawn_2 <- as.data.frame(matrix(data=NA,nrow=nrow(catch),ncol=1))
  spawn_3 <- as.data.frame(matrix(data=NA,nrow=nrow(catch),ncol=1))
  counter = 1
  
  for (i in 1:nrow(catch)){
    
    
    before1_spawn = catch[i,68]-1
    before2_spawn = catch[i,68]-2
    before3_spawn = catch[i,68]-3
    cat_year = catch[i,61] 
    
    
    for (j in 1:nrow(seas_nitro)){
      year = seas_nitro[j,1]
      month = seas_nitro[j,2]
      DIN = seas_nitro[j,3]
      
      if(cat_year == year) {
        if (before1_spawn == month) {
          catch[i,127] <- DIN
          spawn_1[i,] <- DIN
        }
        else if(before2_spawn == month) {
          spawn_2[i,] <- DIN
        }
        else if(before3_spawn == month) {
          spawn_3[i,] <- DIN
        }
      }
    }
    counter=counter+1
  }
  spawn_1
  spawn_2
  spawn_3
  
  
  catch[,128] <- rowMeans(cbind(spawn_1, spawn_2), na.rm=TRUE)
  catch[,129] <- rowMeans(cbind(spawn_1, spawn_2, spawn_3), na.rm=TRUE)
  catch
}

#merge rainfall CH ####
#Charlotte harbor Date rainfall datasheet is set up differently than all of the rest so I can't do this using the function-must do this manually 

# cleanRF_CH <- function(rf, name) {
#   rf <- rf %>% dplyr::mutate(Date = as.Date(DATE, format= "%Y-%m-%d"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  dplyr::select(year, month, STATION_NAME, HOURLYPrecip)
#   rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
#   tot_rf <- aggregate(HOURLYPrecip ~ year + month, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
#   tot_rf$month <- as.numeric(tot_rf$month)
#   tot_rf$year <- as.numeric(tot_rf$year)
#   #tb_tot_rf$month <- as.numeric(tb_tot_rf$month)
#   colnames(tot_rf) <- c("year", "month", name)
#   tot_rf
# }

#merge seas rainfall CH ####
# #by hand because column names are different for some reason
# 
# clean_seasRF_CH <- function(rf) {
#   rf <- rf
#   rf <- rf %>% mutate(Date = as.Date(DATE, format= "%Y-%m-%d"), year = substr(Date,1,4), month= substr(Date, 6,7)) %>%  select(year, month, HOURLYPrecip)
#   rf$HOURLYPrecip <- as.numeric(rf$HOURLYPrecip)
#   rf$season <- ifelse(rf$month %in% c("03","04","05"), "spring", ifelse(rf$month %in% c("06","07","08","09"), "summer", ifelse(rf$month %in% c("10","11", "12"), "autumn", ifelse(rf$month %in% c("01","02"), "winter", "NA"))))
#   tot_rf <- aggregate(HOURLYPrecip ~ year + season, FUN=mean, data=rf)%>% rename(Monthly_precip=HOURLYPrecip)
#   tot_rf$year <- as.numeric(tot_rf$year)
#   colnames(tot_rf) <- c("year", "season", "total_rf")
#   tot_rf
# }

