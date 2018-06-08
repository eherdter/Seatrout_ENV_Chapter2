# making maps of environmental data (specifically nutrients, water temp, salinity)
rm(list=ls())
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(raster)
library(gridExtra)
library(dplyr)

enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
enviro_data = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData"

#add in nitrogen
tb_nit1 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_nit2 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Middle_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit3 <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_nit <- rbind(tb_nit1, tb_nit2, tb_nit3)

tb_nit_min <- tb_nit %>% distinct(Actual_Latitude, Actual_Longitude, .keep_all=TRUE)
tb_nit_min$StationID <- as.factor(tb_nit_min$StationID)

tb_nit_nox <- tb_nit %>% subset(Parameter=="NOx_ugl")

tb_nit_nox$StationID <- as.factor(tb_nit_nox$StationID)


ch_nit <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/Nitrogen_CH.csv", sep="/"))
ch_nit_min <- ch_nit %>% distinct(Actual_Latitude, Actual_Longitude, .keep_all=TRUE)
ch_nit_min$StationID <- as.factor(ch_nit_min$StationID)

ch_nit_nox <- ch_nit %>% subset(Parameter=="NOx_ugl")




#add in phosphorous
tb_ph1 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Hillsborough_Bay_EPC_Routine.csv", sep="/"))
tb_ph2 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Lower_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph3 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Middle_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph4 <- read.csv(paste(enviro_data, "Nutrients/Phosphorous/Phosphorous_Old_Tampa_Bay_EPC_Routine.csv", sep="/"))
tb_ph <- rbind(tb_ph1, tb_ph2, tb_ph3, tb_ph4)

tb_ph_min <- tb_ph %>% distinct(Actual_Latitude, Actual_Longitude, .keep_all=TRUE)

#add in salinity
tb_sal1 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_HillsboroughBay_EPCRoutine.csv", sep="/"))
tb_sal2 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_LowerTampaBay_EPCRoutine.csv", sep="/"))
tb_sal3 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_MiddleTampaBay_EPCRoutine.csv", sep="/"))
tb_sal4 <- read.csv(paste(enviro_data, "Salinity/TB/Salinity_OldTampaBay_EPCRoutine.csv", sep="/"))
tb_sal <- rbind(tb_sal1, tb_sal2, tb_sal3, tb_sal4)
tb_sal$StationID <- as.factor(tb_sal$StationID)

tb_sal_min2 <- tb_sal %>% subset(StationID =="2")

tb_sal_min1 <- tb_sal1 %>% distinct(Actual_Latitude, Actual_Longitude, .keep_all=TRUE)

#Here- Basic plot####
map('state', xlim=c(-83, -81), y=c(27.5,29), col="gray90", fill=TRUE)
points(tb_nit_min$Actual_Longitude, tb_nit_min$Actual_Latitude, pch=1, col='black', cex=0.35)


florida <- map_data("state", "florida")

plot = ggplot() +
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=tb_nit_min, aes(x=Actual_Longitude, y=Actual_Latitude, label=StationID), size=0.5, color="grey20") +
  geom_text(data=tb_nit_min, aes(x=Actual_Longitude, y=Actual_Latitude, label=StationID))+#
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-83, -82), y=c(27.5,28.25)) 

#charlotte harbor
plot = ggplot() +
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=ch_nit_min, aes(x=Actual_Longitude, y=Actual_Latitude, label=StationID), size=0.5, color="grey20") +
  geom_text(data=ch_nit_min, aes(x=Actual_Longitude, y=Actual_Latitude, label=StationID))+#
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-82.5, -82), y=c(26,27)) 

plot = ggplot() +
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=tb_ph_min, aes(x=Actual_Longitude, y=Actual_Latitude), size=0.5, color="grey20") + #
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-83, -82), y=c(27.5,28.25))

plot = ggplot() +
  geom_polygon(data=florida, aes(x=long, y=lat, group=group),color="grey10", fill ="#fff7bc") + 
  geom_point(data=tb_sal_min2, aes(x=Actual_Longitude, y=Actual_Latitude), size=2, color="red") + #
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-83, -82), y=c(27.5,28.25))


# apalachicola 

ap_nit <- read.csv(paste(enviro_data, "Nutrients/AP/3522.csv", sep="/")) %>% dplyr::select(StationCode, DateTimeStamp, NH4F,F_NH4F, NO2F, F_NO2F, NO3F, F_NO3F)
ap_nit <- ap_nit %>% mutate(total_DIN = rowSums(ap_nit[,c(3,5,7)], na.rm=TRUE))
ap_nit <- ap_nit %>% mutate(DINug = total_DIN*1000) %>% select(-total_DIN) %>% rename(DIN=DINug)
ap_nit$DateTimeStamp <- as.factor(ap_nit$DateTimeStamp)
ap_nit <- ap_nit %>% mutate(Date = as.Date(DateTimeStamp, format = " %m/%d/%Y"))
ap_nit$Date <- as.character(ap_nit$Date)

ap_nit <- ap_nit %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
ap_nit <- ap_nit %>% distinct(StationCode, Year, Month, Day, DateTimeStamp, .keep_all=TRUE)
ap_nit_up <- ap_nit %>% group_by(StationCode, Year, Month) %>% summarize(DIN = mean(DIN, na.rm=T))

#add lat/long to the nit data set
unique(ap_nit$StationCode)
sampling_stations <- read.csv(paste(enviro_data,"Nutrients/AP/sampling_stations.csv", sep="/"), header=T) %>% select(Station.Code, Latitude, Longitude, Station.Name) %>% rename(StationCode=Station.Code)
ap_nit_up <- left_join(ap_nit_up, sampling_stations)
ap_nit_up$StationCode <- as.factor(ap_nit_up$StationCode)



#http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
ap_box <- make_bbox(lon=ap_nit_up$Longitude, lat=ap_nit_up$Latitude, f=0.5)
ap_map <- get_map(location = ap_box, maptype = "satellite", source = "google")

ggmap(ap_map)+ geom_point(data=ap_nit_up, aes(x=Longitude, y=Latitude, label=StationCode), size=0.5, color="grey20") +
  geom_text(data=ap_nit_up, aes(x=Longitude, y=Latitude, label=StationCode), color="white")#


#indian river lagoon
station1 <- read.csv(paste(enviro_data, "active and inactive WQ stations_LSJ and IRL.csv", sep="/")) %>% select(-STN_STTS_CD)
station2 <- read.csv(paste(enviro_data,"SJR_STN_ET_IRL and LSJ_active mainstem no IRLSG.csv", sep="/"))
station2 <- station2 %>% dplyr::rename(MJR_BSN_NM= Major.Basin, STN_ID=Station.ID, STN_NAME=Station.Name, PNT_LOC_DSC=Location.Description, LAT_NO=LAT..DDMMSS.S., LONG_NO=LONG..DDMMSS.S.) %>%
  dplyr::select(-c(Comments, All.surfacewater.stations))
stations <- rbind(station1, station2) %>% dplyr::distinct(STN_NAME, .keep_all=TRUE) 

stations[,c("LAT_NO","LONG_NO")] <- as.character(unlist(stations[,c("LAT_NO","LONG_NO")]))

stations <- stations %>% mutate(LatDeg = as.numeric(substr(LAT_NO,1,2)), 
                                LongDeg = as.numeric(substr(LONG_NO, 1,2)), 
                                LatMin = as.numeric(substr(LAT_NO,3,4)), 
                                LongMin= as.numeric(substr(LONG_NO,3,4)), 
                                LatSec = as.numeric(substr(LAT_NO, 5,6)), 
                                LongSec= as.numeric(substr(LONG_NO,5,6)), 
                                LAT = (LatDeg + (LatMin/60) + (LatSec/3600)),
                                LONG = -1*(LongDeg + (LongMin/60) + (LongSec/3600))) %>% select (-c(LAT_NO, LONG_NO, LatDeg, LongDeg, LatMin, LongMin, LatSec, LongSec))

library(lubridate)
ir_nit <- read.csv(paste(enviro_data, "Nutrients/Nitrogen/IR/IR_nit.csv", sep="/"), skip=2) 

ir_nit <- subset(ir_nit, Analyte == "NOx-D" & !(Qualifier.Code %in% c("T", "TQ", "I", "IQ", "Y", "YT","YIQ", "YQ", "YI", "Q", "YTQ")))
ir_nit <- ir_nit %>% mutate(Valueug = Value*1000) %>% dplyr::select(-Value) %>% dplyr::rename(Value=Valueug)
ir_nit <- ir_nit %>% mutate(DATE = mdy_hm(Date)) %>% mutate(Year = year(DATE), Month=month(DATE), Day = day(DATE))

hist(ir_nit$Value)
range(ir_nit$Value, na.rm=T)
mean(ir_nit$Value, na.rm=T)
ir_nit$Value[ir_nit$Value>=1500] <- NA
ir_nit$Value[ir_nit$Value<0] <- NA

ir_nit <- ir_nit %>% dplyr::distinct(Station, Year, Month, Day, .keep_all=TRUE)
ir_nit_up <- ir_nit %>% dplyr::group_by(Station, Year, Month) %>% dplyr::summarize(Value = mean(Value, na.rm=T)) %>% dplyr::rename(STN_NAME=Station)

#add lat/long to the nit data set
ir_nit_match <- dplyr::left_join(ir_nit_up, stations, by="STN_NAME")
ir_nit_select <- left_join(ir_nit_up, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))

ir_box <- make_bbox(lon=ir_nit_match$LONG, lat=ir_nit_match$LAT, f=0.5)
ir_map <- get_map(location = ir_box, maptype = "satellite", source = "google")

ggmap(ir_map)+ geom_point(data=ir_nit_match, aes(x=LONG, y=LAT, label=STN_NAME), size=0.5, color="grey20")  +
  geom_text(data=ir_nit_match, aes(x=LONG, y=LAT, label=STN_NAME), color="white")#




ir_sal <- read.csv(paste(enviro_data, "Salinity/IR/IR_sal.csv", sep="/"), skip=2) 
ir_sal <- ir_sal %>% dplyr::rename(STN_NAME = Station)
hist(ir_sal$Value)
ir_sal$Val[ir_sal$Val >40] <- NA

ir_sal <- ir_sal %>% mutate(Date = as.Date(Date, format = " %m/%d/%Y"))
ir_sal$Date <- as.character(ir_sal$Date)
ir_sal <- ir_sal %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
ir_sal <- ir_sal %>% distinct(STN_NAME, Year, Month, Day, .keep_all=TRUE)
ir_sal <- ir_sal %>% group_by(STN_NAME, Year, Month) %>% dplyr::summarize(Val = mean(Value, na.rm=T))

ir_sal_select <- left_join(ir_sal, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))


ir_box <- make_bbox(lon=ir_sal_select$LONG, lat=ir_sal_select$LAT, f=0.5)
ir_map <- get_map(location = ir_box, maptype = "satellite", source = "google")

ggmap(ir_map)+ geom_point(data=ir_sal_select, aes(x=LONG, y=LAT, label=STN_NAME), size=0.5, color="grey20")  +
  geom_text(data=ir_sal_select, aes(x=LONG, y=LAT, label=STN_NAME), color="white") +
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-80.6, -80.2), y=c(27.6,28.0))


# use identified spawning locations from Kupschus 
# fort pierce, Longpoint park, Northern Banana River, and Black point, Figure 

#jax
#add in salinity
jx_sal <- read.csv(paste(enviro_data, "Salinity/JX/JX_sal.csv", sep="/"), skip=2) 

jx_sal <- jx_sal %>% rename(STN_NAME = Station)
hist(jx_sal$Value)
jx_sal$Val[jx_sal$Val >40] <- NA

jx_sal <- jx_sal %>% mutate(Date = as.Date(Date, format = " %m/%d/%Y"))
jx_sal$Date <- as.character(jx_sal$Date)
jx_sal <- jx_sal %>% mutate(Year = substr(Date,1,4), Month=substr(Date,6,7), Day = substr(Date, 9,10))
jx_sal <- jx_sal %>% distinct(STN_NAME, Year, Month, Day, .keep_all=TRUE)
jx_sal <- jx_sal %>% group_by( STN_NAME, Year, Month) %>% summarize(Val = mean(Value, na.rm=T))

jx_sal_select <- left_join(jx_sal, stations, by="STN_NAME") %>% subset(!(is.na(STN_ID)))

jx_box <- make_bbox(lon=jx_sal_select$LONG, lat=jx_sal_select$LAT, f=0.5)
jx_map <- get_map(location = jx_box, maptype = "satellite", source = "google")

ggmap(jx_map)+ geom_point(data=jx_sal_select, aes(x=LONG, y=LAT, label=STN_NAME), size=0.5, color="grey20")  +
  geom_text(data=jx_sal_select, aes(x=LONG, y=LAT, label=STN_NAME), color="white") +
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-80.6, -80.2), y=c(27.6,28.0))


#cedar key
station1 <- read.csv(paste(enviro_data, "station_levy.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
station2 <- read.csv(paste(enviro_data, "station_dixie.csv", sep="/"))%>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
stations <- rbind(station1, station2)

result1 <- read.csv(paste(enviro_data, "result_levy.csv", sep="/")) 
result2 <- read.csv(paste(enviro_data, "result_dixie.csv", sep="/")) 
results <- rbind(result1, result2)

test <- results[str_detect(results$CharacteristicName, "Dep"),]

meta <- read.csv(paste(enviro_data, "meta_dixie.csv", sep="/")) 

ck_data <- left_join(stations, results, by="MonitoringLocationIdentifier")


#select salinity
ck_sal <- ck_data[ck_data$CharacteristicName %in% c("Salinity","Depth"),]
ck_sal$ResultMeasureValue <- as.numeric(as.character(ck_sal$ResultMeasureValue))
hist(ck_sal$ResultMeasureValue)
ck_sal <- ck_sal %>% dplyr::select(ActivityStartDate, CharacteristicName, ResultMeasureValue, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier) %>%
  dplyr::mutate(DATE=ymd(ActivityStartDate), Year=year(DATE), Month=month(DATE)) %>% dplyr::arrange(Year, Month) %>%
  dplyr::select(Year, Month, LatitudeMeasure, LongitudeMeasure, CharacteristicName, ResultMeasureValue, MonitoringLocationIdentifier)

ck_sal <- ck_sal[str_detect(ck_sal$MonitoringLocationIdentifier, c("SEAS")),]


CK_stations <- data.frame(unique(ck_sal$MonitoringLocationIdentifier))
write.csv(CK_stations, paste(out, "Seatrout_ENV_Chapter2/CK_FLDEP_stations.csv", sep="/"))


ck_box <- make_bbox(lon=ck_sal$LONG, lat=ck_sal$LAT, f=0.5)
ck_map <- get_map(location = ck_box, maptype = "satellite", source = "google")

ggmap(ck_map)+ geom_point(data=ck_sal, aes(x=LONG, y=LAT, label=STN_NAME), size=0.5, color="grey20")  +
  geom_text(data=ck_sal, aes(x=LONG, y=LAT, label=STN_NAME), color="white") +
  coord_equal()+theme_bw()+xlab("")+ylab("") + coord_cartesian(xlim=c(-80.6, -80.2), y=c(27.6,28.0))














#box plots for salinity

ggplot(tb_nit_nox, aes(StationID, Result_Value, fill=StationID)) +geom_boxplot()









