#importance plots for BRT and GLM 

#"Seatrout_ENV_Chapter2/xgboost_results",


# Set Location
IS_HOME = FALSE

if (IS_HOME == TRUE) {
  data =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes/Seatrou"
  source_location= "/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2"
  out =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes"
  setwd(data)
  source("~/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2/brt.functions.R")
} else {
  data =   "U:/PhD_projectfiles/Exported_R_Datafiles/Seatrout_ENV_Chapter2"
  setwd(data)
}


library(tidyverse)
library(xgboost)

#read in importance for xgboost
var_names =data.frame(c("number", "year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                        "aten_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                        "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                        "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                        "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                        "first_spawn_waterT",  "DissolvedO2", "StartDepth",     
                        "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val") )
colnames(var_names) <- "Feature"
require(data.table)
AP <- read.csv(paste(data, "/xgboost_results/AP4_pospos_importance.csv", sep=""), header=T)
setDT(AP)
AP$Feature <- as.character(AP$Feature)
AP <- full_join(var_names,AP)
AP$Area = "AP"
AP[is.na(AP)] <- 0

CK <- read.csv(paste(data, "/xgboost_results/CK4_pospos_importance.csv", sep=""), header=T)
setDT(CK)
CK$Feature <- as.character(CK$Feature)
CK <- full_join(var_names,CK)
CK$Area = "CK"
CK[is.na(CK)] <- 0

TB <- read.csv(paste(data, "/xgboost_results/TB4_pospos_importance.csv", sep=""), header=T)
setDT(TB)
TB$Feature <- as.character(TB$Feature)
TB <- full_join(var_names,TB)
TB$Area = "TB"
TB[is.na(TB)] <- 0

CH <- read.csv(paste(data, "/xgboost_results/CH4_pospos_importance.csv", sep=""), header=T)
setDT(CH)
CH$Feature <- as.character(CH$Feature)
CH <- full_join(var_names,CH)
CH$Area = "CH"
CH[is.na(CH)] <- 0

IR <- read.csv(paste(data, "/xgboost_results/IR4_pospos_importance.csv", sep=""), header=T)
setDT(IR)
IR$Feature <- as.character(IR$Feature)
IR <- full_join(var_names,IR)
IR$Area = "IR"
IR[is.na(IR)] <- 0

JX <- read.csv(paste(data, "/xgboost_results/JX4_pospos_importance.csv", sep=""), header=T)
setDT(JX)
JX$Feature <- as.character(JX$Feature)
JX <- full_join(var_names,JX)
JX$Area = "JX"
JX[is.na(JX)] <- 0


all <- rbind(AP, CK, TB, CH, IR, JX) 
all <- all[!(all$Feature %in% c("number", "aten_ceof")),]

all_CD <- all[all$Feature %in% c("Z_anom", "MaxT_anom", "MinT_anom", "")]


#remove <- all %>% group_by(Feature) %>% summarize(mean=mean(Gain))
# CK$Feature <- toupper(CK$Feature)
# 
# CK$Feature[CK$Feature == "TOTALMONTHLYRF"] <- "MONTHLY_PRECIPITATION"
# CK$Feature[CK$Feature == "EXT_CEOF"] <- "ATTENUATION_COEF"
# CK$Feature[CK$Feature == "EXT_CEOF"] <- "ATTENUATION_COEF"
# CK$Feature[CK$Feature == "STARTDEPTH"] <- "DEPTH"
# CK$Feature[CK$Feature == "WINTER_MINT_ANOM"] <- "WINTER_MIN_TEMP_ANOM"


#xgb.ggplot.importance(AP) + guides(fill=FALSE) + theme_bw()

ggplot(all, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge()) + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
                                                                                                              panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=90, vjust=0.5, size=16)) 



