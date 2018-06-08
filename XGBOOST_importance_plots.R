#importance plots for BRT and GLM 

#"Seatrout_ENV_Chapter2/xgboost_results",

rm(list=ls())
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
var_names =data.frame(c("number",   "year",      "month", "salinity", "temperature", "riv_flow","allrivers", "Nit_val",
                        "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                        "aten_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                        "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    
                        "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                        "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                        "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     
                        "atspawn_waterT",     
                       "DissolvedO2", "StartDepth",     
                        "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val") )

colnames(var_names) <- "Feature"
require(data.table)
AP <- read.csv(paste(data, "/xgboost_results/AP4_pos_importance.csv", sep=""), header=T)
setDT(AP)
AP <- AP[AP$Gain > 0.01,]
AP$Feature <- as.character(AP$Feature)
AP <- full_join(var_names,AP)
AP$Area = "AP"
AP[is.na(AP)] <- 0

CK <- read.csv(paste(data, "/xgboost_results/CK4_pos_importance.csv", sep=""), header=T)
setDT(CK)
CK <- CK[CK$Gain > 0.01,]
CK$Feature <- as.character(CK$Feature)
CK <- full_join(var_names,CK)
CK$Area = "CK"
CK[is.na(CK)] <- 0

TB <- read.csv(paste(data, "/xgboost_results/TB4_pos_importance.csv", sep=""), header=T)
setDT(TB)
TB <- TB[TB$Gain > 0.01,]
TB$Feature <- as.character(TB$Feature)
TB <- full_join(var_names,TB)
TB$Area = "TB"
TB[is.na(TB)] <- 0

CH <- read.csv(paste(data, "/xgboost_results/CH4_pos_importance.csv", sep=""), header=T)
setDT(CH)
CH<- CH[CH$Gain > 0.01,]
CH$Feature <- as.character(CH$Feature)
CH <- full_join(var_names,CH)
CH$Area = "CH"
CH[is.na(CH)] <- 0

IR <- read.csv(paste(data, "/xgboost_results/IR4_pos_importance.csv", sep=""), header=T)
setDT(IR)
IR <- IR[IR$Gain > 0.01,]
IR$Feature <- as.character(IR$Feature)
IR <- full_join(var_names,IR)
IR$Area = "IR"
IR[is.na(IR)] <- 0

JX <- read.csv(paste(data, "/xgboost_results/JX4_pos_importance.csv", sep=""), header=T)
setDT(JX)
JX <- JX[JX$Gain > 0.01,]
JX$Feature <- as.character(JX$Feature)
JX <- full_join(var_names,JX)
JX$Area = "JX"
JX[is.na(JX)] <- 0


all <- rbind(AP, CK, TB, CH, JX, IR) 
all$Area <- factor(all$Area, levels=c("AP", "CK", "TB", "CH", "JX", "IR"))
all <- all[!(all$Feature %in% c("number", "aten_ceof", "avg_last3_nitro", "avg_last2_nitro", "astspawn_nitro", "atspawn_watert", "atspawn_salinity", "prev aut river flow all rivers")),]

all$Feature <- toupper(all$Feature)
all$Feature[all$Feature == "RIV_FLOW"] <- "RIVER FLOW (CLOSEST)"
all$Feature[all$Feature == "ALLRIVERS"] <- "RIVER FLOW (ALL)"
all$Feature[all$Feature == "TOTALMONTHLYRF"] <- "PRECIPITATION"
all$Feature[all$Feature == "WINTER_RF"] <- "WINTER PRECIPITATION"
all$Feature[all$Feature == "WINTER_DIS_ALL"] <- "WINTER RIVER FLOW ALL RIVERS"
all$Feature[all$Feature == "WINTER_DIS"] <- "WINTER RIVER FLOW (CLOSEST)"
all$Feature[all$Feature == "PREV_AUTUMN_RF"] <- "PREV AUT PRECIPITATION"
all$Feature[all$Feature == "PREV_AUTUMN_DIS"] <- "PREV AUT RIVER FLOW (CLOSEST)"
all$Feature[all$Feature == "NIT_VAL"] <- "DIN"
all$Feature[all$Feature == "CLOR_VAL"] <- "CHLOR A"
all$Feature[all$Feature == "EXT_CEOF"] <- "ATEN COEF"
all$Feature[all$Feature == "STARTDEPTH"] <- "DEPTH"
all$Feature[all$Feature == "Z_ANOM"] <- "Z ANOM"
all$Feature[all$Feature == "MAXT_ANOM"] <- "MAX T ANOM"
all$Feature[all$Feature == "MINT_ANOM"] <- "MIN T ANOM"
all$Feature[all$Feature == "WINTER_Z_ANOM"] <- "WINTER Z ANOM"
all$Feature[all$Feature == "WINTER_MAXT_ANOM"] <- "WINTER MAXT ANOM"
all$Feature[all$Feature == "WINTER_MINT_ANOM"] <- "WINTER MINT ANOM"
all$Feature[all$Feature == "PREV_AUTUMN_Z_ANOM"] <- "PREV AUTUMN Z ANOM"
all$Feature[all$Feature == "PREV_AUTUMN_MAXT_ANOM"] <- "PREV AUTUMN MAXT ANOM"
all$Feature[all$Feature == "PREV_AUTUMN_MINT_ANOM"] <- "PREV AUTUMN MINT ANOM"

CD_vars = c("Z ANOM","MAX T ANOM","MIN T ANOM","PREV AUTUMN Z ANOM","PREV AUTUMN MAXT ANOM" , "PREV AUTUMN MINT ANOM", "WINTER Z ANOM","WINTER MAXT ANOM","WINTER MINT ANOM")
Fim_vars = c("YEAR" ,"MONTH","SALINITY" ,"TEMPERATURE" ,"DISSOLVEDO2" ,"DEPTH", "ATEN COEF")
water_vars =c("PRECIPITATION" ,"RIVER FLOW (CLOSEST)","RIVER FLOW (ALL)", "WINTER RIVER FLOW (CLOSEST)","PREV AUT RIVER FLOW (CLOSEST)","WINTER PRECIPITATION","PREV AUT PRECIPITATION","WINTER RIVER FLOW ALL RIVERS")
bio_vars = c("DIN", "CHLOR A" )


#xgb.ggplot.importance(AP) + guides(fill=FALSE) + theme_bw()

orderfeats = c(Fim_vars, bio_vars, CD_vars, water_vars)
all <- all[all$Feature %in% orderfeats,]
all$Feature <- factor(all$Feature, levels=orderfeats)

File <- (paste("U:/PhD_projectfiles/Figures/importance_plots.png"))
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, units="in", width=7, height=9, res=300)

ggplot(all, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  
  xlab("Variable")+
  geom_vline(xintercept=seq(1.5, length(unique(all$Feature))-0.5, 1), 
                             lwd=0.25,linetype="dashed", colour="black")

dev.off()


#by management region
File <- (paste("U:/PhD_projectfiles/Figures/importance_plots_SWregion.png"))
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, units="in", width=7, height=9, res=300)


all_sw <- all[all$Area %in% c("TB", "CH"),]
all_sw <- all_sw[!(all_sw$Feature %in% c("FIRST_SPAWN_WATERT", "FIRST_SPAWN_SALINITY", "PREV AUT PRECIPITATION", "WINTER PRECIPITATION", "PREV AUTUMN MINT ANOM", "WINTER Z ANOM")),]

ggplot(all_sw, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() + 
  xlab("Variable")+
  geom_vline(xintercept=seq(1.5, length(unique(all$Feature))-0.5, 1), 
                             lwd=0.25,linetype="dashed", colour="black")
dev.off()


File <- (paste("U:/PhD_projectfiles/Figures/importance_plots_NWregion.png"))
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, units="in", width=7, height=9, res=300)


all_nw <- all[all$Area %in% c("AP", "CK"),]
all_nw <- all_nw[!all_nw$Feature %in% c("CHLOR A", "DIN", "WINTER RIVER FLOW ALL RIVERS", "PREV AUTUMN MAXT ANOM"),]
ggplot(all_nw, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  
  xlab("Variable")+
  geom_vline(xintercept=seq(1.5, length(unique(all$Feature))-0.5, 1), 
                             lwd=0.25,linetype="dashed", colour="black")

dev.off()


#by varaibles
all_CD <- all[all$Feature %in% c("prev_autumn_Z_anom", "prev_autumn_MaxT_anom", "prev_autumn_MinT_anom",                                "winter_Z_anom", "winter_MaxT_anom", "winter_MinT_anom", "Z_anom", "MaxT_anom", "MinT_anom"),]
all_CD$Feature <- toupper(all_CD$Feature)

all_fim <- all[all$Feature %in% c("year", "month", "temperature", "salinity", "DissolvedO2", "StartDepth", "aten_ceof"),]
all_fim$Feature <- toupper(all_fim$Feature)
all_fim$Feature[all_fim$Feature == "ATEN_CEOF"] <- "ATTENUATION_COEF"

all_water <- all[all$Feature %in% c("riv_flow", "TotalMonthlyRF", "winter_dis", "prev_autumn_dis", "winter_RF", "prev_autumn_RF", "winter_dis_ALL", "prev_autumn_dis_ALL"),]
all_water$Feature <- toupper(all_water$Feature)
all_water$Feature[all_water$Feature == "RIV_FLOW"] <- "RIVER FLOW (CLOSEST)"
all_water$Feature[all_water$Feature == "TOTALMONTHLYRF"] <- "PRECIPITATION"
all_water$Feature[all_water$Feature == "WINTER_RF"] <- "WINTER PRECIPITATION"
all_water$Feature[all_water$Feature == "WINTER_DIS_ALL"] <- "WINTER RIVER FLOW ALL RIVERS"
all_water$Feature[all_water$Feature == "WINTER_DIS"] <- "WINTER RIVER FLOW (CLOSEST)"
all_water$Feature[all_water$Feature == "PREV_AUTUMN_RF"] <- "PREV AUT PRECIPITATION"
all_water$Feature[all_water$Feature == "PREV_AUTUMN_DIS_ALL"] <- "PREV AUT RIVER FLOW ALL RIVERS"
all_water$Feature[all_water$Feature == "PREV_AUTUMN_DIS"] <- "PREV AUT RIVER FLOW (CLOSEST)"
all_water <- all_water[!(all_water$Feature == "PREV AUT RIVER FLOW ALL RIVERS"),]

all_bio <- all[all$Feature %in% c("Nit_val", "first_spawn_salinity", "first_spawn_waterT", "atpsawn_nitro",  "Clor_val"),]
all_bio$Feature <- toupper(all_bio$Feature)
all_bio$Feature[all_bio$Feature == "NIT_VAL"] <- 'DIN'


CD <- ggplot(all_CD, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) +
  scale_x_discrete(labels=function(x) str_wrap(x, width=10))+
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  geom_vline(xintercept=seq(1.5, length(unique(all_CD$Feature))-0.5, 1), 
                             lwd=1,linetype="dashed", colour="black") 


Water <- ggplot(all_water, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  geom_vline(xintercept=seq(1.5, length(unique(all_water$Feature))-0.5, 1), 
                             lwd=1,linetype="dashed", colour="black")

fim <- ggplot(all_fim, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  geom_vline(xintercept=seq(1.5, length(unique(all_fim$Feature))-0.5, 1), 
                             lwd=1,linetype="dashed", colour="black")

bio <- ggplot(all_bio, aes(x=Feature, y=Gain, fill=Area)) + geom_bar(stat="identity", position=position_dodge(), width=0.75) + 
  theme(panel.grid.minor=element_line(colour='grey'),panel.background=element_rect(fill='white', colour='black'),axis.text.x  = element_text(angle=0, vjust=0.5, size=16)) +
  coord_flip() +  geom_vline(xintercept=seq(1.5, length(unique(all_bio$Feature))-0.5, 1), 
                             lwd=1,linetype="dashed", colour="black")
library(gridExtra)
grid.arrange(CD, Water, fim, bio, ncol=2)

#basic plot
require(data.table)
AP <- read.csv(paste(data, "/xgboost_results/AP4_pospos_importance.csv", sep=""), header=T)
setDT(AP)

xgb.ggplot.importance(AP) + guides(fill=FALSE) + theme_bw()


CK <- read.csv(paste(data, "/xgboost_results/CK4_pospos_importance.csv", sep=""), header=T)
setDT(CK)
xgb.ggplot.importance(CK) + guides(fill=FALSE) + theme_bw()

TB <- read.csv(paste(data, "/xgboost_results/TB4_pospos_importance.csv", sep=""), header=T)
setDT(TB)
xgb.ggplot.importance(TB) + guides(fill=FALSE) + theme_bw()

CH <- read.csv(paste(data, "/xgboost_results/CH4_pospos_importance.csv", sep=""), header=T)
setDT(CH)
xgb.ggplot.importance(CH) + guides(fill=FALSE) + theme_bw()

IR <- read.csv(paste(data, "/xgboost_results/IR4_pospos_importance.csv", sep=""), header=T)
setDT(IR)
xgb.ggplot.importance(IR) + guides(fill=FALSE) + theme_bw()

JX <- read.csv(paste(data, "/xgboost_results/JX4_pospos_importance.csv", sep=""), header=T)
setDT(JX)
xgb.ggplot.importance(JX) + guides(fill=FALSE) + theme_bw()

