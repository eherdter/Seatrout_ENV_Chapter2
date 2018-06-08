#ABOUT ####
#complete cases GLM to compare to complete cases XGBOOST 
# exports important varaibles for each area and type (binomial vs poisson)
# calculates error statistics
rm(list=ls())

# Set Location
trace(utils:::unpackPkgZip, edit=TRUE)
IS_HOME = FALSE

if (IS_HOME == TRUE) {
  #personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
  #phys_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data"
  #enviro_data = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData"
  data =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes/Seatrout_ENV_Chapter2"
  source_location= "/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2"
  out =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes"
  setwd(data)
  source("~/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2/brt.functions.R")
} else {
  #work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
  #phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
  #enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
  data =   "U:/PhD_projectfiles/Exported_R_Datafiles/Seatrout_ENV_Chapter2"
  out =   "U:/PhD_projectfiles/Exported_R_Datafiles"
  source_location = "U:/R_For_School/Seatrout_ENV_Chapter2"
  source("U:/R_For_School/Seatrout_ENV_Chapter2/brt.functions.R")
  source("U:/R_For_School/Seatrout_ENV_Chapter2/functions_for_mfe_xgboost.R")
  source("G:/DATA/SPECIES/HOGFISH/Hogfish_2017/Recreational/IoA/Rscripts/cpue_functions_fromHog13.r")
  
  setwd(data)
}

library(tidyverse)
library(DescTools)

min.percent.red <- 0.1       # parameter must reduce the mean deviance by at least 0.5%
cutoff.percent <- min.percent.red 


#filter function
extra_filter = function(data, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE){
  
  results <- matrix(data=NA, nrow=45, ncol=5)
  
  #Process and create train and test
  TB_rgrs <- data
  if(dopospos == TRUE) {
    TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
    
    if (is.ckjxir == TRUE) {
      TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
      
      if(complete.cases==TRUE){
        TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
        
      } else if(complete.cases==FALSE) {
        TB_rgrs <- TB_rgrs
      }
    }
    else if (is.ckjxir==FALSE) {
      
      if(complete.cases==TRUE){
        TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
        
      } else if(complete.cases==FALSE) {
        TB_rgrs <- TB_rgrs
      }
    }
  }
  
  else if(dopospos == FALSE) {
    TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
    if (complete.cases==TRUE){
      TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
      
    } else if(complete.cases==FALSE){
      TB_rgrs <- TB_rgrs
    }
  }
}


# TAMPA BAY ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases

Name="TB"
TB <- filter_TB(4)
TB <- TB %>% dplyr::select(-aten_ceof)

#create positive and binomial datasets
d <- TB

#name the response variable
d$response = d$number
d.pos = d[d$response>=1,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(d.pos,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(d.pos),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)


predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
TB_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
TB_predCC$source <- "xgboost_predicted"
colnames(TB_predCC)[1] <- "y"

TB_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
TB_obsCC$source <- "observed"
colnames(TB_obsCC)[1] <- "y"
all_xgboostCC <- rbind(TB_predCC, TB_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, TB_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
TB_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB_ppos4_preds.csv", sep="/"), header=T, row.names=1)))
TB_pred$source <- "xgboost_predicted"
colnames(TB_pred)[1] <- "y"

TB_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB_ppos4_labels.csv", sep="/"), header=T, row.names=1))
TB_obs$source <- "observed"
colnames(TB_obs)[1] <- "y"
all_xgboost <- rbind(TB_pred, TB_obs)


#Error calculations ####
#Mean absolute error (MAE)
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- TB_predCC$y
obs_full <- TB_obs$y
pred_xgboost <- TB_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)

#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## Run binomial model ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable
TB <- filter_TB(4)
TB <- TB %>% dplyr::select(-c(aten_ceof, atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
d <- TB
d <- d[complete.cases(d),]
d$response = d$number
d <- d %>% dplyr::select(-number)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# HERE compare predictions those from xgboost ####

TB_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
TB_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
colnames(TB_xgboostCC_preds_ALL)[1] <- "y"
# 
# #should be the same as observedCC_ALL
TB_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
TB_xgboostCC_obs_ALL$source <- "observedCC"
colnames(TB_xgboostCC_obs_ALL)[1] <- "y"
# 
xgboostCC_ALL <- rbind(TB_xgboostCC_preds_ALL, TB_xgboostCC_obs_ALL)
ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
TB_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4_pos_preds.csv", sep="/"), header=T, row.names=1)))
TB_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(TB_xgboost_pred_ALL)[1] <- "y"

TB_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/TB4_pos_labels.csv", sep="/"), header=T, row.names=1))
TB_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(TB_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(TB_xgboost_pred_ALL, TB_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL$y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
TB_xgboostCC_preds_ALL <- TB_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
TB_xgboost_obs_ALL <- TB_xgboost_obs_ALL$y
TB_xgboost_pred_ALL <- TB_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, TB_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(TB_xgboost_obs_ALL, TB_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-TB_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(TB_xgboost_obs_ALL - TB_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL,MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm_ALL, ME_xgboostCC_ALL, ME_xgboost_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MTBE_glm, MTBE_xgboostCC, MTBE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


#....####
# APPALACH ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases

Name="AP"
AP <- filter_AP(4)
AP <- AP %>% dplyr::select(-c(ext_ceof, Nit_val))

#create positive and binomial datasets
d <- AP

#name the response variable
d$response = d$number
d.pos = d[d$response>0,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(AP,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(AP),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# POSITIVE Poisson ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)


predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
AP_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
AP_predCC$source <- "xgboost_predicted"
colnames(AP_predCC)[1] <- "y"

AP_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
AP_obsCC$source <- "observed"
colnames(AP_obsCC)[1] <- "y"
all_xgboostCC <- rbind(AP_predCC, AP_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, AP_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
AP_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4_pospos_preds.csv", sep="/"), header=T, row.names=1)))
AP_pred$source <- "xgboost_predicted"
colnames(AP_pred)[1] <- "y"

AP_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4_pospos_labels.csv", sep="/"), header=T, row.names=1))
AP_obs$source <- "observed"
colnames(AP_obs)[1] <- "y"
all_xgboost <- rbind(AP_pred, AP_obs)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")


#Error calculations ####
#MPE- mean percent error
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- AP_predCC$y
obs_full <- AP_obs$y
pred_xgboost <- AP_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)
#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## BINOMIAL ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable
AP <- filter_AP(4)
AP <- AP %>% dplyr::select(-c(ext_ceof, Nit_val)) %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

#create positive and binomial datasets
d <- AP

d$response = d$number
d <- d[complete.cases(d),]
d <- d %>% dplyr::select(-number)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# compare predictions those from xgboost ####

AP_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
AP_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
colnames(AP_xgboostCC_preds_ALL)[1] <- "y"
# 
#should be the same as observedCC_ALL
AP_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
AP_xgboostCC_obs_ALL$source <- "observedCC"
colnames(AP_xgboostCC_obs_ALL)[1] <- "y"
# 
xgboostCC_ALL <- rbind(AP_xgboostCC_preds_ALL, AP_xgboostCC_obs_ALL)
ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
AP_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4_pos_preds.csv", sep="/"), header=T, row.names=1)))
AP_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(AP_xgboost_pred_ALL)[1] <- "y"

AP_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/AP4_pos_labels.csv", sep="/"), header=T, row.names=1))
AP_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(AP_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(AP_xgboost_pred_ALL, AP_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL $y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
AP_xgboostCC_preds_ALL <- AP_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
AP_xgboost_obs_ALL <- AP_xgboost_obs_ALL$y
AP_xgboost_pred_ALL <- AP_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, AP_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(AP_xgboost_obs_ALL, AP_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-AP_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(AP_xgboost_obs_ALL - AP_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL, MAE_xgboost_ALL, MAE_xgboostCC_ALL, ME_glm_ALL,  ME_xgboost_ALL, ME_xgboostCC_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MAPE_glm, MAPE_xgboostCC, MAPE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)




#....####
# CEDAR KEY ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases

Name="CK"
CK4 <- filter_CK(4) %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
CK4sht<- CK4 %>% dplyr::select(-c(ext_ceof, winter_dis_ALL, prev_autumn_dis_ALL))

CK <- extra_filter(CK4sht, dopospos=TRUE, complete.cases=TRUE,is.ckjxir=TRUE)

#create positive and binomial datasets
d <- CK

#name the response variable
d$response = d$number
d.pos = d[d$response>0,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(CK,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(CK),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)


predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
CK_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
CK_predCC$source <- "xgboost_predicted"
colnames(CK_predCC)[1] <- "y"

CK_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
CK_obsCC$source <- "observed"
colnames(CK_obsCC)[1] <- "y"
all_xgboostCC <- rbind(CK_predCC, CK_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, CK_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
CK_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4_ppos_preds.csv", sep="/"), header=T, row.names=1)))
CK_pred$source <- "xgboost_predicted"
colnames(CK_pred)[1] <- "y"

CK_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4_ppos_labels.csv", sep="/"), header=T, row.names=1))
CK_obs$source <- "observed"
colnames(CK_obs)[1] <- "y"
all_xgboost <- rbind(CK_pred, CK_obs)

ggplot(all_xgboost, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#Error calculations ####
#MPE- mean percent error
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- CK_predCC$y
obs_full <- CK_obs$y +0.0001
pred_xgboost <- CK_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)

#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## Run binomial model ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable
CK <- filter_CK(4)
CK <- CK %>%dplyr::select(-c(ext_ceof, winter_dis_ALL, prev_autumn_dis_ALL)) 
d <- CK
d$response = d$number
d <- d[complete.cases(d),]
d <- d %>% dplyr::select(-number)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# HERE compare predictions those from xgboost ####

 CK_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
CK_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
colnames(CK_xgboostCC_preds_ALL)[1] <- "y"
# 
 #should be the same as observedCC_ALL
 CK_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
 CK_xgboostCC_obs_ALL$source <- "observedCC"
 colnames(CK_xgboostCC_obs_ALL)[1] <- "y"
 
 xgboostCC_ALL <- rbind(CK_xgboostCC_preds_ALL, CK_xgboostCC_obs_ALL)
 ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
CK_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4_pos_preds.csv", sep="/"), header=T, row.names=1)))
CK_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(CK_xgboost_pred_ALL)[1] <- "y"

CK_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CK4_pos_labels.csv", sep="/"), header=T, row.names=1))
CK_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(CK_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(CK_xgboost_pred_ALL, CK_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL $y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
CK_xgboostCC_preds_ALL <- CK_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
CK_xgboost_obs_ALL <- CK_xgboost_obs_ALL$y
CK_xgboost_pred_ALL <- CK_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, CK_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(CK_xgboost_obs_ALL, CK_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-CK_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(CK_xgboost_obs_ALL - CK_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL, MAE_xgboost_ALL, MAE_xgboostCC_ALL, ME_glm_ALL,  ME_xgboost_ALL, ME_xgboostCC_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MCKE_glm, MCKE_xgboostCC, MCKE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


#....####
# CHARLOTTE HARBOR ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases

Name="CH"
CH <- filter_CH(4)
CH <- CH %>% dplyr::select(-c(ext_ceof, Nit_val))

#create positive and binomial datasets
d <- CH

#name the response variable
d$response = d$number
d.pos = d[d$response>0,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(d.pos,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(d.pos),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)


predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
CH_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
CH_predCC$source <- "xgboost_predicted"
colnames(CH_predCC)[1] <- "y"

CH_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
CH_obsCC$source <- "observed"
colnames(CH_obsCC)[1] <- "y"
all_xgboostCC <- rbind(CH_predCC, CH_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, CH_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
CH_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4_ppos_preds.csv", sep="/"), header=T, row.names=1)))
CH_pred$source <- "xgboost_predicted"
colnames(CH_pred)[1] <- "y"

CH_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4_ppos_labels.csv", sep="/"), header=T, row.names=1))
CH_obs$source <- "observed"
colnames(CH_obs)[1] <- "y"
all_xgboost <- rbind(CH_pred, CH_obs)

ggplot(all_xgboost, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#Error calculations ####
#MPE- mean percent error
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- CH_predCC$y
obs_full <- CH_obs$y +0.0001
pred_xgboost <- CH_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)

#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## Run binomial model ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable
CH <- filter_CH(4)
CH <- CH %>% dplyr::select(-c(ext_ceof, Nit_val))%>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
d <- CH
d$response = d$number
d <- d[complete.cases(d),]
d <- d %>% dplyr::select(-number)


#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# HERE compare predictions those from xgboost ####
 CH_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
 CH_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
 colnames(CH_xgboostCC_preds_ALL)[1] <- "y"

 #should be the same as observedCC_ALL
 CH_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
 CH_xgboostCC_obs_ALL$source <- "observedCC"
 colnames(CH_xgboostCC_obs_ALL)[1] <- "y"
 
 xgboostCC_ALL <- rbind(CH_xgboostCC_preds_ALL, CH_xgboostCC_obs_ALL)
 ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
CH_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4_pos_preds.csv", sep="/"), header=T, row.names=1)))
CH_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(CH_xgboost_pred_ALL)[1] <- "y"

CH_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/CH4_pos_labels.csv", sep="/"), header=T, row.names=1))
CH_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(CH_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(CH_xgboost_pred_ALL, CH_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL $y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
CH_xgboostCC_preds_ALL <- CH_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
CH_xgboost_obs_ALL <- CH_xgboost_obs_ALL$y
CH_xgboost_pred_ALL <- CH_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, CH_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(CH_xgboost_obs_ALL, CH_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-CH_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(CH_xgboost_obs_ALL - CH_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL, MAE_xgboost_ALL, MAE_xgboostCC_ALL,  ME_glm_ALL,  ME_xgboost_ALL, ME_xgboostCC_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MCHE_glm, MCHE_xgboostCC, MCHE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


#....####
# JAX ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases
Name="JX"
JX <- filter_JX(4)
JX <- JX %>% dplyr::select(-ext_ceof)

#create positive and binomial datasets
d <- JX

#name the response variable
d$response = d$number
d.pos = d[d$response>0,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(d.pos,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(d.pos),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)


predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
JX_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
JX_predCC$source <- "xgboost_predicted"
colnames(JX_predCC)[1] <- "y"

JX_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
JX_obsCC$source <- "observed"
colnames(JX_obsCC)[1] <- "y"
all_xgboostCC <- rbind(JX_predCC, JX_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, JX_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
JX_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4_pospos_preds.csv", sep="/"), header=T, row.names=1)))
JX_pred$source <- "xgboost_predicted"
colnames(JX_pred)[1] <- "y"

JX_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4_pospos_labels.csv", sep="/"), header=T, row.names=1))
JX_obs$source <- "observed"
colnames(JX_obs)[1] <- "y"
all_xgboost <- rbind(JX_pred, JX_obs)

ggplot(all_xgboost, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#Error calculations ####
#MPE- mean percent error
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- JX_predCC$y
obs_full <- JX_obs$y +0.0001
pred_xgboost <- JX_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)

#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## Run binomial model ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable
JX <- filter_JX(4)
JX <- JX %>% dplyr::select(-c(ext_ceof))
d <- JX

d$response = d$number
d <- d[complete.cases(d),]
d <- d %>% dplyr::select(-number)
#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# HERE compare predictions those from xgboost ####
JX_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
JX_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
colnames(JX_xgboostCC_preds_ALL)[1] <- "y"
# 
#should be the same as observedCC_ALL
 JX_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
 JX_xgboostCC_obs_ALL$source <- "observedCC"
 colnames(JX_xgboostCC_obs_ALL)[1] <- "y"
# 
 xgboostCC_ALL <- rbind(JX_xgboostCC_preds_ALL, JX_xgboostCC_obs_ALL)
ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
JX_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4_pos_preds.csv", sep="/"), header=T, row.names=1)))
JX_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(JX_xgboost_pred_ALL)[1] <- "y"

JX_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/JX4_pos_labels.csv", sep="/"), header=T, row.names=1))
JX_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(JX_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(JX_xgboost_pred_ALL, JX_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL $y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
JX_xgboostCC_preds_ALL <- JX_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
JX_xgboost_obs_ALL <- JX_xgboost_obs_ALL$y
JX_xgboost_pred_ALL <- JX_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, JX_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(JX_xgboost_obs_ALL, JX_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-JX_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(JX_xgboost_obs_ALL - JX_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL, MAE_xgboost_ALL, MAE_xgboostCC_ALL, ME_glm_ALL,  ME_xgboost_ALL, ME_xgboostCC_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MJXE_glm, MJXE_xgboostCC, MJXE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)



#....####
# INDIAN RIVER ####
#import and filter ####
# filter and remove the same variables as was removed in exploration plots and BRT.R when running complete cases

Name="IR"
IR <- filter_IR(4)
IR <- IR %>% dplyr::select(-c(Nit_val, ext_ceof, atspawn_nitro, avg_last2_nitro, avg_last3_nitro))

#create positive and binomial datasets
d <- IR

#name the response variable
d$response = d$number
d.pos = d[d$response>0,]
d.pos <- d.pos[complete.cases(d.pos),]
d.pos <- d.pos %>% dplyr::select(-number)

Num_NA<-sapply(d.pos,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(d.pos),Count=Num_NA)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d.pos <- d.pos[sample(nrow(d.pos)),]
numberOfTrainingSamples <- round(nrow(d.pos) * .7)
d.pos_slct<- d.pos[1:numberOfTrainingSamples,]
d.pos_test <- d.pos[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full.pos = glm(response ~ .,family=poisson,data=d.pos_slct)
glm.base.pos = glm(response ~ 1,family=poisson,data=d.pos_slct)

glm.step.pos = step(glm.base.pos,scope=list(upper=glm.full.pos,lower=glm.base.pos),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTabPos=try(get.dev.table(glm.step.pos$anova))
if (class(devTabPos) != 'try-error'){
  devTabPos$Step=as.character(devTabPos$Step)
  devTabPos$Step=sapply(devTabPos$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabPos=devTabPos[,c(1,2,3,4,5,6,9)]
  write.table(devTabPos, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabPos_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step.pos$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step.pos$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabPos_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d.pos_slct)
glm.final.pos  = final.p.glm

#run on out of bag observations ####
test_predictions <- predict(glm.final.pos, newdata=d.pos_test, type="response")
test_predictions <- round(test_predictions)

predictions <- round(as.data.frame(test_predictions))
predictions$source <- "glm_predicted"
colnames(predictions)[1] <- "y"

observed <- as.data.frame(d.pos_test$response)
observed$source <- "observed"
colnames(observed)[1] <- "y"

all_glm <- rbind(predictions,observed)
ggplot(all_glm, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

#compare predictions those from xgboost ####
IR_predCC <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4sht_pospos_preds_CC.csv", sep="/"), header=T, row.names=1)))
IR_predCC$source <- "xgboost_predicted"
colnames(IR_predCC)[1] <- "y"

IR_obsCC <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4sht_pospos_labels_CC.csv", sep="/"), header=T, row.names=1))
IR_obsCC$source <- "observed"
colnames(IR_obsCC)[1] <- "y"
all_xgboostCC <- rbind(IR_predCC, IR_obsCC)
ggplot(all_xgboostCC, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Plot comparison ####
comb <- rbind(observed, predictions, IR_predCC)
ggplot(comb, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#add in full xgboost (no complete cases )
IR_pred <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4_pospos_preds.csv", sep="/"), header=T, row.names=1)))
IR_pred$source <- "xgboost_predicted"
colnames(IR_pred)[1] <- "y"

IR_obs <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4_pospos_labels.csv", sep="/"), header=T, row.names=1))
IR_obs$source <- "observed"
colnames(IR_obs)[1] <- "y"
all_xgboost <- rbind(IR_pred, IR_obs)

ggplot(all_xgboost, aes(x=y, fill=source))+ geom_histogram(position="dodge") +ggtitle("predicted and observed counts")

#Error calculations ####
#MPE- mean percent error
#glm formulation

obs <- observed$y +0.0001
pred_glm <- predictions$y
pred_xgboostCC <- IR_predCC$y
obs_full <- IR_obs$y +0.0001
pred_xgboost <- IR_pred$y

MAE_glm = MAE(obs, pred_glm)
MAE_xgboostCC = MAE(obs, pred_xgboostCC)
MAE_xgboost = MAE(obs_full, pred_xgboost)

#mean error
ME_glm = mean(obs-pred_glm)
ME_xgboostCC = mean(obs-pred_xgboostCC)
ME_xgboost =mean(obs_full - pred_xgboost)

#MAPE mean absolute percentage error
MAPE_glm = MAPE(obs, pred_glm)
MAPE_xgboostCC = MAPE(obs, pred_xgboostCC)
MAPE_xgboost = MAPE(obs_full, pred_xgboost)

#RMSE
RMSE_glm = RMSE(obs, pred_glm)
RMSE_xgboostCC =RMSE(obs, pred_xgboostCC)
RMSE_xgboost = RMSE(obs_full, pred_xgboost)

## Run binomial model ####
#create binary dataset
d.bin = d
d.bin$response = ifelse(d$response>0,1,0)
d.bin <- d.bin %>% dplyr::select(-c(atspawn_salinity, atspawn_waterT))

d.bin <- d.bin[complete.cases(d.bin),]
d.bin <- d.bin %>% dplyr::select(-number)

set.seed(12345) #set the seed so that it always samples the same

d.bin <- d.bin[sample(nrow(d.bin)),]
numberOfTrainingSamples <- round(nrow(d.bin) * .7)
d.bin_slct<- d.bin[1:numberOfTrainingSamples,]
d.bin_test <- d.bin[-(1:numberOfTrainingSamples),]

#run binomial model ####
glm.full.bin = glm(response ~ .,family=binomial,data=d.bin_slct)
glm.base.bin = glm(response ~ 1,family=binomial,data=d.bin_slct)

glm.step.b  <- step(glm.base.bin,scope=list(upper=glm.full.bin,lower=glm.base.bin),direction='forward',trace=1,k=2)


#produce and export the deviance table for binary 
#This function is in cpue_functions_fromHog13.R and it should be sourced above. 
# You can call get.dev.table(glm.step.b$anova) to get the deviance and AIC table
devTabBin=try(get.dev.table(glm.step.b$anova))
if (class(devTabBin) != 'try-error'){ #if the devTab does not produce, or not equal to, a "try-error
  devTabBin$Step=as.character(devTabBin$Step)
  devTabBin$Step=sapply(devTabBin$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTabBin=devTabBin[,c(1,2,3,4,5,6,9)]
  write.table(devTabBin, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- find.formula(glm.step.b$anova)[[1]]
  form.b
} else {
  write.table(glm.step.b$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabBin_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.b <- ""
}

f.1             <- paste("response ~ ",form.b)
final.b.glm     <- glm(f.1,family=binomial,data=d.bin_slct)

test_predictions <- predict(final.b.glm, newdata=d.bin_test, type="response")
test_predictions <- round(test_predictions,2)

#Error calculation ####
obs <- d.bin_test$response 
err_glm <- mean(as.numeric(test_predictions > 0.5) != obs)

# POISSON ####
#name the response variable

IR <- filter_IR(4)
IR <- IR %>% dplyr::select(-c(Nit_val, ext_ceof, atspawn_nitro, avg_last2_nitro, avg_last3_nitro,atspawn_waterT, atspawn_salinity ))

d <- IR

d$response = d$number
d <- d[complete.cases(d),]
d <- d %>% dplyr::select(-number)

#create test and training set
set.seed(12345) #set the seed so that it always samples the same

d <- d[sample(nrow(d)),]
numberOfTrainingSamples <- round(nrow(d) * .7)
d_slct<- d[1:numberOfTrainingSamples,]
d_test <- d[-(1:numberOfTrainingSamples),]

# Run positive model ####
glm.full = glm(response ~ .,family=poisson,data=d_slct)
glm.base = glm(response ~ 1,family=poisson,data=d_slct)

glm.step = step(glm.base,scope=list(upper=glm.full,lower=glm.base),direction='forward',trace=1,k=2)
#the anova variables is a deviance and AIC table
#produce and export the deviance table for positive
devTab=try(get.dev.table(glm.step$anova))
if (class(devTab) != 'try-error'){
  devTab$Step=as.character(devTab$Step)
  devTab$Step=sapply(devTab$Step,function(x) sub("+ ","",x, fixed = TRUE))
  devTab=devTab[,c(1,2,3,4,5,6,9)]
  write.table(devTab, file=paste(paste(out, "Seatrout_ENV_Chapter2/devTabAll_", sep="/") ,Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p <- find.formula(glm.step$anova)[[1]] 
  form.p
  
} else {
  write.table(glm.step$anova, file=paste(paste(out,"Seatrout_ENV_Chapter2/devTabAll_", sep="/"), Name,".csv",sep=""), row.names=FALSE, sep=",")
  form.p = ""
}


f.1             <- paste("response ~",form.p)
# the variables come from the anova output
final.p.glm     <- glm(f.1,family=poisson,data=d_slct)
glm.final  = final.p.glm

#run on out of bag observations ####
glm_predsCC_ALL <- round(as.data.frame(predict(glm.final, newdata=d_test, type="response")))

glm_predsCC_ALL$source <- "glm_predicted_ALL"
colnames(glm_predsCC_ALL)[1] <- "y"

observedCC_ALL <- as.data.frame(d_test$response)
observedCC_ALL$source <- "observed_ALL"
colnames(observedCC_ALL)[1] <- "y"

glmCC_ALL <- rbind(glm_predsCC_ALL,observedCC_ALL)
ggplot(glmCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("glm")

# HERE compare predictions those from xgboost ####
#ADD IN ####
IR_xgboostCC_preds_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4sht_pos_preds_CC.csv", sep="/"), header=T, row.names=1)))
IR_xgboostCC_preds_ALL$source <- "xgboost_predicted_fullposCC"
colnames(IR_xgboostCC_preds_ALL)[1] <- "y"
# 
# #should be the same as observedCC_ALL
IR_xgboostCC_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4sht_pos_labels_CC.csv", sep="/"), header=T, row.names=1))
 IR_xgboostCC_obs_ALL$source <- "observedCC"
 colnames(IR_xgboostCC_obs_ALL)[1] <- "y"
# 
xgboostCC_ALL <- rbind(IR_xgboostCC_preds_ALL, IR_xgboostCC_obs_ALL)
 ggplot(xgboostCC_ALL, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#add in full xgboost (no complete cases )
IR_xgboost_pred_ALL <- round(as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4_pos_preds.csv", sep="/"), header=T, row.names=1)))
IR_xgboost_pred_ALL$source <- "xgboost_predicted_fullpos"
colnames(IR_xgboost_pred_ALL)[1] <- "y"

IR_xgboost_obs_ALL <-  as.data.frame(read.csv(paste(out, "Seatrout_ENV_Chapter2/xgboost_results/IR4_pos_labels.csv", sep="/"), header=T, row.names=1))
IR_xgboost_obs_ALL$source <- "observed_fullpos"
colnames(IR_xgboost_obs_ALL)[1] <- "y"
all_xgboost <- rbind(IR_xgboost_pred_ALL, IR_xgboost_obs_ALL)
ggplot(all_xgboost, aes(x=y, fill=source)) + geom_histogram(position="dodge") +ggtitle("xgboost")

#Error calculations ####
#MPE- mean percent error
#glm formulation

#complete cases observed and predicted for poisson (ALL)
obsCC_ALL <- observedCC_ALL $y +0.0001
glm_predsCC_ALL <- glm_predsCC_ALL$y
IR_xgboostCC_preds_ALL <- IR_xgboostCC_preds_ALL$y

# full dataset observed and predicted for poisson (in xgboost only)
IR_xgboost_obs_ALL <- IR_xgboost_obs_ALL$y
IR_xgboost_pred_ALL <- IR_xgboost_pred_ALL$y

#Mean absolute error 
MAE_glm_ALL = MAE(obsCC_ALL, glm_predsCC_ALL)
MAE_xgboostCC_ALL = MAE(obsCC_ALL, IR_xgboostCC_preds_ALL)
MAE_xgboost_ALL = MAE(IR_xgboost_obs_ALL, IR_xgboost_pred_ALL)


#mean error
ME_glm_ALL = mean(obsCC_ALL-glm_predsCC_ALL)
ME_xgboostCC_ALL = mean(obsCC_ALL-IR_xgboostCC_preds_ALL)
ME_xgboost_ALL =mean(IR_xgboost_obs_ALL - IR_xgboost_pred_ALL)


#write out error calcs ####
err_ALL <- cbind(MAE_glm_ALL, MAE_xgboost_ALL, MAE_xgboostCC_ALL, ME_glm_ALL,  ME_xgboost_ALL, ME_xgboostCC_ALL)
#err_ALL <- cbind(MAE_glm, MAE_xgboostCC_ALL, MAE_xgboost_ALL,ME_glm, ME_xgboostCC_ALL, ME_xgboost_ALL)

write.csv(err_ALL, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs_FULLPOISSON", sep="/"), Name, ".csv", sep=""), row.names=FALSE)


err <- cbind(err_glm, ME_glm, ME_xgboostCC, ME_xgboost, MAE_glm, MAE_xgboostCC, MAE_xgboost, MIRE_glm, MIRE_xgboostCC, MIRE_xgboost, RMSE_glm, RMSE_xgboostCC, RMSE_xgboost)
write.csv(err, paste(paste(out, "Seatrout_ENV_Chapter2/error_calcs", sep="/"), Name, ".csv", sep=""), row.names=FALSE)



